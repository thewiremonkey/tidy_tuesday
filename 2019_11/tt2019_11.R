library(tidyverse)
library(tidytext)
library(caret)
library(randomForest)

options(scipen = 999999)

if(!grepl("2019_11", getwd())){setwd("./2019_11")}


##assist: https://cfss.uchicago.edu/text_classification.html

# url<-"https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv"

raw<-read_csv("board_games.csv")

Encoding(raw$name)<-"UTF-8"
Encoding(raw$description)<-"UTF-8"

#get a count of how many categories a game falls into.
raw$cnt<-unlist(lapply(str_split(raw$category, ","), length))

#For ease, let's just use those with a single category
single_cat<-raw %>% filter(cnt==1, !is.na(category)) %>%
  select(game_id, description,category) %>%
  mutate(category = gsub(' \\/ ', ' ', category)) %>%
  mutate(category=gsub('\\/', ' ', category))

top_cat<-single_cat %>%
  group_by(category) %>%
  tally() %>%
  top_n(10) %>%
  ungroup() %>%
  arrange(desc(n))

single_cat<-single_cat %>% filter(category %in% top_cat$category)
##-----this uses the words inthe mechanic field as variables
m_cat<-raw %>% filter(cnt==1, !is.na(category)) %>%
  select(game_id, mechanic, category) %>%
  mutate(category=gsub(' \\/ ', ' ', category)) %>%
  mutate(category=gsub('\\/', ' ', category)) %>%
  filter(category %in% top_cat$category)

g_tok<-m_cat%>% select(game_id, mechanic, category) %>%
  filter(category %in% top_cat$category) %>%
  unnest_tokens(output = word, input=mechanic) %>%
  anti_join(tidytext::stop_words) %>%
  mutate(word=SnowballC::wordStem(words = `word`)) %>%
  mutate(word=tm::removeNumbers(word)) %>%
  mutate(word=tm::removePunctuation(word))

g_tok_dtm<-g_tok %>%
  count(game_id, word) %>%
  filter(word !="") %>%
  tidytext::cast_dtm(document=game_id, term=word, value=n) %>%
  tm::removeSparseTerms(sparse=.99)


g_dtm<-g_tok %>%
  count(game_id, word) %>%
  filter(word !="") %>%
  tidytext::cast_dtm(document=game_id, term=word, value=n) %>%
  tm::removeSparseTerms(sparse=.99)

g_tfidf<-g_tok %>%
  count(category, word) %>%
  bind_tf_idf(term=word, document=category, n=n)

g_df<-as.data.frame(as.matrix(g_dtm)) %>%
  mutate(game_id=as.numeric(row.names(.))) %>%
  left_join(raw %>% select(game_id, category)) %>%
  select(-game_id)
colnames(g_df)<-paste0(colnames(g_df), "_c","")

g_df$category_c<-factor(g_df$category_c)

g_fm<-randomForest(category_c~.,data=g_df,  ntree=500)
varImpPlot(g_fm)

sum(diag(g_fm$confusion))/nrow(g_df)

g_cm<-g_fm$confusion

####---------

game_tokens<-single_cat %>%
  filter(category %in% top_cat$category) %>%
  unnest_tokens(output = word, input=description) %>%
  anti_join(tidytext::stop_words) %>%
  mutate(word=SnowballC::wordStem(words = `word`)) %>%
  mutate(word=tm::removeNumbers(word)) %>%
  mutate(word=tm::removePunctuation(word))


game_dtm<-game_tokens %>%
  count(game_id, word) %>%
  filter(word !="") %>%
  tidytext::cast_dtm(document=game_id, term=word, value=n) %>%
  tm::removeSparseTerms(sparse=.99)

game_tfidf<-game_tokens %>%
  count(category, word) %>%
  bind_tf_idf(term=word, document=category, n=n)


# sort the data frame and convert word to a factor column
plot_game <- game_tfidf %>%
  ungroup() %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  # filter(category %in% top_cat$category) %>%
  group_by(category) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf))+
  geom_col()+
  labs(x=NULL, y="tf-idf")+
  coord_flip()+
  facet_wrap(~category, scales="free_y")+
  ggthemes::theme_economist()



#Estimate Model
start=Sys.time()
game_rf <- train(x = as.matrix(game_dtm),
                 y = factor(single_cat$category),
                 method = "rf",
                 ntree = 200,
                 metric="Accuracy",
                 trControl = trainControl(method = "oob"))

randomForest::varImpPlot(game_rf$finalModel)
final_df<-as.data.frame(game_rf$finalModel$confusion)
end=Sys.time()
print(end-start)

game_fit_df<-as.data.frame(as.matrix(game_dtm)) %>%
  mutate(game_id=as.numeric(row.names(.))) %>%
  left_join(raw %>% select(category, game_id))

colnames(game_fit_df)<-paste0(colnames(game_fit_df), "_c","")
game_fit_df$category_c<-factor(game_fit_df$category_c)

ingametrain<-createDataPartition(game_fit_df$category_c, p=.7, list=FALSE)
gametrain<-game_fit_df[ingametrain,]
gametest<-game_fit_df[-ingametrain,]
gamefit<-randomForest(category_c~.,data=gametrain,  ntree=500)
varImpPlot(gamefit)
game_fit_cm<-gamefit$confusion
gamepred<-predict(gamefit, gametest)
table(gamepred, gametest$category_c)
gamepred_cm<-confusionMatrix(gamepred,factor(gametest$category_c))
sum(diag(table(gamepred, gametest$category_c)))/nrow(gametest)
gamepred_cm_df<-broom::tidy(gamepred_cm$byClass)


##create training and test set

#####----include additional variables
other_vars_ints<-raw %>%
  select_if(is.integer)

other_vars<-as.data.frame(as.matrix(game_dtm)) %>%
  mutate(game_id=row.names(.)) %>%
  select(game_id,1:823) %>%
  mutate(game_id = as.integer(game_id)) %>%
  left_join(other_vars_ints) %>%
  left_join(raw %>% select(game_id, category)) %>%
  filter(complete.cases(.))

#ensure that no names are accidentally functions
colnames(other_vars)<-paste0(colnames(other_vars),"_c", "")

other_vars$category_c<-factor(other_vars$category_c)
intrain<-createDataPartition(y=other_vars$category_c,p=0.7,list=FALSE)
training<-other_vars[intrain,]
testing<-other_vars[-intrain,]
fit <- randomForest(category_c~.,data=training,  ntree=500, importance=TRUE)
varImpPlot(fit)
final_vars_df<-as.data.frame(fit$confusion)

pred <- predict(fit, newdata = testing)
table(pred,testing$category_c)
pred_cm<-confusionMatrix(pred,factor(testing$category_c))
sum(diag(table(pred, testing$category_c)))/nrow(testing)
pred_cm_df<-broom::tidy(pred_cm)



other_vars_df<-other_vars %>%  select(-game_id_c)
other_vars_df$category_c<-factor(other_vars_df$category_c)
intrain1<-createDataPartition(y=other_vars_df$category_c,p=0.7,list=FALSE)
training1<-other_vars_df[intrain,]
testing1<-other_vars_df[-intrain,]

fit1 <- randomForest(category_c~.,data=training1,  ntree=500)
varImpPlot(fit1)
final_vars_df1<-as.data.frame(fit1$confusion)


other_rf<-train(factor(category_c)~., data=training1,
                method="rf",
                ntree=2000,
                trControl = trainControl(method = "oob"))

pred1<-predict(other_rf, testing1)
confusionMatrix(pred1, factor(testing1$category_c))
