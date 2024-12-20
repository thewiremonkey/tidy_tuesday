library(tidytuesdayR)
library(tidyverse)
library(tidytext)
library(cluster) 
library(factoextra)
library(circlize)
library(dendextend)
library(paletteer)


tt<-tt_load(2024, 51)

#Get the dataframe
raw<-tt$spells

##### hierarchical cluster###

# Tokenize and clean the text
# Extract the numeric data from range and duration
data<- raw %>%
  mutate(range = as.numeric(str_extract(range, "(\\d)+")),
         range = ifelse(is.na(range),0,range),
         dur_num = as.numeric(str_extract(duration, "(\\d)+")),
         dur_num = ifelse(is.na(dur_num),0,dur_num),
         dur_unit = str_extract(duration, "day|hour|minute|round"),
         dur_unit = ifelse(is.na(dur_unit),"instant",dur_unit)) %>% 
  mutate(ID = name)

#convert boolean data to numeric data
#Total number of spells is 314. To make more manageable select only those that are used by three or more character types
boolean_data <-data %>% 
  select_if(., .predicate = is.logical) %>% 
  mutate_if(is.logical, as.numeric) %>% 
  rowwise() %>% 
  mutate(total_chars = bard+cleric+druid+paladin+ranger+sorcerer+warlock+wizard) %>% 
  cbind(data[,"ID"],.) %>% 
  filter(total_chars > 3)

f_data<-data %>% 
  filter(ID %in% boolean_data$ID)

#Filter to by boolean IDs to ensure only those spells are included
#Unnest tokens, remove stopwords, count words by spell(ID)
tidy_text <- f_data%>% 
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word") %>%    
  count(ID, word, sort = TRUE)             

# Convert to Document-Term Matrix
dtm <- tidy_text %>%
  cast_dtm(document = ID, term = word, value = n)

# Convert DTM into a matrix
dtm_matrix <- as.matrix(dtm)

# One-hot encode the factor features on filtered data
factor_dummies <-model.matrix(~ range_type + dur_unit + school - 1, data = f_data)

# Combine numeric features, text matrix, and factor dummies
combined_data <- cbind(
  f_data %>% select(level, range, dur_num),
  dtm_matrix,
  factor_dummies                                
) 

# Scale the combined data
combined_data_scaled <- scale(combined_data)

# Compute distance matrix
distance_matrix <- dist(combined_data_scaled, method = "euclidean")

# Perform hierarchical clustering using median
hclust_result <- hclust(distance_matrix, method = "ward.D2") 

# Convert to dendrogram object
dend <- as.dendrogram(hclust_result)

#extract the data from the dendrogram into 
data <- dendro_data(dend, type = "rectangle")

#modify the labels dataframe to include schools - use the filtered data
#this will enable coloring the labels by school, which is not part of the original dend data.
data$labels <- data$labels %>% left_join(f_data %>% select(name, school), by = c("label" = "name"))

#key_glyph allows you to change the key symbol! New information for me.
ggspells <- ggplot(segment(data)) + 
  geom_segment(aes(x = x, 
                   y = y, 
                   xend = xend, 
                   yend = yend)) + 
  geom_text(data=label(data), 
            aes(x = x, 
                y=y, 
                label=label, 
                hjust=0, 
                color = school), 
            size=3,angle = 90, 
            key_glyph = draw_key_point) +
  scale_y_reverse(expand = c(0.3, 0))+
  scale_color_paletteer_d("feathers::cassowary")+
  theme_dendro()+
  labs(title = "Hierarchical Clusterering of Dungeon & Dragons Spells (Mixed Data)", 
       caption = "data source: https://www.dndbeyond.com/sources/dnd/free-rules/spell-descriptions")+
  coord_radial(rotate_angle = TRUE, 
               expand = TRUE,
               start = 1, 
               end = (pi*2.1)+1,
               clip = "off")+
  theme(plot.margin = unit(c(.25, 0, 0, 0), "line"),
        legend.position = "right",
        legend.text = element_text(size = 10), 
        legend.key.height = unit(0.25, "cm"),
        plot.title.position =  "plot")+
  guides(color = guide_legend(byrow = TRUE))
        
ggsave("2024_51_dnd_revised.png", dpi = 300, type = "cairo")
