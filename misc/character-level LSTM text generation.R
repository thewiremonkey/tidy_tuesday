# install_keras()
library(keras)
library(stringr)
setwd("./week_15")

library(tidyverse)
library(readxl)
library(tidytext)

beers <- read_excel("beers.xlsx", sheet = "beers") %>%
  rename_all(.funs = funs(paste0("beer_", .))) %>%
  left_join(read_excel("beers.xlsx", sheet="breweries"),by=c("beer_brewery_id"="id"))

bitterness<-beers %>% filter(!is.na(beer_ibu))

styles<-beers %>%
  group_by(beer_style) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  ungroup() %>%
  top_n(11) %>%
  left_join(beers %>% select(beer_style, beer_name)) %>%
  select(-count)

text_beer<-styles %>%
 group_by(beer_style) %>%
 summarise(beer_text=paste0(tolower(beer_name), collapse=" ")) %>%
 mutate(char_len=nchar(beer_text))




# # #download the corpus and convert to lowercase:
# path <- get_file(
#   "nietzsche.txt",
#   origin = "https://s3.amazonaws.com/text-datasets/nietzsche.txt"
# )
#

# text <- tolower(readChar(path, file.info(path)$size))
# cat("Corpus length:", nchar(text), "\n")
# text<-paste0(tolower(bitterness$beer_name), collapse=" ")
cat("Corpus lenth:", nchar(text),"\n")


# #Next, you'll extract partially overlapping sequences of length maxlen, one-hot encode them, and pack them in a 3D array x of shape (sequences, maxlen, unique_characters). Simultaneously, you'll prepare an array y containing the corresponding targets: the one-hot-encoded characters that come after each extracted sequence.
# #
maxlen <- 50  # Length of extracted character sequences
#
step <- 3  # We sample a new sequence every `step` characters
#
# text_indexes <- seq(from = 1, to = nchar(text) - maxlen, by = step)

text_beer$text_indexes<-lapply(X = text_beer$beer_text, function(x)seq(from=1, to=nchar(x)-maxlen, by=step))
# This holds our extracted sequences
# sentences <- str_sub(text, text_indexes, text_indexes + maxlen - 1)
text_beer$sentences<-lapply(X=text_beer$beer_text, function(x)str_sub(x, text_indexes, text_indexes + maxlen -1))

# This holds the targets (the follow-up characters)
next_chars <- str_sub(text, text_indexes + maxlen, text_indexes + maxlen)
text_beer$next_chars<-lapply(text_beer$beer_text, function(x)str_sub(x, text_indexes + maxlen, text_indexes + maxlen))

cat("Number of sequences: ", length(sentences), "\n")

# List of unique characters in the corpus
chars <- unique(sort(strsplit(text, "")[[1]]))
cat("Unique characters:", length(chars), "\n")

# Dictionary mapping unique characters to their index in `chars`
char_indices <- 1:length(chars)
names(char_indices) <- chars

# Next, one-hot encode the characters into binary arrays.
cat("Vectorization...\n")
x <- array(0L, dim = c(length(sentences), maxlen, length(chars)))
y <- array(0L, dim = c(length(sentences), length(chars)))



for (i in 1:length(sentences)) {
  sentence <- strsplit(sentences[[i]], "")[[1]]
  for (t in 1:length(sentence)) {

    char <- sentence[[t]]
    x[i, t, char_indices[[char]]] <- 1
  }
  next_char <- next_chars[[i]]
  y[i, char_indices[[next_char]]] <- 1
}

#This network is a single LSTM layer followed by a dense classifier and softmax over all possible characters. But note that recurrent neural networks aren't the only way to do sequence data generation; 1D convnets also have proven extremely successful at this task in recent times.
model <- keras_model_sequential() %>%
  layer_lstm(units = 128, input_shape = c(maxlen, length(chars))) %>%
  layer_dense(units = length(chars), activation = "softmax")

# Since our targets are one-hot encoded, we will use categorical_crossentropy as the loss to train the model:
optimizer <- optimizer_rmsprop(lr = 0.01)

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer
)
#Given a trained model and a seed text snippet, we generate new text by repeatedly:

## Drawing from the model a probability distribution over the next character given the text available so far
## Reweighting the distribution to a certain "temperature"
## Sampling the next character at random according to the reweighted distribution
## Adding the new character at the end of the available text
## This is the code we use to reweight the original probability distribution coming out of the model, and draw a character index from it (the "sampling function"):
sample_next_char <- function(preds, temperature = 1.0) {
  preds <- as.numeric(preds)
  preds <- log(preds) / temperature
  exp_preds <- exp(preds)
  preds <- exp_preds / sum(exp_preds)
  which.max(t(rmultinom(1, 1, preds)))
}

new_beers<-list()
## Finally, the following loop repeatedly trains and generates text. You begin generating text using a range of different temperatures after every epoch. This allows you to see how the generated text evolves as the model begins to converge, as well as the impact of temperature in the sampling strategy.
for (epoch in 1:50) {

  # cat("epoch", epoch, "\n")

  # Fit the model for 1 epoch on the available training data
  model %>% fit(x, y, batch_size = 128, epochs = 1)

  # Select a text seed at random
  start_index <- sample(1:(nchar(text) - maxlen - 1), 1)
  seed_text <- str_sub(text, start_index, start_index + maxlen - 1)

  # cat("--- Generating with seed:", seed_text, "\n\n")

  # for (temperature in c(0.2, 0.5, 1.0, 1.2)) {
  for (temperature in c(0.2)) {

    # cat("------ temperature:", temperature, "\n")
    # cat(seed_text, "\n")

    generated_text <- seed_text

    # We generate 400 characters
    # for (i in 1:400) {
      for (i in 1:30) {
        sampled <- array(0, dim = c(1, maxlen, length(chars)))
        generated_chars <- strsplit(generated_text, "")[[1]]
        for (t in 1:length(generated_chars)) {
          char <- generated_chars[[t]]
          sampled[1, t, char_indices[[char]]] <- 1
          }
        preds <- model %>% predict(sampled, verbose = 0)
        next_index <- sample_next_char(preds[1,], temperature)
        next_char <- chars[[next_index]]

      generated_text <- paste0(generated_text, next_char)
      generated_text <- substring(generated_text, 2)


      cat(next_char)


    }
    cat("\n\n")
  }

}
