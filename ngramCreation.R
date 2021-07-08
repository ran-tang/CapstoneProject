setwd("C:/Users/Valor/Desktop/Data Science/Capstone")
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(stringr))
suppressMessages(library(tidytext))


#setup
furl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
                 
if(!file.exists("./Coursera-SwiftKey.zip")){
  download.file(furl,destfile="./Coursera-SwiftKey.zip",mode = "wb")
  unzip("Coursera-SwiftKey.zip")
}                                  
          
#import data                 
blogs   <- readLines("./final/en_US/en_US.blogs.txt", skipNul = TRUE)
news    <- readLines("./final/en_US/en_US.news.txt",  skipNul = TRUE)
twitter <- readLines("./final/en_US/en_US.twitter.txt" , skipNul = TRUE)

blogs   <- data_frame(text = blogs)
news    <- data_frame(text = news)
twitter <- data_frame(text = twitter)                 

# Sampling
set.seed(123)
blogssample <- blogs %>%
  sample_n(., nrow(blogs)* 0.1, replace = FALSE)
newssample <- news %>%
  sample_n(., nrow(news)* 0.1, replace = FALSE)
twitsample <- twitter %>%
  sample_n(., nrow(twitter)* 0.1, replace = FALSE)

# Combine into preliminary Training Set
trainingSet <- rbind(blogssample, newssample, twitsample)

rm(list = c("blogs", "news", "twitter", "newssample", "blogssample","twitsample"))

#Cleaning Data
cleanTraining <-  trainingSet %>%
  mutate(text = str_replace_all(text, "[^[:alpha:][:space:]]*", "")) %>%
  mutate(text = str_replace_all(text, "http[^[:space:]]*", "")) %>%
  mutate(text = iconv(text, "ASCII//TRANSLIT"))

fullSampleText <- unnest_tokens(cleanTraining, word, text)

#ngram creation and filtering for most common iterations
bigrams <- unnest_tokens(cleanTraining, bigram, text, token = "ngrams", n = 2)
mostCommonBigrams <- arrange(filter(count(bigrams, bigram), n >= 10), desc(n))
bigram <- separate(data= mostCommonBigrams, bigram, c("one", "two"), sep = " ")

trigrams <- unnest_tokens(cleanTraining, trigram, text, token = "ngrams", n = 3)
mostCommonTrigrams <- arrange(filter(count(trigrams, trigram), n >= 7), desc(n))
trigram <- separate(data= mostCommonTrigrams, trigram, c("one", "two", "three"), sep = " ")

quadgrams <- unnest_tokens(cleanTraining, quadgram, text, token = "ngrams", n = 4)
mostCommonQuadgrams <- arrange(filter(count(quadgrams, quadgram), n >= 5), desc(n))
quadgram <- separate(data= mostCommonQuadgrams, quadgram, c("one", "two", "three", "four"), sep = " ")
  
rm(list = c("bigrams", "trigrams", "quadgrams", "mostCommonBigrams", "mostCommonTrigrams", "mostCommonQuadgrams"))

#save files
saveRDS(cleanTraining, "./cleanTraining.RDS")
saveRDS(fullSampleText, "./fullSampleText.RDS")
saveRDS(bigram, "./bigram.RDS")
saveRDS(trigram, "./trigram.RDS")
saveRDS(quadgram, "./quadgram.RDS")


