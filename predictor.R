suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(stringr))
suppressMessages(library(tidytext))

#Load ngram data that was previously generated
bigram <- readRDS("./bigram.RDS")
trigram <- readRDS("./trigram.RDS")
quadgram <- readRDS("./quadgram.RDS")

bigramMatch <- function(userInput){
  len <- length(userInput)
  filter(bigram, one == userInput[len]) %>% 
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(two) %>%
    as.character() -> prediction
  ifelse(prediction =="character(0)", "No Prediction Found", return(prediction))
}

trigramMatch <- function(userInput){
  len <- length(userInput)
  filter(trigram, one==userInput[len-1], two==userInput[len]) %>% 
    top_n(1, n) %>%
    filter(row_number() == 1) %>%
    select(three) %>%
    as.character() -> prediction
  ifelse(prediction=="character(0)", bigramMatch(userInput), return(prediction))
}

quadgramMatch <- function(userInput){
  len <- length(userInput)
  filter(quadgram, one == userInput[len-2], two == userInput[len-1], three == userInput[len])  %>% 
    top_n(1, n) %>%
    filter(row_number() == 1) %>%
    select(four) %>%
    as.character() -> prediction
  
  ifelse(prediction=="character(0)", trigramMatch(userInput), return(prediction))
}

#prediction
ngrams <- function(userInput){
  userInput <- data_frame(text = userInput)
  
  # Clean the Input
  userInput <- userInput %>%
    mutate(text = str_replace_all(text, "[^[:alpha:][:space:]]*", "")) %>%
    mutate(text = iconv(text, "ASCII//TRANSLIT"))
  wordCnt <- str_count(userInput, boundary("word"))
  userInput <- unlist(str_split(userInput, boundary("word")))
  userInput <- tolower(userInput)
  
  #Use ngram matching, if blank, have awaiting input
  prediction <- ifelse(wordCnt >= 3, quadgramMatch(userInput),
                        ifelse(wordCnt == 2, trigramMatch(userInput), 
                               ifelse (wordCnt == 1, bigramMatch(userInput), "Awaiting Input" )))
  return(prediction)
}
