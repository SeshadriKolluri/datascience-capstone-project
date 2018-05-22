require(readtext)
require(quanteda)
require(data.table)
require(dplyr)

zipped_data<-'Coursera-SwiftKey.zip'

#Check if the file already exists, and download if it doesn't exist already

if(!file.exists(zipped_data)){
  download.file(destfile = trainingfile,method='curl',
                url='https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip')}
if(!file.exists('./final')){
  unzip(zipped_data)
}

data_directgory <- './final/en_US'
system2("rm", paste0(data_directgory,"/*sampled*.txt"))
file_names <- list.files(data_directgory)

sapply(file_names, function(x) system2("cat",paste0(data_directgory,"/",x," | perl -n -e 'print if (rand() < .2)' >",data_directgory,"/",x,"_sampled.txt")))

sampled_corpus <- corpus(readtext(paste0(data_directgory,"/*sampled*.txt")))

summary(sampled_corpus)

#Cleanup the corpus (Pre-process)

clean_tokens <- tokens(x = sampled_corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
       remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE)

profanity.file <- "profanity.txt"
if(!file.exists(profanity.file)){
  download.file(destfile = profanity.file,method='curl',
                url='https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en')}

profanity <- readLines(profanity.file)

clean_tokens <- tokens_remove(clean_tokens,profanity)

stemmed_tokens <- tokens_wordstem(x=clean_tokens, language = "english")
stemmed_tokens <- tokens_remove(stemmed_tokens,profanity)
rm(clean_tokens)

bigrams <- tokens_ngrams(x = stemmed_tokens, n = 2)
dfm_1grams <- dfm(stemmed_tokens)
dfm_2grams <- dfm(bigrams)
remove(bigrams)

trigrams <- tokens_ngrams(x = stemmed_tokens, n = 3)
dfm_3grams <- dfm(trigrams)
remove(trigrams)

quadgrams <- tokens_ngrams(x = stemmed_tokens, n = 4)
dfm_4grams <- dfm(quadgrams)
remove(quadgrams)

dfm_1grams <- dfm_trim(dfm_1grams, min_count = 4)
dfm_2grams <- dfm_trim(dfm_2grams, min_count = 4)
dfm_3grams <- dfm_trim(dfm_3grams, min_count = 4)
dfm_4grams <- dfm_trim(dfm_4grams, min_count = 3)

freq_table_1grams <- as.data.table(colSums(dfm_1grams),keep.rownames = TRUE) %>% `colnames<-`(c("ngram", "count"))
freq_table_2grams <- as.data.table(colSums(dfm_2grams),keep.rownames = TRUE) %>% `colnames<-`(c("ngram", "count"))
freq_table_3grams <- as.data.table(colSums(dfm_3grams),keep.rownames = TRUE) %>% `colnames<-`(c("ngram", "count"))
freq_table_4grams <- as.data.table(colSums(dfm_4grams),keep.rownames = TRUE) %>% `colnames<-`(c("ngram", "count"))

freq_table_1grams$word_1 <- freq_table_1grams$ngram
freq_table_2grams <- cbind(freq_table_2grams,str_split_fixed(freq_table_2grams$ngram,'_',2) %>% `colnames<-`(c("word_1", "word_2")))
freq_table_3grams <- cbind(freq_table_3grams,str_split_fixed(freq_table_3grams$ngram,'_',3) %>% `colnames<-`(c("word_1", "word_2", "word_3")))
freq_table_4grams <- cbind(freq_table_4grams,str_split_fixed(freq_table_4grams$ngram,'_',4) %>% `colnames<-`(c("word_1", "word_2", "word_3", "word_4")))

freq_table_1grams[freq_table_1grams == ''] <- NA
freq_table_2grams[freq_table_2grams == ''] <- NA
freq_table_3grams[freq_table_3grams == ''] <- NA
freq_table_4grams[freq_table_4grams == ''] <- NA

freq_table_1grams <- freq_table_1grams[complete.cases(freq_table_1grams),]
freq_table_2grams <- freq_table_2grams[complete.cases(freq_table_2grams),]
freq_table_3grams <- freq_table_3grams[complete.cases(freq_table_3grams),]
freq_table_4grams <- freq_table_4grams[complete.cases(freq_table_4grams),]

tmp <- aggregate(count ~ word_1, freq_table_2grams, FUN = sum)
names(tmp)[names(tmp) == 'count'] <- 'minus_1_gram_count'
freq_table_2grams <- left_join(freq_table_2grams, tmp, by = 'word_1')
freq_table_2grams$probabilities <- freq_table_2grams$count/freq_table_2grams$minus_1_gram_count

tmp <- aggregate(count ~ word_1 + word_2, freq_table_3grams, FUN = sum)
names(tmp)[names(tmp) == 'count'] <- 'minus_1_gram_count'
freq_table_3grams <- left_join(freq_table_3grams, tmp, by = c('word_1', 'word_2'))
freq_table_3grams$probabilities <- freq_table_3grams$count/freq_table_3grams$minus_1_gram_count

tmp <- aggregate(count ~ word_1 + word_2 + word_3, freq_table_4grams, FUN = sum)
names(tmp)[names(tmp) == 'count'] <- 'minus_1_gram_count'
freq_table_4grams <- left_join(freq_table_4grams, tmp, by = c('word_1', 'word_2', 'word_3'))
freq_table_4grams$probabilities <- freq_table_4grams$count/freq_table_4grams$minus_1_gram_count

predict4thWord <- function(w1, w2, w3, num_predictions = 10){
  predictions <- subset(freq_table_4grams, word_1 == w1 & word_2 == w2 & word_3 == w3)
  if(nrow(predictions) == 0){
    next_words = NA
  } else{
    next_words = predictions$word_4[1:min(num_predictions,nrow(predictions))]
  }
  next_words
}

predict3rdWord <- function(w1, w2, num_predictions = 10){
  predictions <- subset(freq_table_3grams, word_1 == w1 & word_2 == w2)
  if(nrow(predictions) == 0){
    next_words = NA
  } else{
    next_words = predictions$word_3[1:min(num_predictions,nrow(predictions))]
  }
  next_words
}

predict2ndWord <- function(w1, num_predictions = 10){
  predictions <- subset(freq_table_2grams, word_1 == w1)
  if(nrow(predictions) == 0){
    next_words = NA
  } else{
    next_words = predictions$word_2[1:min(num_predictions,nrow(predictions))]
  }
  next_words
}

top_words = freq_table_1grams$word_1[1:50]

getPredictions <- function(str){
  tokens <- tokens(x = char_tolower(str))
  tokens <- char_wordstem(rev(rev(tokens[[1]])[1:3]), language = "english")

  next_words <- predict4thWord(tokens[1], tokens[2], tokens[3])
  if(any(is.na(next_words))){
    next_words <- predict3rdWord(tokens[2], tokens[3])
  }
  if(any(is.na(next_words))){
    next_words <- predict2ndWord(tokens[3])
  } 
  if(any(is.na(next_words))){
    next_words <- sample(top_words,5)
  }
  next_words
}

# Now some test cases
getPredictions("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some")

getPredcitions("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")

write.csv(freq_table_4grams,file = 'freq_table_4grams.csv',row.names = FALSE,quote = FALSE)
write.csv(freq_table_3grams,file = 'freq_table_3grams.csv',row.names = FALSE,quote = FALSE)
write.csv(freq_table_2grams,file = 'freq_table_2grams.csv',row.names = FALSE,quote = FALSE)
write.csv(freq_table_1grams,file = 'freq_table_1grams.csv',row.names = FALSE,quote = FALSE)

saveRDS(freq_table_1grams,'freq_table_1grams.RDS')
saveRDS(freq_table_2grams,'freq_table_2grams.RDS')
saveRDS(freq_table_3grams,'freq_table_3grams.RDS')
saveRDS(freq_table_4grams,'freq_table_4grams.RDS')
