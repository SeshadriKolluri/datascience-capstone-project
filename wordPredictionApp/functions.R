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

getPredictions <- function(str){
  #tokens <- tokens(x = char_tolower(str))
  #tokens <- char_wordstem(rev(rev(tokens[[1]])[1:3]), language = "english")
  
  tokens <- strsplit(tolower(str), "\\s+")
  
  tokens <- rev(rev(tokens[[1]])[1:3])
  
  print(tokens)
  
  if(all(is.na(tokens))){
    next_words <- c(paste('Waiting', 'for', 'your', 'input', '...'))
  }else{
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
  }
  #print(next_words)
  next_words
}

top_words <- freq_table_1grams$word_1[1:50]
