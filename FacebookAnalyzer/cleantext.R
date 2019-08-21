clean_text <- function(text){
  text <- text
  cleaned <- tm::stripWhitespace(x = text)
  cleaned <- tm::removePunctuation(x = cleaned)
  clean <- removeWords(x = cleaned,words = stopwords(kind = 'en'))
  clean
  return(clean)
  
  
}