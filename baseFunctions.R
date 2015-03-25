library(dplyr)

perplexity <- function(prob) {
  N <- length(prob)
  p <- (-1/N) * sum(log2(test.bigram.prob))
  return(2 ^ p)
}

generateNgramProb <- function(data,N) {
  data.targets <- sapply(strsplit(data$Term, ' '), function(a) a[N])
  
  data <- data.frame(data,target = data.targets, stringsAsFactors = F)
  rm(data.targets)
  
  data.keys <- sapply(strsplit(data$Term, ' '), function(a) paste(a[1:N-1], collapse = " ") )
  
  data <- data.frame(data,key = data.keys, stringsAsFactors = F)
  rm(data.keys)
  
  data.sum <- data %>%
    group_by(key) %>%
    summarise(total=sum(Freq))
  
  data <- inner_join(data,data.sum,by="key") %>% 
    mutate(prob = Freq/total)
  rm(data.sum) 
  
  return(data)
}

generateNgramDf <- function(corpus,N) {
  tdm <- TermDocumentMatrix(corpus, 
                            control = list(
                              tokenize = function(x) NGramTokenizer(x, Weka_control(min = N, max = N)) 
                            )) 
  df <- data.frame(Term = tdm$dimnames$Terms, Freq = tdm$v, stringsAsFactors = F) 
  #test.trigram.df.blogs$Term <- as.character(test.trigram.df.blogs$Term)
  return(df)
}