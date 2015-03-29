# Stupid backoff without the stops words

load("unigram.df.remstop.RData")
load("bigram.df.clean.remstop.RData")
load("trigram.df.clean.remstop.RData")
load("tetragram.df.clean.remstop.RData")


#Stupid backoff 4-gram------------------------------------------------------------------------------
# http://www.aclweb.org/anthology/D07-1090.pdf

##Parameters
input <- "If this isn't the cutest thing you've ever seen, then you must be"
num.results <- 30
alpha = 0.4

#Clean input
input <- tolower(input) 
input <- removePunctuation(input)
input <- removeNumbers(input)
input <- removeWords(input, stopwords("english"))
input <- stripWhitespace(input)

#Get keys
input.words <- strsplit(input," ")[[1]]
input.words.size <- length(input.words)
tetragram.key <- ifelse(input.words.size >= 3, paste(input.words[(input.words.size-2):input.words.size],collapse = " "), NA)
trigram.key <- ifelse(input.words.size >= 2, paste(input.words[(input.words.size-1):input.words.size],collapse = " "), NA)
bigram.key <- input.words[input.words.size]
print(tetragram.key)
print(trigram.key)
print(bigram.key)

#4-gram case, 1.0 (for this 4-gram model) , just the top num.results
tetragram.lines <- grepl(paste0("^",tetragram.key,"$"),tetragram.df.clean$key)
sum(tetragram.lines)
tetragram.subset <- tetragram.df.clean[tetragram.lines,]
tetragram.subset$score <- tetragram.subset$prob
tetragram.predictions <- head(tetragram.subset[order(tetragram.subset$score, decreasing = T),],n = num.results)
tetragram.predictions

#3-gram case, 0.4 (for this 4-gram model) , just the top num.results
trigram.lines <- grepl(paste0("^",trigram.key,"$"),trigram.df.clean$key)
sum(trigram.lines)
trigram.subset <- trigram.df.clean[trigram.lines,]
trigram.subset$score <- trigram.subset$prob * 0.4
trigram.predictions <- head(trigram.subset[order(trigram.subset$score, decreasing = T),],n = num.results)
trigram.predictions

#2-gram case, 0.4 * 0.4 (for this 4-gram model) , just the top num.results
bigram.lines <- grepl(paste0("^",bigram.key,"$"),bigram.df.blogs$key)
bigram.subset <- bigram.df.blogs[bigram.lines,]
bigram.subset$score <- bigram.subset$prob * 0.16
trigram.predictions <- head(bigram.subset[order(bigram.subset$score, decreasing = T),],n = num.results)
trigram.predictions

#Unigram case, 0.4 * 0.4 * 0.4 (for this 4-gram model) , just the top num.results
unigram.df.blogs$score <- unigram.df.blogs$prob * 0.064
unigram.predictions <- head(unigram.df.blogs[order(unigram.df.blogs$score, decreasing = T),],n = num.results)
unigram.predictions

#------------------------------------------------------------------------------