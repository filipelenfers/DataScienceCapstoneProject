# Stupid backoff

# some part of the N-grams are dominated by stopwords

library(tm)

load("unigram.df.RData")
load("bigram.df.clean.RData")
load("trigram.df.clean.RData")
load("tetragram.df.clean.RData")


#Stupid backoff 4-gram------------------------------------------------------------------------------
# http://www.aclweb.org/anthology/D07-1090.pdf

##Parameters
input <- "Very early observations on the Bills game: Offense still struggling but the"
num.results <- 3
alpha = 0.4

#Clean input
input <- tolower(input) 
input <- removePunctuation(input)
input <- removeNumbers(input)
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
trigram.subset$score <- trigram.subset$prob * alpha
trigram.predictions <- head(trigram.subset[order(trigram.subset$score, decreasing = T),],n = num.results)
trigram.predictions

#2-gram case, 0.4 * 0.4 (for this 4-gram model) , just the top num.results
bigram.lines <- grepl(paste0("^",bigram.key,"$"),bigram.df.clean$key)
bigram.subset <- bigram.df.clean[bigram.lines,]
bigram.subset$score <- bigram.subset$prob * (alpha ^ 2)
bigram.predictions <- head(bigram.subset[order(bigram.subset$score, decreasing = T),],n = num.results)
bigram.predictions

#Unigram case, 0.4 * 0.4 * 0.4 (for this 4-gram model) , just the top num.results
unigram.df$score <- unigram.df$prob * (alpha ^ 3)
unigram.predictions <- head(unigram.df[order(unigram.df$score, decreasing = T),],n = num.results)
unigram.predictions

spPredictions <- rbind(tetragram.predictions,trigram.predictions,bigram.predictions,unigram.predictions)


#------------------------------------------------------------------------------