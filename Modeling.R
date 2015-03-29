options( java.parameters = "-Xmx4g" )

library(tm)
library(RWeka)
library(dplyr)
library(doParallel)


nodes <- detectCores()
cl <- makeCluster(nodes)
registerDoParallel(cl)

source("baseFunctions.R")

blog.file <- "data/en_US/en_US.blogs.txt"
blogs <- readLines(blog.file, encoding="UTF-8")
blogs <- sample(blogs,length(blogs)*0.01)

twitter.file <- "data/en_US/en_US.twitter.txt"
tweets <- readLines(twitter.file, encoding="UTF-8")

news.file <- "data/en_US/en_US.news.txt"
news <- readLines(news.file, encoding="UTF-8")

all <- c(blogs,tweets,news)

rm(blogs,tweets,news)

#sample.size <- trunc(length(blogs)*0.01)
#sample.blogs <- sample(blogs,sample.size)
#rm(blogs)

train.sample.idx <- sample.int(length(all), length(all)*0.80)
train.sample <- all[train.sample.idx]
test.sample <- all[-train.sample.idx]
rm(all,train.sample.idx)
save(train.sample,file = "train.sample.RData")
save(test.sample,file = "test.sample.RData")


#Create clean corpus-------------------------------------------
train.corpus <- createCorpus(train.sample)
rm(train.sample)
save(train.corpus,file = "train.corpus.RData")


test.corpus <- createCorpus(test.sample)
rm(test.sample)
save(test.corpus,file = "test.corpus.RData")
#dummy.corpus <- createCorpus(c("<s> I am Sam </s>",
#                               "<s> Sam I am </s>",
#                               "<s> I do not like green eggs and ham </s>"),clean = F)
#------------------------------------------------------


#Generate N-grams-------------
unigram.df <- generateNgramDf(train.corpus,1)
save(unigram.df,file = "unigram.df.RData")

bigram.df <- generateNgramDf(train.corpus,2)
save(bigram.df,file = "bigram.df.RData")

trigram.df <- generateNgramDf(train.corpus,3)
save(trigram.df,file = "trigram.df.RData")

tetragram.df <- generateNgramDf(train.corpus,4)
save(tetragram.df,file = "tetragram.df.RData")

rm(train.corpus)


test.unigram.df <- generateNgramDf(test.corpus,1)
save(test.unigram.df,file = "test.unigram.df.RData")

test.bigram.df <- generateNgramDf(test.corpus,2)
save(test.bigram.df,file = "test.bigram.df.RData")

test.trigram.df <- generateNgramDf(test.corpus,3)
save(test.trigram.df,file = "test.trigram.df.RData")

test.tetragram.df <- generateNgramDf(test.corpus,4)
save(test.tetragram.df,file = "test.tetragram.df.RData")

rm(test.corpus)
#dummy.bigram.df <- generateNgramDf(dummy.corpus,2)
#-------------------------------------------------------------------------------------------

#Generate N-gram probabilities---------------------------------------
unigram.df <- generateNgramProb(unigram.df,1)
save(unigram.df,file = "unigram.df.RData")

bigram.df <- generateNgramProb(bigram.df,2)
save(bigram.df,file = "bigram.df.RData")

trigram.df <- generateNgramProb(trigram.df,3)
save(trigram.df,file = "trigram.df.RData")

tetragram.df <- generateNgramProb(tetragram.df,4)
save(tetragram.df,file = "tetragram.df.RData")


#dummy.bigram.df <- generateNgramProb(dummy.bigram.df,2)
#------------------------------------------------------------------------------
print("Done!")

#load("unigram.df.RData")
#load("bigram.df.clean.RData")
#load("trigram.df.clean.RData")
#load("tetragram.df.clean.RData")
#trigram.df.clean <- trigram.df[trigram.df.$Freq > 1,]

#Stupid backoff 3-gram------------------------------------------------------------------------------
# http://www.aclweb.org/anthology/D07-1090.pdf

##Parameters
input <- "Go on a romantic date at the"
num.results <- 30
alpha = 0.4

#Clean input
input <- tolower(input) 
input <- removePunctuation(input)
input <- removeNumbers(input)
#input <- removeWords(input, stopwords("english"))
input <- stripWhitespace(input)

#Get keys
input.words <- strsplit(input," ")[[1]]
input.words.size <- length(input.words)
tetragram.key <- ifelse(input.words.size >= 3, paste(input.words[(input.words.size-2):input.words.size],collapse = " "), NA)
trigram.key <- ifelse(input.words.size >= 2, paste(input.words[(input.words.size-1):input.words.size],collapse = " "), NA)
bigram.key <- input.words[input.words.size]
print(trigram.key)
print(bigram.key)

#3-gram case, 1.0 (for this 3-gram model) , just the top num.results
trigram.lines <- grepl(paste0("^",trigram.key,"$"),trigram.df.clean$key)
sum(trigram.lines)
trigram.subset <- trigram.df.clean[trigram.lines,]
trigram.subset$score <- trigram.subset$prob
trigram.predictions <- head(trigram.subset[order(trigram.subset$score, decreasing = T),],n = num.results)
trigram.predictions

#2-gram case, 0.4 (for this 3-gram model) , just the top num.results
bigram.lines <- grepl(paste0("^",bigram.key,"$"),bigram.df.blogs$key)
bigram.subset <- bigram.df.blogs[bigram.lines,]
bigram.subset$score <- bigram.subset$prob * 0.4
bigram.predictions <- head(bigram.subset[order(bigram.subset$score, decreasing = T),],n = num.results)

#Unigram case, 0.4 * 0.4 (for this 3-gram model) , just the top num.results
unigram.df.blogs$score <- unigram.df.blogs$prob * 0.16
unigram.predictions <- head(unigram.df.blogs[order(unigram.df.blogs$score, decreasing = T),],n = num.results)


#------------------------------------------------------------------------------


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
#input <- removeWords(input, stopwords("english"))
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
bigram.predictions <- head(bigram.subset[order(bigram.subset$score, decreasing = T),],n = num.results)

#Unigram case, 0.4 * 0.4 * 0.4 (for this 4-gram model) , just the top num.results
unigram.df.blogs$score <- unigram.df.blogs$prob * 0.064
unigram.predictions <- head(unigram.df.blogs[order(unigram.df.blogs$score, decreasing = T),],n = num.results)

#------------------------------------------------------------------------------


#Calculate perplexidade --------------------
#using http://en.wikipedia.org/wiki/Perplexity, Perplexity of a probability model
# https://courses.engr.illinois.edu/cs498jh/HW/hw1.pdf

test.bigram.prob <- (left_join(test.bigram.df.blogs,bigram.df.blogs,by="Term") %>% select(prob))$prob
test.bigram.prob[is.na(test.bigram.prob)] <- 1/5000 #TODO fazer smotthing dos NA usando stupid backoff para quando eu tiver vários grams
perplexity(test.bigram.prob)
summary(test.bigram.prob)

test.trigram.prob <- (left_join(test.trigram.df.blogs,trigram.df.blogs,by="Term") %>% select(prob))$prob
test.trigram.prob[is.na(test.trigram.prob)] <- 1/5000 #TODO fazer smotthing dos NA usando stupid backoff para quando eu tiver vários grams
perplexity(test.trigram.prob)
summary(test.trigram.prob)

test.tetragram.prob <- (left_join(test.tetragram.df.blogs,tetragram.df.blogs,by="Term") %>% select(prob))$prob
test.tetragram.prob[is.na(test.tetragram.prob)] <- 1/5000 #TODO fazer smotthing dos NA usando stupid backoff para quando eu tiver vários grams
perplexity(test.tetragram.prob)
summary(test.tetragram.prob)

dummy.prob <- (left_join(dummy.bigram.df,dummy.bigram.df,by="Term") %>% select(prob.x))$prob.x
perplexity(dummy.prob)
#Do stupid backoff

#--------------------------------------------------------------------------------

#Generate 2-gram test key-target-----------------------------------------------
test.bigram.targets <- sapply(strsplit(test.bigram.df.blogs$Term, ' '), function(a) a[2])

test.bigram.df.blogs <- data.frame(test.bigram.df.blogs,target = test.bigram.targets, stringsAsFactors = F)
rm(test.bigram.targets)

test.bigram.keys <- sapply(strsplit(test.bigram.df.blogs$Term, ' '), function(a) a[1])

test.bigram.df.blogs <- data.frame(test.bigram.df.blogs,key = test.bigram.keys, stringsAsFactors = F)
rm(test.bigram.keys)
#------------------------------------------------------------------------------

#Calculate accuracy --------------------
 

#----------------------------------------



#TODO pesquisa rápida, fastmatch? hash?

#TODO probabilidades de N-Gram, para poder fazer o backoff fazer algusn
#Prunning de Ngram abaixo de um certo X

#TODO trainar em um conjunto, testar em outro

#TODO avaliar via perplexidade e accuracy no conjunto de teste

#TODO Interpolation, melhor que backoff
#TODO stupid back off works weel
#https://github.com/yufree/nlpshiny/blob/master/models.R

#TODO unknow words special token