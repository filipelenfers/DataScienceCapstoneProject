options( java.parameters = "-Xmx4g" )

library(stringi)
library(tm)
library(RWeka)
library(ggplot2)
library(dplyr)

source("baseFunctions.R")

blog.file <- "data/en_US/en_US.blogs.txt"
blogs <- readLines(blog.file, encoding="UTF-8")
sample.size <- trunc(length(blogs)*0.02)
sample.blogs <- sample(blogs,sample.size)
rm(blogs)

train.sample.idx <- sample.int(length(sample.blogs), length(sample.blogs)*0.80)
train.sample.blogs <- sample.blogs[train.sample.idx]
test.sample.blogs <- sample(sample.blogs[-train.sample.idx])

#Create clean corpus-------------------------------------------
train.corpus.blogs <- createCorpus(train.sample.blogs)
test.corpus.blogs <- createCorpus(test.sample.blogs)
dummy.corpus <- createCorpus(c("<s> I am Sam </s>",
                               "<s> Sam I am </s>",
                               "<s> I do not like green eggs and ham </s>"),clean = F)
#------------------------------------------------------


#Generate N-grams-------------
unigram.df.blogs <- generateNgramDf(train.corpus.blogs,1)
bigram.df.blogs <- generateNgramDf(train.corpus.blogs,2)
trigram.df.blogs <- generateNgramDf(train.corpus.blogs,3)
tetragram.df.blogs <- generateNgramDf(train.corpus.blogs,4)

test.bigram.df.blogs <- generateNgramDf(test.corpus.blogs,2)
test.trigram.df.blogs <- generateNgramDf(test.corpus.blogs,3)
test.tetragram.df.blogs <- generateNgramDf(test.corpus.blogs,4)

dummy.bigram.df <- generateNgramDf(dummy.corpus,2)

#-------------------------------------------------------------------------------------------

#Generate N-gram probabilities---------------------------------------
unigram.df.blogs <- generateNgramProb(unigram.df.blogs,1)
bigram.df.blogs <- generateNgramProb(bigram.df.blogs,2)
trigram.df.blogs <- generateNgramProb(trigram.df.blogs,3)
tetragram.df.blogs <- generateNgramProb(tetragram.df.blogs,4)

dummy.bigram.df <- generateNgramProb(dummy.bigram.df,2)
#------------------------------------------------------------------------------


#Stupid backoff 3-gram------------------------------------------------------------------------------
# http://www.aclweb.org/anthology/D07-1090.pdf

##Parameters
input <- "I love to live in New york"
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

#3-gram case, 1.0 (for this 3-gram model) , just the top num.results
trigram.lines <- grepl(paste0("^",trigram.key,"$"),trigram.df.blogs$key)
trigram.subset <- trigram.df.blogs[trigram.lines,]
trigram.subset$score <- trigram.subset$prob * 0.4
trigram.predictions <- head(trigram.subset[order(trigram.subset$score, decreasing = T),],n = num.results)


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