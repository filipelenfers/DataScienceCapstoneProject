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

createCorpus
train.sample.blogs <- iconv(train.sample.blogs, "latin1", "ASCII", sub="")
test.sample.blogs <- iconv(train.sample.blogs, "latin1", "ASCII", sub="")


#Create clean corpus-------------------------------------------
train.corpus.blogs <- createCorpus(train.sample.blogs)
test.corpus.blogs <- createCorpus(test.sample.blogs)
#------------------------------------------------------


#Generate N-grams-------------
unigram.termdocmatrix.blogs <- generateNgramDf(train.corpus.blogs,1)
bigram.df.blogs <- generateNgramDf(train.corpus.blogs,2)
trigram.df.blogs <- generateNgramDf(train.corpus.blogs,3)
tetragram.df.blogs <- generateNgramDf(train.corpus.blogs,4)

test.bigram.df.blogs <- generateNgramDf(test.corpus.blogs,2)
test.trigram.df.blogs <- generateNgramDf(test.corpus.blogs,3)
test.tetragram.df.blogs <- generateNgramDf(test.corpus.blogs,4)
#-------------------------------------------------------------------------------------------

#Generate N-gram probabilities---------------------------------------
bigram.df.blogs <- generateNgramProb(bigram.df.blogs,2)
trigram.df.blogs <- generateNgramProb(trigram.df.blogs,3)
tetragram.df.blogs <- generateNgramProb(tetragram.df.blogs,4)
#------------------------------------------------------------------------------


#Calculate perplexidade --------------------
#using http://en.wikipedia.org/wiki/Perplexity, Perplexity of a probability model
# https://courses.engr.illinois.edu/cs498jh/HW/hw1.pdf

test.bigram.prob <- (left_join(test.bigram.df.blogs,bigram.df.blogs,by="Term") %>% select(prob))$prob
test.bigram.prob[is.na(test.bigram.prob)] <- 1/500000 #TODO fazer smotthing dos NA usando stupid backoff para quando eu tiver vários grams
perplexity(test.bigram.prob)

test.trigram.prob <- (left_join(test.trigram.df.blogs,trigram.df.blogs,by="Term") %>% select(prob))$prob
test.trigram.prob[is.na(test.trigram.prob)] <- 1/500000 #TODO fazer smotthing dos NA usando stupid backoff para quando eu tiver vários grams
perplexity(test.trigram.prob)

test.tetragram.prob <- (left_join(test.tetragram.df.blogs,tetragram.df.blogs,by="Term") %>% select(prob))$prob
test.tetragram.prob[is.na(test.tetragram.prob)] <- 1/500000 #TODO fazer smotthing dos NA usando stupid backoff para quando eu tiver vários grams
perplexity(test.tetragram.prob)
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