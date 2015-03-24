options( java.parameters = "-Xmx4g" )

library(stringi)
library(tm)
library(RWeka)
library(ggplot2)
library(dplyr)

blog.file <- "data/en_US/en_US.blogs.txt"
blogs <- readLines(blog.file, encoding="UTF-8")
sample.size <- trunc(length(blogs)*0.02)
sample.blogs <- sample(blogs,sample.size)
rm(blogs)

train.sample.idx <- sample.int(length(sample.blogs), length(sample.blogs)*0.80)
train.sample.blogs <- sample.blogs[train.sample.idx]
test.sample.blogs <- sample(sample.blogs[-train.sample.idx])

train.sample.blogs <- iconv(train.sample.blogs, "latin1", "ASCII", sub="")
test.sample.blogs <- iconv(train.sample.blogs, "latin1", "ASCII", sub="")


#Clean data-------------------------------------------
profanity.words <- readLines("en_profanity_words.txt")

train.corpus.blogs <- Corpus(VectorSource(list(train.sample.blogs)))
train.corpus.blogs <- tm_map(train.corpus.blogs, content_transformer(tolower))
train.corpus.blogs <- tm_map(train.corpus.blogs, content_transformer(removePunctuation))
train.corpus.blogs <- tm_map(train.corpus.blogs, content_transformer(removeNumbers))
#train.corpus.blogs <- tm_map(train.corpus.blogs, removeWords, stopwords("english"))
#train.corpus.blogs <- tm_map(train.corpus.blogs, removeWords, profanity.words)
train.corpus.blogs <- tm_map(train.corpus.blogs, stripWhitespace)
#train.corpus.blogs <- tm_map(train.corpus.blogs, stemDocument, language='english')

test.corpus.blogs <- Corpus(VectorSource(list(test.sample.blogs)))
test.corpus.blogs <- tm_map(test.corpus.blogs, content_transformer(tolower))
test.corpus.blogs <- tm_map(test.corpus.blogs, content_transformer(removePunctuation))
test.corpus.blogs <- tm_map(test.corpus.blogs, content_transformer(removeNumbers))
#test.corpus.blogs <- tm_map(test.corpus.blogs, removeWords, stopwords("english"))
#test.corpus.blogs <- tm_map(test.corpus.blogs, removeWords, profanity.words)
test.corpus.blogs <- tm_map(test.corpus.blogs, stripWhitespace)
#test.corpus.blogs <- tm_map(test.corpus.blogs, stemDocument, language='english')
#------------------------------------------------------


#Generate N-grams-------------
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1)) 
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2)) 
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3)) 
TetragramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4)) 

unigram.termdocmatrix.blogs <- TermDocumentMatrix(train.corpus.blogs, control = list(tokenize = UnigramTokenizer)) 
unigram.df.blogs <- data.frame(Term = unigram.termdocmatrix.blogs$dimnames$Terms, 
                              Freq = unigram.termdocmatrix.blogs$v) 
unigram.df.blogs <- unigram.df.blogs[order(unigram.df.blogs$Freq,decreasing = T),] 
rm(unigram.termdocmatrix.blogs)

bigram.termdocmatrix.blogs <- TermDocumentMatrix(train.corpus.blogs, control = list(tokenize = BigramTokenizer)) 
bigram.df.blogs <- data.frame(Term = bigram.termdocmatrix.blogs$dimnames$Terms, 
                              Freq = bigram.termdocmatrix.blogs$v) 
bigram.df.blogs <- bigram.df.blogs[order(bigram.df.blogs$Freq,decreasing = T),] 
rm(bigram.termdocmatrix.blogs)
bigram.df.blogs$Term <- as.character(bigram.df.blogs$Term)

trigram.termdocmatrix.blogs <- TermDocumentMatrix(train.corpus.blogs, control = list(tokenize = TrigramTokenizer)) 
trigram.df.blogs <- data.frame(Term = trigram.termdocmatrix.blogs$dimnames$Terms, 
                              Freq = trigram.termdocmatrix.blogs$v) 
trigram.df.blogs <- trigram.df.blogs[order(trigram.df.blogs$Freq,decreasing = T),] 
rm(trigram.termdocmatrix.blogs)
trigram.df.blogs$Term <- as.character(trigram.df.blogs$Term)

tetragram.termdocmatrix.blogs <- TermDocumentMatrix(train.corpus.blogs, control = list(tokenize = TetragramTokenizer)) 
tetragram.df.blogs <- data.frame(Term = tetragram.termdocmatrix.blogs$dimnames$Terms, 
                               Freq = tetragram.termdocmatrix.blogs$v) 
tetragram.df.blogs <- tetragram.df.blogs[order(tetragram.df.blogs$Freq,decreasing = T),] 
rm(tetragram.termdocmatrix.blogs)
tetragram.df.blogs$Term <- as.character(tetragram.df.blogs$Term)


test.bigram.termdocmatrix.blogs <- TermDocumentMatrix(test.corpus.blogs, control = list(tokenize = BigramTokenizer)) 
test.bigram.df.blogs <- data.frame(Term = test.bigram.termdocmatrix.blogs$dimnames$Terms) 
rm(test.bigram.termdocmatrix.blogs)
test.bigram.df.blogs$Term <- as.character(test.bigram.df.blogs$Term)
#-------------------------------------------------------------------------------------------

#Generate 2-gram probabilities---------------------------------------
bigram.targets <- sapply(strsplit(bigram.df.blogs$Term, ' '), function(a) a[2])

bigram.df.blogs <- data.frame(bigram.df.blogs,target = bigram.targets)
rm(bigram.targets)

bigram.keys <- sapply(strsplit(bigram.df.blogs$Term, ' '), function(a) a[1])

bigram.df.blogs <- data.frame(bigram.df.blogs,key = bigram.keys)
rm(bigram.keys)

bigram.key.sum <- bigram.df.blogs %>%
  group_by(key) %>%
  summarise(total=sum(Freq))

bigram.df.blogs <- inner_join(bigram.df.blogs,bigram.key.sum,by="key") %>% 
  mutate(prob = Freq/total)
rm(bigram.key.sum) 
#------------------------------------------------------------------------------

#Calculate perplexidade --------------------
#using http://en.wikipedia.org/wiki/Perplexity, Perplexity of a probability model
# https://courses.engr.illinois.edu/cs498jh/HW/hw1.pdf

test.bigram.prob <- (left_join(test.bigram.df.blogs,bigram.df.blogs,by="Term") %>% select(prob))$prob
test.bigram.prob[is.na(test.bigram.prob)] <- 1/500000
#TODO fazer smotthing dos NA usando stupid backoff para quando eu tiver vários grams

perplexity(test.bigram.prob)
#--------------------------------------------------------------------------------




trigram.targets <- foreach( d = iter(as.character(trigram.df.blogs$Term)), .combine = "c") %dopar%
  strsplit(d," ")[[1]][3]

trigram.df.blogs <- data.frame(trigram.df.blogs,target = trigram.targets)
rm(bigram.targets)

trigram.keys <- foreach( d = iter(as.character(trigram.df.blogs$Term)), .combine = "c") %dopar% {
  temp <- strsplit(d," ")[[1]]
  paste[temp[1],temp[2])
}
  

trigram.df.blogs <- data.frame(trigram.df.blogs,key = trigram.keys)
rm(trigram.keys)




tetragram.targets <- foreach( d = iter(as.character(tretragram.df.blogs$Term)), .combine = "c") %dopar%
  strsplit(d," ")[[1]][4]

#TODO pesquisa rápida, fastmatch? hash?

#TODO probabilidades de N-Gram, para poder fazer o backoff fazer algusn
#Prunning de Ngram abaixo de um certo X

#TODO trainar em um conjunto, testar em outro

#TODO avaliar via perplexidade e accuracy no conjunto de teste

#TODO Interpolation, melhor que backoff
#TODO stupid back off works weel
#https://github.com/yufree/nlpshiny/blob/master/models.R

#TODO unknow words special token