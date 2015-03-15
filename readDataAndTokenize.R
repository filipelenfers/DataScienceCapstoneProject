library(stringi)
library(tm)
library(RWeka)

blogs <- readLines("data/en_US/en_US.blogs.txt", encoding="UTF-8")
twitter <- readLines("data/en_US/en_US.twitter.txt", encoding="UTF-8")
news  <- readLines("data/en_US/en_US.news.txt", encoding="UTF-8")

profanity.words <- readLines("en_profanity_words.txt")

#sample_blogs <- sample(blogs,100000)
#sample_twitter <- sample(twitter,100000)
#sample_news <- sample(news,100000)
load("samples.RData")

#rm(blogs,twitter,news)

sample_blogs <- iconv(sample_blogs, "latin1", "ASCII", sub="")
sample_twitter <- iconv(sample_twitter, "latin1", "ASCII", sub="")
sample_news <- iconv(sample_twitter, "latin1", "ASCII", sub="")

# dirSource <- DirSource(directory='data/en_us',
#                        encoding='utf-8')
# corpus <- Corpus(dirSource)
#corpus <- Corpus(VectorSource(list(sample_blogs,sample_twitter,sample_news)))
corpus <- Corpus(VectorSource(list(sample.blogs,sample.twitter)))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removePunctuation))
corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, profanity.words)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument, language='english')
#corpus <- tm_map(corpus, removeSparseTerms,0.9999)

UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
unigram.termdocmatrix <- TermDocumentMatrix(corpus[1], control = list(tokenize = UnigramTokenizer))
unigram.df <- data.frame(Term = unigram.termdocmatrix$dimnames$Terms, Freq = unigram.termdocmatrix$v)
sum(row_sums(unigram.termdocmatrix)) == sum(unigram.df$Freq) #Sanity check

head(sort(row_sums(unigram.termdocmatrix), decreasing = T))
head(unigram.df[order(unigram.df$Freq,decreasing = T),])



TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
trigram.termdocmatrix <- TermDocumentMatrix(corpus[1], control = list(tokenize = TrigramTokenizer))
#termdocmatrix <- removeSparseTerms(termdocmatrix,0.999)

#head(data.frame(inspect(termdocmatrix)),100)
trigram.df <- data.frame(termdocmatrix$dimnames$Terms,termdocmatrix$v)
head(trigram.df,200)
#head(data.frame(inspect(termdocmatrix)),200)
head(trigram.df[order(trigram.df$termdocmatrix.v,decreasing = T),],100)
as.character(trigram.df$termdocmatrix.dimnames.Terms[1])


#UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))

#options(mc.cores=4)

#tdm1gram <- TermDocumentMatrix(corpus, control = list(tokenize = UnigramTokenizer))



