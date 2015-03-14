library(stringi)
library(tm)
library(RWeka)

blogs <- readLines("data/en_US/en_US.blogs.txt", encoding="UTF-8")
twitter <- readLines("data/en_US/en_US.twitter.txt", encoding="UTF-8")
news  <- readLines("data/en_US/en_US.news.txt", encoding="UTF-8")

sample_blogs <- sample(blogs,100000)
sample_twitter <- sample(twitter,100000)
sample_news <- sample(news,100000)

rm(blogs,twitter,news)

# dirSource <- DirSource(directory='data/en_us',
#                        encoding='utf-8')
# corpus <- Corpus(dirSource)
#corpus <- Corpus(VectorSource(list(sample_blogs,sample_twitter,sample_news)))
corpus <- Corpus(VectorSource(list(sample_blogs)))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removePunctuation))
corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
#corpus <- tm_map(corpus, removeSparseTerms,0.8)

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
termdocmatrix <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))

#head(data.frame(inspect(termdocmatrix)),100)
trigram.df <- data.frame(termdocmatrix$dimnames$Terms,termdocmatrix$v)


#UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))

#options(mc.cores=4)

#tdm1gram <- TermDocumentMatrix(corpus, control = list(tokenize = UnigramTokenizer))



