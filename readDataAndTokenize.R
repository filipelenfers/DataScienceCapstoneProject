library(stringi)
library(tm)

blogs <- readLines("data/en_US/en_US.blogs.txt", encoding="UTF-8")
twitter <- readLines("data/en_US/en_US.twitter.txt", encoding="UTF-8")
news  <- readLines("data/en_US/en_US.news.txt", encoding="UTF-8")

sample_blogs <- sample(blogs,1000)
sample_twitter <- sample(twitter,1000)
sample_news <- sample(news,1000)

rm(blogs,twitter,news)

# dirSource <- DirSource(directory='data/en_us',
#                        encoding='utf-8')
# corpus <- Corpus(dirSource)
corpus <- Corpus(VectorSource(list(sample_blogs,sample_twitter,sample_news)))

corpus <- tm_map(corpus, content_transformer(tolower))

#UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))

#options(mc.cores=4)

#tdm1gram <- TermDocumentMatrix(corpus, control = list(tokenize = UnigramTokenizer))



