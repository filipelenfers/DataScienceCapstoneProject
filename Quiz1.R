#Q1 The en_US.blogs.txt file is how many megabytes?
file.info("data/en_US/en_US.blogs.txt")$size/1024/1024 # 200.4242 mb

#Q2 The en_US.twitter.txt has how many lines of text?
twitter <- readLines("data/en_US/en_US.twitter.txt", encoding="UTF-8")
length(twitter) #2360148

#Q3 What is the length of the longest line seen in any of the three en_US data sets?
blog.file <- "data/en_US/en_US.blogs.txt"
#twitter.file <- "data/en_US/en_US.twitter.txt"
news.file <- "data/en_US/en_US.news.txt"
blogs <- readLines(blog.file, encoding="UTF-8")
#twitter <- readLines(twitter.file, encoding="UTF-8")
news  <- readLines(news.file, encoding="UTF-8")
max(nchar(blogs)) #40833
max(nchar(twitter)) #140
max(nchar(news)) #11384

#Q4 In the en_US twitter data set, if you divide the number of lines where the word "love" (all lowercase) occurs by the number of lines the word "hate" (all lowercase) occurs, about what do you get?
love <- grepl("love",twitter)
hate <- grepl("hate",twitter)
sum(love)/sum(hate)

#Q5 The one tweet in the en_US twitter data set that matches the word "biostats" says what?
twitter[grepl("biostats",twitter)]

#Q6 How many tweets have the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing". (I.e. the line matches those characters exactly.)
twitter[grepl("A computer once beat me at chess, but it was no match for me at kickboxing",twitter)]
