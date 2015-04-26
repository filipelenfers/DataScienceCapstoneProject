library(stringi)
library(data.table)
library(doParallel)

load("train.phrases.RData")
train.phrases <- tolower(train.phrases)
#load("unigram.df.RData")
load("spData4.RData")



#text1 <- "I really love this place, really. Live in New York is one os the best things I have done. Kisses to  everyone, ok?"
#text2 <- "New yOrk is the best place to reside!"
#text3 <- "Anyone that lives in new york herE? I cant find a good restaurant in new york"
#train.phrases <- c(text1,text2,text3)
#train.phrases <- tolower(train.phrases)
#train.phrases <- unlist(stri_split_regex(train.phrases,pattern="[\\.\\?!]", simplify = F, omit_empty = T))

#only uniograms seem at least 3 times
#targets <- unigram.df$Term[unigram.df$Freq > 10]
#rm(unigram.df)
targets <- unique(c(bigram.data.table$target,tetragram.data.table$target,trigram.data.table$target,unigram.predictions.cache.data.table$target))
rm(tetragram.data.table)
rm(bigram.data.table)
rm(trigram.data.table)
rm(unigram.predictions.cache.data.table)
gc()

#reside group
reside.dictionary <- c("\\sreside", "\\slive", "\\sinhabit")
reside.pattern <- sprintf("%s",paste(reside.dictionary,collapse="|"))



patternFreq <- function(target,pattern) {
  #Acessing train.phrases on enviroment to evict copys of this large data
  phrases.with.target <- train.phrases[stri_detect_fixed(train.phrases,target)]
  if(length(phrases.with.target) == 0){
    0.0
  } else {
    sum(stri_detect_regex(phrases.with.target,pattern))/length(phrases.with.target)
  }  
}

#target <- "york"
#pattern <- reside.pattern


#system.time(reside.prob <- sapply(targets[1:100],patternFreq,pattern=reside.pattern))

#rm(train.phrases)
#reside.prob <- data.table(word = names(reside.prob) ,prob = reside.prob, key = "word")


nodes <- detectCores()-1
cl <- makeCluster(nodes)
registerDoParallel(cl)

reside.prob <- foreach(target=iter(targets),.combine=c, .packages=c("stringi","data.table")) %dopar% {
  patternFreq(target,reside.pattern)
}

stopCluster(cl)

gc()

reside.prob <- data.table(word = targets ,prob = reside.prob, key = "word")



#Sanity check
length(targets) == nrow(reside.prob)

save(reside.prob, file = "reside.prob.RData")


#------------------------------------------------------------------------------