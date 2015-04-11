library(stringi)
library(data.table)
library(doParallel)

load("train.phrases.RData")
load("unigram.df.RData")

#text1 <- "I really love this place, really. Live in New York is one os the best things I have done. Kisses to  everyone, ok?"
#text2 <- "New yOrk is the best place to reside!"
#text3 <- "Anyone that lives in new york herE? I cant find a good restaurant in new york"
#train.phrases <- c(text1,text2,text3)
train.phrases <- tolower(train.phrases)
#train.phrases <- unlist(stri_split_regex(train.phrases,pattern="[\\.\\?!]", simplify = F, omit_empty = T))

#only uniograms seem at least 3 times
targets <- unigram.df$Term[unigram.df$Freq > 2]
rm(unigram.df)
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


system.time(reside.prob <- sapply(targets,patternFreq,pattern=reside.pattern))

rm(train.phrases)
#reside.prob <- data.table(word = names(reside.prob) ,prob = reside.prob, key = "word")


nodes <- detectCores()
cl <- makeCluster(nodes)
registerDoParallel(cl)

reside.prob <- foreach(target=iter(targets),.combine=c) %dopar% {
  patternFreq(target,reside.pattern)
}

stopCluster(cl)

gc()

reside.prob <- data.table(word = targets ,prob = reside.prob, key = "word")



#Sanity check
length(targets) == nrow(reside.prob)

save(reside.prob, file = "reside.prob.RData")


#------------------------------------------------------------------------------