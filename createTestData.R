load("test.sample.RData")

library(doParallel)
library(tm)

nodes <- detectCores() - 1
cl <- makeCluster(nodes)
registerDoParallel(cl)

phraseToTestPattern <- function(phrase) {
  words <- unlist(strsplit(phrase," "))
  target <- removePunctuation(words[length(words)])
  input <- paste(words[1:length(words)-1], collapse = " ")
  c(input,target)
}


#For each item, everything as input, and las word as taget.
parallelFunction <- function() {
  system.time(outForeach <- foreach(i=1:length(test.sample),.combine = rbind, .inorder = F, .export = c("removePunctuation"), .multicombine = T  ) %dopar% {
    phraseToTestPattern(test.sample[i])  
  })
}
#Sapply version, the winner!
system.time({
  outSapply <- sapply(test.sample,phraseToTestPattern,USE.NAMES = F) 
  outSapply <- t(outSapply)
}  )


stopCluster(cl)

save(outSapply,file = "outSapply.RData")
save(outForeach,file = "outForeach.RData")