library(stringi)

load("train.sample.RData")

train.phrases <- unlist(stri_split_regex(train.sample,pattern="[\\.\\?!]", simplify = F, omit_empty = T))

save(train.phrases,file = "train.phrases.RData")