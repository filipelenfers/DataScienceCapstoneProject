

perplexity <- function(prob) {
  N <- length(prob)
  p <- (-1/N) * sum(log2(test.bigram.prob))
  return(2 ^ p)
}