source("benchmark.R")

################################################################################################
#
# 03. Define the wrapper function to be called by benchmark
#
################################################################################################

# As an example, we create a very simple baseline algorithm which always returns
# the three most frequent English words.
predict.baseline <- function(x){c('the', 'on', 'a')}



################################################################################################
#
# 04. Perform the benchmark
#
################################################################################################
benchmark(predict.baseline, 
          # additional parameters to be passed to the prediction function can be inserted here
          sent.list = list('quizzes' = quizzes, 
                           'tweets' = tweets, 
                           'blogs' = blogs), 
          ext.output = T)