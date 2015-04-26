Next Word Predictor
========================================================
author: Filipe Pais Lenfers
A presentation for the Coursera/JHU Data Science Capstone Project.

Introduction
========================================================

The Next Word Predictor is an web application that given a sentence can predict the next word.

Predict next word for a sentence can be used in devices where is difficult to type, like cellphones, to show the user auto complete options of next possible words. So the user can type the text more fast and with less type errors.

In this presentation the following topics will be presented:
- The algorithm used
- How to use the app
- Next steps

Algorithm
========================================================

- [N-grams](http://en.wikipedia.org/wiki/N-gram) models are used to map the most frequent sequences of words. N-grams from 1-gram to 4-gram were used in the app. All the data to process the n-grams was obtained from sources of news, blogs and twitters.
- The algorithm used is [Stupid Backoff](http://www.aclweb.org/anthology/D07-1090.pdf), this algorithm is very simple and fast, but its quality on prediction approach some of the state of art algorithms (like Kneser-Ney Smoothing) when its trained on large data sets. More information about this algorithm can be found here: [http://www.aclweb.org/anthology/D07-1090.pdf](http://www.aclweb.org/anthology/D07-1090.pdf).
    - Basically the algorithm try to use the 4-grams to predict the next word, if less then 3 unique words are found it back-off to the 3-gram and so on, until it completes 3 unique words for the prediction.
    - When nothing is found we use the most frequent words of the unigram.

How to use?
========================================================

You can access the app here: [https://filipelenfers.shinyapps.io/CapstoneShiny/](https://filipelenfers.shinyapps.io/CapstoneShiny/)

Just fill the box below "Input the sentence:" and click on submit. The 3 predictions will be show on the right side, ordered by relevance.

![alt text](figures/appScreenshoot2.png)

Conclusion & Next Steps
========================================================

The app can predict correctly some cases proving the possibility to predict words and help the users do create text faster. This app was made in very short time and adjusted to run in [ShinyApps.io](ShinyApps.io) free tier. On more hardware capabilities more information could be used, an with more time some other techniques can be tested.

Some improvements that can be made in this app:

- Use context information, for example if there are words that denote this sentence is about sports (like a name of a game, or of a well know player or stadium) some words/n-grams will have more probability than others due the context.
- Use user information: monitor the user behavior and adjust the probabilities based on his/her use.  
