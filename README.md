Text Prediction App
========================================================
author: yagnesh0
date: 2/6/18
autosize: true

Text Prediction App
========================================================

Overview:
- A tool that looks up to the last 3 words entered and returns 5 words
- Always returns 5 unique words
- Super duper fast!

https://yagnesh0.shinyapps.io/TextPredictionNGram/

Algorithm
========================================================
Shifting through roughly 70K tweets, news articles, and blogs, this model collected
unigrams, bigrams, trigrams, and tetragrams.

From there it assigns probabilites based on its frequency, and after trimming the lowest
occurring n-grams from each list, it came down to a possible list of:
- 78K Unigrams
- 530K Bigrams
- 506K Trigrams
- 189K Tetragrams

Based on how many words you provided, it'll go through the various list of n-grams and will return what it believes, with the highest probabilities, the next word may be.


What makes it special?
========================================================
- Probability adjustment to rebalance the different levels of n-grams to give more a reasonable ordering of suggestions


```r
#'n' here refers to n-gram level, aka for bi-grams, n = 2
ngram_table_trim$adj_prob = log10(ngram_table_trim$prop) + n
```


- Always returns 5 unique suggestions

- No clutter on the UI


How to use it?
========================================================
On the application: https://yagnesh0.shinyapps.io/TextPredictionNGram/ please insert
your text into the appropriate text box.

After completing your phrase or word, it'll look up to the last 3 words and give a fair
suggestion on what your next word will be!

You can even try to make a whole sentence by just continously choosing one of the 5 words presented.

Reply back with your most interesting sentence, I got "donald trump said that he was going through his life and then you can see what i am doing"

Future Steps
========================================================
While this model has shown some early potential, there is always room to grow!
Possible next steps that would help improve this model would be:
- Adding in more documents to find more n-grams
- Continue to improve the probability balancing algorithm to move more interesting and accurate suggestions up the list
- Add in a topic-modeling feature that will look at all the current words of the entered in text to help find more pertinent word suggestions

Thank you and hope you enjoy the application!
