##Packages and Environment
library(tm)
library(dplyr)
library(ngram)
library(stringr)
set.seed(1000)


##Read data
twitter_conn <- file("~/datasciencecoursera/Capstone/final/en_US/en_US.twitter.txt", "r") 
blogs_conn <- file("~/datasciencecoursera/Capstone/final/en_US/en_US.blogs.txt", "r") 
news_conn <- file("~/datasciencecoursera/Capstone/final/en_US/en_US.news.txt", "r") 

twitter_data <- readLines(twitter_conn)
close(twitter_conn)

blogs_data <- readLines(blogs_conn)
close(blogs_conn)

news_data <- readLines(news_conn)
close(news_conn)

##Sampling cause this junk too big
twitter_sample <- sample(twitter_data, size = as.integer(.1*length(twitter_data)), replace = FALSE)
blogs_sample <- sample(blogs_data, size = as.integer(.1*length(twitter_data)), replace = FALSE)
news_sample <- sample(news_data, size = as.integer(.1*length(twitter_data)), replace = FALSE)

text_sample <- c(twitter_sample, blogs_sample, news_sample)

morenaughtywords <- c("bullshit")
naughtylist <- readLines("bad_words.txt")
naughtylist <- c(naughtylist, morenaughtywords)
load(file="foreignChars.RData")


text_corpus <- Corpus(VectorSource(text_sample)) %>%
    tm_map(function(x) tolower(x)) %>%
    tm_map(function(x) removeWords(x,naughtylist)) %>%
    tm_map(function(x) removeWords(x,foreignChars)) %>%
    tm_map(function(x) gsub(x,pattern = "([a-zA-z]+[0-9]+([a-zA-z]+?|.?)|[0-9]+([a-zA-z]+)|[0-9]+)", replacement = "")) %>%
    tm_map(function(x) gsub(x,pattern="’", replacement = "'")) %>%
    tm_map(function(x) gsub(x,pattern="é", replacement = "e")) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)

# save(text_corpus, file="textCorpusTM.RData")
load(file="textCorpusTM.RData")
str <- concatenate(lapply(text_corpus, "[", 1))

ng1 <- ngram(str, n=1)
ng2 <- ngram(str, n=2)
ng3 <- ngram(str, n=3)
ng4 <- ngram(str, n=4)

save(ng1, file="ngobjects/ng1.RData")
save(ng2, file="ngobjects/ng2.RData")
save(ng3, file="ngobjects/ng3.RData")
save(ng4, file="ngobjects/ng4.RData")

print(ng4, output="truncated")


load("gramlists/unigram_list_ngrampkg.RData")
load("gramlists/bigram_list_ngrampkg.RData")
load("gramlists/trigram_list_ngrampkg.RData")
load("gramlists/tetragram_list_ngrampkg.RData")
# 
# unigram_table <- get.phrasetable(ng = ng1)
# bigram_table <- get.phrasetable(ng = ng2)
# trigram_table <- get.phrasetable(ng = ng3)
# tetragram_table <- get.phrasetable(ng = ng4)
# 
# save(unigram_table_trim, file="gramlists/trim_unigram_list_ngrampkg.RData")
# save(bigram_table_trim, file="gramlists/trim_bigram_list_ngrampkg.RData")
# save(trigram_table_trim, file="gramlists/trim_trigram_list_ngrampkg.RData")
# save(tetragram_table_trim, file="gramlists/trim_tetragram_list_ngrampkg.RData")

unigram_table[grep(x = unigram_table$ngrams, pattern = "^hi"),]

unigram_table_trim = unigram_table[unigram_table$freq>=4,]
bigram_table_trim = bigram_table[bigram_table$freq>=4,]
trigram_table_trim = trigram_table[trigram_table$freq>=4,]
tetragram_table_trim = tetragram_table[tetragram_table$freq>=4,]

## HUEHUEHUEHUHEUHE IT WORKS
unigram_table_trim$smooth_prop = log10(unigram_table_trim$prop*10)
bigram_table_trim$smooth_prop = log10(bigram_table_trim$prop*100)
trigram_table_trim$smooth_prop = log10(trigram_table_trim$prop*1000)
tetragram_table_trim$smooth_prop = log10(tetragram_table_trim$prop*10000)

load("gramlists/trim_unigram_list_ngrampkg.RData")
load("gramlists/trim_bigram_list_ngrampkg.RData")
load("gramlists/trim_trigram_list_ngrampkg.RData")
load("gramlists/trim_tetragram_list_ngrampkg.RData")

default_suggestion <- unigram_table_trim[1:5,"ngrams"]
default_suggestion <- sapply(unigram_table_trim[1:5,"ngrams"], word)
getnextword <- function(insertstr){
    i = 0
    insertstr = tolower(insertstr)
    insertstr <- gsub(x= insertstr, pattern = "'", replacement = "")
    suggestion <- list()
    splitstring <- strsplit(x=insertstr, split=" ")[[1]]
    strlength <- length(splitstring)
    
    if(strlength == 0){
        suggestion <- default_suggestion
        return(suggestion)
    }
    
    else if(strlength == 1){
        bigram_split <- paste(splitstring[length(splitstring)], collapse = "", sep = "")
        bigram_suggestion <- head(bigram_table_trim[grep(x=bigram_table_trim$ngrams, pattern=paste0('^',bigram_split," ")),],5)
        if(nrow(bigram_suggestion) != 0){
            suggestions_with_lead <- bigram_suggestion$ngrams
            suggestion <- sapply(X = suggestions_with_lead,FUN = word, start = -2 )
            suggestion <- unique(suggestion)
            suggestion <- suggestion[1:min(5,length(suggestion))]
            while(length(suggestion) < 5){
                suggestion = c(suggestion,default_suggestion[i])
                suggestion = unique(suggestion)
                i = i +  1
            }
            return(suggestion)
        }
        else{
            suggestion <- default_suggestion
            return(suggestion)
        }
    }
    
    else if(strlength == 2){
        bigram_split <- paste(splitstring[length(splitstring)], collapse = "", sep = "")
        trigram_split <- paste(splitstring[max(1,(length(splitstring)-1)):length(splitstring)], collapse = " ", sep = "")
        
        bigram_suggestion <- head(bigram_table_trim[grep(x=bigram_table_trim$ngrams, pattern=paste0('^',bigram_split," ")),],5)
        trigram_suggestion <- head(trigram_table_trim[grep(x=trigram_table_trim$ngrams, pattern=paste0('^',trigram_split," ")),],4)
        
        combined_suggestions <- rbind(trigram_suggestion,bigram_suggestion)
        
        if(nrow(combined_suggestions) != 0){
            combined_suggestions <- combined_suggestions[order(combined_suggestions$smooth_prop,decreasing = T),]
            combined_suggestions <- sapply(X = combined_suggestions$ngrams,FUN = word, start = -2 )
            combined_suggestions <- unique(combined_suggestions)
            combined_suggestions <- combined_suggestions[1:min(5,length(combined_suggestions))]
            while(length(combined_suggestions) < 5){
                combined_suggestions = c(combined_suggestions,default_suggestion[i])
                combined_suggestions = unique(combined_suggestions)
                i = i +  1
            }
            
            return(combined_suggestions)
        }
        else{
            suggestion <- default_suggestion
            return(suggestion)
        }
    }
    
    else{
        
        bigram_split <- paste(splitstring[length(splitstring)], collapse = "", sep = "")
        trigram_split <- paste(splitstring[max(1,(length(splitstring)-1)):length(splitstring)], collapse = " ", sep = "")
        tetragram_split <- paste(splitstring[max(1,(length(splitstring)-2)):length(splitstring)], collapse = " ", sep = "")
        
        bigram_suggestion <- head(bigram_table_trim[grep(x=bigram_table_trim$ngrams, pattern=paste0('^',bigram_split," ")),],5)
        trigram_suggestion <- head(trigram_table_trim[grep(x=trigram_table_trim$ngrams, pattern=paste0('^',trigram_split," ")),],4)
        tetragram_suggestion <- head(tetragram_table_trim[grep(x=tetragram_table_trim$ngrams, pattern=paste0('^',tetragram_split," ")),],3)
        
        combined_suggestions <- rbind(tetragram_suggestion,trigram_suggestion,bigram_suggestion)
        
        if(nrow(combined_suggestions) != 0){
            combined_suggestions <- combined_suggestions[order(combined_suggestions$smooth_prop,decreasing = T),]
            combined_suggestions <- sapply(X = combined_suggestions$ngrams,FUN = word, start = -2 )
            combined_suggestions <- unique(combined_suggestions)
            combined_suggestions <- combined_suggestions[1:min(5,length(combined_suggestions))]
            while(length(combined_suggestions) < 5){
                combined_suggestions = c(combined_suggestions,default_suggestion[i])
                combined_suggestions = unique(combined_suggestions)
                i = i +  1
            }
            
            return(combined_suggestions)
        }
        else{
            suggestion <- default_suggestion
            return(suggestion)
        }
    }
    
}

getnextword("")
getnextword("alphabet")
getnextword("napa")
getnextword("roses are red")
