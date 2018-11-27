#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    load("unigram_table_trim.RData")
    load("bigram_table_trim.RData")
    load("trigram_table_trim.RData")
    load("tetragram_table_trim.RData")   
    default_suggestion <- gsub(x=unigram_table_trim[1:5,"ngrams"],pattern= " ", replacement = "")
    # default_suggestion <- sapply(unigram_table_trim[1:5,"ngrams"], word)
    
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
    
    
  output$numberOfWords <- renderText({
      input$requestedPredictions
  })
  
  output$predictedWords <- renderPrint({
    inputtedText <- input$predictionText
    inputtedText <- gsub("[[:punct:]]", "", x = inputtedText)
    inputtedText <- gsub("\\s+", " ",  x = inputtedText)
    print(getnextword(inputtedText))
  })

  # })
  
})
