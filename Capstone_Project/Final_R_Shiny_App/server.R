#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tm)
library(dplyr)
library(stringi)
library(stringr)
library(shiny)

word_frequencies_pruned <- readRDS('data/wfp.Rda')
bigram_frequencies_pruned <- readRDS('data/bifp.Rda')
trigram_frequencies_pruned <- readRDS('data/trifp.Rda')
tetragram_frequencies_pruned <- readRDS('data/tetfp.Rda')


bigram_kbo <- bigram_frequencies_pruned
bigram_kbo$kbo_frequency <- bigram_kbo$frequency - 0.5
bi_sum <- sum(bigram_kbo$frequency)

trigram_kbo <- trigram_frequencies_pruned
trigram_kbo$kbo_frequency <- trigram_kbo$frequency - 0.5
tri_sum <- sum(trigram_kbo$frequency)

tetragram_kbo <- tetragram_frequencies_pruned
tetragram_kbo$kbo_frequency <- tetragram_kbo$frequency - 0.5
tetra_sum <- sum(tetragram_kbo$frequency)




sbo <- function(sentence, n) {
  if (n == 1) {
    b <- word_frequencies_pruned
    colnames(b)[1] <- "last_word"
    b_sum <- sum(b$frequency)
    b <- b[!(b$last_word %in% recom$last_word),]
    b <- b[1:(min(length(b$last_word), (5-length(recom[[1]])))), ]
    b$probability <- b$frequency*((0.4*0.4*0.4)/b_sum)
    
    recom <<- rbind(recom, b)
    recom <- recom[order(recom$probability, decreasing = TRUE), ]
    rownames(recom) <- NULL
    return(recom[,c(1,3)])
    
  }
  if (n == 2) {
    gram <- word(sentence, n-3)
    b <- bigram_frequencies_pruned
    b <- b %>% filter(str_detect(bigrams, paste0(c("^", gram, "\\s+"), collapse= "")))
    
    if (length(b$bigrams) == 0) {
      return(sbo(sentence, n-1))  
    }
    b <- data.frame("last_word" = word(b$bigrams, -1), "frequency" = b$frequency)
    b_sum <- sum(b$frequency)
    b <- b[!(b$last_word %in% recom$last_word),]
    b <- b[1:(min(length(b$last_word), (5-length(recom[[1]])))), ]
    b$probability <- b$frequency*((0.4*0.4)/b_sum)
    
    recom <<- rbind(recom, b)
    
    if (length(recom$last_word) < 5) {
      return(sbo(sentence, n-1))
    }
    
    recom <- recom[order(recom$probability, decreasing = TRUE), ]
    rownames(recom) <- NULL
    return(recom[,c(1,3)])
    
    
  } 
  if (n == 3) {
    gram <- word(sentence, n-5, n-4)
    b <- trigram_frequencies_pruned
    b <- b %>% filter(str_detect(trigrams, paste0(c("^", gram, "\\s+"), collapse= "")))
    
    if (length(b$trigrams) == 0) {
      return(sbo(sentence, n-1))  
    }
    
    b <- data.frame("last_word" = word(b$trigrams, -1), "frequency" = b$frequency)
    b_sum <- sum(b$frequency)
    b <- b[!(b$last_word %in% recom$last_word),]
    b <- b[1:(min(length(b$last_word), 5-length(recom[[1]]))), ]
    b$probability <- b$frequency*(0.4/b_sum)
    
    recom <<- rbind(recom, b)
    
    if  (length(recom$last_word) < 5) {
      return(sbo(sentence, n-1))
    } 
    
    recom <- recom[order(recom$probability, decreasing = TRUE), ]
    rownames(recom) <- NULL
    return(recom[,c(1,3)])
  }
  
  if (n == 4) {
    gram <- word(sentence, n-7, n-5)
    b <- tetragram_frequencies_pruned
    b <- b %>% filter(str_detect(tetragrams, paste0(c("^", gram, "\\s+"), collapse= "")))
    
    if (length(b$tetragrams) == 0) {
      return(sbo(sentence, n-1))  
    }
    
    b <- data.frame("last_word" = word(b$tetragrams, -1), "frequency" = b$frequency)
    b_sum <- sum(b$frequency)
    
    if (length(b$last_word) < 5) {
      b$probability <- b$frequency/b_sum
      recom <<- b   
      return(sbo(sentence, n-1))  
    }
    b <- head(b, 5)
    b$probability <- b$frequency/b_sum
    b <- b[order(b$probability, decreasing = TRUE), ]
    rownames(b) <- NULL
    return(b[, c(1,3)])
  }           
}  


kbo <- function(sentence) {
  
  #unigrams
  uni <- word_frequencies_pruned
  uni_total <- sum(word_frequencies_pruned$frequency)
  uni$probability <- (uni$frequency)/uni_total
  
  gram <- word(sentence, -1)
  
  b <- bigram_frequencies_pruned
  b <- b %>% filter(str_detect(bigrams, paste0(c("^", gram, "\\s+"), collapse= "")))
  
  
  if (length(b$bigrams) == 0)  {
    return(head(uni[,c(1,3)], 5))
    
  }           
  
  #bigrams
  bigram_start <- word(sentence, -1)
  bigram_table <- data.frame(uni[, 1])
  colnames(bigram_table)[1] <- "bigrams"
  bigram_table$bigrams <- paste(bigram_start, bigram_table$bigrams, sep = " ")
  
  
  obs_bigrams <- bigram_kbo[bigram_kbo$bigrams %in% bigram_table$bigrams,]
  obs_bigrams_sum <- sum(obs_bigrams$frequency)
  bigram_table$probability <- NA
  
  bigram_table$probability[match(obs_bigrams$bigrams, bigram_table$bigrams)] <- obs_bigrams$kbo_frequency/obs_bigrams_sum
  bigram_leftover <- 1 - sum(bigram_table$probability, na.rm = TRUE)
  
  bigram_na_indx <- which(is.na(bigram_table$probability))
  bigram_lower_sum <- sum(uni$probability[bigram_na_indx])
  bigram_alpha <- bigram_leftover/bigram_lower_sum
  
  bigram_table$probability[bigram_na_indx] <- bigram_alpha*(uni$probability[bigram_na_indx])
  
  gram <- word(sentence, -2, -1)
  
  b <- trigram_frequencies_pruned
  b <- b %>% filter(str_detect(trigrams, paste0(c("^", gram, "\\s+"), collapse= "")))
  
  
  if (length(b$trigrams) == 0)  {
    final_result <- data.frame("last_word" = uni$words, "probability" = bigram_table$probability)
    final_result <- final_result[order(final_result$probability, decreasing = TRUE),]
    rownames(final_result) <- NULL
    
    return(head(final_result, 5))
    
  } 
  
  #trigrams
  trigram_start <- word(sentence, -2, -1)
  trigram_table <- data.frame(uni[, 1])
  colnames(trigram_table)[1] <- "trigrams"
  trigram_table$trigrams <- paste(trigram_start, trigram_table$trigrams, sep = " ")
  
  obs_trigrams <- trigram_kbo[trigram_kbo$trigrams %in% trigram_table$trigrams,]
  obs_trigrams_sum <- sum(obs_trigrams$frequency)
  trigram_table$probability <- NA
  
  trigram_table$probability[match(obs_trigrams$trigrams, trigram_table$trigrams)] <- obs_trigrams$kbo_frequency/obs_trigrams_sum
  trigram_leftover <- 1 - sum(trigram_table$probability, na.rm = TRUE)
  
  trigram_na_indx <- which(is.na(trigram_table$probability))
  trigram_lower_sum <- sum(bigram_table$probability[trigram_na_indx])
  trigram_alpha <- trigram_leftover/trigram_lower_sum
  
  trigram_table$probability[trigram_na_indx] <- trigram_alpha*(bigram_table$probability[trigram_na_indx])
  
  gram <- word(sentence, -3, -1)
  
  b <- tetragram_frequencies_pruned
  b <- b %>% filter(str_detect(tetragrams, paste0(c("^", gram, "\\s+"), collapse= "")))
  
  if (length(b$tetragrams) == 0)  {
    final_result <- data.frame("last_word" = uni$words, "probability" = trigram_table$probability)
    final_result <- final_result[order(final_result$probability, decreasing = TRUE),]
    rownames(final_result) <- NULL
    
    return(head(final_result, 5))
    
  }
  
  
  #tetragrams
  tetragram_start <- word(sentence, -3, -1)
  tetragram_table <- data.frame(uni[, 1])
  colnames(tetragram_table)[1] <- "tetragrams"
  tetragram_table$tetragrams <- paste(tetragram_start, tetragram_table$tetragrams, sep = " ")
  
  obs_tetragrams <- tetragram_kbo[tetragram_kbo$tetragrams %in% tetragram_table$tetragrams,]
  obs_tetragrams_sum <- sum(obs_tetragrams$frequency)
  tetragram_table$probability <- NA
  
  tetragram_table$probability[match(obs_tetragrams$tetragrams, tetragram_table$tetragrams)] <- obs_tetragrams$kbo_frequency/obs_tetragrams_sum
  tetragram_leftover <- 1 - sum(tetragram_table$probability, na.rm = TRUE)
  
  tetragram_na_indx <- which(is.na(tetragram_table$probability))
  tetragram_lower_sum <- sum(trigram_table$probability[tetragram_na_indx])
  tetragram_alpha <- tetragram_leftover/tetragram_lower_sum
  
  tetragram_table$probability[tetragram_na_indx] <- tetragram_alpha*(trigram_table$probability[tetragram_na_indx])
  final_result <- data.frame("last_word" = uni$words, "probability" = tetragram_table$probability)
  final_result <- final_result[order(final_result$probability, decreasing = TRUE),]
  rownames(final_result) <- NULL
  
  return(head(final_result, 5))
  
  
}




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  df1 <- eventReactive(input$goButton, {
    
    
    sentence <- input$itxt
    sentence <- gsub('[[:punct:]]','', sentence)
    sentence <- gsub('[0-9]+', '', sentence)
    sentence <- tolower(sentence)
    sentence <- stripWhitespace(sentence)
    sentence <- str_trim(sentence)
    recom <<- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("last_word", "frequency"))))
    
    return(sbo(sentence, 4))
    
  }) 
  
  df2 <- eventReactive(input$goButton, {
    
    
    sentence <- input$itxt
    sentence <- gsub('[[:punct:]]','', sentence)
    sentence <- gsub('[0-9]+', '', sentence)
    sentence <- tolower(sentence)
    sentence <- stripWhitespace(sentence)
    sentence <- str_trim(sentence)
    
    return(kbo(sentence))
    
    
    
  })
  output$odf1 <- renderTable({
    df1()}, bordered = TRUE, caption = as.character(strong("SBO MODEL OUTPUT")), caption.placement = "top"
  )
  output$odf2 <- renderTable({
    df2()}, bordered = TRUE, caption = as.character(strong("KBO MODEL OUTPUT")), caption.placement = "top"
  )
  
  
})
