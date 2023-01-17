library(tm)
library(ggplot2)
library(wordcloud)
library(RWeka)
library(cld3)
library(dplyr)
library(stringi)
library(stringr)

---PREPROCESSING---

# Loading and preprocessing corpus text data. Please refer to coursera course for source of corpus data.
length(readLines("en_US.blogs.txt"))
length(readLines("en_US.news.txt"))
length(readLines("en_US.twitter.txt"))

# Creating sample data of corpus
en_US.blogs.sample <- sample(iconv(readLines("en_US.blogs.txt"), from = "UTF-8", to = "ASCII", sub = ""), 90000)
write(en_US.blogs.sample, "en_US.blogs.sample.txt")

en_US.news.sample <- sample(iconv(readLines("en_US.news.txt"), from = "UTF-8", to = "ASCII", sub = ""), 7800)
write(en_US.news.sample, "en_US.news.sample.txt")

en_US.twitter.sample <- sample(iconv(readLines("en_US.twitter.txt"), from = "UTF-8", to = "ASCII", sub = ""), 240000)
write(en_US.twitter.sample, "en_US.twitter.sample.txt")



folder <- getwd()
corpus <- VCorpus(DirSource(directory = folder, pattern = "*sample.txt"))
summary(corpus)

# Creating corpus variable using tm 
corpus <- tm_map(corpus, content_transformer(removePunctuation))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))

profane_words <- read.table("https://www.cs.cmu.edu/~biglou/resources/bad-words.txt")
corpus <- tm_map(corpus, removeWords, profane_words$V1)
corpus <- tm_map(corpus, stripWhitespace)

# Tokenized corpus
corpus_tokenized <- corpus
corpus_tokenized[[1]]$content <- Boost_tokenizer(corpus[[1]]$content)
corpus_tokenized[[2]]$content <- Boost_tokenizer(corpus[[2]]$content)
corpus_tokenized[[3]]$content <- Boost_tokenizer(corpus[[3]]$content)

---EDA---
  
#unigram
tdm <- TermDocumentMatrix(corpus_tokenized)
tdm <- as.matrix(tdm)

frequencies <- sort(rowSums(tdm), decreasing = TRUE)
word_frequencies <- data.frame(words = names(frequencies), frequency = frequencies)
row.names(word_frequencies) <- NULL

head(word_frequencies)
word_frequencies$words <- factor(word_frequencies$words, levels = word_frequencies$words)
ggplot(word_frequencies[1:30,], aes(x= words, y=frequency)) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), axis.title.x = element_text(vjust=-1.8)) + 
  labs(title = "Corpora Word Frequencies", x = "Words", y = "Frequency")
wordcloud(words = word_frequencies$words, freq = word_frequencies$frequency, min.freq = 1000,
          max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"), scale=c(2,.5))


#bigram
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm_bigram <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
tdm_bigram <- as.matrix(tdm_bigram)
bigram_freq <- sort(rowSums(tdm_bigram), decreasing = TRUE)
bigram_frequencies <- data.frame(bigrams = names(bigram_freq), frequency = bigram_freq)
row.names(bigram_frequencies) <- NULL
head(bigram_frequencies)
bigram_frequencies$bigrams <- factor(bigram_frequencies$bigrams, levels = bigram_frequencies$bigrams)
ggplot(bigram_frequencies[1:30,], aes(x= bigrams, y=frequency)) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), axis.title.x = element_text(vjust=-2)) + 
  labs(title = "Corpora bigram Frequencies", x = "Bigrams", y = "Frequency")
wordcloud(words = bigram_frequencies$bigrams, freq = bigram_frequencies$frequency, min.freq = 100,
          max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"), scale=c(2,.5))


#trigram
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm_trigram <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))
tdm_trigram <- as.matrix(tdm_trigram)
trigram_freq <- sort(rowSums(tdm_trigram), decreasing = TRUE)
trigram_frequencies <- data.frame(trigrams = names(trigram_freq), frequency = trigram_freq)
row.names(trigram_frequencies) <- NULL
head(trigram_frequencies)
trigram_frequencies$trigrams <- factor(trigram_frequencies$trigrams, levels = trigram_frequencies$trigrams)
ggplot(trigram_frequencies[1:30,], aes(x= trigrams, y=frequency)) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), axis.title.x = element_text(vjust=-2)) + 
  labs(title = "Corpora trigram Frequencies", x = "Trigrams", y = "Frequency")
wordcloud(words = trigram_frequencies$trigrams, freq = trigram_frequencies$frequency, min.freq = 10,
          max.words=50, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"), scale=c(2,.5))


#tetragram
TetragramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
tdm_tetragram <- TermDocumentMatrix(corpus, control = list(tokenize = TetragramTokenizer))
tdm_tetragram <- as.matrix(tdm_tetragram)
tetragram_freq <- sort(rowSums(tdm_tetragram), decreasing = TRUE)
tetragram_frequencies <- data.frame(tetragrams = names(tetragram_freq), frequency = tetragram_freq)
row.names(tetragram_frequencies) <- NULL
head(tetragram_frequencies)
tetragram_frequencies$tetragrams <- factor(tetragram_frequencies$tetragrams, levels = tetragram_frequencies$tetragrams)
ggplot(tetragram_frequencies[1:30,], aes(x= tetragrams, y=frequency)) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1), axis.title.x = element_text(vjust=-2)) + 
  labs(title = "Corpora tetragram Frequencies", x = "Tetragrams", y = "Frequency")
wordcloud(words = tetragram_frequencies$tetragrams, freq = tetragram_frequencies$frequency, min.freq = 10,
          max.words=50, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"), scale=c(2,.5))




#no of unique words for covering word instances
cumm_freq <- cumsum(word_frequencies$frequency)
cumm_freq <- as.data.frame(cumm_freq)

length(cumm_freq[cumm_freq$cumm_freq <= (0.5*3782944),])
length(cumm_freq[cumm_freq$cumm_freq <= (0.9*3782944),])


#foreign words
corpus_original <- VCorpus(DirSource(directory = folder, pattern = "*.txt"))
corpus_original <- tm_map(corpus_original, content_transformer(removePunctuation))
corpus_foreign <- tm_map(corpus_original, content_transformer(function(s){
  gsub(pattern = '[?!^a-zA-Z0-9\\s]+',
       x = s, 
       replacement = " ",
       ignore.case = TRUE,
       perl = TRUE)
}))
unique(corpus_foreign[[1]]$content)
corpus_original[[1]]$content[which(detect_language(corpus_original[[1]]$content) != "en")]



synmfunc <- function(X) {
  
  
}
synm_list <- list()

for (i in word_frequencies$words) {
  synm_list <- append(synm_list, synonyms(as.character(i), pos = "NOUN"))
  
}


#pruning
word_frequencies_pruned <- word_frequencies %>% filter(word_frequencies$frequency > 1)
bigram_frequencies_pruned <- bigram_frequencies %>% filter(bigram_frequencies$frequency >1)
trigram_frequencies_pruned <- trigram_frequencies %>% filter(trigram_frequencies$frequency >1)
tetragram_frequencies_pruned <- tetragram_frequencies %>% filter(tetragram_frequencies$frequency >1)

# Save the variables as Rds files to be used for the R shiny App

