#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)
library(NLP)
library(RWeka)
library(stringr)
library(utils)

set.seed(1000)

profanityFile <- "./profanities.txt"
blogFilePath <- "./final/en_US/en_US.blogs.txt"
twitterFilePath <- "./final/en_US/en_US.twitter.txt"
newsFilePath <- "./final/en_US/en_US.news.txt"

#
# functions
#

# cleans a data set of numbers, punctuation, etc.
cleanDataSet <- function(dataSet) {
    # remove punctuation
    cleanedDataSet <- gsub("[[:punct:]]", "", dataSet)
    # remove numbers
    cleanedDataSet <- gsub("[0-9]", "", cleanedDataSet)
    # remove non-ascii characters (just replace with spaces - because this would remove spaces)
    cleanedDataSet <- gsub("[^a-zA-Z]", " ", cleanedDataSet)
    # make lower case
    cleanedDataSet <- tolower(cleanedDataSet)
    # remove profanities and stop words
    cleanedDataSet <- removeWords(cleanedDataSet, remWords)
}

# Cleans the input text. Does same as cleanDataSet() plus it removes some spaces.
cleanText <- function(text) {
    cleanedText <- cleanDataSet(text)
    # trim spaces from beginning and end of line, and combine multiple consecutive spaces into a single space
    cleanedText <- gsub("\\s+", " ", str_trim(cleanedText))
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$predictedLabel <- renderText({
        outputText <- "Predicted Word:"
    })

    output$predictedWord <- renderText({

        inputText <- input$inputText
        textLength <- nchar(inputText)
        if(textLength > 0) {
            #outputText <- cleanText(inputText)

            cleanedInputText <- cleanText(inputText)
            
            # need to tokenize the input String
            
            # will have to check the length of inputString
            # - if length 0, just return the most common word from unigram
            # - if length 1, check the bigram - so only get 1 word
            # - if length 2, can use this algorithm to get 2 words to check
            inputStringSplit <- unlist(strsplit(cleanedInputText, " "))
            
            if(length(inputStringSplit) == 0) {
                # just return the most used unigram
                outputText <- entireUnigramDataFrame$allWords[1]
            } else if (length(inputStringSplit) == 1) {
                bigramSearchResult <- entireBigramDataFrame[which(entireBigramDataFrame$firstWord == inputStringSplit[1]), ]
                if(nrow(bigramSearchResult) > 0) {
                    outputText <- bigramSearchResult$secondWord[1]
                } else {
                    # just return the most used unigram
                    outputText <- entireUnigramDataFrame$allWords[1]
                }
            } else {
                inputLastWord <- inputStringSplit[length(inputStringSplit)]
                inputSecondToLastWord <- inputStringSplit[length(inputStringSplit)-1]
                
                trigramSearchResult <- entireTrigramDataFrame[which(entireTrigramDataFrame$firstWord == inputSecondToLastWord & entireTrigramDataFrame$secondWord == inputLastWord), ]
                
                # If there is at least one row, just use the first row's "thirdWord"
                if(nrow(trigramSearchResult) > 0) {
                    outputText <- trigramSearchResult$thirdWord[1]
                } else {
                    # should check bigrams at this point
                    bigramSearchResult <- entireBigramDataFrame[which(entireBigramDataFrame$firstWord == inputLastWord), ]
                    if(nrow(bigramSearchResult) > 0) {
                        outputText <- bigramSearchResult$secondWord[1]
                    } else {
                        # just return the most used unigram
                        outputText <- entireUnigramDataFrame$allWords[1]
                    }
                }
            }
        }
    })
})

#
# set up the words to remove
#

# remove profanities
profanityTable <- read.table(profanityFile,
                             header = FALSE, sep = "\t", quote = "\"", stringsAsFactors = FALSE,
                             col.names = c('Profanity'))
# remove the first 4 lines, they aren't any of the profanity list
profanitySet <- profanityTable[-c(1:4),]
# get rid of all commas
profanityWords <- gsub(",", "", profanitySet, fixed = TRUE)

remWords <- c(profanityWords[-1])

#
# set up text sources
#

print("reading blog file")
# get the file contents
blogFile <- readLines(blogFilePath, skipNul = TRUE)
blogFileSampleIndexes <- sample(length(blogFile), length(blogFile)*0.005)
blogFileSample <- blogFile[blogFileSampleIndexes]
rm(blogFile)

print("reading twitter file")
twitterFile <- readLines(twitterFilePath, skipNul = TRUE)
twitterFileSampleIndexes <- sample(length(twitterFile), length(twitterFile)*0.005)
twitterFileSample <- twitterFile[twitterFileSampleIndexes]
rm(twitterFile)

print("reading news file")
# need to account for something weird with the news file
newsFileConnection <- file(newsFilePath, "rb")
newsFile <- readLines(newsFileConnection, encoding = "UTF-8", skipNul = TRUE)
close(newsFileConnection)
newsFileSampleIndexes <- sample(length(newsFile), length(newsFile)*0.005)
newsFileSample <- newsFile[newsFileSampleIndexes]
rm(newsFile)

#
# do some cleaning
#

print("cleaning data sets")
blogFileSample <- cleanDataSet(blogFileSample)
twitterFileSample <- cleanDataSet(twitterFileSample)
newsFileSample <- cleanDataSet(newsFileSample)

#
# make corpus - need to combine the separate line vectors into one string to be used as a text source
#

blogString <- paste(blogFileSample, collapse = " ")
twitterString <- paste(twitterFileSample, collapse = " ")
newsString <- paste(newsFileSample, collapse = " ")
rm(blogFileSample)
rm(twitterFileSample)
rm(newsFileSample)

fileVector <- c(blogString, twitterString, newsString)
fileCorpus <- VCorpus(VectorSource(fileVector))

#
# create unigrams, bigrams, and trigrams
#

createUnigram <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
createBigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
createTrigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

# unigrams
print("create unigram")
unigramMatrix <- DocumentTermMatrix(fileCorpus, control = list(tokenize = createUnigram))
unigramMatrix$dimnames$Docs <- c("blog", "twitter", "news")
print("set up unigram table")
unigramCount <- sort(colSums(as.matrix(unigramMatrix)), decreasing = TRUE)
rm(unigramMatrix)
allWords <- names(unigramCount)
allCounts <- unname(unigramCount)
entireUnigramDataFrame <- data.frame(allWords, allCounts)
rm(allWords)
rm(allCounts)

#bigrams
print("create bigram")
bigramMatrix <- DocumentTermMatrix(fileCorpus, control = list(tokenize = createBigram))
bigramMatrix$dimnames$Docs <- c("blog", "twitter", "news")
print("set up bigram table")
bigramCount <- sort(colSums(as.matrix(bigramMatrix)), decreasing = TRUE)
rm(bigramMatrix)
allWords <- names(bigramCount)
allCounts <- unname(bigramCount)
entireBigramDataFrame <- data.frame(allWords, allCounts)
rm(allWords)
rm(allCounts)
# create columns of each word of the bigram
entireBigramDataFrame$firstWord <- sub("([A-Za-z]+).*", "\\1", entireBigramDataFrame$allWords)
entireBigramDataFrame$secondWord <- sub(".* ([A-Za-z]+)", "\\1", entireBigramDataFrame$allWords)

# trigrams
print("create trigram")
trigramMatrix <- DocumentTermMatrix(fileCorpus, control = list(tokenize = createTrigram))
trigramMatrix$dimnames$Docs <- c("blog", "twitter", "news")
print("set up trigram table")
trigramCount <- sort(colSums(as.matrix(trigramMatrix)), decreasing = TRUE)
rm(trigramMatrix)
allWords <- names(trigramCount)
allCounts <- unname(trigramCount)
entireTrigramDataFrame <- data.frame(allWords, allCounts)
rm(allWords)
rm(allCounts)
# create columns of each word of the trigram
entireTrigramDataFrame$firstWord <- sub("([A-Za-z]+).*", "\\1", entireTrigramDataFrame$allWords)
entireTrigramDataFrame$secondWord <- sub(".* ([A-Za-z]+) .*", "\\1", entireTrigramDataFrame$allWords)
entireTrigramDataFrame$thirdWord <- sub(".* ([A-Za-z]+)", "\\1", entireTrigramDataFrame$allWords)
print("setup complete")