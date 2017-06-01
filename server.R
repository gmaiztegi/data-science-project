library(shiny)
library(dplyr)
library(quanteda)
library(stringr)

fullDT <- readRDS("full.rds")

shinyServer(function(input, output) {
    wordPrediction <- reactive({
        text <- input$text
        wordPrediction <- getNextWord(text)
    })

    output$wordPred <- renderPrint(wordPrediction())
    output$inputWords <- renderText({ input$text }, quoted = FALSE)
})

getNextWord <- function(text) {

    text <- toLower(text)
    words <- tokenize(text, removeNumbers = FALSE, removePunct = TRUE)[[1]]

    n <- length(words)
    current <- ''
    first <- ''
    second <- ''
    third <- ''
    fourth <- ''

    if (n > 0 && str_sub(text, -1) != " ") {
        current <- words[n]
        n <- n - 1
    }

    if (n > 0) {
        fourth <- words[n]
        n <- n - 1
    }

    if (n > 0) {
        third <- words[n]
        n <- n - 1
    }

    if (n > 0) {
        second <- words[n]
        n <- n - 1
    }

    if (n > 0) {
        first <- words[n]
        n <- n - 1
    }

    predictions <- predictNext(first, second, third, fourth, current, 1)

    print(toString(predictions))
}

predictNext <- function(word1 = "", word2 = "", word3 = "", word4 = "", current = "", maxResults = 10) {

    if (word1 != "") {
        result <- filter(fullDT, (X1==word1 | is.na(X1)) & X2==word2 & X3==word3 & X4==word4)
    } else if (word2 != "") {
        result <- filter(fullDT, is.na(X1) & (X2 == word2 | is.na(X2)) & X3==word3 & X4==word4)
    } else if (word3 != "") {
        result <- filter(fullDT, is.na(X1) & is.na(X2) & (X3 == word3 | is.na(X3)) & X4==word4)
    } else if (word4 != "") {
        result <- filter(fullDT, is.na(X1) & is.na(X2) & is.na(X3) & (X4 == word4 | is.na(X4)))
    } else {
        result <- filter(fullDT, is.na(X1) & is.na(X2) & is.na(X3) & is.na(X4))
    }

    if (current != "") {
        result <- filter(result, grepl(paste0("^", current), X5))
    }

    slice(result, 1:maxResults)
    unique(na.omit(result$X5[1:maxResults]))
}
