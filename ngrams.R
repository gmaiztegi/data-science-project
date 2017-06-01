library(dplyr)
library(ggplot2)
library(quanteda)

# Read the files

blogsCon <- file("en_US/en_US.blogs.txt", "r", encoding = "UTF-8")
blogs <- readLines(blogsCon, skipNul = TRUE)
close(blogsCon)

newsCon <- file("en_US/en_US.news.txt", "r", encoding = "UTF-8")
news <- readLines(newsCon, skipNul = TRUE)
close(newsCon)

twitterCon <- file("en_US/en_US.twitter.txt", "r", encoding = "UTF-8")
twitter <- readLines(twitterCon, skipNul = TRUE)
close(twitterCon)

rm(blogsCon, newsCon, twitterCon)

# Sampling

sampleSize <- 70000
set.seed(1234)
sampleTwitter <- sample(twitter, sampleSize)
sampleNews <- sample(news, sampleSize)
sampleBlogs <- sample(blogs, sampleSize)
textSample <- c(sampleTwitter,sampleNews,sampleBlogs)

rm(blogs, news, twitter)
rm(sampleSize, sampleTwitter, sampleNews, sampleBlogs)

# Ngram construction

#myUnigrams <- dfm(textSample, ignoredFeatures=stopwords("english"), stem = TRUE)
myUnigrams <- dfm(textSample, ngrams = 1)
myBigrams <- dfm(textSample, ngrams = 2, concatenator = " ")
myTrigrams <- dfm(textSample, ngrams = 3, concatenator = " ")
myQuatrigrams <- dfm(textSample, ngrams = 4, concatenator = " ")
myPentagrams <- dfm(textSample, ngrams = 5, concatenator = " ")

# Unigram contruction

uniSums <- sort(colSums(myUnigrams), decreasing = TRUE)
uniDT <- data.frame(X1=names(uniSums), freq=unname(uniSums))
uniDT$prob <- uniDT$freq / sum(uniDT$freq)
rm(uniSums)

# Bigram construction

biSums <- sort(colSums(myBigrams), decreasing = TRUE)
biNames <- strsplit(names(biSums), " ")
biDT <- data.frame(matrix(unlist(biNames), nrow = length(biNames), byrow = TRUE))
biDT$freq <- unname(biSums)
rm(biNames, biSums)

# Trigram construction

triSums <- sort(colSums(myTrigrams), decreasing = TRUE)
triNames <- strsplit(names(triSums), " ")
triDT <- data.frame(matrix(unlist(triNames), nrow = length(triNames), byrow = TRUE))
triDT$freq <- unname(triSums)
rm(triNames, triSums)

# Quatrigram construction

quadSums <- sort(colSums(myQuatrigrams), decreasing = TRUE)
quadNames <- strsplit(names(quadSums), " ")
quadDT <- data.frame(matrix(unlist(quadNames), nrow = length(quadNames), byrow = TRUE))
quadDT$freq <- unname(quadSums)
rm(quadNames, quadSums)

# Pentagram construction

pentaSums <- sort(colSums(myPentagrams), decreasing = TRUE)
pentaNames <- strsplit(names(pentaSums), " ")
pentaDT <- data.frame(matrix(unlist(pentaNames), nrow = length(pentaNames), byrow = TRUE))
pentaDT$freq <- unname(pentaSums)
rm(pentaNames, pentaSums)

biDT[,1] <- factor(biDT[,1], levels(uniDT[,1]))
biDT[,2] <- factor(biDT[,2], levels(uniDT[,1]))
triDT[,1] <- factor(triDT[,1], levels(uniDT[,1]))
triDT[,2] <- factor(triDT[,2], levels(uniDT[,1]))
triDT[,3] <- factor(triDT[,3], levels(uniDT[,1]))
quadDT[,1] <- factor(quadDT[,1], levels(uniDT[,1]))
quadDT[,2] <- factor(quadDT[,2], levels(uniDT[,1]))
quadDT[,3] <- factor(quadDT[,3], levels(uniDT[,1]))
quadDT[,4] <- factor(quadDT[,4], levels(uniDT[,1]))
pentaDT[,1] <- factor(pentaDT[,1], levels(uniDT[,1]))
pentaDT[,2] <- factor(pentaDT[,2], levels(uniDT[,1]))
pentaDT[,3] <- factor(pentaDT[,3], levels(uniDT[,1]))
pentaDT[,4] <- factor(pentaDT[,4], levels(uniDT[,1]))
pentaDT[,5] <- factor(pentaDT[,5], levels(uniDT[,1]))


makeUniDT <- function(uniDT, biDT) {

    endingTypes <- biDT %>%
        group_by(X2) %>%
        summarise(count = n())

    endingTypesMatchUni <- endingTypes[match(uniDT$X1, endingTypes$X2),]$count

    uniDT$prob <- endingTypesMatchUni/nrow(biDT)

    arrange(uniDT, desc(prob)) %>% filter(!is.na(prob))
}

makeBiDT <- function(uniDT, biDT, discount = 1) {
    startingTypes <- biDT %>%
        group_by(X1) %>%
        summarise(count = n())

    pairStartingFreq <- biDT %>%
        group_by(X1) %>%
        summarise(sum = sum(freq))

    freqOrZero <- biDT[,"freq"]-discount
    freqOrZero[freqOrZero<0] <- 0

    pairStartingFreqMatchBi <- pairStartingFreq[match(biDT$X1, pairStartingFreq$X1),]$sum
    startingTypesMatchBi <- startingTypes[match(biDT$X1, startingTypes$X1),]$count
    fallbackPknMatchUni <- uniDT[match(biDT$X2, uniDT$X1),]$prob

    biDT$prob <- freqOrZero/pairStartingFreqMatchBi + discount/pairStartingFreqMatchBi*startingTypesMatchBi*fallbackPknMatchUni

    arrange(biDT, desc(prob))
}

makeTriDT <- function(biDT, triDT, discount = 1) {

    #dt$prob <- uniDT$freq / sum(uniDT$freq)

    pairStartingFreq <- triDT %>%
        group_by(X1, X2) %>%
        summarise(sum = sum(freq))

    startingTypes <- triDT %>%
        group_by(X1, X2) %>%
        summarise(count = n())

    freqOrZero <- triDT[,"freq"]-discount
    freqOrZero[freqOrZero<0] <- 0

    pairStartingFreqMatchTri <- inner_join(triDT, pairStartingFreq, by = c("X1", "X2"))$sum
    startingTypesMatchTri <- inner_join(triDT, startingTypes, by = c("X1", "X2"))$count
    fallbackPknMatchBi <- inner_join(triDT, biDT, by = c("X2"="X1", "X3"="X2"))$prob

    triDT$prob <- freqOrZero/pairStartingFreqMatchTri + discount/pairStartingFreqMatchTri*startingTypesMatchTri*fallbackPknMatchBi

    arrange(triDT, desc(prob))
}

makeQuadDT <- function(triDT, quadDT, discount = 1) {

    #dt$prob <- uniDT$freq / sum(uniDT$freq)

    pairStartingFreq <- quadDT %>%
        group_by(X1, X2, X3) %>%
        summarise(sum = sum(freq))

    startingTypes <- quadDT %>%
        group_by(X1, X2, X3) %>%
        summarise(count = n())

    freqOrZero <- quadDT[,"freq"]-discount
    freqOrZero[freqOrZero<0] <- 0

    pairStartingFreqMatchQuad <- inner_join(quadDT, pairStartingFreq, by = c("X1", "X2", "X3"))$sum
    startingTypesMatchQuad <- inner_join(quadDT, startingTypes, by = c("X1", "X2", "X3"))$count
    fallbackPknMatchTri <- inner_join(quadDT, triDT, by = c("X2"="X1", "X3"="X2", "X4"="X3"))$prob

    quadDT$prob <- freqOrZero/pairStartingFreqMatchQuad + discount/pairStartingFreqMatchQuad*startingTypesMatchQuad*fallbackPknMatchTri

    arrange(quadDT, desc(prob))
}

makePentaDT <- function(quadDT, pentaDT, discount = 1) {

    #dt$prob <- uniDT$freq / sum(uniDT$freq)

    pairStartingFreq <- pentaDT %>%
        group_by(X1, X2, X3, X4) %>%
        summarise(sum = sum(freq))

    startingTypes <- pentaDT %>%
        group_by(X1, X2, X3, X4) %>%
        summarise(count = n())

    freqOrZero <- pentaDT[,"freq"]-discount
    freqOrZero[freqOrZero<0] <- 0

    pairStartingFreqMatchPenta <- inner_join(pentaDT, pairStartingFreq, by = c("X1", "X2", "X3", "X4"))$sum
    startingTypesMatchPenta <- inner_join(pentaDT, startingTypes, by = c("X1", "X2", "X3", "X4"))$count
    fallbackPknMatchQuad <- inner_join(pentaDT, quadDT, by = c("X2"="X1", "X3"="X2", "X4"="X3", "X5"="X4"))$prob

    pentaDT$prob <- freqOrZero/pairStartingFreqMatchPenta + discount/pairStartingFreqMatchPenta*startingTypesMatchPenta*fallbackPknMatchQuad

    arrange(pentaDT, desc(prob))
}

discount <- 1

uniDT <- makeUniDT(uniDT, biDT)
biDT <- makeBiDT(uniDT, biDT, discount)
triDT <- makeTriDT(biDT, triDT, discount)
quadDT <- makeQuadDT(triDT, quadDT, discount)
pentaDT <- makePentaDT(quadDT, pentaDT, discount)

uniDT1 <- rename(uniDT, X5=X1) %>% mutate(X1=NA, X2=NA, X3=NA, X4=NA)
biDT1 <- rename(biDT, X4=X1, X5=X2) %>% mutate(X1=NA, X2=NA, X3=NA)
triDT1 <- rename(triDT, X3=X1, X4=X2, X5=X3) %>% mutate(X1=NA, X2=NA)
quadDT1 <- rename(quadDT, X2=X1, X3=X2, X4=X3, X5=X4) %>% mutate(X1=NA)

fullDT <- rbind(pentaDT, quadDT1, triDT1, biDT1, uniDT1)

fullDT1 <- filter(fullDT, freq > discount) %>% select(-freq) %>% arrange(desc(prob))

uniDT1 <- filter(uniDT, freq > discount) %>% select(-freq)
biDT1 <- filter(biDT, freq > discount) %>% select(-freq)
triDT1 <- filter(triDT, freq > discount) %>% select(-freq)
quadDT1 <- filter(quadDT, freq > discount) %>% select(-freq)
pentaDT1 <- filter(pentaDT, freq > discount) %>% select(-freq)

fullDT1 <- filter(fullDT, freq > discount) %>% select(-freq) %>% arrange(desc(prob))

saveRDS(uniDT1, "uni.rds")
saveRDS(biDT1, "bi.rds")
saveRDS(triDT1, "tri.rds")
saveRDS(quadDT1, "quad.rds")
saveRDS(pentaDT1, "penta.rds")

saveRDS(fullDT1, "full.rds")
