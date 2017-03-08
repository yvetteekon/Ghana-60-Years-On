# load libraries
packages <- c("RColorBrewer", "stringr", "syuzhet", "lubridate", "ggplot2", "scales",
              "reshape2", "dplyr")
lapply(packages, require, character.only = T)

# Sentiment Analysis
# load textCopy
texts <- load("textCopy.RData")

# length of texts
length(texts)
head(textCopy)
class(textCopy)

# more info about tweets
textsDF <- as.data.frame(textCopy)
class(textsDF)
head(textsDF, 3)
dim(textsDF)

# transpose dataframe
textsDF_transpose <- t(textsDF)
dim(textsDF_transpose)

# clean tweets
cleanTexts <- lapply(textsDF_transpose, function(x) {
  x = gsub('http\\S+\\s*', '', x) ## Remove URLs
  x = gsub('\\b+RT', '', x) ## Remove RT
  x = gsub('#\\S+', '', x) ## Remove Hashtags
  x = gsub('@\\S+', '', x) ## Remove Mentions
  x = gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  x = gsub("\\d", '', x) ## Remove Controls and special characters
  x = gsub('[[:punct:]]', '', x) ## Remove Punctuations
  x = iconv(x, "UTF-8", "ASCII", sub="") ## Remove emojis
  x = gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  x = gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  x = gsub(' +',' ',x) ## Remove extra whitespaces
})

class(cleanTexts)
length(cleanTexts)
cleanTexts[3200]

# convert from list to character vector
cleanTexts <- unlist(cleanTexts, recursive = FALSE)
class(cleanTexts)
cleanTexts[3200]

# get sentiment words from cleaned tweetss
sentiment <- get_nrc_sentiment(cleanTexts)
head(sentiment)
dim(sentiment)
textSentiment <- cbind(cleanTexts, sentiment)
dim(textSentiment)

# Count the sentiment words by category
head(textSentiment)
sentiment_totals <- data.frame(colSums(textSentiment[,c(2:11)]))
sentiment_totals
names(sentiment_totals) <- "count"
sentiment_totals <- cbind("sentiment" = rownames(sentiment_totals), sentiment_totals)
rownames(sentiment_totals) <- NULL

# Total sentiment score of all texts
ggplot(data = sentiment_totals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(color = "black")) + theme(legend.position = "none") +
  theme(axis.title = element_text(family = "Times", face = "bold", hjust = 0.5)) +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Twitter Sentiments - Ghana 60 Years On") +
  theme(plot.title = element_text(family = "Times", face = "bold", hjust = 0.5))

