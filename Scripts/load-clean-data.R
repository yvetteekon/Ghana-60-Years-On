# load libraries
packages <- c("tm", "SnowballC", "magrittr")
lapply(packages, require, character.only = T)

# load tweets
load("tweets.RData")

# length of tweets
l <- length(tweets)

# create an empty vector for tweets
texts <- vector(mode = "character", length = l)

# extract the tweet text from each tweet status
for (i in 1:l) texts[i] <- tweets[[i]]$getText()
texts[1]

# create copy of tweets and unlist
textCopy = texts

# save tweet texts for reproducibility
save(textCopy, file = "textCopy.RData")
save(texts, file = "text.RData")
write.csv(texts, file = "text.csv", quote = FALSE, row.names = FALSE)

# convert character vector to corpus
docs <- Corpus(VectorSource(texts))
inspect(docs[1])
inspect(docs)[1]
viewDocs <- function(d, n){
  d %>% extract2(n) %>% as.character() %>% writeLines()
}
viewDocs(docs, 16)

# text cleaning
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/|#|\\|") # remove hastags

removeEmojis <- content_transformer(function(x) iconv(x, "UTF-8", "ASCII", sub=""))
docs <- tm_map(docs, removeEmojis)

docs <- tm_map(docs, content_transformer(tolower))

removeUsername <- content_transformer(function(x) gsub("@\\w+", "", x))
docs <- tm_map(docs, removeUsername)

docs <- tm_map(docs, removePunctuation)

docs <- tm_map(docs, removeNumbers)

toString <- content_transformer(function(x, from, to) gsub(from, to, x))
docs <- tm_map(docs, toString, "ghanas", "ghana")
docs <- tm_map(docs, toString, "ghanaians", "ghanaian")
docs <- tm_map(docs, toString, "celebrating", "celebrate")

myStopwords <- c(stopwords("en"), "tco", "ghanaat", "ghanayearson",
                 "ghanais", "amp", "kknitsdj", "idgyofl", "ghanaindependenceday",
                 "indepence", "ghanaindependence", "ejbfpdz", "grcyksseq",
                 "ghanathink", "pazuqelie", "nyu", "sxknemyf", "jpuyplr", "heres",
                 "nbesyqia", "tccdhtob", "liitt", "tie", "tie", "kjfosirno",
                  "ghanaianfollowtrain", "gyal", "rsemnseki", "qeijliwgd", "aaupbnujz",
                 "vvvvp", "wzyzuztq", "rjpulhbih", "mani", "qtzqyfkfli", "dcufmwqtb",
                 "rnnjnlbj", "ahfwctezfj", "bkchatldn", "dlffitbn")

docs <- tm_map(docs, removeWords, myStopwords)

removeURLS <- content_transformer(function(x) gsub("http[[:alnum:]]*", "", x))
docs <- tm_map(docs, removeURLS)

removeControls <- content_transformer(function(x) gsub('[[:cntrl:]]', '', x))
docs <- tm_map(docs, removeControls)

removeRT <- content_transformer(function(x) gsub('\\b+rt', '', x))
docs <- tm_map(docs, removeRT)

#removeEmojis <- content_transformer(function(x) iconv(x, "latin1", "ASCII", sub=""))
#docs <- tm_map(docs, removeEmojis)

removeLeadingSpaces <- content_transformer(function(x) gsub("^[[:space:]]*", "", x))
docs <- tm_map(docs, removeLeadingSpaces)

removeTrailingSpaces <- content_transformer(function(x) gsub("[[:space:]]*$", "", x))
docs <- tm_map(docs, removeTrailingSpaces)

docs <- tm_map(docs, stripWhitespace)

# stemming
#docsCopy <- docs
#docs <- tm_map(docs, stemDocument)
#viewDocs(docs, 16)
#viewDocs(docsCopy, 16)

# clean corpus
#docs <- tm_map(docs, PlainTextDocument)

# create document term matrix
dtm <- DocumentTermMatrix(docs)#, control = list(wordLengths = c(1, Inf)))
dtm

# reduce sparsity in matrix
dtm <- removeSparseTerms(dtm, 0.99)
save(dtm, file = "dtm.RData")

# Create matrix and dataframe
freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
words <- names(freq)
words

# stem completion
#words <- stemCompletion(words, docsCopy, type = "prevalent")
#words

df <- data.frame(words = words, freq = freq)
df
row.names(df) <- NULL
which(df$words == "god")
df$words <- as.character(df$words)
df$words[56] <- "God"
save(df, file = "df.RData")

