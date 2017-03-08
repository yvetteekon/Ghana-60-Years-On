# load libraries
packages <- c("twitteR", "RCurl")
lapply(packages, require, character.only = T)

# set up api credentials
consumer_key <- "xxxxx"
consumer_secret <- "xxxxx"
access_token <- "xxxxx"
access_secret <- "xxxxx"

# create handshake
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# extract tweets
tweets <- searchTwitter("#Ghana60YearsOn OR #GhanaAt60 OR #Ghanaat60 OR #Ghana@60
                        OR #GhanaIs60", n = 3200, resultType = "recent", lang = "en")
head(tweets, 3)

# save tweets for reproducibility
save(tweets, file = "tweets.RData")
