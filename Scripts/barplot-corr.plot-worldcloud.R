# load libraries
packages <- c("tm", "ggplot2", "wordcloud", "Rgraphviz")
lapply(packages, require, character.only = T)

# load data
load("df.RData")
load("dtm.RData")

# create term document matrix
tdm <- as.TermDocumentMatrix(dtm)
save(tdm, file = "tdm.RData")

# term frequency
findFreqTerms(dtm, lowfreq = 5)

dim(tdm)
inspect(tdm[60:65, 20:25])
idx <- which(dimnames(tdm)$Terms == "ghana")
idx
inspect(tdm[idx + (0:5), 20:25])
inspect(tdm[idx + (0:5), 16])

# graph of term frequency
pdf("Term Frequency.pdf")
# The palette with grey:
subset(df, freq > 90) %>% ggplot(aes(x = words, y = freq, fill = freq)) +
  geom_bar(stat = "identity", alpha = 0.6, width = 0.8) + scale_color_hue() +
  labs(title = "Words Which Occur at Least 90 Times", x = "Words", y = "Count") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))  + coord_flip()
dev.off()

# term association
findAssocs(dtm, c("ghana", "country"), c(0.05, 0.7))

# correlation plot
pdf("Term Association.pdf")
plot(dtm, terms = findFreqTerms(dtm, lowfreq = 90), corThreshold = 0.5,
     weighting = F, main = "Correlation Plot")
dev.off()

# both term frequency and association
# word cloud
pdf("WordCloud.pdf")
# color palettes
color1 <- rainbow(6)
color2 <- brewer.pal(6, "Dark2")
color2 <- color2[-(1)]
color3 <- brewer.pal(6, "Set1")
color4 <- brewer.pal(6, "Set3")
color5 <- brewer.pal(6, "Accent")
color6 <- brewer.pal(12, "Paired")

set.seed(123)
wordcloud(df$words, df$freq, scale = c(3, 0.4), min.freq = 11 , random.order = FALSE,
          rot.per = 0.15, colors = color6, vfont = c("script","plain"))

set.seed(123)
wordcloud(df$words, df$freq, scale = c(3, 0.4), min.freq = 11 , random.order = FALSE,
          rot.per = 0.15, colors = color6, vfont = c("gothic english","plain"))

set.seed(123)
wordcloud(df$words, df$freq, scale = c(3, 0.4), min.freq = 11 , random.order = TRUE,
          rot.per = 0.15, colors = color6, vfont = c("serif","plain"))

wordcloud(df$words, df$freq, scale = c(3, 0.4), min.freq = 11 , random.order = FALSE,
          rot.per = 0.15, colors = color3, vfont = c("gothic english", "plain"))

wordcloud(df$words, df$freq, scale = c(3, 1), min.freq = 11 , random.order = FALSE,
          rot.per = 0.15, colors = color3, vfont = c("gothic german", "plain"))

wordcloud(df$words, df$freq, scale = c(3, 0.4), min.freq = 11 , random.order = FALSE,
          rot.per = 0.15, colors = color3, vfont = c("gothic italian", "plain"))

wordcloud(df$words, df$freq, scale = c(3, 1), min.freq = 11 , random.order = FALSE,
          rot.per = 0.15, colors = color3, vfont = c("sans serif", "bold"))

wordcloud(df$words, df$freq, scale = c(3, 1), min.freq = 11 , random.order = FALSE,
          rot.per = 0.15, colors = color3, vfont = c("serif", "cyrillic"))

par(bg = "black")
wordcloud(df$words, df$freq, scale = c(3, 0.8), colors = c("tomato", "wheat", "lightblue"),
          random.color = TRUE, rot.per = 0.5, min.freq = 80, font = 2, family = "serif")

par(bg = "black")
wordcloud(df$words, df$freq, scale = c(3, 0.8), colors = terrain.colors(8, alpha = 1),
          random.color = TRUE, rot.per = 0.5, min.freq = 80, font = 2, family = "serif")

par(bg = "black")
wordcloud(df$words, df$freq, scale = c(3, 0.8), colors = terrain.colors(8, alpha = 1),
          random.color = TRUE, rot.per = 0.5, min.freq = 80, vfont = c("gothic english", "plain"))

par(bg = "black")
wordcloud(df$words, df$freq, scale = c(3, 0.8), min.freq = 80 , random.order = FALSE,
          rot.per = 0.15, colors = color3, vfont = c("sans serif", "bold"))

dev.off()

