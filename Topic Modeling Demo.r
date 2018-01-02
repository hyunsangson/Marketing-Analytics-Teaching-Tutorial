#########################################################
### This is an exampler code for the Basic topic modeling
##########################################################

### I obtained small amount of actual review of coffee shop named "Thinking Cup" in Boston and did an preliminary topic modeling 

### set working directory.
### My machine is Window base, directory path for Mac would be differ
setwd("C:/Users/Hyunsang Son/Dropbox/Working paper/eWOM review valence and cosumption type/yelp example")

### Install and load package for preminary data cleaning (called Preprocessing)
install.packages(tm)
install.packages(NLP)
library(tm)
library(NLP)

### Load file to your machine
reviews = read.csv("thinking cup_review only.csv", header = F)

### creat corpus from vector
docs <- Corpus
corpus <- Corpus(DataframeSource(reviews))

### words in documents --> matrix
dtm <- DocumentTermMatrix(corpus)
dtm 


#remove whitespace
corpus <- tm_map(corpus, stripWhitespace)

#convert to Lower case
corpus <- tm_map(corpus, content_transformer(tolower))

#remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))

############### Frequency analysis
### find all words that appeareed more than 3 times
findFreqTerms(dtm, 3)

### you can change the number
findFreqTerms(dtm, 20)
findFreqTerms(dtm, 5)


freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq,decreasing = TRUE)

### Sorting the most frequently appeared words
freqr <- colSums(as.matrix(dtm))
length(freqr)
ordr <- order(freqr, decreasing = TRUE)
freqr[head(ordr)]
freqr[tail(ordr)]
findFreqTerms(dtm, lowfreq = 5)

################## 
## Visualization
#################


############## plotting
library(topicmodels)
library(ggplot2)
wf = data.frame(term=names(freq), occurrences=freqr)
p <- ggplot(subset(wf, freq > 5), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

########### word cloud
library(wordcloud)
set.seed(42)
wordcloud(names (freqr), freqr, min.freq = 3, color=brewer.pal(6, "Dark2"))

###################  
##topicmodels 
##################


install.packages("topicmodels")
library(topicmodels)

#set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics (you can change)
k <- 3

#LDA using gibbs sampling
#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

#write out results
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

#top 15 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,15))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))

#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])
topic1ToTopic2

#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])

#write to file
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))



############################################ 
## Now predict the rating based on the topic
############################################


