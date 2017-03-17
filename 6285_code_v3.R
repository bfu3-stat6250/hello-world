#install.packages("wordcloud")
#install.packages("topicmodels")
# Set SSL certs globallyl
#install.packages("ROAuth")
#twitteR, tm, SnowballC, ggplot2, RColorBrewer, wordcloud, topicmodels
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("graph")  #na in 3.3 r
#install.packages("Rgraphviz")  #na in 3.3 r
#install.packages("syuzhet")

library(RCurl)
library(syuzhet)
library(igraph)
library(XML)
library(twitteR)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(RTextTools)
library(topicmodels) 
library(tm)
library(SnowballC) #for wordcloud
#library(ROAuth) for old auth twitter
library(ggplot2) 
library(cluster)

##old style twitter auth
#reqURL <- "https://api.twitter.com/oauth/request_token"
#accessURL <- "https://api.twitter.com/oauth/access_token"
#authURL <- "https://api.twitter.com/oauth/authorize"
#apiKey <-  "    "
#apiSecret <- "    "

#twitCred <- OAuthFactory$new(
#  consumerKey = apiKey, 
#  consumerSecret = apiSecret,
#  requestURL = reqURL,
#  accessURL = accessURL, 
#  authURL = authURL
#)

#twitCred$handshake(
#  cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")
#)

#registerTwitterOAuth(twitCred)

# save credentials #should work these code for auth
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "  "
consumerSecret <- " "

accessToken <- " "
accessSecret <- " "

setup_twitter_oauth(consumerKey,
                    consumerSecret,
                    accessToken,
                    accessSecret)


dlcar <- searchTwitter("Driverless", n=1000)  #collect 1000 twitter 
dlcar.htag <- searchTwitter("#Driverless", n=1000) #collect 1000 twitter with hash tag #
#head(dlcar)
#dlcarbackup <- dlcar #> class(dlcar)[1] "list" #dlcar for driverless car
#dlcarbakkup.htag <- dlcar.htag


#get the twitter text portion
tweets.text <- sapply(dlcar, function(x) x$getText())

#get the twitter text portion for the dataset from hash tag #
tweetshtag.text <- sapply(dlcar.htag, function(x) x$getText())

#for twitter file conversion if needed
#tweets.df <- twListToDF(dlcar) 
#dim(tweets.df)
#View(tweets.df)


#data prepare for text mining, working for the dataset from tweets.text first, where it is from no hash tag #
# Replace blank space ("rt")
tweets.text <- gsub("rt", "", tweets.text)


# Replace @UserName
tweets.text <- gsub("@\\w+", "", tweets.text)

# Remove punctuation
tweets.text <- gsub("[[:punct:]]", "", tweets.text)

# Remove links
tweets.text <- gsub("http\\w+", "", tweets.text)

# Remove tabs
tweets.text <- gsub("[ |\t]{2,}", " ", tweets.text)

# Remove \n
tweets.text <- gsub("[ |\n]{2,}", " ", tweets.text)

# Remove blank spaces at the beginning
tweets.text <- gsub("^ ", "", tweets.text)

# Remove blank spaces at the end
tweets.text <- gsub(" $", "", tweets.text)

#convert all text to lower case
tweets.text <- tolower(tweets.text)

tweets.text <- gsub("rt", "", tweets.text)

#for twitter sentiment analysis
dlchar <- as.character(tweets.text)
mySentiment <- get_nrc_sentiment(dlchar)

#dlhatagchar <- as.character(tweetshtag.text)  #for prepare the dataset from hash tag #
#mySentimenthtag <- get_nrc_sentiment(dlhatagchar)

#testdl$tweets.text <- as.character(testdl$tweets.text)
#testdl[] <- lapply(testdl, as.character)
#str(testdl)

Positive <-sum(mySentiment$positive)
Negative <-sum(mySentiment$negative)
dlpl <- data.frame(Positive,Negative)
dlplma <- as.matrix(dlpl)
barplot(dlplma)




#prepare for uber subset analysis
testdl <-as.data.frame(tweets.text)
View(testdl)

#subset uber 
abc <- testdl[grep("uber", testdl$tweets.text),]
abcd <-as.data.frame(abc)
abc <- as.character(abc)
uberfb <- get_sentiment(abc)
uberdf <- data.frame(abcd, uberfb)
View(uberdf)
summary(uberdf$uberfb)
##> summary(uberdf$uberfb)
##Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##-2.0000 -0.7500 -0.7500 -0.4017  0.2500  1.2500





#further data cleaning for building the cluster
#create corpus
tweets.text.corpus <- Corpus(VectorSource(tweets.text))

#clean up by removing stop words

myStopwords <- c(stopwords('english'), "A", "rt", "h", "s")  

tweets.text.corpus <- tm_map(tweets.text.corpus, removePunctuation)

#tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords()))
tweets.text.corpus <- tm_map(tweets.text.corpus, removeWords, myStopwords)


# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 


tweets.text.corpus <- tm_map(tweets.text.corpus, content_transformer(removeNumPunct))

#plotting no more than 150 words that occur more than once with random color, order, and position.
#generate wordcloud

wordcloud(tweets.text.corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)

# term-document matrix
tdm = TermDocumentMatrix(tweets.text.corpus)


#the below code for building the cluster using the hclust method, in our case,
#I will use the agnes method instead
# convert as matrix
#m = as.matrix(tdm)
# remove sparse terms (word frequency > 90% percentile)
#wf = rowSums(m)
#m1 = m[wf>quantile(wf,probs=0.9), ]
# remove columns with all zeros
#m1 = m1[,colSums(m1)!=0]
# for convenience, every matrix entry must be binary (0 or 1)
#m1[m1 > 1] = 1
# distance matrix with binary distance
#m1dist = dist(m1, method="binary")
# cluster with ward method
#clus1 = hclust(m1dist, method="ward.D")
# plot dendrogram
#plot(clus1, cex=0.9)



# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.98)
m2 <- as.matrix(tdm2)

dlcarMatrix <- (scale(m2))
dlcarclusters <- agnes(dlcarMatrix, method = "complete", metric = "euclidean")
plot(dlcarclusters, which.plots=2, cex = 1.2)
rect.hclust(dlcarclusters, k = 6)

myCorpus <- tweets.text.corpus
inspect(myCorpus[1:5]) 
#class(myCorpus)
#Str(myCorpus)

#optional code: if want to replace the "car" with "cars", etc.
# The code below is used for to make text fit for paper width 
#for (i in c(1:2, 1000)) { 
#  cat(paste0("[", i, "] ")) 
#  writeLines(strwrap(as.character(myCorpus[[i]]), 60)) 
#}


#stemCompletion2 <- function(x, dictionary) { 
#  x <- unlist(strsplit(as.character(x), " ")) 
  # Unexpectedly, stemCompletion completes an empty string to 
  # a word in dictionary. Remove empty string to avoid above issue. 
#  x <- x[x != ""] 
#  x <- stemCompletion(x, dictionary=dictionary) 
#  x <- paste(x, sep="", collapse=" ") 
#  PlainTextDocument(stripWhitespace(x)) 
#} 

#myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy) 
#myCorpus <- Corpus(VectorSource(myCorpus))


# count frequency of "car" 
#miningCases <- lapply(myCorpusCopy, 
#        function(x) { grep(as.character(x), pattern = "\\<car")} ) 
#sum(unlist(miningCases))
# count frequency of "cars" 
#minerCases <- lapply(myCorpusCopy, 
#          function(x) {grep(as.character(x), pattern = "\\<cars")} ) 
# replace "cars" with "car" 
#myCorpus <- tm_map(myCorpus, content_transformer(gsub), 
#                   pattern = "cars", replacement = "car")

tdm <- TermDocumentMatrix(myCorpus, 
                          control = list(wordLengths = c(1, Inf))) 

tdm

#> tdm
#<<TermDocumentMatrix (terms: 1572, documents: 1000)>>
#  Non-/sparse entries: 8130/1563870
#Sparsity           : 99%
#Maximal term length: 26
#Weighting          : term frequency (tf)

inspect(tdm)




# inspect frequent words 
(freq.terms <- findFreqTerms(tdm, lowfreq = 15))
##[1] "driverless"      "future"          "google"          "steal"          
##[5] "uber"            "new"             "car"             "see"            
##[9] "vehicles"        "will"            "world"           "need"           
##[13] "road"            "dubai"           "technology"      "electric"       
##[17] "shared"          "via"             "trucks"          "cars"           
##[21] "long"            "people"          "chip"            "graphics"       
##[25] "nvidia"          "specialist"      "working"         "news"           
##[29] "uk"              "vehicle"         "autonomous"      "california"     
##[33] "detail"          "good"            "lots"            "ottolevandowski"
##[37] "piece"           "vs"              "first"           "fury"           
##[41] "selfdriving"     "ai"              "selfdrivingcars" "house"          
##[45] "tech"            "chipmaker"       "paccar"          "says"           
##[49] "study"           "get"             "amp"             "way"            
##[53] "make"            "company"         "impact"          "can"            
##[57] "cover"           "intel"           "mobileye"        "otto"           
##[61] "safety"          "ubers"           "developing"      "us"             
##[65] "heard"           "mobility"        "Â"               "frederick"      
#[69] "lowe"            "poor"            "speed"           "workers"

term.freq <- rowSums(as.matrix(tdm)) 
term.freq <- subset(term.freq, term.freq >= 15) 
df <- data.frame(term = names(term.freq), freq = term.freq)

#library(ggplot2) 
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + 
  xlab("Terms") + ylab("Count") + coord_flip()


# which words are associated with 'car','uber', 'google', etc.? 
findAssocs(tdm, "car", 0.2)
findAssocs(tdm, "uber", 0.2)
findAssocs(tdm, "google", 0.2)
findAssocs(tdm, "future", 0.2)
findAssocs(tmd, "Safty", 0.05)
findAssocs(tmd, "Saft", 0.05)
findAssocs(tmd, "buy", 0.05)


# calculate the frequency of words and sort it by frequency 
# the other way to plot word cloud 
#m <- as.matrix(tdm) 
#word.freq <- sort(rowSums(m), decreasing = T) 

# colors 
#lib:RColorBrewer
#pal <- brewer.pal(9, "BuGn") 
#pal <- pal[-(1:4)]

#wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, 
#          random.order = F, colors = pal)


#clustering but K means

# remove sparse terms 
tdm2 <- removeSparseTerms(tdm, sparse = 0.95) 
m2 <- as.matrix(tdm2) 

# cluster terms 

m3 <- t(m2) # transpose the matrix to cluster documents (tweets) 

set.seed(122) # set a fixed random seed 

k <- 6 # number of clusters 
kmeansResult <- kmeans(m3, k) 
round(kmeansResult$centers, digits = 3) # cluster centers

for (i in 1:k) { 
  cat(paste("cluster ", i, ": ", sep = "")) 
  s <- sort(kmeansResult$centers[i, ], decreasing = T) 
  cat(names(s)[1:5], "\n") 
  # print the tweets of every cluster 
  # print(tweets[which(kmeansResult£cluster==i)]) 
}
