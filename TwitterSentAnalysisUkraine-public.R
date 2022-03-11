
library(rtweet)
library(ROAuth)
require(RCurl)
library(stringr)
library(ggmap)
library(plyr)
library(dplyr)
library(tm)
library(wordcloud)
library(syuzhet)
library(tidyr)


key="hidden"
secret="hidden"

access_token="hidden"
access_secret="hidden"
token <- create_token(
  app = "hidden",
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)

#get_token()

#testing
#?search_tweets
#rt <- search_tweets("Ukraine", n =1, lang="en")
#usrs <- search_users("#rstats", n = 1000)




N=500  # tweets to request from each query
S=200  # radius in miles
lats=c(38.9,40.7,37.8,39,37.4,28,30,42.4,48,36,32.3,33.5,34.7,33.8,37.2,41.2,46.8,
       46.6,37.2,43,42.7,40.8,36.2,38.6,35.8,40.3,43.6,40.8,44.9,44.9)

lons=c(-77,-74,-122,-105.5,-122,-82.5,-98,-71,-122,-115,-86.3,-112,-92.3,-84.4,-93.3,
       -104.8,-100.8,-112, -93.3,-89,-84.5,-111.8,-86.8,-92.2,-78.6,-76.8,-116.2,-98.7,-123,-93)

#cities=DC,New York,San Fransisco,Colorado,Mountainview,Tampa,Austin,Boston,
#       Seatle,Vegas,Montgomery,Phoenix,Little Rock,Atlanta,Springfield,
#       Cheyenne,Bisruk,Helena,Springfield,Madison,Lansing,Salt Lake City,Nashville
#       Jefferson City,Raleigh,Harrisburg,Boise,Lincoln,Salem,St. Paul


Ukraine=do.call(rbind,lapply(1:length(lats), function(i) search_tweets2('Ukraine',
                                                                      lang="en",n=N,type="recent",
                                                                      geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","))))


Ukrainedf<-data.frame(Ukraine)
str(Ukrainedf)
Ukrainedf$location[1000]

vector = Ukrainedf$text
Corpus <- Corpus(VectorSource(vector))
Corpus = tm_map(Corpus,removeNumbers)
Corpus = tm_map(Corpus,str_replace_all,pattern = "http\\w+", replacement =" ")
Corpus = tm_map(Corpus,str_replace_all,pattern = "<.*?>", replacement =" ")
Corpus = tm_map(Corpus,str_replace_all,pattern = "@\\w+", replacement =" ")
Corpus = tm_map(Corpus,str_replace_all,pattern ="\\=", replacement =" ")
Corpus = tm_map(Corpus,str_replace_all,pattern = "[[:punct:]]", replacement =" ")
Corpus = tm_map(Corpus,str_replace_all,pattern = "amp", replacement =" ")
Corpus = tm_map(Corpus,removeWords, words= stopwords("en"))
Corpus = tm_map(Corpus,tolower)
Corpus = tm_map(Corpus,stripWhitespace)


tdm = TermDocumentMatrix(Corpus)
tdm
#review tdm structure
wordcloud(words = Corpus, 
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#review world cloud as ouput



UkraineSearch<-as.vector(Ukrainedf$text)
cleanTweet = gsub("rt|RT", "", UkraineSearch) # remove Retweet
cleanTweet = gsub("http\\w+", "", cleanTweet)  # remove links http
cleanTweet = gsub("<.*?>", "", cleanTweet) # remove html tags
cleanTweet = gsub("@\\w+", "", cleanTweet) # remove at(@)
cleanTweet = gsub("[[:punct:]]", "", cleanTweet) # remove punctuation
cleanTweet  = gsub("\r?\n|\r", " ", cleanTweet) # remove /n
cleanTweet = gsub("[[:digit:]]", "", cleanTweet) # remove numbers/Digits
cleanTweet = gsub("㠼|㸵|㤼|㸲|㸱|㸳|㸴|㸶|攼|㹤", "", cleanTweet) #  asian letters
cleanTweet = gsub("[ |\t]{2,}", "", cleanTweet) # remove tabs
cleanTweet = gsub("^ ", "", cleanTweet)  # remove blank spaces at the beginning
cleanTweet = gsub(" $", "", cleanTweet) # remove blank spaces at the end 

Ukraine_Sentiment = get_nrc_sentiment(cleanTweet)
head(Ukraine_Sentiment,5)
plotData1 =gather(Ukraine_Sentiment,"sentiment","values",1:8)  %>% 
  group_by( sentiment) %>%
  summarise(Total = sum(values))
ggplot(data = plotData1, aes(x = plotData1$sentiment, y = plotData1$Total)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Emotions") + ylab("Total") + ggtitle("Emotion for Search Ukraine in Big US cities")+
  geom_text(aes(label =   plotData1$Total), position = position_dodge(width=0.75), vjust = -0.25)
#review input from plot 

plotData2 =gather(Ukraine_Sentiment,"Polarity","values",9:10)  %>% 
  group_by( Polarity) %>%
  summarise(Total = sum(values))

ggplot(data = plotData2, aes(x = plotData2$Polarity, y = plotData2$Total)) +
  geom_bar(aes(fill = plotData2$Polarity), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total") + ggtitle("Sentiment for Search Ukraine in Big US cities")+
  geom_text(aes(label =   plotData2$Total), position = position_dodge(width=0.75), vjust = -0.25)

#review input from plot
