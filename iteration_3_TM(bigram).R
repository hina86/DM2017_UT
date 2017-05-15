library(SnowballC)
library(e1071)
library(quanteda)
library(Matrix)
library(wordcloud)

data <- as.data.frame(read.csv  ("c:/users/hina/documents/Energy_aware_data.csv",header=T,stringsAsFactors=F,sep=','))

data <- as.data.frame(data[,c(2,3,5)])

t<- data$payload_commit_msg

t<- tokens(t, remove_numbers = TRUE, remove_punct = TRUE, remove_hyphens = TRUE, remove_symbols = TRUE, remove_separators = TRUE, remove_twitter = TRUE, remove_url = TRUE )

t <- removeFeatures(t , stopwords("english"))

test<-tokens_skipgrams(t, n = 2, skip = 0:2, concatenator = " ")



fm <- dfm(test, ngrams = 2:2, verbose = FALSE, tolower = TRUE)

fm2 <-dfm_trim(fm, min_docfreq= 5)

vv <- as.matrix(fm2)

tt <-as.data.frame(cbind(vv, data$TRUE_VALUE))

colnames(tt)[colnames(tt)== "V796"] <- "target"

#write.csv(tt,file='c:/users/hina/documents/Energy Aware Commits/project/trigrams.csv',quote = FALSE,row.names = FALSE)


freq = data.frame(sort(colSums(as.matrix(fm2)), decreasing=TRUE))

wordcloud(rownames(freq), freq[,1], max.words=50, colors= palette() , min.freq = 3, scale=c(2,.4), rot.per = .5, random.order = FALSE)


