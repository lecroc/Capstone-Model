# Set up Data for Model

# some usefull functions

sizeme<-function(x) {
  print(object.size(x), units="auto")
}

# libraries

if("dplyr" %in% rownames(installed.packages()) == FALSE)
{install.packages("dplyr")}

if("quanteda" %in% rownames(installed.packages()) == FALSE)
{install.packages("quanteda")}

if("ngram" %in% rownames(installed.packages()) == FALSE)
{install.packages("ngram")}

if("ggplot2" %in% rownames(installed.packages()) == FALSE)
{install.packages("ggplot2")}

library(quanteda)
library(ngram)
library(dplyr)
library(ggplot2)

# Check if data file exists, if not, create

ifelse(!file.exists("AllText.Rda"),
       {
        pblog<-"C:/Coursera/Capstone/final/en_US.blogs.txt"
        pnews<-"C:/Coursera/Capstone/final/en_US.news.txt"
        ptweet<-"C:/Coursera/Capstone/final/en_US.twitter.txt"

        # load text into data frames, combine into one df

        blogs<-as.data.frame(readLines(pblog, encoding="UTF-8"))
        names(blogs)<-c("Text")
        blogs$Text<-as.character(blogs$Text)
        blogs$Source<-c("Blogs")
        blogwords<-wordcount(blogs$Text)
        blogdocs<-nrow(blogs)

        news<-as.data.frame(readLines(pnews, encoding="UTF-8"))
        names(news)<-c("Text")
        news$Text<-as.character(news$Text)
        news$Source<-c("News")
        newswords<-wordcount(news$Text)
        newsdocs<-nrow(news)
        
        tweets<-as.data.frame(readLines(ptweet, encoding="UTF-8"))
        names(tweets)<-c("Text")
        tweets$Text<-as.character(tweets$Text)
        tweets$Source<-c("Tweets")
        tweetwords<-wordcount(tweets$Text)
        tweetdocs<-nrow(tweets)
        
        # Use only blogs data for corpus

        AllText<-as.data.frame(rbind(blogs,tweets,news))
        
        # store word and document counts for each text type in counts df
        
        counts<-as.data.frame(cbind(tweetwords, tweetdocs, newswords, newsdocs, blogwords, blogdocs))
        names(counts)<-c("tweetwords", "tweetdocs", "newswords", "newsdocs", "blogwords", "blogdocs")
        counts$tweetwordsperdoc<-counts$tweetwords/counts$tweetdocs
        counts$newswordsperdoc<-counts$newswords/counts$newsdocs
        counts$blogwordsperdoc<-counts$blogwords/counts$blogdocs
        
        # Save dfs just created to disk so we won't need to make them again
        
        save(AllText, file = "AllText.Rda")
        
        save(counts, file = "counts.Rda")
        
        # remove some data objects to manage memory
        
        rm(blogdocs, blogs, blogwords, counts, news, newsdocs, newswords, pblog, pnews, ptweet, tweetdocs, tweets, tweetwords)
        gc(T)
        
        },

load(file = "AllText.Rda"))

load(file = "counts.Rda")


# Create corpus of all blog data

TxtCorpAll <- corpus(AllText$Text)

# remove the big data frame from memory

rm(AllText)

# recover some system memory

gc(T)


# Create document feature matrix (dfm) of uni-grams

Mydfm1 <- dfm(TxtCorpAll, tolower = T, stem = F, remove_punct = T, verbose=F,
              remove_numbers=T, remove = "badwords.txt", remove_symbols=T, ngrams = 1)


tbl1<-textstat_frequency(Mydfm1)

save(tbl1, file="tbl1.Rda")

rm(tbl1, Mydfm1)

gc()


# Create dfm of bi-grams

Mydfm2 <- dfm(TxtCorpAll, tolower = T, stem = F, remove_punct = T, verbose=F,
              remove_numbers=T, remove = "badwords.txt", remove_symbols=T, ngrams = 2)


tbl2<-textstat_frequency(Mydfm2)

save(tbl2, file="tbl2.Rda")

rm(tbl2, Mydfm2)

gc()


# Create dfm of tri-grams

Mydfm3 <- dfm(TxtCorpAll, tolower = T, stem = F, remove_punct = T, verbose=F,
              remove_numbers=T, remove = "badwords.txt", remove_symbols=T, ngrams = 3)

tbl3<-textstat_frequency(Mydfm3)

save(tbl3, file="tbl3.Rda")

rm(tbl3, Mydfm3)

gc()

# Create dfm of four-grams

Mydfm4 <- dfm(TxtCorpAll, tolower = T, stem = F, remove_punct = T, verbose=F,
              remove_numbers=T, remove = "badwords.txt", remove_symbols=T, ngrams = 4)

tbl4<-textstat_frequency(Mydfm4)

save(tbl4, file="tbl4.Rda")

rm(tbl4, Mydfm4)

gc()

# Create dfm of five-grams

Mydfm5 <- dfm(TxtCorpAll, tolower = T, stem = F, remove_punct = T, verbose=F,
              remove_numbers=T, remove = "badwords.txt", remove_symbols=T, ngrams = 5)

tbl5<-textstat_frequency(Mydfm5)

save(tbl5, file="tbl5.Rda")

rm(tbl5, Mydfm5)

gc()

# Create dfm of six-grams

Mydfm6 <- dfm(TxtCorpAll, tolower = T, stem = F, remove_punct = T, verbose=F,
              remove_numbers=T, remove = "badwords.txt", remove_symbols=T, ngrams = 6)

tbl6<-textstat_frequency(Mydfm6)

save(tbl6, file="tbl6.Rda")

rm(tbl6, Mydfm6, TxtCoprAll)

gc()


