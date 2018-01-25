# Model for Shiny Ap

# libraries

library(igraph)
library(dplyr)
library(ngram)
library(quanteda)
library(stringr)


# Load tables

load("one.Rda")
load("two.Rda")
load("three.Rda")
load("four.Rda")
load("five.Rda")
load("six.Rda")

# Input strings for predition

# read in text and select the last 5 words of each

newtext1<-"i just bought a new"
toke<-tokens_tolower(tokens(newtext1, remove_punct=T, remove_numbers=T, remove_symbols=T))
newtext<-as.character(unlist(toke))
newtext<-gsub("'", '', newtext)
if (length(newtext)>5) {
  newtext<-newtext[(length(newtext)-4):length(newtext)]
}


# Find matches

if (length(newtext)>=5) {

findsix<-six %>%
  filter(w1==newtext[1] & w2==newtext[2] & w3==newtext[3] & w4==newtext[4] & w5==newtext[5])

findfive<-five %>%
  filter(w1==newtext[2] & w2==newtext[3] & w3==newtext[4] & w4==newtext[5])

findfour<-four %>%
  filter(w1==newtext[3] & w2==newtext[4] & w3==newtext[5])

findthree<-three %>%
  filter(w1==newtext[4] & w2==newtext[5])

findtwo<-two %>%
  filter(w1==newtext[5])

sixwords<-findsix$w6
fivewords<-findfive$w5
fourwords<-findfour$w4
threewords<-findthree$w3
twowords<-findtwo$w2

words<-unique(c(sixwords, fivewords, fourwords, threewords, twowords))[1:10]

}

if (length(newtext)==4) {
  
  findfive<-five %>%
    filter(w1==newtext[1] & w2==newtext[2] & w3==newtext[3] & w4==newtext[4])
  
  findfour<-four %>%
    filter(w1==newtext[2] & w2==newtext[3] & w3==newtext[4])
  
  findthree<-three %>%
    filter(w1==newtext[3] & w2==newtext[4])
  
  findtwo<-two %>%
    filter(w1==newtext[4])
  
  fivewords<-findfive$w5
  fourwords<-findfour$w4
  threewords<-findthree$w3
  twowords<-findtwo$w2
  
  words<-unique(c(fivewords, fourwords, threewords, twowords))[1:10]
  
}

if (length(newtext)==3) {
  
  findfour<-four %>%
    filter(w1==newtext[1] & w2==newtext[2] & w3==newtext[3])
  
  findthree<-three %>%
    filter(w1==newtext[2] & w2==newtext[3])
  
  findtwo<-two %>%
    filter(w1==newtext[3])
  
  fourwords<-findfour$w4
  threewords<-findthree$w3
  twowords<-findtwo$w2
  
  words<-unique(c(fourwords, threewords, twowords))[1:10]
  
}

if (length(newtext)==2) {
  
  findthree<-three %>%
    filter(w1==newtext[1] & w2==newtext[2])
  
  findtwo<-two %>%
    filter(w1==newtext[2])
  
  threewords<-findthree$w3
  twowords<-findtwo$w2
  
  words<-unique(c(threewords, twowords))[1:10]
}

if (length(newtext)==1) {
  
  findtwo<-two %>%
    filter(w1==newtext[1])
  
  twowords<-findtwo$w2
  
  words<-unique(c(twowords))[1:10]
}


print(noquote(newtext))

print(noquote(unique(words)))

