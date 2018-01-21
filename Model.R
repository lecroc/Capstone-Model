# libraries

library(igraph)
library(dplyr)
library(ngram)
library(quanteda)
library(stringr)

# load data files

load("tbl1.Rda")
load("tbl2.Rda")
load("tbl3.Rda")
load("tbl4.Rda")
load("tbl5.Rda")
load("tbl6.Rda")

# Get rid of ngrams with freq<5

one<-tbl1 %>%
  filter(frequency>5)

two<-tbl2 %>%
  filter(frequency>5)

three<-tbl3 %>%
  filter(frequency>5)

four<-tbl4 %>%
  filter(frequency>5)

five<-tbl5 %>%
  filter(frequency>5)

six<-tbl6 %>%
  filter(frequency>5)

one<-one[,1:2]

two<-two[,1:2]

three<-three[,1:2]

four<-four[,1:2]

five<-five[,1:2]

six<-six[,1:2]

rm(tbl1, tbl2, tbl3, tbl4, tbl5, tbl6)

gc()

# functions

first.word <- function(my.string){
  unlist(strsplit(my.string, "_"))[1]
}

second.word <-function(my.string){
  unlist(strsplit(my.string, "_"))[2]
}

third.word<-function(my.string){
  unlist(strsplit(my.string, "_"))[3]
}

fourth.word<-function(my.string){
  unlist(strsplit(my.string, "_"))[4]
}

fifth.word<-function(my.string){
  unlist(strsplit(my.string, "_"))[5]
}

sixth.word<-function(my.string){
  unlist(strsplit(my.string, "_"))[6]
}

sizeme<-function(x){
  print(object.size(x), units="auto")
}


two$w1<-sapply(two$feature, first.word)
two$w2<-sapply(two$feature, second.word)
two<-two[,2:4]

three$w1<-sapply(three$feature, first.word)
three$w2<-sapply(three$feature, second.word)
three$w3<-sapply(three$feature, third.word)
three<-three[,2:5]

four$w1<-sapply(four$feature, first.word)
four$w2<-sapply(four$feature, second.word)
four$w3<-sapply(four$feature, third.word)
four$w4<-sapply(four$feature, fourth.word)
four<-four[,2:6]

five$w1<-sapply(five$feature, first.word)
five$w2<-sapply(five$feature, second.word)
five$w3<-sapply(five$feature, third.word)
five$w4<-sapply(five$feature, fourth.word)
five$w5<-sapply(five$feature, fifth.word)
five<-five[,2:7]

six$w1<-sapply(six$feature, first.word)
six$w2<-sapply(six$feature, second.word)
six$w3<-sapply(six$feature, third.word)
six$w4<-sapply(six$feature, fourth.word)
six$w5<-sapply(six$feature, fifth.word)
six$w6<-sapply(six$feature, sixth.word)
six<-six[,2:8]

# Save tables

save(one, file="one.Rda")
save(two, file="two.Rda")
save(three, file="three.Rda")
save(four, file="four.Rda")
save(five, file="five.Rda")
save(six, file="six.Rda")

# Input strings for predition

# read in text and select the last 5 words of each

newtext1<-"i don't give a"
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

