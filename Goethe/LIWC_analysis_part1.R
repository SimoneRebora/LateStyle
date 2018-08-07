####Note: the following code simply prepares files to be processed with LIWC
####More info and download: https://liwc.wpengine.com/

library(stylo)
load("Goethe_corpus.RData")
load("Other_authors_corpus.RData")

##########
#####Goethe corpus preparation
##########

###Uniform spelling (but keep umlauts and double S)
for(i in 1:length(Goethe_LateStyle_corpus$PG_de_fulltexts)){
  Kolimo_string <- Goethe_LateStyle_corpus$PG_de_fulltexts[i]
  ##substitute old S
  Kolimo_string <- gsub("ſ", "s", Kolimo_string)
  ##substitute umlauts and double S
  # Kolimo_string <- gsub("ä", "ae", Kolimo_string)
  # Kolimo_string <- gsub("ö", "oe", Kolimo_string)
  # Kolimo_string <- gsub("ü", "ue", Kolimo_string)
  # Kolimo_string <- gsub("ß", "ss", Kolimo_string)
  
  ##substitute line breaks
  Kolimo_string <- gsub("([a-z])-\\s+([a-z])", "\\1\\2", Kolimo_string)
  Kolimo_string <- gsub("([a-z])¬\\s+([a-z])", "\\1\\2", Kolimo_string)
  
  ##save new version
  Goethe_LateStyle_corpus$PG_de_fulltexts[i] <- Kolimo_string
  print(i)
}

###subdivide in young, mean and old
Goethe_young <- Goethe_LateStyle_corpus$PG_de_fulltexts[which(Goethe_LateStyle_corpus$first_pub <= 1776)]
Goethe_mean <- Goethe_LateStyle_corpus$PG_de_fulltexts[which(Goethe_LateStyle_corpus$first_pub > 1776 & Goethe_LateStyle_corpus$first_pub < 1809)]
Goethe_old <- Goethe_LateStyle_corpus$PG_de_fulltexts[which(Goethe_LateStyle_corpus$first_pub >= 1809)]

##########
#####Other authors corpus preparation
##########

###Uniform spelling (but keep umlauts and double S)
for(i in 1:length(other_authors_corpus$text)){
  tmp_string <- other_authors_corpus$text[i]
  ##substitute old S
  tmp_string <- gsub("ſ", "s", tmp_string)
  ##substitute umlauts and double S
  # tmp_string <- gsub("ä", "ae", tmp_string)
  # tmp_string <- gsub("ö", "oe", tmp_string)
  # tmp_string <- gsub("ü", "ue", tmp_string)
  # tmp_string <- gsub("ß", "ss", tmp_string)
  
  ##substitute line breaks
  tmp_string <- gsub("([a-z])-\\s+([a-z])", "\\1\\2", tmp_string)
  tmp_string <- gsub("([a-z])¬\\s+([a-z])", "\\1\\2", tmp_string)
  
  ##save new version
  other_authors_corpus$text[i] <- tmp_string
  print(i)
}

###subdivide in young, mean and old
other_authors_young <- other_authors_corpus$text[which(other_authors_corpus$date <= 1776)]
other_authors_mean <- other_authors_corpus$text[which(other_authors_corpus$date > 1776 & other_authors_corpus$date < 1809)]
other_authors_old <- other_authors_corpus$text[which(other_authors_corpus$date >= 1809)]

###export datasets

##Goethe
sink("Goethe_young_mean.txt")
for(i in 1:length(Goethe_young)){
  cat(Goethe_young[i], "\n\n")
}

for(i in 1:length(Goethe_mean)){
  cat(Goethe_mean[i], "\n\n")
}
sink()

sink("Goethe_old.txt")
for(i in 1:length(Goethe_old)){
  cat(Goethe_old[i], "\n\n")
}
sink()

##Others

sink("Other_authors_old.txt")
for(i in 1:length(other_authors_old)){
  cat(other_authors_old[i], "\n\n")
}
sink()

sink("Other_authors_young_mean.txt")
for(i in 1:length(other_authors_young)){
  cat(other_authors_young[i], "\n\n")
}
for(i in 1:length(other_authors_mean)){
  cat(other_authors_mean[i], "\n\n")
}
sink()