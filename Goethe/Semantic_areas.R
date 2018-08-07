library(stylo)
library(tidyverse)
library(reshape2)
library(SnowballC)

load("Goethe_corpus.RData")

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

###prepare Goethe sub-corpus
Goethe_corpus <- list()
for(i in 1:length(Goethe_LateStyle_corpus$PG_de_fulltexts)){
  Goethe_corpus[[i]] <- txt.to.words(Goethe_LateStyle_corpus$PG_de_fulltexts[i])
}
names_tmp <- paste("Goethe", Goethe_LateStyle_corpus$PG_de_texts, Goethe_LateStyle_corpus$first_pub, sep = "_")
names(Goethe_corpus) <- gsub(" ", "", names_tmp)
names(Goethe_corpus) <- gsub(",", "", names_tmp)

###subdivide in young, mean and old
Goethe_young <- Goethe_corpus[which(Goethe_LateStyle_corpus$first_pub <= 1776)]
Goethe_mean <- Goethe_corpus[which(Goethe_LateStyle_corpus$first_pub > 1776 & Goethe_LateStyle_corpus$first_pub < 1809)]
Goethe_old <- Goethe_corpus[which(Goethe_LateStyle_corpus$first_pub >= 1809)]

######
#### Analysis starts
#####

semantic_areas <- list(
  light = c("sonne", "licht", "glanz", "hell", "gold"),
  totality = c("all", "totalität", "ganz", "kosmos"),
  cloud = c("wolke", "nebel"),
  eye = c("auge", "blick", "sehen", "sicht")
)

### Get wordstems
semantic_areas_stems <- lapply(semantic_areas, function(x) wordStem(x))

###prepare main function (alternative with stems and partial matches)

check_occurences_stem_partial <- function(corpus_list){
  ###prepare output frequency list
  semantic_areas_freq <- rep(list(0),length(semantic_areas))
  ###transform corpus into vector of words
  test <- unlist(corpus_list)
  ###calculate total number of words (for normalization)
  total_words <- length(test)
  ###main iteration on semantic areas: for each one, check how many words are present in the corpus
  for(i in 1:length(semantic_areas_stems)){
    for(n in 1:length(semantic_areas_stems[[i]])){
      semantic_areas_freq[[i]][n] <- length(grep(semantic_areas_stems[[i]][n], test))
    }
    print(i)
  }
  ###transform discrete frequencies (for each word) into a single number (for all words)
  semantic_areas_freq <- unlist(lapply(semantic_areas_freq, function(x){sum(x)}))
  ###normalization
  semantic_areas_freq <- semantic_areas_freq/total_words
  return(semantic_areas_freq)
}

##Analysis on Goethe's subcorpora
semantic_areas_freq_young <- check_occurences_stem_partial(Goethe_young)
semantic_areas_freq_mean <- check_occurences_stem_partial(Goethe_mean)
semantic_areas_freq_old <- check_occurences_stem_partial(Goethe_old)

##prepare main data_frame
semantic_areas_full <- data.frame(semantic_areas = names(semantic_areas), Young = semantic_areas_freq_young, Middle = semantic_areas_freq_mean, Old = semantic_areas_freq_old)
semantic_areas_full_melt <- melt(semantic_areas_full, variable.name = "semantic_areas")
colnames(semantic_areas_full_melt) <- c("semantic_areas", "period", "value")

##plot results
p3 <- ggplot(semantic_areas_full_melt) +
  geom_bar(aes(semantic_areas, value, fill = period), stat = "identity", position = "dodge")

ggsave("Goethe_semantic_areas.png", p3, height = 6, width = 10)