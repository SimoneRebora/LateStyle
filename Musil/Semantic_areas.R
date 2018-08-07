library(stylo)
library(tidyverse)
library(reshape2)
library(SnowballC)

###load corpus

setwd("./Musil_corpus")
filenames <- list.files(path = ".", pattern="*.txt", full.names=TRUE, recursive = TRUE)

###select only fiction
selection <- c(1:3, 6, 8, 10, 13:15, 19, 21:23)
filenames <- filenames[selection]

###prepare corpus for analysis

Musil_young <- list()
Musil_middle <- list()
Musil_old <- list()

for(i in filenames){
  text <- readLines(i)
  tmp_selection <- list(txt.to.words(text))
  if(unlist(strsplit(i, "/"))[2] == "Old")
    Musil_old <- c(Musil_old, tmp_selection)
  if(unlist(strsplit(i, "/"))[2] == "Middle")
    Musil_middle <- c(Musil_middle, tmp_selection)
  if(unlist(strsplit(i, "/"))[2] == "Young")
    Musil_young <- c(Musil_young, tmp_selection)
}

####prepare semantic areas

semantic_areas <- list(
  tree_of_violence = tolower(c("Seinesgleichen", "Muskeln", "Scharf", "nervig", "schlank", "unpersönlich", "anonym", "Härte", "hart", "Gewalt", "Selbstliebe", "Ichsucht", "Angriff", "Herrschaft", "Streben", "Fehlen", "Mangel", "konvex", "erstarrt", "teilnahmslos", "fremd", "kalt", "gefühllos", "erfroren", "versteint")),
  tree_of_love = tolower(c("Sanftmut", "Mystik", "Weich", "zusammengesunken", "sinken", "einsinken", "gesunken", "umgestülpt", "eingesenkt", "zauberhaft", "Übernatürliche", "Zauberwelt", "Signal", "weich", "zart", "gut", "gekehrt", "umkehren", "Süße", "süß", "Umarmt", "umschlossen", "angenehm", "Liebe", "Nächstenliebe", "Schattenhaft", "Traum", "kindhaft", "sanft", "Vertrauen", "Hingabe", "Schattenseite", "Ahnung", "ahnenhaft", "voll", "Fülle", "Umschlossensein", "umschliessen", "umschlossen", "konkav", "Inversion", "Umkehr", "Gleichnis"))
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
semantic_areas_freq_young <- check_occurences_stem_partial(Musil_young)
semantic_areas_freq_mean <- check_occurences_stem_partial(Musil_middle)
semantic_areas_freq_old <- check_occurences_stem_partial(Musil_old)

##############
#### Internal analysis
###########

##prepare main data_frame
semantic_areas_full <- data.frame(semantic_areas = names(semantic_areas), Young = semantic_areas_freq_young, Middle = semantic_areas_freq_mean, Old = semantic_areas_freq_old)

semantic_areas_full_melt <- melt(semantic_areas_full, variable.name = "semantic_areas")
colnames(semantic_areas_full_melt) <- c("semantic_areas", "period", "value")

##plot results
p3 <- ggplot(semantic_areas_full_melt) +
  geom_bar(aes(semantic_areas, value, fill = period), stat = "identity", position = "dodge")

setwd("..")
ggsave("Musil_semantic_areas_int_stem_part.png", p3, height = 6, width = 10)
