library(stylo)
library(tidyverse)
library(reshape2)
library(SnowballC)

###load corpus

setwd("./Kafka_corpus")
filenames <- list.files(path = ".", pattern="*.txt", full.names=TRUE, recursive = TRUE)


###prepare corpus for analysis

Kafka_young <- list()
Kafka_middle <- list()
Kafka_old <- list()

for(i in filenames){
  text <- readLines(i)
  tmp_selection <- list(txt.to.words(text))
  if(unlist(strsplit(i, "/"))[2] == "Old")
    Kafka_old <- c(Kafka_old, tmp_selection)
  if(unlist(strsplit(i, "/"))[2] == "Middle")
    Kafka_middle <- c(Kafka_middle, tmp_selection)
  if(unlist(strsplit(i, "/"))[2] == "Young")
    Kafka_young <- c(Kafka_young, tmp_selection)
}

####prepare semantic areas

semantic_areas <- list(
  artist = tolower(c("Kunst", "Künstlertum")),
  music = tolower(c("Musik", "Klang", "lauschen", "hören", "lauschen", "Melodie", "Zuhörer", "Musikant", "Musiker", "musizieren", "Gesang", "Gesänge", "klagen", "singen", "klingen", "zischen", "Konzert")),
  not_music = tolower(c("Unmusikalität", "Schweigen", "Geräusch", "Summen", "Rascheln", "Stille", "Rieseln", "Zischen", "Zischer", "Pfeifen", "horchen", "Lärm", "tanzen", "Tanz", "Schweigsamkeit",  "still", "piepsen", "Ton", "Töne")),
  disrespect_audience = tolower(c("Ungeschicklichkeit", "Missverständnis", "Unbeholfenheit", "Unfertigkeit", "Auslachen", "Ansehen", "Ärger", "Lachen", "verringern", "Gleichgültigkeit", "Unfähig", "Publikum", "Menge", "Impresario", "Öffentlich", "öffentlichkeit", "Unzufrieden", "Interesse", "bewundern", "Bewunderung", "Vorführung")),
  asceticism = tolower(c("Schlichtheit", "Einfachheit", "Schwach", "Fleisch", "schwächlich", "verkümmern", "einfach", "fasten", "Nahrung", "Essen", "Speise", "hunger", "Bedürfnis", "Existenz", "mager", "abgemagert", "dünn", "ausgehöhlt", "Leib", "verlöscht", "entkräftung", "zerbrechlich", "schonungsbedürftig")),
  self_destruction = tolower(c("Selbstkritik", "Selbstzerstörung", "Opfern", "Vernichtung", "Verzweiflung", "heulen", "Schuld", "unsinnig", "wertlos", "Selbstüberwindung", "Zögern", "weinen", "Scham", "unnütz", "quälen")),
  loneliness_metropolis = tolower(c("Einsamkeit", "Fremdheit", "Kommunikation", "Leer", "ruhig", "einsam", "zurückgezogen", "verzweifelt", "trostlos", "allein", "hoffnungslos", "fremd", "Fremdling", "arm", "erdrückend", "Angst", "lebensunfähig", "vergraben", "melancholisch", "menschenleer", "Tränen"))
)

### Get wordstems
semantic_areas_stems <- lapply(semantic_areas, function(x) wordStem(x))
semantic_areas_stems <- lapply(semantic_areas, function(x) unique(x))

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

##Analysis on Kafka's subcorpora
semantic_areas_freq_young <- check_occurences_stem_partial(Kafka_young)
semantic_areas_freq_mean <- check_occurences_stem_partial(Kafka_middle)
semantic_areas_freq_old <- check_occurences_stem_partial(Kafka_old)

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
ggsave("Kafka_semantic_areas_int_stem_part_NEW.png", p3, height = 6, width = 10)
