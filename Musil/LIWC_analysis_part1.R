####Note: the following code simply prepares files to be processed with LIWC
####More info and download: https://liwc.wpengine.com/

setwd("./Musil_corpus")
filenames <- list.files(path = ".", pattern="*.txt", full.names=TRUE, recursive = TRUE)

### define extra functions

is.between <- function(x, range=c(a, b)) {
  x > range[1] & x < range[2]
}

reduce_name_length <- function(corpus, chars_lim = 20){
  names_tmp <- names(corpus)
  for(i in 1:length(names_tmp)){
    if(nchar(names_tmp[i])>chars_lim)
      names_tmp[i] <- paste(substr(names_tmp[i], 1, chars_lim), "...", sep = "")
  }
  names(corpus) <- names_tmp
  return(corpus)
}

###Musil cleaning

###select only fiction
selection <- c(1:3, 6, 8, 10, 13:15, 19, 21:23)
filenames <- filenames[selection]

Musil_young_middle <- character()
Musil_old <- character()

for(i in filenames){
  Kolimo_string <- readLines(i)
  ###Normalize text
  ##substitute old S
  Kolimo_string <- gsub("ſ", "s", Kolimo_string)
  ##collapse into single string
  tmp_selection <- paste(Kolimo_string, collapse = "\n")
  if(unlist(strsplit(i, "/"))[2] == "Old")
    Musil_old <- paste(Musil_old, tmp_selection, sep = "\n")
  else
    Musil_young_middle <- paste(Musil_young_middle, tmp_selection, sep = "\n")
}


#####################
###Kolimo Selection

setwd("..")
load("Other_authors_corpus.RData")

range_young=c(1906, 1917)
range_mean=c(1918, 1927)
range_old=c(1928, 1942)

###select only texts published in range (first_work/last_work)
###Young
other_authors_metadata <- full_metadata_withDates
other_authors_subcorp <- Kolimo_texts
range_dates <- range_young
works_in_range <- which(is.between(other_authors_metadata$date, range_dates))
other_authors_metadata <- other_authors_metadata[works_in_range,]
other_authors_subcorp <- other_authors_subcorp[works_in_range]
other_authors_young <- other_authors_subcorp

###select only texts published in range (first_work/last_work)
###Middle Age
other_authors_metadata <- full_metadata_withDates
other_authors_subcorp <- Kolimo_texts
range_dates <- range_mean
works_in_range <- which(is.between(other_authors_metadata$date, range_dates))
other_authors_metadata <- other_authors_metadata[works_in_range,]
other_authors_subcorp <- other_authors_subcorp[works_in_range]
other_authors_mean <- other_authors_subcorp

###select only texts published in range (first_work/last_work)
###Old
other_authors_metadata <- full_metadata_withDates
other_authors_subcorp <- Kolimo_texts
range_dates <- range_old
works_in_range <- which(is.between(other_authors_metadata$date, range_dates))
other_authors_metadata <- other_authors_metadata[works_in_range,]
other_authors_subcorp <- other_authors_subcorp[works_in_range]
other_authors_old <- other_authors_subcorp

###save in files
sink("Other_authors_old_Musil.txt")
for(i in 1:length(other_authors_old)){
  cat(other_authors_old[i], "\n\n")
}
sink()

other_authors_young_mean <- c(other_authors_young, other_authors_mean)
sink("Other_authors_young_mean_Musil.txt")
for(i in 1:length(other_authors_young_mean)){
  cat(other_authors_young_mean[i], "\n\n")
}
sink()

###For Musil
cat(Musil_young_middle, file = "Musil_young_middle.txt")
cat(Musil_old, file = "Musil_old.txt")