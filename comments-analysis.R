library(readr)
library(dplyr)
library(stringr)

### FUNCTION FOR IMPORTING COMMENTS FILE
# This function imports a file containing People Survey comments. It must be a
# single column file. Function will convert text to lower case.
#
# x <- import_comments("PATHTOFILE") will produce a data frame
#

import_comments <- function(filename) {
  df <- read_csv(filename)
  df[[1]] <- tolower(df[[1]])
  df
}

### FUNCTION FOR COUNTING A PHRASE
# This function counts instances of a given phrase in a data frame
#
# x <- phrase_count(y,"the") counts the number of times the prhase "the" is
# mentioned in data frame y
#

phrase_count <- function(dataset, phrase) {
  string_count <- str_count(dataset[[1]], tolower(phrase))
  sum(string_count, na.rm = TRUE)
}

### FUNCTION FOR PRODUCING A TABLE
# This function prodcues a table of counts of phrases passed to it
#
# x <- phrase_table(y,"the") will return a table with a single row with counts
# of the
#
# x <- phrase_table(y,c("the","and")) will return a table with two rows, one
# showing counts of the word "the", one showing counts of the word "and" in y
#

phrase_table <- function(dataset, phrases) {
  df <- data_frame(phrase = character(), count = integer())
  for (i in phrases) {
    this_count <- phrase_count(dataset, i)
    df <- bind_rows(df,data_frame(phrase = i, count = this_count))
  }
  df
}
