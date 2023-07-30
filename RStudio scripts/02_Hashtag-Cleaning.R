##########################################
#                                        #
#          Masterarbeit am               #
#  Lehrstuhl für quantitatives Marketing #
#        und Konsumentenverhalten        #
#                                        #
#          (c) 2023, Thomas Frost        #
#           Universität Mannheim         #
#                                        #
#        tfrost@mail.uni-mannheim.de     #
#                                        #
##########################################

# 01 - Library Installs #######################################

install.packages('tidyverse')
install.packages('rlist')

# 02 - Setup ##################################################

library(tidyverse)
library(rlist)

# 03 - Data Import ############################################

posts <- read_delim("data/Instagram__Posts_corrected.tsv", delim = "\t", quote="", col_types = "ccnTccnnccccc")


# 04 - split posts in words ###################################
posts$Words <- strsplit(posts$Text, " ")

# 05 - Hashtag detection ######################################

hashtag_detection <- function(strng) {
  return(substring(strng, 1, 1) == "#")
}

posts$Hashtags <- lapply(posts$Words, hashtag_detection)

# 06 - Count Hashtags #########################################

# Count number of following(!) Hashtags

count_hashtags <- function(liste) {
  counter <- 0
  b <- list()
  listlength <- length(unlist(liste))
  if (is.na(liste[1]) == FALSE) {
    for (i in 1:listlength) {
      if (unlist(liste)[i]) {
        if (counter == 0) {
          counter <- counter + 1
          if (i+1 <= listlength) {
            for (j in (i+1):listlength) {
              if (unlist(liste)[j]) {
                counter <- counter + 1
              } else if (unlist(liste)[j] == FALSE) {
                break
              }
            }
          }
        }
      } else {
        counter <- 0
      }
      b <- list.append(b, counter)
    }
  }
  return(b)
}

posts$NumHashtags <- lapply(posts$Hashtags, count_hashtags)

# Count number of all hashtags in a single post

count_all_hashtags <- function(liste) {
  return(sum(unlist(liste)))
}

posts$TotalNumHashtags <- lapply(posts$Hashtags, count_all_hashtags)

posts$TotalNumHashtags <- unlist(posts$TotalNumHashtags)

# 07 - decide hashtag deletion ################################

deletebin <- function(liste) {
  output <- unlist(liste) >= 2
  return(output)
}

posts$delete <- lapply(posts$NumHashtags, deletebin)

check <- posts[c(6,14,18)]


# 07.5 - plausibility check ###################################

posts$html <- ''

for (i in 1:dim(posts)[1]) {
  text <- ''
  text <- paste(text, '<p>')
  for (j in 1:length(unlist(posts$Words[i]))) {
    if (is.na(unlist(posts$Words[i])[j]) == FALSE) {
      if (unlist(posts$delete[i])[j] == TRUE) {
        text <- paste(text,'<span class="del">', unlist(posts$Words[i])[j], '</span> ')
      } else if (unlist(posts$delete[i])[j] == FALSE) {
        text <- paste(text, unlist(posts$Words[i])[j])
      }
    }
  }
  text <- paste(text, '</p>')
  posts$html[i] <- text
}

output <- as.data.frame(posts$html)
write_csv(output, 'test.html')

# 08 - Rebuild Post Texts #####################################

for (i in 1:dim(posts)[1]) {
  text <- ''
  for (j in 1:length(unlist(posts$Words[i]))) {
    if (is.na(unlist(posts$Words[i])[j]) == FALSE) {
      if (unlist(posts$delete[i])[j] == FALSE) {
        text <- paste(text, unlist(posts$Words[i])[j])
      }
    }
  }
  posts$finaltext[i] <- text
}

# 09 - Export to final dataset file ###########################

export <- posts
export$Text <- export$finaltext
export <- select(export, -c("Words", "Hashtags", "NumHashtags", "delete", "finaltext"))

write_tsv(export, "data/Instagram__Posts_corrected_v5.tsv", na = "")
