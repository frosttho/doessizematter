##########################################
#                                        #
#          Masterarbeit am               #
#  Lehrstuhl für quantitatives Marketing #
#        und Konsumentenverhalten        #
#                                        #
#          (c) 2023, Thomas Frost        #
#           Universität Mannheim         #
#                                        #
#                                        #
#        tfrost@mail.uni-mannheim.de     #
#                                        #
##########################################

# 01 - Library Installs #######################################

install.packages('tidyverse')


# 02 - Setup ##################################################

library(tidyverse)

# 03 - Data Import ############################################

dataset <- read_csv("data/finaldataset_v3.csv", col_types = "ccnccnffnnfnf")
profiles <- read_csv2("data/profiles_gender.csv", col_types = "cnfllnff")
liwc <- read_csv("data/finaldataset_v3_LIWC.csv")

# 04 - Merge SiEBERT with LIWC ################################

dataset$uid <- paste(dataset$Username, "_", dataset$Post.ID)
liwc$uid <- paste(liwc$Username, "_", liwc$Post.ID)

merged <- merge(dataset, liwc, by = "uid", all.x = TRUE, sort = FALSE)

merged <- merged[-1]
merged <- merged[-c(14:26)]

colnames(merged)[1:13] <- colnames(dataset)[1:13]

# 05 - Create new sentiment prediction ########################

sentimentliwc <- function(tonenum) {
  if(is.na(tonenum) == FALSE) {
    if(tonenum > 50) {
      r <- "POSITIVE"
    } else {
      r <- "NEGATIVE"
    }
    return(r)
  } else {
    return(NA)
  }
}

merged$sentiment.liwc <- sapply(merged$Tone, sentimentliwc)

table(merged$sentiment.label, merged$sentiment.liwc)

write_csv(merged, "finaldataset_v3_inklLIWC.csv")


# 06 - calculate relative positive shares #####################

merged <- drop_na(merged, sentiment.liwc)

merged$positive <- merged$sentiment.liwc == "POSITIVE"
merged$positive <- merged$positive * 1

merged_by_account <- merged %>%
  group_by(Username) %>%
  summarise(positives = sum(positive), num_posts = n()) %>%
  merge(data.frame(Username = profiles$Username, followers.new = profiles$followers_new, size.new = profiles$size_new), by = "Username", all.y = FALSE, sort = FALSE)

merged_by_account$rel.positives <- merged_by_account$positives / merged_by_account$num_posts

merged_by_account$size.new <- ordered(merged_by_account$size.new, c("nano", "mikro", "midtier", "makro", "mega"))

merged_by_account$rel.negatives <- 1 - merged_by_account$rel.positives

# 07 - Graphen ################################################

a <- ggplot(merged, aes(x = 1:length(Post.ID), y = Tone)) +
  geom_point()

a

b <- ggplot(merged_by_account, aes(x = size.new, y = rel.negatives)) +
  geom_boxplot() +
  xlab("Size classes") +
  ylab("Share of negative posts") +
  ggtitle("Share of negative accounts per account (LIWC)")


b

# 08 - Numbers, please ########################################

cor((merged$sentiment.label == "POSITIVE") * 1, (merged$sentiment.liwc == "POSITIVE") * 1)

## 08.1 - ANOVA ###############################################

anova <- aov(merged_by_account$rel.negatives ~ merged_by_account$size.new)
summary(anova)





