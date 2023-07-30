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

# 02 - Setup ##################################################

library(tidyverse)
library(colorspace)

set.seed(4120)
sequential_hcl(n = 7, h = 245, c = c(50, 75, NA), l = c(33, 84), power = 0.95, register = "thomascolor")

#03 - Import ##################################################

nano <- read_csv('data/nano-full.csv')
nano2 <- read_csv2('data/ainfluencer_nano_after.csv')
mikro <- read_csv('data/mikro-full.csv')
midtier <- read_csv('data/midtier-full.csv')
makro <- read_csv('data/makro-full.csv')
mega <- read_csv('data/mega-full.csv')

#03.01 - Data Cleaning ########################################

colnames(nano) <- make.names(colnames(nano))
colnames(mikro) <- make.names(colnames(mikro))
colnames(midtier) <- make.names(colnames(midtier))
colnames(makro) <- make.names(colnames(makro))
colnames(mega) <- make.names(colnames(mega))

nano$FlagshipSocialNetwork <- as.factor(nano$FlagshipSocialNetwork)
mikro$FlagshipSocialNetwork <- as.factor(mikro$FlagshipSocialNetwork)
midtier$FlagshipSocialNetwork <- as.factor(midtier$FlagshipSocialNetwork)
makro$FlagshipSocialNetwork <- as.factor(makro$FlagshipSocialNetwork)
mega$FlagshipSocialNetwork <- as.factor(mega$FlagshipSocialNetwork)


#04 - Filtering ###############################################

nano <- filter(nano,
               FlagshipSocialNetwork == 'instagram',
               Instagram.Followers <= 10000,
               IG_Total_Posts >= 20)

nano2 <- filter(nano2,
                yes_no == 'y')
nano2 <- select(nano2, -yes_no)
nano <- rbind(nano, nano2)

mikro <- filter(mikro,
               FlagshipSocialNetwork == 'instagram',
               Instagram.Followers > 10000 & Instagram.Followers <= 50000,
               IG_Total_Posts >= 20)

midtier <- filter(midtier,
                FlagshipSocialNetwork == 'instagram',
                Instagram.Followers > 50000 & Instagram.Followers <= 500000,
                IG_Total_Posts >= 20)

makro <- filter(makro,
                FlagshipSocialNetwork == 'instagram',
                Instagram.Followers > 100000 & Instagram.Followers <= 1000000,
                IG_Total_Posts >= 20)

# Limit of 4,000,000 followers to reduce outliers and extreme cases
mega <- filter(mega,
                FlagshipSocialNetwork == 'instagram',
               Instagram.Followers > 1000000 & Instagram.Followers <= 4000000,
                IG_Total_Posts >= 20)


#05 - Equal distribution inside size classes ##################

#05.01 - Function #############################################
newvar <- function(df, min, max) {
  int <- (max - min)/5
  df$subclass <- ((df$Instagram.Followers - min) %/% int) + 1
  
  df$subclass <- as.factor(df$subclass)
  
  return(df)
}


#05.02 - Application ###########################################

nano <- newvar(nano, 1000, 10000)
mikro <- newvar(mikro, 10000, 50000)
midtier <- newvar(midtier, 50000, 500000)
makro <- newvar(makro, 500000, 1000000)
mega <- newvar(mega, 1000000, 4000000)


#06 - Descriptive Visualizations ##############################

# 06.01 - Boxplot #############################################
#mega_box <- ggplot(mega, aes(y = `Instagram Followers`)) + geom_boxplot()
#mega_box

#
# 06.02. - Bar Charts #########################################
#

nano_bar <- ggplot(nano, aes(y = subclass)) + geom_bar()
nano_bar

mikro_bar <- ggplot(mikro, aes(y = subclass)) + geom_bar()
mikro_bar

midtier_bar <- ggplot(midtier, aes(y = subclass)) + geom_bar()
midtier_bar

makro_bar <- ggplot(makro, aes(y = subclass)) + geom_bar()
makro_bar

mega_bar <- ggplot(mega, aes(y = subclass)) + geom_bar()
mega_bar

# 07 - Create equal distribution in size classes ##############

randsel <- function(df, min, max, n) {
  resultdf <- data.frame(
    FlagshipSocialNetwork = factor(),
    IN_NumOfAccounts = numeric(),
    Instagram.URL = character(),
    Instagram.Followers = numeric(),
    IG_Total_Posts = numeric(),
    subclass = factor())
  
  dist <- as.data.frame(table(df$subclass))
  
  if (min(dist$Freq >= 20)) {
    for (j in 1:5) {
      tempdf <- filter(df, subclass == j)
      #print(as.data.frame(slice_sample(tempdf, n = n/5)))
      resultdf <- rbind(resultdf, as.data.frame(slice_sample(tempdf, n = n/5)))
    }
    
  } else {
    m <- 5
    interval <- n/m
    for (i in order(dist$Freq)) {
      if (nrow(filter(df, subclass == i)) <= interval) {
        resultdf <- rbind(resultdf, filter(df, subclass == i))
        n <- n - nrow(filter(df, subclass == i))
        m <- m - 1
        interval <- ceiling(n/m)
      } else {
        tempdf <- filter(df, subclass == i)
        resultdf <- rbind(resultdf, as.data.frame(slice_sample(tempdf, n = interval)))
      }
    }
  }
  
  return(resultdf)
}

nanosel <- randsel(nano, 1000, 10000, 100)
mikrosel <- randsel(mikro, 1000, 10000, 100)
midtiersel <- randsel(midtier, 1000, 10000, 100)
makrosel <- randsel(makro, 1000, 10000, 100)
megasel <- randsel(mega, 1000, 10000, 100)


# 08 - create full list #######################################


nanosel$size <- 'nano'
mikrosel$size <- 'mikro'
midtiersel$size <- 'midtier'
makrosel$size <- 'makro'
megasel$size <- 'mega'

fulldf <- rbind(nanosel, mikrosel, midtiersel, makrosel, megasel)
fulldf <- filter(fulldf, Instagram.URL != '')
fulldf$Instagram.Account <- str_remove(fulldf$Instagram.URL, 'https://instagram.com/')
fulldf$Instagram.Account <- str_remove(fulldf$Instagram.Account, 'http://www.instagram.com/')

fulldfo <- fulldf[order(fulldf$Instagram.Followers), ]

nmm <- filter(fulldfo, size == 'nano' | size == 'mikro')
nmmdist <- ggplot(nmm, aes( x = 1:length(Instagram.Followers), y = Instagram.Followers, fill = size)) +
  geom_col() +
  xlab('account') +
  ylab('Followers of respectve account in Million')

nmmdist

fulldfo$size[fulldfo$size == "mikro"] <- "micro"
fulldfo$size[fulldfo$size == "makro"] <- "macro"

fulldfo$size <- factor(fulldfo$size, ordered = TRUE, levels = c("nano", "micro", "midtier", "macro", "mega"))

fulldist <- ggplot(fulldfo, aes(x = 1:length(Instagram.Followers), y = Instagram.Followers , fill = size)) +
  geom_col() +
  xlab('account') +
  ylab('Followers of respective account, log_10-scaled') +
  scale_y_log10() +
  theme_light() +
  scale_fill_discrete_sequential(palette = "thomascolor")


fulldist


fulldfexport <- select(fulldf, Instagram.URL, Instagram.Account, Instagram.Followers, size)
write_csv(fulldfexport, 'data/fulldf-export.csv')
