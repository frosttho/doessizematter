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
install.packages("quanteda")
install.packages("ldaPrototype")
install.packages("Twitmo")
install.packages("textmineR")
install.packages("LDAvis")


# 02 - Setup ##################################################

set.seed(123)

library(tidyverse)
library(readxl)
library(quanteda)
library(ldaPrototype)
library(Twitmo)
library(textmineR)
library(LDAvis)
library(Matrix)
library(slam)
library(stringi)

# 03 - Data Import ############################################

posts <- read_delim("data/Instagram__Posts_corrected_SiEBERT-input.tsv", delim = "\t", quote="", col_types = "ccncccnncccfcnc")
posts <- select(posts, c("Username (shortcode)", "Post ID", "Text"))
lemma <- read.delim("lemmatization-en.txt", sep = "\t")

posts <- posts[-which(duplicated(posts$`Post ID`)),]                   # remove duplicates
posts$Text <- gsub("\\\\n", ' ', posts$Text)                           # replace \n with space
posts <- posts[-which(is.na(posts$Text)),]

posts$Text <- stri_enc_toutf8(posts$Text)

# 04 - Preprocessing ##########################################

# 04.01 Tokenization

posttexts <- as.vector(posts$Text)
posttexts <- setNames(posttexts, posts$`Post ID`)

tok <- tokens(posttexts, split_tags = TRUE)

# 04.02 Convert to lowercase
tok <- tokens_tolower(tok)


# 04.03 Remove punctuation
tok <- tokens(tok, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)


# 04.04 Remove Stop words
tok <- tokens_remove(tok, pattern = c(stopwords("en"), "n", "i’m"))

# 04.05 Lemmatization
colnames(lemma) <- c("base", "variant")
lemma$base <- tolower(lemma$base)
lemma$variant <- tolower(lemma$variant)

lemma <- lemma[-which(lemma$variant == "data"),]          # don't change "data" to "datum"
lemma <- lemma[-which(lemma$variant == "sentencing"),]    # don't change "sentencing" to "sentence"
lemma <- lemma[!duplicated(lemma$variant),]               # remove duplicates

tok_lemmatized <- tokens_replace(tok, lemma$variant, lemma$base)

DFM <- dfm(tok_lemmatized)



# 04.06 Pruning ###############################################
# 04.06.01 Effect of Pruning ##################################

# How many words are included if words are excluded that don't appear at least count times?
get_countaffected <- function(dfm, count) {
  trimmed <- dfm_trim(dfm, min_termfreq = count)
  return(nfeat(trimmed))
}

# Ho many words are included if words are excluded that don't appear in at least prop% of the documents
get_propaffected <- function(dfm, prop) {
  trimmed <- dfm_trim(dfm, min_docfreq = prop, docfreq_type = "prop")
  return(nfeat(trimmed))
}

affected_count <- data.frame(count = 1:10)
affected_prop <- data.frame(prop = seq(0.00001, 0.001, by = 0.00002))
affected_count$affected <- sapply(X = affected_count$count, FUN = get_countaffected, dfm = DFM)
affected_prop$affected  <- sapply(X = affected_prop$prop, FUN = get_propaffected, dfm = DFM)

plot_countaffected <- ggplot(affected_count, aes(x = count, y = affected)) +
  geom_line() +
  xlab("Minimum occurence of word in documents") +
  ylab("vocabulary size") +
  ggtitle("Effect of absolute pruning") +
  scale_x_continuous(minor_breaks = seq(1, 25, by = 1))
plot_countaffected

plot_propaffected <- ggplot(affected_prop, aes(x = prop, y = affected)) +
  geom_line() +
  xlab("occurence in at least x% of documents") +
  ylab("vocabulary size") +
  ggtitle("Effect of relative pruning")
  #scale_x_continuous(minor_breaks = seq(0, 25, by = 1))
plot_propaffected



# 04.06.02 Execution ##########################################

DFM_trimmed <- dfm_trim(DFM, min_termfreq = 2)

topfeatures(DFM_trimmed)

dfm_matrix <- convert(DFM_trimmed, to = "lda")


# 05 - Model Generation #######################################

K_range <- seq(4, 35, 1)                                             # Adapted from Bittermann (2022a)
candidate_models <- list()
model_iter <- 100

for (i in 1:length(K_range)){
  print(paste(i, " / ", length(K_range)))
  candidate_models[[i]] <- FitLdaModel(dtm = DFM_trimmed,
                                       k = K_range[i],
                                       iterations = model_iter,
                                       alpha = 1/K_range[i],
                                       optimize_alpha = TRUE)
}



# 06 - LDAvis #################################################

# jsona <- createJSON(phi = model$phi, theta = model$theta, doc.length = as.vector(ntoken(DFM_trimmed)), vocab = colnames(model$phi), term.frequency = TermDocFreq(model$data)$term_freq)
# serVis(jsona)


# 07 -  Coherence and Exclusivity #############################

exclusivity <- function(model, dfm, num.words) {                     # Adapted from Bittermann (2022a)
  phi <- model$phi
  p_w <- colSums(dfm)/sum(dfm)
  
  res <- apply(phi, 1, function(x, num.words, pw) {
    x <- log(x/pw)
    return((sort(x, decreasing = TRUE)[1:num.words]))
  }, num.words, p_w)
  
  return(colMeans(res))
}

coherence <- function(model, DFM, N = 10) {                          # Adapted from Wiedemann and Niekler (2017)
  
  # Ensure matrix or Matrix-format (convert if SparseM)
  require(Matrix)
  require(slam)
  if (is.simple_triplet_matrix(DFM)) {
    DFM <- sparseMatrix(i=DTM$i, j=DTM$j, x=DTM$v, dims=c(DTM$nrow, DTM$ncol), dimnames = dimnames(DTM))
  }
  
  DTMBIN <- DFM
  DTMBIN[DTMBIN > 0] <- 1
  
  documentFrequency <- colSums(DTMBIN)
  names(documentFrequency) <- colnames(DTMBIN)
  
  K <- dim(model$phi)[1]
  
  topNtermsPerTopic <- GetTopTerms(model$phi, M = N)
  allTopicModelTerms <- unique(as.vector(topNtermsPerTopic))
  
  DTMpreprocessed <- DTMBIN[, allTopicModelTerms]
  DTMpreprocessedCooc <- t(DTMpreprocessed) %*% DTMpreprocessed
  DTMpreprocessedCooc <- t((DTMpreprocessedCooc + 1) / colSums(DTMpreprocessed))
  DTMpreprocessedCooc <- log(DTMpreprocessedCooc)
  DTMpreprocessedCooc <- as.matrix(DTMpreprocessedCooc)
  
  coherence <- rep(0, K)
  pb <- txtProgressBar(max = K)
  for (topicIdx in 1:K) {
    setTxtProgressBar(pb, topicIdx)
    topWordsOfTopic <- topNtermsPerTopic[, topicIdx]
    coherence[topicIdx] <- 0
    for (m in 2:length(topWordsOfTopic)) {
      for (l in 1:(m-1)) {
        mTerm <- topWordsOfTopic[m]
        lTerm <- topWordsOfTopic[l]
        coherence[topicIdx] <- coherence[topicIdx] + DTMpreprocessedCooc[mTerm, lTerm]
      }
    }
  }
  close(pb)
  return(coherence)
}
  
  
  
exc <- lapply(candidate_models, exclusivity, DFM_trimmed, num.words = 10)
exc_mean <- unlist(lapply(exc, mean))

coh <- lapply(candidate_models, coherence, DFM_trimmed, N = 10)
coh_mean <- unlist(lapply(coh, mean))


graph_df <- data.frame(scale(exc_mean), scale(coh_mean))
graph_df$mean <- apply(graph_df, 1, mean)
graph_df$x <- min(K_range):max(K_range)
colnames(graph_df) <- c("excl", "cohe", "mean", "x")

graph <- ggplot(graph_df, aes(y = exc_mean)) +
  geom_line(aes(x = x, y = excl, colour = "exclusivity"), linewidth = 1) +
  geom_line(aes(x = x, y = cohe, colour = "coherence"), linewidth = 1) +
  geom_line(aes(x = x, y = mean, colour = "mean"), linewidth = 1) +
  scale_color_manual(name = "Quality Metrics", values = c("exclusivity" = "darkblue", "coherence" = "darkred", "mean" = "darkgreen")) +
  ggtitle("Topic Quality Metrics per post") +
  xlab("number of topics K") +
  ylab("Scaled Score") +
  scale_x_continuous(breaks = seq(min(graph_df$x), max(graph_df$x), by = 2)) +
  theme_light()

graph




# 08 - Manual Labeling of Topics ##############################          
                                                                     # adapted from Bittermann (2022a)

# post basis
select <- K_range %in% c(29)
candidates_inspect <- candidate_models[select]

for (i in 1:length(candidates_inspect)){
  print(paste("### Topics for k =", dim(candidates_inspect[[i]]$phi)[1], "###"))
  print(GetTopTerms(candidates_inspect[[i]]$phi, 10))
}

final_model <- candidates_inspect[[1]]

labels <- c("REMOVE",
            "Horoscope",
            "REMOVE",
            "Media Content",
            "TV and Series",
            "Football",
            "Giveaway",
            "TV and Series",
            "REMOVE",
            "Fashion",
            "Cooking",
            "Communities",
            "Sports and Fitness",
            "Hair",
            "Cooking",
            "REMOVE",
            "Cooking",
            "Skin and Beauty",
            "Horoscope",
            "Shopping Promo",
            "Nutrition and Lifestyle",
            "Baby and Family",
            "Baby and Family",
            "REMOVE",
            "Nutrition and Lifestyle",
            "Shopping Promo",
            "Shopping Promo",
            "REMOVE",
            "Happy / Celebrations"
            )

theta <- as.data.frame(final_model$theta)
theta <- theta[,-which(labels == "REMOVE")]

max_theta <- apply(theta, 1, function(x){which.max(x)})
max_theta <- as.data.frame(max_theta)
max_theta$label <- labels[-which(labels == "REMOVE")][max_theta$max_theta]


plot_max_theta <- ggplot(max_theta, aes(x = label)) +
  geom_bar() +
  ggtitle("Topic Distribution on post level (k = 26)") +
  xlab("Topic Label") +
  ylab("Number of posts") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

plot_max_theta


# 10 - Finally export this data for insertion #################

# POST
max_theta$Post.ID <- rownames(max_theta)
rownames(max_theta) <- 1:dim(max_theta)[1]
max_theta <- max_theta[,-1]

write_csv(max_theta, 'data/study2_topics_posts.csv')



