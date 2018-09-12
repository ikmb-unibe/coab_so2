#################
## translation ##
#################

ifelse(!require("tidyverse"), install.packages("tidyverse"), require(tidyverse))
ifelse(!require("quanteda"), install.packages("quanteda"), require(quanteda))
ifelse(!require("translateR"), install.packages("translateR"), require(translateR))

options(stringsAsFactors = FALSE)

# load online data
path_to_file <- url("https://campuscloud.unibe.ch:443/ssf/s/readFile/share/24056/-1422573848356377275/publicLink/data_online.RData")
load(path_to_file)

# create variable to flag duplicates
docs <- mutate(data.ondata, duplicate = ifelse(duplicate_id != d_id & duplicate_id %in% data.ondata$d_id, 1, 0))

# separate data set per language
docs.en <- filter(docs, language == "en")

# make a document-feature matrix
make_corpus <- function(data) {
  corp <- corpus(data, docid_field = "d_id", text_field = "text")
  docvars(corp, "d_id") <- data$d_id
  docvars(corp, "page_id") <- data$page_id
  docvars(corp, "crawl") <- data$crawl
  docvars(corp, "language") <- data$language
  docvars(corp, "page_id") <- data$duplicate_id
  docvars(corp, "domain") <- data$domain
  docvars(corp, "organisation") <- data$organisation
  docvars(corp, "actor_type") <- data$actor_type
  docvars(corp, "actor_country") <- data$actor_country
  docvars(corp, "actor_position") <- data$actor_position
  docvars(corp, "cooperation_climate") <- data$cooperation_climate
  docvars(corp, "duplicate") <- data$duplicate
  return(corp)
}
corpus.en <- make_corpus(docs.en)
tokens.en <- tokens(corpus.en, what = 'word', remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE, remove_twitter = TRUE,
                    remove_url = TRUE, remove_numbers = TRUE, remove_hyphens = FALSE, include_docvars = TRUE, verbose = TRUE)
dfm.en <- dfm(tokens.en, tolower = FALSE, stem = FALSE, verbose = TRUE)

# create a data frame with vocabulary for translation
feat.en <- as.data.frame(featnames(dfm.en))
colnames(feat.en) <- 'en_voc'

# translate vocabulary to english to german
source("google_key.R")
translated <- data.frame(en_voc = character(),
                         translated = character())
for(i in 1:nrow(feat.en)) {
  feat <- feat.en[i,1]
  feat.translated <- translate(content.vec = feat, google.api.key = google_key, source.lang = "en", target.lang = "de")
  translated <- rbind(translated,
                      data.frame(en_voc = feat,
                                 translated = feat.translated))
}
save(translated, file = "data/vocab_translated.RData")
