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

# make a document-feature matrix (dfm)
source("functions/make_corpus.R")
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

# save translation result
save(translated, file = "data/vocab_translated.RData")

# put translated vocabulary back into dfm (all lower case)
translated$translated <- tolower(translated$translated)
dfm.en@Dimnames$features <- translated$translated

# save translated dfm
save(dfm.en, file = "data/dfm_en_translated.RData")
