#####################################
## building of unilingual data set ##
#####################################

ifelse(!require("tidyverse"), install.packages("tidyverse"), require(tidyverse))
ifelse(!require("quanteda"), install.packages("quanteda"), require(quanteda))
ifelse(!require("translateR"), install.packages("translateR"), require(translateR))
source("functions/make_corpus.R")

options(stringsAsFactors = FALSE)

################
## translation

# load online data
path_to_file <- url("https://campuscloud.unibe.ch:443/ssf/s/readFile/share/24056/-1422573848356377275/publicLink/data_online.RData")
load(path_to_file)

# create variable to flag duplicates
docs <- mutate(data.ondata, duplicate = ifelse(duplicate_id != d_id & duplicate_id %in% data.ondata$d_id, 1, 0))

# separate data set per language
docs.en <- filter(docs, language == "en")
docs.de <- filter(docs, language == "de")

# make a document-feature matrix (dfm)
corpus.en <- make_online_corpus(docs.en)
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

########################
## assembling data set

## make online dfm ##

# make a dfm for german documents
corpus.de <- make_online_corpus(docs.de)
tokens.de <- tokens(corpus.de, what = 'word', remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE, remove_twitter = TRUE,
                    remove_url = TRUE, remove_numbers = TRUE, remove_hyphens = FALSE, include_docvars = TRUE, verbose = TRUE)
dfm.de <- dfm(tokens.de, tolower = TRUE, stem = FALSE, verbose = TRUE)

# combine german and "english" dfm
dfm.online <- dfm_compress(rbind(dfm.de, dfm.en), margin = "features")

# add docvars again
docvars.online <- rbind(dfm.de@docvars, dfm.en@docvars)
docvars.online <- docvars.online[match(dfm.online@Dimnames$docs, docvars.online$d_id),]
dfm.online@docvars <- docvars.online

# save online dfm (raw)
save(dfm.online, file = "data/dfm_online.RData")

# make online dfm without duplicates
dfm.online.nd <- dfm_subset(dfm.online, duplicate == 0)

# save online dfm withouth duplicates
save(dfm.online.nd, file = "data/dfm_online_nd.RData")

## make offline dfm ##

# load offline data
data_offline <- url("https://campuscloud.unibe.ch:443/ssf/s/readFile/share/24081/-6801514032582368813/publicLink/data_offline.RData")
load(data_offline)

# make a dfm for offline documents
corpus.offline <- make_offline_corpus(data.offdata)
tokens.offline <- tokens(corpus.offline, what = 'word', remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE, remove_twitter = TRUE,
                    remove_url = TRUE, remove_numbers = TRUE, remove_hyphens = FALSE, include_docvars = TRUE, verbose = TRUE)
dfm.offline <- dfm(tokens.offline, tolower = TRUE, stem = FALSE, verbose = TRUE)

# save offline dfm (raw)
save(dfm.offline, file = "data/dfm_offline.RData")

## make combined dfm ##

# combine online and offline dfm
dfm.combined <- dfm_compress(rbind(dfm.online, dfm.offline), margin = "features")

# save combined dfm (raw)
save(dfm.combined, file = "data/dfm_combined.RData")

# combine online and offline dfm
dfm.combined.nd <- dfm_compress(rbind(dfm.online.nd, dfm.offline), margin = "features")

# save combined dfm (raw)
save(dfm.combined.nd, file = "data/dfm_combined_nd.RData")
