#######################
## Do classification ##
#######################

ifelse(!require(tidyverse), install.packages("tidyverse"), require(tidyverse))
ifelse(!require(SparseM), install.packages("SparseM"), require(SparseM))
ifelse(!require(Matrix), install.packages("Matrix"), require(Matrix))
ifelse(!require(LiblineaR), install.packages("LiblineaR"), require(LiblineaR))
ifelse(!require(tm), install.packages("tm"), require(tm))
ifelse(!require(SnowballC), install.packages("SnowballC"), require(SnowballC))
ifelse(!require(foreach), install.packages("foreach"), require(foreach))
ifelse(!require(hash), install.packages("hash"), require(hash))
ifelse(!require(data.table), install.packages("data.table"), require(data.table))
ifelse(!require(tidytext), install.packages("tidytext"), require(tidytext))

source("functions/do_classification.R")

# Parallelization (requires Linux)
ifelse(!require(doMC), install.packages("doMC"), require(doMC))
registerDoMC(4)

options(stringsAsFactors = FALSE)

# load data
load("data/data_offline.RData")
load("data/data_online_filtered.RData")

# prepare data: get sentences, separate languages
off_sent <- unnest_tokens(data.offdata, output = sentence, input = text, token = "sentences", 
                          to_lower = FALSE, strip_punct = FALSE)

on_sent_de <- pages %>%
  filter(language == "de") %>%
  unnest_tokens(output = sentence, input = text, token = "sentences", 
                to_lower = FALSE, strip_punct = FALSE)
on_sent_en <- pages %>%
  filter(language == "en") %>%
  unnest_tokens(output = sentence, input = text, token = "sentences", 
                to_lower = FALSE, strip_punct = FALSE)

# do classification: offline data
LANGUAGE <- "de"

source("classifier/set_properties.R")
source("classifier/functions_for_classification.R")

load(final_svm_model_file)
load(final_svm_model_featurelist)

off_sent_class <- do_classification(off_sent)

save(off_sent_class, file = "data/off_sent_class.RData")

# do classification: online data de
LANGUAGE <- "de"

source("classifier/set_properties.R")
source("classifier/functions_for_classification.R")

load(final_svm_model_file)
load(final_svm_model_featurelist)

on_sent_de_class <- do_classification(on_sent_de)

save(on_sent_de_class, file = "data/on_sent_de_class.RData")

# do classification: online data en
LANGUAGE <- "en"

source("classifier/set_properties.R")
source("classifier/functions_for_classification.R")

load(final_svm_model_file)
load(final_svm_model_featurelist)

on_sent_en_class <- do_classification(on_sent_en)

save(on_sent_en_class, file = "data/on_sent_en_class.RData")
