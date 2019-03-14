#################
## topic model ##
#################

ifelse(!require(stm), install.packages("stm"), require(stm))
ifelse(!require(tidyverse), install.packages("tidyverse"), require(tidyverse))
source("functions/sample_topdocs.R")

options(stringsAsFactors = FALSE)

######################
# compute topic model

# load data
load("data/dfm_prep.RData")


# determine optimal numbber of topics
determine_k <- searchK(dfm_prep_converted$documents, dfm_prep_converted$vocab, K = c(5,10,15,20,25,30,35,40), cores = 2)

pdf("tm/determine_k.pdf")
plot(determine_k)
dev.off()

# estimate stm (K = 20)
stm_k20 <- selectModel(dfm_prep_converted$documents, dfm_prep_converted$vocab, 
                       K = 20, runs = 40, max.em.its = 300, init.type = "LDA", 
                       seed = 1234, verbose = TRUE)
save(stm_k20, file = "tm/stm_k20.RData")
# estimate stm (K = 30)
stm_k30 <- selectModel(dfm_prep_converted$documents, dfm_prep_converted$vocab, 
                       K = 30, runs = 40, max.em.its = 300, init.type = "LDA", 
                       seed = 1234, verbose = TRUE)
save(stm_k30, file = "tm/stm_k30.RData")

# select best model (k = 20)
plotModels(stm_k20)
stm_k20_sel <- stm_k20$runout[[6]]
save(stm_k20_sel, file = "tm/stm_k20_sel.RData")
# select best model (k = 30)
plotModels(stm_k30)
stm_k30_sel <- stm_k30$runout[[7]]
save(stm_k30_sel, file = "tm/stm_k30_sel.RData")

###############################
# interpretation and labeling

load("tm/stm_k20_sel.RData")
load("tm/stm_k30_sel.RData")
load("data/dfm_prep.RData")

# get list with topic topwords (n = 10)
topwords_k20 <- data.frame(t_id = paste0("t_", 1:20),
                           topwords = apply(sageLabels(stm_k20_sel, n = 10)$marginal$prob, 1, paste, collapse = ", "))
write.csv(topwords_k20, file = "tm/topwords_k20.csv", row.names = FALSE)
topwords_k30 <- data.frame(t_id = paste0("t_", 1:30),
                           topwords = apply(sageLabels(stm_k30_sel, n = 10)$marginal$prob, 1, paste, collapse = ", "))
write.csv(topwords_k30, file = "tm/topwords_k30.csv", row.names = FALSE)

# sample topdocs
model <- stm_k20_sel # which model?
n_docs <- 1 # how many documents?
thresh <- 0.6 # What threshold? (topic probability)
topic <- 1 # which topic?

# Wordcloud anzeigen
cloud(model, topic = topic, max.words = 50)
# Dokumente anzeigen
sample_topdocs(model, dfm_prep_converted$meta$short_text, topic, n_docs, thresh)
  