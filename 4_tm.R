#################
## topic model ##
#################

ifelse(!require("stm"), install.packages("stm"), require("stm"))
ifelse(!require("tidyverse"), install.packages("tidyverse"), require(tidyverse))
source("functions/get_topdocs.R")

# load data
load("data/dfm_prep.RData")


# determine optimal numbber of topics
determine_k <- searchK(dfm_prep_converted$documents, dfm_prep_converted$vocab, K = c(5,10,15,20,25,30,35,40), cores = 2)

pdf("tm/determine_k.pdf")
plot(determine_k)
dev.off()

# estimate stm (K = 30)
stm_k30 <- selectModel(dfm_prep_converted$documents, dfm_prep_converted$vocab, 
                       K = 30, runs = 50, max.em.its = 300, init.type = "LDA", 
                       seed = 1234, verbose = TRUE)
save(stm_k30, file = "tm/stm_k30.RData")

# select best model
plotModels(stm_k30)
stm_k30_sel <- stm_k30$runout[[8]]
save(stm_k30_sel, file = "tm/stm_k30_sel.RData")

# get topic topwords (n = 10)
topwords <- data.frame(t_id = paste0("t_", 1:30),
                       topwords = apply(sageLabels(stm_k30_sel, n = 10)$marginal$prob, 1, paste, collapse = ", "))
write.csv(topwords, file = "tm/topwords.csv", row.names = FALSE)

# prepare documents for selection of topdocs
load("data/data_online_filtered.RData")
load("data/data_offline.RData")
texts <- rbind(select(data.offdata, d_id, text),
               select(pages, d_id, text))
texts <- filter(texts, d_id %in% names(dfm_prep_converted$documents))
texts <- texts[match(names(dfm_prep_converted$documents), texts$d_id), ]

# get topdocs (n = 10, thresh = 0.6)
topdocs <- get_topdocs(stm_k30_sel, texts, 10, 0.6)
write.csv(topdocs, file = "tm/topdocs.csv", row.names = FALSE)
