################
## Prep Theta ##
################

ifelse(!require(tidyverse), install.packages("tidyverse"), require(tidyverse))
ifelse(!require(quanteda), install.packages("quanteda"), require(quanteda))
ifelse(!require(stm), install.packages("stm"), require(stm))

options(stringsAsFactors = FALSE)

# load data
load("data/data_offline.RData")
load("data/data_online_filtered.RData")
load("data/dfm_prep.RData")
load("tm/stm_k20_sel.RData")
load("data/dfm_offline.RData")
load("data/dfm_online_nd.RData")
labels <- read_csv("tm/topwords_k20.csv")

# get document-topic distribution
theta <- as.data.frame(stm_k20_sel$theta)
# name topic variables
colnames(theta) <- paste0("t_", 1:20)
# sort out trash topics
good_topics <- filter(labels, label != "TRASH")
# sort out trash topics from theta
theta_nt <- select(theta, good_topics$t_id)
# add d_id
theta_id <- cbind(d_id = dfm_prep_converted$meta$d_id, theta_nt)

# make full data set (with duplicates and metadata)
theta_off <- data.offdata %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  mutate(type = "off") %>%
  select(type, d_id, actor = newspaper, month) %>%
  left_join(theta_id, by = "d_id") %>%
  select(-d_id) %>%
  mutate(t_2 = t_2 + t_13) %>%
  select(-t_13) %>%
  group_by(type, actor, month) %>%
  summarise_all(mean, na.rm = TRUE)
theta_off_page <- data.offdata %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  mutate(type = "off") %>%
  select(type, d_id, actor = newspaper, month) %>%
  left_join(theta_id, by = "d_id") %>%
  select(-d_id) %>%
  mutate(t_2 = t_2 + t_13) %>%
  select(-t_13)

crawl_month <- data.frame(crawl = 1:25,
                          month = c(paste("2012", c("06", "07", "08", "09", "10", "11", "12"), sep = "-"),
                                    paste("2013", c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), sep = "-"),
                                    paste("2014", c("01", "02", "03", "04", "05", "06"), sep = "-")))


theta_on_dup <- pages %>%
  filter(duplicate == 1) %>%
  left_join(crawl_month, by = "crawl") %>%
  mutate(type = "on") %>%
  select(type, d_id = duplicate_id, actor = organisation, month) %>%
  left_join(theta_id, by = "d_id")
theta_on_ndup <- pages %>%
  filter(duplicate == 0) %>%
  left_join(crawl_month, by = "crawl") %>%
  mutate(type = "on") %>%
  select(type, d_id, actor = organisation, month) %>%
  left_join(theta_id, by = "d_id")
theta_on <- theta_on_dup %>%
  bind_rows(theta_on_ndup) %>%
  mutate(t_2 = t_2 + t_13) %>%
  select(-t_13, -d_id) %>%
  group_by(type, actor, month) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
theta_on_page <- theta_on_dup %>%
  bind_rows(theta_on_ndup) %>%
  mutate(t_2 = t_2 + t_13) %>%
  select(-t_13, -d_id)
  
save(theta_off, theta_on, theta_off_page, theta_on_page, file = "tm/theta.RData")
