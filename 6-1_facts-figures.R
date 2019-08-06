################################
## get some facts and figures ##
################################

ifelse(!require(tidyverse), install.packages("tidyverse"), require(tidyverse))

options(stringsAsFactors = FALSE)

# load data
load("data/data_online_filtered.RData")
load("data/data_offline.RData")

# Number of different actors in online data
n_distinct(pages$organisation)

# Number of unique web pages in online data
pages %>%
  filter(duplicate == 0) %>%
  summarise(n_distinct(d_id))
  
# Number of national (i.e., German actors)
pages %>%
  filter(actor_country == 1) %>%
  summarise(n_distinct(organisation))

# Number of transnational actors (i.e., non-German actors)
pages %>%
  filter(actor_country != 1) %>%
  summarise(n_distinct(organisation))

# Different actor types
pages %>%
  group_by(actor_type) %>%
  summarise(n_distinct(organisation))

# Number of different newspaper/magazines
n_distinct(data.offdata$newspaper)

# Number of articles in every newspaper/magazine
data.offdata %>%
  group_by(newspaper) %>%
  summarise(n_distinct(d_id))

# Total number of articles in offline media
n_distinct(data.offdata$d_id)

# Numer of articles in right-leaning offline media
right_media <- c("BILD", "BILD am Sonntag", "Die Welt", "FAZ", "FAZ am Sonntag", "Financial Times Deutschland", "Focus", "Handelsblatt", "Welt am Sonntag")

data.offdata %>%
  filter(newspaper %in% right_media) %>%
  summarise(n_distinct(d_id))
