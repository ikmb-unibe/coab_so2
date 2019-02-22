####################################
## building of skeptics data set  ##
####################################

ifelse(!require("tidyverse"), install.packages("tidyverse"), require(tidyverse))
ifelse(!require("quanteda"), install.packages("quanteda"), require(quanteda))
source("functions/make_corpus.R")

options(stringsAsFactors = FALSE)

# load data
load("data/data_online.RData")
load("data/data_online_links.RData")
media_coded <- read_csv("data/media_coded.csv")

# recode position
# 1 = skeptic
# 2 = alarmist
# 99 = unknown
link_data <- data.ondata.links %>%
  mutate(s_position_new = case_when(s_actor_position == 2 ~ 2,
                                    s_actor_position == 10 ~ 1,
                                    s_actor_position == 11 ~ 1,
                                    s_actor_position == 3 ~ 99,
                                    s_actor_position == 4 ~ 99,
                                    s_actor_position == 99 ~ 99)) %>%
  mutate(t_position_new = case_when(t_actor_position == 2 ~ 2,
                                    t_actor_position == 10 ~ 1,
                                    t_actor_position == 11 ~ 1,
                                    t_actor_position == 3 ~ 99,
                                    t_actor_position == 4 ~ 99,
                                    t_actor_position == 99 ~ 99))

# get skeptical legacy media
media_s <- link_data %>%
  filter(s_organisation %in% unique(data.ondata$organisation)) %>%
  filter(s_actor_type == 4, legacy == 1) %>%
  filter(s_position_new == 1)
media_t <- link_data %>%
  filter(t_organisation %in% unique(data.ondata$organisation)) %>%
  filter(t_actor_type == 4) %>%
  filter(t_position_new == 1)
media <- data.frame(organisation = c(media_s$s_organisation, media_t$t_organisation),
                    domain = c(media_s$s_domain, media_t$t_domain)) %>%
  group_by(organisation, domain) %>%
  summarise(n()) %>%
  select(organisation, domain)

write_csv(media, path = "data/media_to_code.csv")

# simulating first step of crawler (seed -> linked organisations)
skept_seeds <- c("Analyse + Aktion", "Klimaüberraschung", "EIKE - Europäisches Institut für Klima und Energie", "klimaskeptiker.info")

linked_by_seed <- link_data %>%
  left_join(media_coded, by = c("t_domain" = "domain")) %>%
  filter(s_organisation %in% unique(data.ondata$organisation)) %>% # remove organisations without keywords
  filter(t_organisation %in% unique(data.ondata$organisation)) %>%
  filter(t_position_new == 1) %>% # remove links to non-skeptical actors
  filter(legacy == 0 | is.na(legacy)) %>% # remove links to legacy media
  group_by(crawl) %>%
  filter(s_organisation %in% skept_seeds) # remove all links not coming from a seed

# simulating second step of crawler (seed -> linked organisations, linked organisations -> some other organisation) and do some filtering
el_pages <- link_data %>%
  left_join(media_coded, by = c("t_domain" = "domain")) %>%
  filter(s_organisation %in% unique(data.ondata$organisation)) %>% # remove organisations without keywords
  filter(t_organisation %in% unique(data.ondata$organisation)) %>%
  filter(t_position_new == 1) %>% # remove links to non-skeptical actors
  filter(legacy == 0 | is.na(legacy)) %>% # remove links to legacy media
  group_by(crawl) %>%
  filter(s_organisation %in% c(skept_seeds, unique(linked_by_seed$t_organisation))) %>% # remove links not coming from a seed or an organisation linked by a seed
  filter(s_organisation %in% t_organisation) %>% # keep only links from organisations with > 1 inlink (several iterations needed)
  filter(s_organisation %in% t_organisation) %>%
  filter(s_organisation %in% t_organisation) %>%
  filter(s_organisation %in% t_organisation) %>%
  filter(s_organisation %in% t_organisation) %>%
  filter(s_organisation %in% t_organisation)

# aggregate edge list on organisation (actor) level
el_actors <- el_pages %>%
  group_by(crawl, 
           s_organisation, t_organisation, 
           s_actor_type, t_actor_type, 
           s_actor_country, t_actor_country) %>%
  summarise(n_links = n())

# How many German actors in the net?
el_actors %>%
  group_by(crawl) %>%
  filter(t_actor_country == 1) %>%
  summarise(sum = n_distinct(t_organisation))

save(el_pages, el_actors, file = "data/el_filtered.RData")
