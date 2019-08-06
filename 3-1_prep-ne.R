############################
## Prepare named entities ##
############################

ifelse(!require(tidyverse), install.packages("tidyverse"), require(tidyverse))
ifelse(!require(haven), install.packages("haven"), require(haven))

options(stringsAsFactors = FALSE)

# prepare climate position data

# load data from manual coding
actors_raw <- read_sav("data/Akteursebene_mit Frame-Ebene_mit Dok-Ebene_überall 35_04122015.sav", encoding = "utf-8")
encoding_problems <- read.csv("data/encoding_problems.csv", encoding = "latin1", header = TRUE, stringsAsFactors = FALSE)

# recode position of actors
mips_recode <- actors_raw %>%
  select(surname = MIPSURN, given_name = MIPNAME, orga_name = MIPORGA,
         climch_1 = CLIMCH1_first, climch_2 = CLIMCH2_first, climch_3 = CLIMCH3_first, climch_99 = CLIMCH99_first,
         cprob_1 = CPROB1_first, cprob_2 = CPROB2_first, cprob_3 = CPROB3_first, cprob_99 = CPROB99_first) %>%
  mutate(pers_name = paste(given_name, surname, sep = " ")) %>%
  mutate(pos = case_when(climch_2 == 1 ~ 1, # "real" skeptics
                         climch_1 == 1 & cprob_2 == 1 ~ 1, # other skeptics
                         climch_1 == 1 & cprob_1 == 1 ~ 2, # alarmists
                         climch_1 == 1 & cprob_3 == 1 ~ 99, # ambivalent
                         climch_1 == 1 & cprob_99 == 1 ~ 99, # not identifiable
                         climch_3 == 1 ~ 99, # ambivalent
                         climch_99 == 1 ~ 99)) %>% # not identifiable
  select(pers_name, orga_name, pos)

# some data wrangling
mips_df <- mips_recode %>%
  select(ne = pers_name, pos) %>%
  bind_rows(select(mips_recode, ne = orga_name, pos)) %>%
  filter(ne != " ") %>%
  filter(ne != "") %>%
  left_join(encoding_problems, by = "ne") %>%
  mutate(ne = ifelse(is.na(ne_corr), ne, ne_corr)) %>%
  select(-ne_corr) %>%
  group_by(ne, pos) %>%
  count() %>%
  group_by(ne) %>%
  arrange(desc(n)) %>%
  top_n(1, n) %>%
  select(ne_name = ne, ne_pos = pos, n_coded = n)

# identify named entities that were coded multiple times
multi_mips <- mips_df %>%
  group_by(ne_name) %>%
  count(name = "n_entries") %>%
  filter(n_entries > 1) %>%
  right_join(mips_df, by = "ne_name") %>%
  filter(!is.na(n_entries))

# save list with multi-coded named entities for additional manual coding
#write.csv(multi_mips, file = "data/multi_mips.csv", row.names = FALSE)

# load corrected version of multi-coded named entities and correct them
mips_corr <- read.csv("data/multi_mips.csv", encoding = "utf-8", header = TRUE) %>%
  filter(!is.na(ne_pos_corr)) %>%
  select(ne_name, ne_pos_corr) %>%
  right_join(mips_df, by = "ne_name") %>%
  mutate(ne_pos = ifelse(is.na(ne_pos_corr), ne_pos, ne_pos_corr)) %>%
  distinct(ne_name, .keep_all = TRUE) %>%
  select(-ne_pos_corr)

save(mips_corr, file = "data/mips_corr.RData")
