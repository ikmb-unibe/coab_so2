########################
## Get named entities ##
########################

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

#write.csv(multi_mips, file = "data/multi_mips.csv", row.names = FALSE)

# correct multi-coded mips 

mips_corr <- read.csv("data/multi_mips.csv", encoding = "utf-8", header = TRUE) %>%
  filter(!is.na(ne_pos_corr)) %>%
  select(ne_name, ne_pos_corr) %>%
  right_join(mips_df, by = "ne_name") %>%
  mutate(ne_pos = ifelse(is.na(ne_pos_corr), ne_pos, ne_pos_corr)) %>%
  distinct(ne_name, .keep_all = TRUE) %>%
  select(-ne_pos_corr)

# named entities for online data

# load online data
load("data/data_online_filtered.RData")
ne_on_raw <- read.csv("data/ne_on.csv", header = TRUE, encoding = "utf-8")

crawl_month <- data.frame(crawl = 2:25,
                          month = c(paste("2012", c("07", "08", "09", "10", "11", "12"), sep = "-"),
                                    paste("2013", c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), sep = "-"),
                                    paste("2014", c("01", "02", "03", "04", "05", "06"), sep = "-")))

# merge with coded mips
ne_on <- ne_on_raw %>%
  filter(ne_name != "NULL") %>%
  left_join(pages, by = "d_id") %>%
  left_join(mips_corr, by = "ne_name") %>%
  filter(!is.na(duplicate_id)) %>%
  left_join(crawl_month, by = "crawl") %>%
  select(d_id, month, ne_name, ne_pos, ne_scope, ne_country, ne_group, ne_party)  %>%
  mutate(type = "on")

# named entities for offline data

# load offline data
load("data/data_offline.RData")
ne_off_raw <- read.csv("data/ne_off.csv", header = TRUE, encoding = "utf-8")

# merge with coded mips
ne_off <- ne_off_raw %>%
  filter(ne_name != "NULL") %>%
  left_join(data.offdata, by = "d_id") %>%
  left_join(mips_corr, by = "ne_name") %>%
  filter(!is.na(newspaper)) %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  select(d_id, month, ne_name, ne_pos, ne_scope, ne_country, ne_group, ne_party) %>%
  mutate(type = "off")

# combine online and offline data

# most prominent skeptics
most_prom_skep <- bind_rows(ne_off, ne_on) %>%
  filter(ne_pos == 1) %>%
  group_by(ne_name) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  slice(1:200)

# 200 most prominent na (= not previously coded ne)
most_prom_na <- bind_rows(ne_off, ne_on) %>%
  filter(is.na(ne_pos)) %>%
  group_by(ne_name) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  slice(1:200)

# prepare plots: aggregate level
off <- ne_off %>%
  select(month, ne_pos) %>%
  group_by(month, ne_pos) %>%
  count() %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  spread(key = ne_pos, value = n) %>%
  mutate(total = `1` + `2` + `99` + `<NA>`) %>%
  mutate(skep_share = `1` / total) %>%
  select(month, skep_share) %>%
  left_join(crawl_month, by = "month") %>%
  mutate(type = "offline")
on <- ne_on %>%
  select(month, ne_pos) %>%
  group_by(month, ne_pos) %>%
  count() %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  spread(key = ne_pos, value = n) %>%
  mutate(total = `1` + `2` + `99` + `<NA>`) %>%
  mutate(skep_share = `1` / total) %>%
  select(month, skep_share)  %>%
  left_join(crawl_month, by = "month") %>%
  mutate(type = "online")
comb <- bind_rows(off, on)

plt <- ggplot(data = comb) +
  geom_point(aes(x = crawl, y = skep_share, color = type)) +
  geom_smooth(aes(x = crawl, y = skep_share, color = type),
              method = "lm", se = FALSE) +
  scale_x_continuous(breaks = 2:25, 
                     minor_breaks = NULL,
                     labels = crawl_month$month) +
  labs(y = "Share of skeptical actors [0,1]") +
  theme_minimal() +
  scale_colour_brewer(palette = "Set1") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank())
plt

ggsave(plt, file = "plots/actor_trend.png", device = "png")
ggsave(plt, file = "plots/actor_trend.pdf", device = "pdf")


# prepare plots: individual newspaper level
off_n <- ne_off %>%
  left_join(select(data.offdata, d_id, newspaper), by = "d_id") %>%
  select(newspaper, month, ne_pos) %>%
  group_by(newspaper, month, ne_pos) %>%
  count() %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  spread(key = ne_pos, value = n) %>%
  mutate(total = `1` + `2` + `99` + `<NA>`) %>%
  mutate(skep_share = `1` / total) %>%
  mutate(skep_share = ifelse(is.na(skep_share), 0, skep_share)) %>%
  left_join(crawl_month, by = "month") %>%
  select(month, skep_share, crawl, type = newspaper)
comb <- bind_rows(off, on, off_n)

newsp <- unique(comb$type)

for(i in 1:length(newsp)){
  plt <- ggplot() +
    geom_point(data = subset(comb, type == newsp[i]),
               mapping = aes(x = crawl, y = skep_share, color = type)) +
    geom_smooth(data = subset(comb, type == "online" | type == "offline" | type == newsp[i]),
                mapping = aes(x = crawl, y = skep_share, color = type),
                method = "lm", se = FALSE) +
    scale_x_continuous(breaks = 2:25, 
                       minor_breaks = NULL,
                       labels = crawl_month$month) +
    labs(y = "Share of skeptical actors [0,1]") +
    theme_minimal() +
    scale_colour_brewer(palette = "Set1") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.title.x = element_blank())
  ggsave(plt, file = paste0("plots/actor_n/class_trend_", newsp[i], ".png"), device = "png")
  ggsave(plt, file = paste0("plots/actor_n/class_trend_", newsp[i], ".pdf"), device = "pdf")
}
