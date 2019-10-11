#########################
## Plot named entities ##
#########################

ifelse(!require(tidyverse), install.packages("tidyverse"), require(tidyverse))

options(stringsAsFactors = FALSE)

# named entities for online data

# load online data
load("data/mips_corr.RData")
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
  mutate(type = "Online")

# named entities for offline data

# load offline data
load("data/data_offline.RData")
ne_off_raw <- read.csv("data/ne_off.csv", header = TRUE, encoding = "utf-8")

right_media <- c("BILD", "BILD am Sonntag", "Die Welt", "FAZ", "FAZ am Sonntag", "Financial Times Deutschland", "Focus", "Handelsblatt", "Welt am Sonntag")

# merge with coded mips
ne_off <- ne_off_raw %>%
  filter(ne_name != "NULL") %>%
  left_join(data.offdata, by = "d_id") %>%
  left_join(mips_corr, by = "ne_name") %>%
  filter(!is.na(newspaper)) %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  mutate(type = ifelse(newspaper %in% right_media, "Right-leaning media", "Other media")) %>%
  select(d_id, month, ne_name, ne_pos, ne_scope, ne_country, ne_group, ne_party, type)

# number of identified named entities
bind_rows(ne_off, ne_on) %>%
  mutate(ne_pos = ifelse(is.na(ne_pos), 99, ne_pos)) %>%
  group_by(ne_pos) %>%
  count()

# prepare plots
off <- ne_off %>%
  select(month, ne_pos, type) %>%
  group_by(month, ne_pos, type) %>%
  count() %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  spread(key = ne_pos, value = n) %>%
  mutate(total = `1` + `2` + `99` + `<NA>`) %>%
  mutate(skep_share = `1` / total) %>%
  select(month, skep_share, type) %>%
  left_join(crawl_month, by = "month") %>%
  select(month, skep_share, crawl, type)
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
  mutate(type = "Online")
comb <- bind_rows(off, on)

plt_act <- ggplot(data = comb) +
  geom_point(aes(x = crawl, y = skep_share, color = type)) +
  geom_smooth(aes(x = crawl, y = skep_share, color = type),
              method = "lm", se = TRUE) +
  scale_x_continuous(breaks = 2:25, 
                     minor_breaks = NULL,
                     labels = crawl_month$month) +
  labs(y = "Share of skeptical actors [0,1]",
       title = "4. Actor spillover") +
  theme_minimal() +
  scale_colour_manual(values = c("#4daf4a", "#e41a1c", "#377eb8")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank())

plt_act

# make simplified plot for presentation

# merge with coded mips
ne_on_simp <- ne_on_raw %>%
  filter(ne_name != "NULL") %>%
  left_join(pages, by = "d_id") %>%
  left_join(mips_corr, by = "ne_name") %>%
  filter(!is.na(duplicate_id)) %>%
  left_join(crawl_month, by = "crawl") %>%
  select(d_id, month, ne_name, ne_pos, ne_scope, ne_country, ne_group, ne_party)  %>%
  mutate(type = " Websites of climate change skeptics    ")

# merge with coded mips
ne_off_simp <- ne_off_raw %>%
  filter(ne_name != "NULL") %>%
  left_join(data.offdata, by = "d_id") %>%
  left_join(mips_corr, by = "ne_name") %>%
  filter(!is.na(newspaper)) %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  mutate(type = " Legacy media    ") %>%
  select(d_id, month, ne_name, ne_pos, ne_scope, ne_country, ne_group, ne_party, type)

# prepare plots
off_simp <- ne_off_simp %>%
  select(month, ne_pos, type) %>%
  group_by(month, ne_pos, type) %>%
  count() %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  spread(key = ne_pos, value = n) %>%
  mutate(total = `1` + `2` + `99` + `<NA>`) %>%
  mutate(skep_share = `1` / total) %>%
  select(month, skep_share, type) %>%
  left_join(crawl_month, by = "month") %>%
  select(month, skep_share, crawl, type)
on_simp <- ne_on_simp %>%
  select(month, ne_pos, type) %>%
  group_by(month, ne_pos, type) %>%
  count() %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  spread(key = ne_pos, value = n) %>%
  mutate(total = `1` + `2` + `99` + `<NA>`) %>%
  mutate(skep_share = `1` / total) %>%
  select(month, skep_share, type)  %>%
  left_join(crawl_month, by = "month") %>%
  select(month, skep_share, crawl, type)
comb_simp <- bind_rows(off_simp, on_simp)  %>%
  ungroup() %>%
  mutate(type = factor(type, levels = c(" Websites of climate change skeptics    ", " Legacy media    ")))

plt_act_simp <- ggplot(data = comb_simp) +
  geom_point(aes(x = crawl, y = skep_share, color = type)) +
  geom_smooth(aes(x = crawl, y = skep_share, color = type),
              method = "lm", se = TRUE) +
  scale_x_continuous(breaks = 2:25, 
                     minor_breaks = NULL,
                     labels = crawl_month$month) +
  labs(y = "Share of skeptical actors [0,1]") +
  theme_minimal() +
  scale_colour_manual(values = c("#4daf4a", "#377eb8", "#e41a1c")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank())

plt_act_simp

ggsave(plt_act_simp, file = "plots/simp/act.png")
