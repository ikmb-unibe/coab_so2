#########################
## classifcation plots ##
#########################

ifelse(!require(tidyverse), install.packages("tidyverse"), require(tidyverse))

options(stringsAsFactors = FALSE)

# load data
load("data/off_sent_class.RData")
load("data/on_sent_de_class.RData")
load("data/on_sent_en_class.RData")

crawl_month <- data.frame(crawl = 2:25,
                          month = c(paste("2012", c("07", "08", "09", "10", "11", "12"), sep = "-"),
                                    paste("2013", c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), sep = "-"),
                                    paste("2014", c("01", "02", "03", "04", "05", "06"), sep = "-")))

right_media <- c("BILD", "BILD am Sonntag", "Die Welt", "FAZ", "FAZ am Sonntag", "Focus", "Handelsblatt", "Welt am Sonntag")

# prepare plots
off <- off_sent_class %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  mutate(type = ifelse(newspaper %in% right_media, "Right-leaning media    ", "Other media    ")) %>%
  left_join(crawl_month, by = "month") %>%
  select(type, crawl, classification) %>%
  group_by(type, crawl, classification) %>%
  count() %>%
  spread(key = classification, value = n) %>%
  mutate(Sceptics = ifelse(is.na(Sceptics), 0, Sceptics)) %>%
  mutate(Alarmists = ifelse(is.na(Alarmists), 0, Alarmists)) %>%
  mutate(irrelevant = ifelse(is.na(irrelevant), 0, irrelevant)) %>%
  mutate(total = Sceptics + Alarmists + irrelevant) %>%
  mutate(skep_share = Sceptics / total) %>%
  select(type, crawl, skep_share)
on <- on_sent_de_class %>%
  bind_rows(on_sent_en_class) %>%
  select(crawl, classification) %>%
  group_by(crawl, classification) %>%
  count() %>%
  spread(key = classification, value = n) %>%
  mutate(Sceptics = ifelse(is.na(Sceptics), 0, Sceptics)) %>%
  mutate(Alarmists = ifelse(is.na(Alarmists), 0, Alarmists)) %>%
  mutate(irrelevant = ifelse(is.na(irrelevant), 0, irrelevant)) %>%
  mutate(total = Sceptics + Alarmists + irrelevant) %>%
  mutate(skep_share = Sceptics / total) %>%
  mutate(type = "Online    ") %>%
  select(type, crawl, skep_share)
comb <- bind_rows(off, on) %>%
  ungroup() %>%
  mutate(type = factor(type, levels = c("Online    ", "Right-leaning media    ", "Other media    ")))

# plot with share of skeptical sentences
plt_pos <- ggplot() +
  geom_point(data = comb, mapping = aes(x = crawl, y = skep_share, color = type)) +
  geom_smooth(data = comb, mapping = aes(x = crawl, y = skep_share, color = type),
              method = "lm", se = FALSE) +
  scale_x_continuous(breaks = 2:25, 
                     minor_breaks = NULL,
                     labels = crawl_month$month) +
  labs(y = "Share of skeptical sentences [0,1]",
       title = "3. Positional spill-over") +
  theme_minimal() +
  scale_colour_manual(values = c("#4daf4a", "#377eb8", "#e41a1c")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank())
plt_pos

ggsave(plt, file = "plots/class_trend.png", device = "png")
ggsave(plt, file = "plots/class_trend.pdf", device = "pdf")

# plots with salience
off_sal <- off_sent_class %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  left_join(crawl_month, by = "month") %>%
  select(d_id, crawl, month) %>%
  group_by(crawl, month) %>%
  summarise(n_docs = n_distinct(d_id))
off <- off_sent_class %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  left_join(crawl_month, by = "month") %>%
  select(crawl, classification) %>%
  group_by(crawl, classification) %>%
  count() %>%
  spread(key = classification, value = n) %>%
  mutate(Sceptics = ifelse(is.na(Sceptics), 0, Sceptics)) %>%
  mutate(Alarmists = ifelse(is.na(Alarmists), 0, Alarmists)) %>%
  mutate(irrelevant = ifelse(is.na(irrelevant), 0, irrelevant)) %>%
  mutate(total = Sceptics + Alarmists + irrelevant) %>%
  mutate(skep_share = Sceptics / total) %>%
  select(crawl, skep_share) %>%
  left_join(off_sal, by = "crawl") %>%
  mutate(type = "offline")
on_sal <- on_sent_de_class %>%
  bind_rows(on_sent_en_class) %>%
  left_join(crawl_month, by = "crawl") %>%
  select(d_id, crawl, month) %>%
  group_by(crawl, month) %>%
  summarise(n_docs = n_distinct(d_id))
on <- on_sent_de_class %>%
  bind_rows(on_sent_en_class) %>%
  select(crawl, classification) %>%
  group_by(crawl, classification) %>%
  count() %>%
  spread(key = classification, value = n) %>%
  mutate(Sceptics = ifelse(is.na(Sceptics), 0, Sceptics)) %>%
  mutate(Alarmists = ifelse(is.na(Alarmists), 0, Alarmists)) %>%
  mutate(irrelevant = ifelse(is.na(irrelevant), 0, irrelevant)) %>%
  mutate(total = Sceptics + Alarmists + irrelevant) %>%
  mutate(skep_share = Sceptics / total) %>%
  select(crawl, skep_share) %>%
  left_join(on_sal, by = "crawl") %>%
  mutate(type = "online")
comb <- bind_rows(off, on)

# plot with salience
plt_sal <- ggplot() +
  geom_point(data = comb, mapping = aes(x = crawl, y = n_docs, color = type)) +
  geom_smooth(data = comb, mapping = aes(x = crawl, y = n_docs, color = type),
              method = "lm", se = FALSE) +
  scale_x_continuous(breaks = 2:25, 
                     minor_breaks = NULL,
                     labels = crawl_month$month) +
  
  labs(y = "Number of documents") +
  theme_minimal() +
  scale_colour_brewer(palette = "Set1") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank())
plt_sal

ggsave(plt_sal, file = "plots/salience.png", device = "png")
ggsave(plt_sal, file = "plots/salience.pdf", device = "pdf")

# prepare plots: individual newspaper level
off_sal <- off_sent_class %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  left_join(crawl_month, by = "month") %>%
  select(d_id, crawl, month) %>%
  group_by(crawl, month) %>%
  summarise(n_docs = n_distinct(d_id))
off <- off_sent_class %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  left_join(crawl_month, by = "month") %>%
  select(crawl, classification) %>%
  group_by(crawl, classification) %>%
  count() %>%
  spread(key = classification, value = n) %>%
  mutate(Sceptics = ifelse(is.na(Sceptics), 0, Sceptics)) %>%
  mutate(Alarmists = ifelse(is.na(Alarmists), 0, Alarmists)) %>%
  mutate(irrelevant = ifelse(is.na(irrelevant), 0, irrelevant)) %>%
  mutate(total = Sceptics + Alarmists + irrelevant) %>%
  mutate(skep_share = Sceptics / total) %>%
  left_join(crawl_month, by = "crawl") %>%
  mutate(newspaper = "offline") %>%
  left_join(off_sal, by = c("crawl" = "crawl", "month" = "month")) %>%
  select(newspaper, crawl, skep_share, month, n_docs)
off_sal_n <- off_sent_class %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  left_join(crawl_month, by = "month") %>%
  select(d_id, newspaper, crawl) %>%
  group_by(newspaper, crawl) %>%
  summarise(n_docs = n_distinct(d_id))
off_n <- off_sent_class %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  left_join(crawl_month, by = "month") %>%
  select(newspaper, crawl, classification) %>%
  group_by(newspaper, crawl, classification) %>%
  count() %>%
  spread(key = classification, value = n) %>%
  mutate(Sceptics = ifelse(is.na(Sceptics), 0, Sceptics)) %>%
  mutate(Alarmists = ifelse(is.na(Alarmists), 0, Alarmists)) %>%
  mutate(irrelevant = ifelse(is.na(irrelevant), 0, irrelevant)) %>%
  mutate(total = Sceptics + Alarmists + irrelevant) %>%
  mutate(skep_share = Sceptics / total) %>%
  select(newspaper, crawl, skep_share) %>%
  left_join(crawl_month, by = "crawl") %>%
  left_join(off_sal_n, by = c("crawl" = "crawl", "newspaper" = "newspaper"))
on_sal <- on_sent_de_class %>%
  bind_rows(on_sent_en_class) %>%
  left_join(crawl_month, by = "crawl") %>%
  select(d_id, crawl, month) %>%
  group_by(crawl, month) %>%
  summarise(n_docs = n_distinct(d_id))
on <- on_sent_de_class %>%
  bind_rows(on_sent_en_class) %>%
  select(crawl, classification) %>%
  group_by(crawl, classification) %>%
  count() %>%
  spread(key = classification, value = n) %>%
  mutate(Sceptics = ifelse(is.na(Sceptics), 0, Sceptics)) %>%
  mutate(Alarmists = ifelse(is.na(Alarmists), 0, Alarmists)) %>%
  mutate(irrelevant = ifelse(is.na(irrelevant), 0, irrelevant)) %>%
  mutate(total = Sceptics + Alarmists + irrelevant) %>%
  mutate(skep_share = Sceptics / total) %>%
  select(crawl, skep_share) %>%
  left_join(on_sal, by = "crawl") %>%
  mutate(newspaper = "online") %>%
  select(newspaper, crawl, skep_share, month, n_docs)
comb <- bind_rows(off, on, off_n)

newsp <- unique(comb$newspaper)

for(i in 1:length(newsp)){
  plt <- ggplot() +
    geom_point(data = subset(comb, newspaper == newsp[i]),
               mapping = aes(x = crawl, y = skep_share, color = newspaper)) +
    geom_smooth(data = subset(comb, newspaper == "online" | newspaper == "offline" | newspaper == newsp[i]),
                mapping = aes(x = crawl, y = skep_share, color = newspaper),
                method = "lm", se = FALSE) +
    scale_x_continuous(breaks = 2:25, 
                       minor_breaks = NULL,
                       labels = crawl_month$month) +
    labs(y = "Share of skeptical sentences [0,1]") +
    theme_minimal() +
    scale_colour_brewer(palette = "Set1") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.title.x = element_blank())
  ggsave(plt, file = paste0("plots/class_n/class_trend_", newsp[i], ".png"), device = "png")
  ggsave(plt, file = paste0("plots/class_n/class_trend_", newsp[i], ".pdf"), device = "pdf")
}

