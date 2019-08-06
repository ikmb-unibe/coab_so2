###############
## Salience ##
##############

ifelse(!require(tidyverse), install.packages("tidyverse"), require(tidyverse))

options(stringsAsFactors = FALSE)

# load data
load("data/data_online_filtered.RData")
load("data/data_offline.RData")

crawl_month <- data.frame(crawl = 2:25,
                          month = c(paste("2012", c("07", "08", "09", "10", "11", "12"), sep = "-"),
                                    paste("2013", c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), sep = "-"),
                                    paste("2014", c("01", "02", "03", "04", "05", "06"), sep = "-")))

right_media <- c("BILD", "BILD am Sonntag", "Die Welt", "FAZ", "FAZ am Sonntag", "Focus", "Handelsblatt", "Welt am Sonntag")

# plot salience: left-right newspaper
off_sal_r <- data.offdata %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  mutate(right = ifelse(newspaper %in% right_media, "Right-leaning media", "Other media")) %>%
  left_join(crawl_month, by = "month") %>%
  select(d_id, crawl, month, right) %>%
  group_by(crawl, month, right) %>%
  summarise(n_docs = n_distinct(d_id)) %>%
  select(crawl, month, n_docs, type = right)

plt_sal_r <- ggplot() +
  geom_point(data = off_sal_r, mapping = aes(x = crawl, y = n_docs, color = type)) +
  geom_smooth(data = off_sal_r, mapping = aes(x = crawl, y = n_docs, color = type),
              method = "lm", se = FALSE) +
  scale_x_continuous(breaks = 2:25, 
                     minor_breaks = NULL,
                     labels = crawl_month$month) +
  labs(y = "Number of articles", title = "1. Salience of climate change in legacy media") +
  theme_minimal() +
  scale_colour_brewer(palette = "Set1") +
  theme(legend.position = "none",
        #        legend.position = c(.95, .95),
        #        legend.justification = c("right", "top"),
        #        legend.box.just = "right",
        #        legend.margin = margin(6, 6, 6, 6),
        #        legend.box.background = element_rect(fill = "white", color = "black"),
        #        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank())
plt_sal_r

# salience: all online and offline
off_sal <- data.offdata %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  left_join(crawl_month, by = "month") %>%
  select(d_id, crawl, month) %>%
  group_by(crawl, month) %>%
  summarise(n_docs = n_distinct(d_id)) %>%
  mutate(type = "offline")
on_sal <- pages %>%
  left_join(crawl_month, by = "crawl") %>%
  select(d_id, crawl, month) %>%
  group_by(crawl, month) %>%
  summarise(n_docs = n_distinct(d_id)) %>%
  mutate(type = "online")
comb <- bind_rows(off_sal, on_sal)

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
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_blank(),
        legend.box.background = element_rect(fill = "white", color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank())
plt_sal

ggsave(plt_sal, file = "plots/salience.png", device = "png")
ggsave(plt_sal, file = "plots/salience.pdf", device = "pdf")

# plot salience: individual newspaper level
off_sal_n <- data.offdata %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  left_join(crawl_month, by = "month") %>%
  select(d_id, crawl, month, newspaper) %>%
  group_by(crawl, month, newspaper) %>%
  summarise(n_docs = n_distinct(d_id)) %>%
  select(crawl, month, n_docs, type = newspaper)
comb <- bind_rows(off_sal, on_sal, off_sal_n)

newsp <- unique(comb$type)

for(i in 1:length(newsp)){
  plt <- ggplot() +
    geom_point(data = subset(comb, type == newsp[i]),
               mapping = aes(x = crawl, y = n_docs, color = type)) +
    geom_smooth(data = subset(comb, type == newsp[i]),
                mapping = aes(x = crawl, y = n_docs, color = type),
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
  ggsave(plt, file = paste0("plots/salience_n/salience_trend_", newsp[i], ".png"), device = "png")
  ggsave(plt, file = paste0("plots/salience_n/salience_trend_", newsp[i], ".pdf"), device = "pdf")
}

