#########################
## classifcation plots ##
#########################

ifelse(!require(tidyverse), install.packages("tidyverse"), require(tidyverse))

# load data
load("data/off_sent_class.RData")
load("data/on_sent_de_class.RData")
load("data/on_sent_en_class.RData")

crawl_month <- data.frame(crawl = 1:25,
                          month = c(paste("2012", c("06", "07", "08", "09", "10", "11", "12"), sep = "-"),
                                    paste("2013", c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), sep = "-"),
                                    paste("2014", c("01", "02", "03", "04", "05", "06"), sep = "-")))

# prepare plot
off <- off_sent_class %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  select(month, classification) %>%
  group_by(month, classification) %>%
  count() %>%
  spread(classification, -month) %>%
  mutate(total = Sceptics + Alarmists + irrelevant) %>%
  mutate(skep_share_off = Sceptics / total) %>%
  select(month, skep_share_off) %>%
  rowid_to_column("id_off")
on <- on_sent_de_class %>%
  bind_rows(on_sent_en_class) %>%
  left_join(crawl_month, by = "crawl") %>%
  select(month, classification) %>%
  group_by(month, classification) %>%
  count() %>%
  spread(classification, -month) %>%
  mutate(total = Sceptics + Alarmists + irrelevant) %>%
  mutate(skep_share_on = Sceptics / total) %>%
  select(month, skep_share_on) %>%
  rowid_to_column("id_on")
comb <- off %>%
  full_join(on, by = "month") %>%
  gather(key = "type", value = "share", -month, -id_off, -id_on) %>%
  mutate(type = case_when(type == "skep_share_off" ~ "offline",
                          type == "skep_share_on" ~ "online")) %>%
  mutate(id = case_when(type == "offline" ~ id_off,
                        type == "online" ~ id_on))
       
plt <- ggplot(data = comb) +
  geom_point(aes(x = month, y = share, color = type)) +
  geom_smooth(aes(x = id, y = share, color = type),
              method = "lm", se = FALSE) +
  labs(y = "Share of skeptical sentences",
       x = "Month") +
  theme_minimal() +
  scale_colour_brewer(palette = "Set1") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggsave(plt, file = "plots/class_trend.png", device = "png")
ggsave(plt, file = "plots/class_trend.pdf", device = "pdf")
