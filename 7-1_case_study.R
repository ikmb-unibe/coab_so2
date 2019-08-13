################
## case study ##
################

ifelse(!require(tidyverse), install.packages("tidyverse"), require(tidyverse))

options(stringsAsFactors = FALSE)

# load data
load("data/data_offline.RData")

load("tm/theta.RData")
labels <- read_csv("tm/topwords_k20.csv")

load("data/off_sent_class.RData")

crawl_month <- data.frame(crawl = 2:25,
                          month = c(paste("2012", c("07", "08", "09", "10", "11", "12"), sep = "-"),
                                    paste("2013", c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), sep = "-"),
                                    paste("2014", c("01", "02", "03", "04", "05", "06"), sep = "-")))

right_media <- c("BILD", "BILD am Sonntag", "Die Welt", "FAZ", "FAZ am Sonntag", "Financial Times Deutschland", "Focus", "Handelsblatt", "Welt am Sonntag")

# proportion of individual topics over time in legacy media
theta_time <- bind_rows(theta_off, theta_on) %>%
  group_by(type, month) %>%
  summarise_if(is.numeric, list(mean)) %>%
  gather(key = "topic", value = "prob", -type, - month) %>%
  left_join(select(labels, -topwords), by = c("topic" = "t_id")) %>%
  mutate(t_label = paste(topic, label))

case_plot <- ggplot(data = theta_time) +
  theme_minimal() +
  geom_line(mapping = aes(x = month,
                          y = prob,
                          group = topic,
                          color = label)) +
  labs(y = "Mean topic probability",
       x = "Month", color = "Topic") +
  facet_wrap(~ type, labeller = as_labeller(c("on" = "Websites of climate change skeptics", "off" = "Legacy media"))) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        strip.text.x = element_text(size = 12, face = "bold"))

# save case plot
ggsave(case_plot, file = "plots/case_plot.pdf", device = "pdf", width = 12)
ggsave(case_plot, file = "plots/case_plot.png", device = "png", width = 12)

# Prepare classification data
off_pos <- off_sent_class %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  select(d_id, month, classification) %>%
  group_by(d_id, month, classification) %>%
  count() %>%
  spread(key = classification, value = n) %>%
  mutate(Sceptics = ifelse(is.na(Sceptics), 0, Sceptics)) %>%
  mutate(Alarmists = ifelse(is.na(Alarmists), 0, Alarmists)) %>%
  mutate(irrelevant = ifelse(is.na(irrelevant), 0, irrelevant)) %>%
  mutate(total = Sceptics + Alarmists + irrelevant) %>%
  mutate(skep_share = Sceptics / total)

# Identify legacy articles with a high probability of topic nr. 14 and a lot of skeptical sentences
off_t14 <- theta_off_page %>%
  bind_cols(data.offdata) %>%
  ungroup() %>%
  filter(month == "2013-09") %>%
  left_join(off_pos, by = "d_id") %>%
  select(d_id, newspaper, month = month.x, t_14, skep_share, text) %>%
  arrange(desc(t_14), desc(skep_share)) %>%
  slice(1:20)

# Sample articles
sample_n(off_t14, 1)  
