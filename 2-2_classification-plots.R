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

right_media <- c("BILD", "BILD am Sonntag", "Die Welt", "FAZ", "FAZ am Sonntag", "Financial Times Deutschland", "Focus", "Handelsblatt", "Welt am Sonntag")

# prepare plots
off <- off_sent_class %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  mutate(type = ifelse(newspaper %in% right_media, " Conservative legacy media    ", " Other legacy media    ")) %>%
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
  mutate(type = " Websites of climate change skeptics    ") %>%
  select(type, crawl, skep_share)
comb <- bind_rows(off, on) %>%
  ungroup() %>%
  mutate(type = factor(type, levels = c(" Websites of climate change skeptics    ", " Conservative legacy media    ", " Other legacy media    ")))

# occasional spillover plot
plt_pos <- ggplot() +
  geom_point(data = comb, mapping = aes(x = crawl, y = skep_share, color = type)) +
  scale_x_continuous(breaks = 2:25, 
                     minor_breaks = NULL,
                     labels = crawl_month$month) +
  labs(y = "Share of skeptical sentences [0,1]",
       title = "2. Positional spillover") +
  theme_minimal() +
  scale_colour_manual(values = c("#4daf4a", "#377eb8", "#e41a1c")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank())

plt_pos

# continuous spillover plot
comb_dist <-  comb %>% 
  spread(type, skep_share) %>%
  rename_all(funs(c("crawl", "skeptics", "cons_media", "oth_media"))) %>%
  mutate(dist_cons = skeptics - cons_media) %>%
  mutate(dist_oth = skeptics - oth_media)

dist_plot <- comb_dist %>%
  select(crawl, dist_cons, dist_oth) %>%
  gather(key = "type", value = "dist", -crawl) %>%
  mutate(type = case_when(type == "dist_cons" ~ " Conservative legacy media    ",
                          type == "dist_oth" ~ " Other legacy media    ")) %>%
  mutate(type = factor(type, levels = c(" Conservative legacy media    ", " Other legacy media    ")))

# calculate regression
fit_oth <- lm(dist_oth ~ crawl, data = comb_dist, na.action = na.omit)
summary(fit_oth)

fit_cons <- lm(dist_cons ~ crawl, data = comb_dist, na.action = na.omit)
summary(fit_cons)

plt_pos_new <- ggplot() +
  geom_point(data = dist_plot, mapping = aes(x = crawl, y = dist, color = type)) +
  geom_abline(intercept = summary(fit_oth)$coefficients[1,1],
              slope = summary(fit_oth)$coefficients[2,1],
              color = "#e41a1c", size = 1) +
  geom_abline(intercept = summary(fit_cons)$coefficients[1,1],
              slope = summary(fit_cons)$coefficients[2,1],
              color = "#377eb8", size = 1) +
  scale_x_continuous(breaks = 2:25, 
                     minor_breaks = NULL,
                     labels = crawl_month$month) +
  labs(y = "Difference of skeptical sentences rates on-/offline [0,1]",
       title = "3. Positional spillover") +
  theme_minimal() +
  scale_colour_manual(values = c("#377eb8", "#e41a1c")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank())

plt_pos_new

# make simplified plot for presentation

# prepare plots
off_simp <- off_sent_class %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  mutate(type = " Legacy media    ") %>%
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
on_simp <- on_sent_de_class %>%
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
  mutate(type = " Websites of climate change skeptics    ") %>%
  select(type, crawl, skep_share)
comb_simp <- bind_rows(off_simp, on_simp) %>%
  ungroup() %>%
  mutate(type = factor(type, levels = c(" Websites of climate change skeptics    ", " Legacy media    ")))

# make plot
plt_pos_simp <- ggplot() +
  geom_point(data = comb_simp, mapping = aes(x = crawl, y = skep_share, color = type)) +
  geom_smooth(data = comb_simp, mapping = aes(x = crawl, y = skep_share, color = type),
              method = "lm", se = TRUE) +
  scale_x_continuous(breaks = 2:25, 
                     minor_breaks = NULL,
                     labels = crawl_month$month) +
  labs(y = "Share of skeptical sentences [0,1]") +
  theme_minimal() +
  scale_colour_manual(values = c("#4daf4a", "#377eb8", "#e41a1c")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank())

plt_pos_simp

ggsave(plt_pos_simp, file = "plots/simp/pos.png")
