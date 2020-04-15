################################################
## Salience of climate change in legacy media ##
################################################

ifelse(!require(tidyverse), install.packages("tidyverse"), require(tidyverse))

options(stringsAsFactors = FALSE)

# load data
load("data/data_online_filtered.RData")
load("data/data_offline.RData")

crawl_month <- data.frame(crawl = 2:25,
                          month = c(paste("2012", c("07", "08", "09", "10", "11", "12"), sep = "-"),
                                    paste("2013", c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), sep = "-"),
                                    paste("2014", c("01", "02", "03", "04", "05", "06"), sep = "-")))

right_media <- c("BILD", "BILD am Sonntag", "Die Welt", "FAZ", "FAZ am Sonntag", "Financial Times Deutschland", "Focus", "Handelsblatt", "Welt am Sonntag")

# prepare plot
off_sal_r <- data.offdata %>%
  mutate(month = substr(as.character(date), 1, 7)) %>%
  mutate(right = ifelse(newspaper %in% right_media, "Right-leaning media", "Other media")) %>%
  left_join(crawl_month, by = "month") %>%
  select(d_id, crawl, month, right) %>%
  group_by(crawl, month, right) %>%
  summarise(n_docs = n_distinct(d_id)) %>%
  select(crawl, month, n_docs, type = right)

# calculate regression
fit_oth <- lm(n_docs ~ crawl, data = subset(off_sal_r, type == "Other media"), na.action = na.omit)
summary(fit_oth)

fit_cons <- lm(n_docs ~ crawl, data = subset(off_sal_r, type == "Right-leaning media"), na.action = na.omit)
summary(fit_cons)

# make plot
plt_sal <- ggplot() +
  geom_point(data = off_sal_r, mapping = aes(x = crawl, y = n_docs, color = type)) +
  geom_abline(intercept = summary(fit_oth)$coefficients[1,1],
              slope = summary(fit_oth)$coefficients[2,1],
              color = "#e41a1c", size = 1) +
  geom_abline(intercept = summary(fit_cons)$coefficients[1,1],
              slope = summary(fit_cons)$coefficients[2,1],
              color = "#377eb8", size = 1) +
  scale_x_continuous(breaks = 2:25, 
                     minor_breaks = NULL,
                     labels = crawl_month$month) +
  labs(y = "Number of articles", 
       title = "1. Salience of climate change in legacy media") +
  theme_minimal() +
  scale_colour_brewer(palette = "Set1") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank())

plt_sal
