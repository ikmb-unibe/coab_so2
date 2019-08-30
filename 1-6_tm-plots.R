#######################
# JSD and other plots #
#######################

ifelse(!require(tidyverse), install.packages("tidyverse"), require(tidyverse))

options(stringsAsFactors = FALSE)

# load data
load("tm/theta.RData")
labels <- read_csv("tm/topwords_k20.csv")

right_media <- c("BILD", "BILD am Sonntag", "Die Welt", "FAZ", "FAZ am Sonntag", "Financial Times Deutschland", "Focus", "Handelsblatt", "Welt am Sonntag")

# prepare data sets
theta_comp <-   bind_rows(theta_on, theta_off) %>%
  ungroup() %>%
  mutate(type = case_when(actor %in% right_media ~ "Right-leaning media",
                          type == "on" ~ "Online",
                          TRUE ~ "Other media")) %>%
  group_by(type, month) %>%
  summarise_if(is.numeric, list(mean)) %>%
  gather(key = "topic", value = "prob", -type, - month) %>%
  spread(key = month, value = prob) %>%
  group_split()

# calculate Jensen-Shannon divergence for "other media"
jsd_2 <- vector()

for (i in 1:(ncol(theta_comp[[1]]) - 2)) {
  p <- theta_comp[[1]][, (2 + i)]
  q <- theta_comp[[2]][, (2 + i)]
  m <- 0.5 * (p + q)
  jsd_2[i] <- 0.5 * (sum(p * log2(p / m)) + sum(q * log2(q / m))) # Jensen-Shannon divergence bounded in [0,1]
}

jsd_df_2 <- data.frame(month = as.character(names(theta_comp[[1]])[3:ncol(theta_comp[[1]])]),
                       jsd = jsd_2,
                       month_c = 1:(ncol(theta_comp[[1]]) - 2),
                       type = unique(theta_comp[[2]]$type))

# calculate Jensen-Shannon divergence for "right-leaning media"
jsd_3 <- vector()

for (i in 1:(ncol(theta_comp[[1]]) - 2)) {
  p <- theta_comp[[1]][, (2 + i)]
  q <- theta_comp[[3]][, (2 + i)]
  m <- 0.5 * (p + q)
  jsd_3[i] <- 0.5 * (sum(p * log2(p / m)) + sum(q * log2(q / m))) # Jensen-Shannon divergence bounded in [0,1]
}

jsd_df_3 <- data.frame(month = as.character(names(theta_comp[[1]])[3:ncol(theta_comp[[1]])]),
                       jsd = jsd_3,
                       month_c = 1:(ncol(theta_comp[[1]]) - 2),
                       type = unique(theta_comp[[3]]$type))

# combine data sets for plot
jsd_df <- bind_rows(jsd_df_2, jsd_df_3)

# make plot for publication
plt_thema <- ggplot(data = jsd_df) +
  theme_minimal() +
  geom_point(mapping = aes(x = month, y = jsd, color = type)) +
  geom_smooth(mapping = aes(x = month_c, y = jsd, color = type),
              method = "lm", se = TRUE) +
  labs(y = "Jensen-Shannon divergence [0,1]",
       x = "Month",
       title = "2. Thematic spill-over") +
  theme_minimal() +
  scale_colour_brewer(palette = "Set1") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank())

plt_thema

# make simplified plot for presentation

# prepare data sets
theta_comp_simp <-   bind_rows(theta_on, theta_off) %>%
  group_by(type, month) %>%
  summarise_if(is.numeric, list(mean)) %>%
  gather(key = "topic", value = "prob", -type, - month) %>%
  spread(key = month, value = prob) %>%
  group_split()

# calculate Jensen-Shannon divergence for "other media"
jsd <- vector()

for (i in 1:(ncol(theta_comp_simp[[1]]) - 2)) {
  p <- theta_comp_simp[[1]][, (2 + i)]
  q <- theta_comp_simp[[2]][, (2 + i)]
  m <- 0.5 * (p + q)
  jsd[i] <- 0.5 * (sum(p * log2(p / m)) + sum(q * log2(q / m))) # Jensen-Shannon divergence bounded in [0,1]
}

jsd_df_simp <- data.frame(month = as.character(names(theta_comp_simp[[1]])[3:ncol(theta_comp_simp[[1]])]),
                     jsd = jsd,
                     month_c = 1:(ncol(theta_comp_simp[[1]]) - 2))

# make plot for publication
plt_thema_simp <- ggplot(data = jsd_df_simp) +
  theme_minimal() +
  geom_point(mapping = aes(x = month, y = jsd),
             color = "#e6002e") +
  geom_smooth(mapping = aes(x = month_c, y = jsd),
              method = "lm", se = TRUE,
              color = "#e6002e") +
  labs(y = "Jensen-Shannon divergence [0,1]",
       x = "Month") +
  theme_minimal() +
  scale_colour_brewer(palette = "Set1") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank())
plt_thema_simp

ggsave(plt_thema_simp, file = "plots/simp/thema.png")
