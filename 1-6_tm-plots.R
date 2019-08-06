#######################
# JSD and other plots #
#######################

ifelse(!require(tidyverse), install.packages("tidyverse"), require(tidyverse))
ifelse(!require(quanteda), install.packages("quanteda"), require(quanteda))
ifelse(!require(gridExtra), install.packages("gridExtra"), require(gridExtra))

options(stringsAsFactors = FALSE)

load("tm/theta.RData")
labels <- read_csv("tm/topwords_k20.csv")

right_media <- c("BILD", "BILD am Sonntag", "Die Welt", "FAZ", "FAZ am Sonntag", "Focus", "Handelsblatt", "Welt am Sonntag")

# Jensen-Shannon divergence

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

jsd_df <- bind_rows(jsd_df_2, jsd_df_3)

plt_thema <- ggplot(data = jsd_df) +
  theme_minimal() +
  geom_point(mapping = aes(x = month, y = jsd, color = type)) +
  geom_smooth(mapping = aes(x = month_c, y = jsd, color = type),
              method = "lm", se = FALSE) +
  labs(y = "Jensen-Shannon divergence [0,1]",
       x = "Month",
       title = "2. Thematic spill-over") +
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
plt_thema

# Jensen-Shannon divergence: aggregate level

theta_comp <- bind_rows(theta_off, theta_on) %>%
  group_by(type, month) %>%
  summarise_if(is.numeric, list(mean)) %>%
  gather(key = "topic", value = "prob", -type, - month) %>%
  spread(key = month, value = prob) %>%
  group_split()

jsd <- vector()

for (i in 1:(ncol(theta_comp[[1]]) - 2)) {
  p <- theta_comp[[1]][, (2 + i)]
  q <- theta_comp[[2]][, (2 + i)]
  m <- 0.5 * (p + q)
  jsd[i] <- 0.5 * (sum(p * log2(p / m)) + sum(q * log2(q / m))) # Jensen-Shannon divergence bounded in [0,1]
}

jsd_df <- data.frame(month = as.character(names(theta_comp[[1]])[3:ncol(theta_comp[[1]])]),
                     jsd = jsd,
                     month_c = 1:(ncol(theta_comp[[1]]) - 2))

plt <- ggplot(data = jsd_df) +
  theme_minimal() +
  geom_point(mapping = aes(x = month, y = jsd), col = "#4daf4a") +
  geom_smooth(mapping = aes(x = month_c, y = jsd),
              method = "lm", se = FALSE, col = "#4daf4a") +
  scale_colour_brewer(palette = "Set1") +
  labs(y = "Jensen-Shannon divergence [0,1]",
       x = "Month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ggsave(plt, file = "plots/jsd_trend.png", device = "png")
ggsave(plt, file = "plots/jsd_trend.pdf", device = "pdf")

# theta over time

theta_time <- bind_rows(theta_off, theta_on) %>%
  group_by(type, month) %>%
  summarise_if(is.numeric, list(mean)) %>%
  gather(key = "topic", value = "prob", -type, - month) %>%
  left_join(select(labels, -topwords), by = c("topic" = "t_id")) %>%
  mutate(t_label = paste(topic, label))

ggplot(data = theta_time) +
  theme_minimal() +
  geom_line(mapping = aes(x = month,
                          y = prob,
                          group = topic,
                          color = t_label)) +
  labs(y = "Mean probability overall actors",
       x = "Month", color = "Topic") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  facet_wrap(~ type, labeller = as_labeller(c("on" = "online", "off" = "offline")))

# zoom in on miot (most important online topics)

miot <- theta_time %>%
  filter(type == "on") %>% 
  group_by(type, t_label) %>%
  summarise(prob = mean(prob)) %>%
  arrange(desc(prob)) %>%
  slice(1:4)

plts <- list()

for (i in 1:nrow(miot)) {
  theta_time_miot <- theta_time %>%
    filter(t_label == miot$t_label[i]) %>%
    spread(key = type, value = prob) %>%
    mutate(delta = on - off) %>%
    gather(key = "type", value = "prob", -month, -topic, -label, -t_label) %>%
    mutate(type = case_when(type == "on" ~ "online",
                            type == "off" ~ "offline",
                            type == "delta" ~ "difference"))
  
  plts[[i]] <- ggplot(data = theta_time_miot) +
    theme_minimal() +
    geom_line(mapping = aes(x = month,
                            y = prob,
                            group = type,
                            color = type)) +
    geom_smooth(mapping = aes(x = month, 
                              y = prob, 
                              group = type,
                              color = type),
                method = "lm", se = FALSE) +
    labs(y = "Mean probability overall actors",
         x = "Month", color = "Topic", 
         title = theta_time_miot$t_label[i]) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          legend.title = element_blank())
}

grid.arrange(plts[[1]], plts[[2]], plts[[3]], plts[[4]], ncol = 2)

# Stats

# Documents online and offline without duplicates
dfm_online_nd
dfm_offline

# Documents online and offline with duplicates
bind_rows(theta_off_page, theta_on_page) %>%
  group_by(type) %>%
  summarise(sum = n())

# Actors online and offline
bind_rows(theta_off, theta_on) %>%
  group_by(type) %>%
  summarise(sum = n())

head(textstat_frequency(dfm_prep), 50)

# Jensen-Shannon divergence: individual newspaper level

crawl_month <- data.frame(crawl = 2:25,
                          month = c(paste("2012", c("07", "08", "09", "10", "11", "12"), sep = "-"),
                                    paste("2013", c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), sep = "-"),
                                    paste("2014", c("01", "02", "03", "04", "05", "06"), sep = "-")))

theta_comp_n <- theta_on %>%
  group_by(type, month) %>%
  summarise_if(is.numeric, list(mean)) %>%
  mutate(actor = "online") %>%
  select(type, actor, month, t_1, t_2, t_3, t_4, t_5, t_7, t_7, t_9, t_10, t_14, t_15, t_16, t_17, t_18, t_19) %>%
  bind_rows(theta_off) %>%
  gather(key = "topic", value = "prob", -type, -actor, -month) %>%
  spread(key = month, value = prob) %>%
  group_split()


newsp <- unique(theta_comp_n[[1]]$actor)

for(j in 1:length(newsp)){
  
  jsd <- vector()
  theta_comp_sel <- theta_comp_n[[1]] %>%
    filter(actor == newsp[j])
  
  for(i in 1:(ncol(theta_comp_sel) - 3)){
    p <- theta_comp_sel[, (3 + i)]
    q <- theta_comp_n[[2]][, (3 + i)]
    m <- 0.5 * (p + q)
    jsd[i] <- 0.5 * (sum(p * log2(p / m)) + sum(q * log2(q / m))) # Jensen-Shannon divergence bounded in [0,1]
  }
  
  jsd_df <- data.frame(month = as.character(names(theta_comp_sel)[4:ncol(theta_comp_sel)]),
                       jsd = jsd,
                       month_c = 1:(ncol(theta_comp_sel) - 3))
  
  plt <- ggplot(data = jsd_df) +
    theme_minimal() +
    geom_point(mapping = aes(x = month, y = jsd), col = "#4daf4a") +
    geom_smooth(mapping = aes(x = month_c, y = jsd),
                method = "lm", se = FALSE, col = "#4daf4a") +
    labs(y = "Jensen-Shannon divergence [0,1]") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.title.x = element_blank())
  
  ggsave(plt, file = paste0("plots/jsd_n/jsd_trend_", newsp[j], ".png"), device = "png")
  ggsave(plt, file = paste0("plots/jsd_n/jsd_trend_", newsp[j], ".pdf"), device = "pdf")
  
}




