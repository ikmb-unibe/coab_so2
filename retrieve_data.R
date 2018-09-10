###################
## retrieve data ##
###################

ifelse(!require("tidyverse"), install.packages("tidyverse"), require(tidyverse))
ifelse(!require("RMySQL"), install.packages("RMySQL"), require(RMySQL))

options(stringsAsFactors = FALSE)

# establish ssh tunnel to COAB server in separate terminal (path to key file may be different on your machine)
# ssh -N research@coab.sowi.unibe.ch -p 3322 -i ~/Dokumente/SSH/ssh2.private -L 3306:localhost:3306 password:32rnHgQo

# establish connection to COAB MySQL data base
con <- dbConnect(RMySQL::MySQL(),
                 user = "research",
                 password = "H0XrTe4rgvHOe3OkoCSQ",
                 host = "127.0.0.1",
                 port = 3306)

# retrieve some stats for German online data
query.stats <- "SELECT crawl, COUNT(DISTINCT md.d_id) as n_pages, COUNT(DISTINCT md.top_domain) as n_domains
                FROM coab.climate_online_metadata as md 
                WHERE md.country_id = 1
                AND md.issue = 1
                AND md.has_keyword = 1
                GROUP BY md.crawl;"
data.stats <- dbGetQuery(con, query.stats)

data.stats$crawl <- as.numeric(data.stats$crawl)
data.stats <- data.stats[order(data.stats$crawl),]

# plot number of pages per crawl
ggplot(data = data.stats, aes(x = crawl, y = n_pages, group = 1)) +
  geom_line() +
  geom_point()
ggsave(filename = "plots/stats_pages_per_crawl.png", plot = last_plot(), device = "png")

# plot number of domains per crawl
ggplot(data = data.stats, aes(x = crawl, y = n_domains, group = 1)) +
  geom_line() +
  geom_point()
ggsave(filename = "plots/stats_domains_per_crawl.png", plot = last_plot(), device = "png")

# define good crawls (based on plots)
good.crawls <- c(2,4,5,6,7,8,9,10,11,12,13,15,16,17,18,19,20,21,22,25)

# retrieve German online data
query.ondata <- "SELECT md.d_id, md.page_id, md.crawl, md.country_id, md.issue, md.language, md.has_keyword, md.duplicate_id, d.text,
                        st.Host as domain, do.organisation, dca.Actor_type as actor_type, dca.Actor_country as actor_country, 
                        dcc.Actor_position as actor_position, dcc.Cooperation_Climate as cooperation_climate 
                 FROM coab.climate_online_metadata as md
                 LEFT JOIN coab.climate_online_documents as d on md.d_id = d.d_id
                 LEFT JOIN toolssql1.Pages as pg on md.page_id = pg.Page_ID
                 LEFT JOIN toolssql1.Sites as st on pg.Site_ID = st.Site_ID
                 LEFT JOIN toolssql1.Domain_Coding_All as dca on st.Host = dca.Domain
                 LEFT JOIN toolssql1.Domain_Coding_Climate as dcc on st.Host = dcc.Domain
                 LEFT JOIN toolssql1.Domain_Organisation as do on st.Host = do.domain
                 WHERE md.country_id = 1
                 AND md.issue = 1
                 AND md.has_keyword = 1;"
data.ondata <- dbGetQuery(con, query.ondata)

# delete data from faulty crawls
data.ondata.good <- filter(data.ondata, crawl %in% good.crawls)

# save clean online data
save(data.ondata.good, file = "data/data_online_good.RData")
