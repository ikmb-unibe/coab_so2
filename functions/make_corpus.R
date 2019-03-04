make_online_corpus <- function(data) {
  corp <- corpus(data, docid_field = "d_id", text_field = "text")
  docvars(corp, "d_id") <- data$d_id
  docvars(corp, "duplicate_id") <- data$duplicate_id
  docvars(corp, "duplicate") <- data$duplicate
  docvars(corp, "page_id") <- data$page_id
  docvars(corp, "crawl") <- data$crawl
  docvars(corp, "language") <- data$language
  docvars(corp, "organisation") <- data$organisation
  docvars(corp, "actor_type") <- data$actor_type
  docvars(corp, "actor_country") <- data$actor_country
  docvars(corp, "position_new") <- data$position_new
  docvars(corp, "cooperation_climate") <- data$cooperation_climate
  return(corp)
}

make_offline_corpus <- function(data) {
  corp <- corpus(data, docid_field = "d_id", text_field = "text")
  docvars(corp, "d_id") <- data$d_id
  docvars(corp, "nsource_id") <- data$nsource_id
  docvars(corp, "language") <- data$language
  docvars(corp, "newspaper") <- data$newspaper
  docvars(corp, "date") <- data$date
  return(corp)
}
