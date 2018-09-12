require(quanteda)

make_corpus <- function(data) {
  corp <- corpus(data, docid_field = "d_id", text_field = "text")
  docvars(corp, "d_id") <- data$d_id
  docvars(corp, "page_id") <- data$page_id
  docvars(corp, "crawl") <- data$crawl
  docvars(corp, "language") <- data$language
  docvars(corp, "page_id") <- data$duplicate_id
  docvars(corp, "domain") <- data$domain
  docvars(corp, "organisation") <- data$organisation
  docvars(corp, "actor_type") <- data$actor_type
  docvars(corp, "actor_country") <- data$actor_country
  docvars(corp, "actor_position") <- data$actor_position
  docvars(corp, "cooperation_climate") <- data$cooperation_climate
  docvars(corp, "duplicate") <- data$duplicate
  return(corp)
}