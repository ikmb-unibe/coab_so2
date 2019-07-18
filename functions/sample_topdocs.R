sample_topdocs <- function(model, texts, topics, n_sample, thresh) {
  thoughts <- findThoughts(model, texts = texts, topics = topics, n = 500, thresh = thresh)
  df <- as.data.frame(c(thoughts$index, thoughts$docs))
  if(nrow(df) <= n_sample){
    df <- df
  } else {
    df <- sample_n(df, n_sample)
  }
  colnames(df) <- c("d_index", "text")
  return(df$text)
}