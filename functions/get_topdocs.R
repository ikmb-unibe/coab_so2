get_topdocs <- function (model, texts, n.docs, threshold) {
  thoughts.df <- data.frame(topic = integer(),
                            d_id = integer(),
                            prop = integer(),
                            text = as.character())
  for(i in 1:model$settings$dim$K){
    #get documents fulfilling the requirements (treshhold, n.docs)
    thoughts <- findThoughts(model = model, texts = texts$text, topics = i, n = 500, thres = threshold)
    df <- as.data.frame(c(thoughts$index, thoughts$docs))
    if(nrow(df) <= n.docs){
      df <- df
    } else {
      df <- sample_n(df, n.docs)
      colnames(df) <- c("d_index", "text")
      # add correct d_id of the documents
      d_id <- as.data.frame(texts[df$d_index,]$d_id)
      colnames(d_id) <- "d_id"
      df <- cbind(d_id, df)
      # add exact topic proportion
      theta <- as.data.frame(model$theta[df$d_index,])
      theta <- select(theta, prop = paste0("V", i))
      df <- cbind(theta, df)
      # combine everything
      df <- df %>%
        mutate(t_id = paste0("t_",i)) %>%
        select(t_id, d_id, prop, text)
      # append documents for every topic
      thoughts.df <- rbind(thoughts.df, df)
    }
  }
  return(thoughts.df)
}
