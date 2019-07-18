do_classification <- function(df) {
  
  # prepare text for classification
  sentence_texts <- df$sentence
  feature_matrix <- generateFeatureMatrix(sentence_texts, TRAIN = FALSE, featureDictionary = feature_list)
  
  # do actual classification
  result <- predict(svm_model, convertToSparseM(feature_matrix))
  
  # obtain results
  result.df <- data.frame(result)
  colnames(result.df) <- "classification"
  classification.df <- cbind(df, result.df)
  
  return(classification.df)

}