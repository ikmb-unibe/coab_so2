if (!exists("LANGUAGE")) stop("Set LANGUAGE = \"de\" or \"en\" in script which sources this config file")

# EN
if (LANGUAGE == "en") {
  
  LANG_CODE = "en"
  final_svm_model_file <- "classifier/final_svm_model_eng.RData"
  final_svm_model_featurelist <- "classifier/final_svm_featurelist_eng.RData"
  export_file_ids <- "classified_sentences_ids_eng.csv"
  export_file_fulltext <- "classified_sentences_full_eng.csv"
  
  FE_useStemming = T
  FE_useBigrams = T 
  FE_removeSW = F 
  FE_negation = T 
  FE_tolower = T
  
}

# DE
if (LANGUAGE == "de") {
  
  LANG_CODE = "de"
  final_svm_model_file <- "classifier/final_svm_model_deu.RData"
  final_svm_model_featurelist <- "classifier/final_svm_featurelist_deu.RData"
  export_file_ids <- "classified_sentences_ids_deu.csv"
  export_file_fulltext <- "classified_sentences_full_deu.csv"
  
  FE_useStemming = T
  FE_useBigrams = F 
  FE_removeSW = T 
  FE_negation = T 
  FE_tolower = F
  
}

