
prepareData <- function(DATADIR, file) {
  
  files <- list.files(DATADIR)
  allData <- data.frame()
  for (datafile in files) {
    currentData <- read.csv(paste0(DATADIR, datafile))
    metadata <- strsplit(gsub(".csv", "", datafile), "_")[[1]]
    currentData <- cbind(currentData, country = metadata[1], party = metadata[2], year = metadata[3])
    # clean NA codes
    currentData <- currentData[!is.na(currentData["cmp_code"]), ]
    allData <- rbind(allData, currentData)
  }
  
  write.csv(allData, file = file)
  
}


extractFeatures = function(sentence, useStemming = TRUE, useBigrams = FALSE, removeSW = FALSE, negation = TRUE, language = "english", toLower = TRUE) {
  
  global_negation_words <- negation_word_list(language = LANG_CODE)
  globalStopWordList <- setdiff(stopwords(LANG_CODE), global_negation_words)
  
  # C weights YES!!!
  # stem -> yes
  # bigrams -> YES, but filtered (Boulis, Ostendorf)
  # bigrams w/o sw in between -> YES, but filtered
  # tf-idf / unitLength / 01-scaling -> NOT (makes all worse)
  
  words <- regmatches(sentence, gregexpr("[\\p{L}-]+|[^\\p{L} ]+", sentence, perl = T))[[1]]
  
  features <- c()
  
  if (toLower) {
    wordsInS <- tolower(words)
  } else {
    wordsInS <- words
  }
  
  if (length(wordsInS) > 2) {
    i <- 0
    while (i < length(wordsInS)-2) {
      i <- i + 1
      if (grepl("n$", wordsInS[i]) & wordsInS[i+1] == "'" & wordsInS[i+2] == "t") {
        wordsInS[i] <- gsub("n$", "", wordsInS[i])
        wordsInS <- wordsInS[-(i+1)]
        wordsInS[i+1] <- "not"
      }
    }
  }
  
  if (useStemming) {
    wordsInS <- wordStem(wordsInS, language = LANG_CODE)
    # globalStopWordList <- unique(wordStem(globalStopWordList))
  }
  
  if (removeSW) {
    wordsInS <- wordsInS[!(wordsInS %in% globalStopWordList)]
  }
  
  # UNIGRAMS
  unigrams <- table(wordsInS)
  features <- c(features, unigrams)
  
  # NEGATION
  negation_scope <- 4
  if (negation) {
    negation_words <- tolower(wordsInS) %in% global_negation_words
    if (any(negation_words)) {
      n <- length(wordsInS)
      for (neg_word_idx in which(negation_words)) {
        if (neg_word_idx < n) {
          max_pos <- min(n, neg_word_idx + negation_scope)
          next_words <- wordsInS[(neg_word_idx+1):max_pos]
          words_negated <- paste0("neg#", next_words)
        } else {
          min_pos <- max(1, neg_word_idx - negation_scope)
          prev_words <- wordsInS[min_pos:(neg_word_idx-1)]
          words_negated <- paste0("neg#", prev_words)
        }
        features <- c(features, table(words_negated))
      }
    }
  }
  
  # BIGRAMS
  if (useBigrams & length(wordsInS) > 1) {
    bigrams <- c()
    for (i in 1:(length(wordsInS)-1)) {
      bigram <- paste0(c(wordsInS[i], wordsInS[(i+1)]), collapse="_")
      bigrams <- c(bigrams, bigram)
    }
    features <- c(features, table(bigrams))
    
    # if stop words are not removed, still do it and concat new bigrams
    if (!removeSW) {
      wordsNotSW <- wordsInS[!(wordsInS %in% globalStopWordList)]
      if (length(wordsNotSW) > 1) {
        bigramsNSW <- c()
        for (i in 1:(length(wordsNotSW)-1)) {
          bigram <- paste0(c(wordsNotSW[i], wordsNotSW[(i+1)]), collapse="_")
          if (!(bigram %in% names(features))) {
            bigramsNSW <- c(bigramsNSW, bigram)
          }      
        }
        features <- c(features, table(bigramsNSW))
      }
    }
    
  }
  
  # remove empty feature
  features <- features[!names(features) %in% c("", " ")]
  
  return(features)
  
}




performTestsetValidation = function(featureMatrixTrain, resultVectorTrain, featureMatrixTest, resultVectorTest, tuneC = TRUE) {
  
  # for debug:
  #featureMatrixTrain <- featureMatrix_Trainset
  #resultVectorTrain <- resultVectorForTraining
  #featureMatrixTest <- featureMatrix_Testset
  #resultVectorTest <- resultVectorForTesting
  
  print(paste0("Training on ", nrow(featureMatrixTrain), " examples"))
  
  
  if (tuneC) {
    optimalC <- getOptimalCParameter(featureMatrixTrain, resultVectorTrain)
  } else {
    optimalC <- 1
  }
  
  # SVM weights
  cWeights <- table(resultVectorTrain) / length(resultVectorTrain)
  cWeights <- abs(cWeights - 1)
  
  
  m <- LiblineaR(convertToSparseM(featureMatrixTrain), resultVectorTrain, wi = cWeights, cost = optimalC, type = 7)
  print(paste0("Testing on ", nrow(featureMatrixTest), " examples"))
  predLabels <- predict(m, convertToSparseM(featureMatrixTest))$predictions
  
  result <- F.measure(predLabels, resultVectorTest)
  
  require(irr)
  result <- c(result, alpha = kripp.alpha(t(data.frame(predLabels, resultVectorTest)))$value)
  result <- c(result, kappa = kappa2(data.frame(predLabels, resultVectorTest))$value)
  
  print(result)
  
  return(result)
}


generateFeatureMatrix <- function(textVector, TRAIN = TRUE, featureDictionary = NULL, OPTION_minimumFeatureOccurrence = 1, binary = TRUE) {
  
  require("hash")
  require("Matrix")
  
  print("-----------")
  if (TRAIN) {    
    print("Extracting features from training data")
  } else {
    print("Extracting features from test data")
  }
  
  # extract features from CMP data
  corpusLength <- length(textVector)
  #corpusLength <- 3000
  #featureList <- vector("list", corpusLength)
  if (exists("featureList")) {
    clear(featureList)
  } else {
    featureList <- hash()
  }
  
  if (exists("globalVocabHash")) {
    clear(globalVocabHash)
  } else {
    globalVocabHash <- hash()
  }
  
  if (!TRAIN) {
    globalVocabHash <- hash(featureDictionary, 0)
  }
  
  countEmptyFeatureVectors <- 0
  
  allSentencesInVector <- as.character(textVector)
  
  featureList <- foreach (sentNumber = 1:corpusLength) %dopar% {
    
    currentFeatureVector <- extractFeatures(allSentencesInVector[sentNumber], useStemming = FE_useStemming, useBigrams = FE_useBigrams, removeSW = FE_removeSW, negation = FE_negation, toLower = FE_tolower)
    
    if (length(currentFeatureVector) > 0) {
      
      if (!TRAIN) {
        # RESTRICT TO VOCAB
        knownFeatures <- has.key(names(currentFeatureVector), globalVocabHash)        
        if (any(knownFeatures)) {
          currentFeatureVector <- currentFeatureVector[knownFeatures]
        } else {
          currentFeatureVector <- table("the")
          print(paste0("Warning, no features extracted for: ", allSentencesInVector[sentNumber]))
        }
      }
    } else {
      currentFeatureVector <- table("the")
    }
    
    if (binary) {
      count <- 1
    } else {
      count <- as.integer(currentFeatureVector)
    }
    
    dt <- data.table(id = sentNumber, token = names(currentFeatureVector), count = 1)
  }
  
  # combine list of features into feature matrix
  
  print("Create feature matrix from feature list")
  
  combinedDT <- rbindlist(featureList)
  
  if (TRAIN) {
    vocabFactor <- factor(combinedDT$token)
  } else {
    vocabFactor <- factor(combinedDT$token, levels = featureDictionary)
  }
  
  featureNames <- levels(vocabFactor)
  
  featureMatrixFull <- sparseMatrix(i = combinedDT$id, j = as.integer(vocabFactor), x = combinedDT$count, 
                                    dims = c(length(featureList), length(featureNames)), 
                                    dimnames = list(1:length(featureList), featureNames))
  
  if (TRAIN) {
    # reduce feature set size
    featuresToRemove <- which(colSums(featureMatrixFull) < OPTION_minimumFeatureOccurrence)
    if (length(featuresToRemove) > 0) featureMatrixFull <- featureMatrixFull[, -featuresToRemove]
  }
  
  print(dim(featureMatrixFull))
  
  return(featureMatrixFull)
}



generateResultVector <- function(codes, codesToPositiveCategory) {
  corpusLength <- length(codes)
  resultVector <- rep(0, corpusLength)
  
  positiveExamplesLdx <- codes %in% codesToPositiveCategory
  negativeExamplesLdx <- !positiveExamplesLdx
  
  resultVector[positiveExamplesLdx] <- 1
  resultVector <- as.factor(resultVector)
  return(resultVector)
}


cleanVocabulary <- function(words, lang = "english") {
  require("tm")
  words <- words[!grepl("_", words)]
  myStopWords <- union(stopwords(lang), c(",", ";", ".", "(", ")", "?", "!", "-", "–", "—", ":", "•", "“", "„", "…", "·"))
  words <- setdiff(words, myStopWords)
  return(words)
}

convertToSparseM <- function(X) {
  X.csc <- new("matrix.csc", ra = X@x,
               ja = X@i + 1L,
               ia = X@p + 1L,
               dimension = X@Dim)
  return(as.matrix.csr(X.csc))
}


getOptimalCParameter <- function(fm, rv, k = 10, ...) {
  
  # heuristicC from LibLineaR (usually works bad)
  # sampledFm <- as.matrix(fm[sample(nrow(fm), 1000)])
  # optimalC <- heuristicC(sampledFm)
  # return(optimalC)
  
  maxF1 <- 0
  cValues <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  i <- 0
  for (cValue in cValues) {
    i <- i + 1
    cvResult <- performCrossValidation(fm, rv, k = k, cost = cValue, ...)
    if (cvResult$micro["F"] > maxF1) {
      maxF1 <- cvResult$micro["F"]
      optimalC <- cValue
    }
  }
  print(paste0("Selected C = ", optimalC))
  return(optimalC)
}



# helper function to get data fold for cross validation
kFoldGetDataFold = function(featureMatrix, resultVector, k, foldNumber) {
  n <- length(resultVector)
  folds <- rep(seq(1:k),ceiling(n/k))[1:n]
  idxTest <- folds==foldNumber
  idxTrain <- !idxTest
  res <- list(
    trainFeatures = featureMatrix[idxTrain,], 
    trainRes = resultVector[idxTrain], 
    testFeatures = featureMatrix[idxTest,], 
    testRes = resultVector[idxTest]
  )
  return(res)
}

# ------------------------------------------------------------
# K-FOLD CROSS VALIDATION
# ------------------------------------------------------------

performCrossValidation = function(featureMatrix, resultVector, k = 10, verbose = FALSE, ...) {
  
  require(irr)
  
  numberOfClasses <- length(levels(resultVector))
  
  evaluationResult <- NULL
  evaluationResultMicro <- NULL
  
  for (i in 1:k) {
    
    d <- kFoldGetDataFold(featureMatrix, resultVector, k, i)
    
    cWeights <- table(d$trainRes) / length(d$trainRes)
    cWeights <- abs(cWeights - 1)
    
    m <- LiblineaR(convertToSparseM(d$trainFeatures), d$trainRes, wi = cWeights, ...)
    predLabels <- predict(m, convertToSparseM(d$testFeatures))$predictions
    
    tmp <- F.measure(predLabels, d$testRes)
    
    if (is.list(tmp)) {
      partResult <- tmp$macro
      if (is.null(evaluationResultMicro)) {
        evaluationResultMicro <- tmp$micro / k
      } else {
        evaluationResultMicro <- evaluationResultMicro + tmp$micro / k
      }
    } else {
      partResult <- tmp
    }
    partResult[, 2:ncol(partResult)] <- partResult[, 2:ncol(partResult)] / k
    if (is.null(evaluationResult)) {
      evaluationResult <- partResult
    } else {
      evaluationResult <- evaluationResult + partResult
    }
    
  }
  
  result <- list(macro = NULL, micro = NULL)
  
  if (numberOfClasses > 2) {
    # macro
    all <- colMeans(evaluationResult)
    all[1] <- sum(evaluationResult[, "N"])
    evaluationResult <- rbind(evaluationResult, all)
    if (verbose) print(evaluationResult)
    result$macro <- evaluationResult
  } 
  
  # micro
  if (verbose) print(evaluationResultMicro)
  
  result$micro <- evaluationResultMicro
  
  return(result)
}


classify <- function(featureMatrix, resultVector, fmUnlabeled, ...) {
  
  numberOfClasses <- length(levels(resultVector))
  
  cWeights <- table(resultVector) / length(resultVector)
  cWeights <- abs(cWeights - 1)
  
  m <- LiblineaR(convertToSparseM(featureMatrix), resultVector, wi = cWeights, ...)
  predLabels <- predict(m, convertToSparseM(fmUnlabeled))
  
  return(predLabels)
}


convertDTMtoLDAFormat = function(DTM) {
  nOfDocs <- nrow(DTM)
  documents <- vector("list", nOfDocs)
  pb <- txtProgressBar(min = 0, max = nOfDocs)
  for (idx in 1:nOfDocs) {
    setTxtProgressBar(pb, idx)
    LL <- which(DTM[idx,] > 0)
    vocabIdx <- as.integer(LL - 1)
    vocabCounts <- as.integer(DTM[idx,LL])
    documents[[idx]] <- t(matrix(c(vocabIdx, vocabCounts), ncol=2, nrow=length(vocabCounts)))
  }
  close(pb)
  return(documents)
}

computeTTR <- function(m) {
  m <- m[, !grepl("_", colnames(m))]
  tokenCount <- sum(m)
  typeCount <- sum(colSums(m) > 0)
  TTR <- typeCount/tokenCount
  return(TTR)
}



# MICRO AVERAGE: TP,FP,FN over all category decisions first, then F1
# MACRO AVERAGE: F1 over each individual categories first, then average
F.measure <- function(inPred, inLabels, positiveClassName = NULL, eval_kappa = T, eval_alpha = T) {
  
  warningOption <- getOption("warn")
  options(warn = -1)
  
  # PREPARE DATA
  allpred <- as.vector(inPred)
  alllabels <- as.vector(inLabels)
  classes <- sort(unique(c(allpred,alllabels)))
  
  if (length(classes) == 2) {
    # SINGLE CLASS EVALUATION
    
    if (is.null(positiveClassName)) {
      # assume positiveClassName is classes != 0
      positiveClassName <- classes[which(classes != 0)]
    }
    pred <- ifelse(allpred == positiveClassName, 1, 0)
    labels <- ifelse(alllabels == positiveClassName, 1, 0)
    
    if (length(pred)!=length(labels)) stop("F.measure: lengths of true and predicted labels do not match.");
    neg.labels <- which(labels == 0);  
    pos.labels <- which(labels == 1);
    npos <- length(pos.labels);   
    
    TP <- sum(pred[pos.labels] == 1);
    FP <- sum(pred[neg.labels] == 1);
    FN <- sum(pred[pos.labels] == 0);
    TN <- sum(pred[neg.labels] == 0);
    acc <- (TP+TN)/length(labels);  
    if ((TP+FP) == 0) {
      precision <- 0
    } else { 
      precision <- TP/(TP+FP)
    }
    if ((TP+FN) == 0) {
      recall <- 0
    } else {
      recall <- TP/(TP+FN)
    }    
    if ((TN+FP) == 0) {
      specificity <- 0
    } else {
      specificity <- TN/(TN+FP)
    }    
    if ((precision+recall) == 0) {
      F <- 0
    } else { 
      F = 2 *(precision*recall) / (precision+recall); 
    }
    results <- c(precision, recall, specificity, F, acc, npos);
    names(results) <- c("P", "R", "S", "F", "A", "Pos.");  
    if (eval_kappa) {
      require(irr)
      kappa <- kappa2(data.frame(allpred, alllabels))$value
      results <- c(results, kappa)
    }
    if (eval_alpha) {
      require(irr)
      alpha <- kripp.alpha(t(data.frame(allpred, alllabels)))$value
      results <- c(results, alpha)
    }
    fList <- results
    
  } else {
    # MULTI CLASS EVALUATION
    
    # MICRO
    TPmic <- 0
    FPmic <- 0
    FNmic <- 0  
    
    # MACRO
    results <- NULL
    
    # ITERATE POVER CLASSES
    for (classe in classes) {
      
      pred <- ifelse(allpred == classe, 1, 0)
      labels <- ifelse(alllabels == classe, 1, 0)
      
      if (length(pred)!=length(labels)) stop("F.measure: lengths of true and predicted labels do not match.");
      neg.labels <- which(labels == 0);  
      pos.labels <- which(labels == 1);
      npos <- length(pos.labels);   
      
      TP <- sum(pred[pos.labels] == 1);
      FP <- sum(pred[neg.labels] == 1);
      FN <- sum(pred[pos.labels] == 0);
      TN <- sum(pred[neg.labels] == 0);
      #print(c(TP,FP,FN,TN))
      acc <- (TP+TN)/length(labels);
      
      TPmic <- TPmic + TP
      FPmic <- FPmic + FP
      FNmic <- FNmic + FN
      
      if ((TP+FP) == 0) {
        precision <- 0
      } else { 
        precision <- TP/(TP+FP)
      }
      if ((TP+FN) == 0) {
        recall <- 0
      } else {
        recall <- TP/(TP+FN)
      }    
      if ((TN+FP) == 0) {
        specificity <- 0
      } else {
        specificity <- TN/(TN+FP)
      }    
      if ((precision+recall) == 0) {
        F <- 0
      } else { 
        F = 2 *(precision*recall) / (precision+recall); 
      }    
      res <- c(N = npos, P = precision, R = recall, S = specificity, F = F, accuracy = acc);
      
      if (eval_kappa) {
        require(irr)
        kappa <- kappa2(data.frame(pred, labels))$value
        res <- c(res, kappa = kappa)
      }
      if (eval_alpha) {
        require(irr)
        alpha <- kripp.alpha(t(data.frame(pred, labels)))$value
        res <- c(res, alpha = alpha)
      }
      
      results <- rbind(results, res)
      
    }
    
    rownames(results) <- classes
    
    # MICRO
    if ((TPmic+FPmic) == 0) {
      precisionMicro <- 0
    } else { 
      precisionMicro <- TPmic/(TPmic+FPmic)
    }
    if ((TPmic+FNmic) == 0) {
      recallMicro <- 0
    } else {
      recallMicro <- TPmic/(TPmic+FNmic)
    }
    
    #print(recallMicro)
    #print(precisionMicro)
    
    fMicro <- 2 *(precisionMicro*recallMicro) / (precisionMicro+recallMicro); 
    resultMicro <- c(precisionMicro, recallMicro, fMicro)
    names(resultMicro) <- c("P", "R", "F");
    if (eval_kappa) {
      require(irr)
      kappa <- kappa2(data.frame(allpred, alllabels))$value
      resultMicro <- c(resultMicro, kappa = kappa)
    }
    if (eval_alpha) {
      require(irr)
      alpha <- kripp.alpha(t(data.frame(allpred, alllabels)))$value
      resultMicro <- c(resultMicro, alpha = alpha)
    }
    
    fList <- list(macro = results, micro = resultMicro)
    
  }
  options(warn = warningOption)
  return (fList);
}


probClassGivenFeature <- function(feat, cl) {
  idx <- feat > 0
  count <- sum(cl[idx])
  res <- sum(cl[idx]) / length(cl[idx])
  #if (is.nan(res)) res <- 0
  return(res)
}


computeFeatureChiSquare <- function(featureMatrix, resultVector, trimFeature = TRUE) {
  
  if (trimFeature) {
    
    print(paste0("Original feature set: ", ncol(featureMatrix)))
    
    minimumNegativeCount <- 3
    
    negativeIdx <- resultVector == "0"
    positiveIdx <- !negativeIdx
    
    featureNegativeCount <- colSums(featureMatrix[negativeIdx, ])
    featurePositiveCount <- colSums(featureMatrix[positiveIdx, ])
    
    trimCandidates <- featureNegativeCount < minimumNegativeCount
    trimCandidates <- trimCandidates & (featurePositiveCount == 0)
    
    featureMatrix <- featureMatrix[, (!trimCandidates)]
    
    print(paste0("Trimmed feature set: ", ncol(featureMatrix)))
    
  }
  
  probClasses <- table(resultVector) / length(resultVector)
  
  featureSetSize <- ncol(featureMatrix)
  
  pbFeatureSelection <- txtProgressBar(min = 0, max = featureSetSize)
  
  chiSquareMax = rep(0, ncol(featureMatrix))
  names(chiSquareMax) <- colnames(featureMatrix)
  kld <- rep(0, ncol(featureMatrix))
  names(kld) <- colnames(featureMatrix)
  
  for (i in 1:ncol(featureMatrix)) {
    
    setTxtProgressBar(pbFeatureSelection, i)
    maxChiSquare <- 0
    kldFeature <- 0
    for (class in levels(resultVector)) {
      
      testResult <- chiSquareStatistic(table(featureMatrix[, i], resultVector==class))
      
      if (testResult > maxChiSquare) {
        maxChiSquare <- testResult
      }
      
      p1 <- probClassGivenFeature(featureMatrix[, i], resultVector==class)
      if (p1 > 0) {
        kldFeature <- kldFeature + (p1 * log(p1 / probClasses[class]))
      }      
      
    }
    chiSquareMax[i] <- maxChiSquare
    kld[i] <- kldFeature
  }
  
  close(pbFeatureSelection)
  
  result <- list(chiSquare = chiSquareMax, kld = kld)
  
  return(result)
}

chiSquareStatistic <- function (x, correct = TRUE){
  if (is.matrix(x)) {
    
    if (min(dim(x)) == 1L) return(0)
    
    nr <- as.integer(nrow(x))
    nc <- as.integer(ncol(x))
    if (is.na(nr) || is.na(nc) || is.na(nr * nc)) 
      stop("invalid nrow(x) or ncol(x)", domain = NA)
    if ((n <- sum(x)) == 0) 
      stop("at least one entry of 'x' must be positive")
    sr <- rowSums(x)
    sc <- colSums(x)
    E <- outer(sr, sc, "*")/n
    v <- function(r, c, n) c * r * (n - r) * (n - c)/n^3
    V <- outer(sr, sc, v, n)
    dimnames(E) <- dimnames(x)
    
    if (correct && nrow(x) == 2L && ncol(x) == 2L) {
      YATES <- min(0.5, abs(x - E))
    } else {
      YATES <- 0
    }
    
    STATISTIC <- sum((abs(x - E) - YATES)^2/E)
  } else {
    stop("x has false format")
  }
  return(STATISTIC)
}



convertSlamToMatrix <- function(DTM) {
  # Ensure matrix or Matrix-format (convert if SparseM)
  require(Matrix)
  require(slam)
  if (is.simple_triplet_matrix(DTM)) {
    DTM <- sparseMatrix(i=DTM$i, j=DTM$j, x=DTM$v, dims=c(DTM$nrow, DTM$ncol), dimnames = dimnames(DTM))
  }
  return(DTM)
}

negation_word_list <- function(language = "en") {
  switch(language,
         en = {
           negation_list <- c("no", "non", "none", "not", "neither", "nor", "never", "nobody", "nothing", "nowhere", "doesn't", "cannot", "don't", "n't", "wasn't", "hasn't", "didn't", "haven't", "hadn't", "weren't")
         },
         de = {
           negation_list <- c("nicht", "nichts", "kein", "keine", "keiner", "keines", "keinen", "keinem", "weder", "niemand", "nie", "niemals", "nirgends", "nirgendwo", "nirgendwohin", "außer", "ohne")
         },
         {
           stop("Error: language not supported")
         }
  )
  return(negation_list)
}

