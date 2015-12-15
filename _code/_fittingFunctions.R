
#All the functions needed to create and fit the model
library(dplyr); library(stringr);
library(readr); library(tm);


#Samples randomly from a DF created from the DocumentTermMatrix
#to create a training set to fit the model to
createTrainingSet <- function(data, malenum, femalenum) {
  male <- filter(data, gender == 'male');
  female <- filter(data, gender == 'female');
  
  maleSample <- sample_n(male, malenum);
  femaleSample <- sample_n(female, femalenum);
  
  df <- rbind(maleSample, femaleSample);
  
  return(df)
}


#Creates and fits a model to the training set
fitModel <- function(train, words) {
  predictors <- paste(words, collapse = '+')
  formula <- as.formula(paste('gender ~', predictors));
  model <- glm(formula, family = binomial, data = train)
  return(model);
}


#Decides whether a character is male or female based on the
#model's predictions (and a provided threshold)
mapGender <- function(val, thresh) {
  if (val >= thresh) {
    return('male')
  } else {
    return ('female')
  }
}

#Given a a model and a threshold, returns the percentage
#of characters that the model classified coprrectly
testFit <- function(model, words, df, thresh) {
  if (length(words) == 1) {
    keyWords <- data.frame(df[,words]);
    names(keyWords) <- words;
  } else {
    keyWords <- df[, words];
  }
  prediction <- predict.glm(model, newdata= as.data.frame(keyWords), 
                            type = 'resp');
  predictedGender <- mapply(mapGender, val = prediction, thresh = thresh);
  return(predictedGender)
}

#Tests eveery possible threshold from .01 to .99 (by .01)
#and returns the optimal one.
findOptThreshold <- function(model, words, df) {
  gender <- df$gender;
  threshold <- seq(.1, .99, .01);
  optThresh <- 0;
  bestProportion <- 0;
  wrapper <- function(thresh) {
    out <- testFit(model, words, df, thresh)
    success <- sum(gender == out)/1145;
    return(success);
  }
  
  output <- sapply(threshold, wrapper)
  return(max(output))
}


#Simulates the bootstrap that we run to test a model to find the
#most common words that show up in a random sample of 
#characters
findMeaningfulWords <- function(men, women, n) {
  men <- as.data.frame(as.matrix(men));
  women <- as.data.frame(as.matrix(women));
  common <- intersect(names(men), names(women))
  men <- men[,common]; women <- women[,common];
  data <- rbind(men, women);
  data <- mutate(data, 'gender' = c(rep('male', 1019), rep('female', 160)));
  
  differences <- list(length = n);
  for (i in 1:n) {
    train <- createTrainingSet(data, 50, 50);
    n = ncol(train) -1; 
    male <- filter(train, gender == 'male');
    female <- filter(train, gender == 'female');
    maleWordCounts <- colSums(as.matrix(male[, which(names(male) != 'gender')]));
    femaleWordCounts <- colSums(as.matrix(female[, which(names(female) != 'gender')]));
    wordsInCommon <- intersect(names(maleWordCounts), names(femaleWordCounts));
    
    maleInCommon <- maleWordCounts[which(names(maleWordCounts) %in% wordsInCommon)];
    femaleInCommon <- femaleWordCounts[which(names(femaleWordCounts) %in% wordsInCommon)];
    
    differences[[i]] <- sort(abs(maleInCommon - femaleInCommon), decreasing = T)[1:10]
  }
  
  return(differences);
}

#Use a hybrid bootstrap/cross validation approach to test
#the effectiveness of a model on the corpus as a whole.
bootStrapFit <- function(data, n, words, malenum, femalenum){
  trainingsets <- replicate(n, createTrainingSet(data, malenum, femalenum), simplify = F);
  wrapperFit <- function(train) {
    return(fitModel(train, words))
  }
  trainedModels <- lapply(trainingsets, wrapperFit);
  wrapperOpt <- function(model) {
    out <- findOptThreshold(model, words, data)
    return(out)
  }
  result <- sapply(trainedModels, wrapperOpt)
  return(result)
}

#Finds the meaningful words in the list of meaningful words
parseMeaningfulWords <- function(wordlist) {
  allwords <- list(length = length(wordlist));
  for (i in 1:length(wordlist)){
    allwords[[i]] <- names(wordlist[[i]])
  }
  allwords <- stack(allwords);
  return(table(allwords$values))
}