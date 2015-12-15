library(ggplot2); library(tm); library(dplyr);
library(stringr); library(readr); library(wordcloud);
library(RColorBrewer)

source('~/ShakespeareProject/_code/_fittingFunctions.R')

#Create the corpuses of male and female characters
men <- VCorpus(DirSource('~/ShakespeareProject/_cleanData/MaleCharacters/', 
                         recursive = T));
women <- VCorpus(DirSource('~/ShakespeareProject/_cleanData/FemaleCharacters/', 
                           recursive = T));

#The cleaning functions we need to make the data pretty
funs <- list(removePunctuation, removeNumbers, stripWhitespace, content_transformer(tolower));

#Making the data pretty
men <- tm_map(men, FUN = tm_reduce, tmFuns = funs);
women <- tm_map(women, FUN = tm_reduce, tmFuns = funs);


#Our four document term matrices
dtmMen <- DocumentTermMatrix(men); dtmWomen <- DocumentTermMatrix(women);
weightedDtmMen <- weightTfIdf(dtmMen); weightedDtmWomen <- weightTfIdf(dtmWomen);


#Gets both all of the words spoken by all the characters and
#all the times each individual word was spoken
wordCountsMen <- colSums(as.matrix(weightedDtmMen)); 
wordCountsWomen <- colSums(as.matrix(weightedDtmWomen));
weightedCharCountsMen <- rowSums(as.matrix(weightedDtmMen)); 
weightedCharCountsWomen <- rowSums((as.matrix(weightedDtmWomen)))
maleCharCounts <- rowSums(as.matrix(dtmMen)); 
femaleCharCounts <- rowSums(as.matrix(dtmWomen));



#Finds the words both men and women use
wordsInCommon <- intersect(names(wordCountsMen), names(wordCountsWomen));

top60men <- sort(maleCharCounts, decreasing = T)[1:60]
top100women <- sort(femaleCharCounts, decreasing = T)[1:100]

topMenWordCounts <- colSums(as.matrix(weightedDtmMen)[names(top60men),]);
topWomenWordCounts <- colSums(as.matrix(weightedDtmWomen)[names(top100women),]);

commonMen <- subset(topMenWordCounts, names(topMenWordCounts) %in% wordsInCommon);
commonWomen <- subset(topWomenWordCounts, names(topWomenWordCounts) %in% wordsInCommon);

#The words that we got out of this
keyWords <- c('thrust', 'private', 'yourselves', 'whos', 'madam', 'visit', 'dare', 
              'courtesan', 'luce', 'lord', 'rot', 'lips', 'thankfully', 'madam', 'sir', 'ready');

keyWordMatrixMen <- as.matrix(weightedDtmMen)[,keyWords];
keyWordMatrixWomen <- as.matrix(weightedDtmWomen)[,keyWords];
keyWordMatrix <- rbind(keyWordMatrixMen, keyWordMatrixWomen);
gender <- c(rep('male', nrow(keyWordMatrixMen)), rep('female', nrow(keyWordMatrixWomen)));



modelDF <- data.frame('wordcount' = c(weightedCharCountsMen, weightedCharCountsWomen),
                      'gender' = gender, keyWordMatrix)

justCharCount <- bootStrapFit(modelDF, 1500, 'wordcount', 100, 100);
justWords <- bootStrapFit(modelDF, 1500, keyWords, 100, 100);
both <- bootStrapFit(modelDF, 1500, c(keyWords, 'wordcount'), 100, 100);

resultsDF <- data.frame('results' = c(justCharCount, justWords,  both), 
                        'model' = c(rep('wordcount', 1500), rep('words', 1500), 
                                    rep('both', 1500)));

resultsPlot <- ggplot(data = resultsDF, aes(fill = model))
resultsHist <- resultsPlot + geom_histogram(aes(x = results, position = 'identity')) +
  scale_y_continuous() + labs(title = 'Classification Accuracy Distribution by Model Type', 
                              x = 'Count', 'Accuracy');
resultsBox <- resultsPlot + geom_boxplot(aes(x = model, y = results)) + 
  labs(title = 'Classifcation Accuracy by Model Type', x = 'Model', y = 'Accuracy')

setwd('~/ShakespeareProject/_plots/')
pdf(file = 'resultsHist.pdf', width = 8, height = 6)
resultsHist; dev.off();
pdf(file = 'resultsBox.pdf', width = 6, height = 8)
resultsBox; dev.off();
png(filename = 'resultsHist.png'); resultsHist;
dev.off();
png(filename = 'resultsBox.png'); resultsBox;
dev.off();

set.seed(142)
dark2 <- brewer.pal(8, "Dark2")   
menWCPlot <- wordcloud(names(wordCountsMen), wordCountsMen, 
                   max.words = 50, rot.per = 0.2, colors = dark2)
womenWCPlot <- wordcloud(names(wordCountsWomen), wordCountsWomen, 
                         max.words = 50, rot.per = 0.2, colors = dark2)

pdf(file = 'menWordCloud.pdf', width = 8, height = 6)
menWCPlot; dev.off();
png(filename = 'menWordCloud.png'); menWCPlot;
dev.off();
pdf(file = 'womenWordCloud.pdf', width = 8, height = 6)
womenWCPlot; dev.off();
png(filename = 'womenWordCloud.png'); womenWCPlot;
dev.off();


dir.create('~/ShakespeareProject/_cleanData/_modelResults')
write_csv(resultsDF,  '~/ShakespeareProject/_cleanData/_modelResults/bootStrapResults.csv')





