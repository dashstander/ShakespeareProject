#Sets up the directories, downloads the data, and gets all of the plays.


library(readr);

#Create all the directories we'll need and downloads
#the data

#All the functions we will need for this.
source('~/ShakespeareProject/_code/_getPlaysFunctions.R')

#This is the uncleaned up text. The complete works of Shakespeare, all together
dirtyText <- read_lines(paste0(directory, fileName));
dirtyText <- dirtyText[244:length(dirtyText)];

license1 <- '<<THIS ELECTRONIC VERSION OF THE COMPLETE WORKS OF WILLIAM'
license2 <- 'SHAKESPEARE IS COPYRIGHT 1990-1993 BY WORLD LIBRARY, INC., AND IS'
license3  <- 'PROVIDED BY PROJECT GUTENBERG ETEXT OF ILLINOIS BENEDICTINE COLLEGE'
license4 <- 'WITH PERMISSION.  ELECTRONIC AND MACHINE READABLE COPIES MAY BE'
license5 <- 'DISTRIBUTED SO LONG AS SUCH COPIES (1) ARE FOR YOUR OR OTHERS'
license6 <- 'PERSONAL USE ONLY, AND (2) ARE NOT DISTRIBUTED OR USED'
license7 <- "COMMERCIALLY.  PROHIBITED COMMERCIAL DISTRIBUTION INCLUDES BY ANY"
license8 <- 'SERVICE THAT CHARGES FOR DOWNLOAD TIME OR FOR MEMBERSHIP.>>'
license <- c(license1, license2, license3, license4, license5, license6, license7, license8);
for (i in 1:length(license)) {
  dirtyText <- na.omit(str_replace(dirtyText, license[i], ''))
}

distrib1 <- 'DISTRIBUTED SO LONG AS SUCH COPIES \\(1\\) ARE FOR YOUR OR OTHERS'
distrib2 <- 'PERSONAL USE ONLY, AND \\(2\\) ARE NOT DISTRIBUTED OR USED'
distrib <- c(distrib1, distrib2);

for (i in 1:2) {
  dirtyText <- na.omit(str_replace(dirtyText, distrib[i], ''));
}




playNames <- getPlayNames(dirtyText);




end <- 'THE END';
works <- list(mode = 'character');



playSliceRecur(playNames, end, dirtyText)
writeLines(dirtyText, '~/ShakespeareProject/_cleanData/cleanerShakespeare.txt')



