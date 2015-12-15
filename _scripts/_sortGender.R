library(tm); library(dplyr); library(readr);


SHAKESPEARE <- read_csv('~/ShakespeareProject/ShakespeareCharacters.csv')

listofPlays <- unique(SHAKESPEARE$Play);
FLAG <- TRUE;
for (i in 1:length(listofPlays)){
  
  play <- toupper(listofPlays[i]);
  print(play);
  if(FLAG) {
    malePath <- paste0('~/ShakespeareProject/_cleanData/MaleCharacters/', play);
    femalePath <- paste0('~/ShakespeareProject/_cleanData/FemaleCharacters/', play);
    FLAG <- FALSE;
  }
  dir.create(malePath);
  dir.create(femalePath);
  playDF <- filter(SHAKESPEARE, toupper(Play) == play);
  path <- paste0('~/ShakespeareProject/_cleanData/', play);
  
  ourCharList <- list.files(path);
  ourCharList <- str_replace(ourCharList, '.txt$', '');
  ourCharList <- na.omit(intersect(playDF$Speaker, ourCharList));
  for (j in 1:length(ourCharList)) {
    name <- ourCharList[j]; filename <- paste0(name, '.txt');
    gender <- filter(playDF, Speaker == name)$Sex;
    filePath <- paste(path, filename, sep = '/');
    char <- read_lines(filePath);
    if (gender == 'Male') {
        writeLines(char, paste(malePath, filename, sep = '/'))
    } else {
      writeLines(char, paste(femalePath, filename, sep = '/'))
    }
  }
  FLAG <- TRUE;
}

