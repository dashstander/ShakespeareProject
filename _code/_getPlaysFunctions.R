


library(readr); library(stringr); library(dplyr);

#Gets the names of all the plays in the complete works of Shakespeare
getPlayNames <- function(text){
  firstPass <- grep('by William Shakespeare', text) - 1;
  playNames = vector(mode = 'numeric');
  for (i in 1:length(firstPass)){
    num <- firstPass[i];
    line <- text[num];
    if (length(line) != 0){
      playNames[i] <- line;
    } else {
      while (line == ''){
        num = num-1;
        line <- text[num];
      }
      playNames[i] <- line;
    }
  }
  return(na.omit(playNames))
}


#Slices up a text file from beginning at a specified start pattern and
#until a specifified stop pattern
textSlicer <- function(text, startPattern, stopPattern = NULL){
  n <- length(text); 
  start <- grep(startPattern, text);
  end <- grep(stopPattern, text);
  playEnd <- min(subset(end, end > start[1]))
  firstSlice <- text[start[1]:playEnd]; end <- playEnd +1;
  remainder <- text[end:n];
  return(list(firstSlice, remainder))
}


#Uses textSlicer to get the text for all the plays,
#and then writes each play to a textfile.
playSliceRecur <- function(playNames, end, text){
  directory <- '~/ShakespeareProject/';
  documents <- data.frame();
  while (length(playNames) != 1){
    title <- playNames[1];
    sliced <- textSlicer(text, title, end);
    play <- unlist(sliced[1]);
    dir <- paste0(directory, '_rawData/')
    filepath <- paste0(dir, paste0(title, '.txt'))
    writeLines(play, filepath);
    text <- unlist(sliced[2]);
    playNames <- playNames[2:length(playNames)];
  }
}
