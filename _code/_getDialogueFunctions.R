#All the functions we need to 

library(stringr); library(readr); library(tm); library(dplyr);

#Finds the names of all the characters in a play (as they're identified)
#before their dialogue and returns it as a list.
getCharacters <- function(text){
  chars <- unique(str_trim(na.omit(str_match(text, '^  [A-Z ]{3,}\\.'))));
  if (length(chars) < 5){
    chars <- unique(str_trim(na.omit(str_match(text, '^  [:alpha:]{3,}\\.'))))
  }
  if (length(chars) < 5){
    chars <- unique(str_trim(na.omit(str_match(text, '^  [A-Za-z ]{3,}\\.'))))
  }
  dramPersonae <- removePunctuation(chars);
  dramPersonae <- unique(str_trim(dramPersonae));
  dramPersonae <- dramPersonae[which(dramPersonae != '')];
  return(dramPersonae);
}


#Takes in the Dramatis Personae, and outputs a list.
#Each element of the list stands in for a single character and 
#is a vector with all of the locations where a line of that character's
#dialogue begins
getDialogueLoc <- function(chars, play){
  names <- removePunctuation(str_trim(na.omit(chars)));
  chars <- names;
  personae <- list(); n <- length(chars);
  for (i in 1:n) {
    name <- chars[i]; t <- nchar(name);
    period <- str_sub(name, t, t)
    chars[i] <- paste0(name, '\\.');
  }
  
  for (i in 1:n){
    name <- chars[i]; pattern <- paste0('^  ', name);
    locations <- grep(pattern, play);
    if (length(locations) != 0) {
      personae[[i]] <- grep(pattern, play);
    } else {
      pattern <- paste0('^', name);
      personae[[i+ 0]] <- grep(pattern, play);
    }
  }
  names(personae) <- names;
  return(personae)
}


#Takes in a list of vectors and returns the union of all of the 
#vectors in the list
getUnion <- function(locs){
  total <- unlist(locs[1]);
  n <- length(locs);
  for (i in 2:n){
    total <- union(total, unlist(locs[i]))
  }
  return(sort(total));
}

#Takes in a vector of the locations of dialogue for one character
#a vector of the locations for all the other characters (made with getUnion)
#and the text itself. 
#Gets all of a single character's dialogue by taking all of the text between the start
#of one character's dialogue and the start of any of the other characters' dialogue.
getQuoteBlock <- function(locs, diff, text) {
  final <- length(text);
  locs <- na.omit(locs);
  m <- length(locs);
  charDialogue <- vector();
  prev <- vector(mode = 'numeric');
  for (j in 1:m){
    line <- locs[j];
    if (is.na(line)){
      break;
    }
    relevantSection <- diff[which(diff > line)];
    if (length(relevantSection) == 0) {
      charDialogue <- c(charDialogue, na.omit(text[line:final]));
    } else {
      endDialogue <- min(relevantSection);
      charDialogue <- c(charDialogue, text[line:(endDialogue -1)]);
      prev <- j;
    }
  }
  return(charDialogue)
}

#Uses getUnion and getQuote block and iterates over 
#a list of locations (from getDialogueLoc) to get
#all the dialogue of each character and puts it into
#a list.
getAllDialogue <- function(locs, text){
  n <- length(locs);
  dialogue <- list();
  total <- getUnion(locs);
  for (i in 1:n) {
    curr <- unlist(na.omit(locs[[i]]));
    diff <- setdiff(total, curr);
    charDialogue <- getQuoteBlock(curr, diff, text)
    dialogue[[i]] <- na.exclude(charDialogue);
  }
  names(dialogue) <- str_trim(names(locs));
  return(dialogue)
}


writeCharDialogue <- function(dial, path){
  if (!is.list(dial)){
    cat('Wrong input')
    cat(class(dial))
  }
  characters <- removePunctuation(names(dial));
  for (i in 1:length(dial)){
    char <- as.character(dial[[i]]); name <- paste0(characters[i], '.txt');
    char <- str_replace(char, name, '');
    writeLines(char, paste0(path, name));
  }
}


#Lots of fiddly corner cases that have to be taken care of,
#mostly having to do with characters being called different things
#at various points in the play.
checkCornerCases <- function(play, playtext) {
  hamlet <- 'THE TRAGEDY OF HAMLET, PRINCE OF DENMARK.txt';
  henryV <- 'THE LIFE OF KING HENRY THE FIFTH.txt'
  IIhenryVI <- 'THE SECOND PART OF KING HENRY THE SIXTH.txt'
  measure <- 'MEASURE FOR MEASURE.txt';
  richardIII <- 'KING RICHARD III.txt';
  rnj <- 'THE TRAGEDY OF ROMEO AND JULIET.txt'
  windsor <- 'THE MERRY WIVES OF WINDSOR.txt'
  if (play == hamlet) {
    playtext <- str_replace(playtext, 'For\\.', 'Fort\\.');
  } else if (play == IIhenryVI || play == henryV) {
    playtext <- str_replace(playtext, '^  KING\\.', '  KING HENRY\\.')
  } else if (play == measure) {
    playtext <- str_replace(playtext, '^  MRS\\.', '  MISTRESS')
  } else if (play == richardIII) {
    playtext <- str_replace(playtext, '^  STANLEY\\.', '  DERBY\\.')
  } else if (play == rnj) {
    playtext <- str_replace(playtext, '^  C\\. Wife\\.', '  Lady\\.')
  } else if (play == windsor) {
    playtext <- str_replace(playtext, '^  MRS\\.', '  MRS')
  }
  return(playtext)
}



allDialogueIter <- function(playlist){
  directory <- '~/ShakespeareProject/_rawData/';
  for (i in 1:length(playlist)){
    play <- playlist[i];
    playtext <- read_lines(paste0(directory, play));
    playtext <- checkCornerCases(play, playtext);
    dramPersonae <- getCharacters(playtext);
    charLocations <- getDialogueLoc(dramPersonae, playtext);
    dialogue <- getAllDialogue(charLocations, playtext)
    names(dialogue) <- str_trim(dramPersonae);
    n <- nchar(play); playname <- str_sub(play, 1, n-4);
    newdir <- '~/ShakespeareProject/_cleanData/'
    playdir <- paste0(newdir, paste0(playname, '/'));
    dir.create(playdir);
    writeCharDialogue(dialogue, playdir)
  }
}

