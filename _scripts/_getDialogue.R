#Get individual characters' dialogue, write them to individual text files
library(stringr)
library(readr)
library(dplyr)
library(tm)

#All the functions we'll need
source('~/ShakespeareProject/_code/_getDialogueFunctions.R')

#The Comedy of Errors wasn't playing nice with the other function I was writing
playdir <- '~/ShakespeareProject/_rawData/';
play <- 'THE COMEDY OF ERRORS.txt'

#Loads The Comedy of Errors into the Global Environment as a character vector
#Each new line is an element of the vector
comedy <- unlist(na.omit(read_lines(paste0(playdir, play))));
comedy <- str_replace(comedy, 'BALTHAZAR', 'BALTHASAR');
comedy <- str_replace(comedy, 'OFEPHESUS', 'OF EPHESUS');
comedy <- str_replace(comedy, 'COURTEZAN', 'COURTESAN');




comedyDramPersonae <- c('ABBESS', 'ADRIANA', 'ANGELO', 'ANTIPHOLUS OF EPHESUS', 
                        'BALTHASAR', 'COURTESAN', 'DROMIO OF EPHESUS',
                        'DROMIO OF SYRACUSE', 'DUKE', 'AEGEON', 'FIRST MERCHANT',
                        'GAOLER', 'LUCE',  'LUCIANA', 'MESSENGER', 'OFFICER', 'PINCH', 
                        'SECOND MERCHANT', 'ANTIPHOLUS OF SYRACUSE')



#All of the locations of the start of dialogue for each character
charLocations <- getDialogueLoc(comedyDramPersonae[1:18], comedy);
charLocations[[19]] <- grep('^ {0,2}ANTIPHOLUS OF SYRACUSE\\.', comedy);
names(charLocations) <- removePunctuation(comedyDramPersonae);



dialogue <- getAllDialogue(charLocations, comedy);
dir.create('~/ShakespeareProject/_cleanData/THE COMEDY OF ERRORS/');

for (i in 1:length(dialogue)) {
  title <- paste0(names(dialogue)[i], '.txt');
  text <- dialogue[[i]];
  filepath <- '~/ShakespeareProject/_cleanData/THE COMEDY OF ERRORS/'
  writeLines(text, paste0(filepath, title));
}

playlist <- list.files(playdir, '.txt$'); check1 = playlist == 'Shakespeare.txt';
check2 = playlist == 'THE SONNETS.txt'; check3 = playlist == 'THE COMEDY OF ERRORS.txt';
playlist <- playlist[-which(check1 | check2 | check3)];


allDialogueIter(playlist)


