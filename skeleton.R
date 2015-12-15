dir.create('~/ShakespeareProject')
dir.create('~/ShakespeareProject/_code')
dir.create(('~/ShakespeareProject/_scripts'))
dir.create('~/ShakespeareProject/_rawData')
dir.create('~/ShakespeareProject/_cleanData')
shakesURL <- 'http://ocw.mit.edu/ans7870/6/6.006/s08/lecturenotes/files/t8.shakespeare.txt'
directory <- '~/ShakespeareProject/_rawData'; fileName <- '/Shakespeare.txt';
download.file(shakesURL, paste0(directory, fileName));


#First step of cleaning the raw data -- get the individual plays
source('~/ShakespeareProject/_scripts/_setUp.R')


#Then get the dialogue
source('~/ShakespeareProject/_scripts/_getDialogue.R')


dir.create('~/ShakespeareProject/_cleanData/MaleCharacters')
dir.create('~/ShakespeareProject/_cleanData/FemaleCharacters')
source('~/ShakespeareProject/_scripts/_sortGender.R')

dir.create('~/ShakespeareProject/_plots')
#This is the bulk of the analysis and includes the bootstrap
#methods. I did my best to optimize them, but they still take a LONG
#time to run. Uncomment this command if you'd like to run it.
#source("~/ShakespeareProject/_scripts/_fittingModel.R")






