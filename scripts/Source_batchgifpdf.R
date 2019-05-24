library(magick)

source("scripts/Source_AssignMultVar_BinaryMode.R")
source("scripts/Source_Magick_Functions.R")

setwd("/home/parker/Downloads/")
whatever <- getwd()
thing <- list.files(pattern = "speciesSpecificityAndMimicryInBirdSongAreTheyParadoxesAReevaluationOfSongMimicryInTheEuropeanStarling")
thing <- c(thing[1],thing[12],thing[23:29],thing[2:11],thing[13:22])

g(stuff1, stuff2, stuff3, stuff4, stuff5, stuff6, stuff7, stuff8, stuff9, stuff10, 
  stuff11, stuff12, stuff13, stuff14, stuff15, stuff16, stuff17, stuff18, stuff19, 
  stuff20, stuff21, stuff22, stuff23, stuff24, stuff25, stuff26, stuff27, stuff28, 
  stuff29) %=% c(
    image_read(path = file.path(whatever, thing[1])),
    image_read(path = file.path(whatever, thing[2])),
    image_read(path = file.path(whatever, thing[3])),
    image_read(path = file.path(whatever, thing[4])),
    image_read(path = file.path(whatever, thing[5])),
    image_read(path = file.path(whatever, thing[6])),
    image_read(path = file.path(whatever, thing[7])),
    image_read(path = file.path(whatever, thing[8])),
    image_read(path = file.path(whatever, thing[9])),
    image_read(path = file.path(whatever, thing[10])),
    image_read(path = file.path(whatever, thing[11])),
    image_read(path = file.path(whatever, thing[12])),
    image_read(path = file.path(whatever, thing[13])),
    image_read(path = file.path(whatever, thing[14])),
    image_read(path = file.path(whatever, thing[15])),
    image_read(path = file.path(whatever, thing[16])),
    image_read(path = file.path(whatever, thing[17])),
    image_read(path = file.path(whatever, thing[18])),
    image_read(path = file.path(whatever, thing[19])),
    image_read(path = file.path(whatever, thing[20])),
    image_read(path = file.path(whatever, thing[21])),
    image_read(path = file.path(whatever, thing[22])),
    image_read(path = file.path(whatever, thing[23])),
    image_read(path = file.path(whatever, thing[24])),
    image_read(path = file.path(whatever, thing[25])),
    image_read(path = file.path(whatever, thing[26])),
    image_read(path = file.path(whatever, thing[27])),
    image_read(path = file.path(whatever, thing[28])),
    image_read(path = file.path(whatever, thing[29]))
  )


for(i in 1:29) {
    
}