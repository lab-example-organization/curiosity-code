#setwd("That one directory that has all the curiosity code in it... )
#setwd("/Users/bryangitschlag/Downloads/Lab_Notebook/GitHub/curiosity-code")
setwd("/home/labuser/Documents/Parker Scratch Folder/Code/Curiosity Code")
#setwd(getwd())

number_of_runs <- 10
for(run_number in 1:number_of_runs) {
  saveRDS(object = run_number, file = "holdover_line.RData")
  source("Source_Life_Cycle.R")
  #rm(list=objects())
  run_number <- readRDS(file = "holdover_line.RData")
  number_of_runs <- 10
  print(paste0("Run Number: ", run_number, ", comes right before (YYYY-MM-DD-HHMMSS): ", format(Sys.time(), "%F-%H%M%S")))
}

conv_outputToFolderName <- function(normal_output = TRUE, single = TRUE, number_of_runs) { # takes strings of the form "storing data packet 100 at 2018-10-09 01:55:51" from console output, and outputs a folder name "2018-10-09-010315-GMT-variable-store"
  #scan(filename_document,what=list(NULL),sep='\n',blank.lines.skip = F)
  con <- file(description = "console_copy.txt", open = "rt")
  fn_doc_lines <- read.table(con, -1L)
  close(con)
  fn_doc_line <- as.vector(fn_doc_lines[[2]])
  
  fn_doc_last_line <- fn_doc_line[length(fn_doc_line)] # [1] "storing data packet 100 at 2018-10-09 01:55:51"
  
  if(normal_output == TRUE) {
    last_line_pieces <- strsplit(fn_doc_line[length(fn_doc_line)], " ")[[1]]
    length_of_single_run <- as.integer(last_line_pieces[4]) # [1] 100
    #first_line_last_run <- fn_doc_line[nrow(fn_doc_lines)-(length_of_single_run-1)] # [1] "storing data packet 1 at 2018-10-09 01:03:15"
    
    
    if(single == TRUE) {
      first_line_pieces <- strsplit(fn_doc_line[length(fn_doc_line)-(length_of_single_run-1)], " ")[[1]]
      string_time <- formatC(sapply(1:3, function(x) {as.integer(strsplit(first_line_pieces[7], ":")[[1]][x])}),width=2,format="d",flag="0")
      string_time <- paste0(string_time[1], string_time[2], string_time[3])
      output <- paste0(first_line_pieces[6], "-",string_time, "-GMT-variable-store")
    } else {
      runNames <- vector(mode = "character", length = number_of_runs)
      for(runds in 1:number_of_runs) {
        runNames[runds] <- fn_doc_line[nrow(fn_doc_lines)-(length_of_single_run-1)-((number_of_runs-runds)*length_of_single_run)]
        first_line_pieces <- strsplit(runNames[runds][length(runNames[runds])-(length_of_single_run-1)], " ")[[1]]
        string_time <- formatC(sapply(1:3, function(x) {as.integer(strsplit(first_line_pieces[7], ":")[[1]][x])}),width=2,format="d",flag="0")
        string_time <- paste0(string_time[1], string_time[2], string_time[3])
        runNames[runds] <- paste0(first_line_pieces[6], "-",string_time, "-GMT-variable-store")
      }
      output <- runNames
      #output <- paste0()
    }
  }
  return(output)
}

multiRun_folderList <- conv_outputToFolderName(normal_output = T, single = F, number_of_runs = number_of_runs)

source("Source_Figure_Produxn_Multiple_Runs.R")

#rm(list=objects())



# Transform name of data packet to folder name (example folder name: 2018-10-09-010315-GMT-variable-store)

# run loop that processes fn_doc_line from start of run to the end; 
# stitches together the pieces of individual runs, and plots them
# with an average line in black and the rest in various shades of grey.



###2018-10-09-010315-GMT-variable-store

#"2018-10-09" <- 6
#"01:55:51" <- 7
#"-GMT-variable-store"

