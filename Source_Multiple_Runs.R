setwd("/home/labuser/Documents/Parker Scratch Folder/Code/Curiosity Code")

number_of_runs <- 20
for(run_number in 1:number_of_runs) {
  source("Source_Life_Cycle.R")
  print()
  print(paste0("Run Number: ", run_number, ", comes right before (YYYY-MM-DD-HHMMSS): ", format(Sys.time(), "%F-%H%M%S")))
}


filename_document <- file(description = "~/Documents/Parker Scratch Folder/Code/Curiosity Code/console_copy.txt", open = "rt")
#scan(filename_document,what=list(NULL),sep='\n',blank.lines.skip = F)
fn_doc_lines <- read.table(filename_document, -1L)
fn_doc_line <- as.vector(fn_doc_lines[[2]])

first_line_last_run <- fn_doc_line[nrow(fn_doc_lines)-(length_of_single_run-1)]
fn_doc_last_line <- fn_doc_line[length(fn_doc_line)]
length_of_single_run <- as.integer(strsplit(fn_doc_last_line, " ")[[1]][4])





###2018-10-09-010315-GMT-variable-store

#"2018-10-09" <- 6
#"01:55:51" <- 7
#"-GMT-variable-store"

