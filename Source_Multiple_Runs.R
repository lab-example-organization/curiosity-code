setwd("/home/labuser/Documents/Parker Scratch Folder/Code/Curiosity Code")

number_of_runs <- 20
for(run_number in 1:number_of_runs) {
  source("Source_Life_Cycle.R")
  print()
  print(paste0("Run Number: ", run_number, ", comes right before (YYYY-MM-DD-HHMMSS): ", format(Sys.time(), "%F-%H%M%S")))
}

rm(list=objects())
#scan(filename_document,what=list(NULL),sep='\n',blank.lines.skip = F)
con <- file(description = "~/Documents/Parker Scratch Folder/Code/Curiosity Code/console_copy.txt", open = "rt")
fn_doc_lines <- read.table(con, -1L)
close(con)
fn_doc_line <- as.vector(fn_doc_lines[[2]])

fn_doc_last_line <- fn_doc_line[length(fn_doc_line)] # [1] "storing data packet 100 at 2018-10-09 01:55:51"
fn_doc_pieces <- strsplit(fn_doc_last_line, " ")[[1]]
length_of_single_run <- as.integer(fn_doc_pieces[4]) # [1] 100

first_line_last_run <- fn_doc_line[nrow(fn_doc_lines)-(length_of_single_run-1)] # [1] "storing data packet 1 at 2018-10-09 01:03:15"

fn_doc_time <- paste0(as.integer(strsplit(fn_doc_pieces[7], ":")[[1]][1]),as.integer(strsplit(fn_doc_pieces[7], ":")[[1]][2]),as.integer(strsplit(fn_doc_pieces[7], ":")[[1]][3]))
paste0(fn_doc_pieces[6], "-",fn_doc_time, "-GMT-variable-store")
# Transform name of data packet to folder name (example folder name: 2018-10-09-010315-GMT-variable-store)

# run loop that processes fn_doc_line from start of run to the end; 
# stitches together the pieces of individual runs, and plots them
# with an average line in black and the rest in various shades of grey.



###2018-10-09-010315-GMT-variable-store

#"2018-10-09" <- 6
#"01:55:51" <- 7
#"-GMT-variable-store"

