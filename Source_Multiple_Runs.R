#
#print("it's starting!")
multi_runs <- function(shifting_curstart) {
  library(yaml)
  #for(shifting_curstart in 1:250) 
  params <- yaml.load_file("params.yaml")
  source("Source_Life_Cycle.R")
  number_of_runs <- 50
  cat(number_of_runs, file = "number_of_runs.txt", append = F)
  
  
  
  if(file.exists("console_copy.txt")) {file.remove("console_copy.txt")}
  if(file.exists("sim_data.txt")) {file.remove("sim_data.txt")}
  for(run_number in 1:number_of_runs) {
    saveRDS(object = run_number, file = "holdover_line.RData")
    if(run_number == 1) {
      sink(file = "sim_data.txt", append = TRUE)
      #print(P)
      print("/please/ignore/this/line/like/you/always/do")
      sink()
    }
    life_cycle(scMin = c(
      params[[1]]$curstarts[[shifting_curstart]]$scMin[1],
      params[[1]]$curstarts[[shifting_curstart]]$scMin[2],
      params[[1]]$curstarts[[shifting_curstart]]$scMin[3],
      params[[1]]$curstarts[[shifting_curstart]]$scMin[4]),
      scMax = c(
        params[[1]]$curstarts[[shifting_curstart]]$scMax[1],
        params[[1]]$curstarts[[shifting_curstart]]$scMax[2],
        params[[1]]$curstarts[[shifting_curstart]]$scMax[3],
        params[[1]]$curstarts[[shifting_curstart]]$scMax[4]),
      simStartDate = params[[2]]$simStartDate,
      simNumber = params[[3]]$simNumber,
      runLength = params[[4]]$runLength,
      SylLearnStyle = params[[5]]$SylLearnStyle,
      vertOblLearn = c(params[[6]]$vertObLearn[[1]]$vertical[[1]]$learn,
                       params[[6]]$vertObLearn[[1]]$vertical[[2]]$invent,
                       params[[6]]$vertObLearn[[2]]$oblique[[1]]$learn,
                       params[[6]]$vertObLearn[[2]]$oblique[[2]]$invent),
      sylDist = params[[7]]$sylDist, 
      curinh_value = params[[8]]$curinh_value,
      number_populations = params[[9]]$num_pop,
      population_size = params[[10]]$pop_size,
      syllable_number = params[[11]]$sylnum,
      number_of_syllables_per_probability_level = params[[12]]$num_sylls_per_prob_lvl
    )
    #rm(list=objects())
    run_number <- readRDS(file = "holdover_line.RData")
    #number_of_runs <- 10
    print(paste0("Run Number: ", run_number, ", comes right before (YYYY-MM-DD-HHMMSS): ", format(Sys.time(), "%F-%H%M%S")))
  }
  
  file.copy(from = "console_copy.txt", to = paste0("../Results/",format(Sys.time(), "%F-%H%M%S"), "_console_copy.txt"))
  
  file.copy(from = "sim_data.txt", to = paste0("../Results/",format(Sys.time(), "%F-%H%M%S"), "_sim_data.txt"))
  
  source("Source_Figure_Produxn_Multiple_Runs.R")
}

#rm(list=objects())
# Transform name of data packet to folder name (example folder name: 2018-10-09-010315-GMT-variable-store)
# run loop that processes fn_doc_line from start of run to the end; 
# stitches together the pieces of individual runs, and plots them
# with an average line in black and the rest in various shades of grey.
###2018-10-09-010315-GMT-variable-store
#"2018-10-09" <- 6
#"01:55:51" <- 7
#"-GMT-variable-store"
