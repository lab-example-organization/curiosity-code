# Amalgamation of functions; current nexus of code.
  # 

# The following text is used to select the curiosity values for initialize.curiosity below:
#zero_to_one_template <- c(0.00,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,
                             #1,  #2,  #3, #4,  #5, #6,  #7, #8,  #9,#10,

                          #0.45,0.49,0.5,0.51,0.55,0.59,0.6,0.65,0.7,0.75,
                            #11, #12,#13, #14, #15, #16,#17, #18,#19, #20,

                           #0.8,0.85,0.9,0.95,0.99,1.0)
                            #21, #22,#23, #24, #25,#26

#setwd("/Users/bryangitschlag/Box Sync/Parker_Nicole/Curiosity Code/July")
setwd("/home/labuser/Documents/Parker\ Scratch\ Folder/Code/")
num_pop = 2
initsylrepmatrix <- array(data = c(7,14,3,4,5,6,7,8,9,10,11,12,13,14,11,13), c(num_pop, 8))
curminvector <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
curmaxvector <- c(2,2,2,2,2,2,2,2,10,10,10,10,10,10,10,10)
#"_low_diff_cur"
docnames <- c(rep("loww_diff_cur", times = 8))

for(bitchin in 1:8) {
  setwd("/home/labuser/Documents/Parker scratch")
  getwd()
  
  #rm(list=objects())
  
  source("180807.1804_Source_Initial_Functions_Parameters.R")
  # num_timesteps, nropsp, num_pop, pop_size, sylnum, nsspl, num_one.pop_singers_sampled, curlearnprob, learnprob, randlearnprob, stand.dev, curflux, new.cur.threshold, offspring_calls, 
  P <- Define.Parameters(num_timesteps = 1000, nropsp = 1, num_pop = 2, pop_size = 400, sylnum = 156, nsspl = 24, num_one.pop_singers_sampled = 10, curlearnprob = 0.95, learnprob = c(0.59, 0.1), randlearnprob = c(0.1, 0.01), stand.dev = 2, curflux = 0.5, new.cur.threshold = 10, offspring_calls = )
  
  sylreps <- initialize.sylrep(P, c(initsylrepmatrix[,bitchin]), T, T)
  
  P$curiosity_level <- initialize.curiosity(P, 
                                            #popXmale,popXfemale,popYmale,popYfemale...
                                            c(curminvector[bitchin],curminvector[bitchin],curminvector[8 + bitchin],curminvector[8 + bitchin]), 
                                            c(curmaxvector[bitchin],curmaxvector[bitchin],curmaxvector[8 + bitchin],curmaxvector[8 + bitchin]))
  
  day.tuh <- recordvariable.initialize(P, timestep_fraction = (P$num_timesteps/1000))
  
  source("180808.1342_Source_Functions_Parameters.R")
  
  for(thousand_timesteps in 1:(P$num_timesteps/1000)) {
    for(single_timestep in 1:1000) {
      
      P$pairing.pool <- sing.selection(P, context = 1, num_select_chances = 100)
      
      offspring_calls <- make.offspring.calls(P, no.parent.turnover = FALSE)
      
      new.curiosity <- curiosity_learn(P, curlearnprob = 0.95)
      
      sylreps <- syll_learn(P = P, context = 1, offspring_calls = offspring_calls, learnprob = c(0.95, 0.1), which.motivation = 1, randlearnprob = c(0.1, 0.01), stand.dev = 2)
      sylreps <- syll_learn(P = P, context = 1, offspring_calls = offspring_calls, learnprob = c(0.95, 0.1), which.motivation = 2, randlearnprob = c(0.1, 0.01), stand.dev = 2)
      
      P$pairing.pool <- sing.selection(P = P, context = 2)
      
      sylreps <- syll_learn(P = P, context = 2, offspring_calls = offspring_calls, learnprob = c(0.95, 0.1), which.motivation = 1, randlearnprob = c(0.1, 0.01), stand.dev = 2)
      
      day.tuh <- variable.archive(P = P, timestep = single_timestep)
      
    }
    #thousand_timesteps <- 1
    print(paste("storing data packet ", thousand_timesteps, sep=""))
    FolderName <- store_timesteps(filename = thousand_timesteps, object_record = day.tuh)
    if((thousand_timesteps==(P$num_timesteps/1000))&&(single_timestep==1000)) {
      sink(file = "pwd.txt", append = T)
      getwd()
      print(P)
      sink()
      stop("Shit's Done, Yo!")
    }
  }
  
  #setwd("/home/labuser/Documents/Parker scratch")
  source("180803.0055_Source_Visualizing_Data.R")
  
  converted_data <- convert_stored_data(P = P, num_timechunks = thousand_timesteps)
  R <- create_plot_info("180803", paste0("00", bitchin, docnames[bitchin]))
  simple_plots(R = R, Q = converted_data, simplification_factor = 10)
  full_plots(R = R, Q = converted_data)
}



setwd("/home/labuser/Documents/Parker scratch")
getwd()

rm(list=objects())
init_params <- paste0("source(\"", "180809", ".", "0031", "_Source_Initial_Functions_Parameters.R\")")
eval(parse(text = init_params))

P <- Define.Parameters(num_timesteps = 1000, nropsp = 1, num_pop = 2, 
                       pop_size = 400, sylnum = 156, nsspl = 24, 
                       num_one.pop_singers_sampled = c(10,10), 
                       curlearnprob = 0.95, learnprob = c(0.1, 0.95), randlearnprob = c(0.01, 0.1), 
                       stand.dev = 2, curflux = 0.5, new.cur.threshold = 10)

sylreps <- initialize.sylrep(P, c(1, 2), T, T)

P$curiosity_level <- initialize.curiosity(P, 
                                          #popXmale,popXfemale,popYmale,popYfemale...
                                        c(1,1,1,1), 
                                        c(26,26,26,26))

day.tuh <- recordvariable.initialize(P, timestep_fraction = (P$num_timesteps/1000))

funx_n_params <- paste0("source(\"", "180809", ".", "0042", "_Source_Functions_Parameters.R\")")
eval(parse(text = funx_n_params))

rm(init_params, funx_n_params)

for(thousand_timesteps in 1:(P$num_timesteps/1000)) {
  for(single_timestep in 1:1000) {
    
    P <- sing.selection(P = P, context = 2, num_select_chances = c(10, 100), verbose_output = TRUE)
    
    P <- make.offspring.calls(P, no.parent.turnover = FALSE)
    
    day.tuh <- variable.archive(P = P, timestep = single_timestep, context = 2)
    
    # curinh.row - calling either the row number or name of row for different curiosity inheritance patterns - 
      # 1: father; 2: mother; 3: same; 4:opposite
    P <- curiosity_learn(P, curlearnprob = 0.95, timestep = single_timestep, curinh.row = 1) 
    
    sylreps <- syll_learn(P = P, context = 2)
    
    P <- sing.selection(P = P, context = 1, num_select_chances = c(10, 100), verbose_output = TRUE)
    
    sylreps <- syll_learn(P = P, context = 1)
    
    day.tuh <- variable.archive(P = P, timestep = single_timestep, context = 1)
    
  }
  #thousand_timesteps <- 1
  print(paste("storing data packet ", thousand_timesteps, sep=""))
  FolderName <- store_timesteps(filename = thousand_timesteps, object_record = day.tuh)
  if((thousand_timesteps==(P$num_timesteps/1000))&&(single_timestep==1000)) {
    file_sink = paste0("180809", "_", thousand_timesteps, ".txt")
    sink(file = file_sink)
    print(P)
    print(FolderName)
    sink()
    stop("It's Done, Yo!")
    }
}

#setwd("/home/labuser/Documents/Parker scratch")
data_visuals <- paste0("source(\"", "180807", ".", "1350", "_Source_Visualizing_Data.R\")")
eval(parse(text = data_visuals))

converted_data <- convert_stored_data(P = P, num_timechunks = thousand_timesteps)
R <- create_plot_info("180808", "001_back_to_basics")
simple_plots(R = R, Q = converted_data, simplification_factor = 10)
full_plots(R = R, Q = converted_data)


