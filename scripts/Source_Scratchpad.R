for (i in 1:length (all_the_runs)) {
  if(length(list.files(file.path(heatmapLand,  all_the_runs[i]))) == 1) {
    print(paste0("Folder ", all_the_runs[i], " is not done"))
  } else {
    # print(paste0("Folder ", all_the_runs[i], " is not done"))
  }
}

sing.selection <- function(universal_parameters, moran, curiosity_level, select_type, sylrep_object, num_select_chances = c(10, 42), sylrep_fill_chances = 10, verbose_output = TRUE, interbreed = FALSE){
  
  for(population in 1 : universal_parameters$num_pop) { #population <- 1 rm(population)
    #print(paste("this is population",population,sep=" "))
    chance_for_selection = 1
    while(chance_for_selection <= num_select_chances[select_type]) {
      stop = FALSE
      if(chance_for_selection == num_select_chances[select_type]) {
        auto.teachers <- matrix(c(sample(universal_parameters$pop_calls_matrix[1, ], sylrep_fill_chances),sample(universal_parameters$pop_calls_matrix[2, ], sylrep_fill_chances)),2,sylrep_fill_chances,T)
        for(MTsylrep_filter in 1:sylrep_fill_chances){
          #c((sample(universal_parameters$pop_calls_matrix[1, ], 1)), (sample(universal_parameters$pop_calls_matrix[2, ], 1)))
          if((
            sum(sylrep_object[auto.teachers[1,MTsylrep_filter], , population]) != 0) && (
            sum(sylrep_object[auto.teachers[2,MTsylrep_filter], , population]) != 0)) {
            if(verbose_output == TRUE) {
              context.name <- c("Tutor", "Mate")
              warning(print(paste0("Automatic Teacher(s) = ", auto.teachers[,MTsylrep_filter], " for Population ", population, " ", context.name[select_type], " Selection")))
            }
            moran = update_selexn_data(universal_parameters, moran, auto.teachers, 1, auto.teachers[2], 
                curiosity_level, population, select_type,
                sylrep_object[auto.teachers[1]:200,,population], sylrep_object[auto.teachers[2],,population], chance_for_selection)

            # should probably fill in some spots in moran$pairing.pool with MTsylrep_filter, provided the value exceeds 1.
            #if(MTsylrep_filter >= 1) {}
            stop = TRUE
            break
          }
        }
        if(stop) {break}
      }
      
      if(select_type == 1) {
        #This statement separates specific mating and tutoring selection qualities:
        # singSuccessFilter will inform the selection of a mate by restricting the successful mate 
        # to those individuals from the same population as the selector. Similarly, 
        # selector.index distinguishes between mating and tutoring, except here it uses
        # a randomly-selected female for the mating context, and the offspring for tutoring.

        singSuccessFilter <- 1 : ((universal_parameters$one_pop_singers[select_type]) * (universal_parameters$num_pop)) # "1-20"
        selector.index <- moran$pairing.pool[3, 1, population]
      } else {
        singSuccessFilter <- (1 + ((population - 1) * (universal_parameters$one_pop_singers[select_type]))) : (population * universal_parameters$one_pop_singers[select_type]) # "1-10," or "11-20"
        selector.index <- sample(universal_parameters$pop_calls_matrix[2, ], 1)
      }
      
      selector.sylrep <- sylrep_object[selector.index, , population]
            
      selection.index <- (
        # This creates sample calls for each population; each population has a sample size of
        # universal_parameters$one_pop_singers, which comes from the male half of the population.
        # probability defined by the fraction of syllable repertoires of each member of 
        # each population divided by the maximum syllrep of the population.
        sapply(
          1:universal_parameters$num_pop, function(x) {
            sample(
              x = universal_parameters$pop_calls_matrix[1,], 
              size = universal_parameters$one_pop_singers[select_type], 
              replace = FALSE, prob = ((
                  apply(sylrep_object[universal_parameters$pop_calls_matrix[1,],,x],1,sum)
                )/max(
                  apply(sylrep_object[universal_parameters$pop_calls_matrix[1,],,x],1,sum)
                )
              )
            )
          }
        ) # probability = the number of times each individual's syllable 
          # repertoire has a 1 in it (sum(sylrep_object[universal_parameters$pop_calls_matrix[1,]])), 
          # divided by the biggest repertoire's total.
      )
      
      # create a matrix of all the sylrep_object of the sample males from selection.index
      selection.sylreps <- t(
        cbind(
          sapply(
            1:universal_parameters$one_pop_singers[select_type], 
            function(x) {sylrep_object[selection.index[x,1],,1]}
          ),
          sapply(
            1:universal_parameters$one_pop_singers[select_type], 
            function(x) {sylrep_object[selection.index[x,2],,2]}
           )
         )
      )
      
      # applies the standard deviation scoring to the males in selection.sylrep_object; 
      # larger score means greater difference between male sylrep and selector's sylrep.
      golf_score <- sort(apply(X = selection.sylreps, MARGIN = 1, FUN = score_similarity, selector_vector = selector.sylrep),index.return = T)$ix
      
      # orders the scored list of suitors; subsets one suitor from the rest,
      # according to the value of the selector's (auditory) curiosity.
      singer <- golf_score[round(curiosity_level[selector.index, population] * (universal_parameters$one_pop_singers[select_type] * universal_parameters$num_pop) + 0.5)]
      if(sum(selection.sylreps[singer,])==0) {
        chance_for_selection = chance_for_selection + 1
        next}
      
      #should_pick_neighbor <- function(index,lower,upper=Inf) {
      
      if(!interbreed) {
        should_continue <- TRUE
        if(singer %in% singSuccessFilter) {
          
          moran = update_selexn_data(universal_parameters, moran, selection.index, singer, selector.index, curiosity_level, 
                             population, select_type, selection.sylreps, selector.sylrep, 
                             chance_for_selection)
          
          should_continue <- FALSE
        }
        
        if(should_continue) {
          for(neighbor in c(1, -1)) {
            if(should_pick_neighbor(neighbor,num_select_chances,select_type,chance_for_selection,golf_score,singSuccessFilter,singer,lower=0.5,upper=0.75)) {
              singer <- golf_score[singer+neighbor]
              
              moran = update_selexn_data(universal_parameters, moran, selection.index, singer, selector.index, curiosity_level, 
                             population, select_type, selection.sylreps, selector.sylrep, 
                             chance_for_selection)
              
              should_continue <- FALSE
              break
            }
          }
        }
          
        if(should_continue) {
          for(neighbor in c(1, -1, 2, -2)) {
            if(should_pick_neighbor(neighbor,num_select_chances,select_type,chance_for_selection,golf_score,singSuccessFilter,singer,lower=0.75)) {
              singer <- golf_score[singer+neighbor]

              moran = update_selexn_data(universal_parameters, moran, selection.index, singer, selector.index, curiosity_level, 
                             population, select_type, selection.sylreps, selector.sylrep, 
                             chance_for_selection)
              
              should_continue <- FALSE
              break
            }
          }
        }
        
        if(!should_continue) {
          break
        }
      } else {
        if(sum(sylrep_object[selection.index[singer], , population]) != 0) {

          moran = update_selexn_data(universal_parameters, moran, selection.index, singer, selector.index, curiosity_level, 
                             population, select_type, selection.sylreps, selector.sylrep, 
                             chance_for_selection)
          
          break
        }
      }
      chance_for_selection = chance_for_selection + 1
    }
  }
  return(moran)
}



  for(population in 1 : 2) {
    print(paste0("population = ", population))
    chance_for_selection = 100
    while(chance_for_selection <= 100) {
      stop = FALSE
      if(chance_for_selection == 100) {
        #auto.teachers <- matrix(c(sample(universal_parameters$pop_calls_matrix[1, ], sylrep_fill_chances),sample(universal_parameters$pop_calls_matrix[2, ], sylrep_fill_chances)),2,sylrep_fill_chances,T)
        for(MTsylrep_filter in 1:10){
          
          if(MTsylrep_filter==2) { # IF parents have syllables in their repertoire; otherwise, cycle back
            print(paste0("MTsylrep_filter = ", MTsylrep_filter))
            print("here it comes")
            stop = TRUE
            break
          } else {print("T _ T")}
        }
        if(stop) {
          print("oh yeah!")
          break}
      }
      print("the break worked! This doesn't get printed!")
    }
    print("This text prints after 'oh yeah!' as long as the break works")
  }

  











source(file.path("scripts", "Source_Reference_Section.R"))
# referenceSection("multirun")
referenceSection("profiler")


shifting_curstart <- 1
paramsFile <- c("params.yaml")
paramsSource = paramsFile
params <- yaml.load_file(file.path("parameters", paramsSource))

scmin = c(
        params$curstarts[[shifting_curstart]]$scmin[1],
        params$curstarts[[shifting_curstart]]$scmin[2],
        params$curstarts[[shifting_curstart]]$scmin[3],
        params$curstarts[[shifting_curstart]]$scmin[4])
      scmax = c(
        params$curstarts[[shifting_curstart]]$scmax[1],
        params$curstarts[[shifting_curstart]]$scmax[2],
        params$curstarts[[shifting_curstart]]$scmax[3],
        params$curstarts[[shifting_curstart]]$scmax[4])
      simNumber = params$simnumberstart + (shifting_curstart - 1)
      runlength = params$runlength
      Syllearnstyle = params$Syllearnstyle
      vertOblLearn = c(
        params$vertoblearn$vertical$learn,
        params$vertoblearn$vertical$invent,
        params$vertoblearn$oblique$learn,
        params$vertoblearn$oblique$invent)
      syldist = params$syldist
      curinh_value = params$curinh_value
      number_populations = params$num_pop
      population_size = params$pop_size
      syllable_number = params$sylnum
      number_sylls_probability_level = params$num_sylls_per_prob_lvl
      standDev = as.numeric(params$standard_deviation)
      SimNumberLC = shifting_curstart
      curinh_style = params$curinh_pattern
      recordingSimpFact = params$Recordsimplifyfactor
      one_pop_singers = params$one_pop_singers

"/home/parker/Documents/projects/Code/curiosity-code/results/190327_296_-_2k_nsL_normVO_oppsyl_1-7_c/variable_store/2019-03-27-144523-GMT-variable-store/"


specificSimNumber = 1

  connection <- file(description = file.path(
    "source","temp", paste0(
      specificSimNumber, "_sim_data.txt")), open = "rt")
  multiRun_folderList <- as.vector(read.table(connection, -1L)[[2]])
  close(connection)

paste(c("T","i","o","s"), c("his ", "s ", "ne ",
"entence."),sep="",collapse="")




thing <- colorRampPalette(c("#7f3b08", "#b35806", "#e08214", "#fdb863", "#fee0b6", "#f7f7f7", "#d8daeb", "#b2abd2", "#8073ac", "#542788", "#2d004b"))

#7f3b08 – BASAL MALEPOOL
#b35806 – SIMILAR MALEPOOL
#e08214 MORAN DETAIL
#fdb863 LH DETAIL BOXES
#fee0b6 – MIDDLE MALEPOOL
#f7f7f7 BACKGROUND
#d8daeb – FAR END MALEPOOL
#b2abd2 MORAN BACKGROUND
#8073ac – FARTHEST MALEPOOL
#542788
#2d004b ARROWS



colorSeqMultPalette <- list(
    BuGn = colorRampPalette(c("#e5f5f9", "#99d8c9", "#2ca25f")), # 3-class BuGn
    BuPu = colorRampPalette(c("#e0ecf4", "#9ebcda", "#8856a7")), # 3-class BuPu
    GnBu = colorRampPalette(c("#e0f3db", "#a8ddb5", "#43a2ca")), # 3-class GnBu
    OrRd = colorRampPalette(c("#fee8c8", "#fdbb84", "#e34a33")), # 3-class OrRd
    PuBu = colorRampPalette(c("#ece7f2", "#a6bddb", "#2b8cbe")), # 3-class PuBu
    PuBuGn = colorRampPalette(c("#ece2f0", "#a6bddb", "#1c9099")), # 3-class PuBuGn
    PuRd = colorRampPalette(c("#e7e1ef", "#c994c7", "#dd1c77")), # 3-class PuRd
    RdPu = colorRampPalette(c("#fde0dd", "#fa9fb5", "#c51b8a")), # 3-class RdPu
    YlGn = colorRampPalette(c("#f7fcb9", "#addd8e", "#31a354")), # 3-class YlGn
    YlGnBu = colorRampPalette(c("#edf8b1", "#7fcdbb", "#2c7fb8")), # 3-class YlGnBu
    YlOrBr = colorRampPalette(c("#fff7bc", "#fec44f", "#d95f0e")), # 3-class YlOrBr
    YlOrRd = colorRampPalette(c("#ffeda0", "#feb24c", "#f03b20")))
stuff <- colorRampPalette(c("#ffeda0", "#feb24c", "#f03b20"))

png(filename = file.path(paste0("colorSpectrum.png")), width = 554, height = 554)
      
plot(matrix(c(rep(1,20),1:20),20,2),col=stuff(20),pch=15,cex=15, xlab = NA, ylab = NA, axes = F)

dev.off()

zero_to_one_template <- c( 0.00,0.01,0.05,0.09, 0.1,0.15,0.18, 0.2,0.25,0.27,
#                            #1,  #2,  #3,  #4,  #5,  #6,  #7,  #8,  #9, #10,
                            0.3,0.35,0.36, 0.4,0.45,0.49, 0.5,0.51,0.54,0.55,
#                           #11, #12, #13, #14, #15, #16, #17, #18, #19, #20,
                            0.59, 0.6,0.63,0.65, 0.7,0.72,0.75, 0.8,0.81,0.85,
#                           #21, #22, #23, #24, #25, #26, #27, #28, #29, #30,
                            0.9,0.95,0.99,1.0)
#                           #31, #32, #33,#34

png(filename = file.path(paste0("colorSpectrum.png"), width = 554, height = 554, units = "px", pointsize = 12, bg = "white"))
      
plot(matrix(c(rep(1,20),1:20),20,2),col=thing(20),pch=15,cex=15, xlab = NA, ylab = NA, axes = F)

dev.off()


thing <- array(c(rep(c(1:3),333), 1), c(10,10,10))
# 2 X 2

image(thing[1:2,1:2,4], axes = F)

axis(1,c(-0.495,  0  ,0.5,    1    ,1.495), c(  ""   ,"1","" ,"2","" ),
  T,0,NA,F,cex.axis=0.8, tck = 0)
axis(1,c(-0.495,0.5,1.495),
  c("","",""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)

axis(2,c(-0.495,  0  ,0.5,    1    ,1.495),
  c(  ""   ,"1","" ,"2","" ),
  T,0,NA,F,cex.axis=0.8, tck = 0)
axis(2,c(-0.495,0.5,1.495),
  c("","",""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)


# 3 X 3

image(thing[1:3,1:3,4], axes = F)

axis(1,c(-0.25, 0, 0.25, 0.5, 0.75, 0.97, 1.25),
  c("", "1", "", "2", "", "3", ""),
  T,0,NA,F,cex.axis=0.8, tck = 0)
axis(1,c(-0.25, 0.25, 0.75, 1.25),
  c("", "", "", ""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)

axis(2,c(-0.25, 0, 0.25, 0.5, 0.75, 0.97, 1.25),
  c("", "1", "", "2", "", "3", ""),
  T,0,NA,F,cex.axis=0.8, tck = 0)
axis(2,c(-0.25, 0.25, 0.75, 1.25),
  c("", "", "", ""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)

# 4 X 4

image(t(thing[c(2,3,5,8),c(1,3,5,8),4]), axes = F)

axis(1,c(-0.165, 0, 0.167, 0.334, 0.5, 0.667, 0.834, 1, 1.1649),
  c("", "1", "", "2", "", "3", "", "4", ""),
  T,0,NA,F,cex.axis=0.8, tck = 0)
axis(1,c(-0.165, 0.168, 0.5, 0.835, 1.1649),
  c("", "", "", "", ""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)

axis(2,c(-0.165, 0, 0.167, 0.334, 0.5, 0.667, 0.834, 1, 1.1649),
  c("", "1", "", "2", "", "3", "", "4", ""),
  T,0,NA,F,cex.axis=0.8, tck = 0)
axis(2,c(-0.165, 0.168, 0.5, 0.835, 1.1649),
  c("", "", "", "", ""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)

# 5 X 5

image(t(thing[c(2,3,5,7,8),c(1,3,5,7,8),4]), axes = F)

axis(1,c(-0.124, 0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.97, 1.124),
  c("", "1", "", "2", "", "3", "", "4", "", "5", ""),
  T,0,NA,F,cex.axis=0.8, tck = 0)
axis(1,c(-0.124, 0.125, 0.375, 0.625, 0.875, 1.124),
  c("", "", "", "", "", ""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)

axis(2,c(-0.124, 0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 0.97, 1.124),
  c("", "1", "", "2", "", "3", "", "4", "", "5", ""),
  T,0,NA,F,cex.axis=0.8, tck = 0)
axis(2,c(-0.124, 0.125, 0.375, 0.625, 0.875, 1.124),
  c("", "", "", "", "", ""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)

# 6 X 6

image(t(thing[c(2,3,4,5,7,8),c(1,3,4,5,7,8),4]), axes = F)

axis(1,c(-0.1, 0, 0.1, 0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1),
  c("", "1", "", "2", "3", "", "4", "", "5", "", "6", ""),
  T,0,NA,F,cex.axis=0.8, tck = 0)
axis(1,c(-0.1, 0.1, 0.3, 0.5, 0.7, 0.9, 1.1),
  c("", "", "", "", "", "", ""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)

axis(2,c(-0.1, 0, 0.1, 0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1),
  c("", "1", "", "2", "3", "", "4", "", "5", "", "6", ""),
  T,0,NA,F,cex.axis=0.8, tck = 0)
axis(2,c(-0.1, 0.1, 0.3, 0.5, 0.7, 0.9, 1.1),
  c("", "", "", "", "", "", ""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)

# 7 X 7

image(t(thing[c(1,2,3,4,5,7,8),c(1,2,3,4,5,7,8),4]), axes = F)

axis(1,c(-0.083,   0, 0.083, 0.167, 0.25, 0.334, 0.416, 0.5, 0.583, 0.667, 0.75, 0.833, 0.916, 1.0, 1.083),
       c(    "", "1",    "",   "2",   "",   "3",    "", "4",    "",   "5",   "",   "6",    "", "7",    ""),
  T,0,NA,F,cex.axis=0.8, tck = 0)
axis(1,c(-0.083, 0.083, 0.25, 0.416, 0.583, 0.75, 0.916, 1.083),
  c("", "", "", "", "", "", "", ""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)

axis(2,c(-0.083,   0, 0.083, 0.167, 0.25, 0.334, 0.416, 0.5, 0.583, 0.667, 0.75, 0.833, 0.916, 1.0, 1.083),
       c(    "", "1",    "",   "2",   "",   "3",    "", "4",    "",   "5",   "",   "6",    "", "7",    ""),
  T,0,NA,F,cex.axis=0.8, tck = 0)
axis(2,c(-0.083, 0.083, 0.25, 0.416, 0.583, 0.75, 0.916, 1.083),
  c("", "", "", "", "", "", "", ""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)

# 8 X 8

image(t(thing[c(1,2,3,4,5,6,7,8),c(1,2,3,4,5,6,7,8),4]), axes = F)

axis(1,c(-0.071,   0, 0.071, 0.145, 0.216, 0.287, 0.358, 0.429, 0.5, 0.571, 0.645, 0.716, 0.787, 0.858, 0.929, 1.0, 1.071),
       c(    "", "1",    "",   "2",    "",   "3",    "",   "4",  "",   "5",    "",   "6",    "",   "7",    "", "8",   ""),
  T,0,NA,F,cex.axis=0.8, tck = 0)
axis(1,c(-0.0714, 0.071, 0.216, 0.358, 0.5, 0.645, 0.787, 0.929, 1.071),
  c("", "", "", "", "", "", "", "", ""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)

axis(2,c(-0.071,   0, 0.071, 0.142, 0.213, 0.284, 0.356, 0.427, 0.5, 0.571, 0.642, 0.713, 0.784, 0.855, 0.93, 1.0, 1.071),
       c(    "", "1",    "",   "2",    "",   "3",    "",   "4",  "",   "5",    "",   "6",    "",   "7",   "", "8",    ""),
  T,0,NA,F,cex.axis=0.8, tck = 0)
axis(2,c(-0.071, 0.071, 0.213, 0.356, 0.498, 0.64, 0.782, 0.93, 1.071),
  c("", "", "", "", "", "", "", "", ""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)

# 9 X 9

image(t(thing[c(1,2,3,4,5,6,7,8,9),c(1,2,3,4,5,6,7,8,9),4]), axes = F)

axis(1,c(-0.0625,   0, 0.0625, 0.125, 0.1875, 0.25, 0.3125, 0.375, 0.4375, 0.5, 0.5625, 0.625, 0.6875, 0.75, 0.8125, 0.875, 0.9375, 1.0, 1.0625),
       c(     "", "1",     "",   "2",     "",  "3",     "",   "4",     "", "5",     "",   "6",     "",  "7",     "",   "8",     "", "9",     ""),
  T,0,NA,F,cex.axis=0.8, tck = 0)
axis(1,c(-0.0625, 0.0625, 0.1875, 0.3125, 0.4375, 0.5625, 0.6875, 0.8125, 0.9375, 1.0625),
  c("", "", "", "", "", "", "", "", "", ""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)

axis(2,c(-0.0625,   0, 0.0625, 0.125, 0.1875, 0.25, 0.3125, 0.375, 0.4375, 0.5, 0.5625, 0.625, 0.6875, 0.75, 0.8125, 0.875, 0.9375, 1.0, 1.0625),
       c(     "", "1",     "",   "2",     "",  "3",     "",   "4",     "", "5",     "",   "6",     "",  "7",     "",   "8",     "", "9",     ""),
  T,0,NA,F,cex.axis=0.8, tck = 0)
axis(2,c(-0.0625, 0.0625, 0.1875, 0.3125, 0.4375, 0.5625, 0.6875, 0.8125, 0.9375, 1.0625),
  c("", "", "", "", "", "", "", "", "", ""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)

# 10 X 10

image(t(thing[c(1,2,3,4,5,6,7,8,9,10),c(1,2,3,4,5,6,7,8,9,10),4]), axes = F)

axis(1,c(-0.0555,   0, 0.0555, 0.111, 0.1665, 0.222, 0.2775, 0.333, 0.3885, 0.444, 0.4995, 0.555, 0.611, 0.6665, 0.722, 0.7775, 0.833, 0.8885, 0.944, 0.9995, 1.055),
       c(     "", "1",     "",   "2",     "",   "3",     "",   "4",     "",   "5",  "",    "6",    "",    "7",    "",    "8",    "",    "9",    "",   "10",    ""),
  T,0,NA,F,cex.axis=0.6, tck = 0)
axis(1,c(-0.0555, 0.0555, 0.1665, 0.2775, 0.3885, 0.5, 0.611, 0.722, 0.833, 0.944, 1.055),
  c("", "", "", "", "", "", "", "", "", "", ""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)

axis(2,c(-0.0555,   0, 0.0555, 0.111, 0.1665, 0.222, 0.2775, 0.333, 0.3885, 0.444, 0.4995, 0.555, 0.611, 0.6665, 0.722, 0.7775, 0.833, 0.8885, 0.944, 0.9995, 1.055),
       c(     "", "1",     "",   "2",     "",   "3",     "",   "4",     "",   "5",  "",    "6",    "",    "7",    "",    "8",    "",    "9",    "",   "10",    ""),
  T,0,NA,F,cex.axis=0.6, tck = 0)
axis(2,c(-0.0555, 0.0555, 0.1665, 0.2775, 0.3885, 0.5, 0.611, 0.722, 0.833, 0.944, 1.055),
  c("", "", "", "", "", "", "", "", "", "", ""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)



















axis(1,c(-0.25, 0, 0.25, 0.5, 0.75, 0.97, 1.25),
  c("", "0-.25", "", ".25-.5", "", ".45-1", ""),
  T,0,NA,F,cex.axis=1, tck = 0)
axis(1,c(-0.25,0.25,0.75,1.25),
  c("","","",""),
  T,-0.03,NA,F,cex.axis=1, tck = -0.03)




axis(1,c(-0.25 ,0      ,0.25  ,0.5      ,0.75  ,0.97    ,1.25),
            c(""    ,"0-.25",""    , ".25-.5",""    , ".45-1",""  ),
            T,0,NA,F,cex.axis=1, tck = 0)
        axis(1,c(-0.25,0.25,0.75,1.25),
            c("","","",""),
            T,-0.03,NA,F,cex.axis=1, tck = -0.03)

          axis(2,c(-0.25 ,0      ,0.25  ,0.5      ,0.75  ,0.97    ,1.25),
            c(""    ,"0-.25",""    , ".25-.5",""    , ".45-1",""  ),
            T,0,NA,F,cex.axis=0.6, tck = 0)
          axis(2,c(-0.25,0.25,0.75,1.25),
            c("","","",""),
            T,-0.03,NA,F,cex.axis=1, tck = -0.03)

      scmin = c(
        params$curstarts[[shifting_curstart]]$scmin[1],
        params$curstarts[[shifting_curstart]]$scmin[2],
        params$curstarts[[shifting_curstart]]$scmin[3],
        params$curstarts[[shifting_curstart]]$scmin[4])
      scmax = c(
        params$curstarts[[shifting_curstart]]$scmax[1],
        params$curstarts[[shifting_curstart]]$scmax[2],
        params$curstarts[[shifting_curstart]]$scmax[3],
        params$curstarts[[shifting_curstart]]$scmax[4])
      simNumber = subsetOrSequence
      # simNumber = params$simnumberstart + (shifting_curstart - 1),
      runlength = params$runlength
      Syllearnstyle = params$Syllearnstyle
      vertOblLearn = c(
        params$vertoblearn$vertical$learn,
        params$vertoblearn$vertical$invent,
        params$vertoblearn$oblique$learn,
        params$vertoblearn$oblique$invent)
      syldist = params$syldist
      curinh_value = params$curinh_value
      number_populations = params$num_pop
      population_size = params$pop_size
      syllable_number = params$sylnum
      number_sylls_probability_level = params$num_sylls_per_prob_lvl
      standDev = as.numeric(params$standard_deviation)
      SimNumberLC = shifting_curstart
      curinh_style = params$curinh_pattern
      recordingSimpFact = params$Recordsimplifyfactor
      one_pop_singers = params$one_pop_singers
      curinhProportion = singleOrMixture # only used if curinh_pattern = 5
      directoryDate = dirDate
      invasion = params$traitinvasion
      invPopSize = params$invasionpopsize
      invStyle = params$invasionstyle



      parameters = simParams
      tempMoran = moranObjects
      curiosity_level = curiosity_level
      select_type = 2
      sylrep_object = sylreps
      num_select_chances = c(40, 40)
      verbose_output = F
      interbreed = F