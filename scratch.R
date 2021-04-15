[[3]]
[1] "Error in if (should_pick_neighbor (neighbor, num_select_chances, select_type,  : \n  missing value where TRUE/FALSE needed\n"
attr (,"class")
[1] "try-error"
attr (,"condition")
<simpleError in 
    if (should_pick_neighbor (neighbor, num_select_chances, select_type,     
                             chance_for_selection, golf_score, singSuccessFilter, 
                             singer,     lower = 0.5, upper = 0.75) == TRUE) {    
                                            
                               singer <- golf_score[singer + neighbor]    
                               tempMoran = update_selexn_data (parameters, tempMoran, selection.index,         
                                 singer, selector.index, curiosity_level, 
                                 population, select_type, selection.sylreps, 
                                 selector.sylrep, chance_for_selection, FALSE)    
                               should_continue <- FALSE    
                               break
                             }: missing value where TRUE/FALSE needed>























for (firstFor in 1 : 2) {
    firstWhile = 1
    while(firstWhile <= 7) {
        stop = FALSE
        print (paste0 ("firstWhile = ", firstWhile))
        if (firstWhile == 7) {
            print ("okay... this is the last of the firstWhile")
            for (whatthehell in 1 : 10) {
                if (whatthehell == 5) {
                    print ("whatthehell = 5")
                    stop = TRUE
                    break
                }
                print (paste0 ("whatthehell = ", whatthehell))
            }
            if (stop) {print ("fugg yeh!")
                      break}
        }
        if (firstWhile == 6) {
            firstWhile = firstWhile + 1
            print ("what's next?")
            next
        }
        if (firstWhile < 100) {
            checkConditionsBool = TRUE
            if (firstFor %in% c (2)) {
                checkConditionsBool = FALSE
            }
        }
        if (checkConditionsBool) {
            print ("THIS IS NOT THE END")
        } else {
            if (firstWhile == 7) {
                print ("wHAT IN THE GODDAMN FUCK ARE YOU DOING HERE?!")
            }
            print ("The End (?)")
            break
        }
        firstWhile <- firstWhile + 1
        print (paste0 ("ForAWhile = ", firstFor))
    }
#     print (paste0 ("firstFor = ", firstFor))
}


# [1] "firstWhile = 1"
# [1] "I just wet myself"
# [1] "firstFor = 1"
# [1] "firstWhile = 2"
# [1] "I just wet myself"
# [1] "firstFor = 1"
# [1] "firstWhile = 3"
# [1] "I just wet myself"
# [1] "firstFor = 1"
# [1] "firstWhile = 4"
# [1] "I just wet myself"
# [1] "firstFor = 1"
# [1] "firstWhile = 5"
# [1] "I just wet myself"
# [1] "firstFor = 1"
# [1] "firstWhile = 6"
# [1] "what's next?"
# [1] "firstWhile = 7"
# [1] "okay... this is the last of the firstWhile"
# [1] "whatthehell = 1"
# [1] "whatthehell = 2"
# [1] "whatthehell = 3"
# [1] "whatthehell = 4"
# [1] "whatthehell == 5"
# [1] "fugg yeh!"
# [1] "fristFor = 1"
# [1] "firstWhile = 1"
# [1] "The End (?)"
# [1] "fristFor = 2"




    if (firstFor == 1) {
        for (whatever in 1 : 2) {
            if (whatever == 2) {
                print ("fuck it!")
                break # this break killed the whatever loop, but preserved everyfirstFor at or above the level of the loop
            } else {
                print ("say checkConditionsBool")
            }
            print ("what")
        }
        print ("sum shit")
        alpha <- "a"
    }
    if (firstFor == 1) {
        print ("broseph")
        if (alpha == "a") {
            print ("sure")
            break # this break cut through the ifs to kill the firstFor loop
        }
    } else {
        print ("break")
        break
    }




for (ricky in 1 : 2) {
    for (ticky in 1 : 2) {
        for (tembo in 1 : 2) {
            for (no in 1 : 2) {
                for (so in 1 : 2) {
                    for (rembo in 1 : 2) {
                        for (hari in 1 : 2) {
                            for (bari in 1 : 2) {

                            }
                        }
                    }
                }
            }
        }
    }
}


attr (,"condition")
<simpleError in if (should_pick_neighbor (index = neighbor, total_chances = num_select_chances,     selection_context = select_type, current_chance = chance_for_selection,     sorted_selections = golf_score, selection_filter = singSuccessFilter,     preferred_bird = singer, lower = 0.5, upper = 0.75) == TRUE) {    singer <- golf_score[singer + neighbor]    tempMoran = update_selexn_data (parameters, tempMoran, selection.index,         singer, selector.index, curiosity_level, population,         select_type, selection.sylreps, selector.sylrep, chance_for_selection,         FALSE)    should_continue <- FALSE    break}: missing value where TRUE/FALSE needed>



for (SxMtPop in 1 : 8) { # curiosity and sylrep data for each subpopulation
    #   for (inhStyle in 1 : 2) { # lowMedHigh and narrowWide
        
        if (! (dir.exists (file.path (
          heatmap_sourceFolder, folderName, "lowMedHigh", 
          slicedPop[htmpView] # paste0 ("slice_", slice)
        )))) {
          dir.create (file.path (
            heatmap_sourceFolder, folderName, "lowMedHigh", 
            slicedPop[htmpView] # paste0 ("slice_", slice)
          ))
        }

        # dir.create (file.path (
        #     heatmap_sourceFolder, folderName, inhOptions[inhStyle + 2]
        # ))
        # if (inhStyle == 1) {
        #   if (htmpView == 3) {
            sliceNum = numOtherPopRuns
        #   } else {
        #     sliceNum = 3}
        # } else if (inhStyle == 2) {
        #   sliceNum = 2}

        # for (slice in 1 : sliceNum) {
          
          file_name <- paste0 (regularNames[SxMtPop], "_", slicedPop[htmpView], ".png")
          # rule of thumb: if we're splitting up htmpView _within_ slice and SxMtPop, then we need to save the output files according to the schema that will help pull back together the slices.
          png (filename = file.path (
              heatmap_sourceFolder, folderName, "lowMedHigh", 
              # paste0 ("slice_", slice), file_name), 
              slicedPop[htmpView], file_name), 
            width = 554, height = 554, units = "px", pointsize = 12, bg = "white")

        #   if (colorRange == "absolute") {
            if (SxMtPop <= 4) {heatmapRange <- c (0,1)
            } else {           heatmapRange <- c (1,156)}
        #   } else {
            
        #     heatmapRange <- inhOptions[[inhStyle]][
        #       dat_array_doh[1,1,1,slice]:dat_array_doh[1,1,2,slice],
        #       dat_array_doh[1,2,1,slice]:dat_array_doh[1,2,2,slice],
        #       dat_array_doh[1,3,1,slice]:dat_array_doh[1,3,2,slice],
        #       SxMtPop]
        #     heatmap_min <- c (
        #       round (min (heatmapRangeDatasetOne), 2),
        #       round (min (heatmapRangeDatasetTwo), 2),
        #       round (min (heatmapRangeDatasetTre), 2)
        #     )
        #     heatmap_max <- c (
        #       round (max (heatmapRangeDatasetOne), 2),
        #       round (max (heatmapRangeDatasetTwo), 2),
        #       round (max (heatmapRangeDatasetTre), 2)
        #     )
            
        #     heatmapRange <- c (heatmap_min[htmpView]-0.01,heatmap_max[htmpView]+0.01)
        #     rm (heatmapRangeDatasetOne, heatmapRangeDatasetTwo, heatmapRangeDatasetTre,
        #       heatmap_min, heatmap_max)
        #   } # UNFINISHED
          findXLab <- heatmap_axes[[htmpView]][1]
          findYLab <- heatmap_axes[[htmpView]][2]
          
        #   if (inhStyle == 1) {
            # dim_1 = 3
            # dim_2 = 3
            # dim_3 = 2
            # dat_array_doh <- array (c (
            #   1,1,1, 1,1,1, 1,1,1, 1,10,10, 10,1,10, numOtherPopRuns,numOtherPopRuns,1,
            #   2,1,1, 1,2,1, 1,1,2, 2,10,10, 10,2,10, numOtherPopRuns,numOtherPopRuns,2,
            #   10,1,1, 1,10,1, 1,1,numOtherPopRuns, 10,10,10, 10,10,10, numOtherPopRuns,numOtherPopRuns,numOtherPopRuns
            #   # rep (c (1, 1, 1, 1), 2), 1, 1, rep (c (3, 3, 3, 1), 2),
            #   # rep (c (2, 1, 1, 1), 2), 2, 2, rep (c (3, 3, 3, 2), 2),
            #   # rep (c (3, 1, 1, 1), 2), 3, 3, rep (c (3, 3, 3, 3), 2)
            # ), c (3,3,2,3))

            image(x = t (t (matrix (c (tempHtMpArray[,,,SxMtPop]), 10, 10))),
                col = colorSeqMultPalette$YlOrRd (100),
                axes = FALSE, 
                xlab = findXLab, 
                ylab = findYLab,cex.lab=1.4, zlim = heatmapRange)

            axis (1,c (-0.25, -0.175, -0.1, -0.025, 0.05, 0.125, 0.2, 0.275, 0.35, 0.425, 0.5, 0.575, 0.65, 0.725, 0.8, 0.875, 0.95, 1.025, 1.1, 1.175, 1.25),
                c ("", "0-.18", "", ".09-.27", "", ".18-.36", "", ".27-.45", "", ".36-.54", "", ".45-.63", "", ".54-.72", "", ".63-.81", "", ".72-.9", "", ".81-1.0", ""),
                # c ("", ".81-1.0", "", ".72-.9", "", ".63-.81", "", ".54-.72", "", ".45-.63", "", ".36-.54", "", ".27-.45", "", ".18-.36", "", ".09-.27", "", "0-.18", ""),
                TRUE,0,NA,FALSE,cex.axis=1, tck = 0)
            axis (1,c (-0.25, -0.1, 0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95, 1.1,1.25),
                c ("","","","","","","","","","",""),
                TRUE,-0.03,NA,FALSE,cex.axis=1, tck = -0.03)

            axis (2,c (-0.25, -0.175, -0.1, -0.025, 0.05, 0.125, 0.2, 0.275, 0.35, 0.425, 0.5, 0.575, 0.65, 0.725, 0.8, 0.875, 0.95, 1.025, 1.1, 1.175, 1.25),
                c ("", "0-.18", "", ".09-.27", "", ".18-.36", "", ".27-.45", "", ".36-.54", "", ".45-.63", "", ".54-.72", "", ".63-.81", "", ".72-.9", "", ".81-1.0", ""),
                # c ("", ".81-1.0", "", ".72-.9", "", ".63-.81", "", ".54-.72", "", ".45-.63", "", ".36-.54", "", ".27-.45", "", ".18-.36", "", ".09-.27", "", "0-.18", ""),
                TRUE,0,NA,FALSE,cex.axis=0.9, tck = 0)
            axis (2,c (-0.25, -0.1, 0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95, 1.1,1.25),
                c ("","","","","","","","","","",""),
                TRUE,-0.03,NA,FALSE,cex.axis=1, tck = -0.03)
          
          dev.off ()
        }



