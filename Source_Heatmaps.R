library(stringr)
setwd("~/Documents/projects/Code/Results/")
#setwd("~/Downloads/Lab_Notebook/GitHub/Code/Results")
if(!(dir.exists("Heatmaps"))) {dir.create("Heatmaps")}
setwd("Heatmaps")
heatmapLand <- getwd()
setwd("../../../../../../../../../../media/parker/A443-E926/simulation runs/shifting_curstart_for_heatmap/")
#setwd("/Volumes/NO NAME/simulation runs/shifting_curstart_for_heatmap/")
heatmap_runs <- list.files()

diffMale_start <- heatmap_runs[grep("[0-9]mp[0-9]", heatmap_runs)][c(1,2,3,4,5,6,7,31,46,8,9,10,11,12,47,13,14,15,48,16,17,18,19,49,20,21,22,23,24,25,26,27,28,29,32,33,34,35,36,37,30,50,38,39,40,41,42,43,44,45)]
sameMale_start <- heatmap_runs[grep("[0-9]m_", heatmap_runs)][c(18,1,2,16,3,5,4,6,14,19,15,20,10,11,13,12,7,9,17,8)]
allSame_start <- heatmap_runs[grep("[0-9]f", heatmap_runs, invert = T)][c(2,4,3,1,5)]

dir_lines <- length(list.files())
runs_ordered_for_heatmap <- vector("character", dir_lines)

diffMale_vector <- c(2,3,4,5,7,8,9,11,12,14,17,18,19,20,22,23,24,26,27,29,32,33,34,35,37,38,39,41,42,44,47,48,49,50,52,53,54,56,57,59,62,63,64,65,67,68,69,71,72,74)
sameMale_vector <- c(6,10,13,15,16,25,28,30,31,36,43,45,46,51,55,60,61,66,70,73)
allSame_vector <- c(1,21,40,58,75)

for(z in 1:50) {
  runs_ordered_for_heatmap[diffMale_vector[z]] <- diffMale_start[z]
}
for(y in 1:20) {
  runs_ordered_for_heatmap[sameMale_vector[y]] <- sameMale_start[y]
}
for(x in 1:5) {
  runs_ordered_for_heatmap[allSame_vector[x]] <- allSame_start[x]
}

#print(runs_ordered_for_heatmap)

heatmap_array <- array(0, dim = c(5,5,5,8), list(c("1-7mp1", "7-13mp1", "11-26mp1", "1-26mp1", "11-15mp1"), c("1-7mp2", "7-13mp2", "11-26mp2", "1-26mp2", "11-15mp2"), c("1-7f", "7-13f", "11-26f", "1-26f", "11-15f"), c("endcurm1","endcurm2","endcurf1","endcurf2","endrepm1","endrepm2","endrepf1","endrepf2")))

reference_array <- array(c( 1, 2, 3, 4, 5, 2, 6, 7, 8, 9, 3, 7,10,11,12, 4, 8,11,13,14, 5, 9,12,14,15,
                           16,17,18,19,20,17,21,22,23,24,18,22,25,26,27,19,23,26,28,29,20,24,27,29,30,
                           31,32,33,34,35,32,36,37,38,39,33,37,40,41,42,34,38,41,43,44,35,39,42,44,45,
                           46,47,48,49,50,47,51,52,53,54,48,52,55,56,57,49,53,56,58,59,50,54,57,59,60,
                           61,62,63,64,65,62,66,67,68,69,63,67,70,71,72,64,68,71,73,74,65,69,72,74,75), c(5,5,5))

for(femalez in 1:5) {
  for(malez1 in 1:5) {
    for(malez2 in 1:5) {
      setwd(paste0(runs_ordered_for_heatmap[reference_array[malez2,malez1,femalez]],"/multirun_output/"))
      setwd(list.files())
      #ending_curiosity_fem[x,y,z] <- 
      sumStats <- readLines(list.files()[grep("Summary_Statistics", list.files())])
      if(malez2>=malez1) {
        eight_numbers <- c(sumStats[24],sumStats[8],sumStats[32],sumStats[16],sumStats[20],sumStats[4],sumStats[28],sumStats[12])
      } else {
        eight_numbers <- c(sumStats[8],sumStats[24],sumStats[16],sumStats[32],sumStats[4],sumStats[20],sumStats[12],sumStats[28])
      }
      
      for(i in 1:8) {
        eight_numbers[i] <- str_sub(eight_numbers[i],5)
      }
      heatmap_array[malez2,malez1,femalez,] <- eight_numbers
      setwd("../../../")
    }
  }
}

#image(x = matrix(as.numeric(heatmap_array[,,1,1]),5,5),col =colorSeqMultPalette$PuBuGn(100), xlab = "")

if(!(file.exists("heatmap_output.RData"))) {saveRDS(heatmap_array, "../heatmap_output.RData")}
heatmap_array <- readRDS("../../../../../../media/parker/A443-E926/simulation runs/heatmap_output.RData")
colorSeqMultPalette <- list(BuGn = colorRampPalette(c("#e5f5f9", "#99d8c9", "#2ca25f")), # 3-class BuGn
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

#image(x = matrix(as.numeric(heatmap_array[,,1,1]),5,5),col =colorSeqMultPalette$PuBuGn(100), xlab = "Pop 1 Male Curstart", ylab = "Pop 2 Male Curstart")

title_names <- c("Ending Curiosity Values - Pop 1 Males","Ending Curiosity Values - Pop 2 Males",
                 "Ending Curiosity Values - Pop 1 Females","Ending Curiosity Values - Pop 2 Females",
                 "Ending Syll Rept Values - Pop 1 Males","Ending Syll Rept Values - Pop 2 Males",
                 "Ending Syll Rept Values - Pop 1 Females","Ending Syll Rept Values - Pop 2 Females")
# heatmap_categories <- c("cat(\"[,,1,1]\")","cat(\"[,1,,1]\")","cat(\"[1,,,1]\")")

heatmap_axes <- list(
  mp2Vfem = c("Pop 2 Male Starting Curiosity", "Female Starting Curiosity"),
  mp1Vfem = c("Pop 1 Male Starting Curiosity", "Female Starting Curiosity"),
  mp1Vmp2 = c("Pop 1 Male Starting Curiosity", "Pop 2 Male Starting Curiosity")
)

range_list <- array(data = c("Less Curiosity", "More Curiosity", "Seeks Similar Songs", "Seeks Novel Songs", "Low SylRep", "High Sylrep", "Limited Song Variety", "Varied Song"), c(2,2,2))

for(SxRpPop in 1:8) {
  SRP_names <- c("P1M_c","P2M_c","P1F_c","P2F_c","P1M_r","P2M_r","P1F_r","P2F_r")
  for(triple_iterator in 1:3) {
    
    layout(matrix(c(rep(1,56),rep(c(0,2,2,2,2,2,2,0),2),rep(0,8)),8,10,F,))
    #image(x = matrix(as.numeric(heatmap_array[,,1,1]),5,5),col =colorSeqMultPalette$PuBuGn(100))
    heatmap_min <- round(min(as.numeric(heatmap_array[mat[triple_iterator,1,1]:mat[triple_iterator,1,2],mat[triple_iterator,2,1]:mat[triple_iterator,2,2],mat[triple_iterator,3,1]:mat[triple_iterator,3,2],SxRpPop])), 3)
    heatmap_max <- round(max(as.numeric(heatmap_array[mat[triple_iterator,1,1]:mat[triple_iterator,1,2],mat[triple_iterator,2,1]:mat[triple_iterator,2,2],mat[triple_iterator,3,1]:mat[triple_iterator,3,2],SxRpPop])), 3)
    file_name <- paste0(title_names[SxRpPop], ".tiff")
    tiff(filename = file_name, width = 554, height = 467, units = "px", pointsize = 12, bg = "white", compression = "none")
    byTheCol <- c(rep(1,8),rep(3,8),rep(1,8),rep(3,8),rep(1,8),rep(3,8),rep(1,8),
                  rep(3,8),rep(1,8),rep(3,8),rep(1,8),rep(3,8),rep(1,8),rep(3,8),
                  rep(1,8),rep(3,8),
                  0,rep(2,6),0,0,rep(6,6),0,0,rep(2,6),0,0,rep(6,6),0,rep(0,16),
                  rep(3,8),rep(7,8),rep(3,8),rep(7,8),rep(3,8),rep(7,8),rep(3,8),rep(7,8),rep(3,8),rep(7,8),rep(3,8),rep(7,8),rep(3,8),rep(7,8),
                  0,rep(4,6),0,0,rep(8,6),0,0,rep(4,6),0,0,rep(8,6),0,rep(0,16))
    next_thing <- matrix(data=byTheCol,16,20)
    image(x = matrix(as.numeric(heatmap_array[mat[triple_iterator,1,1]:mat[triple_iterator,1,2],mat[triple_iterator,2,1]:mat[triple_iterator,2,2],mat[triple_iterator,3,1]:mat[triple_iterator,3,2],SxRpPop]),5,5),col =colorSeqMultPalette$PuBuGn(100), axes = F, xlab = heatmap_axes[[triple_iterator]][1], ylab = heatmap_axes[[triple_iterator]][2])
    #axis(1,c(0,0.25,0.5,0.75,1),c("0-0.25", "0.25-0.5", "0.45-1", "0-1", "0.45-0.55"),T,1,NA,F)
    #axis(1,c(-0.12,0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1,1.12),c("","0-0.25","", "0.25-0.5","", "0.45-1","", "0-1","", "0.45-0.55",""),T,1,NA,F)
    axis(1,c(-0.125,0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1,1.12),c("","0-.25","", ".25-.5","", ".45-1","", "0-1","", ".45-.55",""),T,0,NA,F,cex.axis=1, tck = 0)
    axis(1,c(-0.125,0.125,0.375,0.625,0.875,1.125),c("","","","","",""),T,-0.03,NA,F,cex.axis=1, tck = -0.03)
    
    axis(2,c(-0.125,0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1,1.12),c("","0-.25","", ".25-.5","", ".45-1","", "0-1","", ".45-.55",""),T,0,NA,F,cex.axis=1, tck = 0)
    axis(2,c(-0.125,0.125,0.375,0.625,0.875,1.125),c("","","","","",""),T,-0.03,NA,F,cex.axis=1, tck = -0.03)
    mtext(title_names[SxRpPop],3,1,cex = 1.5)
    #for(perchance in 1:19) {
    #  plot(matrix(c(rep(1,100),1:100),100,2),col=colorSeqMultPalette$PuBuGn(100),pch=perchance,cex=3, xlab = NA, ylab = NA, axes = F)
    #}
    plot(matrix(c(rep(1,100),1:100),100,2),col=colorSeqMultPalette$PuBuGn(100),pch=15,cex=15, xlab = NA, ylab = NA, axes = F)
    a <- -2; b <- 103.5; c <- (b-a)/10
    axis(2, seq(a,b,c),c("","","","","","","","","","",""))
    axis(2, c(15,85),c(range_list[1,1,ceiling(SxRpPop/4)],range_list[2,1,ceiling(SxRpPop/4)]), las=0,tck = 0)
    axis(4, c(5,50,95),c(heatmap_min,round((heatmap_min+heatmap_max)/2,3),heatmap_max), las=1,tck = 0, lwd=0)
    mtext(c(paste0("Auditory Curiosity","     ")),3,2.2,cex=1) # the fecking spaces are for keeping text center-aligned
    mtext("Seeks Novel Songs",3,1,cex = 0.8)
    mtext(range_list[1,2,ceiling(SxRpPop/4)],1,0.7,cex = 0.8)
    box("outer", "solid")
    dev.off()
    par(mfrow=c(1,1))
  }
  
}


# layout(matrix(c(rep(1,49),rep(c(0,2,2,2,2,2,0),2),rep(0,7)),7,10,F,))
# #image(x = matrix(as.numeric(heatmap_array[,,1,1]),5,5),col =colorSeqMultPalette$PuBuGn(100))
# image(x = matrix(as.numeric(heatmap_array[1,,,1]),5,5),col =colorSeqMultPalette$PuBuGn(100), axes = F, xlab = heatmap_axes[[triple_iterate]][1], ylab = heatmap_axes[[triple_iterate]][2])
# #axis(1,c(0,0.25,0.5,0.75,1),c("0-0.25", "0.25-0.5", "0.45-1", "0-1", "0.45-0.55"),T,1,NA,F)
# #axis(1,c(-0.12,0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1,1.12),c("","0-0.25","", "0.25-0.5","", "0.45-1","", "0-1","", "0.45-0.55",""),T,1,NA,F)
# axis(1,c(-0.125,0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1,1.12),c("","0-.25","", ".25-.5","", ".45-1","", "0-1","", ".45-.55",""),T,0,NA,F,cex.axis=1, tck = 0)
# axis(1,c(-0.125,0.125,0.375,0.625,0.875,1.125),c("","","","","",""),T,-0.03,NA,F,cex.axis=1, tck = -0.03)
# 
# axis(2,c(-0.125,0,0.125,0.25,0.375,0.5,0.625,0.75,0.875,1,1.12),c("","0-.25","", ".25-.5","", ".45-1","", "0-1","", ".45-.55",""),T,0,NA,F,cex.axis=1, tck = 0)
# axis(2,c(-0.125,0.125,0.375,0.625,0.875,1.125),c("","","","","",""),T,-0.03,NA,F,cex.axis=1, tck = -0.03)
# mtext(title_names[SxRpPop],3,1,cex = 1.5)
# #for(perchance in 1:19) {
# #  plot(matrix(c(rep(1,100),1:100),100,2),col=colorSeqMultPalette$PuBuGn(100),pch=perchance,cex=3, xlab = NA, ylab = NA, axes = F)
# #}
# plot(matrix(c(rep(1,100),1:100),100,2),col=colorSeqMultPalette$PuBuGn(100),pch=15,cex=15, xlab = NA, ylab = NA, axes = F)
# a <- -2; b <- 103.5; c <- (b-a)/10
# axis(2, seq(a,b,c),c("","","","","","","","","","",""))
# axis(2, c(15,85),c("Less Curiosity","More Curiosity"), las=0,tck = 0)
# mtext(c(paste0("Auditory Curiosity","     ")),3,2.2,cex=1) # the fecking spaces are for keeping text center-aligned
# mtext("Seeks Novel Songs",3,1,cex = 0.8)
# mtext("Seeks Similar Song",1,0.7,cex = 0.8)
# box("outer", "solid")
# par(mfrow=c(1,1))
#source("/home/Documents/projects/Code/curiosity-code/Source_Visualizing_Data.R")

# heatmap(x = t(matrix(data = as.numeric(heatmap_array[1,,,1]),nrow = 5,ncol = 5)),Rowv = NA,Colv = "Rowv",ColSideColors = T,RowSideColors = T,xlab = ,ylab = )
# heatmap(x = matrix(1:25,5,5,F),Rowv = NA,Colv = "Rowv")

#x <- c( rnorm(50,10,2), rnorm(30,20,2) )
#y <- 2+3*x + rnorm(80)
#d.x <- density(x)
#d.y <- density(y)
#layout( matrix( c(0,2,2,1,3,3,1,3,3),ncol=3) )
#plot(d.x$x, d.x$y, xlim=range(x), type='l')
#plot(d.y$y, d.y$x, ylim=range(y), xlim=rev(range(d.y$y)), type='l')
#plot(x,y, xlim=range(x), ylim=range(y) )


#length(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))

#plot(rep(1,100),col=sylnum_palette(100),pch=19,cex=3)


#nsamples <- 20
#mat <- rnorm(nsamples, .5, .15)
#dim(mat) <- c(4, 5)
#colMap <- colorRampPalette(c("red","white","blue" ))(nsamples)
#image(1:4, 1:5, mat, col = colMap, ylab="", xlab="")
#legend(grconvertX(0.5, "device"), grconvertY(1, "device"),
#       c("0",".5","1"), fill = colMap[c(1, 10, 20)], xpd = NA)





##' Prepare dendrograms for gplots' heatmap.2.
##' 
##' This function will prepare dendrograms for the heatmap.2 function (gplots).
##' The type of scaling can be adjusted and is performed before dendrogram
##' calculations (as opposed to native heatmap.2), reordering can be turned
##' on/off and distance and clustering functions can be customized.
##' 
##' 
##' @param x Numeric matrix of the values to be plotted.
##' @param scaledim character indicating if the values should be centered and
##' scaled in either the row direction or the column direction, or none.
##' @param zlim Reassign the extremes within the scaled data: zlim=c(-3,3).
##' @param zlim_select Select when to apply zlim. For the dendrogram
##' calculations and/or for the output data.
##' @param reorder Switch on/off dendrogram reordering for row and column.
##' @param scalefun Function to do the data scaling.
##' @param distfun Function used to compute the distance (dissimilarity)
##' between both rows and columns.
##' @param hclustfun Function used to compute the hierarchical clustering when
##' Rowv or Colv are not dendrograms.
##' @return A list with the scaled data ($data), row ($Rowv) and column ($Colv)
##' dendrograms.
##' @author Original function by Thomas W. Leja. Extended by Jan Stanstrup,
##' \email{stanstrup@@gmail.com}.
##' @references
##' http://stackoverflow.com/questions/17924828/differences-in-heatmap-clustering-defaults-in-r-heatplot-versus-heatmap-2
##' @export
##' @examples
##' 
##' library(massageR)
##' library(gplots)
##' library(RColorBrewer)
##' 
##' #scalefun <- BioMark:::scalefun("auto")
##' scalefun <- scale
##' 
##' distfun <- function(x) as.dist(1-cor(t(x)))
##' #distfun <- function(x) dist(x,method="canberra")
##' 
##' hclustfun <- function(x) hclust(x, method="complete")
##' 
##' 
##' x <- as.matrix(mtcars)
##' z <- heat.clust(x,
##'                 scaledim = "column",
##'                 zlim = c(-3,3),
##'                 zlim_select = c("dend","outdata"),
##'                 reorder  = c("column","row"),
##'                 distfun  = distfun, 
##'                 hclustfun= hclustfun,
##'                 scalefun = scalefun)
##' 
##' 
##' heatmap.2(z$data,
##'           Rowv=z$Rowv, 
##'           Colv=z$Colv,
##'           trace="none",
##'           scale="none",
##'           symbreaks = TRUE,
##'           col=rev(colorRampPalette(brewer.pal(10, "RdBu"))(256))
##'           )
##'           
##' @importFrom stats as.dendrogram as.dist cor hclust
##'

# heat.clust <- function(x, 
#                        scaledim = "column", 
#                        zlim=c(-3,3), 
#                        zlim_select = c("dend","outdata"), 
#                        reorder=c("column","row"),
#                        scalefun = scale,
#                        distfun = function(x) as.dist(1-cor(t(x))),
#                        hclustfun = function(x) hclust(x, method="complete")
# ) {
#   
#   z <- x
#   
#   # scaling
#   if ("row" %in% scaledim)    z <- t(scalefun(t(z)))
#   if ("column" %in% scaledim) z <- scalefun(z)
#   
#   
#   # dendrogram
#   z_dend <- z
#   
#   if ("dend" %in% zlim_select) {
#     z_dend <- pmin(pmax(z_dend, zlim[1]), zlim[2])
#   }
#   
#   hcl_row <- as.dendrogram(hclustfun(distfun(z_dend)))
#   hcl_col <- as.dendrogram(hclustfun(distfun(t(z_dend))))
#   
#   
#   if (("row" %in% reorder)    | isTRUE(reorder)){
#     ro      <-  rowMeans(z_dend, na.rm = T)
#     hcl_row <-  reorder(hcl_row,ro)
#   }
#   
#   if (("column" %in% reorder) | isTRUE(reorder)){
#     co      <-  colMeans(z_dend, na.rm = T)
#     hcl_col <-  reorder(hcl_col,co)
#   }
#   
#   
#   
#   # zlim outdata
#   if ("outdata" %in% zlim_select) {
#     z <- pmin(pmax(z, zlim[1]), zlim[2])
#   }
#   
#   
#   
#   out <- list(
#     data=z, 
#     Rowv=hcl_row, 
#     Colv=hcl_col
#   )
#   
#   return(out)
# }
# 
# 
# library(gplots)
# library(RColorBrewer)
# library(BioMark)
# 
# scalefun <- BioMark:::scalefun("auto")
# scalefun <- scale
# 
# #distfun <- function(x) as.dist(1-cor(t(x)))
# #distfun <- function(x) dist(x,method="canberra")
#  
# #hclustfun <- function(x) hclust(x, method="complete")
# thing <- heatmap_array[1,,,1]
# x <- as.matrix(thing)
# z <- heat.clust(x,
#                 scaledim="column",
#                 zlim=c(-3,3),
#                 zlim_select = c("dend","outdata"),
#                 reorder=c("column","row"),
#                 distfun  = function(x) as.dist(1-cor(t(x))), 
#                 hclustfun= function(x) hclust(x, method="complete"),
#                 scalefun = scale)
# 
# 
# heatmap.2(z$data,
#           Rowv=z$Rowv, 
#           Colv=z$Colv,
#           trace="none",
#           scale="none",
#           symbreaks = TRUE,
#           col=rev(colorRampPalette(brewer.pal(10, "RdBu"))(256)),
#           margins=c(4,7)
# )