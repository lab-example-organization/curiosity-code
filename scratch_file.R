#data1s <- paste0(names[data_subset], "_", 1:num_timechunks, " <- readRDS(file = ", '"', run_number_directory, "/", strsplit(run_number_directory, "-GMT-")[[1]][2], "-", 1:num_timechunks, "-", names[data_subset], ".RData", '"', ")")
#cat(data1s, file = "data_subset.R", sep = "\n")
#source("data_subset.R")

#thing_1 <- paste0("stuff1 <- getwd()")
#thing_2 <- paste0("stuff2 <- getwd()")
#thing_3 <- paste0("stuff3 <- readRDS(file = \"", run_number_directory, "/variable-store-", 1:num_timechunks, "-", names[data_subset], ".RData)")
#eval(parse(text=c(thing_1, thing_2)))
#rm(stuff1,stuff2,thing_1,thing_2)


#thing <- paste0()

#for(data_subset in 1:4) {
#  for(i in 1:num_timechunks) {
#    variable_killer <- paste0("rm(", names[1:4], "_", 1:num_timechunks, ")")
#  }
#}

#for(i in 1:4) {
#  thing <- paste0("variable_killer", i, " <- rm(", old_names[i], "_", 1:num_timechunks, ")")
#  blah <- past
#  stuff <- paste0("rm(variable_killer", i)
#  eval(parse(text=c(thing, stuff)))
#}
### FINAL SUCCESSFUL REMOVE OPTION
#for(i in 1:4) {
#  thing <- paste0("rm(", old_names[i], "_", 1:num_timechunks, ")")
#  eval(parse(text=thing))
#}

# Create the base plotting window
# type = "n" does not plot the points
# Set the background color to "yellow"
#par(bg = "yellow")
#plot(1:10, type = "n")
number_of_runs <- 10
cat(number_of_runs, file = "number_of_runs.txt", append = F)

# A Silly Axis Example

# specify the data 
x <- c(1:10); y <- x; z <- 10/x

# create extra margin room on the right for an axis 
par(mar=c(5, 4, 4, 8) + 0.1)

# plot x vs. y 
low <- min(cursitylist[[1]][1,1,])
high <- max(cursitylist[[1]][1,1,])
ticks <- c(seq.int(low,high,((high-low)/10)))
plot(x = 1:P$num_timesteps, y = cursitylist[[1]][1,1,],type="b", cex=0.1, pch=19, col="red", axes=F, lty=3, xlab="", ylab="")
axis(2, tck=-0.05, at=c(low, ticks, high),labels=c(round(low, 2),round(ticks, 2),round(high, 2)), col.axis="red", las=2)
#axis(1, tck=-0.05, at=c(round((1:P$num_timesteps)/5)),labels=c(round((1:P$num_timesteps)/5)), col.axis="red", las=2)
axis(1, tck=-0.05, at=c(0,which((1:P$num_timesteps)%%(P$num_timesteps/10)==0)),labels=c(0,which((1:P$num_timesteps)%%(P$num_timesteps/10)==0)), col.axis="red", las=2)
which((1:P$num_timesteps)%%100==0)
# add x vs. 1/x 
lines(x, z, type="b", pch=22, col="blue", lty=2)

# draw an axis on the left 
axis(2, at=x,labels=x, col.axis="red", las=2)

# draw an axis on the right, with smaller text and ticks 
axis(4, at=z,labels=round(z,digits=2),
     col.axis="blue", las=2, cex.axis=0.7, tck=-.01)

# add a title for the right axis 
mtext("y=1/x", side=4, line=3, cex.lab=1,las=2, col="blue")

# add a main title and bottom and left axis labels 
title("An Example of Creative Axes", xlab="X values",
      ylab="Y=X")
