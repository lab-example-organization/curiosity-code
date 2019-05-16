source(file.path("scripts", "Source_Reference_Section.R"))
referenceSection("heatmaps")

mult_ImgAppend <- function(
    ...
) {
    arguments <- list(...)
    argsLength <- length(arguments)
    while(argsLength>2) {
    extraArgs <- arguments[-c(1,2)]
    appendedImg <- image_append(c(arguments[[1]], arguments[[2]]))
    arguments <- list(appendedImg, extraArgs)
    argsLength <- length(arguments)
    }
    appendedImg <- image_append(c(arguments[[1]], arguments[[2]]))
    return(appendedImg)
}
