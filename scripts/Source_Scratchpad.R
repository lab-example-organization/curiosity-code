

#install.packages("mailR")
library(mailR)
sender <- "parker.rundstrom@gmail.com"
recipients <- c("parker.j.rundstrom@vanderbilt.edu")
send.mail(from = sender,
          to = recipients,
          subject = "Test mail from Rstudio",
          body = "Test email body",
          smtp = list(host.name = "smtp.gmail.com", port = 465,
                      user.name = "parker.rundstrom@gmail.com",
                      passwd = "your_email_password", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)


# given number range, print out regex pattern necessary to grab all the patterns found within the number range


source(file.path("scripts", "Source_Reference_Section.R"))
referencesection("testings")
# 'testthat', 'dplyr', 'stringr' loaded

print_regex_num_range <- function (
    num_range = "0000-0001"
) {

    # Beginning vars to sort out
    first_term <- str_split(str_split(num_range, "-")[[1]][1], "")
    secnd_term <- str_split(str_split(num_range, "-")[[1]][2], "")



    ### This function lives and breathes on "append" as the builder function;
    ### each step in the control flow below is slowly building the ending
    ### character string depending on the relationship of the two terms.

    # Opening up the variable that will returned
    output_object <- vector (mode = "character", length = 1)

    # start with "*_"
    output_object <- paste0 ("*_")

    # the first number is smaller than the second number.
    if (as.numeric(paste(first_term[[1]], collapse = "")) < as.numeric(paste(secnd_term[[1]], collapse = ""))) {
        ##### # rearrange lists so that writing can be simplified: first_term[[1]]'s values are ordered as follows: [1] is the value in the 1's place, [2] is the value in the 10's place, [3] 100's place, [4] 1000's place, ...
        ##### temp_object <- list(
        #####     one = vector (length = length (first_term)),
        #####     two = vector (length = length (secnd_term))
        ##### )

        ##### for (i in 1:length(first_term[[1]])) {
        #####     temp_object[[1]][i] <- first_term[[1]][length(first_term[[1]]) + 1 - i]
        #####     temp_object[[2]][i] <- secnd_term[[1]][length(secnd_term[[1]]) + 1 - i]
        ##### }

        ##### while (length (temp_object$one) < length (temp_object$two)) {temp_object$one <- append (temp_object$one, "0")}

        # lengths of terms are equal
        if (length (first_term[[1]]) == length (secnd_term[[1]])) {
            # number of digits stops at 1
            if (length (first_term[[1]]) == 1) {
                # append "[1-2]_", where 1 is the first term and 2 is the second term ### "*_[1-2]_"
                append (output_object, paste0 ("[", first_term[[1]][1], "-", secnd_term[[1]][1], "]_"))
            # number of digits stops at 2
            } else if (length (first_term[[1]]) == 2) {
                # append "1" ### "*_1"
                append (output_object, paste0(first_term[[1]][1]))
                # 1X-4X
                if (as.numeric (first_term[[1]][1]) + 1 < as.numeric(secnd_term[[1]][1])) {
                    # 1X where X < 9
                    if (as.numeric (first_term[[1]][2]) < 9) {
                        # append "[X-9]_|*_[(1+1)-(4-1)][0-9]_|*_4" ### "*_1[X-9]_|*_[(1+1)-(4-1)][0-9]_|*_4"
                        append (output_object, paste0 ("[", first_term[[1]][2], "-9]_|*_[", as.numeric (first_term[[1]][1]) + 1, "-", as.numeric (secnd_term[[1]][1]) - 1, "][0-9]_|*_", secnd_term[[1]][1]))
                        # 2X where X > 0
                        if (as.numeric (secnd_term[[1]][2]) > 0) {
                            # append "[0-X]_" ### "*_1[X-9]_|*_[(1+1)-(4-1)][0-9]_|*_4[0-X]_"
                            append (output_object, paste0 ("[0-", secnd_term[[1]][2], "]_"))
                        # otherwise 2X where X = 0
                        } else {
                            # append "0_" ###  ### "*_1[X-9]_|*_[(1+1)-(4-1)][0-9]_|*_40_"
                            append (output_object, "0_")
                        }
                    # 1X where X = 9
                    } else {
                        # append "X_|*_[(1+1)-(4-1)][0-9]_|*_4" ### "*_1X_|*_[(1+1)-(4-1)][0-9]_|*_4"
                        append (output_object, paste0 (first_term[[1]][2], "_|*_[", as.numeric (first_term[[1]][1]) + 1, "-", as.numeric (secnd_term[[1]][1]) - 1, "][0-9]_|*_", secnd_term[[1]][1]))
                        # 2X where X > 0
                        if (as.numeric (secnd_term[[1]][2]) > 0) {
                            # append "[0-X]_" ### "*_1X_|*_[(1+1)-(4-1)][0-9]_|*_4[0-X]_"
                            append (output_object, paste0 ("[0-", secnd_term[[1]][2], "]_"))
                        # 2X where X = 0
                        } else {
                            # append "0_" ### "*_1X_|*_[(1+1)-(4-1)][0-9]_|*_40_"
                            append (output_object, "0_")
                        }
                    }
                # 1X-2X
                } else if (as.numeric (first_term[[1]][1]) + 1 == as.numeric(secnd_term[[1]][1])) {
                    # 1X where X < 9
                    if (as.numeric (first_term[[1]][2]) < 9) {
                        # append "[X-9]_|*_2" ### "*_1[X-9]_|*_2"
                        append (output_object, paste0 ("[", first_term[[1]][2], "-9]_|*_", secnd_term[[1]][1]))
                        # 2X where X > 0
                        if (as.numeric (secnd_term[[1]][2]) > 0) {
                            # append "[0-X]_" ### "*_1[X-9]_|*_2[0-X]_"
                            append (output_object, paste0 ("[0-", secnd_term[[1]][2], "]_"))
                        # otherwise 2X where X = 0
                        } else {
                            # append "0_" ###  ### "*_1[X-9]_|*_20_"
                            append (output_object, "0_")
                        }
                    # 1X where X = 9
                    } else {
                        # append "9_|*_2" ### "*_19_|*_2"
                        append (output_object, paste0 (first_term[[1]][2], "_|*_", secnd_term[[1]][1]))
                        # 2X where X > 0
                        if (as.numeric (secnd_term[[1]][2]) > 0) {
                            # append "[0-X]_" ### "*_19_|*_2[0-X]_"
                            append (output_object, paste0 ("[0-", secnd_term[[1]][2], "]_"))
                        # 2X where X = 0
                        } else {
                            # append "0_" ### "*_19_|*_20_"
                            append (output_object, paste0("0_"))
                        }
                    }
                # 1X -1Y
                } else if (as.numeric (first_term[[1]][1]) == as.numeric(secnd_term[[1]][1])) {
                    # append "[X-Y]_" ### "*_1[X-Y]_"
                    append (output_object, paste0 ("[", first_term[[1]][2], "-", secnd_term[[1]][2], "]_"))
                }
            # number of digits stops at 3 ### at this point, we have output_object = "*_"
            } else if (length (first_term[[1]]) == 3) {
                # append "1" ### "*_1"
                output_object <- append (output_object, paste0 (first_term[[1]][1]))
                # 1XY-4ZA
                if (as.numeric (first_term[[1]][1]) + 1 < as.numeric(secnd_term[[1]][1])) {
                    # 1X where X < 9
                    if (as.numeric (first_term[[1]][2]) < 9) {
                        # append "X" ### "*_1X"
                        output_object <- append (output_object, paste0 (first_term[[1]][2]))
                        # 1XY where Y < 9
                        if (as.numeric (first_term[[1]][3]) < 9) {
                            # append "[Y-9]_|*_1[(X+1)-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9]_|*_4" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9]_|*_4"
                            output_object <- append(output_object, paste0 ("[", first_term[[1]][3], "-9]_|*_", first_term[[1]][1], "[", as.numeric(first_term[[1]][2]) + 1, "-9][0-9]_|*_[", as.numeric (first_term[[1]][1]) + 1, "-", as.numeric (secnd_term[[1]][1]) - 1, "][0-9][0-9]_|*_", secnd_term[[1]][1]))
                            # 4Z where Z > 0
                            if (as.numeric (secnd_term[[1]][2]) > 0) {
                                # append "[0-(Z-1)][0-9]_|*_4Z" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9]_|*_4[0-(Z-1)][0-9]_|*_4Z"
                                output_object <- append (output_object, paste0 ("[0-", as.numeric (secnd_term[[1]][2]) - 1, "][0-9]_|*_", secnd_term[[1]][1], secnd_term[[1]][2]))
                                # 4ZA where A > 0
                                if (as.numeric (secnd_term[[1]][3]) > 0) {
                                    # append "[0-A]_" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9]_|*_4[0-(Z-1)][0-9]_|*_4Z[0-A]_"
                                    output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][3], "]_"))
                                # 4ZA where A == 0
                                } else {
                                    # append "0_" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9]_|*_4[0-(Z-1)][0-9]_|*_4Z0_"
                                    output_object <- append (output_object, paste0 ("0_"))
                                }
                            # 4Z where Z == 0
                            } else {
                                # append "0" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9]_|*_40"
                                output_object <- append (output_object, paste0 ("0"))
                                # 4ZA where A > 0
                                if (as.numeric (secnd_term[[1]][3]) > 0) {
                                    # append "[0-A]_" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9]_|*_40[0-A]_"
                                    output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][3], "]_"))
                                # 4ZA where A == 0
                                } else {
                                    # append "0_" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9]_|*_400_"
                                    output_object <- append (output_object, paste0 ("0_"))
                                }
                            }
                        # 1XY where Y == 9
                        } 
                        
                    }
                # 1XY-2ZA
                } else if (as.numeric (first_term[[1]][1]) + 1 == as.numeric (secnd_term[[1]][1])) {
                    # 1XY where X is less than 9
                    if (as.numeric (first_term[[1]][2]) < 9) {
                        # append "X" ### "*_1X"
                        output_object <- append (output_object, paste0 (first_term[[1]][2]))
                        #1XY where Y is less than 9
                        if (as.numeric (first_term[[1]][3]) < 9) {
                            # append "[Y-9]_|*_1[(X+1)-9][0-9]_|*_2" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_2"
                            output_object <- append (output_object, paste0 ("[", first_term[[1]][3], "-9]_|*_", first_term[[1]][1], "[", as.numeric (first_term[[1]][2]) + 1, "-9][0-9]_|*_", secnd_term[[1]][1]))
                            # 2ZA where Z > 0
                            if (as.numeric (secnd_term[[1]][2]) > 0) {
                                # append "0[0-9]_|*_2[1-(Z-1)][0-9]_|*_2Z" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_20[0-9]_|*_2[1-(Z-1)][0-9]_|*_2Z"
                                output_object <- append (output_object, paste0 ("0[0-9]_|*_", secnd_term[[1]][1], "[1-", as.numeric (secnd_term[[1]][2]) - 1, "][0-9]_|*_", secnd_term[[1]][1], secnd_term[[1]][2]))
                                # 2ZA where A > 0
                                if (as.numeric (secnd_term[[1]][3]) > 0) {
                                    # append "[0-A]_" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_20[0-9]_|*_2[1-(Z-1)][0-9]_|*_2Z[0-A]_"
                                    output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][3], "]_"))
                                # 2ZA where A == 0
                                } else {
                                    # append "0_" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_20[0-9]_|*_2[1-(Z-1)][0-9]_|*_2Z0_"
                                    output_object <- append (output_object, paste0 ("0_"))
                                }
                            # 2ZA where Z == 0
                            } else {
                                # append "0" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_20"
                                output_object <- append (output_object, paste0 ("0"))
                                # 2ZA where A > 0
                                if (as.numeric (secnd_term[[1]][3]) > 0) {
                                    # append "[0-A]_" ### *_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_20[0-A]_"
                                    output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][3], "]_"))
                                # 2ZA where A == 0
                                } else {
                                    # append "0_" ### "*_1X[Y-9]_|*_1[(X+1)-9][0-9]_|*_200_"
                                    output_object <- append (output_object, paste0 ("0_"))
                                }
                            }

                        # 1XY where Y == 9
                        }
                    }
                # 1XY-1ZA
                } else if (as.numeric (first_term[[1]][1]) == as.numeric (secnd_term[[1]][1])) {
                    # 11X-14X
                    if (as.numeric (first_term[[1]][2]) + 1 < as.numeric(secnd_term[[1]][2])) {
                        # 11X where X < 9
                        if (as.numeric (first_term[[1]][3]) < 9) {
                            # append "1[X-9]_|*_1[(1+1)-(4-1)][0-9]_|*_14" ### "*_11[X-9]_|*_1[(1+1)-(4-1)][0-9]_|*_14"
                            output_object <- append (output_object, paste0 (first_term[[1]][2], "[", first_term[[1]][3], "-9]_|*_", first_term[[1]][1], "[", as.numeric (first_term[[1]][2]) + 1, "-", as.numeric (secnd_term[[1]][2]) - 1, "][0-9]_|*_", secnd_term[[1]][1], secnd_term[[1]][2]))
                            # 14X where X > 0
                            if (as.numeric (secnd_term[[1]][3]) > 0) {
                                # append "[0-X]_" ### "*_11[X-9]_|*_1[(1+1)-(4-1)][0-9]_|*_14[0-X]_"
                                output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][3], "]_"))
                            # otherwise 14X where X = 0
                            } else {
                                # append "0_" ###  ### "*_11[X-9]_|*_1[(1+1)-(4-1)][0-9]_|*_140_"
                                output_object <- append (output_object, "0_")
                            }
                        # 11X where X = 9
                        } 
                    # 11X-12X
                    } else if (as.numeric (first_term[[1]][2]) + 1 == as.numeric(secnd_term[[1]][2])) {
                        # 11X where X < 9
                        if (as.numeric (first_term[[1]][3]) < 9) {
                            # append "1[X-9]_|*_12" ### "*_11[X-9]_|*_12"
                            output_object <- append (output_object, paste0 (first_term[[1]][2], "[", first_term[[1]][3], "-9]_|*_", secnd_term[[1]][1], secnd_term[[1]][2]))
                            # 12X where X > 0
                            if (as.numeric (secnd_term[[1]][3]) > 0) {
                                # append "[0-X]_" ### "*_11[X-9]_|*_12[0-X]_"
                                output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][3], "]_"))
                            # otherwise 12X where X = 0
                            } else {
                                # append "0_" ###  ### "*_11[X-9]_|*_120_"
                                output_object <- append (output_object, "0_")
                            }
                        # 
                    # 11X -11Y
                    } else if (as.numeric (first_term[[1]][2]) == as.numeric(secnd_term[[1]][2])) {
                        # append "1[X-Y]_" ### "*_11[X-Y]_"
                        output_object <- append (output_object, paste0 (first_term[[1]][2], "[", first_term[[1]][3], "-", secnd_term[[1]][3], "]_"))
                    }
                }





            } else if (length (first_term[[1]]) == 4) {
                # append "1" ### "*_1"
                output_object <- append (output_object, paste0 (first_term[[1]][1]))
                # 1XXX-4XXX
                if (as.numeric (first_term[[1]][1]) + 1 < as.numeric(secnd_term[[1]][1])) {
                    # 1ABC where A < 9
                    if (as.numeric (first_term[[1]][2]) < 9) {
                        # append "A" ### "*_1A"
                        output_object <- append (output_object, paste0 (first_term[[1]][2]))
                        # 1ABC where B < 9
                        if (as.numeric (first_term[[1]][3]) < 9) {
                            # append "B" ### "*_1AB"
                            output_object <- append (output_object, paste0 (first_term[[1]][3]))
                            # 1ABC where C < 9
                            if (as.numeric (first_term[[1]][4]) < 9) {
                                # append "[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_4" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_4"
                                # 4DEF where D > 0
                                if (as.numeric (secnd_term[[1]][2]) > 0) {
                                    # append "[0-(D-1)][0-9][0-9]_|*_4D" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_4[0-(D-1)][0-9][0-9]_|*_4D"
                                    # 4DEF where E > 0
                                    if (as.numeric (secnd_term[[1]][3]) > 0) {
                                        # append "[0-(E-1)][0-9]_|*_4DE" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_4[0-(D-1)][0-9][0-9]_|*_4D[0-(E-1)][0-9]_|*_4DE"
                                        # 4DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            # append "[0-F]_" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_4[0-(D-1)][0-9][0-9]_|*_4D[0-(E-1)][0-9]_|*_4DE[0-F]_"
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 4DEF where F == 0
                                        } else {
                                            # append "0_" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_4[0-(D-1)][0-9][0-9]_|*_4D[0-(E-1)][0-9]_|*_4DE0_"
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    # 4DEF where E == 0
                                    } else if (as.numeric (secnd_term[[1]][3]) == 0) {
                                        # append "0" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_4[0-(D-1)][0-9][0-9]_|*_4D0"
                                        # 4DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 4DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    }
                                # 4DEF where D == 0
                                } else if (as.numeric (secnd_term[[1]][2]) == 0) {
                                    # append "0" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_40"
                                    # 4DEF where E > 0
                                    if (as.numeric (secnd_term[[1]][3]) > 0) {
                                        # append "[0-(E-1)][0-9]_|*_40E" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_40[0-(E-1)][0-9]_|*_40E"
                                        # 4DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 4DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    # 4DEF where E == 0
                                    } else if (as.numeric (secnd_term[[1]][3]) == 0) {
                                        # append "0" ### "*_1AB[C-9]_|*_1A[B-9][0-9]_|*_1[A-9][0-9][0-9]_|*_[(1+1)-(4-1)][0-9][0-9][0-9]_|*_400"
                                        # 4DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 4DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    }
                                }
                            }
                        }
                    }






                # 1XXX-2XXX
                } else if (as.numeric (first_term[[1]][1]) + 1 == as.numeric (secnd_term[[1]][1])) {
                    # 1ABC where A < 9
                    if (as.numeric (first_term[[1]][2]) < 9) {
                        # 1ABC where B < 9
                        if (as.numeric (first_term[[1]][3]) < 9) {
                            # 1ABC where C < 9
                            if (as.numeric (first_term[[1]][4]) < 9) {
                                # 2DEF where D > 0
                                if (as.numeric (secnd_term[[1]][2]) > 0) {
                                    # 2DEF where E > 0
                                    if (as.numeric (secnd_term[[1]][3]) > 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    # 2DEF where E == 0
                                    } else if (as.numeric (secnd_term[[1]][3]) == 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    }
                                # 2DEF where D == 0
                                } else if (as.numeric (secnd_term[[1]][2]) == 0) {
                                    # 2DEF where E > 0
                                    if (as.numeric (secnd_term[[1]][3]) > 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    # 2DEF where E == 0
                                    } else if (as.numeric (secnd_term[[1]][3]) == 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    }
                                }
                            # 1ABC where C == 9
                            } else if (as.numeric (first_term[[1]][4]) == 9) {
                                # 2DEF where D > 0
                                if (as.numeric (secnd_term[[1]][2]) > 0) {
                                    # 2DEF where E > 0
                                    if (as.numeric (secnd_term[[1]][3]) > 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    # 2DEF where E == 0
                                    } else if (as.numeric (secnd_term[[1]][3]) == 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    }
                                # 2DEF where D == 0
                                } else if (as.numeric (secnd_term[[1]][2]) == 0) {
                                    # 2DEF where E > 0
                                    if (as.numeric (secnd_term[[1]][3]) > 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    # 2DEF where E == 0
                                    } else if (as.numeric (secnd_term[[1]][3]) == 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    }
                                }
                            }
                        # 1ABC where B == 9
                        } else if (as.numeric (first_term[[1]][3]) == 9) {
                            # 1ABC where C < 9
                            if (as.numeric (first_term[[1]][4]) < 9) {
                                # 2DEF where D > 0
                                if (as.numeric (secnd_term[[1]][2]) > 0) {
                                    # 2DEF where E > 0
                                    if (as.numeric (secnd_term[[1]][3]) > 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    # 2DEF where E == 0
                                    } else if (as.numeric (secnd_term[[1]][3]) == 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    }
                                # 2DEF where D == 0
                                } else if (as.numeric (secnd_term[[1]][2]) == 0) {
                                    # 2DEF where E > 0
                                    if (as.numeric (secnd_term[[1]][3]) > 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    # 2DEF where E == 0
                                    } else if (as.numeric (secnd_term[[1]][3]) == 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    }
                                }
                            # 1ABC where C == 9
                            } else if (as.numeric (first_term[[1]][4]) == 9) {
                                # 2DEF where D > 0
                                if (as.numeric (secnd_term[[1]][2]) > 0) {
                                    # 2DEF where E > 0
                                    if (as.numeric (secnd_term[[1]][3]) > 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    # 2DEF where E == 0
                                    } else if (as.numeric (secnd_term[[1]][3]) == 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    }
                                # 2DEF where D == 0
                                } else if (as.numeric (secnd_term[[1]][2]) == 0) {
                                    # 2DEF where E > 0
                                    if (as.numeric (secnd_term[[1]][3]) > 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    # 2DEF where E == 0
                                    } else if (as.numeric (secnd_term[[1]][3]) == 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    }
                                }
                            }
                        }
                    # 1ABC where A == 9
                    } else if (as.numeric (first_term[[1]][2]) == 9) {
                        # 1ABC where B < 9
                        if (as.numeric (first_term[[1]][3]) < 9) {
                            # 1ABC where C < 9
                            if (as.numeric (first_term[[1]][4]) < 9) {
                                # 2DEF where D > 0
                                if (as.numeric (secnd_term[[1]][2]) > 0) {
                                    # 2DEF where E > 0
                                    if (as.numeric (secnd_term[[1]][3]) > 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    # 2DEF where E == 0
                                    } else if (as.numeric (secnd_term[[1]][3]) == 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    }
                                # 2DEF where D == 0
                                } else if (as.numeric (secnd_term[[1]][2]) == 0) {
                                    # 2DEF where E > 0
                                    if (as.numeric (secnd_term[[1]][3]) > 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    # 2DEF where E == 0
                                    } else if (as.numeric (secnd_term[[1]][3]) == 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    }
                                }
                            # 1ABC where C == 9
                            } else if (as.numeric (first_term[[1]][4]) == 9) {
                                # 2DEF where D > 0
                                if (as.numeric (secnd_term[[1]][2]) > 0) {
                                    # 2DEF where E > 0
                                    if (as.numeric (secnd_term[[1]][3]) > 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    # 2DEF where E == 0
                                    } else if (as.numeric (secnd_term[[1]][3]) == 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    }
                                # 2DEF where D == 0
                                } else if (as.numeric (secnd_term[[1]][2]) == 0) {
                                    # 2DEF where E > 0
                                    if (as.numeric (secnd_term[[1]][3]) > 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    # 2DEF where E == 0
                                    } else if (as.numeric (secnd_term[[1]][3]) == 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    }
                                }
                            }
                        # 1ABC where B == 9
                        } else if (as.numeric (first_term[[1]][3]) == 9) {
                            # 1ABC where C < 9
                            if (as.numeric (first_term[[1]][4]) < 9) {
                                # 2DEF where D > 0
                                if (as.numeric (secnd_term[[1]][2]) > 0) {
                                    # 2DEF where E > 0
                                    if (as.numeric (secnd_term[[1]][3]) > 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    # 2DEF where E == 0
                                    } else if (as.numeric (secnd_term[[1]][3]) == 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    }
                                # 2DEF where D == 0
                                } else if (as.numeric (secnd_term[[1]][2]) == 0) {
                                    # 2DEF where E > 0
                                    if (as.numeric (secnd_term[[1]][3]) > 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    # 2DEF where E == 0
                                    } else if (as.numeric (secnd_term[[1]][3]) == 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    }
                                }
                            # 1ABC where C == 9
                            } else if (as.numeric (first_term[[1]][4]) == 9) {
                                # 2DEF where D > 0
                                if (as.numeric (secnd_term[[1]][2]) > 0) {
                                    # 2DEF where E > 0
                                    if (as.numeric (secnd_term[[1]][3]) > 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    # 2DEF where E == 0
                                    } else if (as.numeric (secnd_term[[1]][3]) == 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    }
                                # 2DEF where D == 0
                                } else if (as.numeric (secnd_term[[1]][2]) == 0) {
                                    # 2DEF where E > 0
                                    if (as.numeric (secnd_term[[1]][3]) > 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    # 2DEF where E == 0
                                    } else if (as.numeric (secnd_term[[1]][3]) == 0) {
                                        # 2DEF where F > 0
                                        if (as.numeric (secnd_term[[1]][4]) > 0) {
                                            output_object <- append (output_object, paste0 ("[0-", secnd_term[[1]][4], "]_"))
                                        # 2DEF where F == 0
                                        } else {
                                            output_object <- append (output_object, paste0 ("0_"))
                                        }
                                    }
                                }
                            }
                        }
                    }
                # 1XXX-1YYY
                } else if (as.numeric (first_term[[1]][1]) == as.numeric (secnd_term[[1]][1])) {
                    # 12BC-15EF
                    if (as.numeric (first_term[[1]][2]) + 1 < as.numeric(secnd_term[[1]][2])) {
                        # 12BC where B < 9
                            # 12BC where C < 9
                                # 15EF where E > 0
                                    # 15EF where F > 0
                                    # 15EF where F == 0
                                # 15EF where E == 0
                                    # 15EF where F > 0
                                    # 15EF where F == 0
                            # 12BC where C == 9
                                # 15EF where E > 0
                                    # 15EF where F > 0
                                    # 15EF where F == 0
                                # 15EF where E == 0
                                    # 15EF where F > 0
                                    # 15EF where F == 0
                        # 12BC where B == 9
                            # 12BC where C < 9
                                # 15EF where E > 0
                                    # 15EF where F > 0
                                    # 15EF where F == 0
                                # 15EF where E == 0
                                    # 15EF where F > 0
                                    # 15EF where F == 0
                            # 12BC where C == 9
                                # 15EF where E > 0
                                    # 15EF where F > 0
                                    # 15EF where F == 0
                                # 15EF where E == 0
                                    # 15EF where F > 0
                                    # 15EF where F == 0
                    # 12BC-13EF
                    } else if (as.numeric (first_term[[1]][2]) + 1 == as.numeric (secnd_term[[1]][2])) {
                        # 12BC where B < 9
                            # 12BC where C < 9
                                # 13EF where E > 0
                                    # 13EF where F > 0
                                    # 13EF where F == 0
                                # 13EF where E == 0
                                    # 13EF where F > 0
                                    # 13EF where F == 0
                            # 12BC where C == 9
                                # 13EF where E > 0
                                    # 13EF where F > 0
                                    # 13EF where F == 0
                                # 13EF where E == 0
                                    # 13EF where F > 0
                                    # 13EF where F == 0
                        # 12BC where B == 9
                            # 12BC where C < 9
                                # 13EF where E > 0
                                    # 13EF where F > 0
                                    # 13EF where F == 0
                                # 13EF where E == 0
                                    # 13EF where F > 0
                                    # 13EF where F == 0
                            # 12BC where C == 9
                                # 13EF where E > 0
                                    # 13EF where F > 0
                                    # 13EF where F == 0
                                # 13EF where E == 0
                                    # 13EF where F > 0
                                    # 13EF where F == 0
                    # 12BC-12EF
                    } else if (as.numeric (first_term[[1]][2]) == as.numeric (secnd_term[[1]][2])) {
                        # 123C-126F
                        if (as.numeric (first_term[[1]][3]) + 1 < as.numeric(secnd_term[[1]][3])) {
                        # 123C-124F
                        } else if (as.numeric (first_term[[1]][3]) + 1 == as.numeric (secnd_term[[1]][3])) {
                        # 123C-123F
                        } else if (as.numeric (first_term[[1]][3]) == as.numeric (secnd_term[[1]][3])) {
                        }
                    }
                }
            }
        } else {stop ("numbers need to be the same number of digits, because this function is ridiculous enough as is")}
    } else {stop ("first number needs to be smaller than the second number")}


    #         # the first number contains the same number of digits as the second number
    #         if (as.numeric (first_term[[1]][1]) < as.numeric (secnd_term[[1]][1])) {
    #             # the first term of the first number is smaller than the first term of the second number.
    #             if (as.numeric (first_term[[1]][2]) < as.numeric (secnd_term[[1]][2])) {
    #                 # both the first and second terms for the first number are smaller than the first and second terms of the second number.

    #             } else if (as.numeric (first_term[[1]][2]) == as.numeric (secnd_term[[1]][2])) {
    #                 # the second term is equal, so we need to go down to the third term to see what else needs to be changed.
    #                 if (as.numeric (first_term[[1]][3]) < as.numeric (secnd_term[[1]][3])) {
    #                     # term 1 smaller, term 2 equal, term 3 smaller
    #                     if (first_term[[1]][4]) < as.numeric (secnd_term[[1]][4]))
    #                         # term 1, 3, 4 smaller, term 2 equal
    #                         if ()
    #                 }
    #         } else if (as.numeric (first_term[[1]][2]) > as.numeric (secnd_term[[1]][2])) {

    #         }
    #         } else if (as.numeric (first_term[[1]][2]) < as.numeric (secnd_term[[1]][2])) {
    #             # while this is not true for the first term, the second term in the first number is smaller than the second term of the second number.

    #         } else if (as.numeric (first_term[[1]][3]) < as.numeric (secnd_term[[1]][3])) {
    #             # while not true for the first nor the second term, the third term of the first number is smaller than the third term of the second number.

    #         } else if (as.numeric (first_term[[1]][4]) < as.numeric (secnd_term[[1]][4])) {

    #         } else if (as.numeric (first_term[[1]][5]) < as.numeric (secnd_term[[1]][5])) {

    #         } else if (as.numeric (first_term[[1]][6]) < as.numeric (secnd_term[[1]][6])) {

    #         } else if (length (first_term) > 6) {
    #             stop ("Add additional digits in the ifelses in the function print_regex_num_range")
    #         }
    #         } else if (length(first_term[[1]]) + 1 == length(secnd_term[[1]])) {
    #         if (as.numeric (first_term[[1]][1]) < as.numeric (secnd_term[[1]][2])) {

    #         } else if (as.numeric (first_term[[1]][2]) < as.numeric (secnd_term[[1]][3])) {

    #         } else if (as.numeric (first_term[[1]][3]) < as.numeric (secnd_term[[1]][4])) {

    #         } else if (as.numeric (first_term[[1]][4]) < as.numeric (secnd_term[[1]][5])) {

    #         } else if (as.numeric (first_term[[1]][5]) < as.numeric (secnd_term[[1]][6])) {

    #         } else if (as.numeric (first_term[[1]][6]) < as.numeric (secnd_term[[1]][7])) {

    #         } else if (length (first_term) > 6 || length (secnd_term > 7)) {
    #             stop ("Add additional digits in the ifelses in the function print_regex_num_range")
    #         }
    #     } else if (length(secnd_term[[1]]) - length(first_term[[1]]) >= 2) {stop ("add additional special cases in function print_regex_num_range")}
    # } else {stop ("the first term in the num_range needs be smaller than the second term (reference: function print_regex_num_range)")}
    # output_object <- vector (mode = "character", length = 1)
    # output_object[1] <- paste0 ()
    return (paste(output_object, collapse = ""))
}

# special cases:



### First attempt at making this function:

# source(file.path("scripts", "Source_Reference_Section.R"))
# referencesection("heatmaps")
# source(file.path("scripts", "Source_Heatmap_Functions.R"))


# super_generic_function <- function (
#   4_digit_number_range, # "3456-3567" ### this is "thing"
#   4_digit_div # = "_"
# ) {

# first_term <- str_split(str_split(thing, "-")[[1]][1], "")
# secnd_term <- str_split(str_split(thing, "-")[[1]][2], "")
# new_term <- c()
# new_term_first_part <- c()
# new_term_secnd_part <- c()

# # where is the second term bigger than the first term?

# difference <- as.numeric(paste(str_split(thing, "-")[[1]][2], collapse = "")) - as.numeric(paste(str_split(thing, "-")[[1]][1], collapse = ""))

# if (difference < 1) {
#   string_to_search <- paste(str_split(thing, "-")[[1]][2], collapse = "")
# } else if (difference >= 1 && difference <= 9) {
#   if (as.numeric(secnd_term[[1]][4]) < as.numeric(first_term[[1]][4])) {

#   } else {

#   }
# } else if (difference > 10 && difference <= 100) {

# } else if (difference > 100 && difference <= 1000) {

# }

# if (first_term[[1]][4] != "9") {first_term_ones <- paste0(first_term[[1]][4], "-9")}
# first_term_ones <- paste0(first_term[[1]][1], first_term[[1]][2], first_term[[1]][3], "[", first_term_ones, "]")
# if (as.numeric(first_term[[1]][3]) + 1 != 9) {first_term_tens <- paste0(as.character(as.numeric(first_term[[1]][3]) + 1), "-9")}




# string_to_search <-

# return(string_to_search)
# }



### Potential Trash (DELETE AT THE END OF MAKING THIS FUNCTION)

# append (output_object, paste0 ())
#                     if (as.numeric (first_term[[1]][2] < secnd_term[[1]][2])) {
#                         if ()
#                         output_object <- paste0 ("*_", first_term[[1]][1], "[", first_term[[1]][2], "-", secnd_term[[1]][2], "]_")
#                     } else if (as.numeric (first_term[[1]][2] == secnd_term[[1]][2])) {

#                     } else if (as.numeric (first_term[[1]][2] > secnd_term[[1]][2])) {

#                     }


# output_object <- paste0 ("*_", first_term[[1]][1], "[", first_term[[1]][2], "-", secnd_term[[1]][2], "]_")


