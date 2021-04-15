make_colorpalettes <- function (stuff = c (2,7)) {
    colorseqmultpalette <- list (
        reds = colorRampPalette (c ("#fee0d2", "#fc9272", "#de2d26")), # 3-class reds                                        ### 1
        rdpu = colorRampPalette (c ("#fde0dd", "#fa9fb5", "#c51b8a")), # 3-class rdpu                                        ### 2
        oranges = colorRampPalette (c ("#fee6ce", "#fdae6b", "#e6550d")), # 3-class oranges                                  ### 3
        orrd = colorRampPalette (c ("#fee8c8", "#fdbb84", "#e34a33")), # 3-class orrd                                        ### 4
        five_by_five = colorRampPalette (c ("#ffeda0", "#feb24c", "#f03b20")), # 3-class ylorrd                                    ### 5
        ylorbr = colorRampPalette (c ("#fff7bc", "#fec44f", "#d95f0e")), # 3-class ylorbr                                    ### 6
        ylgn = colorRampPalette (c ("#f7fcb9", "#addd8e", "#31a354")), # 3-class ylgn                                        ### 7
        ylgnbu = colorRampPalette (c ("#edf8b1", "#7fcdbb", "#2c7fb8")), # 3-class ylgnbu                                    ### 8
        greens = colorRampPalette (c ("#e5f5e0", "#a1d99b", "#31a354")), # 3-class greens                                    ### 9
        gnbu = colorRampPalette (c ("#e0f3db", "#a8ddb5", "#43a2ca")), # 3-class gnbu                                        ### 10
        blues = colorRampPalette (c ("#deebf7", "#9ecae1", "#3182bd")), # 3-class blues                                      ### 11
        bugn = colorRampPalette (c ("#e5f5f9", "#99d8c9", "#2ca25f")), # 3-class bugn                                        ### 12
        bupu = colorRampPalette (c ("#e0ecf4", "#9ebcda", "#8856a7")), # 3-class bupu                                        ### 13
        purples = colorRampPalette (c ("#efedf5", "#bcbddc", "#756bb1")), # 3-class purples                                  ### 14
        purd = colorRampPalette (c ("#e7e1ef", "#c994c7", "#dd1c77")), # 3-class purd                                        ### 15
        pubu = colorRampPalette (c ("#ece7f2", "#a6bddb", "#2b8cbe")), # 3-class pubu                                        ### 16
        pubugn = colorRampPalette (c ("#ece2f0", "#a6bddb", "#1c9099")), # 3-class pubugn                                    ### 17
        greys = colorRampPalette (c ("#f0f0f0", "#bdbdbd", "#636363")), # 3-class greys                                      ### 18
        # midpoint = colorRampPalette (c ("#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac"))

        difference_spectrum = eval (parse (text = paste0 ("midpoint = colorRampPalette (c (rep (\"#67001f\", ",
                                                    stuff[2], "), rep (\"#b2182b\", ",
                                                    stuff[2], "), rep (\"#ca0020\", ",
                                                    stuff[2], "), rep (\"#d6604d\", ",
                                                    stuff[2], "), rep (\"#ef8a62\", ",
                                                    stuff[2], "), rep (\"#f4a582\", ",
                                                    stuff[2], "), rep (\"#fddbc7\", ",
                                                    stuff[2], "), rep (\"#f7f7f7\", ",
                                                    stuff[1], "), rep (\"#d1e5f0\", ",
                                                    stuff[2], "), rep (\"#92c5de\", ",
                                                    stuff[2], "), rep (\"#67a9cf\", ",
                                                    stuff[2], "), rep (\"#4393c3\", ",
                                                    stuff[2], "), rep (\"#0571b0\", ",
                                                    stuff[2], "), rep (\"#2166ac\", ",
                                                    stuff[2], "), rep (\"#053061\", ",
                                                    stuff[2], ")))"))),
        midpoint_but_smooth = colorRampPalette (c ("#67001f", "#b2182b", "#ca0020", "#d6604d", "#ef8a62", "#f4a582", "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de", "#67a9cf", "#4393c3", "#0571b0", "#2166ac", "#053061")),
        variance_spectrum = colorRampPalette (c ("#67001f", "#b2182b", "#ca0020", "#d6604d", "#ef8a62", "#f4a582", "#fddbc7", "#f7f7f7", "#d1e5f0", "#92c5de", "#67a9cf", "#4393c3", "#0571b0", "#2166ac", "#053061",
        "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7",
        "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7", "#f7f7f7")),
        midpoint_detail = colorRampPalette (c ("#67001f", "#67001f", "#b2182b", "#b2182b", "#ca0020", "#ca0020", "#d6604d", "#d6604d", "#ef8a62", "#ef8a62", "#f4a582", "#f4a582", "#fddbc7", "#fddbc7", "#f7f7f7", "#f7f7f7", "#d1e5f0", "#d1e5f0", "#92c5de", "#92c5de", "#67a9cf", "#67a9cf", "#4393c3", "#4393c3", "#0571b0", "#0571b0", "#2166ac", "#2166ac", "#053061", "#053061"))
    )
    return (colorseqmultpalette)
}

make_singpalette <- function () {
    colorseqsingpalette <- matrix (data = c (c ("#deebf7", "#9ecae1", "#3182bd"), # 3-class blues
                                          c ("#e5f5e0", "#a1d99b", "#31a354"), # 3-class greens
                                          c ("#f0f0f0", "#bdbdbd", "#636363"), # 3-class greys
                                          c ("#fee6ce", "#fdae6b", "#e6550d"), # 3-class oranges
                                          c ("#efedf5", "#bcbddc", "#756bb1"), # 3-class purples
                                          c ("#fee0d2", "#fc9272", "#de2d26")), # 3-class reds
                                nrow = 6, ncol = 3,byrow = TRUE,dimnames = list (
                                  c ("blue", "green", "grey", "orange", "purple", "red"),
                                  c ("Light first color", "Middle color", "Dark final color")))
    return (colorseqsingpalette)
}