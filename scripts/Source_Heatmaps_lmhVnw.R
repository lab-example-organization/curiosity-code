source(file.path("scripts", "Source_Reference_Section.R"))
referenceSection("heatmaps")

# You Should be Here To: Run some Heatmaps to compar a wide range of inherited traits!
#
#   ___ ______________   _____________________      _____ __________  _________
#  /   |   \_   _____/  /  _  \__    ___/     \    /  _  \\______   \/   _____/
# /    ~    \    __)_  /  /_\  \|    | /  \ /  \  /  /_\  \|     ___/\_____  \ 
# \    Y    /        \/    |    \    |/    Y    \/    |    \    |    /        \
#  \___|_  /_______  /\____|__  /____|\____|__  /\____|__  /____|   /_______  /
#        \/        \/         \/              \/         \/                 \/ 




# This is an Example of what you should NEVER have in your code, presented here,

# "So that I can use it when I'm being a bad person :P"

######  setwd(file.path(strsplit(getwd(), "curiosity-code")[[1]][1], "curiosity-code"))


# Source the Functions

source(file.path("scripts", "Source_Heatmap_Functions.R"))

############## # # ARRANGEMENT OF FUNCTIONS  # # ##############

# heatmapLand <- HtMpDir(extraDir = "sameInh")
# heatmapLand <- file.path("results", "Heatmaps", "sameInh")
# heatmapLand <- file.path("results", "Heatmaps", "maleInh_maleBias")
# heatmapLand <- file.path("results", "Heatmaps", "femInh_maleBias")
# heatmapLand <- file.path("results", "Heatmaps", "femInh_femBias")
# heatmapLand <- file.path("results")
# heatmapLand <- file.path("..", "..", "old_stuff", "curiosity-code", 
#                          "results", "mixCI_10m-90f")
# heatmapLand <- file.path("..", "..", "old_stuff", "curiosity-code", 
#                          "results", "mixCI_25m-75f")
# heatmapLand <- file.path("..", "..", "old_stuff", "curiosity-code", 
#                          "results", "mixCI_40m-60f")
# heatmapLand <- file.path("..", "..", "old_stuff", "curiosity-code", 
#                          "results", "mixCI_60m-40f")
# heatmapLand <- file.path("..", "..", "old_stuff", "curiosity-code", 
#                          "results", "mixCI_75m-25f")
heatmapLand <- file.path("..", "..", "old_stuff", "curiosity-code", 
                         "results", "mixCI_90m-10f")

# heatmapLand <- file.path("results", "Heatmaps")

# all_the_runs <- list.files(heatmapLand, 
all_the_runs <- extractVarDirs(heatmapLand, 
  #"_1[7-9][0-9]|2[0-9][0-9]|3[0-9][0-9]|4[0-1][0-9]_") # <- This was for the very first run - non-automated... more code to follow.
  #"190304_1[7-9][0-9]_|190304_2[0-8][0-9]_|190304_29[0-5]_")
  # "*_1[7-9][0-9]_|*_2[0-8][0-9]_|*_29[0-5]_")                # maleinh maleBias
  # "*_2[9][6-9]_|*_3[0-9][0-9]_|*_4[0-1][0-9]_|*_420_")       # mothinh maleBias
  # "*_42[1-9]_|*_4[3-9][0-9]_|*_5[0-3][0-9]_|*_54[0-5]_")      # mothinh femBias
  # "*_54[6-9]_|*_5[5-9][0-9]_|*_6[0-6][0-9]_|*_670_")     # sameinh femaleBias
  # "*_67[1-9]_|*_6[8-9][0-9]_|*_7[0-8][0-9]_|*_79[0-4]_")  # sameinh_maleBias
  # "*_79[4-9]_|*_8[0-9][0-9]_|*_90[0-9]_|*_91[0-7]_|*_1041_")   # oppinh maleBias
  # "*_794_|*_91[8-9]_|*_9[2-9][0-9]_|*_10[0-3][0-9]_|*_104[0-1]_")   # oppinh femBias
  ##### "*_104[2-9]_|*_10[5-9][0-9]_|*_11[0-5][0-9]_|*_116[0-5]_|*_1289_") # maleinh femBias
  ##### "*_116[6-9]_|*_11[7-9][0-9]_|*_12[0-8][0-9]_") # 
  # "*_129[0-9]_|*_13[0-9][0-9]_|*_140[0-9]_|*_141[0-4]_") # mixedCurInh - sNTn (males 90%, females 10%)
  # "*_141[5-9]_|*_14[2-9][0-9]_|*_15[0-9][0-9]_|*_16[0-5][0-9]_|*_166[0-2]_") # mixedCurInh_-_sSTf (males 75%, females 25%) 
  # "*_166[3-9]_|*_16[7-9][0-9]_|*_1[7-8][0-9][0-9]_|*_190[0-9]_|*_1910_") # mixedCurInh_-_sSFr (males 60%, females 40%) ### running on LeonServer
  # "*_191[1-9]_|*_19[2-9][0-9]_|*_20[1-2][0-9]_|*_203[0-5]_") # mothInh_femaleBias_SD=5 ### running on pComp
  # "*_203[6-9]_|*_20[4-9][0-9]_|*_21[0-9][0-9]_|*_22[0-7][0-9]_|*_228[0-3]_") # mixedCurInh_-_sTnN (sub curinh males - 10%, curinh females - 90%)
  
  # "*_253[2-9]_|*_25[4-9][0-9]_|*_26[0-9][0-9]_|*_27[0-6][0-9]_|*_277[0-9]_")      # mixCI_10m-90f
  # "*_228[4-9]_|*_229[0-9]_|*_23[0-9][0-9]_|*_24[0-9][0-9]_|*_25[0-2][0-9]_|*_253[0-1]_")      # mixCI_25m-75f
  # "*_203[6-9]_|*_20[4-9][0-9]_|*_21[0-9][0-9]_|*_22[0-7][0-9]_|*_228[0-3]_")      # mixCI_40m-60f
  # "*_166[3-9]_|*_16[7-9][0-9]_|*_1[7-8][0-9][0-9]_|*_190[0-9]_|*_1910_")      # mixCI_60m-40f
  # "*_141[5-9]_|*_14[2-9][0-9]_|*_15[0-9][0-9]_|*_16[0-5][0-9]_|*_166[0-2]_")      # mixCI_75m-25f
  "*_27[8-9][0-9]_|*_28[0-9][0-9]_|*_29[0-9][0-9]_|*_30[0-1][0-9]_|*_302[0-7]_")      # mixCI_90m-10f
  
  # "*_302[8-9]_|*_303[0-9]_|*_304[0-5]_")      # sameinh popsplit lmh
  # "*_304[6-9]_|*_305[0-3]_")      # sameinh popsplit nw
  # "*_305[4-9]_|*_30[6][0-9]_|*_307[0-1]")      # mixinh popsplit lmh
  # "*_307[2-9]_")      # mixinh popsplit nw
#   connection <- file(description = file.path("source","temp", paste0(specificSimNumber, "_sim_data.txt")), open = "rt")
#   multiRun_folderList <- as.vector(read.table(connection, -1L)[[2]])
#   close(connection)

# # sTnNinh
# norm1 <- all_the_runs[1:65]  #421-432
# norm2 <- all_the_runs[66:80] #434-458
# norm3 <- all_the_runs[81:95] #460-474
# norm4 <- all_the_runs[96:110] #476-490
# norm5 <- all_the_runs[111:125] #492-506
# norm6 <- all_the_runs[126:140] #508-516
# norm7 <- all_the_runs[141:155] #517-545
# norm8 <- all_the_runs[156:170] #517-545
# norm9 <- all_the_runs[171:185] #517-545
# norm10 <- all_the_runs[186:200] #517-545
# norm11 <- all_the_runs[201:215] #517-545
# norm12 <- all_the_runs[216:230] #517-545
# norm13 <- all_the_runs[231:236] #517-545
# # 92 - 475; 93 - 491; 94 - 507; 124 - 433; 125 - 459
# all_the_runs <- c(norm1, all_the_runs[237], 
#                   norm2, all_the_runs[238], 
#                   norm3, all_the_runs[239], 
#                   norm4, all_the_runs[240], 
#                   norm5, all_the_runs[241], 
#                   norm6, all_the_runs[242], 
#                   norm7, all_the_runs[243], 
#                   norm8, all_the_runs[244], 
#                   norm9, all_the_runs[245], 
#                   norm10, all_the_runs[246], 
#                   norm11, all_the_runs[247], 
#                   norm12, all_the_runs[248], 
#                   norm13
#                   )

# # sTfSinh
# norm1 <- all_the_runs[1:63]  #421-432
# norm2 <- all_the_runs[64:78] #434-458
# norm3 <- all_the_runs[79:93] #460-474
# norm4 <- all_the_runs[94:108] #476-490
# norm5 <- all_the_runs[109:123] #492-506
# norm6 <- all_the_runs[124:138] #508-516
# norm7 <- all_the_runs[139:153] #517-545
# norm8 <- all_the_runs[154:168] #517-545
# norm9 <- all_the_runs[169:176] #517-545
# norm10 <- all_the_runs[177:182] #517-545
# norm11 <- all_the_runs[183:190] #517-545
# norm12 <- all_the_runs[191:196] #517-545
# norm13 <- all_the_runs[197:204] #517-545
# norm14 <- all_the_runs[205:210] #517-545
# norm15 <- all_the_runs[211:218] #517-545
# norm16 <- all_the_runs[219:222] #517-545
# norm17 <- all_the_runs[224:231] #517-545
# # 92 - 475; 93 - 491; 94 - 507; 124 - 433; 125 - 459
# all_the_runs <- c(norm1, all_the_runs[232], 
#                   norm2, all_the_runs[233], 
#                   norm3, all_the_runs[234], 
#                   norm4, all_the_runs[235], 
#                   norm5, all_the_runs[236], 
#                   norm6, all_the_runs[237], 
#                   norm7, all_the_runs[238], 
#                   norm8, all_the_runs[239], 
#                   norm9, all_the_runs[240], 
#                   norm10,all_the_runs[241],  
#                   norm11, all_the_runs[242], 
#                   norm12, all_the_runs[243], 
#                   norm13, all_the_runs[244], 
#                   norm14, all_the_runs[245], 
#                   norm15, all_the_runs[246], 
#                   norm16, all_the_runs[247], 
#                   all_the_runs[223], all_the_runs[248], 
#                   norm17
#                   )

# # sFrSinh
# norm1 <- all_the_runs[1:53] 
# norm2 <- all_the_runs[54:68]
# norm3 <- all_the_runs[69:83]
# norm4 <- all_the_runs[84:98]
# norm5 <- all_the_runs[99:113]
# norm6 <- all_the_runs[114:128]
# norm7 <- all_the_runs[129:143]
# norm8 <- all_the_runs[144:158]
# norm9 <- all_the_runs[159:173]
# norm10 <- all_the_runs[174:176]
# norm11 <- all_the_runs[177:187]
# norm12 <- all_the_runs[188:190]
# norm13 <- all_the_runs[191:201]
# norm14 <- all_the_runs[202:204]
# norm15 <- all_the_runs[205:215]
# norm16 <- all_the_runs[216:218]
# norm17 <- all_the_runs[219:228]
# #92 - 475; 93 - 491; 94 - 507; 124 - 433; 125 - 459
# all_the_runs <- c(norm1, all_the_runs[231], 
#                   norm2, all_the_runs[232], 
#                   norm3, all_the_runs[233], 
#                   norm4, all_the_runs[234], 
#                   norm5, all_the_runs[235], 
#                   norm6, all_the_runs[236], 
#                   norm7, all_the_runs[237], 
#                   norm8, all_the_runs[238], 
#                   norm9, all_the_runs[239], 
#                   norm10,all_the_runs[240],  
#                   norm11, all_the_runs[241], 
#                   norm12, all_the_runs[242], 
#                   norm13, all_the_runs[243], 
#                   norm14, all_the_runs[244], 
#                   norm15, all_the_runs[245], 
#                   norm16,
#                   all_the_runs[246], all_the_runs[247],
#                   norm17, all_the_runs[248],
#                   all_the_runs[229], all_the_runs[230]
#                   )

# sSFrinh
# norm1 <- all_the_runs[1:61] 
# norm2 <- all_the_runs[63:75]
# norm3 <- all_the_runs[77:89]
# norm4 <- all_the_runs[91:103]
# norm5 <- all_the_runs[105:117]
# norm6 <- all_the_runs[119:131]
# norm7 <- all_the_runs[133:145]
# norm8 <- all_the_runs[147:159]
# norm9 <- all_the_runs[161:168]
# norm10 <- all_the_runs[173:180]
# norm11 <- all_the_runs[185:192]
# norm12 <- all_the_runs[197:204]
# norm13 <- all_the_runs[205:206]
# norm14 <- all_the_runs[209:216]
# # norm15 <- all_the_runs[205:215]
# # norm16 <- all_the_runs[216:218]
# # norm17 <- all_the_runs[219:228]
# #92 - 475; 93 - 491; 94 - 507; 124 - 433; 125 - 459
# all_the_runs <- c(norm1, all_the_runs[217], all_the_runs[62], all_the_runs[247],
#                   norm2, all_the_runs[218], all_the_runs[76], all_the_runs[219],
#                   norm3, all_the_runs[220], all_the_runs[90], all_the_runs[221],
#                   norm4, all_the_runs[222], all_the_runs[104], all_the_runs[223],
#                   norm5, all_the_runs[224], all_the_runs[118], all_the_runs[225],
#                   norm6, all_the_runs[226], all_the_runs[132], all_the_runs[227],
#                   norm7, all_the_runs[228], all_the_runs[146], all_the_runs[229],
#                   norm8, all_the_runs[230], all_the_runs[160], all_the_runs[231],
#                   norm9, all_the_runs[232], all_the_runs[169], all_the_runs[170], 
#                   all_the_runs[248], all_the_runs[171], all_the_runs[233], 
#                   all_the_runs[172], all_the_runs[234], norm10, 
#                   all_the_runs[235], all_the_runs[181], all_the_runs[182], 
#                   all_the_runs[236], all_the_runs[183], all_the_runs[237], 
#                   all_the_runs[184], all_the_runs[238], norm11, 
#                   all_the_runs[239], all_the_runs[193], all_the_runs[194], 
#                   all_the_runs[240], all_the_runs[195], all_the_runs[241], 
#                   all_the_runs[196], all_the_runs[242], norm12, 
#                   all_the_runs[243], norm13, all_the_runs[244], 
#                   all_the_runs[207], all_the_runs[245], all_the_runs[208], 
#                   all_the_runs[246], norm14
#                   )

# # sSTfinh
# norm1 <- all_the_runs[1:52] 
# norm2 <- all_the_runs[53:60]
# norm3 <- all_the_runs[61:64]
# norm4 <- all_the_runs[65:72]
# norm5 <- all_the_runs[73:76]
# norm6 <- all_the_runs[77:84]
# norm7 <- all_the_runs[85:88]
# norm8 <- all_the_runs[90:95]
# norm9 <- all_the_runs[96:99]
# norm10 <- all_the_runs[101:106]
# norm11 <- all_the_runs[107:110]
# norm12 <- all_the_runs[112:117]
# norm13 <- all_the_runs[118:121]
# norm14 <- all_the_runs[123:128]
# norm15 <- all_the_runs[129:132]
# norm16 <- all_the_runs[134:139]
# norm17 <- all_the_runs[140:143]
# norm18 <- all_the_runs[145:146]
# norm19 <- all_the_runs[148:151]
# norm20 <- all_the_runs[153:154]
# norm21 <- all_the_runs[156:159]
# norm22 <- all_the_runs[161:162]
# norm23 <- all_the_runs[164:167]
# norm24 <- all_the_runs[169:170]
# norm25 <- all_the_runs[172:175]
# #92 - 475; 93 - 491; 94 - 507; 124 - 433; 125 - 459
# all_the_runs <- c(norm1, all_the_runs[178], norm2, all_the_runs[179], all_the_runs[243], all_the_runs[180],
#                   norm3, all_the_runs[181], norm4, all_the_runs[182], all_the_runs[183], all_the_runs[184],
#                   norm5, all_the_runs[185], norm6, all_the_runs[186], all_the_runs[187], all_the_runs[188],
#                   norm7, all_the_runs[189], all_the_runs[89], all_the_runs[190],
#                   norm8, all_the_runs[191], all_the_runs[192], all_the_runs[193],
#                   norm9, all_the_runs[194], all_the_runs[100], all_the_runs[248], 
#                   norm10, all_the_runs[195], all_the_runs[196], all_the_runs[197], 
#                   norm11, all_the_runs[198], all_the_runs[111], all_the_runs[199], 
#                   norm12, all_the_runs[200], all_the_runs[201], all_the_runs[202],
#                   norm13, all_the_runs[203], all_the_runs[122], all_the_runs[204],
#                   norm14, all_the_runs[205], all_the_runs[206], all_the_runs[207],
#                   norm15, all_the_runs[208], all_the_runs[133], all_the_runs[209],
#                   norm16, all_the_runs[244], all_the_runs[210], all_the_runs[211],
#                   norm17, all_the_runs[212], all_the_runs[144], all_the_runs[213], 
#                   norm18, all_the_runs[214], all_the_runs[242], all_the_runs[215], 
#                   all_the_runs[147], all_the_runs[216], all_the_runs[217], all_the_runs[218],
#                   norm19, all_the_runs[219], all_the_runs[152], all_the_runs[220], norm20,
#                   all_the_runs[221], all_the_runs[222], all_the_runs[223], all_the_runs[155], all_the_runs[245], all_the_runs[224], all_the_runs[225],
#                   norm21, all_the_runs[226], all_the_runs[160], all_the_runs[227],
#                   norm22, all_the_runs[228], all_the_runs[246], all_the_runs[229], all_the_runs[163], all_the_runs[230], all_the_runs[231],
#                   all_the_runs[232], norm23, all_the_runs[233], all_the_runs[168], all_the_runs[234], norm24,
#                   all_the_runs[235], all_the_runs[236], all_the_runs[237], all_the_runs[171], all_the_runs[247], all_the_runs[238], all_the_runs[239],
#                   norm25, all_the_runs[240], all_the_runs[176], all_the_runs[241], all_the_runs[177]
#                   )

# sNTninh
norm1 <- all_the_runs[1:2]
norm1_5 <- all_the_runs[3:17] 
norm2 <- all_the_runs[18:25]
norm2_5 <- all_the_runs[26:31]
norm3 <- all_the_runs[32:39]
norm4 <- all_the_runs[40:45]
norm5 <- all_the_runs[47:52]
norm5_5 <- all_the_runs[53:56]
norm6 <- all_the_runs[59:63]
norm7 <- all_the_runs[64:67]
norm8 <- all_the_runs[70:74]
norm9 <- all_the_runs[75:78]
norm10 <- all_the_runs[81:84]
norm11 <- all_the_runs[85:88]
norm12 <- all_the_runs[91:94]
norm13 <- all_the_runs[95:98]
norm14 <- all_the_runs[101:104]
norm14_5 <- all_the_runs[105:108]
norm15 <- all_the_runs[111:114]
norm16 <- all_the_runs[115:118]
norm17 <- all_the_runs[121:124]
norm18 <- all_the_runs[125:128]
norm19 <- all_the_runs[133:136]
norm20 <- all_the_runs[141:144]
norm21 <- all_the_runs[149:152]
norm22 <- all_the_runs[157:160]
# norm23 <- all_the_runs[164:167]
# norm24 <- all_the_runs[169:170]
# norm25 <- all_the_runs[172:175]
#92 - 475; 93 - 491; 94 - 507; 124 - 433; 125 - 459
all_the_runs <- c(
  norm1, all_the_runs[233], norm1_5, all_the_runs[164], 
  norm2, all_the_runs[231], norm2_5, all_the_runs[165], 
  norm3, all_the_runs[166], norm4, all_the_runs[167], 
  all_the_runs[46], all_the_runs[168], norm5, all_the_runs[169], 
  norm5_5, all_the_runs[170], all_the_runs[57], all_the_runs[171], 
  all_the_runs[58], all_the_runs[172], all_the_runs[173], norm6, 
  all_the_runs[174], norm7, all_the_runs[175], all_the_runs[68], 
  all_the_runs[176], all_the_runs[69], all_the_runs[177], 
  all_the_runs[234], norm8, all_the_runs[178], norm9,
  all_the_runs[235], all_the_runs[79], all_the_runs[179],
  all_the_runs[80], all_the_runs[180], all_the_runs[181], all_the_runs[236], 
  norm10, all_the_runs[182], norm11, all_the_runs[183],
  all_the_runs[89], all_the_runs[184], all_the_runs[90], all_the_runs[185], 
  all_the_runs[186], all_the_runs[187], norm12, all_the_runs[188], 
  norm13, all_the_runs[189], all_the_runs[99], all_the_runs[190],
  all_the_runs[100], all_the_runs[237], all_the_runs[191], all_the_runs[238], 
  norm14, all_the_runs[192],
  norm14_5, all_the_runs[193], all_the_runs[109], all_the_runs[194],
  all_the_runs[110], all_the_runs[239], all_the_runs[195], all_the_runs[196],
  norm15, all_the_runs[197], norm16, all_the_runs[198], 
  all_the_runs[119], all_the_runs[199], all_the_runs[120], all_the_runs[200], 
  all_the_runs[201], all_the_runs[202], norm17, all_the_runs[203],
  norm18, all_the_runs[240], all_the_runs[129], all_the_runs[204],
  all_the_runs[130], all_the_runs[241],
  all_the_runs[205], all_the_runs[206], all_the_runs[131],
  all_the_runs[232], all_the_runs[242], all_the_runs[132], all_the_runs[207],
  norm19, all_the_runs[208], all_the_runs[137],
  all_the_runs[209], all_the_runs[138], all_the_runs[210], all_the_runs[211], 
  all_the_runs[212], all_the_runs[139], all_the_runs[213], all_the_runs[214], all_the_runs[140], 
  all_the_runs[215], norm20, all_the_runs[248], all_the_runs[145],
  all_the_runs[216], all_the_runs[146], all_the_runs[243],
  all_the_runs[217], all_the_runs[218], all_the_runs[147], all_the_runs[244], all_the_runs[245], all_the_runs[148], all_the_runs[219], 
  norm21, all_the_runs[220], all_the_runs[153],                  
  all_the_runs[221], all_the_runs[154], all_the_runs[222], all_the_runs[223], all_the_runs[224], all_the_runs[155],                  
  all_the_runs[225], all_the_runs[226], all_the_runs[156], all_the_runs[227],                   
  norm22, all_the_runs[246], all_the_runs[161], all_the_runs[228], all_the_runs[162],
  all_the_runs[247], all_the_runs[229], all_the_runs[230], all_the_runs[163]
)


# stuff <- vector("character", length(all_the_runs))
# for(thing in length(all_the_runs)) {
# stuff[thing] <- str_sub(all_the_runs[thing], 8L, 10L)
# }

### Opposite Inheritance - Female Bias (differing curstarts between populations)
# norm1 <- all_the_runs[1:5]  #794, 918-921
# norm2 <- all_the_runs[6:35] #1000-1029
# norm3 <- all_the_runs[36:113] #922-999
# norm4 <- all_the_runs[114:125] #1030-1041
# all_the_runs <- c(norm1, norm3, norm2, norm4)

# norm1 <- all_the_runs[1:86]  #546-631
# norm2 <- all_the_runs[87:101] #633-647
# norm3 <- all_the_runs[102:111] #649-658
# norm4 <- all_the_runs[112:115] #660-663
# norm5<- all_the_runs[116:121] #665-670
# all_the_runs <- c(norm1, all_the_runs[123], norm2, all_the_runs[125], norm3, all_the_runs[124], norm4, all_the_runs[122], norm5)

# norm1 <- all_the_runs[1:117]  #671-792
# norm2 <- all_the_runs[120:125] #633-647
# all_the_runs <- c(norm1, norm2, all_the_runs[118:119])

# norm1 <- all_the_runs[1:57]
# norm2 <- all_the_runs[58:124]
# all_the_runs <- c(norm1, all_the_runs[125], norm2)


# norm1 <- all_the_runs[1:12]  #421-432
# norm2 <- all_the_runs[13:37] #434-458
# norm3 <- all_the_runs[38:52] #460-474
# norm4 <- all_the_runs[53:67] #476-490
# norm5<- all_the_runs[68:82] #492-506
# norm6 <- all_the_runs[83:91] #508-516
# norm7 <- all_the_runs[95:123]#517-545
# # 92 - 475; 93 - 491; 94 - 507; 124 - 433; 125 - 459
# all_the_runs <- c(norm1, all_the_runs[124], norm2, all_the_runs[125], norm3, all_the_runs[92], norm4, all_the_runs[93], norm5, all_the_runs[94], norm6, norm7)
# # 


# # norm1 <- all_the_runs[1:57]  #421-432
# # norm2 <- all_the_runs[13:37] #434-458
# # norm3 <- all_the_runs[38:52] #460-474
# # norm4 <- all_the_runs[53:67] #476-490
# # norm5<- all_the_runs[68:82] #492-506
# # norm6 <- all_the_runs[83:91] #508-516
# # norm7 <- all_the_runs[95:123]#517-545
# # 92 - 475; 93 - 491; 94 - 507; 124 - 433; 125 - 459
# # all_the_runs <- c(norm1, all_the_runs[124], norm2, all_the_runs[125], norm3, all_the_runs[92], norm4, all_the_runs[93], norm5, all_the_runs[94], norm6, norm7)
# ;


# profvis({
# #   for(iteration in 1:10) {
#     extractedMeans <- extractMeans(allRunDirs = all_the_runs, 
#         dirHeatMap = heatmapLand, source_of_params = "params.yaml")
# #   }
# })


all_the_MaleRuns <- c(all_the_runs[1:124], all_the_runs[248])
all_the_FemaleRuns <- c(all_the_runs[1], all_the_runs[125:248])

# extractedMeans <- extractMeans(allRunDirs = all_the_runs, dirHeatMap = heatmapLand, source_of_params = "params.yaml", deeper = FALSE)
# all_the_names <- remakeString(all_the_runs, "_", ".")

# names(extractedMeans) <- all_the_names

extractedFemaleMeans <- extractMeans(allRunDirs = all_the_FemaleRuns, dirHeatMap = heatmapLand, source_of_params = "params.yaml", deeper = FALSE)
# all_the_names <- remakeString(all_the_FemaleRuns, "_", ".")
# names(extractedFemaleMeans) <- all_the_names
makeHeatmapFile(inheritance = 5, diffcurstartBias = 2, absolute = TRUE, specialFigs = FALSE, lmhVnw = FALSE, extractedMeans = extractedFemaleMeans)
# makeHeatmapFile(inheritance = 9, diffcurstartBias = 2, absolute = TRUE, specialFigs = FALSE, lmhVnw = FALSE, extractedMeans = extractedMeans)
# makeHeatmapFile(inheritance = 8, diffcurstartBias = 2, absolute = TRUE, specialFigs = FALSE, lmhVnw = FALSE, extractedMeans = extractedMeans)
# makeHeatmapFile(inheritance = 7, diffcurstartBias = 2, absolute = TRUE, specialFigs = FALSE, lmhVnw = FALSE, extractedMeans = extractedMeans)
# makeHeatmapFile(inheritance = 6, diffcurstartBias = 2, absolute = TRUE, specialFigs = FALSE, lmhVnw = FALSE, extractedMeans = extractedMeans)
# makeHeatmapFile(inheritance = 5, diffcurstartBias = 2, absolute = TRUE, specialFigs = FALSE, lmhVnw = FALSE, extractedMeans = extractedMeans)

# names(extractedMeans) <- all_the_names

extractedMaleMeans <- extractMeans(allRunDirs = all_the_MaleRuns, dirHeatMap = heatmapLand, source_of_params = "params.yaml")
# all_the_names <- remakeString(all_the_MaleRuns, "_", ".")
# names(extractedMaleMeans) <- all_the_names
makeHeatmapFile(inheritance = 5, diffcurstartBias = 1, absolute = TRUE, specialFigs = FALSE, lmhVnw = FALSE, extractedMeans = extractedMaleMeans)
# makeHeatmapFile(inheritance = 9, diffcurstartBias = 1, absolute = TRUE, specialFigs = FALSE, lmhVnw = FALSE, extractedMeans = extractedMeans)
# makeHeatmapFile(inheritance = 8, diffcurstartBias = 1, absolute = TRUE, specialFigs = FALSE, lmhVnw = FALSE, extractedMeans = extractedMeans)
# makeHeatmapFile(inheritance = 7, diffcurstartBias = 1, absolute = TRUE, specialFigs = FALSE, lmhVnw = FALSE, extractedMeans = extractedMeans)
# makeHeatmapFile(inheritance = 6, diffcurstartBias = 1, absolute = TRUE, specialFigs = FALSE, lmhVnw = FALSE, extractedMeans = extractedMeans)
# makeHeatmapFile(inheritance = 5, diffcurstartBias = 1, absolute = TRUE, specialFigs = FALSE, lmhVnw = FALSE, extractedMeans = extractedMeans)

# names(extractedMaleMeans) <- all_the_names


# heatmapLand

# makeHeatmaps <- function (
#   inheritance = 1,
#   diffcurstartBias = 1
# )

#   whichInh <- c("male","moth","same","opps","sNTn","sSTf","sSFr","sFrS","sTfS","sTnN", "FfFf")

# whichBias <- c("male","female")

# makeHeatmapFile(inheritance = 3, diffcurstartBias = 3, absolute = TRUE, specialFigs = TRUE, lmhVnw = TRUE, extractedMeans = extractedMeans)
makeHeatmapFile(inheritance = 10, diffcurstartBias = 1, absolute = TRUE, specialFigs = FALSE, lmhVnw = FALSE, extractedMeans = extractedMeans)
makeHeatmapFile(inheritance = 10, diffcurstartBias = 2, absolute = TRUE, specialFigs = FALSE, lmhVnw = FALSE, extractedMeans = extractedMeans)

# makeHeatmapFile(inheritance = 11, diffcurstartBias = 3, absolute = TRUE, specialFigs = TRUE, lmhVnw = TRUE, extractedMeans = extractedMeans)
# makeHeatmapFile(inheritance = 11, diffcurstartBias = 3, absolute = TRUE, specialFigs = TRUE, lmhVnw = FALSE, extractedMeans = extractedMeans)
# makeHeatmaps(inheritance = 1, diffcurstartBias = 1, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 2, diffcurstartBias = 1, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 1, diffcurstartBias = 2, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 2, diffcurstartBias = 2, absolute = TRUE, reDo = TRUE)

# makeHeatmaps(inheritance = 3, diffcurstartBias = 1, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 3, diffcurstartBias = 2, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 4, diffcurstartBias = 1, absolute = TRUE, reDo = TRUE)
# makeHeatmaps(inheritance = 4, diffcurstartBias = 2, absolute = TRUE, reDo = TRUE)





  # whichInh <- c("male","moth","same","opps","sNTn","sSTf","sSFr","sFrS","sTfS","sTnN", "FfFf")






IndividualFigures <- function (

  inheritance = 1, #c("maleinh", "mothinh", "sameinh", "oppsinh","sNTninh", "sSTfinh", "sSFrinh", "sFrSinh", "sTfSinh", "sTnNinh", "FfFfinh")
  colorRange = 2, # c("relative", "absolute")
  thisBias = 1, # 3 or 4
  numOtherPopRuns = 2 # this will force the dimensions of the data structure to include only the number of starting conditions that are present for the population of non-interest
) {
  
  # heatmap_sourceFolder <- file.path("results", "Heatmaps", "output_objects")
  heatmap_sourceFolder <- file.path("results")
  # heatmap_sourceFolder <- file.path("sameSexFigResults", "results")
  

  # Character vectors for args - indices  Not sure what else to call them, but they'll be used to reassign the args to non-numeric objects

  ClrRngContainer <- c("relative", "absolute")
  
  inheritanceContainer <- c("maleinh", "mothinh", "sameinh", "oppsinh",
                            "sNTninh", "sSTfinh", "sSFrinh", "sFrSinh",
                            "sTfSinh", "sTnNinh", "FfFfinh")

  whichBias <- c("maleBias", "femaleBias", "pop1Bias", "pop2Bias", "bothBias")

  if (thisBias == 1) {
    heatmap_axes <- list(
      mp2Vfem = c("Pop 2 Male Starting Curiosity", "Female Starting Curiosity"),    # mp2Vfem
      mp1Vfem = c("Pop 1 Male Starting Curiosity", "Female Starting Curiosity"),    # mp1Vfem
      mp1Vmp2 = c("Pop 1 Male Starting Curiosity", "Pop 2 Male Starting Curiosity") # mp1Vmp2
    )
    slicedPop <- list(
      "MalPop1",
      "MalPop2",
      "FemalePop"
    )
  } else if (thisBias == 2) {
    heatmap_axes <- list(
      mf2Vmal = c("Pop 2 Female Starting Curiosity", "Male Starting Curiosity"),
      mf1Vmal = c("Pop 1 Female Starting Curiosity", "Male Starting Curiosity"),
      mf1Vmf2 = c("Pop 1 Female Starting Curiosity", "Pop 2 Female Starting Curiosity")
    )
    slicedPop <- list(
      "FemPop1",
      "FemPop2",
      "MalePop"
    )
  } else if (thisBias == 3) {
    heatmap_axes <- list(
      fp1Vpp2 = c("Pop 1 Female Starting Curiosity", "Pop 2 Starting Curiosity"),
      mp1Vpp2 = c("Pop 1 Male Starting Curiosity", "Pop 2 Starting Curiosity"),
      mp1Vfp1 = c("Pop 1 Male Starting Curiosity", "Pop 1 Female Starting Curiosity")
    )
    slicedPop <- list(
      "MalPop1",
      "FemPop1",
      "Popula2"
    )
  }

  colorRange <- ClrRngContainer[colorRange]

  inheritance <- inheritanceContainer[inheritance]

  thisBias <- whichBias[thisBias]
  
  folderName <- list.files(heatmap_sourceFolder)[which(sapply(list.files(heatmap_sourceFolder), function(x) (inheritance %in% str_split(x, "_")[[1]][4] && thisBias %in% str_split(x, "_")[[1]][5])))]

  # tempHtMpArray <- readRDS(file.path(heatmap_sourceFolder, folderName, list.files(file.path(heatmap_sourceFolder, folderName), pattern = ".RData")))
  HtMpArrays <- list.files(file.path(heatmap_sourceFolder, folderName), pattern = ".RData")

  if (length (HtMpArrays) == 1) {
    tempHtMpArray <- readRDS(file.path(heatmap_sourceFolder, folderName, HtMpArrays))

    lowMedHigh <- tempHtMpArray[1:3,1:3,1:3,]
    narrowWide <- tempHtMpArray[4:5,4:5,4:5,]
    
  } else if (length (HtMpArrays) == 2) {
    lowMedHigh <- readRDS(file.path("results", folderName, list.files(file.path("results", folderName), pattern = "lowMedHigh.RData")))
    narrowWide <- readRDS(file.path("results", folderName, list.files(file.path("results", folderName), pattern = "narrowWide.RData")))
  }

  inhOptions <- list(
    lowMedHigh = lowMedHigh,
    narrowWide = narrowWide,
    LMHtext = "lowMedHigh",
    NWtext = "narrowWide"
  )
  
  # DONE.
  # NOW WE NEED TO MAKE THE FIGURES AND SORT THEM INTO FOLDERS THAT WE'LL PULL THEM OUT OF TO MAKE THE STITCHED-TOGETHER FILES.
  # TITLES DON'T MATTER CURRENTLY, BUT WILL ONCE THEY GET STITCHED TOGETHER.
  # THIS DIRECTORY (FOR THESE UNSTITCHED ONES) SHOULD BE A SUBFOLDER WITHIN THE STITCHED FIGURE DIR

  if(!(dir.exists(file.path(
    heatmap_sourceFolder, folderName, "lowMedHigh")))
  ) {
    dir.create(file.path(
      heatmap_sourceFolder, folderName, "lowMedHigh"
    ))
    dir.create(file.path(
      heatmap_sourceFolder, folderName, "narrowWide"
    ))
    
  }

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
    YlOrRd = colorRampPalette(c("#ffeda0", "#feb24c", "#f03b20")), # 3-class YlOrRd
    Greys = colorRampPalette(c("#f0f0f0", "#bdbdbd", "#636363"))#, # 3-class Greys
    # Reds = #fee0d2, #fc9272, #de2d26
    # Purples = #efedf5, #bcbddc, #756bb1
    # Oranges = #fee6ce, #fdae6b, #e6550d
    # Greens = #e5f5e0, #a1d99b, #31a354
    # Blues = #deebf7, #9ecae1, #3182bd

  )

  regularNames <- c(
    "EndCurValP1M",
    "EndCurValP2M",
    "EndCurValP1F",
    "EndCurValP2F",
    "EndSRpValP1M",
    "EndSRpValP2M",
    "EndSRpValP1F",
    "EndSRpValP2F"
  )

  # source("/home/parker/Documents/projects/curmodel_pcomp1/Code/curiosity-code/scripts/Source_Magick_Functions.R")

  for (htmpView in 1:3) { # looking at the cubes from different angles (aka which population are we seeing one slice at a time, while the other populations are plotted on the axes?)
    for (SxMtPop in 1:8) { # curiosity and sylrep data for each subpopulation
      for (inhStyle in 1:2) { # lowMedHigh and narrowWide
        
        if(!(dir.exists(file.path(
          heatmap_sourceFolder, folderName, inhOptions[inhStyle + 2], 
          slicedPop[htmpView] # paste0("slice_", slice)
        )))) {
          dir.create(file.path(
            heatmap_sourceFolder, folderName, inhOptions[inhStyle + 2], 
            slicedPop[htmpView] # paste0("slice_", slice)
          ))
        }

        # dir.create(file.path(
        #     heatmap_sourceFolder, folderName, inhOptions[inhStyle + 2]
        # ))
        if (inhStyle == 1) {
          if (htmpView == 3) {
            sliceNum = numOtherPopRuns
          } else {
            sliceNum = 3}
        } else if (inhStyle == 2) {
          sliceNum = 2}

        for (slice in 1:sliceNum) {
          
          file_name <- paste0(regularNames[SxMtPop], "_slice_", slice, "_", slicedPop[htmpView], ".png")
          # rule of thumb: if we're splitting up htmpView _within_ slice and SxMtPop, then we need to save the output files according to the schema that will help pull back together the slices.
          png(filename = file.path(
              heatmap_sourceFolder, folderName, inhOptions[inhStyle + 2], 
              # paste0("slice_", slice), file_name), 
              slicedPop[htmpView], file_name), 
            width = 554, height = 554, units = "px", pointsize = 12, bg = "white")

          if(colorRange == "absolute") {
            if (SxMtPop <= 4) {heatmapRange <- c(0,1)
            } else {           heatmapRange <- c(1,156)}
          } else {
            
            heatmapRange <- inhOptions[[inhStyle]][
              dat_array_doh[1,1,1,slice]:dat_array_doh[1,1,2,slice],
              dat_array_doh[1,2,1,slice]:dat_array_doh[1,2,2,slice],
              dat_array_doh[1,3,1,slice]:dat_array_doh[1,3,2,slice],
              SxMtPop]
            heatmap_min <- c(
              round(min(heatmapRangeDatasetOne), 2),
              round(min(heatmapRangeDatasetTwo), 2),
              round(min(heatmapRangeDatasetTre), 2)
            )
            heatmap_max <- c(
              round(max(heatmapRangeDatasetOne), 2),
              round(max(heatmapRangeDatasetTwo), 2),
              round(max(heatmapRangeDatasetTre), 2)
            )
            
            heatmapRange <- c(heatmap_min[htmpView]-0.01,heatmap_max[htmpView]+0.01)
            rm(heatmapRangeDatasetOne, heatmapRangeDatasetTwo, heatmapRangeDatasetTre,
              heatmap_min, heatmap_max)
          } # UNFINISHED
          findXLab <- heatmap_axes[[htmpView]][1]
          findYLab <- heatmap_axes[[htmpView]][2]
          
          if(inhStyle == 1) {
            dim_1 = 3
            dim_2 = 3
            dim_3 = 2
            dat_array_doh <- array(c(
              1,1,1, 1,1,1, 1,1,1, 1,3,3, 3,1,3, numOtherPopRuns,numOtherPopRuns,1,
              2,1,1, 1,2,1, 1,1,2, 2,3,3, 3,2,3, numOtherPopRuns,numOtherPopRuns,2,
              3,1,1, 1,3,1, 1,1,numOtherPopRuns, 3,3,3, 3,3,3, numOtherPopRuns,numOtherPopRuns,numOtherPopRuns
              # rep(c(1, 1, 1, 1), 2), 1, 1, rep(c(3, 3, 3, 1), 2),
              # rep(c(2, 1, 1, 1), 2), 2, 2, rep(c(3, 3, 3, 2), 2),
              # rep(c(3, 1, 1, 1), 2), 3, 3, rep(c(3, 3, 3, 3), 2)
            ), c(3,3,2,3))

            image(x = matrix(as.numeric(
            inhOptions[[inhStyle]][
              dat_array_doh[htmpView,1,1,slice]:dat_array_doh[htmpView,1,2,slice],
              dat_array_doh[htmpView,2,1,slice]:dat_array_doh[htmpView,2,2,slice],
              dat_array_doh[htmpView,3,1,slice]:dat_array_doh[htmpView,3,2,slice],
              SxMtPop
            ]),nrow(inhOptions[[inhStyle]][
              dat_array_doh[htmpView,1,1,slice]:dat_array_doh[htmpView,1,2,slice],
              dat_array_doh[htmpView,2,1,slice]:dat_array_doh[htmpView,2,2,slice],
              dat_array_doh[htmpView,3,1,slice]:dat_array_doh[htmpView,3,2,slice],
              SxMtPop
            ]), ncol(inhOptions[[inhStyle]][
              dat_array_doh[htmpView,1,1,slice]:dat_array_doh[htmpView,1,2,slice],
              dat_array_doh[htmpView,2,1,slice]:dat_array_doh[htmpView,2,2,slice],
              dat_array_doh[htmpView,3,1,slice]:dat_array_doh[htmpView,3,2,slice],
              SxMtPop
            ])),
            col = colorSeqMultPalette$Greys(100),
            axes = F, 
            xlab = findXLab, 
            ylab = findYLab,cex.lab=1.4, zlim = heatmapRange)

            axis(1,c(-0.25 ,0      ,0.25  ,0.5      ,0.75  ,0.97    ,1.25),
                c(""    ,"0-.25",""    , ".25-.5",""    , ".45-1",""  ),
                T,0,NA,F,cex.axis=1, tck = 0)
            axis(1,c(-0.25,0.25,0.75,1.25),
                c("","","",""),
                T,-0.03,NA,F,cex.axis=1, tck = -0.03)

            if (htmpView == 3) {
              axis(2,c(-0.25 ,0      ,0.25  ,0.5      ,0.75  ,0.97    ,1.25),
                c(""    ,"0-.25",""    , ".25-.5",""    , ".45-1",""  ),
                T,0,NA,F,cex.axis=0.6, tck = 0)
              axis(2,c(-0.25,0.25,0.75,1.25),
                c("","","",""),
                T,-0.03,NA,F,cex.axis=1, tck = -0.03)
            } else {
              axis(2,c(-0.5,  0  ,0.5,    1    ,1.5),
                c(  ""   ,"0-0.25","" ,".45-1","" ),
                T,0,NA,F,cex.axis=0.8, tck = 0)
              axis(2,c(-0.5,0.5,1.5),
                c("","",""),
                T,-0.03,NA,F,cex.axis=1, tck = -0.03)
            }

          } else if (inhStyle == 2) {

            dat_array_doh <- array(c(
              rep(c(1, 1, 1, 1), 2), 1, 1, rep(c(2, 2, 2, 1), 2),
              rep(c(2, 1, 1, 1), 2), 2, 2, rep(c(2, 2, 2, 2), 2)
            ), c(3,3,2,2))

            image(x = matrix(as.numeric(
            inhOptions[[inhStyle]][
              dat_array_doh[htmpView,1,1,slice]:dat_array_doh[htmpView,1,2,slice],
              dat_array_doh[htmpView,2,1,slice]:dat_array_doh[htmpView,2,2,slice],
              dat_array_doh[htmpView,3,1,slice]:dat_array_doh[htmpView,3,2,slice],
              SxMtPop
            ]),nrow(inhOptions[[inhStyle]][
              dat_array_doh[htmpView,1,1,slice]:dat_array_doh[htmpView,1,2,slice],
              dat_array_doh[htmpView,2,1,slice]:dat_array_doh[htmpView,2,2,slice],
              dat_array_doh[htmpView,3,1,slice]:dat_array_doh[htmpView,3,2,slice],
              SxMtPop
            ]), ncol(inhOptions[[inhStyle]][
              dat_array_doh[htmpView,1,1,slice]:dat_array_doh[htmpView,1,2,slice],
              dat_array_doh[htmpView,2,1,slice]:dat_array_doh[htmpView,2,2,slice],
              dat_array_doh[htmpView,3,1,slice]:dat_array_doh[htmpView,3,2,slice],
              SxMtPop
            ])),
            col = colorSeqMultPalette$YlOrBr(100),
            axes = F, 
            xlab = findXLab, 
            ylab = findYLab,cex.lab=1.4, zlim = heatmapRange)

            axis(1,c(-0.5,  0  ,0.5,    1    ,1.5),
                c(  ""   ,"0-1","" ,".45-.55","" ),
                T,0,NA,F,cex.axis=0.8, tck = 0)
            axis(1,c(-0.5,0.5,1.5),
                c("","",""),
                T,-0.03,NA,F,cex.axis=1, tck = -0.03)
            
            axis(2,c(-0.5,  0  ,0.5,    1    ,1.5),
                c(  ""   ,"0-1","" ,".45-.55","" ),
                T,0,NA,F,cex.axis=0.6, tck = 0)
            axis(2,c(-0.5,0.5,1.5),
                c("","",""),
                T,-0.03,NA,F,cex.axis=1, tck = -0.03)
          }
          
          dev.off()
        }

      }
    }
  }
  # }
  return(print("Done, in the specified folder"))
}

CombineEditSingles <- function (
  inheritanceStyle = 1,
  bias = 1,
  metricsSexPop = 1, # only allowed 1-4 (p1m, p2m, p1f, p2f)
  otherPopStyle = 1, # low, high (for lmh & nw stuff round May 2019)
  edit = FALSE
) {
  
#   source("/home/parker/Documents/projects/curmodel_pcomp1/Code/curiosity-code/scripts/Source_Magick_Functions.R")
# Access the same subdirectory where the individual images are stored
  

#  heatmap_sourceFolder <- file.path("results", "Heatmaps", "output_objects")
  heatmap_sourceFolder <- file.path("results")
  
  SxMtPopContainer <- c("EndCurValP1M",
                          "EndCurValP2M",
                          "EndCurValP1F",
                          "EndCurValP2F",
                          "EndSRpValP1M",
                          "EndSRpValP2M",
                          "EndSRpValP1F",
                          "EndSRpValP2F")
  
  
  
  curstartPatternContainer <- c("lowMedHigh", "narrowWide")
  
  inheritanceContainer <- c("maleinh", "mothinh", "sameinh", "oppsinh",
                              "sNTninh", "sSTfinh", "sSFrinh", "sFrSinh",
                              "sTfSinh", "sTnNinh", "FfFfinh")

  titleSxMtPop <- c("Pop 1 Mal", "Pop 2 Mal", 
                    "Pop 1 Fem", "Pop 2 Fem", 
                    "Pop 1 Mal", "Pop 2 Mal", 
                    "Pop 1 Fem", "Pop 2 Fem")

  titleInhStyle <- c("Male", "Female", "Same-Sex", "Opposite",
                     "90M10F", "75M25F", "60M40F", "40M60F", 
                     "25M75F", "10M90F", "50-50")

  titleBackgroundPop <- c("Low", "High")
  
  whichBias <- c("maleBias", "femaleBias", "pop1Bias")
  whichPopBias <- c("FemalePop", "MalePop", "Popula2")
  
  subpopulation <- c("p1m", "p2m", "p1f", "p2f")
  
  folderBias <- list.files(heatmap_sourceFolder)[which(sapply(list.files(heatmap_sourceFolder), function(x) (inheritanceContainer[inheritanceStyle] %in% str_split(x, "_")[[1]][4] && whichBias[bias] %in% str_split(x, "_")[[1]][5])))]
  
  PopBias <- whichPopBias[bias]
  
  #   for (i in 1:2) {
      
  namesForOtherPop <- c("slice_1", "slice_2")
  stylesForOtherPop <- c("low", "high")
  
  lowMedHighFolder <- file.path(heatmap_sourceFolder, folderBias, curstartPatternContainer[1], PopBias)
  narrowWideFolder <- file.path(heatmap_sourceFolder, folderBias, curstartPatternContainer[2], PopBias)

  image_1 <- image_read(file.path(lowMedHighFolder, paste0(SxMtPopContainer[metricsSexPop], "_", namesForOtherPop[otherPopStyle], "_", PopBias, ".png")))
  image_2 <- image_read(file.path(narrowWideFolder, paste0(SxMtPopContainer[metricsSexPop], "_", namesForOtherPop[otherPopStyle], "_", PopBias, ".png")))
  image_3 <- image_read(file.path(lowMedHighFolder, paste0(SxMtPopContainer[metricsSexPop+4], "_", namesForOtherPop[otherPopStyle], "_", PopBias, ".png")))
  image_4 <- image_read(file.path(narrowWideFolder, paste0(SxMtPopContainer[metricsSexPop+4], "_", namesForOtherPop[otherPopStyle], "_", PopBias, ".png")))

  if(! (edit)) {
    
    top_row <- image_append(c(image_1, image_2))
    bottom_row <- image_append(c(image_3, image_4))
    final_set <- image_append(c(top_row, bottom_row), stack = TRUE)
    # image_write(final_set, path = file.path(heatmap_sourceFolder, folderBias, paste0(PopBias, "_", subpopulation[metricsSexPop], "_measure_", stylesForOtherPop[otherPopStyle], "_background.png")))
  
  } else if(edit) {
    
    top_row <- image_append(c(image_1, image_2))
    bottom_row <- image_append(c(image_3, image_4))
    final_set <- image_append(c(top_row, bottom_row), stack = TRUE)
    
    final_set <- image_border(final_set, "white", "75x75")

    final_set <- image_annotate(
        final_set, paste0(titleSxMtPop[metricsSexPop], 
                          " Ending Traits - ", 
                          titleInhStyle[inheritanceStyle],
                          " AC Inheritance"), 
        size="50", 
        location = "+40+25")

    final_set <- image_annotate(
        final_set, paste0(titleBackgroundPop[otherPopStyle], " Background Population Starting Curiosity"), 
        size="30",
        location = "+350+80")

    final_set <- image_annotate(
        final_set, paste0("Syllable Repertoire        |        Auditory Curiosity"), 
        size="40",
        degrees=270,
        location = "+20+1055")

    final_set <- image_border(final_set, "white", "30x30")

    final_set <- image_annotate(
        final_set, paste0("Low/Medium/High        |        Narrow/Wide"), 
        size="35", 
        location = "+315+1235")
    
    
    # image_write(final_set, path = file.path(
    #     heatmap_sourceFolder, folderBias, paste0(
    #                 "Popula2", "_", "p1f", 
    #                 "_measure_", "low", 
    #                 "_background.png")))
      # This is where we edit the stuff we worked on! 
      # There need to be ways to access the files made in the first half, and it should also contain everything within another control structure: "if(files exist in folder) {} else {print("Great Job, oh wait this is an error message. Um, you should make sure the function is pointed at the right files. Are the right ones perhaps absent?")}"

  }
  
  image_write(final_set, path = file.path(heatmap_sourceFolder, folderBias, paste0(PopBias, "_", subpopulation[metricsSexPop], "_measure_", inheritanceContainer[inheritanceStyle], "_", stylesForOtherPop[otherPopStyle], "_background.png")))

  return(print("done"))
}
for (k in 1:2) {
    thing <- c(3, 11)


    for (i in 1:4) {
        for (j in 1:2) {
            CombineEditSingles(thing[k],3,i,j, T)
        }
    }
}
stackMultiples <- function (
  inheritance = 1, # c("sameinh", "oppsinh", "maleinh", "mothinh")
  pattern = 1 # 1 = narrowWide, 2 = lowMedHigh
) {
  

  # maleInhMaleVFemaleBias

  SxMtPopContainer <- c("EndCurValP1M",
                        "EndCurValP2M",
                        "EndCurValP1F",
                        "EndCurValP2F",
                        "EndSRpValP1M",
                        "EndSRpValP2M",
                        "EndSRpValP1F",
                        "EndSRpValP2F")

  heatmap_sourceFolder <- file.path("results", "Heatmaps", "output_objects")
  whichBias <- c("maleBias", "femaleBias")
  whichPopBias <- c("FemalePop", "MalePop")
  # folderBias <- list.files(heatmap_sourceFolder)[which(sapply(list.files(heatmap_sourceFolder), function(x) (inheritance %in% str_split(x, "_")[[1]][4] && whichBias[bias] %in% str_split(x, "_")[[1]][5])))]
  curstartPatternContainer <- c("narrowWide", "lowMedHigh")
  # relevantFolder <- file.path(heatmap_sourceFolder, folderBias, curstartPatternContainer[pattern])

  inheritanceContainer <- c("sameinh", "oppsinh", "maleinh", "mothinh")
  inheritance <- inheritanceContainer[inheritance]
  
  heatmap_sourceFolder <- file.path("results", "Heatmaps", "output_objects")
  
  output_folder <- file.path(heatmap_sourceFolder, paste0("Combined_", inheritance))# "_pattern_", curstartPatternContainer[pattern]))
  if(!(dir.exists(output_folder))) {dir.create(output_folder)}
  if(!(dir.exists(file.path(output_folder, curstartPatternContainer[pattern])))) {dir.create(file.path(output_folder, curstartPatternContainer[pattern]))}

  maleBias <- list.files(heatmap_sourceFolder)[which(sapply(list.files(heatmap_sourceFolder), function(x) (inheritance %in% str_split(x, "_")[[1]][4] && whichBias[1] %in% str_split(x, "_")[[1]][5])))]
  femsBias <- list.files(heatmap_sourceFolder)[which(sapply(list.files(heatmap_sourceFolder), function(x) (inheritance %in% str_split(x, "_")[[1]][4] && whichBias[2] %in% str_split(x, "_")[[1]][5])))]
  
  for (metSxPop in 1:8) {
    stackOne <- CombineSingles(inheritance, 1, metSxPop, pattern)
    stackTwo <- CombineSingles(inheritance, 2, metSxPop, pattern)
    # stackOne <- image_read(file.path(heatmap_sourceFolder, maleBias, curstartPatternContainer[pattern]), paste0(SxMtPopContainer[metSxPop], "_", whichPopBias[1], ".png"))
    # stackTwo <- image_read(file.path(heatmap_sourceFolder, femsBias, curstartPatternContainer[pattern]), paste0(SxMtPopContainer[metSxPop], "_", whichPopBias[2], ".png"))
    thing <- image_append(c(stackOne, stackTwo), stack = TRUE)
    image_write(thing, path = file.path(output_folder, curstartPatternContainer[pattern]))
  }

  # stackOne <- image_read(file.path(heatmap_sourceFolder, maleBias, curstartPatternContainer[pattern]),)
  # stackTwo <- image_read(file.path(heatmap_sourceFolder, femsBias, curstartPatternContainer[pattern]),)


}





for (inhPattern in 1:4) {
  IndividualFigures(inhPattern, 2)

  for (SPranges in 1:2) {
    stackMultiples(inhPattern, SPranges)
  }
}

