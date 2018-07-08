library(dplyr)
library(ggplot2)
library(tidyr)

LibData <- read.csv("PLS_FY2016_AE_pupld16a.csv", stringsAsFactors = FALSE)


# Giving scores for membership in a library cooperative 

lut <- c("HQ" = 1,
         "ME" = .5,
         "NO" = 0)

LibData$C_RELATNscore <- lut[LibData$C_RELATN]

# Normalizing Population Service Area

LibData <- LibData %>% filter(POPU_LSA != -3)  #removes entities that are temp. closed

LibData$POPU_LSAscore <- log(LibData$POPU_LSA)
minSc <-  min(LibData$POPU_LSAscore)
maxSc <-  max(LibData$POPU_LSAscore)
LibData$POPU_LSAscore <- (LibData$POPU_LSAscore - minSc) / (maxSc - minSc) 

# Giving scores for number of branch libraries

LibData$BRANLIBscore <- ifelse(LibData$BRANLIB == 0, 0,
                               ifelse(LibData$BRANLIB <10,.5,1)) 

# Has a bookmobile score

LibData$BKMOBscore <- ifelse(LibData$BKMOB == 0,0,1) 

# Total staff score

LibData$TOTSTAFF <- ifelse(LibData$TOTSTAFF == 0,0.03, LibData$TOTSTAFF) # Moves 0s to 0.03 which is next lowest (log(0) = -Inf)
LibData$TOTSTAFFscore <- log(LibData$TOTSTAFF)
minSc <-  min(LibData$TOTSTAFFscore)
maxSc <-  max(LibData$TOTSTAFFscore)
LibData$TOTSTAFFscore <- (LibData$TOTSTAFFscore - minSc)  / (maxSc - minSc) 

# Total Income Score

LibData$TOTINCM <- ifelse(LibData$TOTINCM < 2,100,LibData$TOTINCM) # Moves 0s to 100 (log(0) = -Inf)

LibData$TOTINCMscore <- log(LibData$TOTINCM)
minSc <-  min(LibData$TOTINCMscore)
maxSc <-  max(LibData$TOTINCMscore)
LibData$TOTINCMscore <- (LibData$TOTINCMscore - minSc) / (maxSc - minSc) 

# Source of Money

LibData$LocIncScore <- LibData$LOCGVT / LibData$TOTINCM

# Hours open

LibData$HRS_OPEN <- ifelse(LibData$HRS_OPEN==0,7,LibData$HRS_OPEN)

LibData$HRS_OPENscore <- log(LibData$HRS_OPEN)
minSc <-  min(LibData$HRS_OPENscore)
maxSc <-  max(LibData$HRS_OPENscore)
LibData$HRS_OPENscore <- (LibData$HRS_OPENscore - minSc) / (maxSc - minSc) 

# Total Circulation

LibData$TOTCIR <- ifelse(LibData$TOTCIR == -1, LibData$KIDCIRCL, LibData$TOTCIR) # replace missing values with children's circ count
LibData$TOTCIR <- ifelse(LibData$TOTCIR == 0, 10, LibData$TOTCIR) # replace 0 values with small value


LibData$TOTCIRscore <- log(LibData$TOTCIR)
minSc <-  min(LibData$TOTCIRscore)
maxSc <-  max(LibData$TOTCIRscore)
LibData$TOTCIRscore <- (LibData$TOTCIRscore - minSc)  / (maxSc - minSc) 

# Share of children's circulation

LibData$CircChildscore <- LibData$KIDCIRCL / LibData$TOTCIR

# Number of Programs

LibData$TOTPRO <- ifelse(LibData$TOTPRO == 0,.5, LibData$TOTPRO) # replace 0s with another small number

LibData$TOTPROscore <- log(LibData$TOTPRO)
minSc <-  min(LibData$TOTPROscore)
maxSc <-  max(LibData$TOTPROscore)
LibData$TOTPROscore <- (LibData$TOTPROscore - minSc)  / (maxSc - minSc) 

#  Share of children's program

LibData$ChildProgScore <- LibData$KIDPRO  / LibData$TOTPRO

# Adjusting weights

ScoreFactorA <- 5
ScoreFactorB <- 10
ScoreFactorC <- 100
ScoreFactorD <- 1000


LibData$C_RELATNscore <- LibData$C_RELATNscore * ScoreFactorA
LibData$POPU_LSAscore <- LibData$POPU_LSAscore * ScoreFactorD
LibData$BRANLIBscore  <- LibData$BRANLIBscore * ScoreFactorA
LibData$BKMOBscore    <- LibData$BKMOBscore * ScoreFactorA
LibData$TOTSTAFFscore <- LibData$TOTSTAFFscore * ScoreFactorC
LibData$TOTINCMscore <- LibData$TOTINCMscore * ScoreFactorD
LibData$LocIncScore <- LibData$LocIncScore * ScoreFactorB
LibData$HRS_OPENscore <- LibData$HRS_OPENscore * ScoreFactorC
LibData$TOTCIRscore <- LibData$TOTCIRscore * ScoreFactorD
LibData$CircChildscore <- LibData$CircChildscore * ScoreFactorC
LibData$TOTPROscore <- LibData$TOTPROscore * ScoreFactorD
LibData$ChildProgScore <- LibData$ChildProgScore * ScoreFactorC



LibData <- LibData %>% tibble::rownames_to_column()



Distances <- LibData %>% select(contains("score"))
Distances$SCORE <- NULL

Distances <- as.data.frame(as.matrix(dist(rbind(Distances))))
Distances <- Distances %>% tibble::rownames_to_column()

LibData %>% filter(LIBNAME == "PIMA COUNTY PUBLIC LIBRARY") %>% select("rowname")

temp <- Distances %>% mutate(rank = row_number(`83`)) %>% select(`83`, rowname, rank) %>% filter (rank < 12 ) %>% select(rowname)

View(LibData %>% filter(rowname %in% temp$rowname) %>% select(1:5,C_RELATN, POPU_LSA, BRANLIB, BKMOB, TOTSTAFF, TOTINCM, LOCGVT,
                                                              HRS_OPEN, TOTCIR, KIDCIRCL, TOTPRO, KIDPRO, 153:164))