library(dplyr)
library(ggplot2)
library(tidyr)

LibData <- read.csv("PLS_FY2016_AE_pupld16a.csv", stringsAsFactors = FALSE)
ScoreFactorA <- 10000
ScoreFactorB <- 1
ScoreFactorC <- 100


# Giving scores for membership in a library cooperative 

lut <- c("HQ" = 1,
         "ME" = .5,
         "NO" = 0)

LibData$C_RELATNscore <- lut[LibData$C_RELATN] * ScoreFactorB

# Normalizing Population Service Area

LibData <- LibData %>% filter(POPU_LSA != -3)  #removes entities that are temp. closed

LibData$POPU_LSAscore <- log(LibData$POPU_LSA)
minSc <-  min(LibData$POPU_LSAscore)
maxSc <-  max(LibData$POPU_LSAscore)
LibData$POPU_LSAscore <- (LibData$POPU_LSAscore - minSc) * ScoreFactorA / (maxSc - minSc) 

# Giving scores for number of branch libraries

LibData$BRANLIBscore <- ifelse(LibData$BRANLIB == 0, 0,
                               ifelse(LibData$BRANLIB <10,.5,1)) * ScoreFactorB

# Has a bookmobile score

LibData$BKMOBscore <- ifelse(LibData$BKMOB == 0,0,1) * ScoreFactorB

# Total staff score

LibData$TOTSTAFF <- ifelse(LibData$TOTSTAFF == 0,0.03, LibData$TOTSTAFF) # Moves 0s to 0.03 which is next lowest (log(0) = -Inf)
LibData$TOTSTAFFscore <- log(LibData$TOTSTAFF)
minSc <-  min(LibData$TOTSTAFFscore)
maxSc <-  max(LibData$TOTSTAFFscore)
LibData$TOTSTAFFscore <- (LibData$TOTSTAFFscore - minSc) * ScoreFactorA / (maxSc - minSc) 

# Total Income Score

LibData$TOTINCM <- ifelse(LibData$TOTINCM < 2,100,LibData$TOTINCM) # Moves 0s to 100 (log(0) = -Inf)

LibData$TOTINCMscore <- log(LibData$TOTINCM)
minSc <-  min(LibData$TOTINCMscore)
maxSc <-  max(LibData$TOTINCMscore)
LibData$TOTINCMscore <- (LibData$TOTINCMscore - minSc) * ScoreFactorA / (maxSc - minSc) 

# Source of Money

LibData$LocIncScore <- LibData$LOCGVT * ScoreFactorC / LibData$TOTINCM

# Hours open

LibData$HRS_OPEN <- ifelse(LibData$HRS_OPEN==0,7,LibData$HRS_OPEN)

LibData$HRS_OPENscore <- log(LibData$HRS_OPEN)
minSc <-  min(LibData$HRS_OPENscore)
maxSc <-  max(LibData$HRS_OPENscore)
LibData$HRS_OPENscore <- (LibData$HRS_OPENscore - minSc) * ScoreFactorA / (maxSc - minSc) 

# Total Circulation

LibData$TOTCIR <- ifelse(LibData$TOTCIR == -1, LibData$KIDCIRCL, LibData$TOTCIR) # replace missing values with children's circ count
LibData$TOTCIR <- ifelse(LibData$TOTCIR == 0, 10, LibData$TOTCIR) # replace 0 values with small value


LibData$TOTCIRscore <- log(LibData$TOTCIR)
minSc <-  min(LibData$TOTCIRscore)
maxSc <-  max(LibData$TOTCIRscore)
LibData$TOTCIRscore <- (LibData$TOTCIRscore - minSc) * ScoreFactorA / (maxSc - minSc) 

# Share of children's circulation

LibData$CircChildscore <- LibData$KIDCIRCL * ScoreFactorC / LibData$TOTCIR

# Number of Programs

LibData$TOTPRO <- ifelse(LibData$TOTPRO == 0,.5, LibData$TOTPRO) # replace 0s with another small number

LibData$TOTPROscore <- log(LibData$TOTPRO)
minSc <-  min(LibData$TOTPROscore)
maxSc <-  max(LibData$TOTPROscore)
LibData$TOTPROscore <- (LibData$TOTPROscore - minSc) * ScoreFactorC / (maxSc - minSc) 

#  Share of children's program

LibData$ChildProgScore <- LibData$KIDPRO * ScoreFactorC / LibData$TOTPRO

# Score for employees

LibData$TOTSTAFFscore <- log(LibData$TOTSTAFF)
minSc <-  min(LibData$TOTSTAFFscore)
maxSc <-  max(LibData$TOTSTAFFscore)
LibData$TOTSTAFFscore <- (LibData$TOTSTAFFscore - minSc) * ScoreFactorC / (maxSc - minSc) 

LibData <- LibData %>% tibble::rownames_to_column()



Distances <- LibData %>% select(contains("score"))
Distances$SCORE <- NULL

Distances <- as.data.frame(as.matrix(dist(rbind(Distances))))
Distances <- Distances %>% tibble::rownames_to_column()

LibData %>% filter(LIBNAME == "WEST ISLIP PUBLIC LIBRARY") %>% select("rowname")

temp <- Distances %>% mutate(rank = row_number(`6316`)) %>% select(`6316`, rowname, rank) %>% filter (rank < 12 ) %>% select(rowname)

View(LibData %>% filter(rowname %in% temp$rowname) %>% select(1:5,C_RELATN, POPU_LSA, BRANLIB, BKMOB, TOTSTAFF, TOTINCM, LOCGVT,
                                                              HRS_OPEN, TOTCIR, KIDCIRCL, TOTPRO, KIDPRO, 153:164))
