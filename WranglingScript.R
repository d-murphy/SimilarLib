library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)

LibData <- read.csv("PLS_FY2016_AE_pupld16a.csv", stringsAsFactors = FALSE)


# Giving scores for membership in a library cooperative 

lut <- c("HQ" = 1,
         "ME" = .5,
         "NO" = 0)

LibData$C_RELATNscore <- lut[LibData$C_RELATN]

# Normalizing Population Service Area

LibData <- LibData %>% filter(POPU_LSA != -3)  #removes entities that are temp. closed

LibData$POPU_LSAscore <- sqrt(LibData$POPU_LSA)
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
LibData$TOTSTAFFscore <-  LibData$TOTSTAFF  # log(LibData$TOTSTAFF)
minSc <-  min(LibData$TOTSTAFFscore)
maxSc <-  max(LibData$TOTSTAFFscore)
LibData$TOTSTAFFscore <- (LibData$TOTSTAFFscore - minSc)  / (maxSc - minSc) 

# Total Income Score

LibData$TOTINCM <- ifelse(LibData$TOTINCM < 2,100,LibData$TOTINCM) # Moves 0s to 100 (log(0) = -Inf)

LibData$TOTINCMscore <- sqrt(LibData$TOTINCM)
minSc <-  min(LibData$TOTINCMscore)
maxSc <-  max(LibData$TOTINCMscore)
LibData$TOTINCMscore <- (LibData$TOTINCMscore - minSc) / (maxSc - minSc) 

# Source of Money

LibData$LocIncScore <- LibData$LOCGVT / LibData$TOTINCM

# Hours open

LibData$HRS_OPEN <- ifelse(LibData$HRS_OPEN==0,7,LibData$HRS_OPEN)

LibData$HRS_OPENscore <- sqrt(LibData$HRS_OPEN)
minSc <-  min(LibData$HRS_OPENscore)
maxSc <-  max(LibData$HRS_OPENscore)
LibData$HRS_OPENscore <- (LibData$HRS_OPENscore - minSc) / (maxSc - minSc) 

# Total Circulation

LibData$TOTCIR <- ifelse(LibData$TOTCIR == -1, LibData$KIDCIRCL, LibData$TOTCIR) # replace missing values with children's circ count
LibData$TOTCIR <- ifelse(LibData$TOTCIR == 0, 10, LibData$TOTCIR) # replace 0 values with small value


LibData$TOTCIRscore <- sqrt(LibData$TOTCIR)
minSc <-  min(LibData$TOTCIRscore)
maxSc <-  max(LibData$TOTCIRscore)
LibData$TOTCIRscore <- (LibData$TOTCIRscore - minSc)  / (maxSc - minSc) 

# Share of children's circulation

LibData$CircChildscore <- LibData$KIDCIRCL / LibData$TOTCIR

# Number of Programs

LibData$TOTPRO <- ifelse(LibData$TOTPRO == 0,.5, LibData$TOTPRO) # replace 0s with another small number

LibData$TOTPROscore <- sqrt(LibData$TOTPRO)
minSc <-  min(LibData$TOTPROscore)
maxSc <-  max(LibData$TOTPROscore)
LibData$TOTPROscore <- (LibData$TOTPROscore - minSc)  / (maxSc - minSc) 

#  Share of children's program

LibData$ChildProgScore <- LibData$KIDPRO  / LibData$TOTPRO

# Adjusting weights

ScoreFactorA <- 5
ScoreFactorB <- 50
ScoreFactorC <- 100
ScoreFactorD <- 1000


# LibData$C_RELATNscore <- LibData$C_RELATNscore * ScoreFactorB
# LibData$POPU_LSAscore <- LibData$POPU_LSAscore * ScoreFactorD
# LibData$BRANLIBscore  <- LibData$BRANLIBscore * ScoreFactorB
# LibData$BKMOBscore    <- LibData$BKMOBscore * ScoreFactorB
# LibData$TOTSTAFFscore <- LibData$TOTSTAFFscore * ScoreFactorC
# LibData$TOTINCMscore <- LibData$TOTINCMscore * ScoreFactorD
# LibData$LocIncScore <- LibData$LocIncScore * ScoreFactorC
# LibData$HRS_OPENscore <- LibData$HRS_OPENscore * ScoreFactorC
# LibData$TOTCIRscore <- LibData$TOTCIRscore * ScoreFactorD
# LibData$CircChildscore <- LibData$CircChildscore * ScoreFactorC
# LibData$TOTPROscore <- LibData$TOTPROscore * ScoreFactorD
# LibData$ChildProgScore <- LibData$ChildProgScore * ScoreFactorC

LibData$C_RELATNscore <- LibData$C_RELATNscore * ScoreFactorC
LibData$POPU_LSAscore <- LibData$POPU_LSAscore * ScoreFactorD
LibData$BRANLIBscore  <- LibData$BRANLIBscore * ScoreFactorB
LibData$BKMOBscore    <- LibData$BKMOBscore * ScoreFactorB
LibData$TOTSTAFFscore <- LibData$TOTSTAFFscore * ScoreFactorD
LibData$TOTINCMscore <- LibData$TOTINCMscore * ScoreFactorD
LibData$LocIncScore <- LibData$LocIncScore * ScoreFactorB
LibData$HRS_OPENscore <- LibData$HRS_OPENscore * ScoreFactorD
LibData$TOTCIRscore <- LibData$TOTCIRscore * ScoreFactorD
LibData$CircChildscore <- LibData$CircChildscore * ScoreFactorB
LibData$TOTPROscore <- LibData$TOTPROscore * ScoreFactorD
LibData$ChildProgScore <- LibData$ChildProgScore * ScoreFactorB



LibData <- LibData %>% tibble::rownames_to_column()



Distances <- LibData %>% select(contains("score"))
Distances$SCORE <- NULL

Distances <- as.data.frame(as.matrix(dist(rbind(Distances))))
Distances <- Distances %>% tibble::rownames_to_column()

Distances <- Distances %>% gather(rownameOf2ndLib, distance, -rowname)
Distances <- Distances %>% 
  filter(rowname != rownameOf2ndLib) %>%
  group_by(rowname) %>% 
  mutate(rank = rank(distance, ties.method = "first")) %>% 
  filter(rank <= 25) %>%
  ungroup()

Distances$rowname <- as.numeric(Distances$rowname)
Distances$rownameOf2ndLib <- as.numeric(Distances$rownameOf2ndLib)

Distances <- Distances %>% select(rowname, rownameOf2ndLib, rank)



## Select data to display in shiny app


LibDataDisplay <- LibData %>% select(rowname, STABR, LIBNAME, ADDRESS, CITY, ZIP, PHONE, LONGITUD, LATITUDE,
                                     C_RELATN,POPU_LSA, BRANLIB, BKMOB, TOTSTAFF, TOTINCM, 
                                     LOCGVT, HRS_OPEN, TOTCIR, KIDCIRCL, TOTPRO, KIDPRO )

LibDataDisplay$rowname <- as.numeric(LibDataDisplay$rowname)
LibDataDisplay$LIBNAME <- str_to_title(LibDataDisplay$LIBNAME)
LibDataDisplay$ADDRESS <- str_to_title(LibDataDisplay$ADDRESS)
LibDataDisplay$CITY <- str_to_title(LibDataDisplay$CITY)

LibDataDisplay$C_RELATN <- ifelse(LibDataDisplay$C_RELATN == "HQ", "Headquarters", 
                                  ifelse(LibDataDisplay$C_RELATN == "ME", "Member", "No"))


LibDataDisplay$`Local Gvt Funding Percentage` <- percent(LibDataDisplay$LOCGVT / LibDataDisplay$TOTINCM )
LibDataDisplay$`Children's Circulation Share of Total` <- percent(LibDataDisplay$KIDCIRCL / LibDataDisplay$TOTCIR )
LibDataDisplay$`Children's Programming Share of Total` <- percent(LibDataDisplay$KIDPRO / LibDataDisplay$TOTPRO )

colnames(LibDataDisplay) <- c("rowname", "State", "Library Name", "Address", "City", "Zip Code", "Phone Number", "Longitude", "Latitude",
                              "Belongs to a Cooperative", "Population of Legal Service Area", "# of Branch Libraries", "# of Bookmobiles", 
                              "Total Staff Count", "Total Income", "LOCGVT", "Hours Open", "Total Circulation", "KIDCIRCL", 
                              "Total Programs", "KIDPRO", "Local Gvt Funding Percentage", "Children's Circulation Share of Total",
                              "Children's Programming Share of Total")

LibDataDisplay$PopupText <- (paste0("<b>",LibDataDisplay$`Library Name`,"</b>", 
                                    "</br></br>",
                                    LibDataDisplay$Address, "</br>",
                                    LibDataDisplay$City, ", ", LibDataDisplay$State,
                                    "</br></br>", 
                                    substring(LibDataDisplay$`Phone Number`,1,3),"-",
                                    substring(LibDataDisplay$`Phone Number`,4,6),"-",
                                    substring(LibDataDisplay$`Phone Number`,7,10)))

LibDataDisplay <- LibDataDisplay %>% group_by(State, `Library Name`) %>%
                    mutate(NameCt = row_number()) %>% 
                    ungroup()

LibDataDisplay$`Library Name` <- ifelse(LibDataDisplay$NameCt == 2,
                                        paste0(LibDataDisplay$`Library Name`, " 2"),
                                        LibDataDisplay$`Library Name`)


LibNames <- LibDataDisplay %>% select(`Library Name`, rowname, State) %>% arrange(`Library Name`)




#remove(LibData, ScoreFactorA, ScoreFactorB, ScoreFactorC, ScoreFactorD, maxSc, minSc, lut, temp)

# 
# LibData %>% filter(LIBNAME == "PIMA COUNTY PUBLIC LIBRARY") %>% select("rowname")
# 
# temp <- Distances %>% mutate(rank = row_number(`83`)) %>% select(`83`, rowname, rank) %>% filter (rank < 12 ) %>% select(rowname)
# 
# View(LibData %>% filter(rowname %in% temp$rowname) %>% select(1:5,C_RELATN, POPU_LSA, BRANLIB, BKMOB, TOTSTAFF, TOTINCM, LOCGVT,
#                                                               HRS_OPEN, TOTCIR, KIDCIRCL, TOTPRO, KIDPRO, 153:164))