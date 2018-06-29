library(dplyr)
library(ggplot2)

LibData <- read.csv("C:/Users/murphyd/Documents/R/SimilarLib/PLS_FY2016_AE_pupld16a.csv", stringsAsFactors = FALSE)

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
LibData$TOTSTAFFscore <- (LibData$TOTSTAFFscore - minSc) / (maxSc - minSc) 

# Total Income Score

LibData$TOTINCM <- ifelse(LibData$TOTINCM < 2,100,LibData$TOTINCM) # Moves 0s to 100 (log(0) = -Inf)

LibData$TOTINCMscore <- log(LibData$TOTINCM)
minSc <-  min(LibData$TOTINCMscore)
maxSc <-  max(LibData$TOTINCMscore)
LibData$TOTINCMscore <- (LibData$TOTINCMscore - minSc) / (maxSc - minSc) 

# Need to add source of money

LibData %>% ggplot(aes(log(HRS_OPEN))) + geom_density()
LibData %>% ggplot(aes(log(TOTCIR))) + geom_density()
LibData %>% ggplot(aes(log(KIDCIRCL))) + geom_density()
LibData %>% ggplot(aes(log(TOTPRO))) + geom_density()
LibData %>% ggplot(aes(log(KIDPRO))) + geom_density()

summary(LibData$TOTSTAFF)