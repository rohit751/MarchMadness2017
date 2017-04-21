#***************************************************
# Clean the environment ####
#***************************************************
rm(list = ls())

#***************************************************
# Load the libraries ####
#***************************************************
library(data.table)
library(dplyr)
library(stringr)
library(PlayerRatings)
source("functions.R")

#***************************************************
## Load the data ####
#***************************************************
tourneyDetailed = fread("TourneyDetailedResults.csv")
regularDetailed = fread("RegularSeasonDetailedResults.csv")
tourneySeeds = fread("TourneySeeds.csv")
sampleSubmission = fread("SampleSubmission.csv")

#***************************************************
# Seed Prediction ####
#***************************************************
# Kaggle Stage 1
# finalSeedPredictions = seedPrediction(tourneySeeds,
#                                       sample = sampleSubmission)
# Kaggle Stage 2
# seedPrediction() can be found in functions.R
finalSeedPredictions = seedPrediction(tourneySeeds,
                                      sampleSubmission,
                                      seasons = 2017)
finalSeedPredictions$Pred = as.numeric(as.character
                                       (finalSeedPredictions$Pred))

#***************************************************
# Elo Ratings - Predictions ####
#***************************************************
seasons = seq(2013, 2017)

regularSeasonRating = lapply(seasons, FUN = eloRating, regularDetailed)
names(regularSeasonRating) = seasons

# eloRating() can be found in functions.R
AllRegSeasonRating = eloRating(data = regularDetailed)
AllTouSeasonRating = eloRating(data = tourneyDetailed)

#***************************************************
# Extract matches to predict ####
#***************************************************
#stage1 = extractMatches(sampleSubmission)
# extractMatches() can be found in functions.R
stage2 = extractMatches(sampleSubmission)

# Check teams that are playing NCAA 2017 for the first time
index = which((unique(c(stage2$team1, stage2$team2))) %in% 
                  AllTouSeasonRating$Player)
newPlayers = (unique(c(stage2$team1, stage2$team2)))[-index]
NewPlayersReg = regularSeasonRating[["2017"]]
regRatNewPlayers = NewPlayersReg[NewPlayersReg$Player %in% newPlayers ,]
AllTouSeasonRating = rbind(AllTouSeasonRating, regRatNewPlayers)
AllTouSeasonRating = arrange(AllTouSeasonRating, desc(Rating))

#***************************************************
# Team Rating difference Calculation ####
#***************************************************
# Stage 1
# regEloMatchRatings = apply(stage1, 1, FUN = ratingDifference,
#                            regularSeasonRating)
# allRegMatchRatings = apply(stage1, 1, FUN = ratingDifference,
#                            AllRegSeasonRating, all = T)
# allTouMatchRatings = apply(stage1, 1, FUN = ratingDifference,
#                            AllTouSeasonRating, all = T)

# Stage 2
regEloMatchRatings = as.numeric(apply(stage2, 1, FUN = ratingDifference,
                           regularSeasonRating))
allRegMatchRatings = as.numeric(apply(stage2, 1, FUN = ratingDifference,
                           AllRegSeasonRating, all = T))
allTouMatchRatings = as.numeric(apply(stage2, 1, FUN = ratingDifference,
                           AllTouSeasonRating, all = T))




#***************************************************
# Convert these into probabilities
#***************************************************
# elo2Prob() can be found in functions.R
regEloMatchRatings = elo2Prob(regEloMatchRatings, 400)
allRegMatchRatings = elo2Prob(allRegMatchRatings, 400)
allTouMatchRatings = elo2Prob(allTouMatchRatings, 400)

finalRegEloPreds = as.data.frame(cbind(Id = sampleSubmission$Id,
                                       Pred = regEloMatchRatings))
finalAllRegEloPreds = as.data.frame(cbind(Id = sampleSubmission$Id,
                                       Pred = allRegMatchRatings))
finalAllTouEloPreds = as.data.frame(cbind(Id = sampleSubmission$Id,
                                       Pred = allTouMatchRatings))

finalRegEloPreds$Pred = as.numeric(as.character(finalRegEloPreds$Pred))
finalAllRegEloPreds$Pred = as.numeric(as.character
                                      (finalAllRegEloPreds$Pred))
finalAllTouEloPreds$Pred = as.numeric(as.character
                                      (finalAllTouEloPreds$Pred))

#***************************************************
# Evaluate all our predictions - Stage 1 ####
#***************************************************
# evaluate() can be found in functions.R
# evaluate(finalSeedPredictions)
# evaluate(finalRegEloPreds)
# evaluate(finalAllRegEloPreds)
# evaluate(finalAllTouEloPreds)

#*********************************************************
# Weighted Average Prediction Probabilities ####
#*********************************************************
# score = 1
# weightage = 0
# pred1 = finalAllTouEloPreds
# pred2 = finalSeedPredictions
# for (i in seq(0, 1, 0.01)){
#     final = wtAvgWithSeed(pred1, pred2, i)
#     if (score > evaluate(final)){
#         score = evaluate(final)
#         weightage = i
#     }
# }
# weightage
# score

# Best Score - Stage 1
# wtAvg() can be found in functions.R
# Avg_TouElo_Seed = wtAvg(finalAllTouEloPreds,
#                     finalSeedPredictions,
#                     # Weight of first arg
#                     0.82)
# evaluate(Avg_TouElo_Seed) # 0.513708

#*********************************************************
# Ensemble ####
#*********************************************************
glimpse(finalRegEloPreds)
glimpse(finalAllRegEloPreds)
glimpse(finalAllTouEloPreds)
names(finalAllRegEloPreds)[2] = "Pred_AllRegElo"
names(finalAllTouEloPreds)[2] = "Pred_AllTouElo"

temp = merge(finalRegEloPreds, finalAllRegEloPreds)
temp = merge(temp, finalAllTouEloPreds)

sameResult = (temp$Pred >= 0.5 &
                  temp$Pred_AllRegElo >= 0.5 &
                  temp$Pred_AllTouElo >= 0.5) |
                (temp$Pred < 0.5 &
                     temp$Pred_AllRegElo < 0.5 &
                     temp$Pred_AllTouElo < 0.5)
temp$sameResult = sameResult
temp = temp %>% mutate(new = ifelse(sameResult, Pred,
                                    0.75*(Pred) +
                                        0.2*(Pred_AllTouElo) +
                                        0.05*(Pred_AllRegElo)))
finalPreds = select(temp, Id, new)
names(finalPreds)[2] = "Pred"

#*********************************************************
# Write a submission ####
#*********************************************************
write.csv(finalPreds, "FinalPredictions.csv", row.names = F)
