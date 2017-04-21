# Load the libraries
require(data.table)
require(dplyr)

#****************************************************************
# Function to calcuate predictions based on Tournament Seeds ####
#****************************************************************
seedPrediction = function(tourneySeeds, sample,
                          seasons = seq(2013, 2016),
                          seedWeightage = 0.03) {
    
    # Select appropriate data
    tourneySeeds = tourneySeeds %>% mutate(Division = substr(Seed, 1, 1)) %>% 
        mutate(Seed = substr(Seed, 2, 3)) %>% filter(Season %in% seasons)
    
    # Seasonal Matches Extraction to Predict
    matches2Predict = extractMatches(sample)
    
    # Seed Based Probabilities calculation
    seedPred = function(matches) {
        season = matches["season"]
        team1 = matches["team1"]
        team2 = matches["team2"]
        
        seasonalSeeds = tourneySeeds[tourneySeeds$Season == season ,]
        seed1 = as.numeric(seasonalSeeds[seasonalSeeds$Team == team1, "Seed"])
        seed2 = as.numeric(seasonalSeeds[seasonalSeeds$Team == team2, "Seed"])
        
        seedPred = 0.5 + (seed2 - seed1) * seedWeightage
    }
    
    predictions = as.numeric(apply(matches2Predict, 1, FUN = seedPred))
    
    seedPredictions = as.data.frame(cbind(Id = sample$Id,
                                          Pred = predictions))
    
    return(seedPredictions)
}

#*****************************************************
# Calculate Elo Rating for all teams in the data. ####
#*****************************************************
eloRating = function(seasons = NULL, data = data) {
    
    if (is.null(seasons))
    {seasonData = data[, -1]}
    else
    {seasonData = data[Season == seasons, -1]}
    
    result = rep(1, nrow(seasonData))
    Wadv = seasonData$Wscore + seasonData$Wor + seasonData$Wdr +
        seasonData$Wstl - seasonData$Wpf
    
    Ladv = seasonData$Lscore + seasonData$Lor + seasonData$Ldr +
        seasonData$Lstl - seasonData$Lpf
    
    Wadv1 = Wadv / (Wadv + Ladv)
    Ladv1 = Ladv / (Wadv + Ladv)
    
    Wadv = ifelse(is.na(seasonData$Wfgm / seasonData$Wfga), 0, (seasonData$Wfgm / seasonData$Wfga)) +
        ifelse(is.na(seasonData$Wfgm3 / seasonData$Wfga3), 0, (seasonData$Wfgm3 / seasonData$Wfga3)) +
        ifelse(is.na(seasonData$Wftm / seasonData$Wfta), 0, (seasonData$Wftm / seasonData$Wfta))
    Wast_to = ifelse(is.na(seasonData$Wast / seasonData$Wto), 0, 
                     ifelse(seasonData$Wast / seasonData$Wto == Inf,
                            seasonData$Wast,
                            seasonData$Wast / seasonData$Wto))
    Wadv = Wadv + Wast_to
    
    Ladv = ifelse(is.na(seasonData$Lfgm / seasonData$Lfga), 0, (seasonData$Lfgm / seasonData$Lfga)) +
        ifelse(is.na(seasonData$Lfgm3 / seasonData$Lfga3), 0, (seasonData$Lfgm3 / seasonData$Lfga3)) +
        ifelse(is.na(seasonData$Lftm / seasonData$Lfta), 0, (seasonData$Lftm / seasonData$Lfta))
    Last_to = ifelse(is.na(seasonData$Last / seasonData$Lto), 0, 
                     ifelse(seasonData$Last / seasonData$Lto == Inf,
                            seasonData$Last,
                            seasonData$Last / seasonData$Lto))
    Ladv = Ladv + Last_to
    
    Wadv2 = Wadv / (Wadv + Ladv)
    Ladv2 = Ladv / (Wadv + Ladv)
    
    advantage = (Wadv1 + Wadv2 - Ladv1 - Ladv2) *
        ifelse(seasonData$Wloc == "H", 0.75,
               ifelse(seasonData$Wloc == "N", 0.5, 0.25))
    
    eloSeasonData = data.frame(seasonData$Daynum,
                               seasonData$Wteam,
                               seasonData$Lteam,
                               result)
    eloSeasonRating = elo(eloSeasonData, init = 2200, gamma = advantage)
    eloSeasonRating = eloSeasonRating$ratings
}

#*******************************************************************
# Function to extract all possible matches for the given season ####
#*******************************************************************
extractMatches = function(sampleSubmission) {
    
    matches2Predict = function(id) {
        matches = as.numeric(strsplit(id, "_")[[1]])
    }
    
    seaonsalMatches = lapply(sampleSubmission$Id, FUN = matches2Predict)
    seasonMatches2Predict = as.data.table(do.call(rbind, seaonsalMatches))
    names(seasonMatches2Predict) = c("season", "team1", "team2")
    
    return (seasonMatches2Predict)
}

#*************************************************************
# Function to calculate rating difference between 2 teams ####
#*************************************************************
ratingDifference = function(matches, ratings, all = F) {
    
    if (!all)
    {
        season = matches["season"]
        ratings = ratings[[as.character(season)]]
    }
    team1 = matches["team1"]
    team2 = matches["team2"]
    
    rating1 = as.numeric(ratings[ratings$Player == team1, "Rating"])
    rating2 = as.numeric(ratings[ratings$Player == team2, "Rating"])
    return (rating1 - rating2)
}

#*******************************************
# Elo Ratings to Probability Conversion ####
#*******************************************
elo2Prob = function(elo, cValue){
    probabilityOfVictory = 1 / (1 + 10 ^ (-(elo)/cValue))
    return(probabilityOfVictory)
}

#*******************************************************************
# Function to evaluate the final prediction and produce a score ####
#*******************************************************************
evaluate = function(finalPredictions){
    
    # Load necessary libraries
    library(data.table)
    library(dplyr)
    
    # Load the data
    tourneyCompact = fread("TourneyCompactResults.csv")
    
    # Filter data for seasons: 2013, 2014, 2015 & 2016
    seasons2Test = seq(2013, 2016)
    tourneyCompact = filter(tourneyCompact, Season %in% seasons2Test)
    
    # Remove the Play-In ("First Four") games
    tourneyCompact = filter(tourneyCompact, Daynum > 135)
    
    # Select only relavent columns
    tourneyCompact = tourneyCompact[, c(1, 3, 5)]
    tourneyCompact$Result = 1
    names(tourneyCompact)[c(2:3)] = c("Team1", "Team2")
    
    # Swap the teams and change the result if the winning team number is 
    # greater than the losing team number
    tourneyCompact$Result = as.numeric(!(tourneyCompact$Team1 > tourneyCompact$Team2))
    tourneyCompact$temp = 0
    tourneyCompact$temp[tourneyCompact$Result == 0] = tourneyCompact$Team1[tourneyCompact$Result == 0]
    tourneyCompact$Team1[tourneyCompact$Result == 0] = tourneyCompact$Team2[tourneyCompact$Result == 0]
    tourneyCompact$Team2[tourneyCompact$Result == 0] = tourneyCompact$temp[tourneyCompact$Result == 0]
    tourneyCompact = tourneyCompact[, -5]
    
    # Create a dataset with "id" and "Result" variables
    actualResults = tourneyCompact %>% mutate(Id = paste(Season, Team1, Team2, sep = "_"))
    actualResults = select(actualResults, Id, Result)
    
    # Merge our predictions with actual results to form a "final" dataset
    finalPredictions$Id = as.character(finalPredictions$Id)
    finalPredictions$Pred = as.numeric(as.character(finalPredictions$Pred))
    final = merge(actualResults, finalPredictions)
    
    # Calculate the log loss for each match
    final = final %>% mutate(predProb = (Result*log(Pred) + (1-Result)*log(1-Pred)))
    
    # Calculate our final score
    score = -sum(final$predProb)/nrow(final)
    return (score)
}

#*********************************************************************
# Cal. wt. avg. score for a particular prediction with Seed Pred. ####
#*********************************************************************
wtAvg = function(finalPredictions,
                 finalSeedPredictions,
                 predWeightage) {
    
    names(finalPredictions)[2] = "Pred2"
    
    # Merge both Predictions
    finalAvgPredictions = merge(finalSeedPredictions, finalPredictions)
    finalAvgPredictions$Pred = as.numeric(as.character(finalAvgPredictions$Pred))
    finalAvgPredictions$Pred2 = as.numeric(as.character(finalAvgPredictions$Pred2))
    
    # Calculate weighted average
    finalAvgPredictions = finalAvgPredictions %>%
        mutate(avg_prob = (Pred*(1-predWeightage) + Pred2*predWeightage))
    finalAvgPredictions = finalAvgPredictions[, c(1, 4)]
    names(finalAvgPredictions)[2] = "Pred"
    
    return(finalAvgPredictions)
}
