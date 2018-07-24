# ===================================================================
# Title: comparison.R
# Description:
#   This script takes in boxscore data, and puts out average
#   statistics report for each team
# Input: 2017-NBA-BOXSCORE.csv
# Output: teamname.csv
# Author: Yoon Sung Hong
# Date: 11-15-2017
# ===================================================================
#
rm(list=ls())
library(dplyr)

rootPath <- "xxxxxxxxxxxx"
savePath <- "xxxxxxxxxxxx"
#dir.create(savePath)

nbaPath <- "xxxxxxxxxxxxx"                  # Where NBA data is

#===== Reading files from directory ======================================
box <- read.csv(paste0(nbaPath, "2017-NBA-BOXSCORE.csv"),stringsAsFactors = F)
box_2 <- read.csv(paste0(nbaPath, "2017-NBA-BOXSCORE.csv"),stringsAsFactors = F)
box_bench <- read.csv(paste0(nbaPath, "2017-NBA-BOXSCORE.csv"),stringsAsFactors = F)
game_no <- box$Game.Number
start_col <- which(colnames(box)=="Game.Msg_boxscore.Team_stats.Team_id")
end_col <- which(colnames(box)=="Game.Msg_boxscore.Team_stats.SecondChancePointsAttempted")
box <- box[,start_col:end_col]
colnames(box) <- c( "team_id", 
                             "team_city", 
                             "team_abr", 
                             "team_name", 
                             "minutes", 
                             "fg_made", 
                             "fg_attempted", 
                             "ft_made", 
                             "ft_attempted", 
                             "three_made", 
                             "three_attempted", 
                             "offensive_rebounds", 
                             "defensive_rebounds", 
                             "team_rebounds", 
                             "assists", 
                             "fouls", 
                             "team_fouls", 
                             "steals", 
                             "turnovers", 
                             "team_turnovers", 
                             "blocks", 
                             "points", 
                             "flagrant_fouls", 
                             "technical_fouls", 
                             "ejections", 
                             "blocks_agsinst", 
                             "full_timeouts_remaining", 
                             "short_timeouts_remaining", 
                             "pointsoffturnovers", 
                             "unanswered_points", 
                             "longest_run", 
                             "total_turnovers", 
                             "fastbreak_points", 
                             "fastbreak_points_made", 
                             "fastbreak_points_attempted", 
                             "points_in_the_paint", 
                             "points_in_the_paint_made", 
                             "points_in_the_paint_attempted", 
                             "second_chance_points", 
                             "second_chance_points_made", 
                             "second_chance_points_attempted")
box$game_no <- game_no
#===========Removing the irrelevant rows
box <- box %>%
  filter(team_id != "NA")
#===========Finding out which team won the game
box$win <- rep(0, nrow(box))
for(i in 1:(nrow(box)/2)){
   if(box$points[((2*i)-1)] < box$points[2*i]){
     box$win[2*i] <- 1
     box$win[(2*i)-1] <- 0
   }
    else {
      box$win[2*i] <- 0
      box$win[(2*i)-1] <- 1
    } 
  }
box$win
#counting the number of wins and losses
teams <- as.data.frame(aggregate(box$win, by=list(Teams = box$team_abr), FUN = sum))
colnames(teams) <- c("teams", "wins")
games <- box %>%
  group_by(team_abr) %>%
  tally()
teams$games <- games$n
teams$losses <- teams$games - teams$wins
for(i in 1:30){
  if(teams$losses[i] < 0){
    teams$losses[i] <- 0 
  }
}
#=========home and away game
#It seems to be that team on the bottom is the home team in the arranged spreadsheet.
box$homewin <- rep(0, nrow(box))
box$awaywin <- rep(0, nrow(box))
for(i in 1:(nrow(box)/2)){
  if(box$win[2*i] == 1){
    box$homewin[2*i] <- 1
  }
  else{
    box$homewin[2*i] <- 0
  }
}
for(i in 1:(nrow(box)/2)){
  if(box$win[(2*i)-1] == 1){
    box$awaywin[(2*i)-1] <- 1
  }
  else{
    box$awaywin[(2*i)-1] <- 0
  }
}
hometeams <- as.data.frame(aggregate(box$homewin, by=list(Teams = box$team_abr), FUN = sum))
teams$homewins <- hometeams$x
awayteams <- as.data.frame(aggregate(box$awaywin, by=list(Teams = box$team_abr), FUN = sum))
teams$awaywins <- awayteams$x
box$home_count <- rep(c(0,1))
box$away_count <- rep(c(1,0))
hometally <- as.data.frame(aggregate(box$home_count, by=list(Teams = box$team_abr), FUN = sum))
awaytally <- as.data.frame(aggregate(box$away_count, by=list(Teams = box$team_abr), FUN = sum))
teams$homegp <- hometally$x
teams$awaygp <- awaytally$x
#working out the home and away losses
teams$homelosses <- teams$homegp - teams$homewins
teams$awaylosses <- teams$awaygp - teams$awaywins
for(i in 1:30){
  if(teams$homelosses[i] < 0){
    teams$homelosses[i] <- 0 
  }
}
for(i in 1:30){
  if(teams$awaylosses[i] < 0){
    teams$awaylosses[i] <- 0 
  }
}
#===Decided by 3 points or less
box$three_less <- rep(0, nrow(box))
for(i in 1:(nrow(box)/2)){
  if(box$points[((2*i)-1)] - box$points[2*i] <= 3 & box$points[((2*i)-1)] - box$points[2*i] > 0){
    box$three_less[2*i] <- box$points[(2*i)] - box$points[(2*i)-1]
    box$three_less[(2*i)-1] <- box$points[((2*i)-1)] - box$points[2*i]
  }
  else if(box$points[(2*i)] - box$points[(2*i)-1] <= 3 & box$points[(2*i)] - box$points[(2*i)-1] > 0){
    box$three_less[2*i] <- box$points[(2*i)] - box$points[(2*i)-1]
    box$three_less[(2*i)-1] <- box$points[((2*i)-1)] - box$points[2*i]
  }
  else{
    box$three_less[2*i] <- 0
    box$three_less[(2*i)-1] <- 0
  }
}
box$three_less_win <- rep(0, nrow(box)) #this only works right now because no teams have won AND lost a three points or less score difference game.
for(i in 1:nrow(box)) {
  if(box$three_less[i] >0){
    box$three_less_win[i] <- 1
  }
  else {
    box$three_less_win[i] <- 0
  }
}
box$three_less_loss <- rep(0, nrow(box))
for(i in 1:nrow(box)){
  if (box$three_less[i] < 0){
    box$three_less_loss[i] <- 1
  } 
  else {
    box$three_less_loss[i] <- 0
  }
}
three_less_win <- as.data.frame(aggregate(box$three_less_win, by=list(Teams = box$team_abr), FUN = sum))
teams$three_less_win <- three_less_win$x
three_less_loss <- as.data.frame(aggregate(box$three_less_loss, by=list(Teams = box$team_abr), FUN = sum))
teams$three_less_loss <- three_less_loss$x

#===Score 100-plus
box$hundred_win <- rep(0,nrow(box))
for(i in 1:nrow(box)){
  if(box$points[i]>=100 && box$win[i] == 1){
    box$hundred_win[i] <- 1
  }
  else{
    box$hundred_win[i] <- 0
  }
}
box$hundred_loss <- rep(0, nrow(box))
for(i in 1:nrow(box)){
  if(box$points[i]>=100 && box$win[i] == 0){
    box$hundred_loss[i] <- 1
  }
  else{
    box$hundred_loss[i] <- 0
  }
}
hundred_win <- as.data.frame(aggregate(box$hundred_win, by=list(Teams = box$team_abr), FUN = sum))
teams$hundred_win <- hundred_win$x
hundred_loss <- as.data.frame(aggregate(box$hundred_loss, by=list(Teams = box$team_abr), FUN = sum))
teams$hundred_loss <- hundred_loss$x
#==99 or below
box$below_win <- rep(0,nrow(box))
for(i in 1:nrow(box)){
  if(box$points[i]<=99 && box$win[i] == 1){
    box$below_win[i] <- 1
  }
  else{
    box$below_win[i] <- 0
  }
}
box$below_loss <- rep(0, nrow(box))
for(i in 1:nrow(box)){
  if(box$points[i]<=100 && box$win[i] == 0){
    box$below_loss[i] <- 1
  }
  else{
    box$below_loss[i] <- 0
  }
}
below_win <- as.data.frame(aggregate(box$below_win, by=list(Teams = box$team_abr), FUN = sum))
teams$below_win <- below_win$x
below_loss <- as.data.frame(aggregate(box$below_loss, by=list(Teams = box$team_abr), FUN = sum))
teams$below_loss <- below_loss$x
#====Opponents scoring 100 plus
box$hundred_win_opp <- rep(0,nrow(box)) #finding out if opponents scored 100+ # code doesnt work
for(i in 1:(nrow(box)/2)){
  if(box$points[2*i]>=100 & box$win[(2*i)-1] == 1){
    box$hundred_win_opp[(2*i)-1] <- 1
  }
  else if(box$points[2*i]>=100 & box$win[(2*i)-1] == 0){
    box$hundred_win_opp[(2*i)-1] <- 0
  }
  else{
    box$hundred_win_opp[(2*i)-1] <- 0
  }
}
for(i in 1:(nrow(box)/2)){
  if(box$points[(2*i)-1] >=100 & box$win[2*i] ==1){
    box$hundred_win_opp[(2*i)] <- 1
  }
  else if(box$points[(2*i)-1] >=100 & box$win[2*i] ==0){
    box$hundred_win_opp[2*i] <- 0
  }
  else{
    box$hundred_win_opp[2*i] <- 0
  }
}
box$hundred_loss_opp <- rep(0, nrow(box))
for(i in 1:(nrow(box)/2)){
  if(box$points[2*i]>=100 & box$win[(2*i)-1] == 0){
    box$hundred_loss_opp[(2*i)-1] <- 1
  }
  else if (box$points[2*i]>=100 & box$win[(2*i)-1] ==1){
    box$hundred_loss_opp[(2*i)-1] <- 0
  }
  else {
    box$hundred_loss_opp[(2*i)-1] <- 0
  }
}
for(i in 1:(nrow(box)/2)){
  if(box$points[(2*i)-1] >=100 & box$win[2*i] == 0){
    box$hundred_loss_opp[(2*i)] <- 1
  }
  else if (box$points[(2*i)-1]>=100 & box$win[2*i] == 1){
    box$hundred_loss_opp[2*i] <- 0
  }
  else{
    box$hundred_loss_opp[2*i] <- 0
  }
}
hundred_win_opp <- as.data.frame(aggregate(box$hundred_win_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$hundred_win_opp <- hundred_win_opp$x
hundred_loss_opp <- as.data.frame(aggregate(box$hundred_loss_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$hundred_loss_opp <- hundred_loss_opp$x
#====Opponents scoring less than 100
box$nine_win_opp <- rep(0,nrow(box)) #finding out if opponents scored 99- # code doesnt work
for(i in 1:(nrow(box)/2)){
  if(box$points[2*i]<=99 & box$win[(2*i)-1] == 1){
    box$nine_win_opp[(2*i)-1] <- 1
  }
  else if(box$points[2*i]<=99 & box$win[(2*i)-1] == 0){
    box$nine_win_opp[(2*i)-1] <- 0
  }
  else{
    box$nine_win_opp[(2*i)-1] <- 0
  }
}
for(i in 1:(nrow(box)/2)){
  if(box$points[(2*i)-1] <=99 & box$win[2*i] ==1){
    box$nine_win_opp[(2*i)] <- 1
  }
  else if(box$points[(2*i)-1] <=99 & box$win[2*i] ==0){
    box$nine_win_opp[2*i] <- 0
  }
  else{
    box$nine_win_opp[2*i] <- 0
  }
}
box$nine_loss_opp <- rep(0, nrow(box))
for(i in 1:(nrow(box)/2)){
  if(box$points[2*i]<=99 & box$win[(2*i)-1] == 0){
    box$nine_loss_opp[(2*i)-1] <- 1
  }
  else if (box$points[2*i]<=99 & box$win[(2*i)-1] ==1){
    box$nine_loss_opp[(2*i)-1] <- 0
  }
  else {
    box$nine_loss_opp[(2*i)-1] <- 0
  }
}
for(i in 1:(nrow(box)/2)){
  if(box$points[(2*i)-1] <=99 & box$win[2*i] == 0){
    box$nine_loss_opp[(2*i)] <- 1
  }
  else if (box$points[(2*i)-1]<=99 & box$win[2*i] == 1){
    box$nine_loss_opp[2*i] <- 0
  }
  else{
    box$nine_loss_opp[2*i] <- 0
  }
}
nine_win_opp <- as.data.frame(aggregate(box$nine_win_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$nine_win_opp <- nine_win_opp$x
nine_loss_opp <- as.data.frame(aggregate(box$nine_loss_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$nine_loss_opp <- nine_loss_opp$x
#====Outshooting opponent
box$fgp <- box$fg_made/box$fg_attempted #working out the field goal percentage
box$col_morefgp <- rep(0,nrow(box))
box$col_lessfgp <- rep(0,nrow(box))
for(i in 1:(nrow(box)/2)) {
  if(box$fgp[(2*i)-1]>box$fgp[2*i]){
    box$col_morefgp[(2*i)-1] <- 1
    box$col_lessfgp[(2*i)] <- 1
  }
  else if (box$fgp[(2*i)] > box$fgp[(2*i)-1]) {
    box$col_lessfgp[(2*i)-1] <- 1
    box$col_morefgp[(2*i)] <- 1
  }
  else if (box$fgp[(2*i)] == box$fgp[(2*i)]) {
    box$col_morefgp[2*i] <- 1
    box$col_morefgp[(2*i)-1] <- 1
  }
}
box$fgp_win <- rep(0, nrow(box))
box$fgp_loss <- rep(0, nrow(box))
box$fgp_win_opp <- rep(0, nrow(box))
box$fgp_loss_opp <- rep(0, nrow(box)) 
for(i in 1:(nrow(box))){
  if(box$col_morefgp[i] == 1 & box$win[i] == 1){
    box$fgp_win[i] <- 1
  }
  else if(box$col_lessfgp[i] == 1 & box$win[i] == 1){
    box$fgp_win_opp[(i)] <- 1
  }
  else if(box$col_morefgp[i] == 1 & box$win[i] == 0){
    box$fgp_loss[i] <- 1
  }
  else if(box$col_lessfgp[i] == 1 & box$win[i] == 0){
    box$fgp_loss_opp[i] <- 1
  }
}
fgp_win <- as.data.frame(aggregate(box$fgp_win, by=list(Teams = box$team_abr), FUN = sum))
teams$fgp_win <- fgp_win$x
fgp_loss <- as.data.frame(aggregate(box$fgp_loss, by=list(Teams = box$team_abr), FUN = sum))
teams$fgp_loss <- fgp_loss$x
fgp_win_opp <- as.data.frame(aggregate(box$fgp_win_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$fgp_win_opp <- fgp_win_opp$x
fgp_loss_opp <- as.data.frame(aggregate(box$fgp_loss_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$fgp_loss_opp <- fgp_loss_opp$x
#===rebounds
box$rebounds <- box$offensive_rebounds + box$defensive_rebounds
box$col_morereb <- rep(0,nrow(box))
box$col_lessreb <- rep(0,nrow(box))
for(i in 1:(nrow(box)/2)) {
  if(box$rebounds[(2*i)-1]>box$rebounds[2*i]){
    box$col_morereb[(2*i)-1] <- 1
    box$col_lessreb[(2*i)] <- 1
  }
  else if (box$rebounds[(2*i)] > box$rebounds[(2*i)-1]) {
    box$col_lessreb[(2*i)-1] <- 1
    box$col_morereb[(2*i)] <- 1
  }
  else if (box$rebounds[(2*i)] == box$rebounds[(2*i)]) {
    box$col_morereb[2*i] <- 1
    box$col_morereb[(2*i)-1] <- 1
  }
}
box$rebounds_win <- rep(0, nrow(box))
box$rebounds_loss <- rep(0, nrow(box))
box$rebounds_win_opp <- rep(0, nrow(box))
box$rebounds_loss_opp <- rep(0, nrow(box)) 
for(i in 1:(nrow(box))){
  if(box$col_morereb[i] == 1 & box$win[i] == 1){
    box$rebounds_win[i] <- 1
  }
  else if(box$col_lessreb[i] == 1 & box$win[i] == 1){
    box$rebounds_win_opp[(i)] <- 1
  }
  else if(box$col_morereb[i] == 1 & box$win[i] == 0){
    box$rebounds_loss[i] <- 1
  }
  else if(box$col_lessreb[i] == 1 & box$win[i] == 0){
    box$rebounds_loss_opp[i] <- 1
  }
}
rebounds_win <- as.data.frame(aggregate(box$rebounds_win, by=list(Teams = box$team_abr), FUN = sum))
teams$rebounds_win <- rebounds_win$x
rebounds_loss <- as.data.frame(aggregate(box$rebounds_loss, by=list(Teams = box$team_abr), FUN = sum))
teams$rebounds_loss <- rebounds_loss$x
rebounds_win_opp <- as.data.frame(aggregate(box$rebounds_win_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$rebounds_win_opp <- rebounds_win_opp$x
rebounds_loss_opp <- as.data.frame(aggregate(box$rebounds_loss_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$rebounds_loss_opp <- rebounds_loss_opp$x
#=====assists
box$col_moreast <- rep(0,nrow(box))
box$col_lessast <- rep(0,nrow(box))
for(i in 1:(nrow(box)/2)) {
  if(box$assists[(2*i)-1]>box$assists[2*i]){
    box$col_moreast[(2*i)-1] <- 1
    box$col_lessast[(2*i)] <- 1
  }
  else if (box$assists[(2*i)] > box$assists[(2*i)-1]) {
    box$col_lessast[(2*i)-1] <- 1
    box$col_moreast[(2*i)] <- 1
  }
  else if (box$assists[(2*i)] == box$assists[(2*i)]) {
    box$col_moreast[2*i] <- 1
    box$col_moreast[(2*i)-1] <- 1
  }
}
box$ast_win <- rep(0, nrow(box))
box$ast_loss <- rep(0, nrow(box))
box$ast_win_opp <- rep(0, nrow(box))
box$ast_loss_opp <- rep(0, nrow(box)) 
for(i in 1:(nrow(box))){
  if(box$col_moreast[i] == 1 & box$win[i] == 1){
    box$ast_win[i] <- 1
  }
  else if(box$col_lessast[i] == 1 & box$win[i] == 1){
    box$ast_win_opp[(i)] <- 1
  }
  else if(box$col_moreast[i] == 1 & box$win[i] == 0){
    box$ast_loss[i] <- 1
  }
  else if(box$col_lessast[i] == 1 & box$win[i] == 0){
    box$ast_loss_opp[i] <- 1
  }
}
ast_win <- as.data.frame(aggregate(box$ast_win, by=list(Teams = box$team_abr), FUN = sum))
teams$ast_win <- ast_win$x
ast_loss <- as.data.frame(aggregate(box$ast_loss, by=list(Teams = box$team_abr), FUN = sum))
teams$ast_loss <- ast_loss$x
ast_win_opp <- as.data.frame(aggregate(box$ast_win_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$ast_win_opp <- ast_win_opp$x
ast_loss_opp <- as.data.frame(aggregate(box$ast_loss_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$ast_loss_opp <- ast_loss_opp$x
#===FTA
box$col_morefta <- rep(0,nrow(box))
box$col_lessfta <- rep(0,nrow(box))
for(i in 1:(nrow(box)/2)) {
  if(box$ft_attempted[(2*i)-1]>box$ft_attempted[2*i]){
    box$col_morefta[(2*i)-1] <- 1
    box$col_lessfta[(2*i)] <- 1
  }
  else if (box$ft_attempted[(2*i)] > box$ft_attempted[(2*i)-1]) {
    box$col_lessfta[(2*i)-1] <- 1
    box$col_morefta[(2*i)] <- 1
  }
  else if (box$ft_attempted[(2*i)] == box$ft_attempted[(2*i)]) {
    box$col_morefta[2*i] <- 1
    box$col_morefta[(2*i)-1] <- 1
  }
}
box$fta_win <- rep(0, nrow(box))
box$fta_loss <- rep(0, nrow(box))
box$fta_win_opp <- rep(0, nrow(box))
box$fta_loss_opp <- rep(0, nrow(box)) 
for(i in 1:(nrow(box))){
  if(box$col_morefta[i] == 1 & box$win[i] == 1){
    box$fta_win[i] <- 1
  }
  else if(box$col_lessfta[i] == 1 & box$win[i] == 1){
    box$fta_win_opp[(i)] <- 1
  }
  else if(box$col_morefta[i] == 1 & box$win[i] == 0){
    box$fta_loss[i] <- 1
  }
  else if(box$col_lessfta[i] == 1 & box$win[i] == 0){
    box$fta_loss_opp[i] <- 1
  }
}
fta_win <- as.data.frame(aggregate(box$fta_win, by=list(Teams = box$team_abr), FUN = sum))
teams$fta_win <- fta_win$x
fta_loss <- as.data.frame(aggregate(box$fta_loss, by=list(Teams = box$team_abr), FUN = sum))
teams$fta_loss <- fta_loss$x
fta_win_opp <- as.data.frame(aggregate(box$fta_win_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$fta_win_opp <- fta_win_opp$x
fta_loss_opp <- as.data.frame(aggregate(box$fta_loss_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$fta_loss_opp <- fta_loss_opp$x
#===3FG 
box$col_morethree <- rep(0,nrow(box))
box$col_lessthree <- rep(0,nrow(box))
for(i in 1:(nrow(box)/2)) {
  if(box$three_made[(2*i)-1]>box$three_made[2*i]){
    box$col_morethree[(2*i)-1] <- 1
    box$col_lessthree[(2*i)] <- 1
  }
  else if (box$three_made[(2*i)] > box$three_made[(2*i)-1]) {
    box$col_lessthree[(2*i)-1] <- 1
    box$col_morethree[(2*i)] <- 1
  }
  else if (box$three_made[(2*i)] == box$three_made[(2*i)]) {
    box$col_morethree[2*i] <- 1
    box$col_morethree[(2*i)-1] <- 1
  }
}
box$three_win <- rep(0, nrow(box))
box$three_loss <- rep(0, nrow(box))
box$three_win_opp <- rep(0, nrow(box))
box$three_loss_opp <- rep(0, nrow(box)) 
for(i in 1:(nrow(box))){
  if(box$col_morethree[i] == 1 & box$win[i] == 1){
    box$three_win[i] <- 1
  }
  else if(box$col_lessthree[i] == 1 & box$win[i] == 1){
    box$three_win_opp[(i)] <- 1
  }
  else if(box$col_morethree[i] == 1 & box$win[i] == 0){
    box$three_loss[i] <- 1
  }
  else if(box$col_lessthree[i] == 1 & box$win[i] == 0){
    box$three_loss_opp[i] <- 1
  }
}
three_win <- as.data.frame(aggregate(box$three_win, by=list(Teams = box$team_abr), FUN = sum))
teams$three_win <- three_win$x
three_loss <- as.data.frame(aggregate(box$three_loss, by=list(Teams = box$team_abr), FUN = sum))
teams$three_loss <- three_loss$x
three_win_opp <- as.data.frame(aggregate(box$three_win_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$three_win_opp <- three_win_opp$x
three_loss_opp <- as.data.frame(aggregate(box$three_loss_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$three_loss_opp <- three_loss_opp$x
#====Steals
box$col_moresteal <- rep(0,nrow(box))
box$col_lesssteal <- rep(0,nrow(box))
for(i in 1:(nrow(box)/2)) {
  if(box$steals[(2*i)-1]>box$steals[2*i]){
    box$col_moresteal[(2*i)-1] <- 1
    box$col_lesssteal[(2*i)] <- 1
  }
  else if (box$steals[(2*i)] > box$steals[(2*i)-1]) {
    box$col_lesssteal[(2*i)-1] <- 1
    box$col_moresteal[(2*i)] <- 1
  }
  else if (box$steals[(2*i)] == box$steals[(2*i)]) {
    box$col_moresteal[2*i] <- 1
    box$col_moresteal[(2*i)-1] <- 1
  }
}
box$steal_win <- rep(0, nrow(box))
box$steal_loss <- rep(0, nrow(box))
box$steal_win_opp <- rep(0, nrow(box))
box$steal_loss_opp <- rep(0, nrow(box)) 
for(i in 1:(nrow(box))){
  if(box$col_moresteal[i] == 1 & box$win[i] == 1){
    box$steal_win[i] <- 1
  }
  else if(box$col_lesssteal[i] == 1 & box$win[i] == 1){
    box$steal_win_opp[(i)] <- 1
  }
  else if(box$col_moresteal[i] == 1 & box$win[i] == 0){
    box$steal_loss[i] <- 1
  }
  else if(box$col_lesssteal[i] == 1 & box$win[i] == 0){
    box$steal_loss_opp[i] <- 1
  }
}
steal_win <- as.data.frame(aggregate(box$steal_win, by=list(Teams = box$team_abr), FUN = sum))
teams$steal_win <- steal_win$x
steal_loss <- as.data.frame(aggregate(box$steal_loss, by=list(Teams = box$team_abr), FUN = sum))
teams$steal_loss <- steal_loss$x
steal_win_opp <- as.data.frame(aggregate(box$steal_win_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$steal_win_opp <- steal_win_opp$x
steal_loss_opp <- as.data.frame(aggregate(box$steal_loss_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$steal_loss_opp <- steal_loss_opp$x
#====Blocks
box$col_moreblock <- rep(0,nrow(box))
box$col_lessblock <- rep(0,nrow(box))
for(i in 1:(nrow(box)/2)) {
  if(box$blocks[(2*i)-1]>box$blocks[2*i]){
    box$col_moreblock[(2*i)-1] <- 1
    box$col_lessblock[(2*i)] <- 1
  }
  else if (box$blocks[(2*i)] > box$blocks[(2*i)-1]) {
    box$col_lessblock[(2*i)-1] <- 1
    box$col_moreblock[(2*i)] <- 1
  }
  else if (box$blocks[(2*i)] == box$blocks[(2*i)]) {
    box$col_moreblock[2*i] <- 1
    box$col_moreblock[(2*i)-1] <- 1
  }
}
box$block_win <- rep(0, nrow(box))
box$block_loss <- rep(0, nrow(box))
box$block_win_opp <- rep(0, nrow(box))
box$block_loss_opp <- rep(0, nrow(box)) 
for(i in 1:(nrow(box))){
  if(box$col_moreblock[i] == 1 & box$win[i] == 1){
    box$block_win[i] <- 1
  }
  else if(box$col_lessblock[i] == 1 & box$win[i] == 1){
    box$block_win_opp[(i)] <- 1
  }
  else if(box$col_moreblock[i] == 1 & box$win[i] == 0){
    box$block_loss[i] <- 1
  }
  else if(box$col_lessblock[i] == 1 & box$win[i] == 0){
    box$block_loss_opp[i] <- 1
  }
}
block_win <- as.data.frame(aggregate(box$block_win, by=list(Teams = box$team_abr), FUN = sum))
teams$block_win <- block_win$x
block_loss <- as.data.frame(aggregate(box$block_loss, by=list(Teams = box$team_abr), FUN = sum))
teams$block_loss <- block_loss$x
block_win_opp <- as.data.frame(aggregate(box$block_win_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$block_win_opp <- block_win_opp$x
block_loss_opp <- as.data.frame(aggregate(box$block_loss_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$block_loss_opp <- block_loss_opp$x
#====2nd chance points
box$col_moresecondchance <- rep(0,nrow(box))
box$col_lesssecondchance <- rep(0,nrow(box))
for(i in 1:(nrow(box)/2)) {
  if(box$second_chance_points[(2*i)-1]>box$second_chance_points[2*i]){
    box$col_moresecondchance[(2*i)-1] <- 1
    box$col_lesssecondchance[(2*i)] <- 1
  }
  else if (box$second_chance_points[(2*i)] > box$second_chance_points[(2*i)-1]) {
    box$col_lesssecondchance[(2*i)-1] <- 1
    box$col_moresecondchance[(2*i)] <- 1
  }
  else if (box$second_chance_points[(2*i)] == box$second_chance_points[(2*i)]) {
    box$col_moresecondchance[2*i] <- 1
    box$col_moresecondchance[(2*i)-1] <- 1
  }
}
box$secondchance_win <- rep(0, nrow(box))
box$secondchance_loss <- rep(0, nrow(box))
box$secondchance_win_opp <- rep(0, nrow(box))
box$secondchance_loss_opp <- rep(0, nrow(box)) 
for(i in 1:(nrow(box))){
  if(box$col_moresecondchance[i] == 1 & box$win[i] == 1){
    box$secondchance_win[i] <- 1
  }
  else if(box$col_lesssecondchance[i] == 1 & box$win[i] == 1){
    box$secondchance_win_opp[(i)] <- 1
  }
  else if(box$col_moresecondchance[i] == 1 & box$win[i] == 0){
    box$secondchance_loss[i] <- 1
  }
  else if(box$col_lesssecondchance[i] == 1 & box$win[i] == 0){
    box$secondchance_loss_opp[i] <- 1
  }
}
secondchance_win <- as.data.frame(aggregate(box$secondchance_win, by=list(Teams = box$team_abr), FUN = sum))
teams$secondchance_win <- secondchance_win$x
secondchance_loss <- as.data.frame(aggregate(box$secondchance_loss, by=list(Teams = box$team_abr), FUN = sum))
teams$secondchance_loss <- secondchance_loss$x
secondchance_win_opp <- as.data.frame(aggregate(box$secondchance_win_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$secondchance_win_opp <- secondchance_win_opp$x
secondchance_loss_opp <- as.data.frame(aggregate(box$secondchance_loss_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$secondchance_loss_opp <- secondchance_loss_opp$x
#====Fast break points
box$col_morefastbreak <- rep(0,nrow(box))
box$col_lessfastbreak <- rep(0,nrow(box))
for(i in 1:(nrow(box)/2)) {
  if(box$fastbreak_points[(2*i)-1]>box$fastbreak_points[2*i]){
    box$col_morefastbreak[(2*i)-1] <- 1
    box$col_lessfastbreak[(2*i)] <- 1
  }
  else if (box$fastbreak_points[(2*i)] > box$fastbreak_points[(2*i)-1]) {
    box$col_lessfastbreak[(2*i)-1] <- 1
    box$col_morefastbreak[(2*i)] <- 1
  }
  else if (box$fastbreak_points[(2*i)] == box$fastbreak_points[(2*i)]) {
    box$col_morefastbreak[2*i] <- 1
    box$col_morefastbreak[(2*i)-1] <- 1
  }
}
box$fastbreak_win <- rep(0, nrow(box))
box$fastbreak_loss <- rep(0, nrow(box))
box$fastbreak_win_opp <- rep(0, nrow(box))
box$fastbreak_loss_opp <- rep(0, nrow(box)) 
for(i in 1:(nrow(box))){
  if(box$col_morefastbreak[i] == 1 & box$win[i] == 1){
    box$fastbreak_win[i] <- 1
  }
  else if(box$col_lessfastbreak[i] == 1 & box$win[i] == 1){
    box$fastbreak_win_opp[(i)] <- 1
  }
  else if(box$col_morefastbreak[i] == 1 & box$win[i] == 0){
    box$fastbreak_loss[i] <- 1
  }
  else if(box$col_lessfastbreak[i] == 1 & box$win[i] == 0){
    box$fastbreak_loss_opp[i] <- 1
  }
}
fastbreak_win <- as.data.frame(aggregate(box$fastbreak_win, by=list(Teams = box$team_abr), FUN = sum))
teams$fastbreak_win <- fastbreak_win$x
fastbreak_loss <- as.data.frame(aggregate(box$fastbreak_loss, by=list(Teams = box$team_abr), FUN = sum))
teams$fastbreak_loss <- fastbreak_loss$x
fastbreak_win_opp <- as.data.frame(aggregate(box$fastbreak_win_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$fastbreak_win_opp <- fastbreak_win_opp$x
fastbreak_loss_opp <- as.data.frame(aggregate(box$fastbreak_loss_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$fastbreak_loss_opp <- fastbreak_loss_opp$x
#====Points in the paint
box$col_morepaint <- rep(0,nrow(box))
box$col_lesspaint <- rep(0,nrow(box))
for(i in 1:(nrow(box)/2)) {
  if(box$points_in_the_paint[(2*i)-1]>box$points_in_the_paint[2*i]){
    box$col_morepaint[(2*i)-1] <- 1
    box$col_lesspaint[(2*i)] <- 1
  }
  else if (box$points_in_the_paint[(2*i)] > box$points_in_the_paint[(2*i)-1]) {
    box$col_lesspaint[(2*i)-1] <- 1
    box$col_morepaint[(2*i)] <- 1
  }
  else if (box$points_in_the_paint[(2*i)] == box$points_in_the_paint[(2*i)]) {
    box$col_morepaint[2*i] <- 1
    box$col_morepaint[(2*i)-1] <- 1
  }
}
box$paint_win <- rep(0, nrow(box))
box$paint_loss <- rep(0, nrow(box))
box$paint_win_opp <- rep(0, nrow(box))
box$paint_loss_opp <- rep(0, nrow(box)) 
for(i in 1:(nrow(box))){
  if(box$col_morepaint[i] == 1 & box$win[i] == 1){
    box$paint_win[i] <- 1
  }
  else if(box$col_lesspaint[i] == 1 & box$win[i] == 1){
    box$paint_win_opp[(i)] <- 1
  }
  else if(box$col_morepaint[i] == 1 & box$win[i] == 0){
    box$paint_loss[i] <- 1
  }
  else if(box$col_lesspaint[i] == 1 & box$win[i] == 0){
    box$paint_loss_opp[i] <- 1
  }
}
paint_win <- as.data.frame(aggregate(box$paint_win, by=list(Teams = box$team_abr), FUN = sum))
teams$paint_win <- paint_win$x
paint_loss <- as.data.frame(aggregate(box$paint_loss, by=list(Teams = box$team_abr), FUN = sum))
teams$paint_loss <- paint_loss$x
paint_win_opp <- as.data.frame(aggregate(box$paint_win_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$paint_win_opp <- paint_win_opp$x
paint_loss_opp <- as.data.frame(aggregate(box$paint_loss_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$paint_loss_opp <- paint_loss_opp$x
#====quarter performance
team_order <- box$team_abr #setting the list of the teams aside for later identification
game_order <- box$game_no
col_quarter_date <- which(colnames(box_2) == "Game.Game_info.Game_date")
col_quarter_start1 <- which(colnames(box_2) == "Game.Msg_score.Visitor_team_score.Qtr_1_score")
col_quarter_end1 <- which(colnames(box_2) == "Game.Msg_score.Visitor_team_score.Qtr_4_score")
col_quarter_start2 <- which(colnames(box_2) == "Game.Msg_score.Home_team_score.Qtr_1_score")
col_quarter_end2 <- which(colnames(box_2) == "Game.Msg_score.Home_team_score.Qtr_4_score")
sort_1 <- box_2[, c(1,col_quarter_date,col_quarter_start1:col_quarter_end1)]
sort_2 <- box_2[, col_quarter_start2:col_quarter_end2]
sort_1 <- unique(sort_1)
sort_1 <- na.omit(sort_1)
sort_2 <- unique(sort_2)
sort_2 <- na.omit(sort_2)
box_2 <- cbind(sort_1,sort_2)
box$firstq <- rep(0, nrow(box))
box$secondq <- rep(0, nrow(box))
box$thirdq <- rep(0, nrow(box))
for(i in 1:(nrow(box)/2)){
  box$firstq[(2*i)-1] <- box_2$Game.Msg_score.Visitor_team_score.Qtr_1_score[i]
  box$secondq[(2*i)-1] <- box_2$Game.Msg_score.Visitor_team_score.Qtr_2_score[i]
  box$thirdq[(2*i)-1] <- box_2$Game.Msg_score.Visitor_team_score.Qtr_3_score[i]
  box$firstq[(2*i)] <- box_2$Game.Msg_score.Home_team_score.Qtr_1_score[i]
  box$secondq[2*i] <- box_2$Game.Msg_score.Home_team_score.Qtr_2_score[i]
  box$thirdq[2*i] <- box_2$Game.Msg_score.Home_team_score.Qtr_3_score[i]
}
box$firstq <- as.numeric(box$firstq)
box$secondq <- as.numeric(box$secondq)
box$thirdq <- as.numeric(box$thirdq)
#===First qurater
box$col_morefirst <- rep(0,nrow(box))
box$col_lessfirst <- rep(0,nrow(box))
for(i in 1:(nrow(box)/2)) {
  if(box$firstq[(2*i)-1]>box$firstq[2*i]){
    box$col_morefirst[(2*i)-1] <- 1
    box$col_lessfirst[(2*i)] <- 1
  }
  else if (box$firstq[(2*i)] > box$firstq[(2*i)-1]) {
    box$col_lessfirst[(2*i)-1] <- 1
    box$col_morefirst[(2*i)] <- 1
  }
  else if (box$firstq[(2*i)] == box$firstq[(2*i)]) {
    box$col_morefirst[2*i] <- 1
    box$col_morefirst[(2*i)-1] <- 1
  }
}
box$first_win <- rep(0, nrow(box))
box$first_loss <- rep(0, nrow(box))
box$first_win_opp <- rep(0, nrow(box))
box$first_loss_opp <- rep(0, nrow(box)) 
for(i in 1:(nrow(box))){
  if(box$col_morefirst[i] == 1 & box$win[i] == 1){
    box$first_win[i] <- 1
  }
  else if(box$col_lessfirst[i] == 1 & box$win[i] == 1){
    box$first_win_opp[(i)] <- 1
  }
  else if(box$col_morefirst[i] == 1 & box$win[i] == 0){
    box$first_loss[i] <- 1
  }
  else if(box$col_lessfirst[i] == 1 & box$win[i] == 0){
    box$first_loss_opp[i] <- 1
  }
}
first_win <- as.data.frame(aggregate(box$first_win, by=list(Teams = box$team_abr), FUN = sum))
teams$first_win <- first_win$x
first_loss <- as.data.frame(aggregate(box$first_loss, by=list(Teams = box$team_abr), FUN = sum))
teams$first_loss <- first_loss$x
first_win_opp <- as.data.frame(aggregate(box$first_win_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$first_win_opp <- first_win_opp$x
first_loss_opp <- as.data.frame(aggregate(box$first_loss_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$first_loss_opp <- first_loss_opp$x
#leading at halftime
box$secondq <- box$firstq + box$secondq #making points cumulative at quarter marks
box$thirdq <- box$secondq + box$thirdq
box$col_moresecond <- rep(0,nrow(box))
box$col_lesssecond <- rep(0,nrow(box))
for(i in 1:(nrow(box)/2)) {
  if(box$secondq[(2*i)-1]>box$secondq[2*i]){
    box$col_moresecond[(2*i)-1] <- 1
    box$col_lesssecond[(2*i)] <- 1
  }
  else if (box$secondq[(2*i)] > box$secondq[(2*i)-1]) {
    box$col_lesssecond[(2*i)-1] <- 1
    box$col_moresecond[(2*i)] <- 1
  }
  else if (box$secondq[(2*i)] == box$secondq[(2*i)]) {
    box$col_moresecond[2*i] <- 1
    box$col_moresecond[(2*i)-1] <- 1
  }
}
box$second_win <- rep(0, nrow(box))
box$second_loss <- rep(0, nrow(box))
box$second_win_opp <- rep(0, nrow(box))
box$second_loss_opp <- rep(0, nrow(box)) 
for(i in 1:(nrow(box))){
  if(box$col_moresecond[i] == 1 & box$win[i] == 1){
    box$second_win[i] <- 1
  }
  else if(box$col_lesssecond[i] == 1 & box$win[i] == 1){
    box$second_win_opp[(i)] <- 1
  }
  else if(box$col_moresecond[i] == 1 & box$win[i] == 0){
    box$second_loss[i] <- 1
  }
  else if(box$col_lesssecond[i] == 1 & box$win[i] == 0){
    box$second_loss_opp[i] <- 1
  }
}
second_win <- as.data.frame(aggregate(box$second_win, by=list(Teams = box$team_abr), FUN = sum))
teams$second_win <- second_win$x
second_loss <- as.data.frame(aggregate(box$second_loss, by=list(Teams = box$team_abr), FUN = sum))
teams$second_loss <- second_loss$x
second_win_opp <- as.data.frame(aggregate(box$second_win_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$second_win_opp <- second_win_opp$x
second_loss_opp <- as.data.frame(aggregate(box$second_loss_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$second_loss_opp <- second_loss_opp$x
#====Third quarter
box$col_morethird <- rep(0,nrow(box))
box$col_lessthird <- rep(0,nrow(box))
for(i in 1:(nrow(box)/2)) {
  if(box$thirdq[(2*i)-1]>box$thirdq[2*i]){
    box$col_morethird[(2*i)-1] <- 1
    box$col_lessthird[(2*i)] <- 1
  }
  else if (box$thirdq[(2*i)] > box$thirdq[(2*i)-1]) {
    box$col_lessthird[(2*i)-1] <- 1
    box$col_morethird[(2*i)] <- 1
  }
  else if (box$thirdq[(2*i)] == box$thirdq[(2*i)]) {
    box$col_morethird[2*i] <- 1
    box$col_morethird[(2*i)-1] <- 1
  }
}
box$third_win <- rep(0, nrow(box))
box$third_loss <- rep(0, nrow(box))
box$third_win_opp <- rep(0, nrow(box))
box$third_loss_opp <- rep(0, nrow(box)) 
for(i in 1:(nrow(box))){
  if(box$col_morethird[i] == 1 & box$win[i] == 1){
    box$third_win[i] <- 1
  }
  else if(box$col_lessthird[i] == 1 & box$win[i] == 1){
    box$third_win_opp[(i)] <- 1
  }
  else if(box$col_morethird[i] == 1 & box$win[i] == 0){
    box$third_loss[i] <- 1
  }
  else if(box$col_lessthird[i] == 1 & box$win[i] == 0){
    box$third_loss_opp[i] <- 1
  }
}
third_win <- as.data.frame(aggregate(box$third_win, by=list(Teams = box$team_abr), FUN = sum))
teams$third_win <- third_win$x
third_loss <- as.data.frame(aggregate(box$third_loss, by=list(Teams = box$team_abr), FUN = sum))
teams$third_loss <- third_loss$x
third_win_opp <- as.data.frame(aggregate(box$third_win_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$third_win_opp <- third_win_opp$x
third_loss_opp <- as.data.frame(aggregate(box$third_loss_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$third_loss_opp <- third_loss_opp$x
#====Back to back
box_2$Game.Game_info.Game_date <- as.Date(box_2$Game.Game_info.Game_date, format = "%m/%d/%Y")
box$date <- rep(0,nrow(box))
for(i in 1:(nrow(box)/2)) {
  box$date[(2*i)-1] <- box_2$Game.Game_info.Game_date[i]
  box$date[2*i] <- box_2$Game.Game_info.Game_date[i]
}
box_3 <- arrange(box, team_abr, date)
box_3$datediff <- rep(0, nrow(box))
for(i in 2:(nrow(box))) {
  box_3$datediff[i] <- box_3$date[i] - box_3$date[i-1]
}
box_3$backtoback <- rep(0,nrow(box))
for(i in 1:nrow(box)){
  if(box_3$datediff[i] == 1) {
    box_3$backtoback[i] <- 1
  }
}
box_3 <- arrange(box_3, game_no, desc(awaywin), homewin)
box$backtoback <- box_3$backtoback 
box$backtoback_win <- rep(0, nrow(box))
box$backtoback_loss <- rep(0, nrow(box))
for(i in 1:nrow(box)){
  if(box$backtoback[i] == 1 & box$win[i] == 1){
    box$backtoback_win[i] <- 1
    box$backtoback_loss[i] <- 0
  }
  else if (box$backtoback[i] == 1 & box$win[i] == 0){
    box$backtoback_win[i] <- 0
    box$backtoback_loss[i] <- 1
  }
  else{
    box$backtoback_win[i] <- 0
    box$backtoback_loss[i] <- 0
  }
}
backtoback_win <- as.data.frame(aggregate(box$backtoback_win, by=list(Teams = box$team_abr), FUN = sum))
teams$backtoback_win <- backtoback_win$x
backtoback_loss <- as.data.frame(aggregate(box$backtoback_loss, by=list(Teams = box$team_abr), FUN = sum))
teams$backtoback_loss <- backtoback_loss$x
#==== bench
bench_col_start <- which(colnames(box_bench) =="Game.Msg_boxscore.Player_stats.Team_abr")
bench_col_1 <- which(colnames(box_bench)=="Game.Msg_boxscore.Player_stats.Starting_position")
bench_col_end <- which(colnames(box_bench)=="Game.Msg_boxscore.Player_stats.Points")
box_bench <- box_bench[,c(1,bench_col_start,bench_col_1,bench_col_end)]
box_bench <- na.omit(box_bench)
box_bench <- box_bench %>%
  filter(Game.Msg_boxscore.Player_stats.Starting_position != "")
bench_coef <- nrow(box_bench)/10
box_bench$ordering <- rep(c(1,1,1,1,1,0,0,0,0,0), bench_coef)
box_bench_sum <- aggregate(box_bench$Game.Msg_boxscore.Player_stats.Points, list(box_bench$Game.Msg_boxscore.Player_stats.Team_abr, box_bench$Game.Number, box_bench$ordering), FUN = sum) # should be fixed here
box_bench_sum <- arrange(box_bench_sum, Group.2, desc(Group.3))
box$starters <- box_bench_sum$x #i actually have the aggregate points for the starters. I will use this to calculate the bench points.
box$bench <- box$points - box$starters
#== now the actual loop part - More or equal bench points as opponent
box$col_morebench <- rep(0,nrow(box))
box$col_lessbench <- rep(0,nrow(box))
for(i in 1:(nrow(box)/2)) {
  if(box$bench[(2*i)-1]>box$bench[2*i]){
    box$col_morebench[(2*i)-1] <- 1
    box$col_lessbench[(2*i)] <- 1
  }
  else if (box$bench[(2*i)] > box$bench[(2*i)-1]) {
    box$col_lessbench[(2*i)-1] <- 1
    box$col_morebench[(2*i)] <- 1
  }
  else if (box$bench[(2*i)] == box$bench[(2*i)]) {
    box$col_morebench[2*i] <- 1
    box$col_morebench[(2*i)-1] <- 1
  }
}
box$bench_win <- rep(0, nrow(box))
box$bench_loss <- rep(0, nrow(box))
box$bench_win_opp <- rep(0, nrow(box))
box$bench_loss_opp <- rep(0, nrow(box)) 
for(i in 1:(nrow(box))){
  if(box$col_morebench[i] == 1 & box$win[i] == 1){
    box$bench_win[i] <- 1
  }
  else if(box$col_lessbench[i] == 1 & box$win[i] == 1){
    box$bench_win_opp[(i)] <- 1
  }
  else if(box$col_morebench[i] == 1 & box$win[i] == 0){
    box$bench_loss[i] <- 1
  }
  else if(box$col_lessbench[i] == 1 & box$win[i] == 0){
    box$bench_loss_opp[i] <- 1
  }
}
bench_win <- as.data.frame(aggregate(box$bench_win, by=list(Teams = box$team_abr), FUN = sum))
teams$bench_win <- bench_win$x
bench_loss <- as.data.frame(aggregate(box$bench_loss, by=list(Teams = box$team_abr), FUN = sum))
teams$bench_loss <- bench_loss$x
bench_win_opp <- as.data.frame(aggregate(box$bench_win_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$bench_win_opp <- bench_win_opp$x
bench_loss_opp <- as.data.frame(aggregate(box$bench_loss_opp, by=list(Teams = box$team_abr), FUN = sum))
teams$bench_loss_opp <- bench_loss_opp$x
##====little bit of sorting
teams <- teams[,c(1,2,4,5,9,6,10,11:78)]
teams$label1 <- rep("Win", nrow(teams))
teams$label2 <- rep("Loss", nrow(teams))
teams <- teams[c(1,76,77,2:75)]
##====loop and save
for (i in 1:30) {
  #dir.create(paste0(savePath, teams$teams[i]))
  data1 <- data.frame(matrix(teams[i, 2:77], ncol = 2, byrow = TRUE))
  row.names(data1) <- 
    c("Variables", "Overall...", "Home...", "Away...", "Decided by three points or less...", "Score 100-plus...", "Score 99 or below...", "Opponents score 100-plus...", "Opponents score 99 or below...", "Outshooting opponent ...", "Outshot by opponent...", "More rebounds than opponent ...", "Fewer rebounds than opponent...", "More assists than opponent ...", "Fewer assists than opponent...", "More FTA than opponent ...", "Fewer FTA than opponent...", "More 3FG than opponent ...", "Fewer 3FG than opponent...", "More steals than opponent ...", "Fewer steals than opponent...", "More blocks than opponent ...", "Fewer blocks than opponent...", "More 2nd-chance points than opponent ...", "Fewer 2nd-chance points than opponent...", "More fastbreak points than opponent ...", "Fewer fastbreak points than opponent...", "More points in the paint than opponent ...", "Fewer points in the paint than opponent...", "Leading after first quarter ...", "Trailing after first quarter...", "Leading at halftime ...", "Trailing at halftime...", "Leading after third quarter ...", "Trailing after third quarter...", "Second game of back-to-back...", "More bench points than the opponents ...", "Fewer bench points than the opponents...")
  data1 <- apply(data1, 2, as.character)
  write.table(data1, file = paste0(savePath, teams$teams[i],'.csv'), row.names = c("Variables", "Overall...", "Home...", "Away...", "Decided by three points or less...", "Score 100-plus...", "Score 99 or below...", "Opponents score 100-plus...", "Opponents score 99 or below...", "Outshooting opponent (tied)...", "Outshot by opponent...", "More rebounds than opponent (tied)...", "Fewer rebounds than opponent...", "More assists than opponent (tied)...", "Fewer assists than opponent...", "More FTA than opponent (tied)...", "Fewer FTA than opponent...", "More 3FG than opponent (tied)...", "Fewer 3FG than opponent...", "More steals than opponent (tied)...", "Fewer steals than opponent...", "More blocks than opponent (tied)...", "Fewer blocks than opponent...", "More 2nd-chance points than opponent (tied)...", "Fewer 2nd-chance points than opponent...", "More fastbreak points than opponent (tied)...", "Fewer fastbreak points than opponent...", "More points in the paint than opponent (tied)...", "Fewer points in the paint than opponent...", "Leading after first quarter (tied)...", "Trailing after first quarter...", "Leading at halftime (tied)...", "Trailing at halftime...", "Leading after third quarter (tied)...", "Trailing after third quarter...", "Second game of back-to-back...", "More bench points than the opponents (tied)...", "Fewer bench points than the opponents..."),
            col.names = c("", ""), sep= ",")
}

