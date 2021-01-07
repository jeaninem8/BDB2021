library(dplyr)
library(stringr)
library(ggplot2)
library(gganimate)
library(cowplot)
library(repr)
library(nflfastR)
library(ggvoronoi)
library(rgeos)
library(data.table)
library(tidyr)
library(randomForest)
library(MLmetrics)
library(gifski)


weeks_list <- c("week1", "week2", "week3", "week4", "week5", "week6", "week7",
                "week8", "week9", "week10", "week11", "week12", "week13", 
                "week14", "week15", "week16", "week17")


for (iter in 9:10) {
  

  week <- read.csv(paste(weeks_list[iter], ".csv", sep = ""))
  
  # standardize play direction
  week <- week %>%
    mutate(x = ifelse(playDirection == "left", 120-x, x),
           y = ifelse(playDirection == "left", 160/3 - y, y))
  
  #merging plays and tracking data
  week <- inner_join(week, plays, by = c("gameId" = "gameId", "playId" = "playId"))
  
  #merging games and tracking data
  week <- inner_join(week, games, by = c("gameId" = "gameId"))
  
  
  # define offense and defense
  week <- week %>% mutate(sideOfBall = ifelse(
    #if tracked player is home and home has ball
    ((team == "home") &
       (possessionTeam == homeTeamAbbr)) |
      
      #if tracked player is away and away has ball
      ((team == "away") &
         (possessionTeam == visitorTeamAbbr)), "offense", "defense"),
    
    #defining defensive team
    defensiveTeam = ifelse(possessionTeam == homeTeamAbbr,
                           as.character(visitorTeamAbbr),
                           as.character(homeTeamAbbr)))
  
  
  week_plays_list <- week %>% distinct(gameId, playId)
  
  week_games_plays_list <- as.data.frame(matrix(data = NA, nrow = 0, ncol = 2))
  colnames(week_games_plays_list) <- colnames(week_plays_list)
  
  for (i in 1:nrow(week_plays_list)) {
    play_events <- week$event[week$gameId == week_plays_list$gameId[i] & week$playId == week_plays_list$playId[i]]
    if (any(grepl("outcome", play_events, fixed = TRUE))) {
      week_games_plays_list <- rbind(week_games_plays_list, week_plays_list[i,])
    } else{next}
  }
  
  
  week_playervar <- as.data.frame(matrix(data = NA, nrow = 0, ncol = 51))
  colnames(week_playervar) <- colnames(current_play) # run first iteration of below loop to get column names
  
  for (p in 1:nrow(week_games_plays_list)) {
    print(p)
    tryCatch({current_play <- week[week$gameId == week_games_plays_list$gameId[p] & week$playId == week_games_plays_list$playId[p],] %>% distinct()
    
    # get only ball snap to play outcome frames
    snap_frame <- current_play$frameId[current_play$event == "ball_snap"][1]
    outcome_frame <- current_play$frameId[grepl("outcome", current_play$event, fixed = TRUE)][1]
    frame_list <- c(snap_frame:outcome_frame)
    # calculate variables for each play
    
    # determine closest offensive player to each defender
    current_play_pass <- current_play[current_play$frameId %in% frame_list,]
    
    for (j in 1:length(frame_list)) {
      off <- current_play_pass[current_play_pass$frameId == frame_list[j] & current_play_pass$sideOfBall == "offense" & current_play_pass$displayName != "Football",]
      def <- current_play_pass[current_play_pass$frameId == frame_list[j] & current_play_pass$sideOfBall == "defense" & current_play_pass$displayName != "Football",]
      for (i in 1:nrow(def)) {
        current_play_pass$closestOffPlayer[current_play_pass$nflId == def$nflId[i] & current_play_pass$frameId == frame_list[j]] <- off$jerseyNumber[which.min(sqrt((off[,2]-def[i,2])^2 + (off[,3]-def[i,3])^2))]
        a <- sqrt((def[-i,2]-def[i,2])^2 + (def[-i,3]-def[i,3])^2)
        anum <- which.min(a)
        index <- if(anum > i) {
          anum + 1
        } else{anum}
        current_play_pass$closestDefPlayer[current_play_pass$nflId == def$nflId[i] & current_play_pass$frameId == frame_list[j]] <- def$jerseyNumber[index]
      }
      frame <- current_play_pass[current_play_pass$frameId == frame_list[j] & current_play_pass$displayName != "Football",]
      for (k in 1:nrow(frame)) {
        if (frame$sideOfBall[k] == "defense") {
          current_play_pass$o_diff[current_play_pass$nflId == frame$nflId[k] & current_play_pass$frameId == frame_list[j]] <- frame$o[k] - frame$o[frame$jerseyNumber == frame$closestOffPlayer[k] & !is.na(frame$jerseyNumber)  & frame$sideOfBall == "offense"]
          current_play_pass$dir_diff[current_play_pass$nflId == frame$nflId[k] & current_play_pass$frameId == frame_list[j]] <- frame$dir[k] - frame$dir[frame$jerseyNumber == frame$closestOffPlayer[k] & !is.na(frame$jerseyNumber)  & frame$sideOfBall == "offense"]
          current_play_pass$dist_to_off[current_play_pass$nflId == frame$nflId[k] & current_play_pass$frameId == frame_list[j]] <- sqrt((frame$x[k]-frame$x[frame$jerseyNumber == frame$closestOffPlayer[k] & !is.na(frame$jerseyNumber) & frame$sideOfBall == "offense"])^2 + (frame$y[k]-frame$y[frame$jerseyNumber == frame$closestOffPlayer[k] & !is.na(frame$jerseyNumber) & frame$sideOfBall == "offense"])^2)
          current_play_pass$dist_to_def[current_play_pass$nflId == frame$nflId[k] & current_play_pass$frameId == frame_list[j]] <- sqrt((frame$x[k]-frame$x[frame$jerseyNumber == frame$closestDefPlayer[k] & !is.na(frame$jerseyNumber) & frame$sideOfBall == "defense"])^2 + (frame$y[k]-frame$y[frame$jerseyNumber == frame$closestDefPlayer[k] & !is.na(frame$jerseyNumber) & frame$sideOfBall == "defense"])^2)
          current_play_pass$dist_nearoffdef[current_play_pass$nflId == frame$nflId[k] & current_play_pass$frameId == frame_list[j]] <- sqrt((frame$x[frame$jerseyNumber == frame$closestOffPlayer[k] & !is.na(frame$jerseyNumber)  & frame$sideOfBall == "offense"]-frame$x[frame$jerseyNumber == frame$closestDefPlayer[k] & !is.na(frame$jerseyNumber)  & frame$sideOfBall == "defense"])^2 + (frame$y[frame$jerseyNumber == frame$closestOffPlayer[k] & !is.na(frame$jerseyNumber)  & frame$sideOfBall == "offense"]-frame$y[frame$jerseyNumber == frame$closestDefPlayer[k] & !is.na(frame$jerseyNumber)  & frame$sideOfBall == "defense"])^2)
        } else {
          current_play_pass$o_diff[current_play_pass$nflId == frame$nflId[k] & current_play_pass$frameId == frame_list[j]] <- NA
          current_play_pass$dir_diff[current_play_pass$nflId == frame$nflId[k] & current_play_pass$frameId == frame_list[j]] <- NA
          current_play_pass$dist_to_off[current_play_pass$nflId == frame$nflId[k] & current_play_pass$frameId == frame_list[j]] <- NA
          current_play_pass$dist_to_def[current_play_pass$nflId == frame$nflId[k] & current_play_pass$frameId == frame_list[j]] <- NA
          current_play_pass$dist_nearoffdef[current_play_pass$nflId == frame$nflId[k] & current_play_pass$frameId == frame_list[j]] <- NA
        }
      }
    }
    week_playervar <- rbind(week_playervar, current_play_pass)}, error = function(e) { skip_to_next <<- TRUE},
    warning = function(w) { skip_to_next <<- TRUE})
    if(skip_to_next) {next}
  }
  
  #week_playervar_copy <- week_playervar
  
  week_playervar$coverage <- NA
  
  playOutcomeEvents <- c('pass_outcome_caught',
                         'pass_outcome_incomplete',
                         'pass_outcome_interception',
                         'pass_outcome_touchdown'
  )
  
  column_names <- c("gameId", "playId", "down", "yardsToGo", "yardlineNumber", "defendersInTheBox",
                    "numberOfPassRushers", "nflId", "displayName", "var_x", "var_y", "speed_var",
                    "off_var", "def_var", "off_mean", "def_mean", "off_dir_var", "off_o_var",
                    "off_dir_mean", "off_o_mean", "rat_mean", "rat_var", "player_coverage")
  
  
  defender_stat_tbl <- as.data.frame(matrix(data = NA, nrow = 0, ncol = 23))
  colnames(defender_stat_tbl) <- column_names
  
  defender_stat_tbl <- week_playervar %>% filter(sideOfBall == "defense" & displayName != "Football") %>% group_by(gameId, playId, nflId) %>% 
    summarise(down = down[1], yardsToGo = yardsToGo[1], yardlineNumber = yardlineNumber[1], defendersInTheBox = defendersInTheBox[1],
              numberOfPassRushers = numberOfPassRushers[1], displayName = displayName[1], week = week[1], var_x = var(x), var_y = var(y),
              speed_var = var(s), off_var = var(dist_to_off), def_var = var(dist_to_def),
              off_mean = mean(dist_to_off), def_mean = mean(dist_to_def), off_dir_var = var(dir_diff),
              off_o_var = var(o_diff), off_dir_mean = mean(dir_diff), off_o_mean = mean(o_diff),
              rat_mean = mean(dist_to_off/dist_nearoffdef), rat_var = var(dist_to_off/dist_nearoffdef))
  
  
  run_def_xgb <- as.matrix(defender_stat_tbl[,c(4:8,11:23)])
  defender_stat_tbl$pred_is_zone <- predict(def_xgb, run_def_xgb)
  defender_stat_tbl$pred_is_zone_conv <- ifelse(defender_stat_tbl$pred_is_zone > 0.50, 1, 0)
  
  defender_stat_tbl_man <- defender_stat_tbl %>% filter(pred_is_zone_conv == 0)
  
  
  week_playervar <- left_join(week_playervar, targeted, by = c("playId", "gameId"), suffix = c("", "_new"))
  week_playervar <- week_playervar %>% group_by(nflId, playId) %>% mutate(targeted_num = jerseyNumber[nflId == targetNflId][1])
  week_playervar <- week_playervar %>% group_by(gameId, playId) %>% fill(targeted_num, .direction = "updown")
  
  week_playervar <- left_join(week_playervar, defender_stat_tbl_man, by = c("playId", "gameId", "nflId"), suffix = c("", "_new"))
  
  week_playervar <- week_playervar %>% group_by(gameId, playId, nflId) %>%
    mutate(covered_recId = closestOffPlayer[grepl("outcome", event, fixed = TRUE)][1],
           targeted = ifelse(targeted_num[1] == covered_recId , 1, 0),
           snap_to_pass_t = frameId[event == "pass_forward"][1] - frameId[event == "ball_snap"][1],
           pass_to_arrival_t = frameId[grepl("outcome", event, fixed = TRUE)][1] - frameId[event == "pass_forward"][1], 
           opp_sideOfBall = ifelse(sideOfBall == "offense", "defense", "offense"))
  
  week_playervar_cov <- week_playervar %>% ungroup() %>% select(jerseyNumber, x, y, s, dir, o, gameId, playId, opp_sideOfBall, frameId) %>% drop_na()
  week_playervar <- left_join(week_playervar, week_playervar_cov, by = c("covered_recId" = "jerseyNumber", "gameId" = "gameId", "playId" = "playId", "sideOfBall" = "opp_sideOfBall", "frameId" = "frameId"), suffix = c("", "_cov"))
  week_playervar_football <- week_playervar %>% ungroup() %>% filter(team == "football") %>% select(x, y, gameId, playId, frameId)
  week_playervar <- left_join(week_playervar, week_playervar_football, by = c("gameId", "playId", "frameId"), suffix = c("", "_fb"))
  
  week_calculated <- week_playervar %>% ungroup() %>% group_by(gameId, playId, nflId) %>% 
    summarise(sep_at_pass = sqrt((x[event == "pass_forward"][1] - x_cov[event == "pass_forward"][1])^2 + 
                                   (y[event == "pass_forward"][1] - y_cov[event == "pass_forward"][1])^2),
              sep_at_arrival = sqrt((x[grepl("outcome", event, fixed = TRUE)][1] - x_cov[grepl("outcome", event, fixed = TRUE)][1])^2 + 
                                      (y[grepl("outcome", event, fixed = TRUE)][1] - y_cov[grepl("outcome", event, fixed = TRUE)][1])^2),
              sep_delta = sep_at_arrival - sep_at_pass,
              rec_speed_pass = s_cov[event == "pass_forward"][1],
              rec_speed_arrival = s_cov[grepl("outcome", event, fixed = TRUE)][1],
              rec_speed_delta = rec_speed_arrival - rec_speed_pass,
              rec_dir_pass = dir_cov[event == "pass_forward"][1],
              rec_dir_arrival = dir_cov[grepl("outcome", event, fixed = TRUE)][1],
              rec_dir_delta = rec_dir_arrival - rec_dir_pass,
              rec_o_pass = o_cov[event == "pass_forward"][1],
              rec_o_arrival = o_cov[grepl("outcome", event, fixed = TRUE)][1],
              rec_o_delta = rec_o_arrival - rec_o_pass,
              ball_rec_dist_pass = sqrt((x_fb[event == "pass_forward"][1] - 
                                           x_cov[event == "pass_forward"][1])^2 + 
                                          (y_fb[event == "pass_forward"][1] - 
                                             y_cov[event == "pass_forward"][1])^2),
              ball_rec_dist_arrival = sqrt((x_fb[grepl("outcome", event, fixed = TRUE)][1] - 
                                              x_cov[grepl("outcome", event, fixed = TRUE)][1])^2 + 
                                             (y_fb[grepl("outcome", event, fixed = TRUE)][1] - 
                                                y_cov[grepl("outcome", event, fixed = TRUE)][1])^2),
              ball_def_dist_pass = sqrt((x_fb[event == "pass_forward"][1] - 
                                           x[event == "pass_forward"][1])^2 + 
                                          (y_fb[event == "pass_forward"][1] - 
                                             y[event == "pass_forward"][1])^2),
              ball_def_dist_arrival = sqrt((x_fb[grepl("outcome", event, fixed = TRUE)][1] - 
                                              x[grepl("outcome", event, fixed = TRUE)][1])^2 + 
                                             (y_fb[grepl("outcome", event, fixed = TRUE)][1] - 
                                                y[grepl("outcome", event, fixed = TRUE)][1])^2),
              snap_frame = frameId[event == "ball_snap"][1],
              pass_frame = frameId[event == "pass_forward"][1],
              arrival_frame = frameId[grepl("outcome", event, fixed = TRUE)][1],
              is_complete = ifelse(passResult == "C", 1, 0)[1],
              is_targeted = ifelse(targeted_num == covered_recId, 1, 0)[1]) 
  
  week_distances_calculated <- week_playervar %>% ungroup() %>% group_by(gameId, playId, nflId) %>% arrange(gameId, playId, frameId, nflId) %>%
    mutate(snap_frame = frameId[event == "ball_snap"][1],
           pass_frame = frameId[event == "pass_forward"][1],
           arrival_frame = frameId[grepl("outcome", event, fixed = TRUE)][1],
           prev_def_x_pass = ifelse(frameId >= snap_frame & frameId <= pass_frame, lag(x), NA),
           prev_def_y_pass = ifelse(frameId >= snap_frame & frameId <= pass_frame, lag(y), NA),
           prev_off_x_pass = ifelse(frameId >= snap_frame & frameId <= pass_frame, lag(x_cov), NA),
           prev_off_y_pass = ifelse(frameId >= snap_frame & frameId <= pass_frame, lag(y_cov), NA),
           prev_def_x_arrival = ifelse(frameId >= pass_frame & frameId <= arrival_frame, lag(x), NA),
           prev_def_y_arrival = ifelse(frameId >= pass_frame & frameId <= arrival_frame, lag(y), NA),
           prev_off_x_arrival = ifelse(frameId >= pass_frame & frameId <= arrival_frame, lag(x_cov), NA),
           prev_off_y_arrival = ifelse(frameId >= pass_frame & frameId <= arrival_frame, lag(y_cov), NA)) %>% 
    summarise(cum_def_dist_pass = sum(sqrt((x - prev_def_x_pass)^2 + (y - prev_def_y_pass)^2), na.rm = TRUE),
              cum_off_dist_pass = sum(sqrt((x_cov - prev_off_x_pass)^2 + (y_cov - prev_off_y_pass)^2), na.rm = TRUE),
              cum_def_dist_arrival = sum(sqrt((x - prev_def_x_arrival)^2 + (y - prev_def_y_arrival)^2), na.rm = TRUE),
              cum_off_dist_arrival = sum(sqrt((x_cov - prev_off_x_arrival)^2 + (y_cov - prev_off_y_arrival)^2), na.rm = TRUE),
              cum_def_dist_delta = cum_def_dist_arrival - cum_def_dist_pass,
              cum_off_dist_delta = cum_off_dist_arrival - cum_off_dist_pass)
  
  week_calculated_final <- left_join(week_calculated, week_distances_calculated, by = c("gameId", "playId", "nflId"))
  week_defender_final <- left_join(defender_stat_tbl_man, week_calculated_final, by = c("gameId", "playId", "nflId"))
  
  week_playervar_grouped <- week_playervar %>% group_by(gameId, playId, nflId) %>% 
    summarise(covered_recId = closestOffPlayer[grepl("outcome", event, fixed = TRUE)][1],
              targeted = ifelse(targeted_num[1] == covered_recId , 1, 0),
              snap_to_pass_t = frameId[event == "pass_forward"][1] - frameId[event == "ball_snap"][1],
              pass_to_arrival_t = frameId[grepl("outcome", event, fixed = TRUE)][1] - frameId[event == "pass_forward"][1])
  
  week_defender_final <- left_join(week_defender_final, week_playervar_grouped, by = c("gameId", "playId", "nflId"))
  
  
  
  ### targeted probability
  run_targ_xgb <- as.matrix(week_defender_final[,c(4:8,55,26,29,32,35,38,40,47,48)])
  week_defender_final$is_targ_pct <- predict(targ_xgb, run_targ_xgb)
  week_defender_final$is_targ_pct_conv <- ifelse(week_defender_final$is_targ_pct > 0.50, 1, 0)
  
  
  week_defender_final_targeted <- week_defender_final %>% filter(is_targ_pct_conv == 1)
  # completion prob 
  run_comp_xgb <- as.matrix(week_defender_final_targeted[,c(4:8,55,56,26:41,47:52)])
  week_defender_final_targeted$is_comp_pct <- predict(comp_xgb, run_comp_xgb)
  week_defender_final_targeted$is_comp_pct_conv <- ifelse(week_defender_final_targeted$is_comp_pct > 0.50, 1, 0)
  
  week_defender_final_targeted <- week_defender_final_targeted %>% group_by(gameId, playId, nflId) %>% summarise(is_comp_pct = is_comp_pct, is_comp_pct_conv = is_comp_pct_conv)
  
  week_defender_final <- left_join(week_defender_final, week_defender_final_targeted, by = c("gameId", "playId", "nflId"))
  
  # write.csv(week_defender_final, file = paste(weeks_list[iter], "_defender_final.csv", sep = ""))
  # write.csv(week_playervar, file = paste(weeks_list[iter], "_playervar.csv", sep = ""))
  
  write.csv(week_defender_final, file = paste(weeks_list[iter], "_defender_final.csv", sep = ""))
  write.csv(week_playervar, file = paste(weeks_list[iter], "_playervar.csv", sep = ""))
}

##########
#########
#######

# retain targeted and completion models and apply to overall dataset
week1 <- read.csv("week1_defender_final.csv")
week2 <- read.csv("week2_defender_final.csv")
week3 <- read.csv("week3_defender_final.csv")
week4 <- read.csv("week4_defender_final.csv")
week567 <- read.csv("week567_defender_final.csv")
week8 <- read.csv("week8_defender_final.csv")
week9 <- read.csv("week9_defender_final.csv")
week10 <- read.csv("week10_defender_final.csv")

allweeks <- rbind(week1, week2, week3, week4, week567, week8, week9, week10)




### targeted probability
defender_stat_tbl <- allweeks %>% filter(!is.na(is_targeted))
sample_targ_new <- sample(nrow(defender_stat_tbl), nrow(defender_stat_tbl)*0.7, replace = FALSE)
train_targ_new <- defender_stat_tbl[sample_targ_new,]
test_targ_new <- defender_stat_tbl[-sample_targ_new,]

train_targ_new <- train_targ_new[,-1]
train_targ_xgb_new <- as.matrix(train_targ_new[,c(4:8,55,26,29,32,35,38,40,47,48)])
train_targ_new_xgb_lbl <- as.matrix(train_targ_new$is_targeted)
targ_xgb_new <- xgboost::xgboost(data = train_targ_xgb_new, label = train_targ_new_xgb_lbl, objective = "binary:logistic", nrounds = 5000)

test_targ_new <- test_targ_new[,-1]
run_targ_xgb_new <- as.matrix(test_targ_new[,c(4:8,55,26,29,32,35,38,40,47,48)])
test_targ_new$is_targ_pct <- predict(targ_xgb_new, run_targ_xgb_new)
test_targ_new$is_targ_pct_conv <- ifelse(test_targ_new$is_targ_pct > 0.50, 1, 0)


AUC(test_targ_new$is_targ_pct, test_targ_new$is_targeted)
LogLoss(test_targ_new$is_targ_pct, test_targ_new$is_targeted)

confusionmatrix = as.matrix(table(Actual = test_targ_new$is_targeted, Predicted = test_targ_new$is_targ_pct_conv))
accuracy = sum(diag(confusionmatrix))/length(test_targ_new$is_targeted) #85.5


# run prediction on whole dataframe
defender_stat_tbl <- defender_stat_tbl[,-1]
dst_targ_xgb <- as.matrix(defender_stat_tbl[,c(4:8,55,26,29,32,35,38,40,47,48)])
defender_stat_tbl$is_targ_pct <- predict(targ_xgb_new, dst_targ_xgb)
defender_stat_tbl$is_targ_pct_conv <- ifelse(defender_stat_tbl$is_targ_pct > 0.50, 1, 0)





# completion
defender_stat_tbl_targeted <- defender_stat_tbl %>% filter(is_targ_pct_conv == 1)
sample_comp_new <- sample(nrow(defender_stat_tbl_targeted), nrow(defender_stat_tbl_targeted)*0.7, replace = FALSE)
train_comp_new <- defender_stat_tbl_targeted[sample_comp_new,]
test_comp_new <- defender_stat_tbl_targeted[-sample_comp_new,]

train_comp_xgb_new <- as.matrix(train_comp_new[,c(4:8,55,56,26:41,47:52)])
train_comp_new_xgb_lbl <- as.matrix(train_comp_new$is_complete)
comp_xgb_new <- xgboost::xgboost(data = train_comp_xgb_new, label = train_comp_new_xgb_lbl, objective = "binary:logistic", nrounds = 5000)

# completion prob 
run_comp_xgb_new <- as.matrix(test_comp_new[,c(4:8,55,56,26:41,47:52)])
test_comp_new$is_comp_pct <- predict(comp_xgb_new, run_comp_xgb_new)
test_comp_new$is_comp_pct_conv <- ifelse(test_comp_new$is_comp_pct > 0.50, 1, 0)

# completed
AUC(test_comp_new$is_comp_pct, test_comp_new$is_complete)
LogLoss(test_comp_new$is_comp_pct, test_comp_new$is_complete)

confusionmatrix = as.matrix(table(Actual = test_comp_new$is_complete, Predicted = test_comp_new$is_comp_pct_conv))
accuracy = sum(diag(confusionmatrix))/length(test_comp_new$is_complete) #91.3


# run prediction on whole dataframe
dst_comp_xgb <- as.matrix(defender_stat_tbl_targeted[,c(4:8,55,56,26:41,47:52)])
defender_stat_tbl_targeted$is_comp_pct <- predict(comp_xgb_new, dst_comp_xgb)
defender_stat_tbl_targeted$is_comp_pct_conv <- ifelse(defender_stat_tbl_targeted$is_comp_pct > 0.50, 1, 0)



#######
###
# combine dataframes for final df
dst_targeted <- defender_stat_tbl_targeted %>% select(nflId, gameId, playId, is_comp_pct, is_comp_pct_conv)
dst_adjust <- defender_stat_tbl[,1:58]
dst <- left_join(dst_adjust, dst_targeted, by = c("nflId", "gameId", "playId"))




# man/zone
AUC(test_def$pred_is_zone, test_def$is_zone)
LogLoss(test_def$pred_is_zone, test_def$is_zone)

# targeted
AUC(test_targ$pred_is_targ, test_targ$targeted)
LogLoss(test_targ$pred_is_targ, test_targ$targeted)

# completed
AUC(test_comp$pred_is_comp, test_comp$is_complete)
LogLoss(test_comp$pred_is_comp, test_comp$is_complete)




week3_playervar_tbl <- read.csv("week3_playervar.csv")

pointtbl <- dst %>% filter(is_targeted == 1, is_comp_pct_conv == 1, is_complete == 1, sep_at_arrival > 8)


example_play <- week3_playervar_tbl %>%
  filter(gameId == "2018092310", playId == "904", nflId == "2557927" | nflId == "2540169" | displayName == "Football")

example_play$nflId <- as.factor(example_play$nflId)


xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3


plot_title <- str_trim(gsub("\\s*\\([^\\)]+\\)","",as.character(example_play$playDescription[1])))

# Specific boundaries for a given play
ymin <- max(round(min(example_play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example_play$x, na.rm = TRUE) + 10, -1), 120)

#hash marks
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)





woodsking <- example_play %>% ggplot( aes(x=(xmax-y), y=x, group=displayName, color=displayName)) +
  #adding hash marks
  annotate("text", x = df.hash$x[df.hash$x < 55/2], 
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 55/2], 
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  
  #adding yard lines
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  
  #adding field yardline text
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  
  #adding field exterior
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") +
  geom_line() +
  geom_point() +
  ylim(ymin, ymax) + 
  coord_fixed() +
  
  #applying theme
  theme(plot.title = element_text(size = 12)) +
  transition_reveal(frameId) + 
  
  #titling plot with play description
  labs(title = plot_title) +
  
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank(),
        text = element_text(family = "DINCondensed-Bold")) +
  
  scale_color_manual(values = c("#0080C6", "#654321", "#003594"))



anim_save("WoodsKingPlot.gif", woodsking)


example_play_line <- example_play %>% filter(nflId == "2557927")

example_play_line %>%
  ggplot(aes(x = dist_to_off, y = frameId)) + geom_path(size = 1.2) +
  geom_hline(yintercept =  43, linetype = "dashed", color = "blue") +
  geom_text(aes(x = 8, y = 43, label = "Pass Forward", vjust = -1, family = "DINCondensed-Bold"), size = 4.5, color = "blue", hjust = 1.2, inherit.aes = T, check_overlap = T) +
  coord_flip() + xlab("Distance from Defender to Receiver (yards)") + ylab("Play Frame") + 
  ggtitle(plot_title) + 
  theme(legend.title = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), text = element_text(family = "DINCondensed-Bold")) +
  guides(color = guide_legend(reverse=TRUE))








### ranking players 
league_prob_tbl <- dst %>% group_by(nflId) %>%
  summarise(snaps = n(), targets = sum(is_targ_pct_conv, na.rm = T), ATP = mean(is_targ_pct, na.rm = T), 
            completions = sum(is_complete, na.rm = T), ACP = mean(is_comp_pct, na.rm = T))

#league_prob_tbl <- league_prob_tbl %>% left_join(players, by = "nflId") %>% filter(snaps >= 60)
#write.csv(league_prob_tbl, file = "league_prob_wk1_10.csv")

## NEED TO QUALIFY PLAYERS AFTER ALL SEASON DATA IS CALCULATED

mean(league_prob_tbl$ATP, na.rm = T)
mean(league_prob_tbl$ACP, na.rm = T)

league_prob_tbl$wATP <- league_prob_tbl$ATP - 0.1978772
league_prob_tbl$wACP <- league_prob_tbl$ACP - 0.5738365
league_prob_tbl$wATCP <- -(league_prob_tbl$wATP + league_prob_tbl$wACP) + 0.04581672
#mean(league_prob_tbl$wATCP, na.rm = T)


league_prob_tbl_qual <- league_prob_tbl %>% filter(snaps >= 60) %>% arrange(desc(wATCP))
league_prob_tbl_qual <- league_prob_tbl_qual %>% left_join(players, by = "nflId")

write.csv(league_prob_tbl_qual, file = "league_wATCOE_wk1_10.csv")


