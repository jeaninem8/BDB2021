# BDB graphing
setwd("/Volumes/My Passport/BigDataBowl21/nfl-big-data-bowl-2021")


week2_defender_tbl <- read.csv("week2_defender_final.csv")
week2_playervar_tbl <- read.csv("week2_playervar.csv")

# General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3



week2_defender_tbl %>% filter(is_targeted == 1, is_comp_pct_conv == 0, is_complete == 1, sep_at_arrival < 1)



# example_play <- week2_playervar_tbl %>%
#   filter(gameId == "2018090901", playId == "977", nflId == "2543720" | nflId == "2552633" | displayName == "Football")

example_play <- week2_playervar_tbl %>%
     filter(gameId == "2018091606", playId == "2418", nflId == "2552689" | nflId == "2543468" | displayName == "Football")

example_play2 <- week2_playervar_tbl %>%
  filter(gameId == "2018092303", playId == "1115", nflId == "2540180" | nflId == "2543468" | displayName == "Football")



example_play$nflId <- as.factor(example_play$nflId)


cols_fill <- c("#a71930", "#663300", "#a5acaf")
cols_col <- c("#004C54", "#663300", "#a5acaf")

plot_title <- str_trim(gsub("\\s*\\([^\\)]+\\)","",as.character(example_play$playDescription[1])))

# Specific boundaries for a given play
ymin <- max(round(min(example_play_plottbl$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example_play_plottbl$x, na.rm = TRUE) + 10, -1), 120)

#hash marks
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)




evansdarby <- example_play %>% ggplot( aes(x=(xmax-y), y=x, group=displayName, color=displayName)) +
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
  
  scale_color_manual(values = c("#654321", "red", "#006a4e"))
  
  

anim_save("EvansDarbyPlot.gif", evansdarby)






# separation time plot

example_play_line <- example_play %>% filter(nflId == "2552689" & frameId <= frameId[event == "pass_arrived"]) %>% 
  mutate(event_color = ifelse(frameId <= 34, "Prior to Pass", "After Pass"))

example_play_line$event_color <- as.factor(example_play_line$event_color)

test <- example_play_line %>%
  ggplot(aes(x = dist_to_off, y = frameId)) + geom_path(size = 1.2) +
  geom_hline(yintercept =  34, linetype = "dashed", color = "blue") +
  geom_text(aes(x = 7.5, y = 34, label = "Pass Forward", vjust = -1, family = "DINCondensed-Bold"), size = 4.5, color = "blue", hjust = -0.2, inherit.aes = T, check_overlap = T) +
  coord_flip() + xlab("Distance from Defender to Receiver (yards)") + ylab("Play Frame") + 
  ggtitle(plot_title) + 
  theme(legend.title = element_blank(), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), text = element_text(family = "DINCondensed-Bold")) +
  guides(color = guide_legend(reverse=TRUE))







xgb_help <- xgboost::xgboost(data = train_comp_xgb, label = train_comp_xgb_lbl, objective = "binary:logistic", nrounds = 5000, eval_metric = "rmse")
test_comp$pred_is_comp <- predict(comp_xgb, test_comp_xgb)
test_comp$pred_is_comp_conv <- ifelse(test_comp$pred_is_comp > 0.50, 1, 0)

AUC(test_comp$pred_is_comp, test_comp$is_complete)
LogLoss(test_comp$pred_is_comp, test_comp$is_complete)






### ranking players 
league_prob_tbl <- week1_defender_tbl %>% group_by(nflId) %>%
  summarise(snaps = n(), targets = sum(is_targ_pct_conv, na.rm = T), ATP = mean(is_targ_pct, na.rm = T), 
            completions = sum(is_complete, na.rm = T), avg_comp_pct = mean(is_comp_pct, na.rm = T), CPOE = mean(is_complete - is_comp_pct, na.rm = T))

## NEED TO QUALIFY PLAYERS AFTER ALL SEASON DATA IS CALCULATED

mean(league_prob_tbl$ATP, na.rm = T)
mean(league_prob_tbl$CPOE, na.rm = T)

ATP_weight <- 50/mean(league_prob_tbl$ATP, na.rm = T)
CPOE_weight <- 50/mean(league_prob_tbl$CPOE, na.rm = T)

league_prob_tbl$wATCOE <- ATP_weight*league_prob_tbl$ATP + CPOE_weight*league_prob_tbl$CPOE
  






