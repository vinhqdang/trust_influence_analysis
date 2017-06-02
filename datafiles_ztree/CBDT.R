source ("ProcessData.R")
zTT <- readMultiXLS ("./all_data")
SBJs <- processMultiSBJ ("./all_data")

# Create my sending proportional
zTT[2]$subjects$my_send_proportional <- ifelse(zTT[2]$subjects$Type == 0, zTT[2]$subjects$Contribution/10, ifelse(zTT[2]$subjects$PartnerDecision > 0, zTT[2]$subjects$Contribution/3/zTT[2]$subjects$PartnerDecision, -1))

zTT[2]$subjects$CurrGameProfit <- ifelse(zTT[2]$subjects$Type == 0, zTT[2]$subjects$PartnerDecision - zTT[2]$subjects$Contribution, zTT[2]$subjects$PartnerDecision*3 - zTT[2]$subjects$Contribution)

# Numbers of user of a group
num_users = 6
# Number of rounds each user play to each other
average_rounds = 5
# Number of games for each group 
num_games = 4
# Number of rounds for each game (should be 25)
num_rounds_per_game = (num_users - 1) * average_rounds 
# Number of rounds for each experiment (should be 100, because we have 4 games)
num_rounds_per_exp = num_rounds_per_game * num_games
# Number of experiments (it is 5 at the time of writing, 
# but can be increased if we organize more experiments)
num_exp = nrow (zTT[1]$globals) / num_rounds_per_exp

Type_names = c("SENDER", "RECEIVER")
SIMPLE_GAME_ORDERS =      c(3,2,4,1,2)
ID_GAME_ORDERS =          c(1,4,1,2,3)
SCORE_GAME_ORDERS =       c(2,1,3,4,4)
COMBINE_GAME_ORDERS =     c(4,3,2,3,1)

# Numbers of user of a group
num_users = 6
# Number of rounds each user play to each other
average_rounds = 5
# Number of games for each group 
num_games = 4
# Number of rounds for each game (should be 25)
num_rounds_per_game = (num_users - 1) * average_rounds 
# Number of rounds for each experiment (should be 100, because we have 4 games)
num_rounds_per_exp = num_rounds_per_game * num_games
# Number of experiments (it is 5 at the time of writing, 
# but can be increased if we organize more experiments)
num_exp = nrow (zTT[1]$globals) / num_rounds_per_exp

Type_names = c("SENDER", "RECEIVER")
SIMPLE_GAME_ORDERS =      c(3,2,4,1,2)
ID_GAME_ORDERS =          c(1,4,1,2,3)
SCORE_GAME_ORDERS =       c(2,1,3,4,4)
COMBINE_GAME_ORDERS =     c(4,3,2,3,1)


simple_games <- zTT[2]$subjects[0,]
id_games <- zTT[2]$subjects[0,]
score_games <- zTT[2]$subjects[0,]
combine_games <- zTT[2]$subjects[0,]

for (exp_id in 1:num_exp) {
  first_round_of_exp_globals = (exp_id - 1) * num_rounds_per_exp + 1
  last_round_of_exp_globals = exp_id * num_rounds_per_exp
  globals_of_exp = zTT[1]$globals[first_round_of_exp_globals:last_round_of_exp_globals,]
  
  first_round_of_exp_subjects = (exp_id - 1) * num_rounds_per_exp * num_users + 1
  last_round_of_exp_subjects = exp_id * num_rounds_per_exp * num_users
  subjects_of_exp = zTT[2]$subjects[first_round_of_exp_subjects:last_round_of_exp_subjects,]
  
  SIMPLE_GAME_ORDER = globals_of_exp[1,]$SIMPLE_GAME
  ID_GAME_ORDER = globals_of_exp[1,]$ID_GAME
  SCORE_GAME_ORDER = globals_of_exp[1,]$SCORE_GAME
  COMBINE_GAME_ORDER = globals_of_exp[1,]$COMBINE_GAME
  
  simple_games <- rbind(simple_games, subjects_of_exp[((SIMPLE_GAME_ORDER - 1) * 
                                                         num_rounds_per_game * num_users + 1): 
                                                        (SIMPLE_GAME_ORDER * num_rounds_per_game * num_users),])
  id_games <- rbind(id_games, subjects_of_exp[((ID_GAME_ORDER - 1) * 
                                                 num_rounds_per_game * num_users + 1): 
                                                (ID_GAME_ORDER * num_rounds_per_game * num_users),])
  score_games <- rbind(score_games, subjects_of_exp[((SCORE_GAME_ORDER - 1) * 
                                                       num_rounds_per_game * num_users + 1): 
                                                      (SCORE_GAME_ORDER * num_rounds_per_game * num_users),])
  combine_games <- rbind(combine_games, subjects_of_exp[((COMBINE_GAME_ORDER - 1) * 
                                                           num_rounds_per_game * num_users + 1): 
                                                          (COMBINE_GAME_ORDER * num_rounds_per_game * num_users),])
}

# simFunction calculate similarity between two scenarios
# sim (p,q) = 1/ (e ^ d(p,q))
# d(p,q) = distance between p and q
simFunction = function (v1, v2)
{
   1 / (exp(1) ^ dist(rbind(v1,v2)))
}

average_send_proportion_over_time = function (game_data) {
  res = c()
  for (round_number in 1:24) {
    average_send_proportion = mean (game_data[game_data$Period %% 25 == round_number &
                                                game_data$send_proportion >= 0,]$send_proportion)
    res = c (res, average_send_proportion)
  }
  average_send_proportion = mean (game_data[game_data$Period %% 25 == 0 &
                                              game_data$send_proportion >= 0,]$send_proportion)
  res = c (res, average_send_proportion)
  res
}

plot_average_send_proportion_over_time = function () {
  res1 = average_send_proportion_over_time(simple_games)
  res2 = average_send_proportion_over_time(id_games)
  res3 = average_send_proportion_over_time(score_games)
  res4 = average_send_proportion_over_time(combine_games)
  
  plot (res1, ylim = c(0,1), type = "o", col="red", pch = 1, xlim = c(1,25),
        ylab = "Sending proportion", xlab = "Round number", cex.lab=1.35, cex.axis=1.35,
        xaxt = "n")
  axis (1, at =c(1,5,10,15,20,25), labels = c(1,5,10,15,20,25), cex.axis=1.5)
  lines (res2, type = "o", col="blue", pch = 2, lty = 2)
  lines (res3, type = "o", col="black", pch = 7, lty = 3)
  lines (res4, type = "o", col="violet", pch = 4, lty = 1)
  
  legend (17,1, c("Simple Game","Partner Identity Game","Partner Information Game","Combine Game"),
          lty = c(1,2,3,1), pch = c(1,2,7,4),
          col = c("red","blue","black","violet"))
}