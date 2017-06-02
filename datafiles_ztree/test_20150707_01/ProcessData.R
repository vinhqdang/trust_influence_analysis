loadZTree <- function (XLS_file) {
  source("http://www.kirchkamp.de/lab/zTree.R")
  zTreeTables (XLS_file)
}

loadZTreeQuestionnaire <- function(SBJ_file) {
  source("http://www.kirchkamp.de/lab/zTree.R")
  zTreeSbj (XLS_file)
}

extractSubjectsData <- function (ZTT) {
  subjects <- ZTT[2]$subjects
  names (subjects) <- sub ('\\]', "", names (subjects))
  names (subjects) <- sub ('\\[', "", names (subjects))
  subjects
}

getSubjects <- function (XLS_filename) {
  s <- loadZTree (XLS_filename)[2]$subjects
  names (s) <- sub ('\\]', "", names (s))
  names (s) <- sub ('\\[', "", names (s))
  s
}

#extract a subgame (e.g: Combine game) from full data
#subjects: subjects data frame, usually a result of extractSubjectData function
#first_round: the index of the last period of previous game
#last_round: the index of the last period of the subgame
#e.g: if you want to extract the first game which plays from period 1 to 25, the two arguments should be 0 and 25
#num_users: number of participants
extractSubGame <- function (subjects, first_round, last_round, num_users) {
  first_row = first_round * num_users + 1
  last_row = last_round * num_users
  subjects [first_row:last_row,]
}

#show the action of the partner in the point of view of the subject
#the function will plot sending action and trust score of the partner on the subject
#directory: directory to save plot images
showOneView <- function (game,subject, partner, directory = ".", game_name = "")
{
  subgame1 <- game [game$Subject == subject & game$Partner == partner,]
  #subgame2 <- game [game$Subject == subject & game$Partner == partner,]
  if (nrow(subgame1) > 0) {
    periods <- c (1:nrow(subgame1)+1)
    
    #par (xpd = TRUE, mar = par()$mar +c(0,0,0,6))
    
    png (paste (directory,"/",game_name,"_TrustOnAction_",partner,"_on_", subject,".png", sep = ""))
    
    plot (subgame1$showing_trust_score, col = "red", type = "o", ylim = c(-1,1), xlim = c(1,20),xlab = "Period",
          main = paste("subject: ", partner, " on subject ", subject, "'s view-game: ", game_name),
          ylab = "Trust score and percentage of sending")
    
    lines (periods, subgame1$send_proportion, col = "blue", type = "o")
    legend ("topright",legend = c("Trust score", "Send amount"), col = c("red", "blue"), lty = 1)
    #legend ("bottomright", legend = c(paste("Period: ", subgame1$Period), 
     #                                 paste("Cor = ",cor(subgame1$showing_trust_score[2:length(subgame1$showing_trust_score)], 
      #                                                   subgame2$send_proportion[1:length(subgame2$send_proportion) - 1]))))
    legend ("bottomright", legend = c(paste("Period: ", subgame1$Period), 
                                      paste("Cor = ",cor(subgame1$showing_trust_score, 
                                                         subgame1$send_proportion))),
            bg = "transparent")
    dev.off ()
  }
}

#show all avaiable pairs of users
showAllPairs <- function (game, num_users = 6, directory = ".", game_name = "") {
  for (i in 1:num_users) {
    for (j in 1:num_users) {
      if (i != j) {
        showOneView (game, i, j, directory = directory, game_name = game_name)
      }
    }
  }
}

showOneActionOnTrustScore <- function (game,subject, partner, directory = ".", game_name = "")
{
  subgame1 <- game [game$Subject == subject & game$Partner == partner,]
  #if two users have interacted during games
  if (nrow(subgame1) > 0) {
    periods1 <- c (1:nrow(subgame1)+1)
    
    
    png (paste (directory,"/",game_name,"_actionOnTrust_",partner,"_on_", subject,".png", sep = ""))
    
    plot (subgame1$showing_trust_score, col = "red", type = "o", ylim = c(-1,1), xlim = c(1,20),xlab = "Period",
          main = paste("action of: ", partner, " on trust of ", subject, "in game: ", game_name),
          ylab = "Trust score and percentage of sending")
    
    #send_proportion is partner's send proportion
    subgame2 <- game [game$Subject == partner & game$Partner == subject,]
    periods2 <- c (1:nrow(subgame2))
    lines (periods2, subgame2$send_proportion, col = "blue", type = "o")
    
    
    legend ("topright",legend = c("Trust score", "Send amount"), col = c("red", "blue"), lty = 1)
    legend ("bottomright", legend = c(paste("Period: ", subgame1$Period), 
                                      paste("Cor = ",cor(subgame1$showing_trust_score, 
                                                         subgame2$send_proportion))),
            bg = "transparent")
    dev.off ()
  }
}

showAllActionOnTrustScore <- function (game, num_users = 6, directory = ".", game_name = "") {
  for (i in 1:num_users) {
    for (j in 1:num_users) {
      if (i != j) {
        showOneActionOnTrustScore (game, i, j, directory = directory, game_name = game_name)
      }
    }
  }
}



#run all functions on a particular XLS file
processFile <- function (XLS_file, num_users, average_rounds = 5, game_names, directory) {
  data <- getSubjects (XLS_filename = XLS_file)
  
  
  game1 = extractSubGame (subjects = data, first_round = 0, last_round = (num_users - 1) * average_rounds, num_users = num_users)
  game2 = extractSubGame (subjects = data, first_round = (num_users - 1) * average_rounds, last_round = (num_users - 1) * average_rounds * 2, 
                          num_users = num_users)
  game3 = extractSubGame (subjects = data, first_round = (num_users - 1) * average_rounds * 2, last_round = (num_users - 1) * average_rounds * 3,
                          num_users = num_users)
  game4 = extractSubGame (subjects = data, first_round = (num_users - 1) * average_rounds * 3, last_round = (num_users - 1) * average_rounds * 4,
                          num_users = num_users)
  
  #show trust score of one user related to his own action
  showAllPairs (game1, num_users = num_users, directory = directory, game_name = game_names[1])
  showAllPairs (game2, num_users = num_users, directory = directory, game_name = game_names[2])
  showAllPairs (game3, num_users = num_users, directory = directory, game_name = game_names[3])
  showAllPairs (game4, num_users = num_users, directory = directory, game_name = game_names[4])
  
  #show action of users based on his partner's trust score
  showAllActionOnTrustScore (game1, num_users = num_users, directory = directory, game_name = game_names[1])
  showAllActionOnTrustScore (game2, num_users = num_users, directory = directory, game_name = game_names[2])
  showAllActionOnTrustScore (game3, num_users = num_users, directory = directory, game_name = game_names[3])
  showAllActionOnTrustScore (game4, num_users = num_users, directory = directory, game_name = game_names[4])
  
}

#suggest to use:
#1. setwd (...)
#2. source ("ProcessData.R")
#3. processFile (XLS = "", num_users = ...)