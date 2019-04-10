# Utility functions

# calculate significant level manually
# df1: degree freedom 1
# df2: degree freedom 2

if (!require(zTree)) {
  install.packages("zTree")
}
library (zTree)

signif_level <- function (F_value, df1, df2) {
  res = ifelse(F_value >= qf(0.999, df1 = df1, df2 = df2), "***", #qf means quantile
               ifelse (F_value >= qf(0.99, df1 = df1, df2 = df2), "**",
                       ifelse(F_value >= qf(0.95, df1 = df1, df2 = df2), "*",
                              ifelse (F_value >= qf(0.90, df1 = df1, df2 = df2), ".", " "))))
}

pop.var <- function(x) var(x, na.rm = TRUE) * (length(x)-1) / length(x) #Unbiaised variance 

pop.sd <- function(x) {  #Standard deviation
  if (length(x) == 0) {
    0
  }
  else {
    if (is.na(sqrt(pop.var(x)))) { #deals with "NA" case
      0
    } else {
      sqrt(pop.var(x))
    }
  }
}

# permutational Kolmogorov-Smirnov p-value for paired data
perml_ks <- function(x,y){
  rb <- rbinom(length(x),1,0.5)
  xp <- ifelse(rb==1, x, y)
  yp <- ifelse(rb==1, y, x)
  ks.test(xp,yp)$p.value
}

loadZTree <- function (XLS_file) {
  # source("http://www.kirchkamp.de/lab/zTree.R")
  zTree::zTreeTables (XLS_file, zTree.silent = TRUE)
}

loadZTreeQuestionnaire <- function(SBJ_file) {
  # source("http://www.kirchkamp.de/lab/zTree.R")
  zTree::zTreeSbj (SBJ_file, zTree.silent = TRUE)
}

extractSubjectsData <- function (ZTT) {
  subjects <- ZTT[2]$subjects
  names (subjects) <- sub ('\\]', "", names (subjects))
  names (subjects) <- sub ('\\[', "", names (subjects))
  subjects
}

getSubjects <- function (XLS_filename) {
  s <- loadZTree (XLS_filename)[2]$subjects
  names (s) <- sub ('\\]', "", names (s)) #replace \\] with "" in the subjects names
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

# calculate average profit per game
calc_average_profit <- function (XLS_file, num_users = 6, average_rounds = 5, game_names) {
  data <- getSubjects (XLS_filename = XLS_file)
  
  game1 = extractSubGame (subjects = data, first_round = 0, last_round = (num_users - 1) * average_rounds, num_users = num_users)
  game2 = extractSubGame (subjects = data, first_round = (num_users - 1) * average_rounds, last_round = (num_users - 1) * average_rounds * 2, 
                          num_users = num_users)
  game3 = extractSubGame (subjects = data, first_round = (num_users - 1) * average_rounds * 2, last_round = (num_users - 1) * average_rounds * 3,
                          num_users = num_users)
  game4 = extractSubGame (subjects = data, first_round = (num_users - 1) * average_rounds * 3, last_round = (num_users - 1) * average_rounds * 4,
                          num_users = num_users)
  
  avg_profit_1 = mean (game1[(nrow(game1) - 5) : nrow(game1),]$Profit - 10)
  avg_profit_2 = mean (game2[(nrow(game2) - 5) : nrow(game2),]$Profit) - avg_profit_1 - 10
  avg_profit_3 = mean (game3[(nrow(game3) - 5) : nrow(game3),]$Profit) - avg_profit_2 - avg_profit_1 - 10
  avg_profit_4 = mean (game4[(nrow(game4) - 5) : nrow(game4),]$Profit) - avg_profit_3 - avg_profit_2 - avg_profit_1 - 10
  
  c (avg_profit_1, avg_profit_2, avg_profit_3, avg_profit_4)
}

# calculate correlation between trust scores and sending amount
calc_cor_trust_sending <- function (XLS_file, num_users = 6, average_rounds = 5, game_names) {
  data <- getSubjects (XLS_filename = XLS_file)
  
  game1 = extractSubGame (subjects = data, first_round = 0, last_round = (num_users - 1) * average_rounds, num_users = num_users)
  game2 = extractSubGame (subjects = data, first_round = (num_users - 1) * average_rounds, last_round = (num_users - 1) * average_rounds * 2, 
                          num_users = num_users)
  game3 = extractSubGame (subjects = data, first_round = (num_users - 1) * average_rounds * 2, last_round = (num_users - 1) * average_rounds * 3,
                          num_users = num_users)
  game4 = extractSubGame (subjects = data, first_round = (num_users - 1) * average_rounds * 3, last_round = (num_users - 1) * average_rounds * 4,
                          num_users = num_users)
  
  print (game_names[1])
  print (cor(game1$showing_trust_score, game1$send_proportion))
  print (game_names[2])
  print (cor(game2$showing_trust_score, game2$send_proportion))
  print (game_names[3])
  print (cor(game3$showing_trust_score, game3$send_proportion))
  print (game_names[4])
  print (cor(game4$showing_trust_score, game4$send_proportion))
}

# calculate average sending amount for each games
calc_average_sending <- function (XLS_file, num_users = 6, average_rounds = 5, game_names) {
  data <- getSubjects (XLS_filename = XLS_file)
  
  game1 = extractSubGame (subjects = data, first_round = 0, last_round = (num_users - 1) * average_rounds, num_users = num_users)
  game2 = extractSubGame (subjects = data, first_round = (num_users - 1) * average_rounds, last_round = (num_users - 1) * average_rounds * 2, 
                          num_users = num_users)
  game3 = extractSubGame (subjects = data, first_round = (num_users - 1) * average_rounds * 2, last_round = (num_users - 1) * average_rounds * 3,
                          num_users = num_users)
  game4 = extractSubGame (subjects = data, first_round = (num_users - 1) * average_rounds * 3, last_round = (num_users - 1) * average_rounds * 4,
                          num_users = num_users)
  
  #calculate means of sending amount of each game
  means_1 = mean (game1[game1$send_proportion >= 0,]$send_proportion)
  means_2 = mean (game2[game2$send_proportion >= 0,]$send_proportion)
  means_3 = mean (game3[game3$send_proportion >= 0,]$send_proportion)
  means_4 = mean (game4[game4$send_proportion >= 0,]$send_proportion)
  
  print (game_names[1])
  print (means_1)
  print (t.test (game1[game1$send_proportion >= 0,]$send_proportion, game2[game2$send_proportion >= 0,]$send_proportion))
  print (t.test (game1[game1$send_proportion >= 0,]$send_proportion, game3[game3$send_proportion >= 0,]$send_proportion))
  print (t.test (game1[game1$send_proportion >= 0,]$send_proportion, game4[game4$send_proportion >= 0,]$send_proportion))
  print (game_names[2])
  print (means_2)
  print (t.test (game2[game2$send_proportion >= 0,]$send_proportion, game3[game4$send_proportion >= 0,]$send_proportion))
  print (t.test (game2[game2$send_proportion >= 0,]$send_proportion, game3[game4$send_proportion >= 0,]$send_proportion))
  print (game_names[3])
  print (means_3)
  print (t.test (game3[game3$send_proportion >= 0,]$send_proportion, game4[game4$send_proportion >= 0,]$send_proportion))
  print (game_names[4])
  print (means_4)
}

calc_average_sending_for_person <- function (XLS_file, num_users = 6, average_rounds = 5, game_names, person_id) {
  data <- getSubjects (XLS_filename = XLS_file)
  
  game1 = extractSubGame (subjects = data, first_round = 0, last_round = (num_users - 1) * average_rounds, num_users = num_users)
  game2 = extractSubGame (subjects = data, first_round = (num_users - 1) * average_rounds, last_round = (num_users - 1) * average_rounds * 2, 
                          num_users = num_users)
  game3 = extractSubGame (subjects = data, first_round = (num_users - 1) * average_rounds * 2, last_round = (num_users - 1) * average_rounds * 3,
                          num_users = num_users)
  game4 = extractSubGame (subjects = data, first_round = (num_users - 1) * average_rounds * 3, last_round = (num_users - 1) * average_rounds * 4,
                          num_users = num_users)
  
  #calculate means of sending amount of each game
  means_1 = mean (game1[game1$send_proportion >= 0 & game1$Subject == person_id,]$send_proportion)
  means_2 = mean (game2[game2$send_proportion >= 0 & game2$Subject == person_id,]$send_proportion)
  means_3 = mean (game3[game3$send_proportion >= 0 & game3$Subject == person_id,]$send_proportion)
  means_4 = mean (game4[game4$send_proportion >= 0 & game4$Subject == person_id,]$send_proportion)
  
  print (game_names[1])
  print (means_1)
  t.test (game1[game1$send_proportion >= 0,]$send_proportion, game2[game2$send_proportion >= 0,]$send_proportion)
  t.test (game1[game1$send_proportion >= 0,]$send_proportion, game3[game3$send_proportion >= 0,]$send_proportion)
  t.test (game1[game1$send_proportion >= 0,]$send_proportion, game4[game4$send_proportion >= 0,]$send_proportion)
  print (game_names[2])
  print (means_2)
  t.test (game2[game2$send_proportion >= 0,]$send_proportion, game3[game4$send_proportion >= 0,]$send_proportion)
  t.test (game2[game2$send_proportion >= 0,]$send_proportion, game3[game4$send_proportion >= 0,]$send_proportion)
  print (game_names[3])
  print (means_3)
  t.test (game3[game3$send_proportion >= 0,]$send_proportion, game4[game4$send_proportion >= 0,]$send_proportion)
  print (game_names[4])
  print (means_4)
}

#run all functions on a particular XLS file
processFile <- function (XLS_file, num_users = 6, average_rounds = 5, game_names, directory) {
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

# update 27 - Aug - 2015
# get behavior and trust score of a player on another players's view

# parameters:
# subjects_data: the record of all games get from the function getSubjects (XLS_file). Usually a full game (Score Game, ID game, ...)
writeAllPlayerRecordToTextFile <- function (subjects_data, num_users = 6, directory = ".", game_name) {
  for (i in 1:num_users) {
    for (j in 1:num_users) {
      if (i != j) {
        # subgame contains all record between two players i and j
        # we want to get player j behavior and trust score on the view of player i
        subgame <- subjects_data [subjects_data$Subject == i & subjects_data$Partner == j,]
        if (nrow (subgame) > 0) {
          subdata <- subgame [,c("showing_trust_score", "send_proportion")]
          file_path = paste (directory,"/",game_name,"_",j,"_on_",i,".txt", sep="")
          write.table (subdata, file_path, sep = "\t")
        }
      }
    }
  }
}

readMultiXLS <- function (directory) {
  # source("http://www.kirchkamp.de/lab/zTree.R")
  sessions <- sessions<-list.files(directory,"[0-9]{6}_[0-9]{4}.xls",recursive=TRUE, full.names = TRUE)
  zTT <- zTree::zTreeTables(sessions, zTree.silent=TRUE)
  zTT
}

anova_analysis <- function (df, type = 0, num_way = 3) {
  require(car)
  Type_names = c("Sender", "Receiver")
#   print ("-------*******--------")
#   print (paste("ANOVA Analysis with Group 1 in long format for type: ", Type_names[type + 1]))
#   aovSPFp.qr <- aov (score ~ Groupbtw*Trust*SHOW_ID + Error (id/(Trust*SHOW_ID)), data = df)
#   print (summary (aovSPFp.qr))

#   print ("-------*******--------")
#   print (paste("ANOVA Analysis without Group 1 for type: ", Type_names [type + 1]))
#   aovSPFp.qr <- aov (Z_score ~ Groupbtw*Trust*SHOW_ID + Error (id/(Trust*SHOW_ID)), data = df[25:120,])
#   print (summary (aovSPFp.qr))
  if (num_way == 4) {
    # Analyze without GroupID
    df$AbsSend <- NULL
    df$RelSend <- NULL
    df$Profit <- NULL
    df$response_time <- NULL
    df$game_pos <- NULL
    df$game_setting <- NULL
    aov1 <- aov (score ~ SHOW_TRUST * SHOW_ID + Error (id/(SHOW_TRUST*SHOW_ID)), data = df)
    print (summary(aov1))
  }
  if (num_way == 3) {
    print ("-------*******--------")
    print (paste("ANOVA 3-ways Analysis in wide format for type (with corrected error terms): ", Type_names[type + 1]))
    dfW1 <- reshape(df, v.names = "score", timevar = "SHOW_TRUST", idvar = c("id","GroupID", "SHOW_ID"), direction = "wide")
    dfSPFp.qrW <- reshape(dfW1, v.names=c("score.0", "score.1"), timevar="SHOW_ID", idvar=c("id", "GroupID"), direction="wide")
    fitSPFp.qr   <- lm(cbind(score.0.0, score.1.0, score.0.1, score.1.1) ~ GroupID, data=dfSPFp.qrW)
    inSPFp.qr    <- expand.grid(SHOW_TRUST=gl(2, 1, labels = c("0","1")), SHOW_ID=gl(2, 1, labels = c("0","1")))
    AnovaSPFp.qr <- Anova(fitSPFp.qr, idata=inSPFp.qr, idesign=~SHOW_TRUST*SHOW_ID)
    
    # calculate F-score manually for random effects
    summary_anova <- summary(AnovaSPFp.qr, multivariate=FALSE, univariate=TRUE)
    u <- summary_anova$univariate.tests
    
    # recalculate for SHOW_TRUST
    u[,5]["SHOW_TRUST"] <- (u[,1]["SHOW_TRUST"] / u[,2]["SHOW_TRUST"]) / (u[,1]["GroupID:SHOW_TRUST"]/u[,2]["GroupID:SHOW_TRUST"])
    u[,6]["SHOW_TRUST"] <- 1 - pf (u[,5]["SHOW_TRUST"], df1 = u[,2]["SHOW_TRUST"], df2 = u[,2]["GroupID:SHOW_TRUST"])
    # u[,7]["SHOW_TRUST"] <- signif_level (u[,5]["SHOW_TRUST"], df1 = u[,2]["SHOW_TRUST"], df2 = u[,2]["GroupID:SHOW_TRUST"])
    
    # recalculate for SHOW_ID
    u[,5]["SHOW_ID"] <- (u[,1]["SHOW_ID"] / u[,2]["SHOW_ID"]) / (u[,1]["GroupID:SHOW_ID"]/u[,2]["GroupID:SHOW_ID"])
    u[,6]["SHOW_ID"] <- 1 - pf (u[,5]["SHOW_ID"], df1 = u[,2]["SHOW_ID"], df2 = u[,2]["GroupID:SHOW_ID"])
    # u[,7]["SHOW_ID"] <- signif_level (u[,5]["SHOW_ID"], df1 = u[,2]["SHOW_ID"], df2 = u[,2]["GroupID:SHOW_ID"])
    
    # recalcuate for SHOW_TRUST:SHOW_ID
    u[,5]["SHOW_TRUST:SHOW_ID"] <- (u[,1]["SHOW_TRUST:SHOW_ID"] / u[,2]["SHOW_TRUST:SHOW_ID"]) / (u[,1]["GroupID:SHOW_TRUST:SHOW_ID"]/u[,2]["GroupID:SHOW_TRUST:SHOW_ID"])
    u[,6]["SHOW_TRUST:SHOW_ID"] <- 1 - pf (u[,5]["SHOW_TRUST:SHOW_ID"], df1 = u[,2]["SHOW_TRUST:SHOW_ID"], df2 = u[,2]["GroupID:SHOW_TRUST:SHOW_ID"])
    # u[,7]["SHOW_TRUST:SHOW_ID"] <- signif_level (u[,5]["SHOW_TRUST:SHOW_ID"], df1 = u[,2]["SHOW_TRUST:SHOW_ID"], df2 = u[,2]["GroupID:SHOW_TRUST:SHOW_ID"])
    
    print (u)
    print ("---****---")
    # print (paste("ANOVA 3-ways Analysis in long format for type: ", Type_names[type + 1]))
#     aovSPFp.qr <- aov (score ~ GroupID * SHOW_TRUST*SHOW_ID + Error (id/(SHOW_TRUST*SHOW_ID)), data = df)
#     print (summary (aovSPFp.qr))
#     print ("With SHOW_TRUST and SHOW_ID are random effects")
#     aovSPFp.qr <- aov (score ~ Error (id * GroupID * SHOW_TRUST * SHOW_ID), data = df)
#     print (summary (aovSPFp.qr))
#     print ("Using lme package")
#     require (lme4)
#     mf <- lmer(score ~ SHOW_TRUST + SHOW_ID + (1|GroupID), data = df, REML = FALSE)
#     print(summary(mf))
#     mf <- lmer(score ~ SHOW_TRUST + SHOW_ID + (1|GroupID) + (1|id), data = df, REML = FALSE)
#     print(summary(mf))
  }
  if (num_way == 2) {
    # analyze with game order
    print ("-------*******--------")
    print (paste("ANOVA 2-ways Analysis in wide format for type (with corrected error terms): ", Type_names[type + 1]))
    if (max (as.numeric(df$game_pos)) == 4) {
      dfSPFpqW <- reshape(df, v.names = "score", timevar = "game_pos", idvar = c("id","GroupID"), direction = "wide")
      fitSPFpq   <- lm(cbind(score.1, score.2, score.3, score.4) ~ GroupID, data=dfSPFpqW)
      inSPFpq    <- data.frame(game_setting = gl(4,1))
      AnovaSPFpq <- Anova(fitSPFpq, idata=inSPFpq, idesign=~game_setting)
      u <-summary(AnovaSPFpq, multivariate=FALSE, univariate=TRUE)$univariate.tests
      u[,5]["game_setting"] <- (u[,1]["game_setting"]/u[,2]["game_setting"])/(u[,1]["GroupID:game_setting"]/u[,2]["GroupID:game_setting"])
      u[,6]["game_setting"] <- 1 - pf(u[,5]["game_setting"], df1 = u[,2]["game_setting"], df2 = u[,2]["GroupID:game_setting"])
      print (u)
    }
    else if (max (as.numeric(df$game_pos)) == 2) {
      dfSPFpqW <- reshape(df, v.names = "score", timevar = "game_pos", idvar = c("id","GroupID"), direction = "wide")
      fitSPFpq   <- lm(cbind(score.0, score.1) ~ GroupID, data=dfSPFpqW)
      inSPFpq    <- data.frame(game_setting = gl(2,1))
      AnovaSPFpq <- Anova(fitSPFpq, idata=inSPFpq, idesign=~game_setting)
      u <-summary(AnovaSPFpq, multivariate=FALSE, univariate=TRUE)$univariate.tests
      u[,5]["game_setting"] <- (u[,1]["game_setting"]/u[,2]["game_setting"])/(u[,1]["GroupID:game_setting"]/u[,2]["GroupID:game_setting"])
      u[,6]["game_setting"] <- 1 - pf(u[,5]["game_setting"], df1 = u[,2]["game_setting"], df2 = u[,2]["GroupID:game_setting"])
      print (u)
    }
    print ("----")
#     print (paste("ANOVA 2-ways Analysis in long format for type: ", Type_names[type + 1]))
#     aov1 <- aov(score ~ GroupID * game_pos + Error (id/game_pos), data = df)
#     print (aov1)
#     print ("Scheffe Test")
#     require(DescTools)
#     aov1 <- aov(score ~ GroupID * game_pos + Error (id/game_pos), data = df)
#     print (aov1)
#     print (ScheffeTest(aov1))
  }
  
#   print ("-------*******--------")
#   print (paste("ANOVA Analysis without Group 1 in wide format for type: ", Type_names[type + 1]))
#   dfW1 <- reshape(df[25:120,], v.names = "Z_score", timevar = "Trust", idvar = c("id","Groupbtw", "SHOW_ID"), direction = "wide")
#   dfSPFp.qrW <- reshape(dfW1, v.names=c("Z_score.0", "Z_score.1"), timevar="SHOW_ID", idvar=c("id", "Groupbtw"), direction="wide")
#   fitSPFp.qr   <- lm(cbind(Z_score.0.0, Z_score.1.0, Z_score.0.1, Z_score.1.1) ~ Groupbtw, data=dfSPFp.qrW)
#   inSPFp.qr    <- expand.grid(Trust=gl(2, 1, labels = c("0","1")), SHOW_ID=gl(2, 1, labels = c("0","1")))
#   AnovaSPFp.qr <- Anova(fitSPFp.qr, idata=inSPFp.qr, idesign=~Trust*SHOW_ID)
#   print (summary(AnovaSPFp.qr, multivariate=FALSE, univariate=TRUE))
}

calc_peak_end_trust <- function (trust_score_list) {
  result = rep (1:length(trust_score_list))
  for (i in 1: length(trust_score_list)) {
    max_score = max (trust_score_list[1:i])
    last_score = trust_score_list[i]
    if (i == 1) {
      last_score = 0.5
    }
    result[i] = (max_score + last_score) / 2
  }
  result
}

# process multiple files
processMultiXLS <- function (zTT, num_users = 6, average_rounds = 5, num_games = 4) {
  num_rounds_per_game = (num_users - 1) * average_rounds 
  num_rounds_per_exp = num_rounds_per_game * num_games
  num_exp = nrow (zTT[1]$globals) / num_rounds_per_exp
  
  # read through games
  # collect corresponding game and add to the list
  
  # first, create empty data frames to hold all the particular games
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
    
    simple_games <- rbind(simple_games, subjects_of_exp[((SIMPLE_GAME_ORDER - 1) * num_rounds_per_game * num_users + 1): (SIMPLE_GAME_ORDER * num_rounds_per_game * num_users),])
    id_games <- rbind(id_games, subjects_of_exp[((ID_GAME_ORDER - 1) * num_rounds_per_game * num_users + 1): (ID_GAME_ORDER * num_rounds_per_game * num_users),])
    score_games <- rbind(score_games, subjects_of_exp[((SCORE_GAME_ORDER - 1) * num_rounds_per_game * num_users + 1): (SCORE_GAME_ORDER * num_rounds_per_game * num_users),])
    combine_games <- rbind(combine_games, subjects_of_exp[((COMBINE_GAME_ORDER - 1) * num_rounds_per_game * num_users + 1): (COMBINE_GAME_ORDER * num_rounds_per_game * num_users),])
  }
  
  # calculate game metrics
  #calculate average sending amount per game
  # calculate average sending proportion per game
#   print ("Mean of sending amount per games")
#   print (mean (simple_games$Contribution))
#   print (mean (id_games$Contribution))
#   print (mean (score_games$Contribution))
#   print (mean (combine_games$Contribution))
  
  # calculate average sending proportion per game
#   print ("Mean of sending proportion per game")
#   print (mean (simple_games[simple_games$send_proportion >= 0,]$send_proportion))
#   print (mean (id_games[id_games$send_proportion >= 0,]$send_proportion))
#   print (mean (score_games[score_games$send_proportion >= 0,]$send_proportion))
#   print (mean (combine_games[combine_games$send_proportion >= 0,]$send_proportion))
#   
#   print ("Average of sending amount by senders per game")
#   print (mean (simple_games[simple_games$send_proportion >= 0 & simple_games$Type == 0,]$send_proportion))
#   print (mean (id_games[id_games$send_proportion >= 0 & id_games$Type == 0,]$send_proportion))
#   print (mean (score_games[score_games$send_proportion >= 0 & score_games$Type == 0,]$send_proportion))
#   print (mean (combine_games[combine_games$send_proportion >= 0 & combine_games$Type == 0,]$send_proportion))
#   
#   print ("Average of sending back by receiver per game")
#   print (mean (simple_games[simple_games$send_proportion >= 0 & simple_games$Type == 1,]$send_proportion))
#   print (mean (id_games[id_games$send_proportion >= 0 & id_games$Type == 1,]$send_proportion))
#   print (mean (score_games[score_games$send_proportion >= 0 & score_games$Type == 1,]$send_proportion))
#   print (mean (combine_games[combine_games$send_proportion >= 0 & combine_games$Type == 1,]$send_proportion))
  
#   print ("Average profit user get in 1 round per game")
#   print (mean (simple_games$CurrGameProfit))
#   print (mean (id_games$CurrGameProfit))
#   print (mean (score_games$CurrGameProfit))
#   print (mean (combine_games$CurrGameProfit))
  
#   print ("Response time for games")
#   print (mean(simple_games[simple_games$Type == 0,]$response_time))
#   print (mean(simple_games[simple_games$Type == 1,]$response_time))
#   
#   print (mean(id_games[id_games$Type == 0,]$response_time))
#   print (mean(id_games[id_games$Type == 1,]$response_time))
#   
#   print (mean(score_games[score_games$Type == 0,]$response_time))
#   print (mean(score_games[score_games$Type == 1,]$response_time))
#   
#   print (mean(combine_games[combine_games$Type == 0,]$response_time))
#   print (mean(combine_games[combine_games$Type == 1,]$response_time))
  
#   library(psych)
#   print ("Correlation between showing trust score and sending proportion for games")
#   print (cor.test(simple_games[simple_games$Type == 0,]$showing_trust_score, simple_games[simple_games$Type == 0,]$send_proportion))
#   print (cor.test(simple_games[simple_games$Type == 1,]$showing_trust_score, simple_games[simple_games$Type == 1,]$send_proportion))
#   
#   print (cor.test(id_games[id_games$Type == 0,]$showing_trust_score, id_games[id_games$Type == 0,]$send_proportion))
#   print (cor.test(id_games[id_games$Type == 1,]$showing_trust_score, id_games[id_games$Type == 1,]$send_proportion))
#   
#   print (cor.test(score_games[score_games$Type == 0,]$showing_trust_score, score_games[score_games$Type == 0,]$send_proportion))
#   print (cor.test(score_games[score_games$Type == 1,]$showing_trust_score, score_games[score_games$Type == 1,]$send_proportion))
#   
#   print (cor.test(combine_games[combine_games$Type == 0,]$showing_trust_score, combine_games[combine_games$Type == 0,]$send_proportion))
#   print (cor.test(combine_games[combine_games$Type == 1,]$showing_trust_score, combine_games[combine_games$Type == 1,]$send_proportion))
#   
#   print ("Significance test between correlation")
#   
#   library(cocor)
#   print("Singificane test of correlation between Simple Games and Score Games (No ID)")
#   print ("For SENDERs")
#   print (cocor(~showing_trust_score + send_proportion | showing_trust_score + send_proportion,list(simple_games[simple_games$Type == 0,],score_games[score_games$Type == 0,])))
#   print ("-----")
#   print ("For RECEIVERs")
#   print (cocor(~showing_trust_score + send_proportion | showing_trust_score + send_proportion,list(simple_games[simple_games$Type == 1,],score_games[score_games$Type == 1,])))
#   
#   print("Singificane test of correlation between ID Games and Combine Games (Have ID)")
#   print ("For SENDERs")
#   print (cocor(~showing_trust_score + send_proportion | showing_trust_score + send_proportion,list(id_games[id_games$Type == 0,],combine_games[combine_games$Type == 0,])))
#   print ("-----")
#   print ("For RECEIVERs")
#   print (cocor(~showing_trust_score + send_proportion | showing_trust_score + send_proportion,list(id_games[id_games$Type == 0,],combine_games[combine_games$Type == 0,])))

  # response time
#   print ("Response time: mean and standard variation")
#   print ("Simple Games - SENDERs")
#   print (mean(simple_games[simple_games$Type == 0,]$response_time))
#   print (pop.sd(simple_games[simple_games$Type == 0,]$response_time))
#   
#   print ("Simple Games - RECEIVERs")
#   print (mean(simple_games[simple_games$Type == 1,]$response_time))
#   print (pop.sd(simple_games[simple_games$Type == 1,]$response_time))
#   
#   print ("ID Games - SENDERs")
#   print (mean(id_games[id_games$Type == 0,]$response_time))
#   print (pop.sd(id_games[id_games$Type == 0,]$response_time))
#   
#   print ("ID Games - RECEIVERs")
#   print (mean(id_games[id_games$Type == 1,]$response_time))
#   print (pop.sd(id_games[id_games$Type == 1,]$response_time))
#   
#   print ("Score Games - SENDERs")
#   print (mean(score_games[score_games$Type == 0,]$response_time))
#   print (pop.sd(score_games[score_games$Type == 0,]$response_time))
#   
#   print ("Score Games - RECEIVERs")
#   print (mean(score_games[score_games$Type == 1,]$response_time))
#   print (pop.sd(score_games[score_games$Type == 1,]$response_time))
#   
#   print ("Combine Games - SENDERs")
#   print (mean(combine_games[combine_games$Type == 0,]$response_time))
#   print (pop.sd(combine_games[combine_games$Type == 0,]$response_time))
#   
#   print ("Combine Games - RECEIVERs")
#   print (mean(combine_games[combine_games$Type == 1,]$response_time))
#   print (pop.sd(combine_games[combine_games$Type == 1,]$response_time))
  
  # write all the data for each games in CSV file
  # using write.csv
  Type_names = c("Sender", "Receiver")
  SIMPLE_GAME_ORDERS =      c(3,2,4,1,2)
  ID_GAME_ORDERS =          c(1,4,1,2,3)
  SCORE_GAME_ORDERS =       c(2,1,3,4,4)
  COMBINE_GAME_ORDERS =     c(4,3,2,3,1)
  
  require(car)
  for (type in 0:1) {
    # AbsSend: absolute sending
    # RelSend: send proportion of maximum
    # Profit: profit get at this round
    # game_pos:  order of game to play in this experiment
    df <- data.frame("id" = as.numeric(), 
                     "GroupID"= as.numeric(),
                     "SHOW_TRUST"=as.numeric(),
                     "SHOW_ID"=as.numeric(),
                     "AbsSend" = as.numeric(),  
                     "RelSend" = as.numeric(),  
                     "Profit" = as.numeric(),
                     "game_pos" = as.numeric(),
                     "response_time" = as.numeric())
    for (j in 1:num_users) {
      for (exp_id in 1:num_exp) {
        for (k in 1:num_games) {
          first_round_of_exp_subjects = (exp_id - 1) * num_rounds_per_game * num_users + 1
          last_round_of_exp_subjects = exp_id * num_rounds_per_game * num_users
          simple_game = simple_games[first_round_of_exp_subjects:last_round_of_exp_subjects,]
          id_game = id_games [first_round_of_exp_subjects:last_round_of_exp_subjects,]
          score_game = score_games [first_round_of_exp_subjects:last_round_of_exp_subjects,]
          combine_game = combine_games[first_round_of_exp_subjects:last_round_of_exp_subjects,]
          
          user_id = j + num_users * (exp_id - 1)
          
          if (k == 1) {
            new_row = c(as.numeric (user_id),
                        as.numeric (exp_id),
                        0,
                        0,
                        sum (simple_game[simple_game$Subject == j & simple_game$Type == type,]$Contribution),
                        mean (simple_game[simple_game$Subject == j & simple_game$Type == type & simple_game$send_proportion >= 0,]$send_proportion),
                        sum (simple_game[simple_game$Subject == j & simple_game$Type == type,]$CurrGameProfit),
                        SIMPLE_GAME_ORDERS[exp_id],
                        mean (simple_game[simple_game$Subject == j & simple_game$Type == type,]$response_time)
            )
            df[nrow(df)+1,] <- new_row
          }
          if (k == 2) {
            new_row = c(user_id,
                        exp_id,
                        0,
                        1,
                        sum (id_game[id_game$Subject == j & id_game$Type == type,]$Contribution),
                        mean (id_game[id_game$Subject == j & id_game$Type == type & id_game$send_proportion >= 0,]$send_proportion),
                        sum (id_game[id_game$Subject == j & id_game$Type == type,]$CurrGameProfit),
                        ID_GAME_ORDERS[exp_id],
                        mean (id_game[id_game$Subject == j & id_game$Type == type,]$response_time)
            )
            df[nrow(df)+1,] <- new_row
          }
          if (k == 3) {
            new_row = c(user_id,
                        exp_id,
                        1,
                        0,
                        sum (score_game[score_game$Subject == j & score_game$Type == type,]$Contribution),
                        mean (score_game[score_game$Subject == j & score_game$Type == type & score_game$send_proportion >= 0,]$send_proportion),
                        sum (score_game[score_game$Subject == j & score_game$Type == type,]$CurrGameProfit),
                        SCORE_GAME_ORDERS[exp_id],
                        mean (score_game[score_game$Subject == j & score_game$Type == type,]$response_time)
            )
            df[nrow(df)+1,] <- new_row
          }
          if (k == 4) {
            new_row = c(user_id,
                        exp_id,
                        1,
                        1,
                        sum (combine_game[combine_game$Subject == j & combine_game$Type == type,]$Contribution),
                        mean (combine_game[combine_game$Subject == j & combine_game$Type == type & combine_game$send_proportion >= 0,]$send_proportion),
                        sum (combine_game[combine_game$Subject == j & combine_game$Type == type,]$CurrGameProfit),
                        COMBINE_GAME_ORDERS[exp_id],
                        mean (combine_game[combine_game$Subject == j & combine_game$Type == type,]$response_time)
            )
            df[nrow(df)+1,] <- new_row
          }
        }
      }
    }
    
    # convert the contribution to Z-score
    #     df["Z_score"] <- NA
    #     for (i in 1:num_exp) {
    #       first_round = 1 + (i - 1) * num_users * num_games
    #       last_round = i * num_users * num_games
    #       mean_period = mean (df[first_round:last_round,]$AbsSend)
    #       std_period = sd (df[first_round:last_round,]$AbsSend) * sqrt ((num_users * num_games - 1)/(num_users * num_games))
    #       for (j in first_round:last_round) {
    #         df[j,]$Z_score = (df[j,]$AbsSend - mean_period) / std_period
    #       }
    #     }
    
    df$id <- as.factor(df$id)
    df$GroupID <- as.factor(df$GroupID)
    df$SHOW_TRUST <- as.factor (df$SHOW_TRUST)
    df$SHOW_ID <- as.factor(df$SHOW_ID)
    df$game_pos <- as.factor(df$game_pos)
    #     df$Z_score <- NULL
    #     df$AbsSend <-NULL
    #     df$RelSend <- NULL
    #     df$Profit <- NULL
    #     df$response_time <- NULL
    
    #     df$Trust <- NULL
    #     df$SHOW_ID <- NULL
    # df$game_pos <- NULL
    
    
#     print ("ANOVA analysis with absolute sending on GroupID:SHOW_TRUST:SHOW_ID")
#     df$score <- df$AbsSend
#     anova_analysis (df, type, num_way = 3)
#     print ("ANOVA analysis with scale absolute sending on GroupID:SHOW_TRUST:SHOW_ID (Z_score)")
#     df$score <- scale(df$AbsSend)
#     anova_analysis (df, type, num_way = 3)
#     print ("ANOVA analysis with relative sending on GroupID:SHOW_TRUST:SHOW_ID")
#     df$score <- df$RelSend
#     anova_analysis (df, type, num_way = 3)
#     print ("ANOVA analysis with scale relative sending on GroupID:SHOW_TRUST:SHOW_ID (Z_score)")
#     df$score <- scale(df$RelSend)
#     anova_analysis (df, type, num_way = 3)
#     print ("ANOVA analysis with absolute profit on GroupID:SHOW_TRUST:SHOW_ID")
#     df$score <- df$Profit
#     anova_analysis (df, type, num_way = 3)
#     print ("ANOVA analysis with scale absolute profit on GroupID:SHOW_TRUST:SHOW_ID (Z_score)")
#     df$score <- scale(df$Profit)
#     anova_analysis (df, type, num_way = 3)
#     
#     print ("ANOVA analysis with absolute sending on GroupID:game_pos")
#     df$score <- df$AbsSend
#     anova_analysis (df, type, num_way = 2)
#     print ("ANOVA analysis with scale absolute sending on GroupID:game_pos (Z_score)")
#     df$score <- scale(df$AbsSend)
#     anova_analysis (df, type, num_way = 2)
#     print ("ANOVA analysis with relative sending on GroupID:game_pos")
#     df$score <- df$RelSend
#     anova_analysis (df, type, num_way = 2)
#     print ("ANOVA analysis with scale relative sending on GroupID:game_pos (Z_score)")
#     df$score <- scale(df$RelSend)
#     anova_analysis (df, type, num_way = 2)
#     print ("ANOVA analysis with absolute profit on GroupID:game_pos")
#     df$score <- df$Profit
#     anova_analysis (df, type, num_way = 2)
#     print ("ANOVA analysis with scale absolute profit on GroupID:game_pos (Z_score)")
#     df$score <- scale(df$Profit)
#     anova_analysis (df, type, num_way = 2)
  }
  
  # calculate regression of showing_trust_score and send_proportion
  
  for (type in 0:1) {
    df_simple <- data.frame ("id" = as.numeric(),
                             "trust_value" = as.numeric(),
                             "RelSend" = as.numeric(),
                             "AbsSend" = as.numeric(),
                             "response_time" = as.numeric(),
                             "game_position" = as.numeric(),
                             "peak_end_trust" = as.numeric(),
                             "AbsPartnerSend" = as.numeric())
    df_id <- data.frame ("id" = as.numeric(),
                         "trust_value" = as.numeric(),
                         "RelSend" = as.numeric(),
                         "AbsSend" = as.numeric(),
                         "response_time" = as.numeric(),
                         "game_position" = as.numeric(),
                         "peak_end_trust" = as.numeric(),
                         "AbsPartnerSend" = as.numeric())
    df_score <- data.frame ("id" = as.numeric(),
                            "trust_value" = as.numeric(),
                            "RelSend" = as.numeric(),
                            "AbsSend" = as.numeric(),
                            "response_time" = as.numeric(),
                            "game_position" = as.numeric(),
                            "peak_end_trust" = as.numeric(),
                            "AbsPartnerSend" = as.numeric())
    df_combine <- data.frame ("id" = as.numeric(),
                              "trust_value" = as.numeric(),
                              "RelSend" = as.numeric(),
                              "AbsSend" = as.numeric(),
                              "response_time" = as.numeric(),
                              "game_position" = as.numeric(),
                              "peak_end_trust" = as.numeric(),
                              "AbsPartnerSend" = as.numeric())
    for (exp_id in 1:num_exp) {
      first_round_of_exp_subjects = (exp_id - 1) * num_rounds_per_game * num_users + 1
      last_round_of_exp_subjects = exp_id * num_rounds_per_game * num_users
      simple_game = simple_games[first_round_of_exp_subjects:last_round_of_exp_subjects,]
      id_game = id_games [first_round_of_exp_subjects:last_round_of_exp_subjects,]
      score_game = score_games [first_round_of_exp_subjects:last_round_of_exp_subjects,]
      combine_game = combine_games[first_round_of_exp_subjects:last_round_of_exp_subjects,]
      for (user_id in 1:num_users) {
        new_row = c((exp_id - 1) * num_users + user_id,
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$showing_trust_score),
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$send_proportion),
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$Contribution),
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$response_time),
                    SIMPLE_GAME_ORDERS[exp_id],
                    mean (calc_peak_end_trust(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$showing_trust_score)),
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$PartnerDecision)
                    )
        df_simple[nrow(df_simple) + 1,] <- new_row
        new_row = c((exp_id - 1) * num_users + user_id,
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$showing_trust_score),
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$send_proportion),
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$Contribution),
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$response_time),
                    ID_GAME_ORDERS[exp_id],
                    mean(calc_peak_end_trust(id_game[id_game$Type == type & id_game$Subject == user_id,]$showing_trust_score)),
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$PartnerDecision)     
        )
        df_id[nrow(df_id) + 1,] <- new_row
        new_row = c((exp_id - 1) * num_users + user_id,
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$showing_trust_score),
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$send_proportion),
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$Contribution),
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$response_time),
                    SCORE_GAME_ORDERS[exp_id],
                    mean (calc_peak_end_trust(score_game[score_game$Type == type & score_game$Subject == user_id,]$showing_trust_score)),
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$PartnerDecision)      
        )
        df_score[nrow(df_score) + 1,] <- new_row
        new_row = c((exp_id - 1) * num_users + user_id,
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$showing_trust_score),
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$send_proportion),
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$Contribution),
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$response_time),
                    COMBINE_GAME_ORDERS[exp_id],
                    mean (calc_peak_end_trust(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$showing_trust_score)),
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$PartnerDecision)
        )
        df_combine[nrow(df_combine) + 1,] <- new_row
      }
    }
    
    # Regression of response time on game position
    #     jpeg(filename = "./all_data/Response_time_simple_game.jpeg", width = 1000,quality = 100)
#     print ("*****")
#     print (paste ("PLAYER ROLE: ", Type_names[type + 1]))
#     print ("*****")
#     print ("Response time of Simple Game (mean and std)")
#     print (mean(df_simple$response_time))
#     print (pop.sd(df_simple$response_time))
#     print ("Response time of Simple Game with the the order 1st has been removed")
#     print (mean(df_simple[df_simple$game_pos > 1,]$response_time))
#     print (pop.sd(df_simple[df_simple$game_pos > 1,]$response_time))
#     print ("Response time of Simple Game when it was played at the 1st game")
#     print (mean(df_simple[df_simple$game_pos == 1,]$response_time))
#     print (pop.sd(df_simple[df_simple$game_pos == 1,]$response_time))
#     
#     print ("Residual plot of response time on game position")
#     mod1 <- lm(response_time ~ game_pos, data = df_simple)
#     plot(df_simple$game_pos, df_simple$response_time, xlim=c(0,5), ylim=c(0, 40), xlab = "Game Postion", ylab = "Response time in seconds",
#          main = "Simple Game")
#     abline(mod1, lwd=2)
#     # calculate residuals and predicted values
#     res <- signif(residuals(mod1), 5)
#     pre <- predict(mod1) # plot distances between points and the regression line
#     segments(df_simple$game_pos, df_simple$response_time, df_simple$game_pos, pre, col="red")
#     # add labels (res values) to points
#     library(calibrate)
#     textxy(df_simple$game_pos, df_simple$response_time, res, cx=0.7)
#     print (summary (mod1))
#     
#     print ("Residual plot of response time on game position without 1st order game")
#     mod1 <- lm(response_time ~ game_pos, data = df_simple[df_simple$game_pos > 1,])
#     plot(df_simple[df_simple$game_pos > 1,]$game_pos, df_simple[df_simple$game_pos > 1,]$response_time, xlim=c(0,5), ylim=c(0, 40), xlab = "Game Postion", ylab = "Response time in seconds",
#          main = "Simple Game without 1st order")
#     abline(mod1, lwd=2)
#     # calculate residuals and predicted values
#     res <- signif(residuals(mod1), 5)
#     pre <- predict(mod1) # plot distances between points and the regression line
#     segments(df_simple[df_simple$game_pos > 1,]$game_pos, df_simple[df_simple$game_pos > 1,]$response_time, df_simple[df_simple$game_pos > 1,]$game_pos, pre, col="red")
#     # add labels (res values) to points
#     library(calibrate)
#     textxy(df_simple[df_simple$game_pos > 1,]$game_pos, df_simple[df_simple$game_pos > 1,]$response_time, res, cx=0.7)
#     print (summary (mod1))
#     #     dev.off()
#     
#     print ("*****")
#     print (paste ("PLAYER ROLE: ", Type_names[type + 1]))
#     print ("*****")
#     print ("Response time of Simple Game (mean and std)")
#     print (mean(df_id$response_time))
#     print (pop.sd(df_id$response_time))
#     print ("Response time of Simple Game with the the order 1st has been removed")
#     print (mean(df_id[df_id$game_pos > 1,]$response_time))
#     print (pop.sd(df_id[df_id$game_pos > 1,]$response_time))
#     print ("Response time of Simple Game when it was played at the 1st game")
#     print (mean(df_id[df_id$game_pos == 1,]$response_time))
#     print (pop.sd(df_id[df_id$game_pos == 1,]$response_time))
#     
#     print ("Residual plot of response time on game position")
#     mod1 <- lm(response_time ~ game_pos, data = df_id)
#     plot(df_id$game_pos, df_id$response_time, xlim=c(0,5), ylim=c(0, 40), xlab = "Game Postion", ylab = "Response time in seconds",
#          main = "ID Game")
#     abline(mod1, lwd=2)
#     # calculate residuals and predicted values
#     res <- signif(residuals(mod1), 5)
#     pre <- predict(mod1) # plot distances between points and the regression line
#     segments(df_id$game_pos, df_id$response_time, df_id$game_pos, pre, col="red")
#     # add labels (res values) to points
#     library(calibrate)
#     textxy(df_id$game_pos, df_id$response_time, res, cx=0.7)
#     print (summary (mod1))
#     
#     print ("Residual plot of response time on game position without 1st order game")
#     mod1 <- lm(response_time ~ game_pos, data = df_id[df_id$game_pos > 1,])
#     plot(df_id[df_id$game_pos > 1,]$game_pos, df_id[df_id$game_pos > 1,]$response_time, xlim=c(0,5), ylim=c(0, 40), xlab = "Game Postion", ylab = "Response time in seconds",
#          main = "ID Game without 1st order")
#     abline(mod1, lwd=2)
#     # calculate residuals and predicted values
#     res <- signif(residuals(mod1), 5)
#     pre <- predict(mod1) # plot distances between points and the regression line
#     segments(df_id[df_id$game_pos > 1,]$game_pos, df_id[df_id$game_pos > 1,]$response_time, df_id[df_id$game_pos > 1,]$game_pos, pre, col="red")
#     # add labels (res values) to points
#     library(calibrate)
#     textxy(df_id[df_id$game_pos > 1,]$game_pos, df_id[df_id$game_pos > 1,]$response_time, res, cx=0.7)
#     print (summary (mod1))
#     
#     print ("*****")
#     print (paste ("PLAYER ROLE: ", Type_names[type + 1]))
#     print ("*****")
#     print ("Response time of Simple Game (mean and std)")
#     print (mean(df_score$response_time))
#     print (pop.sd(df_score$response_time))
#     print ("Response time of Simple Game with the the order 1st has been removed")
#     print (mean(df_score[df_score$game_pos > 1,]$response_time))
#     print (pop.sd(df_score[df_score$game_pos > 1,]$response_time))
#     print ("Response time of Simple Game when it was played at the 1st game")
#     print (mean(df_score[df_score$game_pos == 1,]$response_time))
#     print (pop.sd(df_score[df_score$game_pos == 1,]$response_time))
#     
#     print ("Residual plot of response time on game position")
#     mod1 <- lm(response_time ~ game_pos, data = df_score)
#     plot(df_score$game_pos, df_score$response_time, xlim=c(0,5), ylim=c(0, 40), xlab = "Game Postion", ylab = "Response time in seconds",
#          main = "Score Game")
#     abline(mod1, lwd=2)
#     # calculate residuals and predicted values
#     res <- signif(residuals(mod1), 5)
#     pre <- predict(mod1) # plot distances between points and the regression line
#     segments(df_score$game_pos, df_score$response_time, df_score$game_pos, pre, col="red")
#     # add labels (res values) to points
#     library(calibrate)
#     textxy(df_score$game_pos, df_score$response_time, res, cx=0.7)
#     print (summary (mod1))
#     
#     print ("Residual plot of response time on game position without 1st order game")
#     mod1 <- lm(response_time ~ game_pos, data = df_score[df_score$game_pos > 1,])
#     plot(df_score[df_score$game_pos > 1,]$game_pos, df_score[df_score$game_pos > 1,]$response_time, xlim=c(0,5), ylim=c(0, 40), xlab = "Game Postion", ylab = "Response time in seconds",
#          main = "Score Game without 1st order")
#     abline(mod1, lwd=2)
#     # calculate residuals and predicted values
#     res <- signif(residuals(mod1), 5)
#     pre <- predict(mod1) # plot distances between points and the regression line
#     segments(df_score[df_score$game_pos > 1,]$game_pos, df_score[df_score$game_pos > 1,]$response_time, df_score[df_score$game_pos > 1,]$game_pos, pre, col="red")
#     # add labels (res values) to points
#     library(calibrate)
#     textxy(df_score[df_score$game_pos > 1,]$game_pos, df_score[df_score$game_pos > 1,]$response_time, res, cx=0.7)
#     print (summary (mod1))
#     
#     print ("*****")
#     print (paste ("PLAYER ROLE: ", Type_names[type + 1]))
#     print ("*****")
#     print ("Response time of Simple Game (mean and std)")
#     print (mean(df_combine$response_time))
#     print (pop.sd(df_combine$response_time))
#     print ("Response time of Simple Game with the the order 1st has been removed")
#     print (mean(df_combine[df_combine$game_pos > 1,]$response_time))
#     print (pop.sd(df_combine[df_combine$game_pos > 1,]$response_time))
#     print ("Response time of Simple Game when it was played at the 1st game")
#     print (mean(df_combine[df_combine$game_pos == 1,]$response_time))
#     print (pop.sd(df_combine[df_combine$game_pos == 1,]$response_time))
#     
#     print ("Residual plot of response time on game position")
#     mod1 <- lm(response_time ~ game_pos, data = df_combine)
#     plot(df_combine$game_pos, df_combine$response_time, xlim=c(0,5), ylim=c(0, 40), xlab = "Game Postion", ylab = "Response time in seconds",
#          main = "Combine Game")
#     abline(mod1, lwd=2)
#     # calculate residuals and predicted values
#     res <- signif(residuals(mod1), 5)
#     pre <- predict(mod1) # plot distances between points and the regression line
#     segments(df_combine$game_pos, df_combine$response_time, df_combine$game_pos, pre, col="red")
#     # add labels (res values) to points
#     library(calibrate)
#     textxy(df_combine$game_pos, df_combine$response_time, res, cx=0.7)
#     print (summary (mod1))
#     
#     print ("Residual plot of response time on game position without 1st order game")
#     mod1 <- lm(response_time ~ game_pos, data = df_combine[df_combine$game_pos > 1,])
#     plot(df_combine[df_combine$game_pos > 1,]$game_pos, df_combine[df_combine$game_pos > 1,]$response_time, xlim=c(0,5), ylim=c(0, 40), xlab = "Game Postion", ylab = "Response time in seconds",
#          main = "Combine Game without 1st order")
#     abline(mod1, lwd=2)
#     # calculate residuals and predicted values
#     res <- signif(residuals(mod1), 5)
#     pre <- predict(mod1) # plot distances between points and the regression line
#     segments(df_combine[df_combine$game_pos > 1,]$game_pos, df_combine[df_combine$game_pos > 1,]$response_time, df_combine[df_combine$game_pos > 1,]$game_pos, pre, col="red")
#     # add labels (res values) to points
#     library(calibrate)
#     textxy(df_combine[df_combine$game_pos > 1,]$game_pos, df_combine[df_combine$game_pos > 1,]$response_time, res, cx=0.7)
#     print (summary (mod1))
#     
#     # Linear regression
#     print (paste (("Linear regression of sending on trust value for type: "), Type_names [ type + 1]))
#     print (summary (lm(RelSend ~ trust_value, data = df_simple)))
#     print (summary (lm(AbsSend ~ trust_value, data = df_simple)))
#     
#     print (summary (lm(RelSend ~ trust_value, data = df_id)))
#     print (summary (lm(AbsSend ~ trust_value, data = df_id)))
#     
#     print (summary (lm(RelSend ~ trust_value, data = df_score)))
#     print (summary (lm(AbsSend ~ trust_value, data = df_score)))
#     
#     print (summary (lm(RelSend ~ trust_value, data = df_combine)))
#     print (summary (lm(AbsSend ~ trust_value, data = df_combine)))
#     
#     # Correlation
#     print (paste (("Correlation for type: "), Type_names [ type + 1]))
#     print (cor.test(df_simple$AbsSend, df_simple$trust_value))
#     print (cor.test(df_id$AbsSend, df_id$trust_value))
#     print (cor.test(df_score$AbsSend, df_score$trust_value))
#     print (cor.test(df_combine$AbsSend, df_combine$trust_value))
#     
#     print (cor.test(df_simple$RelSend, df_simple$trust_value))
#     print (cor.test(df_id$RelSend, df_id$trust_value))
#     print (cor.test(df_score$RelSend, df_score$trust_value))
#     print (cor.test(df_combine$RelSend, df_combine$trust_value))
#     
#     require (cocor)
#     print (cocor(~AbsSend + trust_value|AbsSend + trust_value, list(df_simple, df_id)))
#     print (cocor(~RelSend + trust_value|RelSend + trust_value, list(df_simple, df_id)))
#     
#     print (cocor(~AbsSend + trust_value|AbsSend + trust_value, list(df_score, df_combine)))
#     print (cocor(~RelSend + trust_value|RelSend + trust_value, list(df_score, df_combine)))
#     
#     print ("")
#     print ("*****")
    
    # Linear regression with peak_end_trust, instead of trust_value
#     print (paste (("Linear regression of sending on trust value for type: "), Type_names [ type + 1]))
#     print (summary (lm(RelSend ~ peak_end_trust, data = df_simple)))
#     print (summary (lm(AbsSend ~ peak_end_trust, data = df_simple)))
#     
#     print (summary (lm(RelSend ~ peak_end_trust, data = df_id)))
#     print (summary (lm(AbsSend ~ peak_end_trust, data = df_id)))
#     
#     print (summary (lm(RelSend ~ peak_end_trust, data = df_score)))
#     print (summary (lm(AbsSend ~ peak_end_trust, data = df_score)))
#     
#     print (summary (lm(RelSend ~ peak_end_trust, data = df_combine)))
#     print (summary (lm(AbsSend ~ peak_end_trust, data = df_combine)))
    
    # Analysis the prediction power of trust score
#     print (paste (("Linear regression of trust value and partner sending: "), Type_names [ type + 1]))
#     print ("ABSOLUTE SENDING OF PARTNER")
#     print (summary (lm(AbsPartnerSend ~ trust_value, data = df_simple)))
#     
#     print (summary (lm(AbsPartnerSend ~ trust_value, data = df_id)))
#     
#     print (summary (lm(AbsPartnerSend ~ trust_value, data = df_score)))
#     
#     print (summary (lm(AbsPartnerSend ~ trust_value, data = df_combine)))
#     
#     print ("RELATIVE SENDING OF PARTNER")
#     if (type == 1) {
#       print (summary (lm((AbsPartnerSend/10) ~ trust_value, data = df_simple)))
#       
#       print (summary (lm((AbsPartnerSend/10) ~ trust_value, data = df_id)))
#       
#       print (summary (lm((AbsPartnerSend/10) ~ trust_value, data = df_score)))
#       
#       print (summary (lm((AbsPartnerSend/10) ~ trust_value, data = df_combine)))
#     }
#     if (type == 0) {
#       print (summary (lm((AbsPartnerSend/3/AbsSend) ~ trust_value, data = df_simple)))
#       
#       print (summary (lm((AbsPartnerSend/3/AbsSend) ~ trust_value, data = df_id)))
#       
#       print (summary (lm((AbsPartnerSend/3/AbsSend) ~ trust_value, data = df_score)))
#       
#       print (summary (lm((AbsPartnerSend/3/AbsSend) ~ trust_value, data = df_combine)))
#     }
  }
  
  print ("Group interaction on regression power of sending behavior on two trust scores")
  for (type in 0:1) {
    df_simple <- data.frame ("id" = as.numeric(),
                             "trust_value" = as.numeric(),
                             "RelSend" = as.numeric(),
                             "AbsSend" = as.numeric(),
                             "response_time" = as.numeric(),
                             "game_pos" = as.numeric(),
                             "peak_end_trust" = as.numeric(),
                             "AbsPartnerSend" = as.numeric(),
                             "my_trust_value" = as.numeric(),
                             "r_value" = as.numeric())
    df_id <- data.frame ("id" = as.numeric(),
                         "trust_value" = as.numeric(),
                         "RelSend" = as.numeric(),
                         "AbsSend" = as.numeric(),
                         "response_time" = as.numeric(),
                         "game_pos" = as.numeric(),
                         "peak_end_trust" = as.numeric(),
                         "AbsPartnerSend" = as.numeric(),
                         "my_trust_value" = as.numeric(),
                         "r_value" = as.numeric())
    df_score <- data.frame ("id" = as.numeric(),
                            "trust_value" = as.numeric(),
                            "RelSend" = as.numeric(),
                            "AbsSend" = as.numeric(),
                            "response_time" = as.numeric(),
                            "game_pos" = as.numeric(),
                            "peak_end_trust" = as.numeric(),
                            "AbsPartnerSend" = as.numeric(),
                            "my_trust_value" = as.numeric(),
                            "r_value" = as.numeric())
    df_combine <- data.frame ("id" = as.numeric(),
                              "trust_value" = as.numeric(),
                              "RelSend" = as.numeric(),
                              "AbsSend" = as.numeric(),
                              "response_time" = as.numeric(),
                              "game_pos" = as.numeric(),
                              "peak_end_trust" = as.numeric(),
                              "AbsPartnerSend" = as.numeric(),
                              "my_trust_value" = as.numeric(),
                              "r_value" = as.numeric())
    df_total <- data.frame("id" = as.numeric(),
                           "GroupID" = as.numeric(),
                           "SHOW_TRUST" = as.numeric(),
                           "SHOW_ID" = as.numeric(),
                           "score" = as.numeric())
    for (exp_id in 1:num_exp) {
      first_round_of_exp_subjects = (exp_id - 1) * num_rounds_per_game * num_users + 1
      last_round_of_exp_subjects = exp_id * num_rounds_per_game * num_users
      simple_game = simple_games[first_round_of_exp_subjects:last_round_of_exp_subjects,]
      id_game = id_games [first_round_of_exp_subjects:last_round_of_exp_subjects,]
      score_game = score_games [first_round_of_exp_subjects:last_round_of_exp_subjects,]
      combine_game = combine_games[first_round_of_exp_subjects:last_round_of_exp_subjects,]
      for (user_id in 1:num_users) {
        r1 = 0
        r2 = 0
        r3 = 0
        r4 = 0
        for (partner_id in 1:num_users) {
          if (user_id == partner_id) {next()}
          cur_game <- simple_game
          if (length(cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$Contribution) > 0) {
            temp_r <- summary (lm (cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$Contribution ~ 
                                     cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$showing_trust_score
                                + cur_game[cur_game$Type == (1 - type) & cur_game$Partner == user_id & cur_game$Subject == partner_id,]$showing_trust_score))$r.squared
            temp_r <- sqrt (temp_r)
            if (!is.na(temp_r)) {
              r1 <- temp_r
            }
          }
          
          cur_game <- id_game
          if (length(cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$Contribution) > 0) {
            temp_r <- summary (lm (cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$Contribution ~ 
                                     cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$showing_trust_score
                                   + cur_game[cur_game$Type == (1 - type) & cur_game$Partner == user_id & cur_game$Subject == partner_id,]$showing_trust_score))$r.squared
            temp_r <- sqrt (temp_r)
            if (!is.na(temp_r)) {
              r2 <- temp_r
            }
          }
          
          cur_game <- score_game
          if (length(cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$Contribution) > 0) {
            temp_r <- summary (lm (cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$Contribution ~ 
                                     cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$showing_trust_score
                                   + cur_game[cur_game$Type == (1 - type) & cur_game$Partner == user_id & cur_game$Subject == partner_id,]$showing_trust_score))$r.squared
            temp_r <- sqrt (temp_r)
            if (!is.na(temp_r)) {
              r3 <- temp_r
            }
          }
          
          cur_game <- combine_game
          if (length(cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$Contribution) > 0) {
            temp_r <- summary (lm (cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$Contribution ~ 
                                     cur_game[cur_game$Type == type & cur_game$Subject == user_id & cur_game$Partner == partner_id,]$showing_trust_score
                                   + cur_game[cur_game$Type == (1 - type) & cur_game$Partner == user_id & cur_game$Subject == partner_id,]$showing_trust_score))$r.squared
            temp_r <- sqrt (temp_r)
            if (!is.na(temp_r)) {
              r4 <- temp_r
            }
          }
        }
        new_row = c((exp_id - 1) * num_users + user_id,
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$showing_trust_score),
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$send_proportion),
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$Contribution),
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$response_time),
                    SIMPLE_GAME_ORDERS[exp_id],
                    mean (calc_peak_end_trust(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$showing_trust_score)),
                    mean(simple_game[simple_game$Type == type & simple_game$Subject == user_id,]$PartnerDecision),
                    mean(simple_game[simple_game$Type == (1  - type) & simple_game$Partner == user_id,]$showing_trust_score),
                    r1
        )
        df_simple[nrow(df_simple) + 1,] <- new_row
        df_total[nrow(df_total) + 1,] <- c(user_id * num_users * (exp_id - 1),
                                           exp_id,
                                           0,
                                           0,
                                           r1)
        
        new_row = c((exp_id - 1) * num_users + user_id,
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$showing_trust_score),
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$send_proportion),
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$Contribution),
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$response_time),
                    ID_GAME_ORDERS[exp_id],
                    mean(calc_peak_end_trust(id_game[id_game$Type == type & id_game$Subject == user_id,]$showing_trust_score)),
                    mean(id_game[id_game$Type == type & id_game$Subject == user_id,]$PartnerDecision),
                    mean(id_game[id_game$Type == (1  - type) & id_game$Partner == user_id,]$showing_trust_score),
                    r2
        )
        df_id[nrow(df_id) + 1,] <- new_row
        df_total[nrow(df_total) + 1,] <- c(user_id * num_users * (exp_id - 1),
                                           exp_id,
                                           0,
                                           1,
                                           r2)
        
        new_row = c((exp_id - 1) * num_users + user_id,
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$showing_trust_score),
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$send_proportion),
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$Contribution),
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$response_time),
                    SCORE_GAME_ORDERS[exp_id],
                    mean (calc_peak_end_trust(score_game[score_game$Type == type & score_game$Subject == user_id,]$showing_trust_score)),
                    mean(score_game[score_game$Type == type & score_game$Subject == user_id,]$PartnerDecision),
                    mean(score_game[score_game$Type == (1  - type) & score_game$Partner == user_id,]$showing_trust_score),
                    r3
        )
        df_score[nrow(df_score) + 1,] <- new_row
        df_total[nrow(df_total) + 1,] <- c(user_id * num_users * (exp_id - 1),
                                           exp_id,
                                           1,
                                           0,
                                           r3)
        
        new_row = c((exp_id - 1) * num_users + user_id,
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$showing_trust_score),
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$send_proportion),
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$Contribution),
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$response_time),
                    COMBINE_GAME_ORDERS[exp_id],
                    mean (calc_peak_end_trust(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$showing_trust_score)),
                    mean(combine_game[combine_game$Type == type & combine_game$Subject == user_id,]$PartnerDecision),
                    mean(combine_game[combine_game$Type == (1  - type) & combine_game$Partner == user_id,]$showing_trust_score),
                    r4
        )
        df_combine[nrow(df_combine) + 1,] <- new_row
        df_total[nrow(df_total) + 1,] <- c(user_id * num_users * (exp_id - 1),
                                           exp_id,
                                           1,
                                           1,
                                           r4)
      }
    }
    df_total$id <- as.factor (df_total$id)
    df_total$GroupID <- as.factor (df_total$GroupID)
    df_total$SHOW_TRUST <- as.factor (df_total$SHOW_TRUST)
    df_total$SHOW_ID <- as.factor (df_total$SHOW_ID)
    
    print ("ANOVA analysis of regression power on sending behavior on 2 trust scores on Group:SHOW_TRUST:SHOW_ID")
    anova_analysis(df_total, type = type, num_way = 3)
  }
  
  for (type in 0:1) {
    df_simple = data.frame("id" = as.numeric(),
                           "GroupID" = as.numeric(),
                           "SHOW_TRUST" = as.numeric(),
                           "SHOW_ID" = as.numeric(),
                           "R_value" = as.numeric())
    df_id = data.frame("id" = as.numeric(),
                       "GroupID" = as.numeric(),
                       "SHOW_TRUST" = as.numeric(),
                       "SHOW_ID" = as.numeric(),
                       "R_value" = as.numeric())
    df_score = data.frame("id" = as.numeric(),
                          "GroupID" = as.numeric(),
                          "SHOW_TRUST" = as.numeric(),
                          "SHOW_ID" = as.numeric(),
                          "R_value" = as.numeric())
    df_combine = data.frame("id" = as.numeric(),
                            "GroupID" = as.numeric(),
                            "SHOW_TRUST" = as.numeric(),
                            "SHOW_ID" = as.numeric(),
                            "R_value" = as.numeric())
    for (exp_id in 1:num_exp) {
#       if (exp_id == 3) {
#         next()
#       }
      first_round_of_exp_subjects = (exp_id - 1) * num_rounds_per_game * num_users + 1
      last_round_of_exp_subjects = exp_id * num_rounds_per_game * num_users
      
      simple_game = simple_games[first_round_of_exp_subjects:last_round_of_exp_subjects,]
      id_game = id_games [first_round_of_exp_subjects:last_round_of_exp_subjects,]
      score_game = score_games [first_round_of_exp_subjects:last_round_of_exp_subjects,]
      combine_game = combine_games[first_round_of_exp_subjects:last_round_of_exp_subjects,]
      
      # add up all partner decision to 1 unit to remove the NaN problem of R squared
      simple_game$PartnerDecision = simple_game$PartnerDecision + 0
      id_game$PartnerDecision = id_game$PartnerDecision + 0
      score_game$PartnerDecision = score_game$PartnerDecision + 0
      combine_game$PartnerDecision = combine_game$PartnerDecision + 0
      
      for (user_id in 1:num_users) {
        # cur_data <- simple_game[simple_game$Type ==type & simple_game$Partner == user_id & simple_game$send_proportion >= 0,]
        cur_data <- simple_game[simple_game$Type ==type & simple_game$Subject == user_id & simple_game$send_proportion >= 0,]
        new_row <- c((exp_id - 1) * num_users + user_id,
                     exp_id,
                     0,
                     0,
                     #summary(lm(PartnerDecision ~ showing_trust_score, data = simple_game[simple_game$Type ==type & simple_game$Partner == user_id,]))$r.squared
                     # cor(cur_data$PartnerDecision, cur_data$showing_trust_score)
                     cor(cur_data$Contribution, cur_data$showing_trust_score)
        )
        df_simple [nrow(df_simple) + 1,] <- new_row
#         if (type == 1 & user_id == 2 & exp_id == 2) {
#           print (cur_data)
#         }
        
        # cur_data <- id_game[id_game$Type ==type & id_game$Partner == user_id & id_game$send_proportion >= 0,]
        cur_data <- id_game[id_game$Type ==type & id_game$Subject == user_id & id_game$send_proportion >= 0,]
        new_row <- c((exp_id - 1) * num_users + user_id,
                     exp_id,
                     0,
                     1,
                     # summary(lm(PartnerDecision ~ showing_trust_score, data = id_game[id_game$Type ==type & id_game$Partner == user_id,]))$r.squared
                     # cor(cur_data$PartnerDecision, cur_data$showing_trust_score)
                     cor(cur_data$Contribution, cur_data$showing_trust_score)
        )
        df_id [nrow(df_id) + 1,] <- new_row
        
        # cur_data <- score_game[score_game$Type ==type & score_game$Partner == user_id & score_game$send_proportion >= 0,]
        cur_data <- score_game[score_game$Type ==type & score_game$Subject == user_id & score_game$send_proportion >= 0,]
        new_row <- c((exp_id - 1) * num_users + user_id,
                     exp_id,
                     1,
                     0,
                     # summary(lm(PartnerDecision ~ showing_trust_score, data = score_game[score_game$Type ==type & score_game$Partner == user_id,]))$r.squared
                     # cor(cur_data$PartnerDecision, cur_data$showing_trust_score)
                     cor(cur_data$Contribution, cur_data$showing_trust_score)
        )
        df_score [nrow(df_score) + 1,] <- new_row
        # print (summary(lm(PartnerDecision ~ showing_trust_score, data = id_game[id_game$Type ==type & id_game$Partner == user_id,]))$r.squared)
        # print (cor(score_game[score_game$Type ==type & score_game$Partner == user_id,]$PartnerDecision, score_game[score_game$Type ==type & score_game$Partner == user_id,]$showing_trust_score))
        
        # cur_data <- combine_game[combine_game$Type ==type & combine_game$Partner == user_id & combine_game$send_proportion >= 0,]
        cur_data <- combine_game[combine_game$Type ==type & combine_game$Subject == user_id & combine_game$send_proportion >= 0,]
        new_row <- c((exp_id - 1) * num_users + user_id,
                     exp_id,
                     1,
                     1,
                     # summary(lm(PartnerDecision ~ showing_trust_score, data = combine_game[combine_game$Type ==type & combine_game$Partner == user_id,]))$r.squared
                     # cor(cur_data$PartnerDecision, cur_data$showing_trust_score)
                     cor(cur_data$Contribution, cur_data$showing_trust_score)
        )
        df_combine [nrow(df_combine) + 1,] <- new_row
        # print (summary(lm(PartnerDecision ~ showing_trust_score, data = combine_game[combine_game$Type ==type & combine_game$Partner == user_id,]))$r.squared)
        # print (cor(combine_game[combine_game$Type ==type & combine_game$Partner == user_id,]$PartnerDecision, combine_game[combine_game$Type ==type & combine_game$Partner == user_id,]$showing_trust_score))
      }
    }
    df_total <- rbind (df_simple, df_id)
    df_total <- rbind (df_total, df_score)
    df_total <- rbind (df_total, df_combine)
    df_total$id <- as.factor (df_total$id)
    df_total$GroupID <- as.factor (df_total$GroupID)
    df_total$SHOW_TRUST <- as.factor (df_total$SHOW_TRUST)
    df_total$SHOW_ID <- as.factor (df_total$SHOW_ID)
    # df_total$score <- sqrt(df_total$R_value)
    df_total$score <- df_total$R_value
    if (any(is.na(df_total$score))) {
      df_total[is.na(df_total$score),]$score <- 0
    }
    # print (df_total)
    df_total$R_value <- NULL
    
    if (any (is.na(df_simple$R_value))) {
      df_simple[is.na(df_simple$R_value),]$R_value <- 0
    }
    if (any (is.na(df_id$R_value))) {
      df_id[is.na(df_id$R_value),]$R_value <- 0
    }
    if (any (is.na(df_score$R_value))) {
      df_score[is.na(df_score$R_value),]$R_value <- 0
    }
    if (any (is.na(df_combine$R_value))) {
      df_combine[is.na(df_combine$R_value),]$R_value <- 0
    }
    
#     # Get the mean of R_value for each games
#     for (exp_id in 1:num_exp) {
#       print (paste ("Mean of r_value for each games of :", Type_names [(1 - type) + 1], " of group ID: ", exp_id))
#       
#       first_round = num_users * (exp_id - 1) + 1
#       last_round = num_users * exp_id
#       
#       print ("Number of data poinst is 30")
#       print ("Without and With Trust") 
#       print (mean(rbind(df_simple[first_round:last_round,], df_id[first_round:last_round,])$R_value))
#       print (mean(rbind(df_score[first_round:last_round,], df_combine[first_round:last_round,])$R_value))
#       print ("Without and With ID")
#       print (mean(rbind(df_simple[first_round:last_round,], df_score[first_round:last_round,])$R_value))
#       print (mean(rbind(df_id[first_round:last_round,], df_combine[first_round:last_round,])$R_value))
# #       print ("For each games")
# #       print (mean (df_simple$R_value))
# #       print (mean (df_id$R_value))
# #       print (mean (df_score$R_value))
# #       print (mean (df_combine$R_value))
#     }
    
#     for (exp_id in 1:num_exp) {
#       if (exp_id == 3 & type == 1) {
#         first_round = num_users * (exp_id - 1) + 1
#         last_round = num_users * exp_id
#         print ("R value group 3, SENDERs")
# #         print (df_simple[first_round:last_round,]$R_value)
# #         print (df_id[first_round:last_round,]$R_value)
#         print ((df_simple[first_round:last_round,]$R_value + df_id[first_round:last_round,]$R_value) / 2)
#         print ("----")
# #         print (df_score[first_round:last_round,]$R_value)
# #         print (df_combine[first_round:last_round,]$R_value)
#         print ((df_score[first_round:last_round,]$R_value + df_combine[first_round:last_round,]$R_value) / 2)
#       }
#     }
    
#     print (paste("ANOVA analysis for correlation on Group:SHOW_TRUST:SHOW_ID for type: ", Type_names [(type + 1)]))
#     anova_analysis (df = df_total, type = (1 - type))
#     print("*****")
#           print ("Average of R value")
#           print ("Without Trust")
#           for (grp_id in 1:num_exp) {
#             print (mean (df_total[df_total$SHOW_TRUST == 0 & df_total$GroupID == grp_id,]$score))
#           }
#           print ("With Trust")
#           for (grp_id in 1:num_exp) {
#             print (mean (df_total[df_total$SHOW_TRUST == 1 & df_total$GroupID == grp_id,]$score))
#           }
    
    # Make 2 - way ANOVA analysis
    # By making a new code: game_pos, which has 4 possible values 1, 2, 3, 4 for 4 games
#     print (paste("ANOVA analysis for correlation  on Group:Game_Setting for type: ", Type_names [(type + 1)]))
#     df_total$game_pos <- as.factor (((as.numeric (df_total$SHOW_TRUST) - 1) * 2 + (as.numeric (df_total$SHOW_ID) - 1) + 1))
#     df_total$SHOW_TRUST <- NULL
#     df_total$SHOW_ID <- NULL
#     anova_analysis(df_total, num_way = 2, type = (1 - type))
  }
  
  
  print ("---")
  # Create graphs of R_value
  for (type in 0:1) {
    if (type == 1) {next()}
    for (game_id in 1:4) {
      cur_data <- NULL
      if (game_id == 1) {
        cur_data <- simple_games[simple_games$Type == type,]
      }
      if (game_id == 2) {
        cur_data <- id_games[id_games$Type == type,]
      }
      if (game_id == 3) {
        cur_data <- score_games[score_games$Type == type,]
      }
      if (game_id == 4) {
        cur_data <- combine_games[combine_games$Type == type,]
      }
      png (paste("./all_data/R_value_Game_",game_id,"_type_", Type_names [(type + 1)],".png"))
      ys = c (1,2,3,4,5)
      xs = numeric()
      
      # to store confidence interval
      lowers = numeric()
      uppers = numeric()
      
      all_r = numeric ()
      all_grp_id = numeric ()
      for (exp_id in 1:num_exp) {
        num_per_rounds = num_users * (num_users - 1) * average_rounds / 2
        first_round = num_per_rounds * (exp_id - 1) + 1
        last_round = num_per_rounds * exp_id
        sub_data = cur_data[first_round:last_round,]
        
        x <- 0
        lower <- 0
        upper <- 0
        
        rs <- numeric()
        
        for (user_id in 1:num_users) {
          user_data = sub_data[sub_data$Subject == user_id & sub_data$send_proportion >= 0,]
          
          # r = cor (user_data$PartnerDecision, user_data$showing_trust_score)
          r = user_data$Contribution
          if (is.na(r)) {
            r = 0
          }
          rs <- c(rs, r)
          all_r <- c (all_r, r)
          for (i in 1:length(r)) {
            all_grp_id <- c(all_grp_id, exp_id)
          }
        }
        
        x = mean (rs)
        lower = x - pop.sd (rs)
        upper = x + pop.sd (rs)
        
        if (is.na(x)) {
          x = 0
        }
        xs <- c(xs, x)
        if (is.na(lower)) {
          lower = 0
        }
        if (is.na(upper)) {
          upper = 0
        }
        lowers <- c(lowers, lower)
        uppers <- c(uppers, upper)
      }
      if (type == 0) {
        # Do Tukey test
        print (paste ("Absolute sending - Game: ", game_id, " for SENDERs"))
        print ("*********")
#         print (all_r)
#         print (all_grp_id)
        aov1 <- aov (lm (all_r ~ as.factor(all_grp_id)))
        # print (TukeyHSD(aov1))
      }
      plot(xs, main = paste("r_value of Game ", game_id, " type: ", Type_names [(type + 1)]), xlab = "Group ID", ylab = "r value", ylim = c(0,15))
      for (exp_id in 1:num_exp) {
        segments (x0 = exp_id, y0 = lowers[exp_id], x1 = exp_id, y1 = uppers[exp_id])
        epsilon <- 0.02
        segments (x0 = exp_id - epsilon, y0 = lowers[exp_id], x1 = exp_id + epsilon, y1 = lowers[exp_id])
        segments (x0 = exp_id - epsilon, y0 = uppers[exp_id], x1 = exp_id + epsilon, y1 = uppers[exp_id])
      }
      dev.off()
    }
  }
  
  
  # compare our data with data of Giangicomo
  
#   require (BSDA)
#   data2 <- read.csv ("./all_data/Data2.csv")
#   
#   print ("Comparing two dataset for SENDERs")
#   print (z.test(data2[!is.na(data2$daAaB),]$daAaB,  sigma.x = 6.8))
#   print (z.test(simple_games[simple_games$Type == 0,]$Contribution, sigma.x = 6.8))
#   
#   print ("Comparing two dataset for RECEIVERs")
#   print (z.test(data2[!is.na(data2$daBaA),]$daBaA,  sigma.x = 6.8))
#   print (z.test(simple_games[simple_games$Type == 1,]$Contribution, sigma.x = 6.8))
#   
#   means = c (3.002667, 3.485185, 2.277333, 2.792593)
#   xs = c (1,1.5,3,3.5)
#   plot (x = xs, y = means, ylab = "Average sending amount", main = "Comparing two datasets", xaxt = "n", xlim = c(0,8), ylim = c(0,4), xlab = "")
#   axis(1, at=1:5, labels= c("Sender","","Receiver","","'"), las = 2)
#   segments(x0 = 1, y0 = 2.314424, x1 = 1, y1 = 3.690909, col = "red")
#   segments(x0 = 1.5, y0 = 2.91165, x1 = 1.5, y1 = 4.05872, col = "blue")
#   segments(x0 = 3, y0 = 1.589091, x1 = 3, y1 = 2.965576, col = "red")
#   segments(x0 = 3.5, y0 = 2.219057, x1 = 3.5, y1 = 3.366128, col = "blue")
#   legend(4, max(means) * 1.05,
#          c ("Our data", "Other data"),
#          lty = c(1,1),
#          lwd = c(3,2),
#          col = c("red","blue"))
  
  df
}

processMultiSBJ <- function (directory, num_users = 6, average_rounds = 5, num_games = 4) {
  # source("http://www.kirchkamp.de/lab/zTree.R")
  sessions <- sessions<-list.files(directory,"[0-9]{6}_[0-9]{4}.sbj",recursive=TRUE, full.names = TRUE)
  SBJs <- zTreeSbj (files = sessions, zTree.silent=TRUE)
  
  # You are sender, what game you receive back most, with same amount of sending
  SBJs$receive_back_most <- as.factor(SBJs$receive_back_most)
  
  SBJs$best_personal <- as.factor(SBJs$best_personal)
  SBJs$worst_personal <- as.factor(SBJs$worst_personal)
  SBJs$best_total <- as.factor(SBJs$best_total)
  SBJs$worst_total <- as.factor(SBJs$worst_total)
  
  # In Simple Game, profit is higher if you send more
  SBJs$send_more_for_profit <- as.numeric(SBJs$send_more_for_profit)
  # In Simple Game, profit is higher if you send less
  SBJs$send_less_for_profit <- as.numeric(SBJs$send_less_for_profit)
  # In Simple Game, you are receive, you send back more if your sender send more
  SBJs$trust_help_receiver <- as.numeric(SBJs$trust_help_receiver)
  
  # Show ID and Score help to realize behavior of partners in the history
  SBJs$show_id_help <- as.numeric(SBJs$show_id_help)
  SBJs$show_score_help <- as.numeric(SBJs$show_score_help)
  SBJs$show_combine_help <- as.numeric(SBJs$show_combine_help)
  
  # In Combine game, trust score reflects correct behavior of the partner
  SBJs$trust_score_correctness <- as.numeric(SBJs$trust_score_correctness)
  
  # In Score game, you send more if your partner has higher trust score
  SBJs$trust_help_sender <- as.numeric(SBJs$trust_help_sender)
  
  # Showing ID and score help you decide how to behave
  SBJs$identity_help_decide <- as.numeric(SBJs$identity_help_decide)
  SBJs$trust_score_help_decide <- as.numeric(SBJs$trust_score_help_decide)
  SBJs$combine_help_decide <- as.numeric(SBJs$combine_help_decide)
  
  # What factor is most important
  SBJs$important_factor <- as.factor(SBJs$important_factor)
  
  SBJs$partner_fair <- as.numeric(SBJs$partner_fair)
  SBJs
}

processDubois <- function (XLS_file) {
  dubois <- read.csv (XLS_file, sep = ";")
  dubois$SHOW_SENDER <- ifelse(dubois$treatment == 2, 1 , 0)
  dubois$SHOW_SENDER <- as.factor(dubois$SHOW_SENDER)
  dubois$SHOW_RECEIVERS <- ifelse(dubois$treatment == 0, 0 , 1)
  dubois$SHOW_RECEIVERS <- as.factor(dubois$SHOW_RECEIVERS)
  dubois$treatment <- as.factor(dubois$treatment)
  dubois$group <- as.factor(duboi$group)
}

# given a list of behavior, calculate trust
calcTrust <- function(send_amount)
{
  # number of rounds
  ROUNDS = length(send_amount)
  
  res = c()
  Beta = 0.0
  Alpha = 0.0
  ATF = 0.0
  ExpectTrust = 0.0
  DeviationReliability = 0.0
  HistoryTrust = 0.0
  Epsilon = 0.3
  Phi = 0.1
  MAX_ATF = 2.0
  RecentTrust = 0.0
  c = 0.9
  threshold = 0.25
  DeviationFactor = 0.0
  TrustScore = 0.5
  
  for (i in 1:ROUNDS) {
    if (send_amount[i] < 0) {
      next()
    }
    curRecentTrust 	= log (send_amount[i] * (exp (1) - 1) + 1)
    curDelta		= abs (RecentTrust - curRecentTrust)
    RecentTrust 	= curRecentTrust
    Beta			= c * curDelta + (1 - c) * Beta
    Alpha			= threshold + c * curDelta / (1 + Beta)
    HistoryTrust	= Alpha * RecentTrust + (1 - Alpha) * HistoryTrust
    
    #print RecentTrust
    #print Alpha
    
    if (RecentTrust == 0 & HistoryTrust == 0) {
      ExpectTrust = 0
    }
    else if (RecentTrust - HistoryTrust > Epsilon){
      DeviationFactor = DeviationFactor + 0.1
    }
    else if (HistoryTrust - RecentTrust > Epsilon) {
      DeviationFactor =DeviationFactor - 0.1
    }
    ExpectTrust = RecentTrust * DeviationFactor + HistoryTrust * (1 - DeviationFactor)
    
    #print ExpectTrust
    #print DeviationFactor
    #print HistoryTrust
    
    if (RecentTrust - HistoryTrust > Phi) {
      ATF = ATF + (RecentTrust - HistoryTrust) / 2
    }
    else if (HistoryTrust - RecentTrust > Phi) {
      ATF = ATF + (HistoryTrust - RecentTrust)
    }
    
    if (ATF > MAX_ATF) {
      DeviationReliability = 0.0
      ATF = ATF / 2
    }
    else {
      DeviationReliability = cos ((pi / 2) * (ATF/MAX_ATF))
    }
    
    TrustScore = ExpectTrust * DeviationReliability
    #res.append ("%.2f" % TrustScore)
    res = c(res, TrustScore)
  }
  
  
  res
}

calcReputation <- function (send_proportion_list) {
  res <- c()
  for (i in 1:length(send_proportion_list)) {
    res <- c(res, mean (send_proportion_list[1:i]))
  }
  res
}

verifyCBDTwithBravo <- function (file_name = "all_data/Data2.csv", round_number = 5, type =0, p_recall = 0.75) {
  bravo <- read.csv (file = file_name)
  newID <- unique (bravo$newID)
  
  bravo$send_proportion <- ifelse(bravo$type == 1, bravo$daAaB/10, bravo$daBaA / 3 / bravo$actualDaAaB)
  bravo$send_proportion <- ifelse(bravo$send_proportion > 1, 1, bravo$send_proportion)
  
  # store the best memory before this round and the behavior of this round
  last_action = data.frame ("pre_trust" = as.numeric(),
                            "action" = as.numeric())
  
  if (type == 0) {
    for (id in newID) {
      actions <- bravo[bravo$newID == id & !is.na(bravo$daAaB),]$send_proportion
      # actions <- bravo[bravo$newID == id & !is.na(bravo$daBaA),]$daBaA / 10
      
      # looking for max benefit so far
      max_payoff <- 0
      max_payoff_id <- 0
      for (i in 1:nrow(bravo)) {
        if (bravo$newID[i] == id & !is.na(bravo$daAaB[i])) {
          r <- runif (1, min = 0, max = 1)
          if (r <= p_recall) {
            cur_payoff <- bravo$actualDaBaA[i] - bravo$daAaB[i]
            if (cur_payoff > max_payoff) {
              max_payoff <- cur_payoff
              max_payoff_id <- i
            }
          }
        }
      }
      
      x <- 0.5  # default if could not find any good memory
      if (max_payoff_id >= 1) {
        x <- bravo$daAaB [max_payoff_id] / 10
      }
      
      new_row <- c(x, actions[round_number])
      last_action[nrow(last_action) + 1,] <- new_row
    }
  } else if (type == 1) {
    for (id in newID) {
      actions <- bravo[bravo$newID == id & !is.na(bravo$daAaB),]$send_proportion
      # actions <- bravo[bravo$newID == id & !is.na(bravo$daBaA),]$daBaA / 10
      
      # looking for max benefit so far
      max_payoff <- 0
      max_payoff_id <- 0
      for (i in 1:nrow(bravo)) {
        if (bravo$newID[i] == id & !is.na(bravo$daBaA[i])) {
          r <- runif (1, min = 0, max = 1)
          if (r <= p_recall) {
            cur_payoff <- bravo$actualDaBaA[i] * 3 - bravo$daBaA[i]
            if (cur_payoff > max_payoff) {
              max_payoff <- cur_payoff
              max_payoff_id <- i
            }
          }
        }
      }
      
      x <- 0.5  # default if could not find any good memory
      if (max_payoff_id >= 1) {
        x <- bravo$daAaB [max_payoff_id] / 10
      }
      
      new_row <- c(x, actions[round_number])
      last_action[nrow(last_action) + 1,] <- new_row
    }
  }
  
  plot (x = last_action$pre_trust, y = last_action$action, xlab = paste("CBDT prediction"), ylab = paste("Sending behavior"))
  lm1 <- lm (action ~ pre_trust, data = last_action)
  abline(lm1, col = "red")
  print (cor (last_action$pre_trust , last_action$action))
  print ("---")
  print (summary (lm1))
}

verifyReputationWithBravo <- function (file_name = "all_data/Data2.csv", round_number = 5, type =0) {
  bravo <- read.csv (file = file_name)
  newID <- unique (bravo$newID)
  
  bravo$send_proportion <- ifelse(bravo$type == 1, bravo$daAaB/10, bravo$daBaA / 3 / bravo$actualDaAaB)
  bravo$send_proportion <- ifelse(bravo$send_proportion > 1, 1, bravo$send_proportion)
  
  # Trust score before round 5 (last round) and action at round 5 of each sender
  last_action = data.frame ("pre_trust" = as.numeric(),
                            "action" = as.numeric())
  
  if (type == 0) {
    for (id in newID) {
      actions <- bravo[bravo$newID == id & !is.na(bravo$daAaB),]$send_proportion
      # actions <- bravo[bravo$newID == id & !is.na(bravo$daBaA),]$daBaA / 10
      x <- calcReputation (actions)
      
      new_row <- c(x[(round_number - 1)], actions[round_number])
      last_action[nrow(last_action) + 1,] <- new_row
    }
  } else if (type == 1) {
    for (id in newID) {
      actions <- bravo[bravo$newID == id & !is.na(bravo$daBaA),]$send_proportion
      actions <- actions [!is.na(actions)]
      x <- calcReputation (actions)
      
      new_row <- c(x[(round_number - 1)], actions[round_number])
      last_action[nrow(last_action) + 1,] <- new_row
    }
  }
  
  plot (x = last_action$pre_trust, y = last_action$action, xlab = paste("Trust score before round ", round_number), ylab = paste("Sending behavior in round ", round_number))
  lm1 <- lm (action ~ pre_trust, data = last_action)
  abline(lm1, col = "red")
  print (cor (last_action$pre_trust , last_action$action))
  print ("---")
  print (summary (lm1))
}

verifyTrustWithBravo <- function (file_name= "all_data/Data2.csv", round_number = 5, type =0) {
  bravo <- read.csv (file = file_name)
  newID <- unique (bravo$newID)
  
  bravo$send_proportion <- ifelse(bravo$type == 1, bravo$daAaB/10, bravo$daBaA / 3 / bravo$actualDaAaB)
  bravo$send_proportion <- ifelse(bravo$send_proportion > 1, 1, bravo$send_proportion)
  
  # Trust score before round 5 (last round) and action at round 5 of each sender
  last_action = data.frame ("pre_trust" = as.numeric(),
                            "action" = as.numeric())
  
  if (type == 0) {
    for (id in newID) {
      actions <- bravo[bravo$newID == id & !is.na(bravo$daAaB),]$send_proportion
      # actions <- bravo[bravo$newID == id & !is.na(bravo$daBaA),]$daBaA / 10
      x <- calcTrust (actions)
      
      new_row <- c(x[(round_number - 1)], actions[round_number])
      last_action[nrow(last_action) + 1,] <- new_row
    }
  } else if (type == 1) {
    for (id in newID) {
      actions <- bravo[bravo$newID == id & !is.na(bravo$daBaA),]$send_proportion
      actions <- actions [!is.na(actions)]
      x <- calcTrust (actions)
      
      new_row <- c(x[(round_number - 1)], actions[round_number])
      last_action[nrow(last_action) + 1,] <- new_row
    }
  }
  
  plot (x = last_action$pre_trust, y = last_action$action, xlab = paste("Trust score before round ", round_number), ylab = paste("Sending behavior in round ", round_number))
  lm1 <- lm (action ~ pre_trust, data = last_action)
  abline(lm1, col = "red")
  print (cor (last_action$pre_trust , last_action$action))
  print ("---")
  print (summary (lm1))
}

verifyTrustWithDubois <- function (file_name, type = 0, round = 5) {
  dubois <- read.csv("all_data/data_dubois.csv", sep = ";")
  
  # dubois$group <- dubois$group + dubois$treatment * 6
  # dubois$player_uid <- dubois$player_uid + dubois$treatment * 36
  # dubois$group <- as.factor(dubois$group)
  # dubois$treatment <- as.factor(dubois$treatment)
  # dubois$player_uid <- as.factor(dubois$player_uid)
  # 
  # user_sent <- as.vector (aggregate(dubois$sent, list(dubois$player_uid), mean)[,'x'])
  # user_sent_back <- as.vector(aggregate(dubois$sent_back, list(dubois$player_uid), mean)[,'x'])
  # user_reciprocity <- as.vector (aggregate(dubois$sent_back/dubois$received, list(dubois$player_uid), mean, na.rm = TRUE)[,'x'])
  # user_sender_payoff <- as.vector(aggregate(dubois$returned - dubois$sent, list(dubois$player_uid), mean, na.rm = TRUE)[,'x'])
  # user_receiver_payoff <- as.vector(aggregate(dubois$received - dubois$sent_back, list(dubois$player_uid), mean, na.rm = TRUE)[,'x'])
  # treatment <- as.factor (c(rep(0,36), rep (1, 36), rep(2,36)))
  # group <- as.factor (rep (1:18, each = 6))
  
  dubois1 <- dubois[1:1080,]
  
  
  dubois1$trust <- ave(dubois1$sent/10, dubois1$player_uid, FUN = calcTrust)
  last_trust = dubois1[dubois1$period == (round - 1),]$trust
  last_action = dubois1[dubois1$period == round,]$sent/10
  
  if (type == 1) {
    dubois1$send_back_pror <- ifelse(is.nan(dubois1$sent_back/dubois1$received), -1, dubois1$sent_back/dubois1$received)
    # dubois1$trust <- ave (dubois1$sent_back_pror, dubois1$player_uid, FUN = calcTrust)
    last_trust <- c()
    for (uid in unique (dubois1$player_uid)) {
      actions <- dubois1[dubois1$player_uid == uid,]$send_back_pror
      trusts <- calcTrust(actions)
      trust <- trusts [round - 1]
      last_trust <- c(last_trust, trust)
    }
    last_action = dubois1[dubois1$period == round,]$send_back_pror
  }
  
  last_trust <- last_trust [last_action >= 0]
  last_action <- last_action [last_action >= 0]
  
  plot (x = last_trust, y = last_action, xlab = paste("Trust score before round ", round), ylab = paste("Sending behavior in round ", round))
  lm2 <- lm(formula = last_action ~ last_trust)
  abline (lm2, col = "red")
  print (cor (last_action , last_trust))
  print ("--")
  print(summary (lm2))
}

verifyReputationWithDubois <- function (file_name, type = 0, round = 5) {
  dubois <- read.csv("all_data/data_dubois.csv", sep = ";")
  
  dubois1 <- dubois[1:1080,]
  
  dubois1$send_back_pror <- ifelse(is.nan(dubois1$sent_back/dubois1$received), -1, dubois1$sent_back/dubois1$received)
  last_action = dubois1[dubois1$period == round,]$sent/10
  dubois_reputation <- c()
  
  for (i in 1:nrow(dubois1)) {
    cur_id <- dubois1[i,]$player_uid
    cur_rep <- 0.5
    cur_period <- dubois1[i,]$period
    if (cur_period == 1) {
      cur_rep <- 0.5
    } else {
      x <- dubois1[dubois1$player_uid == cur_id,]$send_back_pror[1:(cur_period-1)]
      x <- x[x>=0]
      cur_rep <- mean (c(dubois1[dubois1$player_uid == cur_id,]$sent[1:(cur_period-1)]/10, 
                         x, na.rm = TRUE))
    }
    dubois_reputation <- c(dubois_reputation, cur_rep)
  }
  
  dubois1 <- cbind (dubois1, dubois_reputation)
  
  last_rep <- c()
  
  for (uid in unique (dubois1$player_uid)) {
    actions <- dubois1[dubois1$player_uid == uid,]$send_back_pror
    rep <- dubois1[dubois1$player_uid == uid,]$dubois_reputation[round -1]
    last_rep <- c(last_rep, rep)
  }
  
  if (type == 1) {
    # dubois1$trust <- ave (dubois1$sent_back_pror, dubois1$player_uid, FUN = calcTrust)
    
    
    last_action = dubois1[dubois1$period == round,]$send_back_pror
  }
  
  last_rep <- last_rep [last_action >= 0]
  last_action <- last_action [last_action >= 0]
  
  plot (x = last_rep, y = last_action, xlab = paste("Reputation score before round ", round), ylab = paste("Sending behavior in round ", round))
  lm2 <- lm(formula = last_action ~ last_rep)
  abline (lm2, col = "red")
  print (cor (last_action , last_rep))
  print ("--")
  print(summary (lm2))
}

verifyTrustWithDuboisSimple <- function (file_name = "all_data/data_dubois.csv", type = 0, round = 5) {
  dubois <- read.csv(file_name, sep = ";")
  dubois1 <- dubois[1:1080,]
  last_trust = c()
  for (uid in unique (dubois1$player_uid)) {
    trust = mean (dubois1[dubois1$player_uid == uid & dubois1$period < round,]$sent/10)
    last_trust = c(last_trust, trust)
  }
  last_action = dubois1[dubois1$period == round,]$sent/10
  plot (x = last_trust, y = last_action, xlab = paste("Trust score before round ", round), ylab = paste("Sending behavior in round ", round))
  lm2 <- lm(formula = last_action ~ last_trust)
  abline (lm2, col = "red")
  print (cor (last_action , last_trust))
  print ("--")
  print(summary (lm2))
}

verifyOwnTrust <- function (round_number, type = 0) {
  zTT <- readMultiXLS ("./all_data")
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
  
  # first, create empty data frames to hold all the particular games
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
  
  s = simple_games[simple_games$Type == type,]
  s$my_trust = ave(s$Contribution/10, s$Date, s$Subject, FUN = calcTrust)
  
  last_actions = c()
  last_trusts = c()
  
  for (date in unique(s$Date)) {
    for (subject in 1:6) {
      last_action = s[s$Date == date & s$Subject == subject,]$Contribution[round_number]
      last_trust = s[s$Date == date & s$Subject == subject,]$my_trust[(round_number - 1)]
      last_actions = c(last_actions, last_action/10)
      last_trusts = c(last_trusts, last_trust)
    }
  }
  plot (x = last_trusts, y = last_actions, xlab = paste("Trust score before round ", round_number), ylab = paste("Sending behavior at round ", round_number),
        cex.lab = 1.0)
  lm2 <- lm(formula = last_actions ~ last_trusts)
  abline (lm2, col = "red")
  print (cor (last_actions , last_trusts))
  print ("--")
  print(summary (lm2))
}

verifyOwnTrustWithSimpleMetrics = function (type = 0, round_number = 5)
{
  zTT <- readMultiXLS ("./all_data")
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
  
  # first, create empty data frames to hold all the particular games
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
  
  s = simple_games[simple_games$Type == type,]
  
  last_actions = c()
  last_trusts = c()
  
  for (date in unique(s$Date)) {
    for (subject in 1:6) {
      last_action = s[s$Date == date & s$Subject == subject,]$Contribution[round_number]
      last_trust = mean (s[s$Date == date & s$Subject == subject,]$Contribution[1:(round_number-1)])/10
      last_actions = c(last_actions, last_action/10)
      last_trusts = c(last_trusts, last_trust)
    }
  }
  
  
  plot (x = last_trusts, y = last_actions, xlab = paste("Trust score before round ", round_number), ylab = paste("Sending behavior at round ", round_number),
        cex.lab = 1.5)
  lm2 <- lm(formula = last_actions ~ last_trusts)
  abline (lm2, col = "red")
  print (cor (last_actions , last_trusts))
  print ("--")
  print(summary (lm2))
}
