from random import shuffle
import random

import sys
#first parameter is number of games, should be 4
#second parameter is number of players

full_schedule = []
num_games = int (sys.argv [1])
num_players = int (sys.argv [2])
file_name = sys.argv [3]

#define how many rounds two players interact in a particular game
avg_round_pair = 5

for i in range(num_games):
    first_round = range(num_players)
    #by using the algorithm at https://en.wikipedia.org/wiki/Round-robin_tournament#Scheduling_algorithm
    schedule = []
    for i in range (num_players - 1):
        #each player plays 5 times with each player
        for j in range(avg_round_pair):
            # until now we have a data looks like: 1 3 2 4, means  p1 will play with p2, etc...
            # now we need to convert to zTree format, like: 1 2 4 3, means p1 play with p2 and p1 is sender, p3 with p4 and p4 is sender
            # schedule.append(first_round)
            # print first_round
            period = [0] * num_players
            for k in range (len (first_round)):
                if k >= num_players / 2:
                    continue
                group_id = k
                rand = random.random ()
                if (rand < 0.5):
                    period [first_round[k]] = group_id
                    period [first_round[k + num_players / 2]] = group_id + num_players / 2
                else:
                    period [first_round[k]] = group_id + num_players / 2
                    period [first_round[k + num_players / 2]] = group_id 
            schedule.append (period)
        #generate next round schedule based on the algorithm on wikipedia
        next_round = []
        next_round.append(first_round[0])
        next_round.append (first_round[num_players/2])
        for k in range (num_players/2 - 2):
            next_round.append (first_round [k + 1])
        for k in range (num_players/2 - 1):
            next_round.append (first_round [num_players / 2 + k + 1])
        next_round.append (first_round [num_players/2 - 1])

        first_round = next_round
    shuffle(schedule)
    full_schedule.extend(schedule)

#print full_schedule

# test the schedule
for i in range (num_players):
    for j in range (num_players):
        if i == j:
            continue
        for k in range (num_games):
            start_round = k * (num_players - 1) * avg_round_pair
            end_round = (k + 1) * (num_players - 1) * avg_round_pair
            mets = 0
            for k1 in range (start_round, end_round):
                this_round = full_schedule [k1]
                rank_i = this_round [i]
                rank_j = this_round [j]
                if abs (rank_i - rank_j) == num_players / 2:
                    mets += 1
            print 'In game ' + str (k) + ": " + str(i) + " vs " + str (j) + "   " + str (mets) + " times."


# write to the file
f = open (file_name, "w")

# write first line
f.write ("schedule\tPeriod\tSubject\tRank\n")

for i in range (len (full_schedule)):
    for j in range (num_players):
        # period
        f.write ("schedule\t" + str(i + 1) + "\t")
        # subject
        f.write (str (j + 1) + "\t")
        # rank
        f.write (str(full_schedule[i][j] + 1) + "\n")
        # if not (j == num_players - 1 and i == len (full_schedule) - 1):
            # f.write ("\n")
f.close ()