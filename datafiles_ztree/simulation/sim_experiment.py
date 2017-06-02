import random
import pylab
import numpy as np
import matplotlib.mlab as mlab

#To simulate the experiment between 2 users repeat 5 rounds
#and there are 20 pairs of user

ROUNDS = 5
#number of users = EXPERIMENT_LEN * 2
EXPERIMENT_LEN = 40
N = 2000

send_prob = [1,2,5,14,22,26,15,10,3,1,1]
#back_prob = [1,6,22,30,26,10,1,1,1,1,1]
back_prob = [1,6,16,30,26,16,1,1,1,1,1]

#type = 0: send
#type = 1: send back
def newSend (Type):
	rand = random.uniform (0,1) * 100
	prob = []
	if (Type == 0):
		prob = send_prob
	elif (Type == 1):
		prob = back_prob
	for i in range (len (prob)):
		s = 0
		for j in range (i + 1):
			s += prob [j]
		if (rand <= s):
			return i
	return len (prob) - 1

def normalNewSend (person_type):
	new_send = 0

	mean = 5.0
	sigma = 1.5
	#bad guy
	if (person_type == 0):
		mean = 3
	if (person_type == 1):
		mean = 5
	if (person_type == 2):
		mean = 8
	new_send = random.gauss (mu = mean, sigma = sigma)
	if (person_type == 3):
		rand = random.uniform (0, 1)
		if (rand <= 0.25):
			new_send = 0
		elif (rand <= 0.5):
			new_send = 1
		elif (rand <= 0.75):
			new_send = 9
		else:
			new_send = 10

	if (new_send < 0):
		new_send = 0.0
	if (new_send > 10):
		new_send = 10.0
	return new_send

def sim_period (person_type):
	trusts = []
	trusts2 = []
	sends = []
	send_backs = []

	trust = 0.5
	trust2 = 0.5

	for i in range (ROUNDS):
		Type = 0
		type_rand = random.uniform (0, 1)
		if (type_rand <= 0.5):
			Type = 1

		# if (type == 0):
		# 	new_send = random.gauss (mu = 0.5, sigma = 0.15)
		# 	trust = 0.5 * trust + 0.5 * new_send
		# 	sends.append (new_send)
		# elif (type == 1):
		# 	new_send = random.gauss (mu = 0.4, sigma = 0.1)
		# 	trust = 0.5 * trust + 0.5 * new_send
		# 	send_backs.append (new_send)

		#new_send = newSend (Type)
		new_send = normalNewSend (person_type)
		new_trust = 0.0

		if (Type == 0):
			sends.append (new_send)
			new_trust = float (new_send) / 10
			trust = 0.5 * trust + 0.5 * new_trust
		elif (Type == 1):
			send_backs.append (new_send)
			if (new_send <= (20.0/3)):
				new_trust = (0.75) * float (new_send) / 10
			else:
				new_trust = (1.5) * float (new_send) / 10 - 0.5

			trust = 0.5 * trust + 0.5 * new_trust

			if (new_send <= 10.0/3):
				new_trust = 1.5 * float (new_send) / 10
			else:
				new_trust = 0.75 * float (new_send) / 10 + 0.25
			trust2 = 0.5 * trust2 + 0.5 * new_trust

		trusts.append (trust)
		trusts2.append (trust2)
	return [sends, send_backs, trusts, trusts2]

def sim_information_game ():
	count_1 = 0.0
	count_2 = 0.0
	for i in range (EXPERIMENT_LEN):
		a, a, trusts_1, trusts2_1 = sim_period (0)
		a, a, trusts_2, trusts2_2 = sim_period (1)
		a, a, trusts_3, trusts2_3 = sim_period (2)

		if ((np.mean (trusts_1) + np.std (trusts_1) < np.mean (trusts_2) - np.std (trusts_2)) and (np.mean (trusts_2) + np.std (trusts_2) < np.mean (trusts_3) - np.std (trusts_3))):
			count_1 += 1

		if ((np.mean (trusts2_1) + np.std (trusts2_1) < np.mean (trusts2_2) - np.std (trusts2_2)) and (np.mean (trusts2_2) + np.std (trusts2_2) < np.mean (trusts2_3) - np.std (trusts2_3))):
			count_2 += 1

	return [count_1/EXPERIMENT_LEN, count_2/EXPERIMENT_LEN]

#Run the simulation many times
trusts_1 = []
trusts_2 = []

for i in range (N):
	print "Simulation: " + str (i)
	trust1, trust2 = sim_information_game ()
	trusts_1.append (trust1)
	trusts_2.append (trust2)

weights = np.ones_like(trusts_1)/len(trusts_1)
pylab.hist (trusts_1, alpha = 0.5, label = "Current trust formula", normed = 1, weights = weights)

weights = np.ones_like(trusts_2)/len(trusts_2)
pylab.hist (trusts_2, alpha = 0.5, label = "Proposed trust formula", normed = 1, weights = weights)
pylab.legend ()
pylab.show ()


