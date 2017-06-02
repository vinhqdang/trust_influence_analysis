import random
import pylab
import numpy as np
import matplotlib.mlab as mlab

ROUNDS = 5
#number of users = EXPERIMENT_LEN * 2
EXPERIMENT_LEN = 20
N = 1000000

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

def sim_person (person_type):
	trusts = []
	trusts2 = []
	sends = []
	send_backs = []

	trust = 0.5
	trust2 = 0.5

	for i in range (N):
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

exp_1 = sim_person (0)
exp_2 = sim_person (1)
exp_3 = sim_person (2)
exp_4 = sim_person (3)

print 'Mean'
print np.mean (exp_1[2])
print np.mean (exp_2[2])
print np.mean (exp_3[2])
print np.mean (exp_4[2])
print 'Standard deviation'
print np.std (exp_1[2])
print np.std (exp_2[2])
print np.std (exp_3[2])
print np.std (exp_4[2])


#Plot
x = np.linspace(0,1,1000)
pylab.plot(x,mlab.normpdf(x,np.mean (exp_1[2]),np.std (exp_1[2])), color = 'blue', label = 'Bad guy')
pylab.plot ([np.mean (exp_1[2]), np.mean (exp_1[2])], [0, 10], linestyle = '-', color = 'blue')
pylab.plot ([np.mean (exp_1[2]) - np.std (exp_1[2]), np.mean (exp_1[2]) - np.std (exp_1[2])], [0, 10], linestyle = '--', color = 'blue')
pylab.plot ([np.mean (exp_1[2]) + np.std (exp_1[2]), np.mean (exp_1[2]) + np.std (exp_1[2])], [0, 10], linestyle = '--', color = 'blue')
#pylab.plot ([np.mean (exp_1[2]) - 2 * np.std (exp_1[2]), np.mean (exp_1[2]) - 2 * np.std (exp_1[2])], [0, 10], linestyle = ':', color = 'blue')
#pylab.plot ([np.mean (exp_1[2]) + 2 * np.std (exp_1[2]), np.mean (exp_1[2]) + 2 * np.std (exp_1[2])], [0, 10], linestyle = ':', color = 'blue')

pylab.plot(x,mlab.normpdf(x,np.mean (exp_2[2]),np.std (exp_2[2])), color = 'green', label = 'Normal guy')
pylab.plot ([np.mean (exp_2[2]), np.mean (exp_2[2])], [0, 10], linestyle = '-', color = 'green')
pylab.plot ([np.mean (exp_2[2]) - np.std (exp_2[2]), np.mean (exp_2[2]) - np.std (exp_2[2])], [0, 10], linestyle = '--', color = 'green')
pylab.plot ([np.mean (exp_2[2]) + np.std (exp_2[2]), np.mean (exp_2[2]) + np.std (exp_2[2])], [0, 10], linestyle = '--', color = 'green')
#pylab.plot ([np.mean (exp_2[2]) - 2 * np.std (exp_2[2]), np.mean (exp_2[2]) - 2 * np.std (exp_2[2])], [0, 10], linestyle = ':', color = 'green')
#pylab.plot ([np.mean (exp_2[2]) + 2 * np.std (exp_2[2]), np.mean (exp_2[2]) + 2 * np.std (exp_2[2])], [0, 10], linestyle = ':', color = 'green')

pylab.plot(x,mlab.normpdf(x,np.mean (exp_3[2]),np.std (exp_3[2])), color = 'red', label = 'Good guy')
pylab.plot ([np.mean (exp_3[2]), np.mean (exp_3[2])], [0, 10], linestyle = '-', color = 'red')
pylab.plot ([np.mean (exp_3[2]) - np.std (exp_3[2]), np.mean (exp_3[2]) - np.std (exp_3[2])], [0, 10], linestyle = '--', color = 'red')
pylab.plot ([np.mean (exp_3[2]) + np.std (exp_3[2]), np.mean (exp_3[2]) + np.std (exp_3[2])], [0, 10], linestyle = '--', color = 'red')
#pylab.plot ([np.mean (exp_3[2]) - 2 * np.std (exp_3[2]), np.mean (exp_3[2]) - 2 * np.std (exp_3[2])], [0, 10], linestyle = ':', color = 'red')
#pylab.plot ([np.mean (exp_3[2]) + 2 * np.std (exp_3[2]), np.mean (exp_3[2]) + 2 * np.std (exp_3[2])], [0, 10], linestyle = ':', color = 'red')

pylab.legend ()
pylab.title ('Mean and std of trust scores (2a = 0.5)')
pylab.show ()

print '----'

print 'Mean'
print np.mean (exp_1[3])
print np.mean (exp_2[3])
print np.mean (exp_3[3])
print np.mean (exp_4[3])
print 'Standard deviation'
print np.std (exp_1[3])
print np.std (exp_2[3])
print np.std (exp_3[3])
print np.std (exp_4[3])

x = np.linspace(0,1,1000)
pylab.plot(x,mlab.normpdf(x,np.mean (exp_1[3]),np.std (exp_1[3])), color = 'blue', label = 'Bad guy')
pylab.plot ([np.mean (exp_1[3]), np.mean (exp_1[3])], [0, 10], linestyle = '-', color = 'blue')
pylab.plot ([np.mean (exp_1[3]) - np.std (exp_1[3]), np.mean (exp_1[3]) - np.std (exp_1[3])], [0, 10], linestyle = '--', color = 'blue')
pylab.plot ([np.mean (exp_1[3]) + np.std (exp_1[3]), np.mean (exp_1[3]) + np.std (exp_1[3])], [0, 10], linestyle = '--', color = 'blue')
#pylab.plot ([np.mean (exp_1[3]) - 2 * np.std (exp_1[3]), np.mean (exp_1[3]) - 2 * np.std (exp_1[3])], [0, 10], linestyle = ':', color = 'blue')
#pylab.plot ([np.mean (exp_1[3]) + 2 * np.std (exp_1[3]), np.mean (exp_1[3]) + 2 * np.std (exp_1[3])], [0, 10], linestyle = ':', color = 'blue')

pylab.plot(x,mlab.normpdf(x,np.mean (exp_2[3]),np.std (exp_2[3])), color = 'green', label = 'Normal guy')
pylab.plot ([np.mean (exp_2[3]), np.mean (exp_2[3])], [0, 10], linestyle = '-', color = 'green')
pylab.plot ([np.mean (exp_2[3]) - np.std (exp_2[3]), np.mean (exp_2[3]) - np.std (exp_2[3])], [0, 10], linestyle = '--', color = 'green')
pylab.plot ([np.mean (exp_2[3]) + np.std (exp_2[3]), np.mean (exp_2[3]) + np.std (exp_2[3])], [0, 10], linestyle = '--', color = 'green')
#pylab.plot ([np.mean (exp_2[3]) - 2 * np.std (exp_2[3]), np.mean (exp_2[3]) - 2 * np.std (exp_2[3])], [0, 10], linestyle = ':', color = 'green')
#pylab.plot ([np.mean (exp_2[3]) + 2 * np.std (exp_2[3]), np.mean (exp_2[3]) + 2 * np.std (exp_2[3])], [0, 10], linestyle = ':', color = 'green')

pylab.plot(x,mlab.normpdf(x,np.mean (exp_3[3]),np.std (exp_3[3])), color = 'red', label = 'Good guy')
pylab.plot ([np.mean (exp_3[3]), np.mean (exp_3[3])], [0, 10], linestyle = '-', color = 'red')
pylab.plot ([np.mean (exp_3[3]) - np.std (exp_3[3]), np.mean (exp_3[3]) - np.std (exp_3[3])], [0, 10], linestyle = '--', color = 'red')
pylab.plot ([np.mean (exp_3[3]) + np.std (exp_3[3]), np.mean (exp_3[3]) + np.std (exp_3[3])], [0, 10], linestyle = '--', color = 'red')
#pylab.plot ([np.mean (exp_3[3]) + 2 * np.std (exp_3[3]), np.mean (exp_3[3]) + 2 * np.std (exp_3[3])], [0, 10], linestyle = ':', color = 'red')

pylab.legend ()
pylab.title ('Mean and std of trust scores (a = 0.5)')
pylab.show ()



#display sending first
pylab.hist (exp_1[0], alpha = 0.5, histtype='step', label = 'Bad guy', normed = 1, color = 'red')
pylab.hist (exp_2[0], alpha = 0.5, histtype='step', label = 'Normal guy', normed = 1, color = 'black')
pylab.hist (exp_3[0], alpha = 0.5, histtype='step', label = 'Good guy', normed = 1, color = 'brown')
pylab.hist (exp_4[0], alpha = 0.5, histtype='step', label = 'Fluctation guy', normed = 1, color = 'green')
pylab.legend ()
pylab.title ('Sending amount of different person types')
pylab.show ()

#send back
pylab.hist (exp_1[1], alpha = 0.5, histtype='step', label = 'Bad guy', normed = 1)
pylab.hist (exp_2[1], alpha = 0.5, histtype='step', label = 'Normal guy', normed = 1)
pylab.hist (exp_3[1], alpha = 0.5, histtype='step', label = 'Good guy', normed = 1)
pylab.hist (exp_4[1], alpha = 0.5, histtype='step', label = 'Fluctation guy', normed = 1)
pylab.legend ()
pylab.title ('Sending back amount of different person types')
pylab.show ()

#trust score current (2a = 0.5)
pylab.hist (exp_1[2], alpha = 0.5, histtype='bar', label = 'Bad guy', normed = 1)
pylab.hist (exp_2[2], alpha = 0.5, histtype='bar', label = 'Normal guy',normed = 1)
pylab.hist (exp_3[2], alpha = 0.5, histtype='bar', label = 'Good guy', normed = 1)
#pylab.hist (exp_4[2], alpha = 0.5, histtype='bar', label = 'Fluctation guy', normed = 1)
pylab.legend ()
pylab.title ('Trust score (2a = 0.5) of different person types')
pylab.show ()

#new trust score (a = 0.5)
pylab.hist (exp_1[3], alpha = 0.5, histtype='bar', label = 'Bad guy', normed = 1)
pylab.hist (exp_2[3], alpha = 0.5, histtype='bar', label = 'Normal guy', normed = 1)
pylab.hist (exp_3[3], alpha = 0.5, histtype='bar', label = 'Good guy', normed = 1)
#pylab.hist (exp_4[3], alpha = 0.5, histtype='bar', label = 'Fluctation guy', normed = 1)
pylab.legend ()
pylab.title ('Trust score (a = 0.5) of different person types')
pylab.show ()

# pylab.hist (sends, normed = 1)
# pylab.title ('Sending first')
# pylab.show ()
# pylab.hist (send_backs, normed = 1)
# pylab.title ('Sending back')
# pylab.show ()
# pylab.hist (trusts, normed = 1)
# pylab.title ('Trust score')
# pylab.show ()
# pylab.hist (trusts2, normed = True)
# pylab.title ('Trust score new formula')
# pylab.show ()