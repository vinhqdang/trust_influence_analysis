#Role = 0: sender
#Role = 1: receiver
#CalculationType = 0: calculate by proportion sending
#CalculationType = 1: 2a -> 0.5
#CalculationType = 2: a -> 0.5

import math
import random
import pylab
import numpy as np

SENDER = 0
RECEIVER = 1

class Agent (object):
	def __init__ (self, person_type, calculation_type):
		self.alpha = 1.0
		self.deviationReliability = 1.0
		self.expectedTrust = 0.0
		self.recentTrust = 0.0
		self.historyTrust = 0.0
		self.beta = 0.0
		self.deviationFactor = 0.5
		self.atf = 0.0
		self.overallTrust = 0.0

		#need to adjust
		self.epsilon = 0.3

		self.person_type = person_type
		self.newSend = 0
		self.calculationType = calculation_type
		self.role = 0
		self.maxAT = 1.0

	def normalNewSend (self, round_number):
		#define role
		rand = random.uniform (0, 1)
		if (rand <= 0.5):
			self.role = 0
		else:
			self.role = 1

		new_send = 0

		mean = 5.0
		sigma = 0.5
		#bad guy
		if (self.person_type == 0):
			mean = 2
		if (self.person_type == 1):
			mean = 5
		if (self.person_type == 2):
			mean = 8
		new_send = random.gauss (mu = mean, sigma = sigma)
		if (self.person_type == 3):
			rand = random.uniform (0, 1)
			# if (rand <= 0.25):
			# 	new_send = 0
			# elif (rand <= 0.5):
			# 	new_send = 1
			# elif (rand <= 0.75):
			# 	new_send = 9
			# else:
			# 	new_send = 10
			DEVIATION_THRESHOLD_ROUND = 8
			BETREAY_ROUND = 9
			if (round_number <= DEVIATION_THRESHOLD_ROUND):
				if (rand <= 0.5):
					new_send = 9
				else:
					new_send = 10
			elif (round_number <= BETREAY_ROUND):
				if (rand <= 0.5):
					new_send = 1
				else:
					new_send = 9
			else:
				if (rand <= 0.5):
					new_send = 0
				else:
					new_send = 1

		if (new_send < 0):
			new_send = 0.0
		if (new_send > 10):
			new_send = 10.0
		self.newSend = new_send
		return new_send

	def calcRecentTrust (self, role = 0):
		proportion = float (self.newSend) / 10

		if (role == SENDER):
			if (self.calculationType == 0 or self.calculationType == 5 or self.calculationType == 6):
				self.recentTrust = float (self.newSend) / 10
		if (role == RECEIVER):
			if (self.calculationType == 0):
				self.recentTrust = float (self.newSend) / 10
			elif (self.calculationType == 5):
				if (self.newSend  <= (20.0/3)):
					self.recentTrust = (0.75) * float (self.newSend ) / 10
				else:
					self.recentTrust = (1.5) * float (self.newSend ) / 10 - 0.5
			elif (self.calculationType == 6):
				if (new_send <= 10.0/3):
					self.recentTrust = 1.5 * float (self.newSend ) / 10
				else:
					self.recentTrust = 0.75 * float (self.newSend ) / 10 + 0.25

		if (self.calculationType == 1):
			self.recentTrust = proportion ** 2

		if (self.calculationType == 2):
			#useless
			self.recentTrust = proportion ** 3	

		if (self.calculationType == 3):
			self.recentTrust = math.pow (proportion, 0.5)

		if (self.calculationType == 4):
			self.recentTrust = math.pow (proportion, 1.0/3)

		if (self.calculationType == 7):
			self.recentTrust = (math.exp(proportion) - 1) / (math.exp (1) - 1)

		if (self.calculationType == 8):
			self.recentTrust = math.log (proportion * (math.exp (1) -1) + 1)

		return self.recentTrust

	def calcHistoryTrust (self):
		lastRecentTrust = self.recentTrust
		newDelta = abs (lastRecentTrust - self.calcRecentTrust ())
		#user-defined factor to define how we react with recent error
		c = 0.9

		self.beta = c * newDelta + (1 - c) * self.beta

		threshold = 0.25
		alpha = threshold + c * newDelta / (1 + self.beta)

		self.historyTrust = alpha * self.recentTrust + (1 - alpha) * self.historyTrust
		return self.historyTrust

	def calcExpectedTrust (self):
		if (self.recentTrust == 0.0 and self.historyTrust == 0.0):
			self.expectedTrust = 0.0
		else:
			if (self.recentTrust - self.historyTrust > self.epsilon):
				self.deviationFactor += 0.1
			elif ((self.recentTrust - self.historyTrust) <- (0 - self.epsilon)):
				self.deviationFactor -= 0.1
			self.expectedTrust = self.recentTrust * self.deviationFactor + (1 - self.deviationFactor) * self.historyTrust
		return self.expectedTrust

	def calcDeviationReliability (self):
		lastATF = self.atf

		#punishment factor for sudden rise in trust
		omega = 2

		#tolerrant margin of error
		phi = 0.1

		newATF = 0.0
		if (self.recentTrust - self.historyTrust > phi):
			newATF = lastATF + (self.recentTrust - self.historyTrust) / omega
		elif (self.recentTrust - self.historyTrust < -phi):
			newATF = lastATF + self.historyTrust - self.recentTrust
		else:
			newATF = lastATF
		self.atf = newATF

		self.maxAT = 2.0
		if (self.atf > self.maxAT):
			self.deviationReliability = 0.0
		else:
			self.deviationReliability = math.cos ((math.pi / 2) * (self.atf/self.maxAT))
		return self.deviationReliability


	def calcOverallTrust (self):
		self.overallTrust = self.expectedTrust * self.deviationReliability
		return self.overallTrust

	def simulateOneRound (self, round_number):
		self.normalNewSend (round_number)
		self.calcHistoryTrust ()
		self.calcExpectedTrust ()
		self.calcDeviationReliability ()
		return self.calcOverallTrust ()

	def simulateMultipleRounds (self, numRound = 5):
		for i in range (numRound):
			self.simulateOneRound (i + 1)
		return self.overallTrust

	def getAtf (self):
		return self.atf

	def getMaxAT (self):
		return self.maxAT

def to_percent(y, position):
    # Ignore the passed in position. This has the effect of scaling the default
    # tick locations.
    s = str(100 * y)

    # The percent symbol needs escaping in latex
    if matplotlib.rcParams['text.usetex'] is True:
        return s + r'$\%$'
    else:
        return s + '%'

def test2 ():
	trusts = list ()
	goodGuy = Agent (person_type = 0, calculation_type = 4)
	for i in range (5):
		trusts.append (goodGuy.simulateOneRound())
	print trusts

def test1 ():
	for calcType in range (9):
		if (calcType != 0 and calcType != 8):
			continue
		ROUNDS = 10
		#number of users = EXPERIMENT_LEN * 2
		EXPERIMENT_LEN = 1
		N = 10000

		trusts1 = []
		trusts2 = []
		trusts3 = []
		trusts4 = []

		atf1 = []
		atf2 = []
		atf3 = []
		atf4 = []

		distinguish = []

		maxAT = 0.0

		for i in range (N):
			count = 0.0
			for j in range (EXPERIMENT_LEN):
				badGuy = Agent (person_type = 0, calculation_type = calcType)
				normalGuy = Agent (person_type = 1, calculation_type = calcType)
				goodGuy = Agent (person_type = 2, calculation_type = calcType)
				fluncGuy = Agent (person_type = 3, calculation_type = calcType)

				trusts1.append (badGuy.simulateMultipleRounds(numRound = ROUNDS))
				trusts2.append (normalGuy.simulateMultipleRounds(numRound = ROUNDS))
				trusts3.append (goodGuy.simulateMultipleRounds(numRound = ROUNDS))
				trusts4.append (fluncGuy.simulateMultipleRounds(numRound = ROUNDS))

				maxAT = normalGuy.getMaxAT ()

				if (trusts1[-1] < trusts2[-1] - 0.11 and trusts2[-1] < trusts3[-1] - 0.12):
					count += 1

			distinguish.append (count/EXPERIMENT_LEN)

				# atf1.append (badGuy.getAtf())
				# atf2.append (normalGuy.getAtf ())
				# atf3.append (goodGuy.getAtf ())
				# atf4.append (fluncGuy.getAtf ())

		# pylab.hist (distinguish, normed = 1)
		# pylab.title ('Distinguish ability, mu = ' + "{0:.2f}".format(np.mean (distinguish)) + ', trust formula: ' + str (calcType))
		# pylab.text (60, .025,'maxAT = ' + str (maxAT))
		# pylab.show ()
		# print '---'
		# print 'Mean of Distinguish ability = ' + "{0:.2f}".format(np.mean (distinguish))
		# print 'Std = ' + str (np.std (distinguish))
		# print

		#label_1 = str ('Bad guy, mu = ' + str (np.mean(trusts1) + ', sigma = ' + str (np.std (trusts1)))
		#pylab.subplot(330 + calcType + 1)
		pylab.subplot(210 +int(calcType/8))
		pylab.hist (trusts1, label = "Low profile", alpha = 0.3, color = '0.2', normed = True, range = [0,1], weights=np.ones_like(trusts1)/float(len(trusts1)))
		# error_range = np.std(trusts1) * 2
		# pylab.plot ([np.mean(trusts1) - error_range, np.mean(trusts1) - error_range],[0,5], linestyle="-", color = "k")
		# pylab.plot ([np.mean(trusts1) + error_range, np.mean(trusts1) + error_range],[0,5], linestyle="-", color = "k")

		pylab.hist (trusts2, label = "Medium profile", alpha = 0.3, color = '0.5', normed = True)
		# error_range = np.std(trusts2) * 2
		# pylab.plot ([np.mean(trusts2) - error_range, np.mean(trusts2) - error_range],[0,5], linestyle="--", color = "k")
		# pylab.plot ([np.mean(trusts2) + error_range, np.mean(trusts2) + error_range],[0,5], linestyle="--", color = "k")

		pylab.hist (trusts3, label = "High profile", alpha = 0.3, color = '0.8', normed = True)
		# error_range = np.std(trusts3) * 2
		# pylab.plot ([np.mean(trusts3) - error_range, np.mean(trusts3) - error_range],[0,5], linestyle=":", color = "k")
		# pylab.plot ([np.mean(trusts3) + error_range, np.mean(trusts3) + error_range],[0,5], linestyle=":", color = "k")

		pylab.hist (trusts4, label = "Fluctuate profile", alpha = 0.3, normed = True, hatch = "/")
		# pylab.title ('Distribution of trust score: ratio between variances is ' + "{0:.2f}".format(np.std (trusts3) ** 2 / (np.std (trusts1) ** 2)) + ', trust formula: ' + str (calcType))
		# pylab.title ("Trust function f" + str(calcType + 1))
		if (calcType == 0):
			pylab.title ("Simple trust metric as current_trust", fontsize = 35)
			pylab.xlabel ("Trust value", fontsize = 30)
		elif (calcType == 8):
			pylab.title ("Our current_trust formula", fontsize = 35)
			pylab.gca().get_xaxis().set_ticks([])
		# pylab.text (60, .025,'maxAT = ' + str (maxAT))
		# pylab.legend ()
		if (calcType == 0):
			pylab.legend()
		ax = pylab.gca()
		ax.axes.get_yaxis().set_ticks([])
		# ax.axes.get_xaxis().set_ticks([])
		
		pylab.ylabel ("Frequency", fontsize = 30)
		
	# pylab.subplot(4,3,11)
	# pylab.hist (range(9), label = "Bad guy", alpha = 0.3, color = '0.2', normed = True, range = [0,1])
	# pylab.hist (range(9), label = "Normal guy", alpha = 0.3, color = '0.5', normed = True)
	# pylab.hist (range(9), label = "Good guy", alpha = 0.3, color = '0.8', normed = True)
	# pylab.hist (range(9), label = "Fluctuate guy", alpha = 0.3, normed = True, hatch = "/")
	# pylab.legend()
	# ax = pylab.gca()
	# ax.axes.get_yaxis().set_ticks([])
	# ax.axes.get_xaxis().set_ticks([])

	print ("Show graph")
	pylab.show ()

	#plot ATF distribution
	#to find maxAT value
	# pylab.hist (atf1, label = 'Bad guy', alpha = 0.4)
	# pylab.hist (atf2, label = "Normal guy", alpha = 0.4)
	# pylab.hist (atf3, label = "Good guy", alpha = 0.4)
	# pylab.hist (atf4, label = "Flunctation guy", alpha = 0.4)
	# pylab.legend ()

	# trusts = trusts2
	# #pylab.plot (trusts)
	# print '***'
	# print np.mean (trusts1)
	# print np.std (trusts1)
	# print np.std (trusts1) ** 2
	# print '---'
	# print np.mean (trusts2)
	# print np.std (trusts2)
	# print np.std (trusts2) + np.std (trusts1)
	# print np.std (trusts2) + np.std (trusts3)
	# print '---'
	# print np.mean (trusts3)
	# print np.std (trusts3)
	# print np.std (trusts3) ** 2
	# print '---'
	# print np.mean (trusts4)
	# print np.std (trusts4) 
	# print '---'
	# print 'Ratio = ' + str (np.std (trusts3) ** 2 / (np.std (trusts1) ** 2))
	# pylab.show ()

def test2 ():
	calcType = 0

	ROUNDS = 10
	#number of users = EXPERIMENT_LEN * 2
	EXPERIMENT_LEN = 1
	N = 10000

	trusts1 = []
	trusts2 = []
	trusts3 = []
	trusts4 = []

	atf1 = []
	atf2 = []
	atf3 = []
	atf4 = []

	distinguish = []

	maxAT = 0.0

	for i in range (N):
		count = 0.0
		for j in range (EXPERIMENT_LEN):
			badGuy = Agent (person_type = 0, calculation_type = calcType)
			normalGuy = Agent (person_type = 1, calculation_type = calcType)
			goodGuy = Agent (person_type = 2, calculation_type = calcType)
			fluncGuy = Agent (person_type = 3, calculation_type = calcType)

			trusts1.append (badGuy.simulateMultipleRounds(numRound = ROUNDS))
			trusts2.append (normalGuy.simulateMultipleRounds(numRound = ROUNDS))
			trusts3.append (goodGuy.simulateMultipleRounds(numRound = ROUNDS))
			trusts4.append (fluncGuy.simulateMultipleRounds(numRound = ROUNDS))

			maxAT = normalGuy.getMaxAT ()

			if (trusts1[-1] < trusts2[-1] - 0.11 and trusts2[-1] < trusts3[-1] - 0.12):
				count += 1

		distinguish.append (count/EXPERIMENT_LEN)

	pylab.subplot(210 +int(calcType/8))
	pylab.hist (trusts1, label = "Low profile", alpha = 0.3, color = '0.2', normed = True, range = [0,1], weights=np.ones_like(trusts1)/float(len(trusts1)))

	pylab.hist (trusts2, label = "Medium profile", alpha = 0.3, color = '0.5', normed = True)

	pylab.hist (trusts3, label = "High profile", alpha = 0.3, color = '0.8', normed = True, hatch = "+")

	pylab.hist (trusts4, label = "Fluctuate profile", alpha = 0.3, normed = True, hatch = "/")
	
	ax = pylab.gca()
	# ax.axes.get_yaxis().set_ticks([])

	font_size = 30
	
	pylab.ylabel ("Frequency (%)", fontsize = font_size)
	pylab.xlabel ("Trust value", fontsize = font_size)
	pylab.legend(loc = 2, fontsize = font_size - 10)

	pylab.xticks (fontsize = font_size)
	pylab.yticks (fontsize = font_size - 10)

	print ("Show graph")
	pylab.show ()
	# pylab.savefig ("figure_1.png")

if __name__ == "__main__":
	test2 ()

