import math 
import pylab
import scipy
import numpy as np

def calcTrust (send_amount, ROUNDS = 5):
	res = []
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

	for i in range (ROUNDS):
		curRecentTrust 	= math.log ((float (send_amount[i])) * (math.exp (1) - 1) + 1)
		curDelta		= abs (RecentTrust - curRecentTrust)
		RecentTrust 	= curRecentTrust
		Beta			= c * curDelta + (1 - c) * Beta
		Alpha			= threshold + c * curDelta / (1 + Beta)
		HistoryTrust	= Alpha * RecentTrust + (1 - Alpha) * HistoryTrust

		if (RecentTrust == 0 and HistoryTrust == 0):
			ExpectTrust = 0
		else:
			if (RecentTrust - HistoryTrust > Epsilon):
				DeviationFactor += 0.1
			elif (HistoryTrust - RecentTrust > Epsilon):
				DeviationFactor -= 0.1
			ExpectTrust = RecentTrust * DeviationFactor + HistoryTrust * (1 - DeviationFactor)

		if (RecentTrust - HistoryTrust > Phi):
			ATF += (RecentTrust - HistoryTrust) / 2
		elif (HistoryTrust - RecentTrust > Phi):
			ATF += (HistoryTrust - RecentTrust)

		if (ATF > MAX_ATF):
			DeviationReliability = 0.0
			ATF /= 2
		else:
			DeviationReliability = math.cos ((math.pi / 2) * (ATF/MAX_ATF))

		TrustScore = ExpectTrust * DeviationReliability
		#res.append ("%.2f" % TrustScore)
		res.append (TrustScore)

	return res

def test1 ():
	r = []
	#r.append (0.0)
	r.append (0.2)
	r.append (0.5)
	r.append (0.8)
	#r.append (1.0)

	i = 0
	covars = []
	for s1 in r:
		for s2 in r:
			for s3 in r:
				for s4 in r:
					for s5 in r:
						for s6 in r:
							for s7 in r:
								sends = [s1, s2, s3, s4, s5, s6, s7]
								trusts = calcTrust (sends, ROUNDS = 7)
								covar = np.corrcoef(sends, trusts)[0, 1]
								covars.append (covar)


								pylab.plot (trusts, '-', label = "Trust score")
								pylab.plot (sends,'--', label = "Send amount")
								pylab.legend ()
								pylab.title ("Correlation between send amount and trust is: " + "{0:.2f}".format(covar))
								pylab.savefig (str (int(s1*10)) + "_" + str (int(s2*10)) 
									+ "_" + str (int(s3*10)) + "_" + str (int(s4*10)) + "_" + str (int(s5*10)) + "_"
									+ str (int(s6*10)) + "_" + str (int(s7*10))
									+ ".png")
								pylab.clf ()
								print ("Processed the image number: " + str (i))
								i += 1

	mdat = np.ma.masked_array(covars,np.isnan(covars))
	print "Mean of Correlation is: " + "{0:.2f}".format(np.mean (mdat))
	print "Std of Correlation is: " + "{0:.2f}".format(np.std (mdat))

def test2 ():
	pylab.plot ([0]*10)
	pylab.plot (calcTrust ([0.2] * 7, ROUNDS = 7), linestyle = "--",label = "1")
	pylab.plot (calcTrust ([0.5] * 7, ROUNDS = 7), linestyle = ":", label = "2")
	pylab.plot (calcTrust ([0.8] * 7, ROUNDS = 7), marker = r'$\lambda$',label = "3")
	pylab.plot (calcTrust ([1, 0, 1, 0, 1, 0, 1], ROUNDS = 7), marker = r'$\bowtie$',label = "4")
	pylab.plot (calcTrust ([0, 0, 0, 0, 0.5, 0.8, 1, 0], ROUNDS = 7), marker = r'$\circlearrowleft$',label = "5")
	pylab.plot (calcTrust ([1, 1, 1, 1, 1, 0, 1, 0], ROUNDS = 7), marker = r'$\clubsuit$', label = "6")
	pylab.plot (calcTrust ([0.0, 0.1, 0.3, 0.5, 0.7, 0.9, 1.0], ROUNDS = 7), marker = r'$\checkmark$', label = "7")
	pylab.legend ()
	pylab.show ()

def test3 ():
	covars = []
	sends = [11.0/15, 12.0/15, 11.0/15, 11.5/15, 11.0/12, 0.9]
	trusts = calcTrust (sends, ROUNDS = 6)
	covar = np.corrcoef(sends, trusts)[0, 1]
	covars.append (covar)

	pylab.plot (trusts, '-', label = "Trust score")
	pylab.plot (sends,'--', label = "Send amount")
	pylab.legend ()
	pylab.title ("Correlation between send amount and trust is: " + "{0:.2f}".format(covar))
	pylab.savefig ("1.png")
	pylab.clf ()

def test4():
	pylab.plot ([0]*10)
	axes = pylab.gca()
	axes.set_ylim ([0,1])
	pylab.xlabel ('Round number')
	pylab.ylabel ('Trust score')
	x = [x / 10.0 for x in range(1,11)]
	pylab.plot (calcTrust (x, ROUNDS = 10), linestyle = "--",label = "Increasing sending proportion overtime")
	x = [x / 10.0 for x in list(reversed(range(1,11)))]
	pylab.plot (calcTrust (x, ROUNDS = 10), linestyle = ":", label = "Decreasing sending proportion overtime")
	pylab.legend ()
	pylab.show ()

test4()

# test the data of Claudia Keser (2002)
def test_claudia_keser ():
	print ("Simulating Keser experiments")

	negative_rating = [0.2797, 0.0771, 0.0719, 0.2821, 0.1704, 0.2592, 0.1366, 0.1671, 0.2168, 0.2505, 0.3391, 0.2181]
	neutral_rating = [0.5512, 0.4143, 0.3385, 0.4502, 0.4794, 0.6474, 0.4389, 0.4250, 0.6178, 0.5951, 0.4547, 0.3766]
	positive_rating = [0.6581, 0.5938, 0.4109, 0.6613, 0.5389, 0.7168, 0.6935, 0.5979, 0.6874, 0.6769, 0.5277, 0.5502]

	pylab.plot ([0]*20)
	pylab.plot (calcTrust (positive_rating, ROUNDS = 12), marker = r'$\lambda$',label = "positive user", color = '0.2')
	pylab.plot (calcTrust (neutral_rating, ROUNDS = 12), linestyle = ":", label = "neutral user", color = '0.1')
	pylab.plot (calcTrust (negative_rating, ROUNDS = 12), linestyle = "--",label = "negative user", color = '0.0')

	pylab.xlabel ("Round number", fontsize = 20)
	pylab.ylabel ("Trust score", fontsize = 20)

	x = range(0,10)
	labels = range (1,11)

	pylab.xticks (x, labels)

	pylab.legend ()
	pylab.show ()
	
# test_claudia_keser ()

def test_diff_users ():
	low_profile = [0.2] * 10
	medium_profile = [0.5] * 10
	high_profile = [0.8] * 10
	bad_profile = [0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.1,0.1]

	pylab.plot ([0]*17)

	x = range(0,10)
	labels = range (1,11)
	
	pylab.plot (calcTrust(bad_profile, ROUNDS = 10), marker = r'$\lambda$', label = "fluctuate profile", color = '0.1')
	pylab.plot (calcTrust(high_profile, ROUNDS = 10), linestyle = "--",label = "high profile", color = '0.2')
	pylab.plot (calcTrust(medium_profile, ROUNDS = 10), linestyle = ":",label = "medium profile", color = '0.3')
	pylab.plot (calcTrust(low_profile, ROUNDS = 10), linestyle = "-",label = "low profile", color = '0.4')

	pylab.xlabel ("Round number", fontsize = 20)
	pylab.ylabel ("Trust score", fontsize = 20)

	pylab.xticks (x, labels)

	pylab.legend ()
	pylab.show ()

# test_diff_users ()

def test_with_average_trust_metric ():
	low_profile = [0.2] * 10
	medium_profile = [0.5] * 10
	high_profile = [0.8] * 10
	bad_profile = [0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.811111,0.74]

	pylab.plot ([0]*17)

	x = range(0,10)
	labels = range (1,11)
	
	pylab.plot (bad_profile, marker = r'$\lambda$', label = "fluctuate profile", color = '0.1')
	pylab.plot (high_profile, linestyle = "--",label = "high profile", color = '0.2')
	pylab.plot (medium_profile, linestyle = ":",label = "medium profile", color = '0.3')
	pylab.plot (low_profile, linestyle = "-",label = "low profile", color = '0.4')

	pylab.xlabel ("Round number", fontsize = 20)
	pylab.ylabel ("Trust score", fontsize = 20)

	pylab.xticks (x, labels)

	axes = pylab.gca()
	axes.set_ylim([0,1])

	pylab.legend ()
	pylab.show ()

# test_with_average_trust_metric()