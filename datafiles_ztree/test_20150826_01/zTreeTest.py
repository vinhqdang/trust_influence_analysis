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
		if (send_amount[i] < 0):
			continue
		curRecentTrust 	= math.log ((float (send_amount[i])) * (math.exp (1) - 1) + 1)
		curDelta		= abs (RecentTrust - curRecentTrust)
		RecentTrust 	= curRecentTrust
		Beta			= c * curDelta + (1 - c) * Beta
		Alpha			= threshold + c * curDelta / (1 + Beta)
		HistoryTrust	= Alpha * RecentTrust + (1 - Alpha) * HistoryTrust

		#print RecentTrust
		#print Alpha

		if (RecentTrust == 0 and HistoryTrust == 0):
			ExpectTrust = 0
		else:
			if (RecentTrust - HistoryTrust > Epsilon):
				DeviationFactor += 0.1
			elif (HistoryTrust - RecentTrust > Epsilon):
				DeviationFactor -= 0.1
			ExpectTrust = RecentTrust * DeviationFactor + HistoryTrust * (1 - DeviationFactor)

			#print ExpectTrust
			#print DeviationFactor
			#print HistoryTrust

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
	#sends = [11.0/15, 12.0/15, 11.0/15, 11.5/15, 11.0/12, 0.9]
	sends = [2.0/3, 2.0/3, 0.3, 8.0/9, 11.0/24, 0.8, 11.0/24]
	trusts = calcTrust (sends, ROUNDS = len (sends))
	covar = np.corrcoef(sends, trusts)[0, 1]
	covars.append (covar)

	pylab.plot (trusts, '-', label = "Trust score")
	pylab.plot (sends,'--', label = "Send amount")
	pylab.legend ()
	pylab.title ("Correlation between send amount and trust is: " + "{0:.2f}".format(covar))
	pylab.savefig ("1.png")
	pylab.clf ()

#test3 ()


# test data collected from zTree
def testZTreeData (directory):
	import os
	for file in os.listdir(directory):
	    if file.endswith(".txt"):
	    	f = open (file)
	    	lines = f.readlines ()
	    	f.close ()
	    	trusts = []
	    	sends = []
	    	for i in range (len (lines) -  1):
	    		line = lines [i + 1]
	    		# skip the first trust score: always 0.5
	    		if (i > 0):
	    			trusts.append (float (line.split()[1]))
	    		sends.append (float (line.split()[2]))
	    	py_trusts = calcTrust (sends, ROUNDS = len (sends))

	    	print "--------"
	    	print file
	    	print "Sending	Python_trusts	zTree_trusts	Difference"
	    	for i in range (len (trusts)):
	    		print "{0:.2f}".format(sends[i]) + "\t" + str(py_trusts[i]) + "\t" + "{0:.2f}".format(trusts[i]) + "\t" + "{0:.2f}".format(py_trusts[i] - trusts[i])

testZTreeData (".")
