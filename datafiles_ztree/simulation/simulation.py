import random
import pylab

N = 1000000

trust = 0.5
trust2 = 0.5

trusts = []
trusts2 = []
sends = []
send_backs = []

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

	new_send = newSend (Type)
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

print sum (trusts) / len (trusts)
print sum (trusts2) / len (trusts2)

pylab.hist (sends, normed = 1)
pylab.title ('Sending first')
pylab.show ()
pylab.hist (send_backs, normed = 1)
pylab.title ('Sending back')
pylab.show ()
pylab.hist (trusts, normed = 1)
pylab.title ('Trust score')
pylab.show ()
pylab.hist (trusts2, normed = True)
pylab.title ('Trust score new formula')
pylab.show ()