####### Example for Val

######## Randomized block design 

### n = number of blocks (or subjects if repeated measures)
  
n = 8

### p = number of levels of treatment A

p = 4

ns = c(8,8,8,8)
   
######### example data

a1 = c(3,2,2,3,1,3,4,6)
a2 = c(4,4,3,3,2,3,4,5)
a3 = c(4,4,3,3,4,6,5,5)
a4 = c(3,5,6,5,7,6,10,8)

dat = cbind(a1,a2,a3,a4)

########### compute sum of squares 

Y = sum(dat)^2/(n*p)

AS = sum(dat^2)

A = sum(c(sum(a1)^2,sum(a2)^2,sum(a3)^2,sum(a4)^2) / ns)

S = sum((dat[,1]+dat[,2]+dat[,3]+dat[,4])^2/p)

SSTO = AS - Y 
SSA = A - Y
SSBL = S - Y
SSRES = AS - A - S + Y 

MSTO = SSTO / (n*p-1)
MSA = SSA / (p-1)
MSBL = SSBL / (n-1)
MSRES = SSRES / ((n-1)*(p-1))

### F tests 

F_treatment = MSA / MSRES
F_block = MSBL / MSRES

###################### double check to make sure we have the right sums of squares 

######## rearrange data as in SPSS

Y_ij = c(a1,a2,a3,a4)
A = rep(1:4,each=8)
Subj = rep(1:8,4)

dat1 = as.data.frame(cbind(Subj,A,Y_ij))

######## run anova

with(dat1,summary(aov(Y_ij ~ factor(A) + factor(Subj)))) 

############################################################ Check sphericity assumption #########################################
###############################################################################################################################

################################ locally best invariant test for sphericity - V*

alpha = 0.25  

### if n>10, recommended alpha is 0.15 
### alpha is large because we are testing model assumptions

nu1 = p-1
nu2 = n 
 
### Critical V_star from table E.18
 
V_star_alpha_nu1_nu2 = 5.886

############################################################# Compute V_star 
##### compute matrix of variances and covariances 

Sigma_hat_square_1 = (sum(dat[,1]^2) - sum(dat[,1])^2/n) / (n-1)
Sigma_hat_square_2 = (sum(dat[,2]^2) - sum(dat[,2])^2/n) / (n-1)
Sigma_hat_square_3 = (sum(dat[,3]^2) - sum(dat[,3])^2/n) / (n-1)
Sigma_hat_square_4 = (sum(dat[,4]^2) - sum(dat[,4])^2/n) / (n-1)

Sigma_hat_1_2 = (sum(dat[,1]*dat[,2]) - sum(dat[,1])*sum(dat[,2])/n) / (n-1)
Sigma_hat_1_3 = (sum(dat[,1]*dat[,3]) - sum(dat[,1])*sum(dat[,3])/n) / (n-1)
Sigma_hat_1_4 = (sum(dat[,1]*dat[,4]) - sum(dat[,1])*sum(dat[,4])/n) / (n-1)
Sigma_hat_2_3 = (sum(dat[,2]*dat[,3]) - sum(dat[,2])*sum(dat[,3])/n) / (n-1)
Sigma_hat_2_4 = (sum(dat[,2]*dat[,4]) - sum(dat[,2])*sum(dat[,4])/n) / (n-1)
Sigma_hat_3_4 = (sum(dat[,3]*dat[,4]) - sum(dat[,3])*sum(dat[,4])/n) / (n-1)

Sigma_hat = rbind(c(Sigma_hat_square_1,Sigma_hat_1_2,Sigma_hat_1_3,Sigma_hat_1_4),
				  c(Sigma_hat_1_2,Sigma_hat_square_2,Sigma_hat_2_3,Sigma_hat_2_4),
				  c(Sigma_hat_1_3,Sigma_hat_2_3,Sigma_hat_square_3,Sigma_hat_3_4),
				  c(Sigma_hat_1_4,Sigma_hat_2_4,Sigma_hat_3_4,Sigma_hat_square_4))

#### assemble orthonormal matrix corresponding to omnibus null hypothesis 

##### C_prime has orthogonal rows 

C_prime = rbind(c(1,-1,0,0),c(0,0,1,-1),c(0.5,0.5,-0.5,-0.5))

### mu = rbind(mu1,mu2,mu3,mu4)
### zero = rbind(0,0,0)
### H0: C_prime %*% mu = zero

##### make length of each coefficeint vector = 1

############################### compute length of each coefficient vector in C_prime 

c1 = rbind(1,-1,0,0)
c2 = rbind(0,0,1,-1)
c3 = rbind(0.5,0.5,-0.5,-0.5)

length_c1 = (t(c1) %*% c1)^(1/2)
length_c2 = (t(c2) %*% c2)^(1/2)
length_c3 = (t(c3) %*% c3)^(1/2)

############################## transform each coefficent vector to make it have length=1 

c1_norm = c1 / length_c1[1]
c2_norm = c2 / length_c2[1]
c3_norm = c3 / length_c3[1]

### double check 

length_c1_norm = (t(c1_norm) %*% c1_norm)^(1/2)
length_c2_norm = (t(c2_norm) %*% c2_norm)^(1/2)
length_c3_norm = (t(c3_norm) %*% c3_norm)^(1/2)

###### assemble C_star

C_star = cbind(c1_norm,c2_norm,c3_norm)
C_star_prime = t(C_star)

############### compute product C_star_prime * Sigma * C_star

Product = C_star_prime %*% Sigma_hat %*% C_star

############ compute V_star 

V_star = (n-1)*(p-1)/2 * (((p-1)*sum(diag(Product%*%Product))) / (sum(diag(Product)))^2 -1)

### V_star > V_star_alpha_nu1_nu2 = 5.886
### reject hypothesis that Sigma is spherical -> sphericity assumption not met -> F tests are positively biased -> adjusted F test must be used 

#################################################### Compute adjusted F test 
################################################################################

### when sphericity assumption is met, epsilon = 1 
### when sphericity assumption is not met, 1/(p-1)<= epsilon < 1 

### if F test is significant (positively biased), compute conservative F test 

epsilon_hat = 1/(p-1)
nu1 = (p-1) * epsilon_hat
nu2 = (n-1) * (p-1) * epsilon_hat

### if conservative F test is not significant, compute adjusted F test 

#### compute epsilon_hat from data 

epsilon_hat = (sum(diag(Product)))^2 / ((p-1) * sum(diag(Product%*%Product)))

nu1 = (p-1) * epsilon_hat
nu2 = (p-1) * (n-1) * epsilon_hat 

######################################### determine F critical 
alpha = 0.05

######## conventional F test when sphericity assumption is tenable 

nu1 = p-1
nu2 = (n-1)*(p-1)

F_alpha_nu1_nu2 = 3.07

######## when sphericity assumption is not tenable 
############ conservative F test 

epsilon_hat = 1/(p-1)
nu1 = (p-1) * epsilon_hat
nu2 = (n-1) * (p-1) * epsilon_hat

F_alpha_nu1_nu2 = 5.59

############ adjusted F test 

epsilon_hat = (sum(diag(Product)))^2 / ((p-1) * sum(diag(Product%*%Product)))

nu1 = (p-1) * epsilon_hat
nu2 = (p-1) * (n-1) * epsilon_hat 

F_alpha_nu1_nu2 = 3.89

############################################################ Testing mean contrasts #########################################
###############################################################################################################################

########################################################## if sphericity condition is tenable, use MSRES

###################### for a posteriori all interesting contrasts (at least one is non-pairwise) use Scheffe's F test

Y_bar = with(dat1,tapply(Y_ij,A,mean))

## for example,

c1 = rbind(1,-1/3,-1/3,-1/3)   ######## comparing the first mean agaist the mean of the other three means 

Psi_hat_1 = sum(c_1*Y_bar)
Sigma_hat_psi_1 = sqrt(MSRES * sum(c_1^2/ns))

F = Psi_hat_1^2 / Sigma_hat_psi_1^2

nu1 = p-1 
nu2 = (n-1)*(p-1)
 
#### critical value = (p-1)*F_alpha_nu1_nu2

############################################################# if sphericity condition is NOT tenable, use MSRES_i (contrast-specfic error terms)

### for example,

c1 = rbind(1,-1,0,0)
c2 = rbind(0,0,1,-1)
c3 = rbind(0.5,0.5,-0.5,-0.5)
c4 = rbind(1,-1/3,-1/3,-1/3)

Psi_hat_1 = sum(dat%*%c1)/n
Psi_hat_2 = sum(dat%*%c2)/n
Psi_hat_3 = sum(dat%*%c3)/n
Psi_hat_4 = sum(dat%*%c4)/n

SSRES_1 = (sum((dat%*%c1)^2) - sum(dat%*%c1)^2/n) / sum(c1^2)
SSRES_2 = (sum((dat%*%c2)^2) - sum(dat%*%c2)^2/n) / sum(c2^2)
SSRES_3 = (sum((dat%*%c3)^2) - sum(dat%*%c3)^2/n) / sum(c3^2)
SSRES_4 = (sum((dat%*%c4)^2) - sum(dat%*%c4)^2/n) / sum(c4^2)

MSRES_1 = SSRES_1 / (n-1)
MSRES_2 = SSRES_2 / (n-1)
MSRES_3 = SSRES_3 / (n-1)
MSRES_4 = SSRES_4 / (n-1)

Sigma_hat_psi_1 = sqrt(MSRES_1 * sum(c1^2)/n)
Sigma_hat_psi_2 = sqrt(MSRES_2 * sum(c2^2)/n)
Sigma_hat_psi_3 = sqrt(MSRES_3 * sum(c3^2)/n)
Sigma_hat_psi_4 = sqrt(MSRES_4 * sum(c4^2)/n)

####################################################################### tests for trends ###########################################################
####################################################################################################################################################

############################################################# if sphericity assumption is tenable, use MSRES as error term 

F = MS_Psi_hat_lin / MSRES
F = MS_Psi_hat_quad / MSRES

############################################################# if sphericity assumption is NOT tenable, use a MSRES appropriate for each trend  

c_lin = rbind(-3,-1,1,3)
c_quad = rbind(1,-1,-1,1)
c_cubic = rbind(-1,3,-3,1)

SS_Psi_hat_lin = sum(dat%*%c_lin)^2 / (n * sum(c_lin^2))
SS_Psi_hat_quad = sum(dat%*%c_quad)^2 / (n * sum(c_quad^2))
SS_Psi_hat_cubic = sum(dat%*%c_cubic)^2 / (n * sum(c_cubic^2))

SSRES_lin = (sum((dat%*%c_lin)^2) - sum(dat%*%c_lin)^2/n) / sum(c_lin^2)
SSRES_quad = (sum((dat%*%c_quad)^2) - sum(dat%*%c_quad)^2/n) / sum(c_quad^2)
SSRES_cubic = (sum((dat%*%c_cubic)^2) - sum(dat%*%c_cubic)^2/n) / sum(c_cubic^2)

nu1 = 1

MS_Psi_hat_lin = SS_Psi_hat_lin / nu1
MS_Psi_hat_quad = SS_Psi_hat_quad / nu1
MS_Psi_hat_cubic = SS_Psi_hat_cubic / nu1

nu2 = n-1

MSRES_lin = SSRES_lin / nu2
MSRES_quad = SSRES_quad / nu2
MSRES_cubic = SSRES_cubic / nu2

F_lin = MS_Psi_hat_lin / MSRES_lin
F_quad = MS_Psi_hat_quad / MSRES_quad
F_cubic = MS_Psi_hat_cubic / MSRES_cubic

F_alpha_nu1_nu2 = 5.59

