######################################################
###########     CrypoLexicon  VAR     ################
######################################################

rm(list = ls())

setwd("/Users/alexd/Desktop/Masterarbeit/Sector Reg/Original/")

require(vars)
require(rugarch)


data    = read.csv("Sector_Sentiment.csv",sep = ",", header = TRUE)
T       = nrow(data)
t1      = 675  #start from Jan. 2016
data    = data[t1:T,]
sector  = data[,2:11]
sent    = data$sentimentMA7     #moving average sentiment to iron out the outliear and for no news day
FF      = data[,14:19]
#sent[which(is.na(sent))]=0


IntData=cbind(FF,sent)
cor(IntData)

IntData=cbind(sector[4:7],sent)
cor(IntData)

IntData=cbind(abs(sector[4:7]),sent)
cor(IntData)


FF=as.matrix(FF)
#absolute values of sector[,4] due to volatility?
summary(lm(abs(sector[,4]) ~ sent + sector[,4] + FF ))
#summary(lm(abs(sector[2:T,3]) ~ sent[1:(T-1)]+ FF[1:(T-1),] ))


######################################################################################
# GARCH model  (external.regressors = sentiment )



sentInput = as.matrix(sent)
spec1     = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1), external.regressors = sentInput), 
                   mean.model=list(armaOrder=c(1,0), include.mean=TRUE),  
                   distribution.model="std")


#Finance
fit1=ugarchfit(data=sector[,4],spec=spec1)
show(fit1)

# *---------------------------------*
#   *          GARCH Model Fit        *
#   *---------------------------------*
#   
#   Conditional Variance Dynamics 	
# -----------------------------------
#   GARCH Model	: sGARCH(1,1)
# Mean Model	: ARFIMA(1,0,0)
# Distribution	: std 
# 
# Optimal Parameters
# ------------------------------------
#   Estimate  Std. Error  t value Pr(>|t|)
# mu      0.000620    0.000282   2.1939 0.028243
# ar1    -0.048031    0.037141  -1.2932 0.195937
# omega   0.000010    0.000001  12.9388 0.000000
# alpha1  0.191088    0.048171   3.9669 0.000073
# beta1   0.715559    0.048013  14.9035 0.000000
# vxreg1  0.000015    0.000001  14.3463 0.000000      
# shape   4.236192    0.648860   6.5287 0.000000      
# 
# Robust Standard Errors:
#   Estimate  Std. Error  t value Pr(>|t|)
# mu      0.000620    0.000273   2.2717 0.023103
# ar1    -0.048031    0.039662  -1.2110 0.225888
# omega   0.000010    0.000001  12.4441 0.000000
# alpha1  0.191088    0.043461   4.3968 0.000011
# beta1   0.715559    0.037404  19.1307 0.000000
# vxreg1  0.000015    0.000004   3.7307 0.000191     (coefficent of sentiment)
# shape   4.236192    0.560270   7.5610 0.000000     (degree of freedom in t distribution)
# 
# LogLikelihood : 2544.091 
# 
# Information Criteria
# ------------------------------------
#   
#   Akaike       -6.3826
# Bayes        -6.3414
# Shibata      -6.3828
# Hannan-Quinn -6.3668

#Information Technology
fit2=ugarchfit(data=sector[,7],spec=spec1)
show(fit2)


# *---------------------------------*
#   *          GARCH Model Fit        *
#   *---------------------------------*
#   
#   Conditional Variance Dynamics 	
# -----------------------------------
#   GARCH Model	: sGARCH(1,1)
# Mean Model	: ARFIMA(1,0,0)
# Distribution	: std 
# 
# Optimal Parameters
# ------------------------------------
#   Estimate  Std. Error   t value Pr(>|t|)
# mu      0.001452    0.000225  6.449528 0.000000
# ar1    -0.068125    0.035071 -1.942520 0.052074
# omega   0.000000    0.000002  0.000449 0.999642
# alpha1  0.171163    0.004089 41.855390 0.000000
# beta1   0.832371    0.010244 81.256454 0.000000
# vxreg1  0.000011    0.000001 13.383169 0.000000
# shape   3.740426    0.443866  8.426925 0.000000
# 
# Robust Standard Errors:
#   Estimate  Std. Error   t value Pr(>|t|)
# mu      0.001452    0.000300  4.833545 0.000001
# ar1    -0.068125    0.034654 -1.965876 0.049313
# omega   0.000000    0.000003  0.000279 0.999778
# alpha1  0.171163    0.027561  6.210249 0.000000
# beta1   0.832371    0.022075 37.707093 0.000000
# vxreg1  0.000011    0.000002  4.422192 0.000010   (coefficent of sentiment)
# shape   3.740426    0.433791  8.622643 0.000000   (degree of freedom in t distribution)
# 
# LogLikelihood : 2588.113 
# 
# Information Criteria
# ------------------------------------
#   
#   Akaike       -6.4934
# Bayes        -6.4522
# Shibata      -6.4935
# Hannan-Quinn -6.4775


#Communication
fit3=ugarchfit(data=sector[,1],spec=spec1)
show(fit3)

#  *---------------------------------*
#  *          GARCH Model Fit        *
#  *---------------------------------*
  
#  Conditional Variance Dynamics 	
#-----------------------------------
#  GARCH Model	: sGARCH(1,1)
#Mean Model	: ARFIMA(1,0,0)
#Distribution	: std 

#Optimal Parameters
#------------------------------------
#  Estimate  Std. Error  t value Pr(>|t|)
#mu      0.000364    0.000345  1.05526 0.291306
#ar1    -0.007401    0.041786 -0.17713 0.859407
#omega   0.000002    0.000002  0.76919 0.441780
#alpha1  0.042950    0.008898  4.82700 0.000001
#beta1   0.937166    0.016074 58.30366 0.000000
#vxreg1  0.000002    0.000000 13.98538 0.000000
#shape   4.519531    0.714575  6.32478 0.000000

#Robust Standard Errors:
#  Estimate  Std. Error   t value Pr(>|t|)
#mu      0.000364    0.000611  0.595349 0.551610
#ar1    -0.007401    0.104050 -0.071134 0.943291
#omega   0.000002    0.000008  0.227983 0.819660
#alpha1  0.042950    0.019490  2.203699 0.027546
#beta1   0.937166    0.041719 22.463847 0.000000
#vxreg1  0.000002    0.000001  3.489156 0.000485 (coefficent of sentiment)
#shape   4.519531    0.862758  5.238469 0.000000

#LogLikelihood : 2515.102 

#Information Criteria
#------------------------------------
  
#  Akaike       -6.3097
#Bayes        -6.2685
#Shibata      -6.3098
#Hannan-Quinn -6.2939


#Consumer_Dis
fit1=ugarchfit(data=sector[,2],spec=spec1)
show(fit1)

#*---------------------------------*
#  *          GARCH Model Fit        *
#  *---------------------------------*
  
#  Conditional Variance Dynamics 	
#-----------------------------------
#  GARCH Model	: sGARCH(1,1)
#Mean Model	: ARFIMA(1,0,0)
#Distribution	: std 

#Optimal Parameters
#------------------------------------
#  Estimate  Std. Error  t value Pr(>|t|)
#mu      0.000828    0.000222  3.73156  0.00019
#ar1    -0.017099    0.035726 -0.47861  0.63221
#omega   0.000000    0.000002  0.20818  0.83508
#alpha1  0.142484    0.021642  6.58355  0.00000
#beta1   0.826064    0.029773 27.74573  0.00000
#vxreg1  0.000008    0.000000 29.90062  0.00000
#shape   5.534487    0.929413  5.95482  0.00000

#Robust Standard Errors:
#  Estimate  Std. Error   t value Pr(>|t|)
#mu      0.000828    0.000232  3.567396 0.000361
#ar1    -0.017099    0.038994 -0.438501 0.661023
#omega   0.000000    0.000005  0.079963 0.936267
#alpha1  0.142484    0.031415  4.535547 0.000006
#beta1   0.826064    0.046453 17.782965 0.000000
#vxreg1  0.000008    0.000001  7.281190 0.000000 (coefficent of sentiment)
#shape   5.534487    0.908186  6.093999 0.000000

#LogLikelihood : 2741.714 

#Information Criteria
#------------------------------------
  
#  Akaike       -6.8798
#Bayes        -6.8386
#Shibata      -6.8799
#Hannan-Quinn -6.8640


#Energy
fit1=ugarchfit(data=sector[,3],spec=spec1)
show(fit1)

#*---------------------------------*
#  *          GARCH Model Fit        *
#  *---------------------------------*
  
#  Conditional Variance Dynamics 	
#-----------------------------------
#  GARCH Model	: sGARCH(1,1)
#Mean Model	: ARFIMA(1,0,0)
#Distribution	: std 

#Optimal Parameters
#------------------------------------
#  Estimate  Std. Error   t value Pr(>|t|)
#mu      0.000389    0.000355  1.095775 0.273177
#ar1    -0.029884    0.036338 -0.822403 0.410848
#omega   0.000004    0.000003  1.155709 0.247800
#alpha1  0.090260    0.027481  3.284436 0.001022
#beta1   0.887779    0.037328 23.782957 0.000000
#vxreg1  0.000000    0.000000  0.005066 0.995958
#shape   6.473773    1.496881  4.324840 0.000015

#Robust Standard Errors:
#  Estimate  Std. Error   t value Pr(>|t|)
#mu      0.000389    0.000377  1.031611 0.302254
#ar1    -0.029884    0.035875 -0.833021 0.404833
#omega   0.000004    0.000009  0.451157 0.651876
#alpha1  0.090260    0.068751  1.312853 0.189232
#beta1   0.887779    0.089155  9.957660 0.000000
#vxreg1  0.000000    0.000001  0.002318 0.998151 (coefficent of sentiment)
#shape   6.473773    1.526109  4.242011 0.000022

#LogLikelihood : 2410.397 

#Information Criteria
#------------------------------------
  
#  Akaike       -6.0463
#Bayes        -6.0051
#Shibata      -6.0464
#Hannan-Quinn -6.0305


#Healthcare
fit1=ugarchfit(data=sector[,5],spec=spec1)
show(fit1)

#  *---------------------------------*
#  *          GARCH Model Fit        *
#  *---------------------------------*
  
#  Conditional Variance Dynamics 	
#-----------------------------------
#  GARCH Model	: sGARCH(1,1)
#Mean Model	: ARFIMA(1,0,0)
#Distribution	: std 

#Optimal Parameters
#------------------------------------
#  Estimate  Std. Error   t value Pr(>|t|)
#mu      0.000707    0.000232  3.050129 0.002287
#ar1    -0.029025    0.035929 -0.807836 0.419185
#omega   0.000002    0.000002  1.020963 0.307272
#alpha1  0.104631    0.003491 29.972011 0.000000
#beta1   0.878076    0.014608 60.110897 0.000000
#vxreg1  0.000000    0.000000  0.037373 0.970188
#shape   5.898837    1.162945  5.072326 0.000000

#Robust Standard Errors:
#  Estimate  Std. Error   t value Pr(>|t|)
#mu      0.000707    0.000250  2.830527 0.004647
#ar1    -0.029025    0.033062 -0.877884 0.380007
#omega   0.000002    0.000005  0.333159 0.739015
#alpha1  0.104631    0.006084 17.197456 0.000000
#beta1   0.878076    0.026949 32.582539 0.000000
#vxreg1  0.000000    0.000000  0.003998 0.996810 (coefficent of sentiment)
#shape   5.898837    1.119665  5.268396 0.000000

#LogLikelihood : 2717.435 

#Information Criteria
#------------------------------------
  
#  Akaike       -6.8187
#Bayes        -6.7775
#Shibata      -6.8189
#Hannan-Quinn -6.8029


#Industrials
fit1=ugarchfit(data=sector[,6],spec=spec1)
show(fit1)

#*---------------------------------*
#  *          GARCH Model Fit        *
#  *---------------------------------*
  
#  Conditional Variance Dynamics 	
#-----------------------------------
#  GARCH Model	: sGARCH(1,1)
#Mean Model	: ARFIMA(1,0,0)
#Distribution	: std 

#Optimal Parameters
#------------------------------------
#  Estimate  Std. Error  t value Pr(>|t|)
#mu      0.000838    0.000232  3.60976 0.000306
#ar1    -0.023155    0.036472 -0.63487 0.525513
#omega   0.000002    0.000001  1.63522 0.102003
#alpha1  0.138671    0.015741  8.80951 0.000000
#beta1   0.837366    0.021653 38.67272 0.000000
#vxreg1  0.000002    0.000000 13.75556 0.000000
#shape   4.551238    0.650327  6.99838 0.000000

#Robust Standard Errors:
#  Estimate  Std. Error  t value Pr(>|t|)
#mu      0.000838    0.000244  3.43524 0.000592
#ar1    -0.023155    0.039698 -0.58328 0.559704
#omega   0.000002    0.000002  0.93274 0.350956
#alpha1  0.138671    0.020939  6.62256 0.000000
#beta1   0.837366    0.012901 64.90877 0.000000
#vxreg1  0.000002    0.000001  2.90452 0.003678 (coefficent of sentiment)
#shape   4.551238    0.594009  7.66190 0.000000

#LogLikelihood : 2700.841 

#Information Criteria
#------------------------------------
  
#  Akaike       -6.7770
#Bayes        -6.7358
#Shibata      -6.7771
#Hannan-Quinn -6.7611


#Consumer_st
fit1=ugarchfit(data=sector[,8],spec=spec1)
show(fit1)

#*---------------------------------*
#  *          GARCH Model Fit        *
#  *---------------------------------*
  
#  Conditional Variance Dynamics 	
#-----------------------------------
#  GARCH Model	: sGARCH(1,1)
#Mean Model	: ARFIMA(1,0,0)
#Distribution	: std 

#Optimal Parameters
#------------------------------------
#  Estimate  Std. Error   t value Pr(>|t|)
#mu      0.000354    0.000201  1.764437 0.077658
#ar1    -0.023628    0.034487 -0.685132 0.493260
#omega   0.000003    0.000002  1.138528 0.254900
#alpha1  0.135234    0.030928  4.372588 0.000012
#beta1   0.817668    0.045330 18.038115 0.000000
#vxreg1  0.000000    0.000000  0.004483 0.996423
#shape   6.252584    1.300376  4.808291 0.000002

#Robust Standard Errors:
#  Estimate  Std. Error   t value Pr(>|t|)
#mu      0.000354    0.000210  1.684363 0.092112
#ar1    -0.023628    0.046109 -0.512434 0.608347
#omega   0.000003    0.000009  0.302067 0.762601
#alpha1  0.135234    0.062736  2.155595 0.031115
#beta1   0.817668    0.127420  6.417103 0.000000
#vxreg1  0.000000    0.000000  0.002776 0.997785 (coefficent of sentiment)
#shape   6.252584    1.318752  4.741288 0.000002

#LogLikelihood : 2846.725 

#Information Criteria
#------------------------------------
  
#Akaike       -7.1440
#Bayes        -7.1028
#Shibata      -7.1441
#Hannan-Quinn -7.1281


#Real Estate
fit1=ugarchfit(data=sector[,10],spec=spec1)
show(fit1)

#  *---------------------------------*
#  *          GARCH Model Fit        *
#  *---------------------------------*
  
#  Conditional Variance Dynamics 	
#-----------------------------------
#  GARCH Model	: sGARCH(1,1)
#Mean Model	: ARFIMA(1,0,0)
#Distribution	: std 

#Optimal Parameters
#------------------------------------
#  Estimate  Std. Error   t value Pr(>|t|)
#mu      0.000520    0.000253  2.059676 0.039430
#ar1     0.016246    0.035884  0.452748 0.650730
#omega   0.000003    0.000001  2.876623 0.004020
#alpha1  0.114918    0.028003  4.103839 0.000041
#beta1   0.845897    0.024162 35.009457 0.000000
#vxreg1  0.000000    0.000000  0.000185 0.999853
#shape   7.299550    1.786634  4.085644 0.000044

#Robust Standard Errors:
#  Estimate  Std. Error   t value Pr(>|t|)
#mu      0.000520    0.000352  1.477754 0.139474
#ar1     0.016246    0.039068  0.415846 0.677522
#omega   0.000003    0.000002  1.724171 0.084677
#alpha1  0.114918    0.028889  3.977893 0.000070
#beta1   0.845897    0.018774 45.056269 0.000000
#vxreg1  0.000000    0.000001  0.000057 0.999954 (coefficent of sentiment)
#shape   7.299550    1.665194  4.383604 0.000012

#LogLikelihood : 2674.309 

#Information Criteria
#------------------------------------
  
#  Akaike       -6.7102
#Bayes        -6.6690
#Shibata      -6.7104
#Hannan-Quinn -6.6944
