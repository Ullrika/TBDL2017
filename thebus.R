################################################################
## Tutorial BayesDays Liverpool 2017
## Ullrika Sahlin
## When should I leave the office to not miss the last bus home?
## The purpose demonstrate some basics on how to integrate expert 
## judgment and data in a probabilistic risk assessment
## In addition the assessment separate between epstemic and
## aleatory uncertainty
################################################################
library(SHELF)
library(rjags)

rm(list=ls()) #empty the environment
dev.off() #remove all plots

ms = "
model {

# probability distributions known at the beginning

# priors - for parameters that are to be updated
SP_mean_log_unc ~ dnorm( meanlog , 1/(10*meanlog) )
SP_sd_log_unc ~ dunif(0.1*sdlog,10*sdlog)
# given probability distributions for parameters not updated
B_lambda = lambda

# likelihood
# backward simulation - (this is the likelihood of the MCMC simulation)
for( i in 1 : no_obs ) {
obs[i] ~ dlnorm( SP_mean_log_unc , 1/SP_sd_log_unc ) 
}

# prediction
# forward simulation using N iterations (corresponds to a MC simulation)
for(i in 1:N){
SP[i] ~ dlnorm(SP_mean_log_unc,1/SP_sd_log_unc) #walking speed in min / km
B[i] ~ dexp(B_lambda) #time when the bus leaves in min
}
test = step(( (SP*d) + t) -B) # 1 if we miss the bus, 0 otherwise
rel_freq = mean(test) # fraction of times the bus is missed in the MC simulation

}"

###################################################
d = 1           # distance to bus in km
B_exp_time = 10 # expected time to bus in minutes
t = 0           # time you leave earlier in minutes
obs = c(10,13)  # observations of your walking speed in minutes
N = 10^4
maxt = 30
##################################################
# We will use expert judgement on walking speed
# Let us choose a lognormal distribution for speed

# Here is an expert guess on walking speed:
SP_med = 12 # average walking speed  12 min/km 
SP_cv = 0.1 # coefficient of variation in walking speed
(sdlog = sqrt(log(SP_cv^2 + 1))) # we use the fact that cv = sqrt(exp(sdlog^2) - 1)
(meanlog = log(SP_med)) # we say that your average corresponds to the median of this rather skewed distribution

data_to_model = list(meanlog = meanlog, sdlog = sdlog, 
                     obs = obs, no_obs = length(obs),
                     d = d, N = 10^3, t = t, lambda = 1/B_exp_time)
m = jags.model(textConnection(ms), data=data_to_model, 
               n.adapt=10^6, n.chains=3)

sam = coda.samples(m, c('rel_freq'), n.iter=N, thin=1)
prob_miss = as.matrix(sam)
plot(density(prob_miss))
print(paste('the probability to miss the bus is expected to be ',mean(prob_miss)))

p = 0.9 #probability for the credible interval
post_int = HPDinterval(as.mcmc(prob_miss), prob = p)
print(paste('the probability to miss the bus is with',p*100,
            '% chance expected to be between ',post_int[1,'lower'], 
            'and ',post_int[1,'upper']))


#########################################3
# Decision analysis / risk management 
# At what time should we leave to keep risk at an acceptable level?
when_to_leave = function(N, meanlog, sdlog, obs, maxt){ # function to solve the decision problem and plot

  
  data_to_model = list(meanlog = meanlog, sdlog = sdlog, 
                       obs = obs, no_obs = length(obs),
                       d = d, N = 10^3, t = 0, lambda = 1/B_exp_time)
  m = jags.model(textConnection(ms), data=data_to_model, 
                 n.adapt=10^6, n.chains=3)
  
  # run the mcmc sampling ones and store the joint posterior for SP and B
  # we want to rerun the risk assessment for different t with the same random samples
  sam = coda.samples(m, c('SP','B'), n.iter=round(N/3), thin=1)
  mat = as.matrix(sam)
  B = mat[,grep('B',colnames(mat))]
  SP = mat[,grep('SP',colnames(mat))]
  
  f = function(t, i){mean((as.numeric(SP[i,])*d - t ) > as.numeric(B[i,]))}
  safety_margin = 0:maxt #possible values on t
  g = function(i){ sapply(safety_margin,f, i)}
  prob_miss = sapply(1:dim(sam[[1]])[1],g)
  risk = rowMeans(prob_miss)
  plot(-safety_margin[(maxt:0)+1],risk[(maxt:0)+1],ylim =c(0,1),xlab = 'safety margin', ylab = 'risk')
  if(is.finite(obs[1])){text(-maxt/2, 0.5, paste(length(obs),'observations'))}
  risk_accept = 0.1
  abline(h=risk_accept,col='red')
  text(-maxt,risk_accept,'acceptable level of risk', col = 'red', pos = 4)
  mtext(paste('Leave',safety_margin[sum(risk>risk_accept)],' minutes earlier!'),3)
}

obs = c(10,1,18,5)
when_to_leave(N = N, meanlog = meanlog, sdlog = sdlog, obs = obs, maxt = maxt)

########################################################################
## Now let us elicit uncertainty in walking speed using different methods
########################################################################
# We will use a method in the SHELF package
# It ask the expert to describe points on the probability distribution

# elicit()# add your values and read the distribution on top of the graph
# you have to enter the values on the parameters from the lognormal distr manually
# meanlog = 
# sdlog =

# Enter values from the CDF and find the corresponding distribution 
v <- c(3, 12, 14)
p <- c(0.25, 0.5, 0.75)
myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
#plotfit(myfit,int = TRUE)
plotfit(myfit, d = 'lognormal')
myfit$Log.normal

# Elicit using the roulette method to describe the PDF of the distribution
if(FALSE){
(expert = roulette(lower = 0, upper = 30, nbins = 15, gridheight = 10))
myfit <- fitdist(vals = expert$v, probs = expert$p, lower = 0, upper = 100)
plotfit(myfit, d = 'lognormal')
}

# rerun the risk assessment with your new expert judgement
# study difference in results with and without integrating the observations
when_to_leave(N = N, meanlog = myfit$Log.normal$mean.log.X, sdlog = myfit$Log.normal$sd.log.X, obs = NA, maxt = maxt)
when_to_leave(N = N, meanlog = myfit$Log.normal$mean.log.X, sdlog = myfit$Log.normal$sd.log.X, obs = obs, maxt = maxt)
