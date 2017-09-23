################################################################
## Tutorial BayesDays Liverpool 2017
## Ullrika Sahlin
## When should I leave the office to not miss the last bus home?
## The purpose demonstrate some basics on how to integrate expert 
## judgment and data in a probabilistic risk assessment
################################################################
library(SHELF)
library(rjags)


N = 10^4 # MC sample size
pp = (1:N)/N # vector for plotting
B_exp_time = 10 # expected time to bus  
B = rexp(N, rate = 1/B_exp_time) # random sample of size N from the Bus distribution

d = 1# distance to bus in km

###################################################
# We will use expert judgement on walking speed
# Guess your walking speed!

SP_med = 12 # average walking speed  12 min/km 
SP_cv = 0.1 # coefficient of variation in walking speed
# You choose a a lognormal distribution for your speed
sdlog = sqrt(log(SP_cv^2 + 1)) # we use the fact that cv = sqrt(exp(sdlog^2) - 1)
meanlog = log(SP_med) # we say that your average corresponds to the median of this rather skewed distribution

SP = rlnorm(N, meanlog = meanlog, sdlog = sdlog) # random sample of size N from the speed distribution

#########################################3
# Do the risk assessment
# Forward simulation
TIME = SP*d # time it takes for me to walk in minutes

par(mfrow = c(1,1))
plot(range(TIME,B),c(0,1),type='n',xlab='time',ylab='cdf')
lines(sort(TIME, decreasing = TRUE),pp)
lines(sort(B),pp, col = 'red')
legend('bottomright',c('my time','the bus'),col = c(1,2),lty = c(1,1), bty = 'n')

(p_miss = mean(TIME > B)) # the probability of me missing the bus

#########################################3
# Decision analysis / risk management 
# At what time should we leave to keep risk at an acceptable level?
when_to_leave = function(SP){ # function to solve the decision problem and plot
TIME = SP*d # time it takes for me to walk in minutes
f = function(t){mean((TIME-t) > B)}
safety_margin = 0:30
risk = sapply(safety_margin,f)
plot(safety_margin,risk,ylim =c(0,1))
risk_accept = 0.1
abline(h=risk_accept,col='red')
text(12,risk_accept,'acceptable level of risk', col = 'red', pos = 4)
mtext(paste('Leave',safety_margin[sum(risk>risk_accept)],' minutes earlier!'),3)
}

# this is how you apply the function
# draw a sample from walking speed SP
# then put it into the decision analysis
when_to_leave(SP = rlnorm(N, meanlog = meanlog, sdlog = sdlog))
SP = rlnorm(N, meanlog = meanlog, sdlog = sdlog)
hist(SP)
########################################################################
## Now let us elicit uncertainty in walking speed in a different way
########################################################################
# We will use a method in the SHELF package
# It ask the expert to describe points on the probability distribution

# elicit()# add your values and read the distribution on top of the graph
# you have to enter the values manually
# meanlog = 
# sdlog =
# when_to_leave(SP = rlnorm(N, meanlog = meanlog, sdlog = sdlog)
             

# Enter values directly 
v <- c(3, 12, 14)
p <- c(0.25, 0.5, 0.75)
myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
#plotfit(myfit,int = TRUE)
plotfit(myfit, d = 'lognormal')
myfit$Log.normal
when_to_leave(SP = rlnorm(N,meanlog=myfit$Log.normal$mean.log.X, sdlog=myfit$Log.normal$sd.log.X))

# Elicit by entering judgements using the roulette method
if(FALSE){
(expert = roulette(lower = 0, upper = 30, nbins = 15, gridheight = 10))
myfit <- fitdist(vals = expert$v, probs = expert$p, lower = 0, upper = 100)
plotfit(myfit, d = 'lognormal')
when_to_leave(SP = rlnorm(N,meanlog=myfit$Log.normal$mean.log.X, sdlog=myfit$Log.normal$sd.log.X))
}
#########################################################
## You have observations of your speed - let us use them!
#########################################################
obs = c(10,17,9)
# We will update our belief on walking speed and sample from 
# the predictive posterior (SP)

ms = "
model {
# priors
mean_log_unc ~ dnorm( meanlog , 1/(10*meanlog) )
sd_log_unc ~ dunif(0.1*sdlog,10*sdlog)

# likelihood
for( i in 1 : no_obs ) {
  obs[i] ~ dlnorm( mean_log_unc , 1/sd_log_unc ) 
}

# prediction
SP ~ dlnorm(mean_log_unc,1/sd_log_unc)
}
"

# use expert's judgments on the parameters of the walking speed
meanlog=myfit$Log.normal$mean.log.X
sdlog=myfit$Log.normal$sd.log.X

# perform MCMC sampling using JAGS
data_to_model = list(meanlog = meanlog, sdlog = sdlog, obs = obs, no_obs = length(obs))
m = jags.model(textConnection(ms), data=data_to_model, 
               n.adapt=10^6, n.chains=1)
sam = jags.samples(m, c('SP'), n.iter=N, thin=1)
when_to_leave(SP = as.numeric(sam$SP))

sam.coda = coda.samples(m, c('mean_log_unc'), n.iter=N, thin=1)

plot(sam.coda)
