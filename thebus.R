################################################################
## Tutorial BayesDays Liverpool 2017
## Ullrika Sahlin
## When should I leave the office to not miss the last bus home?
## The purpose demonstrate some basics how to integrate expert 
## judgment and data in probabilistic risk assessment
################################################################
library(SHELF)
library(rjags)


N = 10^4 # MC sample size
pp = (1:N)/N
B_exp_time = 10 # expected time to bus 
B = rexp(N, rate = 1/B_exp_time) # random sample of size N from the Bus distribution

# guess your walking speed
SP_med = 12 # avearge walking speed  12 min/km 
SP_cv = 0.1 # coefficient of variation in walking speed
# you deside to use a lognormal distribution for your speed
sdlog = sqrt(log(SP_cv^2 + 1)) # we use the fact that cv = sqrt(exp(sdlog^2) - 1)
meanlog = log(SP_med)
SP = rlnorm(N, meanlog = meanlog, sdlog = sdlog) # random sample of size N from the speed distribution

d = 1# distance to bus in km

# Forward simulation
TIME = SP*d # time it takes for me to walk in minutes

par(mfrow = c(1,1))
plot(range(TIME,B),c(0,1),type='n',xlab='time',ylab='cdf')
lines(sort(TIME, decreasing = TRUE),pp)
lines(sort(B),pp, col = 'red')
legend('bottomright',c('my time','the bus'),col = c(1,2),lty = c(1,1), bty = 'n')

(p_miss = mean(TIME > B)) # the probability of me missing the bus

# Decision analysis
# At what time should we leave to keep risk at an acceptable level?
when_to_leave = function(SP){
TIME = SP*d # time it takes for me to walk in minutes
f = function(t){mean((TIME-t) > B)}
safety_margin = 0:30
risk = sapply(time_to_leave,f)
plot(safety_margin,risk,ylim =c(0,1))
risk_accept = 0.1
abline(h=risk_accept,col='red')
text(12,risk_accept,'acceptable level of risk', col = 'red', pos = 4)
mtext(paste('Leave',safety_margin[sum(risk>risk_accept)],' minutes earlier!'),3)
}

when_to_leave(SP)

########################################################################
## Now let us elicit uncertainty in walking speed in a different way
########################################################################
# we will use the SHELF package
elicit()# add your values and read the distribution on top of the graph
SP = rlnorm(N,meanlog= , sdlog= ) # you have to enter the values manually
when_to_leave(SP)

# enter values directly 
v <- matrix(c(3, 12, 14), 3, 1)
p <- c(0.25, 0.5, 0.75)
myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
plotfit(myfit,int = TRUE)
plotfit(myfit, d = 'lognormal')
myfit$Log.normal
SP = rlnorm(N,meanlog=myfit$Log.normal$mean.log.X, sdlog=myfit$Log.normal$sd.log.X)
when_to_leave(SP)

# elicit by entering judgements using the roulette method
(expert = roulette(lower = 0, upper = 30, nbins = 15, gridheight = 10))
myfit <- fitdist(vals = expert$v, probs = expert$p, lower = 0, upper = 100)
plotfit(myfit, d = 'lognormal')
SP = rlnorm(N,meanlog=myfit$Log.normal$mean.log.X, sdlog=myfit$Log.normal$sd.log.X)
when_to_leave(SP)

#######################################################
## You have observations of your speed - let us use it
#######################################################
obs = c(10,17,9)
ms = "
model {
mean_log_unc ~ dnorm( meanlog , 1/(10*meanlog) )
sd_log_unc ~ dunif(0.001,10*sdlog)

for( i in 1 : no_obs ) {
  obs[i] ~ dlnorm( mean_log_unc , 1/sd_log_unc ) 
}

SP ~ dlnorm(mean_log_unc,1/sd_log_unc)
}
"

data_to_model = list(meanlog = meanlog, sdlog = sdlog, obs = obs, no_obs = length(obs))
m = jags.model(textConnection(ms), data=data_to_model, 
               n.adapt=10^6, n.chains=3)
sam = jags.samples(m, c('SP'), n.iter=N, thin=1)
SP = as.numeric(sam$SP)

# add the judgement by two experts
v <- matrix(c(30, 40, 50, 20, 25, 35), 3, 2)
p <- c(0.25, 0.5, 0.75)
myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
plotfit(myfit, lp = TRUE)
plotfit(myfit,int = TRUE)


sample_SP = function(i){rlnorm(N,meanlog=myfit$Log.normal$mean.log.X[i], sdlog=myfit$Log.normal$sd.log.X[i])}
no_exp = 2
w_exp = c(1,1)
m = matrix(unlist(lapply(1:no_exp,sample_SP)),ncol=no_exp,byrow = FALSE)
SP = rowSums(m*w_exp)
when_to_leave(SP)
