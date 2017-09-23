###################################################################################
## Is there a problem out there? A simple risk analysis
## Tutorial BayesDays Liverpool 2017
## Ullrika Sahlin
## The purpose demonstrate some basics on how to integrate expert 
## judgment and data in a probabilistic risk assessment
## Here we will have one or several experts and no data, one or two observartions. 
###################################################################################
library(rjags)

# parameters for data and other things
N = 10^4 # MCMC sample size 
alpha = 0.5 # probability to detect the problem if it is there in the first observation
alpha2 = 0.5 # probability to detect the problem if it is there in the second observation
Y = 0 # first observation
Y2 = 0 # second observation

# no evidence, expert judgement only
# uncertainty in the probability that it is there is expressed by a beta distribution
s = 1
t = 0.5

# we will use this as a function later on, but first we go through it
do_it = function(N,t,s,alpha,alpha2,Y,Y2)
  {
  
theta_expert = rbeta(N,s*t, s*(1-t))
mean(theta_expert)
#hist(theta_expert)

# study the prior of the probability that there is a problem out there
if(FALSE){
plot(c(0,1),c(0,1),xlab = expression(theta),ylab = 'cdf', type = 'n', main = 'Is there a problem out there?')
lines(sort(theta_expert),(1:length(theta_expert))/length(theta_expert),lty = 2)
# add the expected value
segments(x0 = mean(theta_expert),y0 = 0, y1=0.1,lty = 2)
}

########################################
## Let us add one observation
########################################
# jags model to do MCMC sampling
ms1 = " 
  model {
    #prior
    theta ~ dbeta(shape1, shape2) 
    #system
    X ~ dbern(theta) 
    #observation
    Y ~ dbinom(alpha,X)
    #sample from the prior without updating 
    theta_prior ~ dbeta(shape1, shape2) 
    
  }"
  
# create a data object, including observations, fixed 
# parameters and hyper parameters
data_to_model = list(shape1 = s*t, shape2 = s*(1-t), alpha=alpha, Y=Y)
# initiate the sampling 
m = jags.model(textConnection(ms1), data=data_to_model, 
               n.adapt=10^6, n.chains=3)
# sample from the posterior
coda.sam = coda.samples(m, c('theta'), n.iter=N, thin=1)
# study the sample
#traceplot(coda.sam)
#densplot(coda.sam)
#plot(coda.sam)
summary(coda.sam)
#gelman.plot(coda.sam)
#gelman.diag(coda.sam)

# alterative way to sample from the jagsmodel generating a slightly different type of output
sam = jags.samples(m, c('theta'), n.iter=N, thin=1)
theta_expert_onedata = as.numeric(array(sam$theta)) # stores the posterior sample 
summary(theta_expert_onedata)
mean(theta_expert_onedata)

# compare prior and posterior of "the probability that there is a problem out there"
if(FALSE){
plot(c(0,1),c(0,1),xlab = expression(theta),ylab = 'cdf', type = 'n', main = 'Is there a problem out there?')
lines(sort(theta_expert),(1:length(theta_expert))/length(theta_expert),lty = 2)
lines(sort(theta_expert_onedata),(1:length(theta_expert_onedata))/length(theta_expert_onedata))
# add the expected values
segments(x0 = mean(theta_expert),y0 = 0, y1 = 0.1,lty = 2)
segments(x0 = mean(theta_expert_onedata),y0 = 0, y1 = 0.1)
}

########################################
## Let us add a second observation
########################################
ms2 = "
model {
  #prior
  theta ~ dbeta(shape1, shape2)
  #system
  X ~ dbern(theta)  
  #observation
  Y1 ~ dbinom(alpha1,X)
  Y2 ~ dbinom(alpha2,X)
}"

data_to_model = list(shape1 = s*t, shape2 = s*(1-t), alpha1=alpha, Y1=Y, alpha2=alpha2, Y2=Y2)
m = jags.model(textConnection(ms2), data=data_to_model, n.adapt=10^6, n.chains=3)
sam = coda.samples(m, c('theta'), n.iter=N, thin=1)
#plot(sam)
summary(sam)

# generate samples using jags.samples
sam = jags.samples(m, c('theta'), n.iter=N, thin=1)
theta_expert_twodata = as.numeric(array(sam$theta))
summary(theta_expert_twodata)

# compare prior and posterior of "the probability that there is a problem out there"
plot(c(0,1),c(0,1),xlab = expression(theta),ylab = 'cdf', type = 'n', main = 'Is there a problem out there?')
lines(sort(theta_expert),(1:length(theta_expert))/length(theta_expert),lty = 2)
lines(sort(theta_expert_onedata),(1:length(theta_expert_onedata))/length(theta_expert_onedata))
lines(sort(theta_expert_twodata),(1:length(theta_expert_twodata))/length(theta_expert_twodata),col = 'blue')

# add the expected values
segments(x0 = mean(theta_expert),y0 = 0, y1= 0.1,lty = 2)
segments(x0 = mean(theta_expert_onedata),y0 = 0, y1 = 0.1)
segments(x0 = mean(theta_expert_twodata),y0 = 0, y1 = 0.1, col = 'blue')
legend('topleft',c('expert','+1 obs','+2 obs'),lty = c(2,1,1), col = c(1,1,4), bty = 'n')

mtext(paste0('t=',t,', s=',s,', obs=',Y,',',Y2,', obs quality=',alpha,',',alpha2))
} #end of function

## go back to line 24 and dehasch it so you can create a function for what we just did. 

## Let us run this function for different choices of priors and observation errors
dev.off()
do_it(N=10^4,t = 0.5, s = 1, alpha = 0.5, alpha2 = 0.9, Y = 0, Y2 = 0)
do_it(N=10^4,t = 0.5, s = 20, alpha = 0.5, alpha2 = 0.9, Y = 0, Y2 = 0)
do_it(N=10^4,t = 0.5, s = 0.1, alpha = 0.5, alpha2 = 0.9, Y = 0, Y2 = 0)

do_it(N=10^4,t = 0.5, s = 1, alpha = 0.5, alpha2 = 0.9, Y = 0, Y2 = 0)
do_it(N=10^4,t = 0.5, s = 1, alpha = 0.3, alpha2 = 0.3, Y = 0, Y2 = 0)
do_it(N=10^4,t = 0.5, s = 1, alpha = 0.9, alpha2 = 0.9, Y = 0, Y2 = 0)

################################################################
## Let us see how to consider multiple experts
###############################################################

# one observation and beta distr from multiple experts
# judgements from three experts: 
# "I dont know", "It is about 50%", "Either it is there or not there"
s_exp = c(2, 20, 0.1)
t_exp = c(0.5, 0.5, 0.5)
w_exp = c(1, 1, 1) # we assign equal weights to the experts

# study cdfs of the experts' distributions
plot(c(0,1),c(0,1),xlab = expression(theta),ylab = 'cdf', type = 'n', main = 'Is there a problem out there?')
pp = (1:1000)/1000
for(i in 1:length(w_exp)){
  lines(qbeta(pp,s_exp[i]*t_exp[i],s_exp[i]*(1 - t_exp[i])),pp,lty = 2, col = i)
}
f = function(i){qbeta(pp,s_exp[i]*t_exp[i],s_exp[i]*(1 - t_exp[i]))}
pooled_experts_cdf = rowMeans(matrix(unlist(lapply(1:length(w_exp),f)),ncol = length(w_exp),byrow = FALSE))
lines(pooled_experts_cdf,pp,lty = 2, col = 1, lwd = 2)

# study pdfs of the experts' distributions
f = function(i){dbeta(pp,s_exp[i]*t_exp[i],s_exp[i]*(1 - t_exp[i]))}
pooled_experts_pdf = rowMeans(matrix(unlist(lapply(1:length(w_exp),f)),ncol = length(w_exp),byrow = FALSE))
plot(c(0,1),c(0,5),xlab = expression(theta),ylab = 'pdf', type = 'n', main = 'Is there a problem out there?')
for(i in 1:length(w_exp)){
  lines(pp,dbeta(pp,s_exp[i]*t_exp[i],s_exp[i]*(1 - t_exp[i])),lty = 2, col = i)
}
lines(pp,pooled_experts_pdf,lty = 2, col = 1, lwd = 2)

## Let us create a sample from the pooled experts using jags (one can do it directly in R as well) 
ms_multexp = "
  model {
# prior for each expert
for(i in 1:no_exp){
theta_exp[i] ~ dbeta(shape1[i], shape2[i])
}
# pooled prior
e ~ dcat(w_exp)
theta = theta_exp[e]
}"

data_to_model = list(shape1=s_exp*t_exp, shape2=s_exp*(1-t_exp), 
                     w_exp = w_exp/sum(w_exp), no_exp = length(w_exp))
m = jags.model(textConnection(ms_multexp), data=data_to_model, 
               n.adapt=10^6, n.chains=3)
sam = jags.samples(m, c('theta'), n.iter=N, thin=1)
theta_multexp = as.numeric(array(sam$theta))
summary(theta_multexp)
plot(density(theta_multexp, from = 0, to = 1))
######

ms1_multexp = "
  model {
for(i in 1:no_exp){
theta_exp[i] ~ dbeta(shape1[i], shape2[i])
}
e ~ dcat(w_exp)
theta = theta_exp[e]

for(i in 1:no_exp){
theta_exp_2[i] ~ dbeta(shape1[i], shape2[i])
}
theta_prior = theta_exp_2[e]

X ~ dbern(theta) 
Y ~ dbinom(alpha,X)
}"
  
data_to_model = list(shape1=s_exp*t_exp, shape2=s_exp*(1-t_exp), 
                     w_exp = w_exp/sum(w_exp), no_exp = length(w_exp),
                     alpha=alpha, Y=Y)
m = jags.model(textConnection(ms1_multexp), data=data_to_model, 
               n.adapt=10^6, n.chains=3)
sam = jags.samples(m, c('theta','theta_prior'), n.iter=N, thin=1)
theta_multexp_onedata = as.numeric(array(sam$theta))
theta_multexp = as.numeric(array(sam$theta_prior))
plot(density(theta_multexp,from=0,to=1))
plot(density(theta_multexp_onedata,from=0,to=1))

## We make a function to plot the cdf's
make_plot = function(prior, posterior, alpha){
plot(c(0,1),c(0,1),xlab = expression(theta),ylab = 'pdf', type = 'n', main = 'Is there a problem out there?')
pp = (1:1000)/1000
lines(sort(prior),(1:length(prior))/length(prior),lty = 2)
lines(sort(posterior),(1:length(posterior))/length(posterior))
segments(x0 = mean(prior),y0 = 0, y1 = 0.1, lty = 2)
segments(x0 = mean(posterior), y0 = 0, y1 = 0.1)
legend('topleft',c('experts','+1 obs'),lty = c(2,1), col = c(1,1),bty = 'n')
mtext(paste0('obs=',Y,',obs quality=',alpha))
}

make_plot(prior=theta_multexp, posterior = theta_multexp_onedata, alpha = data_to_model$alpha)

###################################################################
## Now we are going to use the SHELF package to get the parameters
## for the experts uncertainty
###################################################################

# one expert
# elicit() # find your distribution and write down the parameters

# put in values and find a corresponding distribution
v <- c(25,50)
p <- c(0.33, 0.66) # tertiles
unc_singleexp <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
#plotfit(unc_singleexp,int = TRUE)
plotfit(unc_singleexp, d = 'beta')

# use the roulette method
if(FALSE){
(expert = roulette(lower = 0, upper = 100, nbins = 20, gridheight = 10))
unc_singleexp <- fitdist(vals = expert$v, probs = expert$p, lower = 0, upper = 100)
plotfit(unc_singleexp, d = 'beta')
}

assess = function(w_exp, myfit, alpha){
if(length(w_exp)==1){
  data_to_model = list(shape1=myfit$Beta$shape1, shape2=myfit$Beta$shape2, 
                       alpha=alpha, Y=Y)
  m = jags.model(textConnection(ms1), data=data_to_model, 
                 n.adapt=10^6, n.chains=3)
 }else{
  data_to_model = list(shape1=myfit$Beta$shape1, shape2=myfit$Beta$shape2, 
                     w_exp = w_exp/sum(w_exp), no_exp = length(w_exp),
                     alpha=alpha, Y=Y)
  m = jags.model(textConnection(ms1_multexp), data=data_to_model, 
               n.adapt=10^6, n.chains=3)
}
  sam = jags.samples(m, c('theta','theta_prior'), n.iter=N, thin=1)
  posterior = as.numeric(array(sam$theta))
  prior = as.numeric(array(sam$theta_prior))
  make_plot(prior, posterior, alpha)
}

assess(w_exp=1, myfit = unc_singleexp, alpha = 0.9)
assess(w_exp=1, myfit = unc_singleexp, alpha = 0.2)
# Why is there a difference when we change the accuracy of the observation? 

#########################################
# several experts
v <- matrix(c(60,  90,#pessimist 
              5, 35,#optimist
              33, 66)#unsure 
            , 2, 3)
p <- c(0.33, 0.66)
unc_multexp <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
plotfit(unc_multexp, lp = TRUE, d = 'beta')

dev.off() # clear all plots
assess(w_exp = c(1,1,1), myfit = unc_multexp, alpha = 0.9)
# you can study what happens if the experts get different weights
assess(w_exp = c(1,1,0.2), myfit = unc_multexp, alpha = 0.9)
assess(w_exp = c(1,0,0.2), myfit = unc_multexp, alpha = 0.9)

