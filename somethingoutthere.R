###################################################################################
## Is there a problem out there? A simple risk analysis
## Tutorial BayesDays Liverpool 2017
## Ullrika Sahlin
## The purpose demonstrate some basics on how to integrate expert 
## judgment and data in a probabilistic risk assessment
## Here we will have one or several experts and no data, one or two observartions. 
###################################################################################
library(rjags)
library(SHELF)
rm(list=ls()) #empty the environment
dev.off() #remove all plots

{
ms_one_expert_and_data = "
model {
  theta ~ dbeta(shape1, shape2)
  theta_prior ~ dbeta(shape1, shape2)
  X ~ dbern(theta) 
  for(j in 1:no_obs){
    y[j] ~ dbinom(alpha[j],X)
  }
}"

ms_multiple_experts = "
model {
for(i in 1:no_exp){
theta_exp[i] ~ dbeta(shape1[i], shape2[i])
}
e ~ dcat(w_exp)
theta = theta_exp[e]
}"

ms_multiple_experts_and_data = "
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
for(j in 1:no_obs){
y[j] ~ dbinom(alpha[j],X)
}
}"

}
######################
N = 10^4
w_exp = c(1, 1, 1) # weight of experts
alpha = c(0.5,0.5) # probability to detect the problem if it is there
y = c(0,0) # observations

#########################################
# uncertainty in the probability that it is there is expressed by a beta distribution
# parameters for a beta distribution from several experts
s_exp = c(2, 20, 0.1)
t_exp = c(0.5, 0.5, 0.5)
shape1 = s_exp*t_exp
shape2 = s_exp*(1-t_exp)

# elicit the cdf of a consensus distribution or your distribution by 
# specifying some points from it and let shelf find it for you
#elicit() # find your distribution and write down the parameters

# specify points from the CDF in r and find the corresponding distribution
v <- c(25,50)
p <- c(0.33, 0.66) # tertiles
unc_consensus <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
#plotfit(unc_singleexp,int = TRUE)
plotfit(unc_consensus, d = 'beta')
shape1 = unc_consensus$Beta[,'shape1']
shape2 = unc_consensus$Beta[,'shape2']

# use the roulette method to elicit the pdf of a consenus distriubtio or your own distributions
if(FALSE){
  (expert = roulette(lower = 0, upper = 100, nbins = 20, gridheight = 10))
  unc_consensus <- fitdist(vals = expert$v, probs = expert$p, lower = 0, upper = 100)
  plotfit(unc_consensus, d = 'beta')
}

# elication of the distriubtion from several several experts using the SHELF package
v <- matrix(c(60,  90,#pessimist 
              5, 35,#optimist
              33, 66)#unsure 
            , 2, 3)
p <- c(0.33, 0.66)
unc_multexp <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
plotfit(unc_multexp, lp = TRUE, d = 'beta')
shape1 = unc_multexp$Beta[,'shape1']
shape2 = unc_multexp$Beta[,'shape2']

# study cdfs of the experts' distributions
plot(c(0,1),c(0,1),xlab = expression(theta),ylab = 'cdf', type = 'n', main = 'Is there a problem out there?')
pp = (1:1000)/1000
f = function(i){qbeta(pp,shape1[i],shape2[i])}
for(i in 1:length(w_exp)){
  lines(f(i),pp,lty = 2, col = i+1)
}
pooled_experts_cdf = rowMeans(sapply(1:length(w_exp),f))
lines(pooled_experts_cdf,pp,lty = 2, col = 1, lwd = 2)
legend('topleft','pooled',lty = 2, col = 1, lwd = 2, bty = 'n')

# study pdfs of the experts' distributions
f = function(i){dbeta(pp,shape1[i],shape2[i])}
pooled_experts_pdf = rowMeans(sapply(1:length(w_exp),f))
plot(c(0,1),c(0,5),xlab = expression(theta),ylab = 'pdf', type = 'n', main = 'Is there a problem out there?')
for(i in 1:length(w_exp)){
  lines(pp,f(i),lty = 2, col = i+1)
}
lines(pp,pooled_experts_pdf,lty = 2, col = 1, lwd = 2)
legend('top','pooled',lty = 2, col = 1, lwd = 2, bty = 'n')
################################################

## We make a function to plot the cdf's
make_plot = function(prior, posterior){
  plot(c(0,1),c(0,1),xlab = expression(theta),ylab = 'cdf', type = 'n', main = 'Is there a problem out there?')
  pp = (1:1000)/1000
  lines(sort(prior),(1:length(prior))/length(prior),lty = 2)
  lines(sort(posterior),(1:length(posterior))/length(posterior))
  segments(x0 = mean(prior),y0 = 0, y1 = 0.1, lty = 2, lwd = 2)
  segments(x0 = mean(posterior), y0 = 0, y1 = 0.1, lwd = 2)
  legend('topleft',c('expert judgment','+ observations'),
         lty = c(2,1), col = c(1,1),bty = 'n')
}

#######################################
## Here follows three situations in which the analysis can be made: 
## one or several experts, none or some observations

## one expert or concensus distribution and no data
{
e = 1
theta = rbeta(N, shape1=s_exp[e]*t_exp[e], shape2=s_exp[e]*(1-t_exp[e]))
plot(density(theta,from=0,to=1))
print(paste('the probability that something is out there is', mean(theta)))
}

## multiple experts and no data
{
  data_to_model = list(shape1=s_exp*t_exp, shape2=s_exp*(1-t_exp), 
                       w_exp = w_exp/sum(w_exp), no_exp = length(w_exp))
  m = jags.model(textConnection(ms_multiple_experts), data=data_to_model, 
                 n.adapt=10^6, n.chains=3)
  sam = coda.samples(m, c('theta'), n.iter=N, thin=1)
  mat = as.matrix(sam)
  theta = mat[,grep('theta',colnames(mat))]
# plot(density(theta,from=0,to=1))
  print(paste('the probability that something is out there is', mean(theta)))
}

## one expert or concensus distribution and data
{
data_to_model = list(shape1=s_exp[e]*t_exp[e], shape2=s_exp[e]*(1-t_exp[e]), 
                     alpha=alpha, y=y, no_obs = length(y))
m = jags.model(textConnection(ms_one_expert_and_data), data=data_to_model, 
               n.adapt=10^6, n.chains=3)

sam = coda.samples(m, c('theta','theta_prior'), n.iter=N, thin=1)
mat = as.matrix(sam)
theta_prior = mat[,grep('theta_prior',colnames(mat))]
theta =  mat[,grep('theta',colnames(mat))]
#plot(density(theta_prior,from=0,to=1))
#plot(density(theta,from=0,to=1))
make_plot(theta_prior,theta)
}


## multiple experts and data
{
  data_to_model = list(shape1=s_exp*t_exp, shape2=s_exp*(1-t_exp), 
                       w_exp = w_exp/sum(w_exp), no_exp = length(w_exp),
                       alpha=alpha, y=y, no_obs = length(y))
  m = jags.model(textConnection(ms_multiple_experts_and_data), data=data_to_model, 
                 n.adapt=10^6, n.chains=3)
  sam = coda.samples(m, c('theta','theta_prior'), n.iter=N, thin=1)
  mat = as.matrix(sam)
  theta_prior = mat[,grep('theta_prior',colnames(mat))]
  theta =  mat[,grep('theta',colnames(mat))]
  #plot(density(theta_prior,from=0,to=1))
  #plot(density(theta,from=0,to=1))
  make_plot(theta_prior,theta)
}

#############################################
# What are the differences when we change the accuracy of the observations? 
# What is the influence of the expert's uncertainty on the influence from data and data quality?
y = 0
s_exp = c(2, 20, 0.1)
t_exp = c(0.5, 0.5, 0.5)
shape1 = s_exp*t_exp
shape2 = s_exp*(1-t_exp)
## one expert or concensus distribution and data
do_it = function(alpha,e){
  data_to_model = list(shape1=s_exp[e]*t_exp[e], shape2=s_exp[e]*(1-t_exp[e]), 
                       alpha=alpha, y=y, no_obs = length(y))
  m = jags.model(textConnection(ms_one_expert_and_data), data=data_to_model, 
                 n.adapt=10^6, n.chains=3)
  
  sam = coda.samples(m, c('theta','theta_prior'), n.iter=N, thin=1)
  mat = as.matrix(sam)
  theta_prior = mat[,grep('theta_prior',colnames(mat))]
  theta =  mat[,grep('theta',colnames(mat))]
  make_plot(theta_prior,theta)
  mtext(paste("data quality = ",alpha,"and expert nr", e),3)
}

# probability to detect the problem if it is there
do_it(alpha = 0.1, e = 1)
do_it(alpha = 0.9, e = 1)

do_it(alpha = 0.1, e = 2)
do_it(alpha = 0.9, e = 2)

do_it(alpha = 0.1, e = 3)
do_it(alpha = 0.9, e = 3)
