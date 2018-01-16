##############************************########################
##############------JAGS+CODA -------------########################
##############************************########################

require(rjags)
require(coda)

set.seed(432104)
#the data
 n <- 4
 # y_hat <- 76
 # sig_hat <- sqrt(3.33)
 y <- rnorm(n, 76, sqrt(3.33))

#the model
model1.string <- "
  model {
    for (i in 1:N){
    y[i] ~ dnorm(m, k)
    }
  m ~ dnorm(mu,lam^(-1))
  mu <- 70 #mu_00
  lam <- 0.5 #lambda_0
  k <- 1^(-1) #pow( ,-1)
}
"

model1.spec <- textConnection(model1.string)
#test <- textConnectionValue(model1.spec)

jags <- jags.model(model1.spec,
                   data = list('y' = y,
                               'N' = n),
                   n.chains=4,
                   n.adapt=1000) 
names(jags)

update(jags, 1000) #1200

samp.jags <- jags.samples(jags, #jags model can be used to generate
                          #dependent samples from the posterior distribution of params
                          c('m'),
                          10000) #1100
names(samp.jags)
summary(samp.jags)
meanJAGS <- mean(samp.jags$m); meanJAGS

test <- matrix(samp.jags$m, nrow=10000,ncol=4)
plot(test[,1])
dim(test)


##############************************########################
##############------STAN------------########################
##############************************########################

# Set up the true parameter values
k <- 1^(-1)
yhat <- 76

# Simulate data
N <- n <- 4
y <- rnorm(n, 76, sqrt(3.33))


####Stan model 
modelStan.string <-"
data {
int N;
vector[N] y;
}
parameters {
real<lower=0> m;
//real mu;
//real lam;
//real<lower=0> k;
}

model {
//priors
  m ~ normal(70,2); 

//likelihood
  //y ~ normal(m,1); // 1 = k^(-1)
  for (i in 1:N)
  y[i] ~ normal(m, 1);
}
"
# Fit the model
library(rstan)
#fit <- stan("exponentials.stan", data=list(N=N, x=x, y=y), iter=1000, chains=4)
fit <- stan(model_code = modelStan.string, data=list(N=N,  y=y), iter=10000, 
            chains=4, verbose = FALSE)
summary(fit)
names(fit)
print(fit, pars=c("m"))
plot(fit)


##############************************########################
##############------R-INLA-------------########################
##############************************########################

install.packages("INLA", repos="https://inla.r-inla-download.org/R/stable", dep=TRUE)
library(INLA)
inla.upgrade()
?inla()

# Simulate data
N <- n <- 4
y <- rnorm(n, 76, sqrt(3.33))
m <- rnorm(n, 70, 2)
#eps <- rnorm(n, 0, 1)
d <- data.frame(cbind(y,m))

##Define the model
formula = y ~ f(m, model="iid", param=c(70,2)) 
#+ 
#  f(eps, model="iid")#model="meb"==num of hyperparams=2

result = inla(formula,family="gaussian", verbose=TRUE, data = d)
summary(result)
plot(result)



#################*************Analytic result************###################
#Due to conjugacy the posterior distribution is normal with the following mean and sd
k <- 1
n <- 4
yhat <- 76
#sighat <- sqrt(3.33)
lam <- lam_0 <- 0.5
mu <- mu_0 <- 70

meanAnalyt <- (n*k*yhat + lam*mu)/(n*k + lam); meanAnalyt


##############************************########################
###########-----Epsilon Local Sensitivity----------#################
##############************************########################
#Compute the Bhattacharyya coefficient measures the affinity of both densities
BC <- function(density1, density2) {
  #compute the integral
}

#compute the Hellinger distance using BC
HellDist <- function(density1, density2) {
  sqrt(1-BC(density1, density2))
}

CircSensit <- function(prior_0, posterior_0, prior, posterior, eps) {
  #solve HellDist(prior_0, prior) = eps for gamma
  CircSensitivity <- HellDist(posterior_0, posterior)/eps
}

#wrapper function for JAGS
#JAGS output: samp.jags - MCMC samples
#JAGS input: 1. data N, y
            #2. priors        
wrapper.jags <- function(data.jags, prior.level, mu_0, lam_0, eps){
  #jags.model(data.jags, prior.level, mu_0, lam_0) #a function that performs jags modelling and returns the samples
  #compute CircSensit(posterior_0,posterior, eps ) for JAGS model
}

#wrapper function for Stan
wrapper.stan <- function() {
  
}

#wrapper function for INLA
wrapper.inla <- function() {
  
}

#combined wrapper function
wrapper.all <- function()
{
  
}

