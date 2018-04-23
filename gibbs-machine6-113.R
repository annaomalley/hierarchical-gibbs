# summary statistics of sample
n <- 5
samplemean <- 86
samplevar <- 369.5
burnin <- 1000
runs <- 10000

mu <- rep(NA, runs+burnin)
tau <- rep(NA, runs+burnin)
tau[1] <- 1

# sample from the joint posterior
for(i in 2:(runs+burnin))
{   
  mu[i] <- rnorm(n = 1, mean = samplemean, sd = sqrt(1 / (n * tau[i - 1])))    
  tau[i] <- rgamma(n = 1, shape = n / 2, scale = 2 / ((n - 1) * samplevar + n * (mu[i] - samplemean)^2))
}

#remove burnin
mu <- mu[-(1:burnin)]
tau <- tau[-(1:burnin)] 

#posterior distribution of mean
plot(density(mu))

#posterior predictive
predruns <- 1000
mupred <- replicate(predruns,sample(mu,length(mu),replace = TRUE))
taupred <- replicate(predruns,sample(tau,length(tau),replace = TRUE))
xstar <- rep(NA,predruns)
for (i in 1:predruns)
{
  xstar[i] <- rnorm(n=1,mean=mupred[i],sd = sqrt(1/taupred[i]))
}
