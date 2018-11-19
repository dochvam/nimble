
#--

code <- nimbleCode({
  xi[1:n] ~ dCRP(alpha, n)
  xi1[1:n] ~ dCRP(alpha, n)
  alpha ~ dgamma(1, 1)
  for(i in 1:n){
    s2[i] ~ dinvgamma(1, 1)
    mu[i] ~ dnorm(0, var=s2[i])
    y[i] ~ dnorm(mu[xi[i]],  var=s2[xi1[i]])
  }
})
n <- 82
Consts <- list(n = n)
set.seed(1)
Inits <- list(xi = sample(1:10, size=n, replace=TRUE),
              xi1 = sample(1:10, size=n, replace=TRUE),
              mu = rnorm(n, 0, sd=sqrt(10)), 
              s2 = rinvgamma(n, 1, 1), 
              alpha = 1) #conc0 = 1, c(rep(10, n/2), rep(1, n/2)); rep(5, n)
Data <- list(y = speeds)
m1 <- nimbleModel(code, data=Data, inits=Inits, constants = Consts)
cm1 <- compileNimble(m1)
mConf1 <- configureMCMC(m1, monitors = c('xi', 'mu', 's2', 'alpha'), print=TRUE)  # not work if have sapce in ''
mConf1$removeSampler('xi')
mConf1$addSampler(target='xi', type='sampler_CRP_uniques', print=TRUE)
mMCMC1 <- buildMCMC(mConf1)
cMCMC1 <- compileNimble(mMCMC1, project = m1)
t1 <- proc.time()
cMCMC1$run(1000)
proc.time() - t1

output1 <- as.matrix(cMCMC1$mvSamples)



#--

code <- nimbleCode({
  xi[1:n] ~ dCRP(alpha, n)
  alpha ~ dgamma(1, 1)
  for(i in 1:n){
    s2[i] ~ dinvgamma(1, 1)
    mu[i] ~ dnorm(0, var=s2[i])
    y[i] ~ dnorm(mu[xi[i]],  var=s2[xi[i]])
  }
})
n <- 1000
Consts <- list(n = n)
set.seed(1)
Inits <- list(xi = sample(1:10, size=n, replace=TRUE),
              mu = rnorm(n, 0, sd=sqrt(10)), 
              s2 = rinvgamma(n, 1, 1), 
              alpha = 1) #conc0 = 1, c(rep(10, n/2), rep(1, n/2)); rep(5, n)
Data <- list(y = rnorm(n))

t1 <- proc.time()
m1 <- nimbleModel(code, data=Data, inits=Inits, constants = Consts)
proc.time() - t1 # 1.950   0.037   1.994 

t1 <- proc.time()
cm1 <- compileNimble(m1)
proc.time() - t1 #6.246   0.855   7.253

t1 <- proc.time()
mConf1 <- configureMCMC(m1, monitors = c('xi', 'mu', 's2', 'alpha'), print=TRUE)  # not work if have sapce in ''
proc.time() - t1 # 175.983   1.213 178.802


#--

code <- nimbleCode({
  xi[1:n] ~ dCRP(alpha, n) # n can not be samll that length(x)
  alpha ~ dgamma(1, 1)
  for(i in 1:100){
  s2[i] ~ dinvgamma(1, 1)
  mu[i] ~ dnorm(0, var=s2[i])
  }
  for(i in 1:n){
    y[i] ~ dnorm(mu[xi[i]],  var=s2[xi[i]])
  }
})
n <- 1000
Consts <- list(n = n)
set.seed(1)
Inits <- list(xi = sample(1:10, size=n, replace=TRUE),
              mu = rnorm(100, 0, sd=sqrt(10)), 
              s2 = rinvgamma(100, 1, 1), 
              alpha = 1) #conc0 = 1, c(rep(10, n/2), rep(1, n/2)); rep(5, n)
Data <- list(y = rnorm(n))

t1 <- proc.time()
m1 <- nimbleModel(code, data=Data, inits=Inits, constants = Consts)
proc.time() - t1 # 1.133   0.039   1.180

t1 <- proc.time()
cm1 <- compileNimble(m1)
proc.time() - t1 # 6.281   1.116   7.609 

t1 <- proc.time()
mConf1 <- configureMCMC(m1, monitors = c('xi', 'mu', 's2', 'alpha'), print=TRUE)  # not work if have sapce in ''
proc.time() - t1 # 16.640   0.404  17.067 


