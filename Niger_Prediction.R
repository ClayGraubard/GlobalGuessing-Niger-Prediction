set.seed(103031)

## Bayesian calculation if clears 50+1 based on 20,000 simulations

Outcome <- function(mu, tau, bias_sd, avg, sd){
  sigma = sqrt(sd^2 + bias_sd^2) 
  B = ((sigma^2) / (sigma^2 + tau^2)) 
  posterior_mean = (B*mu + (1-B)*avg) 
  posterior_se = (sqrt( 1 / ((1/sigma^2) + (1/tau^2))))  
  NPP <- replicate(20000, {
    results_beta = (rnorm(1, posterior_mean, posterior_se)) 
    AA = (ifelse(results_beta > 0.50, 1, 0)) 
    mean(AA) 
    
  })
  answer <- ifelse(NPP > 0, 1, 0) 
  answer 
}

## SCENARIO 1: Round One

mu <- 0.485029 # Historical Mean
tau <- 0.0300 # Historical SE
bias_sd <- 0 # Historical Bias SD
avg <- 0.469525 # Current Mean
sd <- 0.025 # Current SD

## SCENARIO 2: Round Two

mu2 <- 0.5505852074
tau2 <- 0.0450
bias_sd2 <- 0 
avg2 <- 0.5337879816
sd2 <- 0.04

## Run Simulations

Results <- Outcome(mu, tau, bias_sd, avg, sd)
ResultsR2 <- Outcome(mu2, tau2, bias_sd2, avg2, sd2)

## Sort victory simulations

b <- c(1:20000) 
Results <- data.frame(b, Results)
ResultsR2 <- data.frame(b, ResultsR2)

Win_Results <- sum(Results$Results > 0)
Win_ResultsR2 <- sum(ResultsR2$ResultsR2 > 0)

## calculate win percent

win_percent <- (Win_Results / 20000)
win_percentR2 <- (Win_ResultsR2 / 20000)

win_percent
win_percentR2
