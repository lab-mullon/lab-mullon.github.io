################################### Exercise sheet 6 ###################################

#rm(list=ls()) # Clear all stored variables.

### Summary of parameters
TF = 3000
Npop = 500

ratio.P = 0.5 # Proportion of high env area
prob.A = 0.5 #Probability of sucess in a risk-taking foraging

fitness = function (resource, env){
  if( env == "high")  return(3*log(1 + resource))
  else return( 0.25*(exp(resource) - 1) )
}

resource = function(xH, xL, env){
  if( env == "low")  ifelse(rbinom(1, 1, xL), rbinom(1,1,prob.A)/prob.A, 1)
  else ifelse(rbinom(1, 1, xH), rbinom(1,1,prob.A)/prob.A, 1)
}

mutation = function(p){
  pp = p + rnorm(1, 0, 0.01) # Adds a mutation
  if(pp > 1) return(1)
  if(pp < 0) return(0)
  else return(pp)
}
  

xH = rep(0.5, Npop) # Probability of risk-proneness in high condition enviroments
xL = rep(0.5, Npop) # Probability of risk-proneness in low condition enviroments
env = sample(c("high", "low"), Npop, prob=c(ratio.P, 1-ratio.P), replace = T) #Enviroment of each individual 


# Vectors to save statistics
gens    = c(0)
xL.mean = c(mean(xL))
xH.mean = c(mean(xH))

for (time in seq(1,TF)) {
  
  # Calculates the number of resources a individual gets
  r.i = mapply(resource, xH, xL, env)
  
  # Calculates the fitness of each individual 
  W.i = mapply(fitness, r.i, env)

  # Sample individuals to replicate
  new.idx = sample.int(Npop, Npop, prob=W.i, replace=T) 
  
  xH = sapply(xH[new.idx], mutation)
  xL = sapply(xL[new.idx], mutation)
  
  # Enviroment of each individual
  new.env = sample(c("high", "low"), Npop, prob=c(ratio.P, 1-ratio.P), replace = T)  
  env = new.env
  
  # Measures at each time step
  gens    = append(gens, time )
  xL.mean = append(xL.mean, mean(xL) )
  xH.mean = append(xH.mean, mean(xH) )
}

plot(gens, xL.mean, 'l', col ="red", ylim=c(-0.01,1.01), lwd =2, 
     ylab="Probability of taking the risk", xlab="Generations")
points(gens, xH.mean, 'l', col ="blue", lwd =2)
curve(1+0*x,col ="gray", lty="dashed", lwd =2, add = T)
curve(0+0*x,col ="gray", lty="dashed", lwd =2, add = T)


