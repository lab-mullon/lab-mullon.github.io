################################### Exercise sheet 1 ###################################

# rm(list=ls()) # Clear all stored variables.

### Summary of parameters
# n0 = initial population size
# n_a = number of age-classes.
# Fec = vector of age-specific fecundities (from age 1 to n_a)
# P = vector of age-specific survival probabilities (from ages 1 to n_a - 1)
# p0 = establishment probability of the produced offspring.
# nt = number of time steps
# nmax = size threshold.

DYN=function(n0, n_a, Fec, P, p0, nt, nmax) # Arguments that will be taken by the function 'DYN' that we are building.
{
  #graphics.off()  # Automatically close graphics windows (if any), comment out if needed.
  
  A=rep(1,n0)  # Initial population is made of n0 individuals of age 1.
  Res=matrix(0, ncol=n_a,nrow=nt) # Initialise the results matrix
  
  D=as.data.frame( table(A) ) # Make a table of the number of individuals in each age-class.
  for( i in 1:nrow(D))
  {
    Res[1,i] = D$Freq[i] # Store these numbers in Res.
  }
  
  for(t in 2:nt)
  {
    # Reproduction
    Off=0 # Total number of offspring produced
    for( i in 1:length(A))
    {
      Off = Off + rpois( 1, Fec[ A[i] ] ) # Each individual produces offspring based on its age-specific fecundity
    }
    
    P0 = p0 # Establishment probability
    
    if(Off > 0) # If at least one offspring has been generated,
    {
      AO=rep( 1, rbinom(n=1, size=Off, prob= P0) ) # Sample the number of survivors in a binomial distribution.
    }
    else
    {
      AO = c() # Otherwise, return an empty vector.
    }

    # Survival of established individuals
    # We replace deceased individuals by 'NA' and then remove NAs from the vector
    for( i in 1:length(A))
    {
      if(A[i] < n_a) # If they are younger than n_a (otherwise they die with probability 1),
      {
        # Established individuals survive to the next time step according to their age-specific survival probability
        if( runif(1) < P[ A[i] ] )
        {
          A[i]= A[i] + 1 # If they survive, they are kept and their age increases by one.
        }
        else
        {
          A[i] = NA # Otherwise they are marked as dead.
        }
      }
      else
      {
        A[i] = NA  # Marked as dead.
      }
    }
    AS=A[!is.na(A)] # Survivors are all the individuals that are not marked with an NA.
    A=c(AO,AS) # Newly established individuals and survivors constitute the population at the next time step
    
    if( length(A) == 0) # If the population size reaches zero, we end the time-loop.
    {
      Res[t,]=rep(0, n_a)
      break 
    }
    else # Otherwise, we simply store results in Res.
    {
      D=as.data.frame( table(A) ) 
      tmp=rep(0, n_a)
      for( i in 1:nrow(D))
      {
        tmp[ D$A[i] ] = D$Freq[i]
      }
      Res[t,]=tmp
    }
    
    if( length(A) >= nmax ) # If the population size reaches the size threshold, the simulation is halted.
    {
      Res=Res[1:t,]
      break
    }

  } # End of loop over time.

  # Some cosmetic changes to the results matrix
  row.names(Res)=NULL
  colnames(Res)=c("n1","n2","n3","n4","n5","n6")
  
  return(Res) # Return the results matrix
}

#Simulation results are stored in the 'sim' object.

sim = DYN(n0= , n_a= , Fec= , P= , p0= , nt= , nmax = )
