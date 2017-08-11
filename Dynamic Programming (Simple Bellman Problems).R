#problem 2 - question
#no discounting
T = 6
operating = c(600,1000,1600,2400,3200,4400)
resale = c(14000,12000,8000,6000,4000,2000)
for (T in 3:T){
  #uncertain
  tValues = seq(0,T)
  tN = length(tValues)
  V = matrix(NA,tN)
  U = matrix(NA,tN)
  for (ti in seq(tN,1,-1)){
      t = tValues[ti];
      if (ti == tN){
        V[ti] = 0
        U[ti] = 0
      }
      else{
        V[ti] = max(c(resale[ti],-operating[ti] + V[ti+1]))
        U[ti] = which.max(c(resale[ti],-operating[ti] + V[ti+1]))
      }
    }
  }
#problem 3
#where you are at is the row, where you want to go is the column
travel_costs = matrix(c(0,50,20,50,0,70,20,70,0),3,3)
rev = c(120,160,170)
T = 3
for (T in 3:T){
  sValues = seq(1,3)
  tValues = seq(0,T)
  
  sN=length(sValues)
  tN=length(tValues)
  
  V=matrix(NA,sN,tN)
  U=matrix(NA,sN,tN)
  
  for (ti in seq(tN,1,-1)){
    for (si in 1:sN){
      t=tValues[ti];
      s=sValues[si];
      #if end of time condition
      if(ti==tN){
        V[si,ti]= -travel_costs[si,1]
        U[si,ti]= 1
      }
      else{
        if (ti ==1){
          if(si == 2){
            V[si,ti]=max(c(rev[1]-travel_costs[si,1]+ V[1,ti+1],rev[2] - travel_costs[si,2]+ V[2,ti+1],rev[3]- travel_costs[si,3]+ V[3,ti+1]))
            U[si,ti]=which.max(c(rev[1]-travel_costs[si,1]+ V[1,ti+1],rev[2] - travel_costs[si,2]+ V[2,ti+1],rev[3]- travel_costs[si,3]+ V[3,ti+1]))
          }
        }
        else{
        V[si,ti]=max(c(rev[1]-travel_costs[si,1]+ V[1,ti+1],rev[2] - travel_costs[si,2]+ V[2,ti+1],rev[3]- travel_costs[si,3]+ V[3,ti+1]))
        U[si,ti]=which.max(c(rev[1]-travel_costs[si,1]+ V[1,ti+1],rev[2] - travel_costs[si,2]+ V[2,ti+1],rev[3]- travel_costs[si,3]+ V[3,ti+1]))
        }
      } #if      
    } #for si
  } #for ti
  
} #for T


