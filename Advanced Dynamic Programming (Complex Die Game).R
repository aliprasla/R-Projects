#i is player
#j is computer
#k is accumulated score
goal = 10
V = array('b',c(goal+6,goal+6,goal+6))
U = array('b',c(goal+6,goal+6,goal+6))
for (z in (goal + goal + 4):0){
  for (j in (goal+5):0){
    i = z - j
    if (i >= goal && j >=goal){
      next()
    }
    if (i >= goal + 6){
      next()
    }
    for (k in (goal+5):0){
      if (j >= goal){
        V[i+1,j+1,k+1] = 0
        U[i+1,j+1,k+1] = 2
      }
      else if ((i + k) >= goal){
        V[i+1,j+1,k+1] = 1
        U[i+1,j+1,k+1] = 2

      }
      #if this is the first roll
      else if (k == 0){
        sum = 0
        #NAs are being coerced here
        for (roll in 2:6){
          sum = sum + (as.numeric(V[i+1,j+1,roll+1])/6)
        }
        V[i+1,j+1,k+1] = (1/6)*(1-as.numeric(V[j+1,i+1,k+2])) + sum
        U[i+1,j+1,k+1] = 1
      }
      else{
        print(cbind('i = ',i+1, 'j = ', j+1, 'k = ',k+1))
        sum = 0
        for (roll in 6:2){
          sum = sum + (1/6)*as.numeric(V[i+1,j+1,(k+roll)+1])
        }
        #problem here with indexing. Can't use index of zero because of NA
        a = as.numeric(V[j+2,(i+k)+1,1])
        
        b = as.numeric(V[j+2,i+1,1])
        val = (max(1 - a,((1-b) + sum)/6))
        V[i+1,j+1,k+1] = val
        U[i+1,j+1,k+1] = (which.max(c((1 - a),((1-b) + sum)/6)))
      }
    }
  }
}

         