#run lp function:
#str = "min" or "max"
#--------------------begin LP-----------------------
{
#c = obj function
#a = transpose of contraints - vertical
#b =other side of less thans
#dir = repeated constraint vectors
library("lpSolve")
lpsol = function(str,c,a,dir,b,is_ip =FALSE){
  if (is_ip){
    mid = lp(str,c,a,dir,b,compute.sens = 1,all.int = TRUE)
  }else{
    mid = lp(str,c,a,dir,b,compute.sens = 1)
  }
  sens = cbind(mid$duals,mid$duals.from,mid$duals.to)
  ed = mid
  colnames(sens) = c("duals","duals.from","duals.to")
  if (is_ip){
    print("This is an IP")
  }else{
    print("This is an LP")
  }
  print(paste("The solution is "))
  print(mid$solution)
  print(paste("With an objective value of ", mid$objval))
  if (is_ip != TRUE){
  print("The duals matrix is :")
  print(sens)
  min_max = cbind(mid$sens.coef.from,mid$sens.coef.to)
  print("The objective sensitivity matrix")
  colnames(min_max) = c("min.c","minimal ob")
  #max.c is the minimal objective that it will be included in the solution
  print(min_max)
  }
}
#farmer's planting problem - sensitvity. duals means that's where the structure of the solution changes
# {
# c =c(3000,2000)
# a = matrix(c(1,2,4,1,3,2),3,2)
# b = c(450,1000,1200)
# dir = rep("<=",3)
# lpsol("max",c,a,dir,b)
# b= c(450,1000,665)
# lpsol("max",c,a,dir,b)
# b = c(450,1000,1601)
# lpsol("max",c,a,dir,b)
# }
#capital budgeting problem - lesson - ask Kumar about whether or not you can invest multiple times
# {
# c = c(13,16,16,14,39)
# a = matrix(c(11,3,53,6,5,5,5,1,29,34),2,5)
# a = rbind(a,diag(1,5))
# b = c(40,20,rep(1,5))
# dir = rep("<=",2+5)
# lpsol("max",c,a,dir,b)
# }
}

# ------------------begin IP------------------------
{
#warehouse factory problem
# {
# a = matrix(c(6,3,5,2),1,4)
# a = rbind(a,diag(1,4))
# a = rbind(a,c(0,0,1,1))
# a = rbind(a,c(-1,-1,0,0))
# b = c(11,rep(1,5),-1)
# c = c(3,2,1,2)
# dir = rep("<=",7)
# lpsol("max",c,a,dir,b,TRUE)
# }


#employee scheduling problem - involves carrying over employees
# c = c(330,300,330,360,360,360,360)
# a = matrix(0,7,7)
# a[1,] = c(1,0,0,1,1,1,1)
# a[2,] = c(1,1,0,0,1,1,1)
# a[3,] = c(1,1,1,0,0,1,1)
# a[4,] = c(1,1,1,1,0,0,1)
# a[5,] = c(1,1,1,1,1,0,0)
# a[6,] = c(0,1,1,1,1,1,0)
# a[7,] = c(0,0,1,1,1,1,1)
# b = c(5,13,12,10,14,8,6)
# dir = rep(">=",7)
# lpsol("min",c,a,dir,b,TRUE)


#airport hub problem - minimize the number of hubs so everything is covered
# a = matrix(0,12,12)
# a[1,] = c(1,0,1,0,1,0,1,1,1,0,0,0)
# a[2,] = c(0,1,0,0,0,0,0,1,1,0,0,0)
# a[3,] = c(1,0,1,0,0,0,1,1,1,0,0,0)
# a[4,] = c(0,0,0,1,0,0,0,0,0,1,0,0)
# a[5,] = c(1,0,0,0,1,0,1,0,0,0,0,0)
# a[6,] = c(0,0,0,0,0,1,0,0,0,1,1,0)
# a[7,] = c(1,0,1,0,1,0,1,0,0,0,0,0)
# a[8,] = c(1,1,1,0,0,0,0,1,1,0,0,0)
# a[9,] = c(1,1,1,0,0,0,0,1,1,0,0,0)
# a[10,] = c(0,0,0,1,0,1,0,0,0,1,1,1)
# a[11,] = c(0,0,0,0,0,1,0,0,0,1,1,1)
# a[12,] = c(0,0,0,0,0,0,0,0,0,1,1,1)
# c = c(rep(1,12))
# b = c(rep(1,12))
# dir = rep(">=",12)
# lpsol("min",c,a,dir,b,TRUE)
# 
}
#------------------begin NLP------------------------ Unless this is a basic portfolio optimization problem, skip.
{
#portfolio optimization
{
# - Why do models become Non-Linear - Non-constant returns to scale, goodness of fit with sum of the squared differences
    #Global vs. Local min/max.
    #function is convex if the slope is always nondecreasing - derivative is 0+ (like a cup)
# - optim is a general purpose maximizer
# - QP - solve.QP(Dmat,dvec,Amat,bvec,mew)
m = c(.1073,.0737,.0627)
s = c(.1667,.1055,.0340)
rho = matrix(c(1,.2199,.0366,.2199,1,-.0545,.0366,-.0545,1),3,3,byrow = TRUE)
Rvals = seq(.065,.1,.005)
StdDev = rep(0,length(Rvals))
portoptim = function(meanvec,stdvec,rho,targetReturn){
  library(quadprog)
  if (length(meanvec) != length(stdvec)){
    return ("Error! Invalid Mean vector or Standard deviation vector")
  }
  cov = diag(s) %*% rho %*% diag(s)
  #use matrix manipulation to create covariance matrix
  Dmat = 2*cov
  dvec = rep(0,length(meanvec))
  Amat = matrix(c(rep(1,length(meanvec)),rep(-1,length(meanvec)),meanvec),length(meanvec))
  bvec = c(1,-1,targetReturn)
  #no shorting
  Amat = cbind(Amat,diag(length(meanvec)))
  bvec = c(bvec,rep(0,length(meanvec)))
  S = solve.QP(Dmat,dvec,Amat,bvec)
  print(paste("With a target return of ",targetReturn," the optimal portfolio weights are:"))
  print(S$solution)
  print(paste("This resulted in a portfolio standard deviation of ",sqrt(S$value)))
}
# portoptim(m,s,rho,0)
}

#NOTE - OPTIM ONLY MINIMIZES
#maximize area of a triangle only happens when the sum of the sides are at their max point
{
negArea = function(x){
  #where x is a two vector solution
  a = x[1]
  b = x[2]
  return(-sqrt(30*(30-a)*(30-b)*(a + b - 30)))
}
sol = optim(c(15,20),negArea ,method = "CG")
}

#labor/capital machine production problem
{
machines = function(K){
  #100,000 available
  #what amount of labor will maximize your resource use
  L = (100000- 15*K)/12
  return(-.05*L^(2/3)*K^(1/3))
}
#lower = lowest possible input value(K in this case)
#upper = highest possible input value
# sol = optim(10,machines,method = "L-BFGS-B",lower = 0, upper = 100000/12)
# print(c((100000- 15 * sol$par)/12,sol$par))
}
}
#---------------begin simulation--------------------
{
#calculates the probablity of objcount of things happening
binsim = function(num_trials,odds_true,num_col,objcount){
  a = runif(num_col*num_trials,0,1)
  #transforms a into a true and false matrix
  a = a > odds_true
  #changes a in a matrix of trues and falses- each row indicates a new trial
  a = matrix(a,num_trials,num_col)
  #changes a into a vector of count of true values 
  a = rowSums(a)
  return(mean(a>objcount))
}

#joe drives a bus with 8 seats, 10% on average do not show up. What is the prob that >8 show up
# out = binsim(10000,.1,10,8)
# print(out)

#this is the probability that more than 8 people show up
# #adjust simulation to find the best number of tickets to sell-
# #adjusting problem - 20 seats available, on average 10% of people don't show
# #rebooking charge - $30 dollars per seat
# #profit per seat filled = $10
{
# 
# 
# #maximize profit
# possiblesales = seq(20,50,1)
# max_payoff = 0
# how_many = 0
# for (k in 1:length(possiblesales)){
# num_trials = 10000
# no_show_prob = .1
# num_sold = possiblesales[k]
# price = 25
# overflow_price = 30
# num_available = 20
# sim = matrix(runif(num_trials*num_sold,0,1)>no_show_prob,num_trials,num_sold)
# payoffs = c(rep(0,num_trials))
# sim = rowSums(sim)
# for (i in 1:num_trials){
#   rev = num_sold * price
#   overflow = 0
#   if (sim[i]>num_available){
#     overflow = overflow_price * (sim[i] - num_available)
#   }
#   payoffs[i] = rev - overflow
# }
# if (mean(payoffs)> max_payoff){
#   max_payoff = mean(payoffs)
#   how_many = possiblesales[k]
# }
# 
# }
}
  #simplebrownianforecaster
{
simplebrownianforecaster = function(meanR,std,num_period,start_price,num_trials = 1){
  out = c(rep(0,num_trials))
  for (i in 1:num_trials){
  returns = meanR + std * rnorm(num_period,meanR,std)
  prices = start_price * cumprod(1+returns)
  out[i] = prices[num_period]
  }
  return(mean(out))
}
}
  #european option pricer with brownian forecasting ----UNDISCOUNTED----
  euroOption = function(currentPrice,K,periodsToMat,meanDailyReturn,std,put = FALSE){
    payoff = c(rep(0,100))
    for (i in (1:length(payoff))){
    final_price = simplebrownianforecaster(meanDailyReturn,std,periodsToMat,currentPrice,100)
    if (put){
      payoff[i] = max(K - final_price,0)
    }else{
      payoff[i] = max(final_price - K,0)
    }
    }
    return(mean(payoff))
  }
  # opt = euroOption(100,120,100,.004,.02)
  # print(opt)

# sim sample
{
# sim = sample(c(3,4,5,6,7,8),prob = c(.05,.10,.25,.3,.2,.1))
# profits = c(rep(0,length(sim)))
# for (i in 1:length(sim)){
#   profits[i] = (4.8 * min(6,sim[i])) - (6*3) + (1.2*(min(sim[i]-6,0)))
# }
# print(mean(profits))
}
}
