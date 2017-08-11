#homework 5
#problem 1
{
game_series = function (prob){
  sims = matrix(runif(70000),10000,7) > prob
  game_decided_before_sev = c(rep(0,10000))
  for (i in 1:nrow(sims))
  {
    if (sum (sims[i,1:ncol(sims)- 1]) == 3){
      game_decided_before_sev[i] = 0
    }
    else{
      game_decided_before_sev[i] = 1
    }
  }
  return(mean(game_decided_before_sev))
}
pVals = seq(.01,.99,.01)
probs_out = rep(0,length(pVals))
for (i in 1:length(pVals)){
  probs_out[i]= game_series(pVals[i])
}
plot(pVals,probs_out,'l')
}
#problem 2
{ 
ticket_price = 10
num_seats = 40
pay_back = 25
num_sell = seq(30,100,1)
max_profit = 0
optim_seat_sell = 0
for (i in 1:length(num_sell)){
  rev = num_sell[i] * ticket_price
  #sims is a vector of 10000 simulations of how many people show up.
  num_sims = num_sell[i]*10000
  sims = rowSums(matrix(runif(num_sims),10000,num_sell[i],byrow = TRUE) > .1)
  profit = rep(0,length(sims))
  for (k in 1:length(sims)){
    costs = 0
    if (sims[k] > num_seats){
      costs = (sims[k] -num_seats) * pay_back
    }
    profit[k] = rev - costs
  }
  if(mean(profit) > max_profit){
    max_profit = mean(profit)
    optim_seat_sell = num_sell[i]
  }
}
print(optim_seat_sell)
#Joe should sell 44 seats to maximize his expected revenue
}
#problem 3
{
ann_g_cap = 1.05
ann_loss_cap = 1 + -.3
returns = matrix(rnorm(100000,1.05,1.1),10000,10)
for (i in 1:nrow(returns)){
  for (k in 1:ncol(returns)){
  if (returns[i,k] > ann_g_cap){
    returns[i,k] = ann_g_cap
  }
  else if (returns[i,k] < ann_loss_cap){
    returns[i,k] = ann_loss_cap
  }
  }
}
#a. the annual return over 10 years
cum_returns = rep(0,nrow(returns))
for (i in 1:length(cum_returns)){
  cum_returns [i] = prod(returns[i,1:ncol(returns)])
}
print(mean(cum_returns)-1)
#the mean cumulative return is -66.29% The bounds of the cap 
#b. I would much rather prefer the fixed 1% CD. The problem here is 
#that caps are restricting my upside.
}