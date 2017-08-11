#stock: GOOG
#Option Maturity closest to six months - March 17,2017
#current stock price: 791.98
#Call Option Prices: 
  #Strike 790.00 - Price: 37.41
  #Strike 795.00 - Price: 32.40

today = as.Date("2016-11-08")
expiry = as.Date("2017-03-17")
days_till_exp = as.numeric(expiry - today,units= 'days')
#days_till_exp = 129
#days_till_exp is the T parameter or callPricer


#callPricer is a function that outputs the price of a European call option using simulation
callPricer = function (p0,K,T,r,sigma,N){
  #p0 is the initial price
  #K is the strike price
  #T is the number of days until expiry
  #r is the risk free rate
  #N is the number of simulations to run
  optionPayOffs= c(rep(0,N))
  for(i in 1:N){
    #-----------------PLEASE CHECK--------------- Assumes historical returns equals the risk free rate and I'm pretty sure this is wrong----------
    returns=rnorm(T,r,sigma)
    prices=p0*cumprod(returns+1)
    optionPayOffs[i]=max(prices[T]-K,0)
  }
  callPricer = mean(optionPayOffs)
}
#runs part 3 with the following parameters
p0 = 791.98
K = 790.00
T = days_till_exp
r = 0
N = 10000
#target_price is the market price of the option when this code was created.
target_price = 37.41
#sigma is a list of values that will be tested.
sigma = seq(.05,.6,.01)
#results is a matrix filled with sigmas and the difference between the calculated price and the market price)
results = matrix(c(rep(0,length(sigma)*2)),length(sigma),2)
for (i in 1:length(sigma)){
  #current is the price of the bond with sigma[i]
  current = callPricer(p0,K,T,r,sigma[i],N)
  results[i,1] = sigma[i]
  results[i,2] = abs(current - target_price)
}
#returns the sigma with the lowest price variance
print(results[which.min(results[,2]),1])
#Implied Vol = .52
