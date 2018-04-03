# Monte Carlo Simulation
N = 1000
sd_seq = c(0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3) # find out the optimao SD
sim_sd = rep(NA, length(sd_seq))
sim_avg = rep(NA, length(sd_seq))
for(i_sd in 1:length(sd_seq)){
  n_rep = 10000
  mean_eva = rep(NA, n_rep)
  for(i_rep in 1:n_rep){
    X= rnorm(N, sd =sd_seq[i_sd])
    eva = f_target(X)/dnorm(X, sd=sd_seq[i_sd])
    mean_eva[i_rep] = mean(eva)
  }
  sim_sd[i_sd] = sd(mean_eva)
  sim_avg[i_sd] = mean(mean_eva)
}
sim_sd
plot(x=sd_seq, y=sim_sd, xlab="SD of Normal", 
     ylab="Error of integration", type="b", lwd=3, col="brown") # shows the optimal SD

## power of a test
fn_power = function(mu, n){
  return(1-pnorm(qnorm(0.95)-sqrt(n)*mu)+pnorm(-qnorm(0.95)-sqrt(n)*mu))
}
n=16
mu0 = 0.5 # start with a fixed \mu
dat =rnorm(n, mean = mu0)
T_stats = sqrt(n)*(abs(mean(dat))-0)/1
T_stats > qnorm(0.95)

## use MC to check the power: repeat many many many many may many times
N = 10000
H0_reject = rep(NA, N)
for(i in 1:N){
  dat =rnorm(n=16, mean = mu0)
  T_stats = sqrt(n)*(abs(mean(dat))-0)/1
  H0_reject[i] = T_stats > qnorm(0.95)
}
H0_reject
mean(H0_reject) # an estimate of the number of H0 being rejected
fn_power(mu0, n) # the answer
sd(H0_reject)/sqrt(N) # error of Monte Carlow Simulation

## power curve
mu_seq = seq(from=-2, to=+2, by=0.05)
plot(x=mu_seq, y= fn_power(mu_seq,n=16), type="l",lwd=3,
     col="blue", ylab="Power", xlab="mu")
abline(h=0.1) # the actual power curve


## using MC Simulation to estimate the power curve
N = 10000
sim_power = rep(NA, length(mu_seq))
for(i_sim in 1:length(mu_seq)){
  H0_reject = rep(NA, N)
  mu0 = mu_seq[i_sim]
  for(i in 1:N){
    dat =rnorm(n=16, mean = mu0)
    T_stats = sqrt(n)*(abs(mean(dat))-0)/1
    H0_reject[i] = T_stats > qnorm(0.95)
  }
  sim_power[i_sim] = mean(H0_reject)
}
plot(x=mu_seq, y= sim_power, type="l",lwd=3,
     col="red", ylab="Power", xlab="mu", main=paste("N =", N))
lines(x=mu_seq, y=fn_power(mu_seq, n=16), lwd=3, col="skyblue")
legend("bottomleft", c("Simulated","True"), col=c("red","blue"), 
       lwd=6)
N=25
paste("N =", N)

plot(x=mu_seq, y= sim_power, type="l",lwd=3,
     col=rgb(1,0,0,0.7), ylab="Power", xlab="mu", main=paste("N =", N))
lines(x=mu_seq, y=fn_power(mu_seq, n=16), lwd=3, col=rgb(0,0,1,0.7))
legend("bottomleft", c("Simulated","True"), 
       col=c(rgb(1,0,0,0.9),rgb(0,0,1,0.9)), lwd=6)