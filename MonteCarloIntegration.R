# Monte Carlo Integration
# The Basics :)
f_target = function(x){ # target function
  return(exp(-x))
}
N = 10000
X_uni = runif(N)
eva_uni = f_target(X_uni)
mean(eva_uni)
1-exp(-1) # 0.6321206, the theoretical answer

N_seq = c(50,100, 500,1000, 5000,10000, 5e4, 1e5, 5e5, 1e6)
mean_eva = rep(NA, length(N_seq))

for(j in 1:length(N_seq)){
  N = N_seq[j]
  X_uni = runif(N)
  eva_uni = f_target(X_uni)
  mean_eva[j] = mean(eva_uni)
}
mean_eva
1-exp(-1)
hist(mean_eva)
abline(v=1-exp(-1),col="red") # UNBIASED

plot(x=N_seq, y=mean_eva, pch=20, log="x", col="dodgerblue")
abline(h=1-exp(-1), lwd=2, col="red")
lines(x=N_seq, y=mean_eva, lwd=2, col="dodgerblue")

plot(NULL, ylim=c(0.6, 0.67), xlim=c(50, 1e6), log="x")
abline(h=1-exp(-1), lwd=2, col="red")
col_vec = seq(from=1,to=10,by=1)
for(i_rep in 1:10){
  mean_eva = rep(NA, length(N_seq))
  for(j in 1:length(N_seq)){
    N = N_seq[j]
    X_uni = runif(N)
    eva_uni = f_target(X_uni)
    mean_eva[j] = mean(eva_uni)
  }
  points(x=N_seq, y=mean_eva, pch=20, col=col_vec[i_rep])
  lines(x=N_seq, y=mean_eva, lwd=2, col=col_vec[i_rep])
}

# # change the color of curves
# col_new = colorRampPalette(c("red","skyblue","limegreen","tan","purple"))
# colorRampPalette(c(vector_of_Color_names)) is a beautiful function
# # create a color palette that mix several color
# col_new
# col_new(10)
# plot(x=1:10,y=1:10, col=col_new(10), pch=20, cex=5)
# ## So here is a fancier version
# plot(NULL, ylim=c(0.6, 0.67), xlim=c(50, 1e6), log="x")
# abline(h=1-exp(-1), lwd=4, col="gray")
# col_curve = col_new(10)
# for(i_rep in 1:10){
#   mean_eva = rep(NA, length(N_seq))
#   for(j in 1:length(N_seq)){
#     N = N_seq[j]
#     X_uni = runif(N)
#     eva_uni = f_target(X_uni)
#     mean_eva[j] = mean(eva_uni)
#   }
#   points(x=N_seq, y=mean_eva, pch=20, col=col_curve[i_rep])
#   lines(x=N_seq, y=mean_eva, lwd=2, col=col_curve[i_rep])
# }


## Comparing different sampling distributions:
N = 100
n_rep = 100000
# method 1: Uniform [0,1]
mean_eva = rep(NA, n_rep)
for(i_rep in 1:n_rep){
  X_uni = runif(N)
  eva_uni = f_target(X_uni)
  mean_eva[i_rep] = mean(eva_uni)
}
mean(mean_eva)
1-exp(-1) # unbiased
sd(mean_eva)

# method 2: Beta (2,2)
mean_eva2 = rep(NA, n_rep)
for(i_rep in 1:n_rep){
  X2 = rbeta(N, shape1=2, shape2=2)
  #eva2 = f_target(X2)/(6*X2*(1-X2))
  eva2 = f_target(X2)/dbeta(X2, shape1 = 2, shape2 = 2)
  mean_eva2[i_rep] = mean(eva2)
}
mean(mean_eva2)
1-exp(-1) # unbiased!
sd(mean_eva2) # way larger sd than previous method :(
