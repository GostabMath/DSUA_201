file.name = 'MAR test'
n = 1000
mx = 3
mim = 0
sigma = 1
dt <- data.table(X= sample(0:2, n,replace = TRUE))
dt[,Y_X:= rnorm(n,X,sigma)]
dt[, Prob_obs:= expit(4-4*X)]
dt[, obs:= runif(n,0,1) <= Prob_obs ]
dt[,EY_X:= X]

grd_1 <- ggplot(data = dt,aes(x = X, y = Y_X,color = obs))+ theme(panel.background = element_blank())+geom_point(size = 1) + xlab("covariate")+ ylab("E(Y|X)")+geom_jitter(width = 0.1,size =0.7)+ stat_smooth(method = "glm", formula = "y ~ x")

grd_2 <- ggplot(data =dt, aes(x=X, fill = obs)) + geom_bar(position = 'stack')

grd_1
ggMarginal(grd_1,type = "histogram",groupColour = TRUE)

grd_ob <- ggplot(data = dt[dt$obs=="TRUE",],aes(x = X, y = Y_X,color = obs))+ theme(panel.background = element_blank())+geom_point(size = 1) + xlab("covariate")+ ylab("E(Y|X)")+geom_jitter(width = 0.1,size =0.7)+ stat_smooth(method = "glm", formula = "y ~ x")

grd_ob
ggMarginal(grd_ob,type = 'histogram')

#Write a function for MCAR setting estimator
ucc_test <- function(N)
{
  dt_full <- data.frame(sample=numeric(),E_R= numeric(),E_RY = numeric(),u_cc = numeric(),est_mean=numeric())
  
  for(i in 1:N){
    #start value for Y  
    init_y <- rbinom(1000,1,0.75)# each time 1000 people
    init_pr <- rbinom(1000,1,0.1)#each time 1000 people having 0.1 to respond
    dt_full[i,]<- c(mean(init_y),mean(init_pr),mean(init_y*init_pr),mean(init_y*init_pr)/mean(init_pr),mean(dt_full$sample))
    
  }
  return(dt_full)
}