alldata <- rbind(s.param.1[101:200,],s.param.2[101:200,],s.param.3[101:200,],s.param.4[101:200,],s.param.5[101:200,],
                 s.param.6[101:200,],s.param.7[101:200,],s.param.8[101:200,],s.param.9[101:200,],s.param.10[101:200,])
alldata <- as.data.frame(alldata)

#question 1
plot(density(alldata$theta6),main="Posterior Distribution of θ6")

#question 2
predictive <- vector(length=1000)
for (i in 1:1000){
  mean <- alldata$theta6[i]
  variance <- alldata$sigma2[i]
  predictive[i] <- rnorm(1,mean,variance)
}
plot(density(predictive),main="Predictive Distribution for Another Measurement From Machine 6")

#question 3
predictive2 <- vector(length=1000)
for (i in 1:1000){
  mean <- alldata$mu[i]
  variance <- alldata$tau2[i]
  predictive2[i] <- rnorm(1,mean,variance)
}
plot(density(predictive2),main="Posterior Distribution for Mean of Machine 7")



