#Daniel Shriner
#February 27, 2024
#probability of "genetic flip-flops" under the null hypothesis and for underpowered studies

#Figure 2A
reps <- 100000
beta <- c(0,0.025,0.05,0.075,0.10,0.125,0.15,0.175,0.20)
rejection.rate <- vector("numeric",length(beta))
for (j in 1:length(beta)) {
	p.val <- vector("numeric",reps)
	for (i in 1:reps) {
		p <- 0.2
		x <- rbinom(1000,2,p)
		y <- beta[j]*x+rnorm(1000,0,sqrt(1-2*p*(1-p)*beta[j]^2))
		p.val[i] <- summary(lm(y~x))$coefficients[2,4]
	}
	rejection.rate[j] <- length(which(p.val<=0.05))/length(p.val)
}
png("Figure 2A.png",height=6,width=6,units="in",res=600)
plot(beta,rejection.rate,xlab="Effect Size",ylab="Rejection Rate",type="l",ylim=c(0,1))
abline(h=0.05,col="red")
dev.off()

##########################################################
#Figure 2B
reps <- 100000
beta <- c(0,0.025,0.05,0.075,0.10,0.125,0.15,0.175,0.20)
rejection.rate <- vector("numeric",length(beta))
for (j in 1:length(beta)) {
	z <- vector("numeric",reps)
	for (i in 1:reps) {
		p <- 0.2
		x <- rbinom(1000,2,p)
		y <- beta[j]*x+rnorm(1000,0,sqrt(1-2*p*(1-p)*beta[j]^2))
		z[i] <- summary(lm(y~x))$coefficients[2,3]
	}
	rejection.rate[j] <- length(which(z<=-1.959964))/length(z)
}
png("Figure 2B.png",height=6,width=6,units="in",res=600)
plot(beta,rejection.rate,xlab="Effect Size",ylab="Rejection Rate",type="l",ylim=c(0,0.025))
abline(h=0.025,col="red")
dev.off()
