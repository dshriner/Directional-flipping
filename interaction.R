#Daniel Shriner
#February 27, 2024
#directional flipping of a marginal effect when the true generating model includes an interaction

reps <- 10000
res <- matrix(0,nrow=reps,ncol=2)
q1 <- 0.2
q2 <- 0.2
q12 <- 0.2
for (i in 1:reps) {
	x1 <- rbinom(1000,2,q1)
	x2 <- rbinom(1000,2,q2)
	beta1 <- sqrt(0.005/(2*q1*(1-q1)))
	beta2 <- -sqrt(0.005/(2*q2*(1-q2)))
	beta12 <- -sqrt(0.005/(2*q12*(1-q12)))
	e <- rnorm(1000)
	y <- x1*beta1+x2*beta2+x1*x2*beta12+e
	a <- summary(lm(y~x1))
	res[i,1] <- a$coefficients[2,1]
	res[i,2] <- a$coefficients[2,4]
}
#power
length(which(res[,2]<=0.05))/reps
#probability of flip-flop
length(which(res[which(res[,2]<=0.05),1]<0))/reps
