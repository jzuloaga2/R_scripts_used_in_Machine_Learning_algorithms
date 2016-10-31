#  **************************************************
#  Read in the data from csv file:
#  **************************************************

# The file we read in has column names and includes
# all the x's (features )and y's (targets)
# without the 1's on left hand side
# like this:
#  x1 x2 x3 ... xN y
#  x1 x2 x3 ... xN y
#  x1 x2 x3 ... xN y
#  x1 x2 x3 ... xN y


df <- read.table("name_of_file.csv",
           header = TRUE, 
   	   sep = ",",
	   stringsAsFactors = FALSE)

#  **************************************************
#  Define Learning Rate (alpha) and number of
#  iterations:
#  **************************************************

alpha <- 0.01
iterations <- 10000


#  **************************************************
# Start program:
#  **************************************************

# Make X matrix with the 1's on the left hand side and
# no y's
# and make the y vector separately:

X <- data.matrix(df[,1:ncol(df)-1], rownames.force = NA)
X <- cbind( rep(1,length.out = nrow(X) ) , X )

y <- df[,ncol(df)]

# Make initial theta vector of zeros (initial guess):
theta <- rep(0, times = ncol(X) )

# Number of training examples which we call 'm':
m <- nrow(X)

#  **************************************************
#  Do Feature Scaling:
#  **************************************************

# Save original values of X before adjusting the
# values for feature scaling:

X_original <- X

#  Now do feature scaling on X:

the_mean <- c()
the_sd <- c()

for(ii in 2:ncol(X)){
  the_mean <- c(the_mean, mean(X[,ii])  )
  the_sd  <- c(the_sd, sd(X[,ii]) )
  X[,ii] <- ( X[,ii]- mean(X[,ii]) ) / sd(X[,ii]) 
}

#  **************************************************
#  Gradient Descent:
#  **************************************************

iteration <- c(0)
J <- 1/(2*m) * sum( (X %*% theta - y) * (X %*% theta - y) )
J_v <- c(J)


for(ii in 1:iterations){
  theta <- theta - alpha/m *  colSums(  as.vector( (X %*% theta - y) ) * X  )
  J <- 1/(2*m) * sum( (X %*% theta - y) * (X %*% theta - y) )
  J_v <- c(J_v,J)
  iteration <- c(iteration,ii)
}
theta <- unname(theta)

# **************************************************

#  Plot the cost function J evolving as a function
#  of the iteration number:

pdf("J.pdf") 
plot(iteration,J_v,
     type = "p",
     main = "Cost Function J vs Iteration",
     xlab = "Iteration Nunber",
     ylab = "Cost Function J",
     lwd  = 1,
     col = "blue"
     )
dev.off()

#  **************************************************
#  Find a guess:
#  **************************************************

# Note that input vector for the guess does not include the
# initial 1, we add that later by hand:

input <- c(1650,3)
input <- (input - the_mean) / the_sd
input <- c(1,input)

answer <- sum(theta * input)
