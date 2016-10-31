#  **************************************************
#  Read in the data from csv file:
#  **************************************************

# The file we read in has column names and includes
# all the x's (features) and y's (targets)
# without the 1's on left hand side
# like this:
#  x1 x2 x3 ... xN y
#  x1 x2 x3 ... xN y
#  x1 x2 x3 ... xN y
#  x1 x2 x3 ... xN y


df <- read.table("name_of_data_file.csv",
           header = TRUE, 
   	   sep = ",",
	   stringsAsFactors = FALSE)


#  **************************************************
# Start program:
#  **************************************************

#  Make X matrix with the 1's on the left hand side
#  and make the y vector separately:

X <- data.matrix(df[,1:ncol(df)-1], rownames.force = NA)
X <- cbind( rep(1,length.out = nrow(X) ) , X )

y <- df[,ncol(df)]


#  Calculate theta:

theta <- unname( solve( t(X) %*% X ) %*% t(X) %*% y )


# **************************************************

#  Here we predict for a guess:
#  Plug in the values for the guess including the initial 1 in the
#  vector defined here by the c() function:


answer <-  sum( c(1,1650,3) * theta)

