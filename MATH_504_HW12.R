#Selina Carter
# MATH-504: HW #12
#April 21, 2018

setwd("C:/Users/selinac/Documents/2-Personal/Courses/MATH 504/HW 12")


nn <- read.table("nn.txt", header = TRUE)

nn$y <- as.factor(nn$y)

## Question 2 a
###
#Visualize the data
library(ggplot2)


ggplot(nn, 
       aes(x = x1, 
           y = x2,
           color = y)) + 
  geom_point()

x <- as.matrix(nn[c("x1", "x2")])
class(x)
dim(x)
## Question 2b
#See Word doc


### Quesion 2c
#See Word doc.

#Step 0: Make my "fake" (test) eta as a matrix (or dataframe):

#Suppose m = 4.

alpha_0 = rnorm(n = 4, mean = 1, sd = 4)
alpha_1 = rnorm(n = 4, mean = -1, sd = 4)
alpha_2 = rnorm(n = 4, mean = -3, sd = 4)
beta_0 = c(rnorm(n = 2, mean = 2, sd = 2), rep(NA, 2))
beta_1 = c(rnorm(n = 2, mean = -4, sd = 2), rep(NA, 2))
beta_2 = c(rnorm(n = 2, mean = 4, sd = 2), rep(NA, 2))
beta_3 = c(rnorm(n = 2, mean = -4, sd = 2), rep(NA, 2))
beta_4 = c(rnorm(n = 2, mean = 4, sd = 2), rep(NA, 2))

i_row <- 1:4
colnames <- c("alpha_0", "alpha_1", "alpha_2", "beta_0", "beta_1", "beta_2", "beta_3", "beta_4")
names = list(i_row, colnames)

eta = matrix(c(alpha_0, alpha_1, alpha_2, beta_0, beta_1, beta_2, beta_3, beta_4), ncol = 8, nrow = 4, dimnames = names)
eta
class(eta[,1])


eta = data.frame(alpha_0, alpha_1, alpha_2, beta_0, beta_1, beta_2, beta_3, beta_4)
eta
class(eta$alpha_0)

#Step 1: create functions for getting the alphas and betas.

get_alpha_0 = function(eta, i){
  return(eta$alpha_0[i])
}

#Test:
get_alpha_0(eta, 1)
get_alpha_0(eta, 3)

get_alphas = function(eta, i){
  alphas = c(eta$alpha_1[i], eta$alpha_2[i])
return(alphas)
}

#Test:
get_alphas(eta, 1)
get_alphas(eta, 3)

get_beta_0 = function(eta, i){
  return(eta$beta_0[i])
}

#Test:
get_beta_0(eta, 1)
get_beta_0(eta, 3) #We wouldn't want to get beta for i = 3, because we only have T1 and T2. So we only need Beta up to i = 1 and i = 2.

get_betas = function(eta, i){
  betas = c(eta$beta_1[i], eta$beta_2[i], eta$beta_3[i], eta$beta_4[i])
  return(betas)
}

#Test:
get_betas(eta, 1)
get_betas(eta, 3) #We wouldn't want to get beta for i = 3, because we only have T1 and T2. So we only need Beta up to i = 1 and i = 2.

NN(x, eta, m = 4)


NN = function(x, eta, m){
  #First, the "hidden layer" (Z1, Z2, ...Zm)
  z <- rep(NA, m)
  
  for (i in 1:m){
  z[i] = 1/(1 + exp(-get_alpha_0(eta, i) - get_alphas(eta, i)%*%x[i,]))
  }
  

  #Next, the "output layer" (T1, T2)
  t <- rep(NA, 2)
  
  for (i in 1:2){
    t[i] = 1/(1 + exp(-get_beta_0(eta, i) - get_betas(eta, i)%*%z))
  }

  #Last, the probability of Y1, Y2
  y_prob <- rep(NA, 2)
  
  for (i in 1:2){
    y_prob[i] = exp(t[i])/(exp(t[1])+ exp(t[2]))
    
  }
return(y_prob)  
}


#Test:
NN(x, eta, m = 4)







#Question 2d: see Word doc

#Function for logL(eta):

logL = function(x, eta, m, y){
  #First use NN(x, eta, m) to get the Y values given the data x and our chosen m.
  Y <- NN(x, eta, m)
  Y1 <- Y[1]
  Y2 <- Y[2]

  N = nrow(x)
  
  logL <- rep(NA, N)
  
  i = 1
  for (i in 1:N){
    logL[i] = (1-y[i])*log(Y1) + y[i]*log(Y2)
  }
  
  result = sum(logL)
  return(result)
}

#Test:

x <- as.matrix(nn[c("x1", "x2")])
y_data = as.numeric(nn$y)

logL(x, eta, m = 4, y = y_data)  #Appears to work.



#Function for gradient of logL(eta):


#Since  writing the analytical function for grad_logL(eta) is too complicated, we will use finite differences instead.



#Two-point forward difference formula
options(digits = 16)

two_point_fd = function(x, eta, m, y, h) {
  n = ncol(x)
  dim_eta = m*(n+3) + 2
  ncol_eta = n+1 + m+1

  alpha_names = NA
  for (i in 0:n){
    alpha_names[i+1] <- paste0("alpha_",as.character(i))
  }
  
  beta_names = NA
  for (i in 0:m){
    beta_names[i+1] <- paste0("beta_",as.character(i))
  }
  
  colnames <- c(alpha_names, beta_names)
  first_d <- data.frame(matrix(NA, ncol = ncol_eta, nrow = m))
  colnames(first_d) <- colnames

  
  #Modify eta by adding 0's to "NA" locations
  eta_0 <- as.matrix(eta)
  eta_0[which(is.na(eta_0)==TRUE)] <- 0
  eta_0 <- data.frame(eta_0)
  colnames(eta_0) <- colnames
  
  #Make eta_h for the "eta + h" values 
  eta_h <- eta_0
  
  for (i in 1:ncol_eta){
    for (j in 1:m){
      eta_h[j,i] <- (eta_0[j,i] + h)

      #Calculate finite difference (approximate the first derivative)
      first_d[j, i] <- (logL(x, eta_h, m, y) - logL(x, eta_mod, m, y))/h 
    }
  }
  
  #Put NA's back for the associated Beta's
  first_d <- as.matrix(first_d)
  first_d[which(is.na(eta)==TRUE)] <- NA
  return(first_d) 
}


two_point_fd(x, eta, m = 4, y = y_data, h = 5) #Seems to work with large h


two_point_fd(x, eta, m = 4, y = y_data, h = 10^-3)



#








