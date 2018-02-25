 rm(list = ls())


fxy<-function(x,y){ 100*(y-x^2)^2 + (1-x)^2 }
dfdx<-function(x,y){ -400*x*y+400*x^3  + 2*x - 2 }
dfdy<-function(x,y){ 200*y - 200 * x^2  }

s<-1

fxy(4,4)
fxy(4,4)-dfdx(4,4)*1
fxy(4,4)-dfdy(4,4)*1

fxy(-4797,16809)-dfdx(-4797,16809)*1
fxy(4,4)-dfdy(4,4)*1


# set up a stepsize
alpha = 0.03

# set up a number of iteration
iter = 500

# define the gradient of f(x) = x^4 - 3*x^3 + 2
gradient = function(x) return((4*x^3) - (9*x^2))

# randomly initialize a value to x
set.seed(100)
x = floor(runif(1)*10)

# create a vector to contain all xs for all steps
x.All = vector("numeric",iter)

# gradient descent method to find the minimum
for(i in 1:iter){
        x = x - alpha*gradient(x)
        x.All[i] = x
        print(x)
}

x<-y<-4

for(i in 1:100000){
	x<-x-dfdx(x,y)
	y<-y-dfdy(x,y)
}
x;y

x<-y<-4
for( i in 1:150000) {

	x1<-x-dfdx(x,y)*.0003 
	y1<-y-dfdy(x,y)*.0003
	x<-x1;y<-y1
}
x;y


x<-y<-4
vecc<-vec<-rep(NA,150)
for( i in 1:150) {

	vecc[i]<-fxy(x,y)
	x1<-x-dfdx(x,y)*.0003 
	y1<-y-dfdy(x,y)*.0003
	x<-x1;y<-y1
	vec[i]<-fxy(x,y)
}

fxy<-function(x,y){ 100*(y-x^2)^2 + (1-x)^2 }

dfdx<-function(x,y){ -400*x*y+400*x^3  + 2*x - 2 }
dfdy<-function(x,y){ 200*y - 200 * x^2  }


x<-y<-4;
for( i in 1:21111) {

	di<-c(dfdx(x,y) ,dfdy(x,y))
	#di<- (  di/(sqrt(sum(di^2)) ) )
	si<-.5
	while( fxy(x,y) <  fxy(  x-si*di[1], y-si*di[2])  ) {
	si<-si/2 }
	x<-x-di[1]*si 
	y<-y-di[2]*si

	fxy(x,y)
}

x;y

a<-c(7,3);
a<-a/(sqrt(sum(a^2))  )
sum(a^2)

x 





 


