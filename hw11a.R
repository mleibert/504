rm(list = ls())
 setwd("G:\\math\\504")
options(scipen=999)
 


integrate(Fx,0,)

Fx(32:34)

plot(Fx,xlim=c(0, 20))

integrate(Fx,0,1000)

Fapprox<-function(n,method){
	Fx<-function(x){(1/sqrt(2*pi)) *  exp((-x^2) / 2) }
	if (method == "useR") {return(integrate(Fx,0,Inf,subdivisions=n)$value)
	} else if (method == "reimann")	 { 
		h=100/n;a=0;i=0:(n-1); return( sum( Fx(a+i*h)*h ))
	} else if (method == "trapezoid")	{ 
		h=100/n;a=0;i=0:(n-1); return( sum( ( Fx(a+i*h) +
		 Fx(a+(i+1)*h) ) / 2 * h )) 
	} else (return("Give me useR, reimann, or trapezoid please."))}
 

10^(1:4)

fapprox<-data.frame(rep(NA,3))
method<-c("reimann","trapezoid","useR")

for( i in 1:4){ fapprox[,i]<-c( Fapprox(10^i,method[1]) ,
	Fapprox(10^i,method[2]), Fapprox(10^i,method[3]) ) } 
 
rownames(fapprox)<-method
colnames(fapprox)<-paste0("10^",1:4)
fapprox

Gx(-1)

	Gx<-function(x){(1/sqrt(2*pi)) *  exp((-x^2) / 2) }
plot(Gx,xlim=c(0,10))
integrate(Gx,0,Inf,subdivisions=10)$value



Fapprox(10,"useR")
Fapprox(100,"trapezoid")
Fapprox(100,"reimann")
Fapprox(100,"reimtrapezoidann")


#############
require(splines)
bones<-read.table("BoneMassData.txt",header=T)
bones<-bones[which(bones$gender == "female"),]
n=10000

k<-seq(min(bones$age),max(bones$age),length.out = n)
B <- splineDesign(knots= k,  x=bones$age, outer.ok=T)
z<-seq(min(bones$age),max(bones$age),	length.out = 10000 )
Bpp <- splineDesign(knots=k, x=z, derivs=2, outer.ok = T)

 
dim(Bpp)
dim(B)

#3 minutes
omega<-matrix(NA,(n-4),(n-4)) 
system.time( 
for( k in 1:(n-4)){
for( l in 1:(n-4)){
	omega[k,l]<- sum( Bpp[,k]*Bpp[,l]*0.001615162 ) }}
)
 
omegas<-matrix(NA,(n-4),(n-4)) 
system.time( 
for( k in 1:(n-4)){
for( l in 1:(n-4)){
	if( is.na(omegas[l,k]) == T ) {	omegas[k,l]<- sum( 
		Bpp[,k]*Bpp[,l]*0.001615162 ) }
	else {omegas[k,l]<-omegas[l,k] }	}}
)
omega<-omegas
all( omegaz-omega  == 0);max(omegaz);max(omegas);min(omegaz);min(omegas);

omega <-0.001615162*(t(Bpp) %*% Bpp )

#invertible?
det(solve(t(B)%*%B))			#NO
det( ( t(B)%*%B + (.01 * omega)))	#YES

y<-as.matrix( bones[,4] )
A<-matrix(NA,(n-4),3)
A[,1]<-solve( t(B)%*%B + (.01 * omega) ) %*% t(B)%*% y
A[,2]<-solve( t(B)%*%B + (1 * omega) ) %*% t(B)%*% y
A[,3]<-solve( t(B)%*%B + (100 * omega) ) %*% t(B)%*% y



 
#datt<-data.frame(x,
#	B%*% solve(t(B) %*% B + .01 * omega) %*% t(B) %*% y,
#	B%*% solve(t(B) %*% B + 1 * omega) %*% t(B) %*% y,
#	B%*% solve(t(B) %*% B + 100 * omega) %*% t(B) %*% y)

datt<-data.frame(bones$age, B%*% A[,1], B%*% A[,2], B%*% A[,3])
datt<-datt[with(datt, order(bones$age)), ]

plot(bones[,2], bones[,4], xlab="age", ylab="spnbmd")
lines( datt[,1],datt[,2],col="red", lwd=2)
lines( datt[,1],datt[,3],col="blue", lwd=2)
lines( datt[,1],datt[,4],col="green", lwd=2)


smooth.spline(x,bones[,4],lambda= 0.01 )

lines(smooth.spline(x,bones[,4],lambda= 0.01 ),type="l")









########

dat<-data.frame(sin(0:10),0:10)
names(dat)<-c("y","x")
dat

model_matrix <- function(x) { 
	nx <- length(x)
	m <- cbind(rep(1, nx), x, x^2, x^3, x^4, x^5, x^6 )
	colnames(m )<-NULL
	return(m)	}

B<-  model_matrix(dat$x)
 
alpha <- solve(t(B) %*% B) %*% t(B) %*% as.matrix(dat$y)

# plot the data
plot(dat$x, dat$y )
x_grid <- seq(min(dat$x), max(dat$x), .01)
B_grid <- model_matrix(x_grid)
y_grid <- B_grid %*% alpha
lines(x_grid, y_grid, col="red", lwd=2)

#4c
D=7
dat4c<-dat[1:D,]
B4c<-  model_matrix(dat4c$x)
alpha4c <- solve(   B4c,    as.matrix(dat4c$y))
alpha4c <- solve(  t(B4c) %*%  B4c, t(B4c) %*%    as.matrix(dat4c$y))

# plot the data
plot(dat4c$x, dat4c$y , ylim= c(-1.25,1.25))
x_grid <- seq(min(dat4c$x), max(dat4c$x), .01)
B_grid <- model_matrix(x_grid)
y_grid <- B_grid %*% alpha4c 
lines(x_grid, y_grid, col="red", lwd=2)

#4d
dat4d<-dat[1:(D-2),]
B4d<-  model_matrix(dat4d$x)
a<-sample( 1:7,2)
a<-c(4,5)
B4d<-  B4d[,-a]

alpha4d <- solve(t(B4d) %*% B4d, t(B4d) %*% as.matrix(dat4d$y) )
x_grid <- seq(min(dat4d$x), max(dat4d$x), .01)
B_grid <- model_matrix(x_grid)
y_grid <-B_grid[,-a] %*% alpha4d 

plot(dat4d$x, dat4d$y , ylim= c(-1.25,1.25))
lines(x_grid, y_grid, col="red", lwd=2)


