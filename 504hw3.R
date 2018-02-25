rm(list = ls())

fx<-function(x){x^4 - (20/3)*x^3 -24*x^2 + 19*x-10}
dfdx<-function(x){ 19 - 48*x - 20*x^2 + 4*x^3 }
dfddx<-function(x){ -48 - 40 *x + 12* x^2 }

curve(fx ,from=-100, to= 100)

curve(fx,from=-4, to=10)

curve(fx,from= dat[1,1]-.5, to=dat[1,1]+.5)
abline(v=dat[1,1])


x=-3;i=0

newton<-function(x){
	fx<-function(x){x^4 - (20/3)*x^3 -24*x^2 + 19*x-10}
	dfdx<-function(x){ 19 - 48*x - 20*x^2 + 4*x^3 }
	dfddx<-function(x){ -48 - 40 *x + 12* x^2 }
	i=0
	repeat{
		xold<-x 	
		x = x - (1/dfddx(x) ) *dfdx(x)

		i=i+1
		dat<-data.frame(x,  i);names(dat)<-c("x", "iterations")
		dat

		if ( (abs(x - xold) < 10^-6) ){ print(dat); break} else {next}}
	#curve(fx,from= dat[1,1]-.5, to=dat[1,1]+.5)
	#abline(v=dat[1,1])
}

newton(.5)


abline(v=-2)
abline(v= -0.934)

curve(fx,from=-4, to=10)

 	dfddx<-function(x){ -48 - 40 *x + 12* x^2 }

dfddx(seq(-2.2,.5,.002))

seq(-2.2,.5,.002)[633]

newton(-)
newton( -0.934 ) 

 seq(-2.2,.5,.002)[632]

newton(0)


##############

#likelihood

#Gradient components
dlda0<-function(a,x,y){ sum( ((1)/(exp(a[1]+a[2]*x)+1)) + y -1  ) }

dlda1<-function(a,x,y){ sum( ((x)/(exp(a[1]+a[2]*x)+1)) + x * (y-1) ) }

OR<-read.table("G:\\504\\o_ring_data.txt",header=T)
 

newtonraph<-function(a,x,y){
	i=0
a<-c(0,0);x<-OR[,1];y<-OR[,2]
	repeat{
		alphaold<-a

		G1<-sum( ((1)/(exp(a[1]+a[2]*x)+1)) + y -1  ) 
		G2<-sum( ((x)/(exp(a[1]+a[2]*x)+1)) + x * (y-1) ) 
		G<-c(G1,G2)
		
		H11= -sum( ( exp(-a[1]-a[2]*x ) ) /
			( ( 1+exp(-a[1]-a[2]*x )  )^2	) )
		H12= -sum( x*( exp(-a[1]-a[2]*x ) )/
			(( 1+ exp(-a[1]-a[2]*x )  )^2	) )
		H22= -sum( x^2*( exp(-a[1]-a[2]*x ) )/
			( (1+ exp(-a[1]-a[2]*x )  )^2	) )
		H <- matrix(c(H11, H12, H12, H22), 2, 2)

 		a <- a - solve(H)%*%G

		i=i+1
		dat<-data.frame(a[1],a[2], i)
		names(dat)<-c("a0","a1","iterations")
		
		if (  (abs(a[1] - alphaold[1]) < 10^-6) &
			(abs(a[2] - alphaold[2]) < 10^-6)
			){ print(dat); break} else {next}}

}
newtonraph(c(0,0),OR[,1],OR[,2])

as.vector(coef( glm(OR[,2]~ OR[,1], family="binomial") ))

l<-function(a,x,y){sum(	(1-y)*(-a[1]-a[2]*x) - log(1+exp(-a[1]-a[2]*x))	)}
h1<-function(a,x,y){sum( ( exp(-a[1]-a[2]*x ) ) /
	((1+  exp(-a[1]-a[2]*x )  )^2	) )}
h2<-function(a,x,y){sum( x*( exp(-a[1]-a[2]*x ) )/
	(1+ ( exp(-a[1]-a[2]*x )  )^2	) )}
h3<-function(a,x,y){sum( x^2*( exp(-a[1]-a[2]*x ) )/
	(1+ ( exp(-a[1]-a[2]*x )  )^2) )}

alp<-as.vector(coef( glm(OR[,2]~ OR[,1], family="binomial") ))

l(alpha,OR[,1],OR[,2])
dlda0(alpha,OR[,1],OR[,2])
dlda1(alpha,OR[,1],OR[,2])
-h1(alp,OR[,1],OR[,2])
-h2(alp,OR[,1],OR[,2])
-h3(alp,OR[,1],OR[,2])

solve(matrix(c(h1(alp,OR[,1],OR[,2]),
 h2(alp,OR[,1],OR[,2]),h2(alp,OR[,1],OR[,2]), h3(alp,OR[,1],OR[,2])), 2, 2))
 

#https://mathinsight.org/taylor_polynomial_multivariable_examples

tl<-function(a){ -10.1576 + 
	t(matrix(0,2,1)) %*% matrix(c(a[1]-15.0429016,a[2]+0.2321627)) + 
	.5*	t( matrix(c(a[1]-15.0429016,a[2]+0.2321627)) ) %*% 
	matrix(c(5.266113,356.4407,356.4407,24236.58),2,2) %*%  
	matrix(c(a[1]-15.0429016,a[2]+0.2321627))
}

a<-alp
x<-OR[,1]

sum( ( exp(-a[1]-a[2]*x ) ) /
			(1+ ( exp(-a[1]-a[2]*x )  )^2	) )

 sum( ( exp(-a[1]-a[2]*x ) ) /	 ( 1+ exp(-a[1]-a[2]*x )  )^2 )


  sum( x*( exp(-a[1]-a[2]*x ) )/((1+  exp(-a[1]-a[2]*x )  )^2	) )


h3<-function(a,x,y){sum( x^2*( exp(-a[1]-a[2]*x ) )/
	(1+ ( exp(-a[1]-a[2]*x )  )^2) )

