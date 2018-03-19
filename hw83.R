dat<-read.table("non_linear_1.txt",header=T)

head(dat)

plot(dat[,2],dat[,1])

 b2=c(96, .009, 103, 106, 1000, 72, 151, 1000)


y-(b[ 1] * exp(-b[ 2] * x ) + b[ 3] *exp((-(x-b[ 4])^2) / b[ 5]^2 ) + b[ 6] *
	exp(-(x-b[ 7])^2 / b[ 8]^2 ) )^2


f(x, b) = b[1]*exp( -b[2]*x ) + b[3]*exp( -(x-b[4])^2 / b[5]^2) 
#                                   + b[6]*exp( -(x-b[7])^2 / b[8]^2)



nls(y ~b1*exp(-b2*x) +b3*exp(-(x-b4)^2/b5^2) +b6*exp(-(x-b7)^2/b8^2),
	data=df,
	start=c(b1=96,b2=.009,b3=103,b4=106,b5=1000,b6=72,b7=151,b8=1000
)
)
 )

get_predictions(df$x,b)











########
x<-df$x
y<-df$y

#loss function
sum( (y - (  b[1]*exp(-b[2]*x) +  b[3]*exp( -((x-b[4])^2) /b[5]^2 ) +
	b[6]*exp( -((x-b[7])^2) /b[8]^2 ) ) )^2 )

L(x,y,b)

pred<-function(x,b){(  b[1]*exp(-b[2]*x) + b[3]*exp( -((x-b[4])^2) /b[5]^2 ) +
	b[6]*exp( -((x-b[7])^2) /b[8]^2 ) ) }
pred(df$x,b)
get_predictions(x,b) - pred(df$x,b)


grad.L(df$x,df$y,b)

#gradient

sum ( 2* (df$y - pred(df$x,b) ) * - exp(-b[2]*x) )
sum ( 2* (df$y - pred(df$x,b) ) *  b[1]*x*exp(-b[2]*x) )
sum ( 2* (df$y - pred(df$x,b) ) *  (-2*b[3]*(x-b[4]) / b[5]^2 * exp(
	-(x-b[4])^2 / b[5]^2 )) )
sum ( 2* (df$y - pred(df$x,b) ) *  (-2*b[3]*(x-b[4])^2 / b[5]^3 * exp(
	-(x-b[4])^2 / b[5]^2 )) )
sum ( 2* (df$y - pred(df$x,b) ) *  (-2*b[6]*(x-b[7]) / b[8]^2 * exp(
	-(x-b[7])^2 / b[8]^2 )) )
sum ( 2* (df$y - pred(df$x,b) ) *  (-2*b[6]*(x-b[7])^2 / b[8]^3 * exp(
	-(x-b[7])^2 / b[8]^2 )) )

######## Newton

######### Netwon w/ bt & hessian mod

NewtonBTHM<-function( x,y, b){
i<-0;
	repeat{ 
	H<-     hessian.L(df$x, df$y, b)  
	if(   all(( eigen(H)$values > 0 ) == T ) ) {	
		A <-H }	else{
	A<-H+ diag(dim(H)[1])*(min(eigen(H)$values )+.5) }
	
	
    	d <-  ( -solve(A, grad.L(x,y,b) ) )
 	s <- 1
	    while(L(x,y, b) < L(x, y, b + s*d))
      s <- s/2
   	b <- b + s*d
	i=i+1
	if( norm(grad.L(x,y,b) ) < 10^-6) {break}	}
	print(b);print(i) }



NewtonModified(df$x,df$y,b) 
 nls(y ~ b1*exp(-b2*x) + b3*exp(-(x-b4)^2/b5^2)  + b6*exp(-(x-b7)^2/b8^2),
               data=df, start=c(b1=96, b2=.009, b3=103, b4=106,
                       b5=18, b6=72, b7=151, b8=18))
NewtonBTHM(df$x,df$y,b2) 


system.time(
NewtonModified(df$x,df$y,b2) 
)

nls(y ~b1*exp(-b2*x) +b3*exp(-(x-b4)^2/b5^2) +b6*exp(-(x-b7)^2/b8^2),
data=df,start=c(b1=96,b2=.009,b3=103,b4=106,b5=1000,b6=72,b7=151,b8=1000))
