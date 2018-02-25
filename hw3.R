options(digits=14)
newton = function(x, y) {
	i=0
	repeat{ 
		xold<-x;yold<-y
		f1 =  x^2 +x*y^3 - 9
		f2 =  3*x^2*y - y^3 - 4
		f = c(f1, f2)
		J11 = 2*x+y^3 		 
		J12 = 3*x*y^2 
		J21 = 6*x*y 
		J22 = 3 * (x^2 - y^2)
		J = matrix(c(J11 , J12 , J21 , J22 ), 2, 2)
		thetanew = c(x,y) - solve(J)%*%f
 		x<-thetanew[1];y<-thetanew[2];thetanew 
		i=i+1
		dat<-data.frame(x,",",y,i);names(dat)<-c("x"," ","y","iterations")
		if ( (abs(x - xold) < 10^-6) & ( abs(y - yold) < 10^-6 ) ){
			print(dat); break}
		 
	}}

newton(3,0)

fxy1<-function(x,y){x^2 +x*y^3 - 9}
fxy2<-function(x,y){3*x^2*y - y^3 - 4}
fxy1( 2.99  , 14     )
fxy2(   2.9983654491257 ,14   )


#################

x=4;y=4
fxy<-function(x,y) { 100*(y - x^2)^2 + (1 - x)^2 }
dfdx<-function(x,y) {2 * (-1 + x + 200* x^3 - 200 *x* y)}
dfdy<-function(x,y) {200* (-x^2 + y)}

xn1<-c(x,y)- fxy(x,y)/c(dfdx(x,y),dfdy(x,y)	)
x=xn1[1];y=xn1[2]

xn1<- c(x,y) - fxy(x,y)/c(dfdx(x,y),dfdy(x,y))
x=xn1[1];y=xn1[2]

xn1<- c(x,y) - fxy(x,y)/c(dfdx(x,y),dfdy(x,y))
x=xn1[1];y=xn1[2]

## ANSWER

 

newtraph= function(x, y) {
	i=0
	repeat{ 
		xold<-x;yold<-y
		U1 = 2* (-1 + x + 200 *x^3 - 200 *x *y) #d/dx
		U2 = 200* (-x^2 + y) #d/dy
		U = c(U1, U2)
		H11 = 1200*x^2-400*y+2		#d/dxx
		H12 = -400*x	#d/dxdy,d/dydx
		H22 = 200	#d/dyy
		H = matrix(c(H11, H12, H12, H22), 2, 2)
		thetanew = c(x,y) - solve(H)%*%U
 		x<-thetanew[1];y<-thetanew[2];thetanew 
		i=i+1
		dat<-data.frame(x, y,i);names(dat)<-c("x","y","iterations")
		if ( (abs(x - xold) < 10^-6) & ( abs(y - yold) < 10^-6 ) ){ 
			print(dat); break}
	}}

newtraph(4,4)