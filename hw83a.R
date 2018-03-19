b<-b2
i=0
	
repeat{ 
	H<-     hessian.L(df$x, df$y, b)  
	if(   all(( eigen(H)$values > 0 ) == T ) 
		) {			A <-H }	else{
	A<-H+ diag(dim(H)[1])*(-min(eigen(H)$values )+1) }
	#A==H;eigen(A)	
    	d <-  ( -solve(A, grad.L(x,y,b) ) )
 	s <- 1
	    while(L(x,y, b) < L(x, y, b + s*d)){
      s <- s/2}
   	b <- b + s*d
	 i=i+1
	if( norm(grad.L(x,y,b) ) < 10^-6) {break}	
	if( i > 1000 ) {break}	
	}
b

repeat{ 
	H<-     hessian.L(df$x, df$y, b)  
	QDQ<-  (eigen(H)$vectors) %*% (eigen(H)$values * diag(dim(H)[1])) %*% 
		t( eigen(H)$vectors	 )

	if(   all(( eigen(QDQ)$values > 0 ) == T ) 
		) {			A <-QDQ }	else {A<-  (eigen(H)$vectors) %*% (
			(eigen(H)$values * diag(dim(H)[1]))+
		diag(dim(QDQ)[1])*(-min(eigen(H)$values )+1) ) %*% 
		t( eigen(QDQ)$vectors) }



    	d <-  ( -solve(A, grad.L(x,y,b) ) )
 	s <- 1
	    while(L(x,y, b) < L(x, y, b + s*d)){
      s <- s/2}
   	b <- b + s*d
	 i=i+1
	if( norm(grad.L(x,y,b) ) < 10^-6) {break}	
 	if( i > 1000 ) {break}	
	}



H<-hessian.L(df$x, df$y, b)  
H 

  (eigen(H)$vectors) %*% 
(eigen(H)$values * diag(dim(H)[1])) %*% 
t( eigen(H)$vectors	 )

A<-H  + (-min(eigen(H)$values) + 1) * diag(dim(H)[1])
 
 
QDQ<-  (eigen(H)$vectors) %*% 
(eigen(H)$values * diag(dim(H)[1])) %*% 
t( eigen(H)$vectors	 )
 A- (QDQ + (-min(eigen(QDQ )$values) + 1) * diag(dim(QDQ )[1]))
