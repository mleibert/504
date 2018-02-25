#Example
x <- 10*1:nrow(volcano)
y <- 10*1:ncol(volcano)
filled.contour(x, y, volcano, color = terrain.colors,
    plot.title = title(main = "The Topography of Maunga Whau",
    xlab = "Meters North", ylab = "Meters West"),
    plot.axes = { axis(1, seq(100, 800, by = 100))
                  axis(2, seq(100, 600, by = 100)) },
    key.title = title(main = "Height\n(meters)"),
    key.axes = axis(4, seq(90, 190, by = 10)))  # maybe also asp = 1




OR<-read.table("G:\\504\\o_ring_data.txt",header=T)
coef( glm(OR[,2]~ OR[,1], family="binomial") )

#Taylor ??

tl<-function(a){ -10.1576 + 
	t(matrix(0,2,1)) %*% matrix(c(a[1]-15.0429016,a[2]+0.2321627)) + 
	.5*	t( matrix(c(a[1]-15.0429016,a[2]+0.2321627)) ) %*% 
	matrix(c(5.266113,356.4407,356.4407,24236.58),2,2) %*%  
	matrix(c(a[1]-15.0429016,a[2]+0.2321627))
}

# Likelihood
l<-function(a,x,y){sum(	(1-y)*(-a[1]-a[2]*x) - log(1+exp(-a[1]-a[2]*x))	)}


tl(c(15.0429016,  -0.2321627))
l(c(15.0429016,  -0.2321627),OR[,1],OR[,2])



tl<-function(a){ -10.1576 + 
	t(matrix(0,2,1)) %*% matrix(c(a[1]-15.0429016,a[2]+0.2321627)) + 
	.5*	t( matrix(c(a[1]-15.0429016,a[2]+0.2321627)) ) %*% 
	matrix(c(-3.262746,- -221.799024,- -221.799024,-15163.092),2,2) %*%  
	matrix(c(a[1]-15.0429016,a[2]+0.2321627))
}



p<-35 
seqa<-seq(15-p ,15+p ,.182 )
m<-length(	seqa	);m

q<-.5
seqb<-seq((-.232)-q,(-.232)+q,.0026)
m<-length(	seqb	);m

mat<- (matrix(NA,m,m))
 

for( i in 1:m ){
	ii<-seqa[i]
for( j in  1:m ){
	jj<-seqb[j]
	alph<-c(ii,jj)
	mat[i,j]<- tl(alph) }}

 
filled.contour(seqa, seqb, mat, color = terrain.colors,nlevels = 50)
 
 
