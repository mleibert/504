set.seed(1)
mat<-matrix(round(runif(4,0,10),0) ,2,2)
mat<-cbind(mat,diag(nrow(mat)))
mat[2,2]<-5

mat[2,]<-mat[2,]-(mat[2,1]/mat[1,1])*mat[1,]
mat

mat[1,]<-mat[1,]-(mat[1,2]/mat[2,2])*mat[2,]


## 3x3
n=3
set.seed(1)
mat<-matrix(round(runif(n^2,0,55),0) ,n,n)
mat<-matrix(round(runif(n^2,1,5),0) ,n,n)
mat<-cbind(mat,diag(nrow(mat)))
mat[1,1]<-3
solve(mat[1:n,1:n])

mat[2,]<-mat[2,]-(mat[2,1]/mat[1,1])*mat[1,]	#4
mat[3,]<-mat[3,]-(mat[3,1]/mat[1,1])*mat[1,]	#4

mat

mat[3,]<-mat[3,]-(mat[3,2]/mat[2,2])*mat[2,]	#3

mat

mat[2,]<-mat[2,]-(mat[2,3]/mat[3,3])*mat[3,]
mat[1,]<-mat[1,]-(mat[1,3]/mat[3,3])*mat[3,]

mat

mat[1,]<-mat[1,]-(mat[1,2]/mat[2,2])*mat[2,]

mat

mat[1,]<-mat[1,]*(1/mat[1,1])
mat[2,]<-mat[2,]*(1/mat[2,2])
mat[3,]<-mat[3,]*(1/mat[3,3])


