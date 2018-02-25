mat<-matrix(round(runif(25,1,15),0),5,5)
solve(mat)
matt<-mat
i=0
dimens<-list()

mat<-cbind(mat,diag(10))
 

#(9 rows) ( 10+1 columns )  * 2
dims<-dim( mat[ (i+2):nrow(mat) , (i+1):10 ]	) 
dimens[[i+1]]<-paste0( "(",dims[1]," rows)" ,"(",dims[2],"+1 columns)" )
dimens[[i+1]]<-data.frame(prod(dims[1],dims[2])),dimens[[i+1]])
names(dimens[[i+1]])<-c("multiplications","reason")
mat[2:nrow(mat),]<-mat[-1, ]/mat[2:nrow(mat),1]*mat[1,1] 
mat[2:nrow(mat),-c(11:20)]
for( j in 2:nrow(mat)){ mat[j,]<- mat[1,] - mat[j,] }
 i=i+1

for(i in 1:(nrow(mat)-2)){

# (8 rows) (9 + 1 columns)
dims<-dim( ( mat[ (i+2):nrow(mat) , (i+1):10 ]	) )
dimens[[i+1]]<-paste0( "(",dims[1]," rows)" ,"(",dims[2],"+1 columns)" )
dimens[[i+1]]<-data.frame(prod(dims[1],2*(dims[2]+1)),dimens[[i+1]])
#names(dimens[[i+1]])<-c("multiplications","reason")
if( mat[ nrow(mat), (nrow(mat)-2) ] == 0){ dimens[[i+1]]<-
	paste0( "(",1," rows)" ,"(",2,"+1 columns)" ) }
if( mat[ nrow(mat), (nrow(mat)-2) ] == 0){ dimens[[i+1]]<-
	dimens[[i+1]]<-data.frame(prod(c(1,2*(2+1))),dimens[[i+1]]) 
	 }
names(dimens[[i+1]])<-c("multiplications","reason")
mat[(i+2):nrow(mat),-c(1:i)]	<-
	mat[(i+2):nrow(mat),-c(1:i)]/mat[(i+2):nrow(mat),(i+1)] * 
	mat[i+1,i+1] 
for( j in (i+2):nrow(mat)){ mat[j, ]<-  mat[j,] - mat[(i+1), ] }
}
dimens
unlist(dimens)
sum( do.call("rbind", dimens)[,1])

n=10
(2/3)*n^3 + .5 * n^2 - (7/6)* n

