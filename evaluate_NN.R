library(ggplot2)
library(plyr)

plot_classifications <- function(eta, p=seq(.1,.91,.1), N=5000)
{
  df <- plyr::adply(p, 1, function(cp) 
                  create_classification_df(cp, eta, N))
  
  gp <- ggplot()
  gp <- gp + geom_point(mapping=aes(x=x1, y=x2, col=y), 
                       data=df, size=1)
  gp <- gp + facet_wrap(~p, nrow=3)
  gp <- gp + xlab(NULL) + ylab(NULL)
  print(gp)
}

# assumes that p is a scalar
create_classification_df <- function(p, eta, n)
{
  x1 <- 4*runif(n) - 2
  x2 <- 4*runif(n) - 2
  X <- matrix(c(x1, x2), ncol=2)
  y_pred <- classify_NN(eta, p, X)
  
  p <- paste("p=", as.character(p), sep="")
  return (data.frame(x1=x1, x2=x2, y=factor(y_pred), p=p, 
                     stringsAsFactors = F))
}