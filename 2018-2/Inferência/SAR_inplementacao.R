I <- diag(rep(1,10))
Sigma <- function(I,rho,W,sigma2){
  out <- (solve(I-(rho%*%W)))%*%(sigma2%*%I)%*%(solve(I-(rho%*%W)))
  return(out)
}

#Verossimilhança
L <- function(p,Sigma,Y){
  out <- prod( ((2*pi)^(-p/2)) * (1/determinant(Sigma,logarithm=F)) * exp((-1/2)*Y%*%solve(Sigma)%*%t(Y)) )
  return(out)
  }

#Log-verossimilhança
lv <- function(p,n,Sigma,Y){
  out <- (-(n*p*ln(2*pi))/2) -(n*ln(abs(determinat(Sigma)))) -(1/2)*sum((Y^2)*solve(Sigma))
  return(out)
}

#Maximizando a log-verossimilhança
lv <- Vectorize(lv,"Sigma") #???
est <- optim(par(0,0), fn=lv, y=Y, x=X, method = "BFGS",control = list(fnscale = -1), hessian = T)

#rho <- est ???

beta <- funciton(X,I,Y,rho,W){
  out <- ((solve(t(X)%*%X))%*%(t(X)%*%(I%*%Y))) -rho*((solve(t(X)%*%X))%*%t(X)%*%W%*%Y)
  return(out)
}

