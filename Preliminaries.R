#Install package with all the data https://github.com/vdorie/aciccomp/tree/master/2016

if (require("remotes", quietly = TRUE) == FALSE) {
  install.packages("remotes")
  require("remotes")
}
remotes::install_github("vdorie/aciccomp/2016")

install.packages("BayesTree")

#After Installation begin here
library('aciccomp2016')
library('BayesTree')

covariates = input_2016
data <- as.data.frame(dgp_2016(input_2016, 50, 87)) 

Ytrain = data$y
Xtrain = cbind( 'Z' = data$z, covariates)

Ytest = Ytrain[Xtrain$Z ==1]
Xtest = Xtrain[Xtrain$Z ==1,]
Xtest$Z = 0 


bartFit = bart(x.train =Xtrain, y.train = Ytrain, x.test = Xtest) #, ndpost = 200 ) Use this setting for speed

Ytesthat = bartFit$yhat.test

errors = mean((Ytest - Ytesthat)^2)
