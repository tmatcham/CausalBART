library('aciccomp2016')
library('BayesTree')



#Use cross validation to find overfitting

covariates = input_2016


for( i in 1:77){
  data <- as.data.frame(dgp_2016(input_2016, i, 1)) 
  
  Ytrain = data$y
  Xtrain = cbind( 'Z' = data$z, covariates)
  
  Ytest = Ytrain[Xtrain$Z ==1]
  Xtest = Xtrain[Xtrain$Z ==1,]
  Xtest$Z = 0 
  
  bartFit = bart(x.train =Xtrain, y.train = Ytrain, x.test = Xtest, nskip = 500, ndpost = 300)
  
  Ytesthat = bartFit$yhat.test.mean
  
  SATT = mean(Ytest - Ytesthat)
  write.csv(SATT,paste0("SATTest/",i,'_',j,'.csv'), row.names = FALSE)
}


###### Linear Model Testing

library('aciccomp2016')
library('BayesTree')



#Use cross validation to find overfitting

covariates = input_2016

for( i in 1:77){
  t1 = proc.time()
  data <- as.data.frame(dgp_2016(input_2016, i, 1)) 
  
  Ytrain = data$y
  Xtrain = cbind( 'Z' = data$z, covariates)
  
  dftrain = cbind('Y'=Ytrain, Xtrain)
  
  
  #bartFit = bart(x.train =Xtrain, y.train = Ytrain, x.test = Xtest, nskip = 500, ndpost = 300)
  model1 = lm(Y~., data = dftrain)
  
  Ytest = Ytrain[Xtrain$Z ==1]
  Xtest = Xtrain[Xtrain$Z ==1,]
  Xtest$Z = 0 
  Ytesthat <- predict(model1, newdata = Xtest)
  

  SATT = mean(Ytest - Ytesthat)
  write.csv(SATT,paste0("SATTest_lm/",i,'_',1,'.csv'), row.names = FALSE)
  t2 = proc.time()
  print(t2-t1)
}










