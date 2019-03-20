############Neural net##############
# 1. Neural net for classification
predVars = c("Sepal.Width","Sepal.Length")
set.seed(1)
grid = expand.grid(seq(min(iris[,predVars[1]]),max(iris[,predVars[1]]),length.out = 300),
                   seq(min(iris[,predVars[2]]),max(iris[,predVars[2]]),length.out = 300))
names(grid) = predVars

nn = nnet(iris$Species ~ .,data = iris[,predVars],size = 10,maxit = 1000)
prediction = as.factor(predict(nn,grid,"class"))

layout(matrix(c(1,1,2)))
predPlot(iris[,predVars],iris$Species,grid,as.factor(prediction),c("blue","red","green"))
plotnet(nn)

layout(matrix(c(1,1,2)))
prediction2 = predict(nn,grid)
plot(grid,col = rgb(prediction2))
points(iris[,predVars],col = Mix(c("red","green","blue"),"white")[as.numeric(iris$Species)],pch = 16)
plotnet(nn)

#################################3
# Tune it!
#Prediction grid
param = expand.grid(1:10,seq(0,0.1,0.01))

#Train/test split
set.seed(1)
index = sample(1:150,75)
train = iris[index,]
test = iris[-index,]

kappa = rep(NA,nrow(param))
for(i in 1:nrow(param))
{
  nn = nnet(iris$Species ~ .,data = iris[,predVars],decay =param[i,2],size = param[i,1],maxit = 1000)
  prediction = as.factor(predict(nn,test,"class"))
  kappa[i] = classAgreement(table(prediction,test$Species))$kappa
}

i = which.max(kappa)
nn = nnet(iris$Species ~ .,data = iris[,predVars],decay =param[i,2],size = param[i,1],maxit = 1000)
prediction = as.factor(predict(nn,grid,"class"))
predPlot(iris[,predVars],iris$Species,grid,as.factor(prediction),c("blue","red","green"))

layout(matrix(c(1,1,2)))
prediction2 = predict(nn,grid)
plot(grid,col = rgb(prediction2))
points(iris[,predVars],col = Mix(c("red","green","blue"),"white")[as.numeric(iris$Species)],pch = 16)
plotnet(nn)
##########################################


#############################################
# 2. Use a NN fit in the northern hemisphere to predict values in the southern
d = read.csv("C:/Users/au526940/Dropbox/????ѧϰ/course/machine learning in R/Mammal data N.csv",as.is=TRUE)
d2 = read.csv("C:/Users/au526940/Dropbox/????ѧϰ/course/machine learning in R/Mammal data S.csv",as.is=TRUE)
d2 = d2[,1:22]

mins = apply(d,2,min)
maxs = apply(d,2,max)

d = (d-matrix(mins,byrow = TRUE,ncol = ncol(d),nrow = nrow(d)))/
  (matrix(maxs,byrow = TRUE,ncol = ncol(d),nrow = nrow(d))-matrix(mins,byrow = TRUE,ncol = ncol(d),nrow = nrow(d)))
d2 = (d2-matrix(mins,byrow = TRUE,ncol = ncol(d2),nrow = nrow(d2)))/
  (matrix(maxs,byrow = TRUE,ncol = ncol(d2),nrow = nrow(d2))-matrix(mins,byrow = TRUE,ncol = ncol(d2),nrow = nrow(d2)))

#Try one out
nn1 = nnet(Mammal ~ .,data = d[,3:22],size = 5,decay = 0.01)

predictTrain = predict(nn1,d)
predictTest = predict(nn1,d2)

plot(predictTrain,d$Mammal)
cor(predictTrain,d$Mammal)^2 #r2 = 0.811

plot(predictTest,d2$Mammal)
cor(predictTest,d2$Mammal)^2 #r2 = 0.697

# Tune based on the NE predicting to the NW
param = expand.grid(1:20,seq(0.001,0.1,length.out = 10))
R2 = rep(NA,nrow(param))
for(i in 1:nrow(param))
{
  nn1 = nnet(Mammal ~ .,data = d[d$Lon<0.5,3:22],size = param[i,1],decay = param[i,2])
  predictTest = predict(nn1,d[d$Lon<0.5,3:22])
  R2[i] = cor(predictTest,d[d$Lon<0.5,"Mammal"])^2
}
i = which.max(R2)
nn1 = nnet(Mammal ~ .,data = d[,3:22],size = param[i,1],decay = param[i,2])
predictTest = predict(nn1,d2)
cor(predictTest,d2$Mammal)^2 #0.715
plot(predictTest,d2$Mammal)

#Response curve - choose a variable, replace all others with their mean
i = which(names(d) == "bio_2")
d3 = matrix(apply(d,2,mean),ncol = ncol(d),nrow =nrow(d),byrow = TRUE)
d3[,i] = d[,i]
d3 = data.frame(d3)
names(d3) = names(d)
predicted = predict(nn1,d3)
plot(d3[,i],predicted)


##########################################
# 3. Regression on Natality data
require(nnet)
require(randomForest)
require(gbm)
d =read.csv("C:/Users/au526940/Dropbox/????ѧϰ/course/machine learning in R/CDC natality data.csv")

d$Age = (d$Age - min(d$Age))/(max(d$Age)-min(d$Age))
d$Year = (d$Year - min(d$Year))/(max(d$Year)-min(d$Year))
d$Births = (d$Births - min(d$Births))/(max(d$Births)-min(d$Births))
d$Weight = (d$Weight - min(d$Weight))/(max(d$Weight)-min(d$Weight))
d$Gestation = (d$Gestation - min(d$Gestation))/(max(d$Gestation)-min(d$Gestation))

nn = nnet(Weight ~ .,d,size = 5)


#########################################


#########################################
# My code for GA model selection
#Create a population, stored a as a matrix (100 individuals, each with 100 "genes")
N = 1000
Genes = 50
Generations = 300
pop1 = matrix(rbinom(n = N * Genes,size = 1, prob = 0.5),nrow = Genes)

#Establish the relationship we want to uncover
X = matrix(rnorm(500 * 100,0,1),nrow = 500, ncol = Genes)
Y = apply(X[,1:Genes/2],1,sum) + rnorm(500,sd = 8)

# Our hope is to find out that the first 50 X should be in the model
train = data.frame(X[1:250,])
test = data.frame(X[251:500,])


#Run for many generations
pdf("Genetic algorithm model specification.pdf")
R2 = matrix(NA,nrow = N,ncol = Generations)
for(g in 1:Generations)
{
  #compute the fitness for each individual
  for(i in 1:N)
  {
    model = lm(Y[1:250] ~.,data = train[,pop1[,i]==1])
    predicted = predict(model,test)
    R2[i,g] = cor(predicted,Y[251:500])
  }
  #Each individual reproduces with prob = fitness
  pop2 = matrix(NA,ncol = N,nrow = Genes)
  for(i in 1:N)
  {
    parents = sample(1:N,2,prob = R2[,g]^2) #squaring the R2 to give a bigger advantage to the best models
    crossover = sample(1:(Genes-1),1)
    pop2[,i] = c(pop1[1:crossover,parents[1]],pop1[(crossover+1):Genes,parents[2]])
    
    #Mutation
    if(runif(1) < 0.1) # 10% mutation rate
    {
      mutation = sample(1:Genes,1)
      pop2[mutation,i] = 1-pop2[mutation,i]
    }
  }
  #The new population replaces the old one
  pop1 = pop2
  print(g)
  image(pop1)
}
dev.off()



plot(apply(R2,2,median),type = "l",lwd = 2,xlab = "Generation",ylab = "Test R2",ylim = c(0.5,1))
points(apply(R2,2,min),type = "l")
points(apply(R2,2,max),type = "l")

index = which(apply(pop1,1,mean)>0.5)
model = lm(Y[1:250] ~.,data = train[,index])
predicted = predict(model,test)
cor(predicted,Y[251:500]) # R2 = 0.753

#Compare to step
model2 = step(lm(Y[1:250] ~.,data = train),trace = 0 )
predicted2 = predict(model2,test)
cor(predicted2,Y[251:500]) # R2 = 0.730
###########################################
