library(caret)
library(ggplot2)
library(Hmisc)
library(nnet)

# after simple visual inspection, notice #DIV/0! occurs and needs to be removed.
training <- read.csv("./data/pml-training.csv", header = TRUE, na.strings = c('', 'NA','#DIV/0!'))
testing <- read.csv("./data/pml-testing.csv", header = TRUE, na.strings = c('', 'NA','#DIV/0!'))
controlFunction <- trainControl(method = "cv", number = 3,  verboseIter = TRUE)
t=training
#t<-createDataPartition(y=training$classe,p=.05,list=F)
#t=training[t,]
t=t[,summarize2(training)]
t=t[,-6]
t=t[,-5]
t=t[,-4]
t=t[,-3]
t=t[,-2]
t=t[,-1]
t2=testing
t2=t2[,summarize2(training)] 
t2=t2[,-6]
t2=t2[,-5]
t2=t2[,-4]
t2=t2[,-3]
t2=t2[,-2]
t2=t2[,-55]
t2=t2[,-1]
M<-abs(cor(t[,-54]))
diag(M)=0
which(M>.8,arr.ind=T)
model <- train(classe ~ ., data = t,  method="rf", trControl = controlFunction)
preproc=preProcess(t[,-54],method="pca")
#
predicttr <- predict(model$f,t)
table(predicttr, t$classe)

trainpc=predict(preproc,t[,-54])
model2 <- train(classe ~ ., data = trainpc,  method="rf", trControl = controlFunction)
testpc=predict(preproc,t2)

predict(model$finalModel,newdata=t2)
## creating a small sample for testing
# @param t - the input dataset
createSample<-function (t){
s1=subset(t,classe=="A")[1:10,]
s2=subset(t,classe=="B")[1:10,]
s3=subset(t,classe=="C")[1:10,]
s4=subset(t,classe=="D")[1:10,]
s5=subset(t,classe=="E")[1:10,]
t3=merge(s1,s2,all=T)
t3=merge(t3,s3,all=T)
t3=merge(t3,s4,all=T)
t3=merge(t3,s4,all=T)
t3
}

summarize2 <- function(d)
{
  v1<-c()
  i=0
  
  for( j in 1:length(colnames(d) ))
  {
    i=i+1
    v1[i]=T
    if(shouldRemove(d[,j]))
    {
      #print(as.vector(colnames(d))[j])
      
      # v1[i]=colnames(d)[j]
      v1[i]=F
    }
  }
  v1;
  
}
somefunction=function(x) { x[ , colSums( is.na(x) ) < nrow(x) ] }
percentNA<-function(x)
{
  y=as.array(x)
  if(nrow(y)==0)
  {
    0;
  }else
  {
    ### print(as.array(is.na(y)))
    logicalVector=is.na(y)
    subsetofy=y[c(logicalVector)]
    ## need to slice the arrray !!!!!!!
    nrow(as.array(subsetofy))/nrow(y)
  }
  
}
shouldRemove<-function(x)
{
  if(percentNA(x)>0)
  {
    TRUE
  }else{
    FALSE
  }
}