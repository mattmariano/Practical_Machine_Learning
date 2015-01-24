## creating a small sample for testing
## this is very useful when creating the markdown in order to see the output quickly without having to train on the entire training set
# @param t - the input dataset
createSample<-function (t){
  s1=subset(t,classe=="A")[1:100,]
  s2=subset(t,classe=="B")[1:100,]
  s3=subset(t,classe=="C")[1:100,]
  s4=subset(t,classe=="D")[1:100,]
  s5=subset(t,classe=="E")[1:100,]
  t3=merge(s1,s2,all=T)
  t3=merge(t3,s3,all=T)
  t3=merge(t3,s4,all=T)
  t3=merge(t3,s5,all=T)
  t3
}
#
#
# @param x the vector or array of predictions 
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}