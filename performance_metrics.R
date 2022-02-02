# path of csv file
CSV_PATH = "C:\\Users\\Gordon\\Downloads\\logistic regression_model_2_probabilities.csv"
LENGTH_OF_CLASS_IDENTIFIER = 6

data=read.csv(CSV_PATH)
data_new=subset(data,select=c('class0','class1', 'class2', 'class3', 'class4'))

# get the prediction probabilities
data_new['class0']=1-data['class0']
data_new['class1']=1-data['class1']
data_new['class2']=1-data['class2']
data_new['class3']=1-data['class3']
data_new['class4']=1-data['class4']

#' Function taken from course website: Dr. Harry Joe STAT 447B 2021
#' @description
#' Prediction intervals for a categorical response 
#'
#' @param ProbMatrix of dimension nxJ, J = # categories, 
#'             each row is a probability mass function 
#' @param labels vector of length J, with short names for categories
#'
#' @details
#' A more general function can be written so the levels of prediction intervals
#' can be other than 0.50 and 0.80.
#'
#' @example
#' labels=c("A","b","c","D")
#' p1=c(0.3,0.2,0.1,0.4)
#' p2=c(0.6,0.1,0.1,0.2)
#' ProbMatrix=rbind(p1,p2)
#' CategoryPredInterval(ProbMatrix,labels)
#'
#' @return list with two string vectors of length n:
#'  pred50 has 50% prediction intervals
#'  pred80 has 80% prediction intervals
#'
CategoryPredInterval = function(ProbMatrix,labels)
{ ncases=nrow(ProbMatrix)
pred50=rep("0",ncases)
pred80=rep("0",ncases)
for(i in 1:ncases)
{ p=ProbMatrix[i,]
ip=order(p)
pOrdered=p[ip] # increasing order
labelsOrdered=labels[rev(ip)] # decreasing order
G=rev(cumsum(c(0,pOrdered))) # cumulative sum from smallest
k1=min(which(G<=0.5))-1  # 1-level1= 1-0.5=0.5
k2=min(which(G<=0.2))-1  # 1-level2= 1-0.8=0.2
pred1=labelsOrdered[1:k1]
pred2=labelsOrdered[1:k2]
pred50[i]=paste(pred1,collapse="")
pred80[i]=paste(pred2,collapse="")
}
list(pred50=pred50, pred80=pred80)
}

#' Function taken from course website: Dr. Harry Joe STAT 447B 2021
#' @description Coverage rate of prediction intervals for a categorical response
#' @param Table table with class labels as row names, subsets as column names
#' @return list with average length, #misses, miss rate, coverage rate by class
coverage=function(Table)
{ nclass=nrow(Table); nsubset=ncol(Table);  rowFreq=rowSums(Table)
labels=rownames(Table); subsetLabels=colnames(Table)
cover=rep(0,nclass); avgLen=rep(0,nclass)
for(irow in 1:nclass)
{ for(icol in 1:nsubset)
{ intervalSize = nchar(subsetLabels[icol])
isCovered = grepl(labels[irow], subsetLabels[icol])
frequency = Table[irow,icol]
cover[irow] = cover[irow] + frequency*isCovered
avgLen[irow] = (avgLen[irow] + frequency*intervalSize)
}
}
miss = rowFreq-cover;   avgLen = avgLen/rowFreq
out=list(avgLen=avgLen/LENGTH_OF_CLASS_IDENTIFIER,miss=miss,missRate=miss/rowFreq,coverRate=cover/rowFreq)
return(out)
}

#' @description Computes the interval loss score for a given prediction interval:
#' interval loss score = 1 - length of prediction interval/number of classes
#' @param list of prediction intervals as a string
#' @return linterval loss score
interval_loss=function(predint){
  loss=0
  nlength=length(predint)
  for (i in 1:nlength){
    if (data$test[i] == 0){
      substring = 'class0'
    }
    else if (data$test[i] == 1){
      substring = 'class1'
    }
    else if (data$test[i] == 2){
      substring = 'class2'
    }
    else if (data$test[i] == 3){
      substring = 'class3'
    }
    else if (data$test[i] == 4){
      substring = 'class4'
    }
    string = predint[i]
    contain_true_class = grepl(substring, string, fixed = TRUE)
    if (string==substring){
      loss=loss
    }
    else if (!contain_true_class){
      loss=loss+1
    }
    else{
      length_interval=nchar(string)/length(substring)
      loss=loss+(1-(1/length_interval))
    }
  }
  return (loss/length(predint))
}

# create prediction intervals
predint=CategoryPredInterval(data_new,labels=c('class0','class1', 'class2', 'class3', 'class4'))
print(table(data$test,predint$pred50))


# create table of 50% prediction interval
table50=table(data$test,predint$pred50)
rownames(table50)=c('class0','class1', 'class2', 'class3', 'class4')

# create table of 80% prediction interval
table80=table(data$test,predint$pred80)
rownames(table80)=c('class0','class1', 'class2', 'class3', 'class4')

table50
table80

interval_loss(predint$pred50)
interval_loss(predint$pred80)

coverage(table50)
coverage(table80)