#Loading packages
library(tidyverse)
#Loading the data
data<-features_atual
summary(data)

#Working on the database
data<-data %>% 
      filter(categoria!="NA") %>% 
      select(-file)

freq<-data %>% 
      group_by(categoria) %>% 
      tally() 
     

#Splitting data
set.seed(42)
train_index<-sample(1:nrow(data),
                    round(0.7*nrow(data)))

train_data<-data[train_index,]
test_data<-data[-train_index,]

#==================
library(xgboost)
library(Matrix)
#Preparing the data
sparse_matrix<-sparse.model.matrix(categoria~.,data=train_data)[,-1]
sparse_matrix_test<-sparse.model.matrix(categoria~.,data=test_data)[,-1]


train_numeric<-train_data %>% 
            select_if(is.numeric)

class_vector<-train_data$categoria %>% as.numeric()
class_vector<-class_vector-1
class_vector<-ifelse(class_vector==7,4,class_vector)
freq_train<-train_data %>% 
      group_by(categoria) %>% 
      tally() 


class_vector_test<-test_data$categoria %>% as.numeric()
class_vector_test<-class_vector_test-1
class_vector_test<-ifelse(class_vector_test==7,4,class_vector_test)


train_matrix<-matrix(sparse_matrix,nrow = nrow(train_data))
dtrain<-xgb.DMatrix(data=(sparse_matrix),label=class_vector)
dtest<-xgb.DMatrix(data=(sparse_matrix_test),label=class_vector_test)


xgb_model<-xgboost(params = list("eta"=0.1, #Range [0,1]
                                 "gamma"=0, #Range [0,Inf]
                                 "max_depth"=6,#Range[1,Inf]   
                                 "min_child_weight"=1,
                                 "subsample"=1,
                                 "colsample_bytree"=1,
                                 "objective"='multi:softmax',
                                 "num_class"=7,
                                 "eval_metric"='mlogloss'),
                   data=dtrain,
                   nround = 300,
                   verbose=1) #  

xgb_cv<-xgb.cv(params = list("eta"=0.1, #Range [0,1]
                             "gamma"=0, #Range [0,Inf]
                             "max_depth"=6,#Range[1,Inf]   
                             "min_child_weight"=1,
                             "subsample"=1,
                             "colsample_bytree"=1,
                             "objective"='multi:softmax',
                             "num_class"=7,
                             "eval_metric"='mlogloss'),
               data=dtrain,
               nrounds = 100,
               verbose=1,nfold =10)

g<-predict(xgb_model,newdata = dtest)

sum(diag(table(g,class_vector_test)))/sum(table(g,class_vector_test))

e<-data.frame(xgb_cv$evaluation_log)

plot(e$iter,e$train_mlogloss_mean,col='blue',type='l')
lines(e$iter,e$test_mlogloss_mean,col='red',type='l')
# library(rpart)
# single_tree<-rpart(categoria~.,data=train_data,control = rpart.control(maxdepth = 2))


#===========================================================
#===========Hold out rep====================================
#===========================================================
acc_rep<-numeric(0)

for(i in 1:30){
train_index<-sample(1:nrow(data),
                    round(0.7*nrow(data)))

train_data<-data[train_index,]
test_data<-data[-train_index,]

sparse_matrix<-sparse.model.matrix(categoria~.,data=train_data)[,-1]
sparse_matrix_test<-sparse.model.matrix(categoria~.,data=test_data)[,-1]


train_numeric<-train_data %>% 
   select_if(is.numeric)

class_vector<-train_data$categoria %>% as.numeric()
class_vector<-class_vector-1
class_vector<-ifelse(class_vector==7,4,class_vector)
# freq_train<-train_data %>% 
#    group_by(categoria) %>% 
#    tally() 


class_vector_test<-test_data$categoria %>% as.numeric()
class_vector_test<-class_vector_test-1
class_vector_test<-ifelse(class_vector_test==7,4,class_vector_test)


train_matrix<-matrix(sparse_matrix,nrow = nrow(train_data))
dtrain<-xgb.DMatrix(data=(sparse_matrix),label=class_vector)
dtest<-xgb.DMatrix(data=(sparse_matrix_test),label=class_vector_test)


xgb_model<-xgboost(params = list("eta"=0.1, #Range [0,1]
                                 "gamma"=0, #Range [0,Inf]
                                 "max_depth"=6,#Range[1,Inf]   
                                 "min_child_weight"=1,
                                 "subsample"=1,
                                 "colsample_bytree"=1,
                                 "objective"='multi:softmax',
                                 "num_class"=7,
                                 "eval_metric"='mlogloss'),
                   data=dtrain,
                   nround = 100,
                   verbose=1) # 

g<-predict(xgb_model,newdata = dtest)

acc_rep[i]<-sum(diag(table(g,class_vector_test)))/sum(table(g,class_vector_test))
print(i)
}
