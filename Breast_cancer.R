setwd("C:/Users/sebas/Desktop/Glm_files/keggle/Breast_cancer")
install.packages("nnet")
install.packages("C50")
install.packages("reader")
install.packages("vcd")
install.packages("funModeling")



summary(cancer_data)

library(caret)
library(nnet)
library(reader)
library(C50)
library(vcd)

library(reshape2)
library(rpart.plot)
library(RColorBrewer)
library(graphics)
library(funModeling)

system("ls ../input", intern=TRUE)

bcancer_data = read.csv("data.csv")
bcancer_data = na.omit(bcancer_data)
any(is.na(bcancer_data))

bcancer_data = bcancer_data[,-33]
bcancer_data = bcancer_data[,-1]

?kmeans

cl = kmeans(bcancer_data[-1], centers = 2)


plot(bcancer_data$diagnosis ~ cl$cluster)
df = data.frame(predicted = cl$cluster,actual = bcancer_data$diagnosis)
df

library(scatterplot3d)
scatterplot3d(as.factor(df$actual) ~ df$predicted)

bcancer_data1 = bcancer_data
trIndex<-createDataPartition(bcancer_data1$diagnosis,
                             p = 0.8,
                             list=F)

train_bcancer1<-bcancer_data1[trIndex,] 
train_bcancer1
valid_bcancer1<-bcancer_data1[-trIndex,] 

model<-C5.0(diagnosis ~., data = train_bcancer1 )
plot(model)

result<-predict(model, valid_bcancer1)
result

(accuracy<-sum(result == valid_bcancer1$diagnosis)/nrow(valid_bcancer1)) 

tb1<-table(pred=result,actual=valid_bcancer1$diagnosis)
tb1

model1<-rpart(diagnosis ~., data = train_bcancer1 ,minbucket=2) 
rpart.plot(model1)

result1<-predict(model1, valid_bcancer1, type="class")
result1

table(Predicted = result1, Actual = valid_bcancer1$diagnosis)

library(neuralnet)


model_nnet<-nnet(diagnosis ~. ,
                 data= train_bcancer1,
                 size=10
)

nnet_res<-predict(model_nnet,
                  valid_bcancer1[,-1],
                  type = c("class"))

tb3<-table(pred=nnet_res,actual=valid_bcancer1$diagnosis)
tb3

train_bcancer1$diagnosis = as.factor(train_bcancer1$diagnosis)
contrasts(train_bcancer1$diagnosis)
valid_bcancer1$diagnosis = ifelse(valid_bcancer1$diagnosis=="B",1,0)
?nnet
train_bcancer1$diagnosis = ifelse(train_bcancer1$diagnosis=="B",1,0)
train_bcancer1$diagnosis
valid_bcancer1$diagnosis
model_n = neuralnet(train_bcancer1$diagnosis ~ train_bcancer1$radius_mean+train_bcancer1$texture_mean,data = train_bcancer1,hidden = 5,threshold = 0.01,linear.output = FALSE)

ne_df = as.data.frame(model_n$net.result)
ne_df_value = as.numeric(ne_df >=0.5)
ne_df_value

test = cbind(train_bcancer1$radius_mean,train_bcancer1$texture_mean)
colnames(test) = c("radius_mean","texture_mean")
predicted_n  = compute(model_n,test)
predicted_n = as.data.frame(predicted_n$net.result)
predicted_n_value = as.numeric(predicted_n>=0.5)
predicted_n_value
table(Actual = train_bcancer1$diagnosis, Predicted = predicted_n_value)
