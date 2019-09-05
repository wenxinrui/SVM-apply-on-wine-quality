library(ISLR)
library(tidyverse)
library(corrplot)
wine = read.csv("winequality-red.csv")
summary(wine)
hist(wine$quality, main = 'Distribution of Wine Quality', xlab='quality', ylab = 'count', breaks=c(2.5,3.5,4.5,5.5,6.5,7.5,8.5), xlim=c(0,10),cex.axis=1.5,cex.lab=1.5,cex.main=1.5)

wine_class = wine %>% mutate(quality.class=ifelse(quality>6, 'Good', ifelse(quality<5, 'Bad','Medium'))) %>% select(-quality)
corrplot(cor(wine),method = "circle", tl.cex = 0.7)
library(cluster)
data=wine
########################################################################################################################
#Specify a MAXIMUM number of clusters for kmeans
MAX_K = 10
wss = numeric(MAX_K)

for(k in 1: MAX_K){
  wss[k] = kmeans(data, k)$tot.withinss
}


#view the WSS plot
plot(1:MAX_K, wss, pch = 20, type = "o",xlab="Number of Clusters (K)",ylab="Within Sum of Squares (WSS)", main = "Total WSS against K")

#########################################################################################################################
#We shall now perform a silhouette Analysis to find the optimum K. Note that 
#there is no silhouette width for K=1 as there are no neighbouring clusters

avgwidths = numeric(MAX_K)

for(k in 2: MAX_K){
  pam = pam(data,k)
  avgwidth = pam$silinfo$avg.width
  avgwidths[k] = avgwidth
}

#View the average silhouette width plot:
plot(1:MAX_K, avgwidths, pch = 20, type = "o", main = "Average Silhouette Width against K")
# vary parameters for most readable graph


kmeans_data_2= kmeans(data, 2)

kmeans_data_2

#We add a new column to the data which is the new cluster it belongs to.
data_new_2 =   data %>% mutate(cluster = factor(kmeans_data_2$cluster))


#########################################################################################################################
#Summary statistics of the clustered data. We find the means of the individual 
#clusters and categories to examine the average trend of each category 
#for each group

summary_stat = data_new_2 %>%
  group_by(cluster) %>%
  summarise(avg_fixed.acidity = mean(fixed.acidity),
            avg_volatile.acidity = mean(volatile.acidity),
            avg_citric.acid = mean(citric.acid),
            avg_residual.sugar = mean(residual.sugar),
            avg_chlorides = mean(chlorides),
            avg_free.sulfur.dioxide = mean(free.sulfur.dioxide),
            ave_total.sulfur.dioxide = mean(total.sulfur.dioxide),
            ave_density = mean(density),
            ave_pH = mean(pH),
            ave_sulphates = mean(sulphates),
            ave_alcohol = mean(alcohol),
            ave_quality = mean(quality),
            n_markets = n()
  )
summary_stat

library(nnet)
set.seed(2019)
wine_class
train = sample(1:nrow(wine_class), nrow(wine_class)*0.8)

#################################
#logistic
View(wine_class)
train_data=wine[train,]
test_data=wine[-train,]
logistic_all = glm(quality ~.,data = train_data)
summary(logistic_all)
remove_least_sig_formula = function(glmobject, response){
  least_sig_name = names(which.max(summary(glmobject)$coefficients[,4][c(-1)]))
  allnames = names(summary(glmobject)$coefficients[,4])[-c(1)]
  cat("Removing: " , least_sig_name , "\n")
  as.formula(paste(response, paste(allnames[allnames != least_sig_name], collapse = "+"),sep ="~"))
}
is_all_sig = function(glmobject, sig){
  all(summary(glmobject)$coefficients[,4][-c(1)] < sig)
}
sig_removal = function(glmobject, response, sig){
  model = glmobject
  while(!is_all_sig(model, sig)){
    formula = remove_least_sig_formula(model, response)
    model = glm(data = train_data, formula)
  }
  summary(model)
  model
}
# We set a 0.05 (1%) level of significance.
level = 0.05
model_imp = sig_removal(logistic_all, "quality", level)
summary(model_imp)
#all significant at 0.05 level
model=model_imp
fitted.results = predict(model,type = "response", test_data)
confusionMatrixDiagnostic = function(model, test_data){
  fitted.results = predict(model,type = "response", test_data)
  fitted.results.category = ifelse(fitted.results > 0.5, 1,0)
  confusionMatrix(factor(fitted.results.binary), factor(test_data$Churn))
}
confusionMatrixDiagnostic(churn_model_imp,test_data)

glm.fit = multinom(quality.class~., data=train_data)
summary(glm.fit)

data=wine
data$quality <- factor(data$quality)
temp = glm(quality ~.,data = data[train,])
################################################
################################################
################################################

####SVM
library(e1071)
wine_class$quality.class <- factor(wine_class$quality.class)
set.seed(1)
svmfit=svm(quality.class~.,data=wine_class,subset=train, kernel="radial",  gamma=1, cost=1)
summary(svmfit)

plot(svmfit,wine_class[train,],fixed.acidity~volatile.acidity)
#can only plot for 2 input variables, not meaning much,so not putting a plot is also ok

tune.out=tune(svm,quality.class~.,data=wine_class[train,],kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
#best parameters: cost=10,gamma=2
#plot(tune.out$best.model, wine_class[train,])

pred=predict(tune.out$best.model,wine_class[-train,])
table(true=wine_class[-train,]$quality.class, pred)

length(pred)
(13+30+1)/(320)
#error rate: 0.1375
(1+16+259)/320
#accuracy=0.8625


