#setwd("~/Datasets")
beer <- read.csv("consumo_cerveja.csv",
                 stringsAsFactors = FALSE, sep = ",",
                 dec=",",nrows=365)
# rename the variables
beer$date <- beer$Data
beer$temp_median_c <- beer$Temperatura.Media..C.
beer$temp_min_c <- beer$Temperatura.Minima..C.
beer$temp_max_c <- beer$Temperatura.Maxima..C.
beer$precip_mm <- beer$Precipitacao..mm.
beer$weekend <- factor(beer$Final.de.Semana)
beer$beer_cons_liters <- as.numeric(beer$Consumo.de.cerveja..litros.)
beer <- beer[ , 8:ncol(beer)]
################################################2################################
#Model Fit
beer_model <- lm(beer_cons_liters~ weekend+ precip_mm+ temp_median_c, data=beer)
summary(beer_model)

## Check for linearity:
# (1) Weekend variable
ggplot(beer,aes(x=weekend, y=beer_model$residual)) +
  geom_point(alpha = .7) + geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs Weekends",x="Weekend",y="Residuals")

# (2) Precip variable (VIOLATED)
ggplot(beer,aes(x=precip_mm, y=beer_model$residual)) +
  geom_point(alpha = .7) + geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs precip_mm",x="precip_mm",y="Residuals")

# (3) Temp Median variable
ggplot(beer,aes(x=temp_median_c, y=beer_model$residual)) +
  geom_point(alpha = .7) + geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs temp_median_c",x="temp",y="Residuals")

## Checking for equal variance and independence:
plot(beer_model,which=1,col=c("blue4"))

## Checking for normality of residuals:
plot(beer_model,which=2,col=c("blue4"))

## Transformation:
beer$log_consp <- log(beer$beer_cons_liters)
hist(beer$log_consp)
hist(beer$beer_cons_liters)

#NEW MODEL
log_beer_model <- lm(log_consp~ weekend+ precip_mm+ temp_median_c, data=beer)
summary(beer_model)

confint(log_beer_model,level = 0.95)
confint(beer_model,level = 0.95)

## Check for linearity (new model):
# (1) Weekend variable
ggplot(beer,aes(x=weekend, y=log_beer_model$residual)) +
  geom_point(alpha = .7) + geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs Weekends",x="Weekend",y="Residuals")

# (2) Precip variable (appears VIOLATED, is it tho????)
ggplot(beer,aes(x=precip_mm, y=log_beer_model$residual)) +
  geom_point(alpha = .7) + geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs precip_mm",x="precip_mm",y="Residuals")

# (3) Temp Median variable
ggplot(beer,aes(x=temp_median_c, y=log_beer_model$residual)) +
  geom_point(alpha = .7) + geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs temp_median_c",x="temp",y="Residuals")

## Checking for equal variance and independence:
plot(log_beer_model,which=1,col=c("blue4"))

## Checking for normality of residuals:
plot(log_beer_model,which=2,col=c("blue4"))
################################################################3###############################
#factoring precip_mm: 0 no rain, 1 light rain, 2 heavy rain
rt<-99
rt[beer$precip_mm>30]<-2
rt[beer$precip_mm<=30 & beer$precip_mm>0]<-1
rt[beer$precip_mm<=0]<-0
beer$rain<-factor(rt)


#Model Fit 
beer_model <- lm(log(beer_cons_liters)~ weekend+ new+ temp_median_c, data=beer)
summary(beer_model)
ggplot(beer_model,aes(x=temp_median_c, y=beer_model$residual))+geom_point()+geom_hline(yintercept = 0,col="red3")
par(mfrow=c(1,2))
plot(beer_model,which=2,col=c("blue4"))

#checking for independence vs variance
plot(beer_model,which=5,col=c("blue4"))

lev_scores<-hatvalues(beer_model)
plot(lev_scores)

rmse<-sqrt(mean(beer_model$residuals^2))
rmse

set.seed(128) # use whatever number you want
# Now randomly re-shuffle the data
beer1 <- beer[sample(nrow(beer)),]
# Define the number of folds you want
K <- 10
# Define a matrix to save your results into
RMSE <- matrix(0,nrow=K,ncol=1)
# Split the row indexes into k equal parts
kth_fold <- cut(seq(1,nrow(beer1)),breaks=K,labels=FALSE)
# Now write the for loop for the k-fold cross validation
for(k in 1:K){
  # Split your data into the training and test datasets
  test_index <- which(kth_fold==k)
  train <- beer1[-test_index,]
  test <- beer1[test_index,]
  # Now that you've split the data,
  trainmodel<-lm(log(beer_cons_liters)~weekend+temp_median_c+new,data=train)
  y_test_pred<-predict(trainmodel,test)
  #temp<-cbind(test$beer_cons_liters,y_test_pred)[1:5,]
  RMSE[k,]<-sqrt(mean((test$beer_cons_liters-exp(y_test_pred))^2))
}
mean(RMSE)

###########################################################4##################################


