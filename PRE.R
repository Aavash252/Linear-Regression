df<-read.csv("house_price.csv",header=T)

str(df)
summary(df)
hist(df$crime_rate)
pairs(~price+crime_rate+n_hot_rooms+rainfall,data=df)
barplot(table(df$airport))
barplot(table(df$waterbody))
barplot(table(df$bus_ter))


uv=3*quantile(df$n_hot_rooms,0.99)
df$n_hot_rooms[df$n_hot_rooms>uv]<-uv

summary(df$n_hot_rooms)

lv=0.3*quantile(df$rainfall,0.01)
df$rainfall[df$rainfall<lv]<-lv
summary(df$rainfall)

mean(df$n_hos_beds,na.rm=T)

which(is.na(df$n_hos_beds))
df$n_hos_beds[is.na(df$n_hos_beds)]<-mean(df$n_hos_beds,na.rm=T)



pairs(~price+crime_rate,data=df)
plot(df$price,df$crime_rate)

df$crime_rate=log(1+df$crime_rate)
plot(df$price,df$crime_rate)

df$avg_dist=(df$dist1+df$dist2+df$dist3+df$dist4)/4



df<-df[,-7:-10]

df<-df[,-14]


df<-dummy.data.frame(df)
df<-df[,-9]
df<-df[,-14]



cor(df)

round(cor(df),2)
df<-df[,-16]


simple_model<-lm(price~room_num,df)
 

set.seed(0)
split=sample.split(df,SplitRatio = 0.8)

train_set=subset(df,split==TRUE)
test_set=subset(df,split==FALSE)


lm_a=lm(price~.,data=train_set)

train_a=predict(lm_a,train_set)
test_a=predict(lm_a,test_set)


mean((train_set$price-train_a)^2)
mean((test_set$price-test_a)^2)

lm_best=regsubsets(price~.,data=df,nvmax = 15)

summary(lm_best)
summary(lm_best)$adjr2

which.max(summary(lm_best)$adjr2)

coef(lm_best,8)

lm_forward=regsubsets(price~.,data=df,nvmax = 15,method = "forward")

summary(lm_forward)

lm_backward=regsubsets(price~.,data=df,nvmax = 15,method = "backward")

summary(lm_backward)



x<-model.matrix(price~.,data=df)[,-1]
y<-df$price

grid=10^seq(10,-2,length=100)

lm_ridge=glmnet(x,y,alpha=0,lambda = grid)

summary(lm_ridge)

cv_fit=cv.glmnet(x,y,alpha=0,lambda = grid)

plot(cv_fit)

opt_lambda<-cv_fit$lambda.min
tss<-sum((y-mean(y))^2)

y_a<-predict(lm_ridge,s=opt_lambda,newx=x)

rss<-sum((y_a-y)^2)

rsq<-1-rss/tss






