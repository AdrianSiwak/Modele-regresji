dane<-read.table("lab1.txt",sep="",header=TRUE)

#zad1
##############

#średnia
x_mean = mean(dane[,1])
y_mean = mean(dane[,2])

#wariancja
x_var = var(dane[,1])
y_var = var(dane[,2])

#odchylenie standardowe
x_sd = sd(dane[,1])
y_sd = sd(dane[,2])

#mediana
x_median = median(dane[,1])
y_median = median(dane[,2])

#pierwszy kwantyl
x_quanrtiles = quantile(dane[,1],c(0.25,.75))
y_quanrtiles = quantile(dane[,2],c(0.25,.75))

#minimum
x_min = min(dane[,1])
y_min = min(dane[,2])

#maksimum
x_max = max(dane[,1])
y_max = max(dane[,2])

#histogram
hist(dane$x,main="histogram zmiennej objaśniającej x",col = "green")
hist(dane$y,main="histogram zmiennej objaśnianej y",col = "red")

#box-plot
boxplot(dane$x,main="box-plot zmiennej objaśniającej x",col = "green")
boxplot(dane$y,main="box-plot zmiennej objaśnianej y",col = "red")

#zad2
##########
plot(dane$x,dane$y,xlab="zmienna objaśniająca x", ylab="zmienna objaśniana y", pch=19)

cor(dane$x,dane$y)

#zad3
################
x<-dane$x
y<-dane$y
model<-lm(y~x)
b_0=model$coefficients[1]
b_1=model$coefficients[2]

#zad4
##############
y_hat=predict(model,newdata=list(dane$x))

sigma_squared_hat=var(model$residuals)

#zad5
####################
summary(model)

#zad6
###################
confint(model,level=0.99)

#zad7
################
x_0<-data.frame(x=1)
predict(model,newdata=x_0,interval=c("confidence"),level=0.99)

#zad8
#################

hist(model$residuals)

qqnorm(model$residuals)

plot(y_hat,model$residuals)

plot(y,model$residuals)

#zad9
###################
dane2=dane
dane2[100,1]=6.9347
dane2[100,2]=148.6400

x2<-dane2$x
y2<-dane2$y
model<-lm(y2~x2)
b_0_2=model$coefficients[1]
b_1_2=model$coefficients[2]

(b_0_2-b_0)
(b_1_2-b_1)

#zad10
################

#a
b0=rep(1,100)
x_new<-runif(100)
errors<-rnorm(100,0,0.1)
y_new<-b0+2*x_new+errors

#b
plot(x_new,y_new)

#c
model<-lm(y_new~x_new)
b_0_new=model$coefficients[1]
b_1_new=model$coefficients[2]

#d

#a
b0=rep(1,100)
x_new<-runif(100)
errors<-rnorm(100,0,0.2)
y_new<-b0+2*x_new+errors

#b
plot(x_new,y_new)

#c
model<-lm(y_new~x_new)
b_0_new=model$coefficients[1]
b_1_new=model$coefficients[2]
