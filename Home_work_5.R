df=read.csv("~/Загрузки/Книги/Программирование/Теорвер/Задачи/fancy.csv",header = TRUE)
fun=function(n,j){
  p=c(1:n)
  for(i in 0:(n-1)) { p[i+1]=ifelse(i%%12==j-1,1,0) }
  return(p)
}
sales=c()
sales=as.numeric(labels(df)[[1]][1:dim(df)[1]-1])
time=c()
time=as.numeric(labels(sales))
time[85:92]=85:92
sales[85:92]=NA
plot(time, sales, type = "l")

n=(length(time))/12
month.01 <- fun(length(time),1)
month.02 <- fun(length(time),2)
month.03 <- fun(length(time),3)
month.04 <- fun(length(time),4)
month.05 <- fun(length(time),5)
month.06 <- fun(length(time),6)
month.07 <- fun(length(time),7)
month.08 <- fun(length(time),8)
month.09 <- fun(length(time),9)
month.10 <- fun(length(time),10)
month.11 <- fun(length(time),11)
month.12 <- fun(length(time),12)

####Находим модель для логарифма
log.sales=c()
log.sales=log(sales)
plot(time,log.sales, type = "l")
log.sales[85:92]=NA

df=data.frame(log.sales, time, month.01, month.02, month.03, 
              month.04, month.05, month.06, month.07, month.08, month.09, month.10, 
              month.11, month.12)

lin=lm(log.sales~time+month.02+month.03+month.04+month.05+month.06+month.07+
         month.08+month.09+month.10+month.11+month.12, df)
sum_lin=summary(lin)
log.sales2=c()
###Для красоты:
#log.sales2[1:84]=lin$fitted.values
#log.sales2[85]=7.60566+85*0.02239
#log.sales2[86]=7.60566+86*0.02239+0.25109
#log.sales2[87]=7.60566+87*0.02239+0.69527
#log.sales2[88]=7.60566+88*0.02239+0.38301
#log.sales2[89]=7.60566+89*0.02239+0.40804
#log.sales2[90]=7.60566+90*0.02239+0.44701
#log.sales2[91]=7.60566+91*0.02239+0.60829
#log.sales2[92]=7.60566+92*0.02239+0.58544
###########
#6
###########
###Вспоминаем, что получилось в модели.
lin=lm(log.sales~time+month.02+month.03+month.04+month.05+month.06+
         month.07+month.08+month.09+month.10+month.11+month.12)
sum_lin=summary(lin)

###Наблюдений 84, а перменных 12. Для 84 наблюдений должно быть примерно
###три переменные, чего в данном случае добится практически невозможно, но все же
###уменьшить их кол-во можно.
###Подозрение падает на 7-8 месяцы, возьмем за базис 7.

lin=lm(log.sales~time+month.01+month.02+month.03+month.04+month.05+month.06+
         month.08+month.09+month.10+month.11+month.12)
sum_lin=summary(lin)


No_winter=month.06+month.07+month.08+month.09+month.10
lin=lm(log.sales~time+month.02+month.03+month.04+month.05+No_winter+month.11+month.12)
sum_lin=summary(lin)

lin=lm(log.sales~time+month.02+month.03+month.01+month.05+No_winter+month.11+month.12)
sum_lin=summary(lin)

May=month.05+month.04
Jan=month.01+month.02
lin=lm(log.sales~time+Jan+month.03+May+No_winter+month.11)
sum_lin=summary(lin)

log.sales2=predict.lm(lin,df)
plot(time,log.sales2,type = "l")
lines(log.sales, col="yellow")


plot(lin)
hist(lin$residuals)
shapiro.test(lin$residuals)
hist(rstudent(lin))
shapiro.test(rstudent(lin))
cook.d=cooks.distance(lin)
plot(cook.d,ylab="cook.d")
abline(h=1/84, col="yellow")
abline(h=1, col="red")
seas.2 <- rep(1:12, 16)[1:84]
boxplot(rstudent(lin)~seas.2)


library(car)
durbinWatsonTest(lin, method="normal")

###Есть зависимость, но ничего не поделаешь.

####Конец.
sales2=exp(log.sales2)
plot(time,sales2,type = "l")
lines(sales, col="yellow")
df=data.frame(sales, sales2, time, month.01, month.02, month.03, 
              month.04, month.05, month.06, month.07, month.08, month.09, month.10, 
              month.11, month.12)
df[,1:2]
f=function(x){return(abs(x[1]-x[2]))}
r=apply(df[,1:2],1,f)
r=mean(r[!is.na(r)])
r0=mean(sales[!is.na(sales)])
error=r/r0*100


######
##7
######
df=read.csv("~/Загрузки/Книги/Программирование/Теорвер/Задачи/fancy.csv",header = TRUE)
sales1=c()
sales1=as.numeric(labels(df)[[1]][1:dim(df)[1]-1])
time=c()
time=as.numeric(labels(sales1))
df=data.frame(time,sales1)

sales1=ts(df[,2], frequency = 12)
ser.g.HW <- HoltWinters(sales1, seasonal = "mult")

ser.g.HW$fitted
plot(ser.g.HW)
plot(fitted(ser.g.HW))
# ??? 5. ???????????. ?? ????????? ????????, ???????? ??? ???.
# ??????????? ?? ???
ser.g.predict <- predict(ser.g.HW, n.ahead=8)
ser.g.predict
# ??? 6. ????? ?????? ????????? ????, ???????? ? ????????
# ???????????, ????? ????????? ????
plot2=ts.plot(sales1, ser.g.predict, ser.g.HW$fitted[,1], col=c("blue", "red", "orange"))
time=1:92
plot(time,sales2,type = "l")
lines(sales, col="yellow")