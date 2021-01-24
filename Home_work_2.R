#Изучить влияние престижности района(NE) и типа постройки(CUST) на площадь жилья(SQFT) (диспер-
#сионный анализ).
#Данные находятся в папке Albuquerque.
library(ggplot2)
library(caret)
library(e1071)
library(car)

Albuq=read.table("~/Загрузки/Книги/Программирование/Теорвер/Zadachi/Albuquerque/Albuquerque Home Prices_data.txt",header = T)
Albuq_Up=data.frame(SQFT=Albuq$SQFT,NE=as.factor(Albuq$NE),CUST=as.factor(Albuq$CUST))
str(Albuq_Up)

#Графики
ggplot(Albuq_Up,aes(Albuq_Up$NE,Albuq_Up$SQFT))+
  geom_boxplot(aes(col=Albuq_Up$CUST))
ggplot(Albuq_Up,aes(Albuq_Up$NE,Albuq_Up$SQFT))+
  stat_summary(aes(col=Albuq_Up$CUST))
ggplot(Albuq_Up,aes(Albuq_Up$SQFT))+
  geom_density()
ggplot(Albuq_Up,aes(Albuq_Up$SQFT,fill=NE:CUST))+
  geom_density(alpha=0.5)

#Проверка на нормальность
shtest_00=shapiro.test(x = Albuq_Up$SQFT[Albuq_Up$NE=="0"&Albuq_Up$CUST=="0"])
shtest_01=shapiro.test(x = Albuq_Up$SQFT[Albuq_Up$NE=="0"&Albuq_Up$CUST=="1"])
shtest_10=shapiro.test(x = Albuq_Up$SQFT[Albuq_Up$NE=="1"&Albuq_Up$CUST=="0"])
shtest_11=shapiro.test(x = Albuq_Up$SQFT[Albuq_Up$NE=="1"&Albuq_Up$CUST=="1"])

#Преобразование Бокса Кокса
Lambda_Albuq_Up=BoxCoxTrans(Albuq_Up$SQFT)
BoxCox_Albuq_Up=predict(Lambda_Albuq_Up,Albuq_Up$SQFT)
Albuq_Up_BoxCox=data.frame(SQFT=BoxCox_Albuq_Up,NE=Albuq_Up$NE,CUST=Albuq_Up$CUST)

#Графики для новых данных
ggplot(Albuq_Up_BoxCox,aes(Albuq_Up_BoxCox$SQFT))+
  geom_density()
ggplot(Albuq_Up_BoxCox,aes(Albuq_Up_BoxCox$SQFT,fill=NE:CUST))+
  geom_density(alpha=0.5)

#Проверка на нормальность для новых данных
shtest_00=shapiro.test(x = Albuq_Up_BoxCox$SQFT[Albuq_Up_BoxCox$NE=="0"&Albuq_Up_BoxCox$CUST=="0"])
shtest_01=shapiro.test(x = Albuq_Up_BoxCox$SQFT[Albuq_Up_BoxCox$NE=="0"&Albuq_Up_BoxCox$CUST=="1"])
shtest_10=shapiro.test(x = Albuq_Up_BoxCox$SQFT[Albuq_Up_BoxCox$NE=="1"&Albuq_Up_BoxCox$CUST=="0"])
shtest_11=shapiro.test(x = Albuq_Up_BoxCox$SQFT[Albuq_Up_BoxCox$NE=="1"&Albuq_Up_BoxCox$CUST=="1"])

#Тест на гомогенность дисперсии
disptest_NE=bartlett.test(SQFT ~ NE, Albuq_Up_BoxCox)
disptest_CUST=bartlett.test(SQFT ~ CUST, Albuq_Up_BoxCox)

#Дисперсионный анализ
aov_Albuq_Up_BoxCox=aov(SQFT ~ NE*CUST, data=Albuq_Up_BoxCox)
summary(aov_Albuq_Up_BoxCox)
