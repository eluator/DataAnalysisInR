#Сравнить средний процент брака на 1 и 5 заводах (t-тест)
#Данные находятся в папке Waste Run Up
library(ggplot2)
library(caret)
library(e1071)
library(car)
#Считывание данных из таблицы Waste Run Up
WasteRunUp=read.csv("~/Загрузки/Книги/Программирование/Zadachi/Waste Run Up/Waste Run Up_data2.csv",dec=",")
WasteRunUp_15=data.frame(PT1=WasteRunUp$PT1,PT5=WasteRunUp$PT5)
#Преобразование в дата фрейм с факторной переменной PT
PT1=c(1:22)
PT5=c(1:22)
f1=function(x){x="PT1"}
f5=function(x){x="PT5"}
PT1=sapply(PT1,f1)
PT5=sapply(PT5,f5)
PT_pos=as.factor(c(PT1[WasteRunUp_15$PT1 >= 0],PT5[WasteRunUp_15$PT5 >= 0]))
PT=as.factor(c(PT1,PT5[WasteRunUp_15$PT5 != -9999.0]))
#WasteRunUp_15_Update_pos=data.frame(Num=c(WasteRunUp_15$PT1[WasteRunUp_15$PT1 >= 0],WasteRunUp_15$PT5[WasteRunUp_15$PT5 >= 0]),PT=PT_pos)
WasteRunUp_15_Update=data.frame(Num=c(WasteRunUp_15$PT1,WasteRunUp_15$PT5[WasteRunUp_15$PT5 != -9999.0]),PT=PT)

#Сдвигаем графики так, чтобы были только положительные значения
min_WasteRunUp_15_Update=min(WasteRunUp_15_Update$Num)
WasteRunUp_15_Update$Num=WasteRunUp_15_Update$Num-min_WasteRunUp_15_Update+1

#Графики
boxplot_WRU_15_Up=ggplot(WasteRunUp_15_Update, aes(PT,Num))+
  geom_boxplot()

hist_WRU_15_Up=ggplot(WasteRunUp_15_Update, aes(Num))+
  geom_histogram(fill="pink",col="yellow",binwidth = 5.0)+
  facet_grid(PT ~ .)

density_WRU_15_Up=ggplot(WasteRunUp_15_Update, aes(Num,fill=PT))+
  geom_density(alpha=0.5)
density_WRU_15_Up_Num=ggplot(WasteRunUp_15_Update, aes(Num))+
  geom_density()

#Тест на нормальность распределения
shtest_PT1=shapiro.test(x = WasteRunUp_15_Update$Num[PT=="PT1"])
shtest_PT5=shapiro.test(x = WasteRunUp_15_Update$Num[PT=="PT5"])

#Преобразование Бокса Кокса
Lambda_WasteRunUp_15_Update=BoxCoxTrans(WasteRunUp_15_Update$Num)
BoxCox_WasteRunUp_15_Update=predict(Lambda_WasteRunUp_15_Update,WasteRunUp_15_Update$Num)
WasteRunUp_15_Update_BoxCox=data.frame(Num=BoxCox_WasteRunUp_15_Update,PT=WasteRunUp_15_Update$PT)

#Графики для Бокса Кокса
boxplot_WRU_15_Up_BoxCox=ggplot(WasteRunUp_15_Update_BoxCox, aes(PT,Num))+
  geom_boxplot()
hist_WRU_15_Up_BoxCox=ggplot(WasteRunUp_15_Update_BoxCox, aes(Num))+
  geom_histogram(fill="white",col="black",binwidth = predict(Lambda_WasteRunUp_15_Update,5.0))+
  facet_grid(PT ~ .)
density_WRU_15_Up_BoxCox=ggplot(WasteRunUp_15_Update_BoxCox, aes(Num,fill=PT))+
  geom_density(alpha=0.5)
density_WRU_15_Up_Num_BoxCox=ggplot(WasteRunUp_15_Update_BoxCox, aes(Num))+
  geom_density(alpha=0.5)

#Тест на нормальность распределения для Бокса Кокса
shtest_BoxCox_PT1=shapiro.test(x = WasteRunUp_15_Update_BoxCox$Num[PT=="PT1"])
shtest_BoxCox_PT5=shapiro.test(x = WasteRunUp_15_Update_BoxCox$Num[PT=="PT5"])

#U-test, Критерий Манна-Уитни
Utest_WasteRunUp_15_Update_BoxCox=wilcox.test(Num ~ PT,WasteRunUp_15_Update_BoxCox, paired = FALSE)
#Тест на гомогенность дисперсии
disptest=bartlett.test(Num ~ PT, WasteRunUp_15_Update_BoxCox)

#t-test, Критерий Стьюдента
ttest=t.test(Num ~ PT,WasteRunUp_15_Update_BoxCox,var.equal = ifelse(disptest$p.value>=0.05,T,F))
