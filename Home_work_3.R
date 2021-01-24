setwd("~/Загрузки/Книги/Программирование/Теорвер/Zadachi_na_klasterizatsiyu/08_Сегментация покупателей")
Edmins=read.csv("Покупатели for spss.csv",header = T, sep = ";",fileEncoding = "UCS-2LE")
Edmins_Up=Edmins
Edmins$пол=scale(Edmins$пол)
Edmins$Профессия=scale(Edmins$Профессия)
Edmins$Телеканал=scale(Edmins$Телеканал)
Edmins$возраст=scale(Edmins$возраст)
Edmins$Пресса=scale(Edmins$Пресса)

#d_max=dist(Edmins, method = "maximum", diag = FALSE, upper = FALSE, p = 2)
d_man=dist(Edmins, method = "manhattan", diag = FALSE, upper = FALSE, p = 2)
#d_can=dist(Edmins, method = "canberra", diag = FALSE, upper = FALSE, p = 2)
#d_euc=dist(Edmins, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)


#h_max=hclust(d_max, method = "mcquitty", members = NULL)
h=hclust(d_man, method = "ward.D2", members = NULL)
#h_can=hclust(d_can, method = "ward.D2", members = NULL)
#h_euc=hclust(d_euc, method = "ward.D2", members = NULL)

#p_max=plot(rev(h_max$height)[1:15], type="b", xlab = "Cluster number", ylab = "maximum")
p_man=plot(rev(h$height)[1:15], type="b", xlab = "Cluster number", ylab = "manhattan")
#p_can=plot(rev(h_can$height)[1:15], type="b", xlab = "Cluster number", ylab = "canberra")
#p_euc=plot(rev(h_euc$height)[1:15], type="b", xlab = "Cluster number", ylab = "euclidean")

plot(h, hang = 1)
rect.hclust(h,3, border="red")

plot(cmdscale(d_man), col = Edmins$clusters, xlab = "Index", ylab = "Y")

c=cutree(h,3)
Edmins$clusters=as.factor(c)
Edmins_Up$clusters=as.factor(c)
Edmins1=subset(Edmins_Up,Edmins_Up$clusters=="1")
Edmins2=subset(Edmins_Up,Edmins_Up$clusters=="2")
Edmins3=subset(Edmins_Up,Edmins_Up$clusters=="3")



p=c(1:(dim(Edmins)[2]-1))

for(i in 1:(dim(Edmins)[2]-1)){
  p[i]=summary(aov(Edmins[,i]~Edmins$clusters,Edmins))[[1]][1,'Pr(>F)']
}





wss <- (nrow(Edmins[,1:5]) - 1) * sum(apply(Edmins[,1:5],2,var))
for (i in 2:9) wss[i] <- kmeans(Edmins[,1:5], 
                                 centers=i, nstart=20)$tot.withinss
plot(1:9, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 
kmean=kmeans(Edmins[,1:5], 
             centers=3, nstart=20)
kmean$centers
kmean$tot.withinss
kmean$withinss
plot(cmdscale(d_man), col = kmean$cluster, xlab = "Index", ylab = "Y")

Edmins_km=Edmins
Edmins_Up_km=Edmins_Up
Edmins_km$clusters=as.factor(kmean$cluster)
Edmins_Up_km$clusters=as.factor(kmean$cluster)
Edmins1=subset(Edmins_Up_km,Edmins_km$cluster=="1")
Edmins2=subset(Edmins_Up_km,Edmins_km$cluster=="2")
Edmins3=subset(Edmins_Up_km,Edmins_km$cluster=="3")

p=c(1:(dim(Edmins)[2]-1))

for(i in 1:(dim(Edmins)[2]-1)){
  p[i]=summary(aov(Edmins[,i]~Edmins$clusters,Edmins))[[1]][1,'Pr(>F)']
}


table(Edmins$clusters,Edmins_km$clusters)
