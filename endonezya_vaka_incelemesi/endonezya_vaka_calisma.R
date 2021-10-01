#Endonezya'ya ait 01.01.2020 ile 14.12.2020 tarihleri arasındaki covid-19 vaka sayılarının indonesia_data değişkenine atanması.
install.packages("readxl")
library("readxl")
indonesia_data<-read_excel("/Users/busraozkan/Desktop/indonesia_data.xlsx") 
indonesia_data

#indonesia_data değişkenine ait ortalama, medyan, mod ve çeyrekliklerinin hesaplanması.
mean(indonesia_data$cases)
median(indonesia_data$cases)
mod_cases <- table(indonesia_data$cases)
names(mod_cases)[which(mod_cases==max(mod_cases))]
quantile(indonesia_data$cases)
summary(indonesia_data$cases)

#indonesia_data değişkenine ait varyans,standart sapma ve standart hata değerlerinin hesaplanması.
var(indonesia_data$cases)
sd(indonesia_data$cases)
std_hata<-sd(indonesia_data$cases)/sqrt(length(indonesia_data$cases))
std_hata

#indonesia_data değişkenine ait çarpıklık ve basıklık katsayısının hesaplanması.
install.packages("GLDEX")
library(GLDEX)
skewness(indonesia_data$cases)
kurtosis(indonesia_data$cases)

#indonesia_data değişkenine ait Bowley ve Pearson asimetri katsayılarının hesaplanması.
Q1<-143
Q2<-1217.5
Q3<-3426.25
bowley<-(Q1+Q3-2*Q2)/(Q3-Q1)
bowley
mean<-mean(indonesia_data$cases)
median<-median(indonesia_data$cases)
S<-sd(indonesia_data$cases)
mod<-0
pearson1<-3*(mean-median)/S
pearson1
pearson2<-(mean-mod)/S
pearson2

#indonesia_data değişkeninin zamana karşı değişimini gözlemlemek için trend grafiğinin çizilmesi.
plot(indonesia_data$dateRep,indonesia_data$cases, type="o", main="Endonezya'daki Ocak_Aralık 2020 Covid-19 Vaka Sayısı")

#indonesia_data değişkenine ait boxplot grafiğinin çizilmesi.
boxplot(indonesia_data$cases, col = "red")

#indonesia_data değişkenine dair aykırı değer incelenmesi.
install.packages("qcc",repos = "https://cloud.r-project.org")
library(qcc)
qcc(indonesia_data$cases, type = "xbar.one",std.dev = "SD",nsigmas = 3)
indonesia_data$cases[which(indonesia_data$cases<mean(indonesia_data$cases)-2*sd(indonesia_data$cases))]
indonesia_data$cases[which(indonesia_data$cases>mean(indonesia_data$cases)+2*sd(indonesia_data$cases))]
indonesia_data$cases[which(indonesia_data$cases<mean(indonesia_data$cases)-3*sd(indonesia_data$cases))]
indonesia_data$cases[which(indonesia_data$cases>mean(indonesia_data$cases)+3*sd(indonesia_data$cases))]
indonesia_data$cases[which(abs((indonesia_data$cases-mean(indonesia_data$cases))/sd(indonesia_data$cases))>3)]

#indonesia_data değişkenine ait değişim katsayısının hesaplanması.
degisim_indonesia<-(sd(indonesia_data$cases)/mean(indonesia_data$cases))*100
degisim_indonesia




