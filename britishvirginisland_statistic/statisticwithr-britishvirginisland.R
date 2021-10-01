# British Virgin Island ülkesine ait 1971-2016 yılları arasındaki kentsel nüfus artışı ve CO2 emisyonu ile ilgili verilerin bulunduğu excel dosyasından verilerin R programına aktarılması.

install.packages("readxl")
library("readxl")
british_virgin_island<-read_excel("/Users/busraozkan/Desktop/British_Virgin_Island Verileri.xlsx")
british_virgin_island

# Kentsel nüfus artışı değişkeni X değişkeni (bağımsız değişken), CO2 emisyonu değişkeni Y değişkeni (bağımlı değişken) olarak tanımlanır.
X<-british_virgin_island$X__Urban_population_growth
Y<-british_virgin_island$Y__CO2_emissions
year<-seq(1971,2016,1)


# 1) Değişkenlerin zaman içindeki değişimini gözlemlemek için en uygun grafik trend grafiğidir.
plot(year,X,type = "o",main = "1971-2016 yılları arasında kentsel popülasyonun artış hızı")
plot(year,Y,type = "o",main = "1971-2016 yılları arasında CO2 emisyonu miktarı")

# 2) X değişkenine ait kitle ortalamasının %95 güven düzeyinde güven aralığının bulunması
ort<-mean(X)
ss<-sd(X)
n<-46
alpha<-0.05
z.alpha<-qnorm(1-alpha/2)
tablo_degeri_aralik<- c(-z.alpha,z.alpha)
tablo_degeri_aralik
X_ortalama_guven_aralik<-c(ort-z.alpha*(ss/sqrt(n)),ort+z.alpha*(ss/sqrt(n)))
X_ortalama_guven_aralik

# 3) X değişkenine ait kitle varyansının %90 güven düzeyinde güven aralığının bulunması
varyans<-var(X)
qchisq(c(0.95, 0.05), n-1)
X_varyans_guven_aralik<-(n-1)*varyans/qchisq(c(0.95, 0.05), n-1)
X_varyans_guven_aralik

# 4) Y değişkeninin kitle ortalamasının 5.5 değerine eşit olup olmadığının test edilmesi.
install.packages("BSDA")
library(BSDA)
z.test(Y, mu=5.5, alternative="two.sided", sigma.x=sqrt(var(Y)),conf.level = 0.90)

# 5) X ve Y değişkenleri arasındaki korelasyon katsayısının bulunması

# Değişkenler normal dağılıma uygun olmadığı için Spearman Korelasyon katsayısı kullanılmalı
install.packages("haven", repos='http://cran.us.r-project.org')
library(haven)
cor(X,Y,method = "spearman")
cor.test(X,Y,alternative = "two.sided",method = "spearman")


# 6) X ve Y değişkenleri arasındaki basit doğrusal regresyon denkleminin kurulması

#X ve Y değişkenlerin normallik dağılım durumunu belirlemek için q-q plot grafiğinden faydalanılır.
install.packages("car")
install.packages('carData')
library(carData)
library("car")
qqPlot(X)
qqPlot(Y)
# Belirtme katsayısının belirlenmesi
r<-cor(X,Y)
r
r_kare<-r^2
r_kare
#Regresyon analizi
regresyon_denklem<-lm(Y ~ X )
summary(regresyon_denklem)
# Anlamlılık düzeyinin test edilmesi
confint(regresyon_denklem,level = .95)





