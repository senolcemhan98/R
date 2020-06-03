veri = read.csv2("uyku.csv")

#Kullanmadigimiz sutunlari kaldirdik.
veri = read.csv2("uyku.csv")
keeps = c('cinsiyet','medeni.hal','egitim.seviyesi','sigara.icme','yas.uc.grup','olculen.uyku.kalitesi','uykuya.dalma.guclugu',
          'epworth.uyku.olcegi','HADS.anksiyete','HADS.depresyon','hafta.ici.uyku.suresi','hafta.sonu.uyku.suresi')
df= veri[,keeps]
str(df)
summary(df)

#Soru-1#
#cinsiyet,medeni.hal,egitim.seviyesi,sigara.icme,yas.uc.grup,olculen.uyku.kalitesi degiskenleri icin frekans tablosu olustur.

#1_1 Cinsiyet Frekans Tablosu 

library(ggplot2)
print(table(df[,c('cinsiyet')])) # 121 Erkek - 150 Kadın
#Gorsellestirme
g <- ggplot(df, aes(cinsiyet))
g + geom_bar()

#1_2 Medeni Hal Frekans Tablosu
print(table(df[,c('medeni.hal')])) # 54 Bekar - 21 Bosanmis - 8 Dul - 188 Evli
#Gorsellestirme
g <- ggplot(df, aes(medeni.hal))
g + geom_bar()

#1_3 Egitim Seviyesi Frekans Tablosu 
print(table(df[,c('egitim.seviyesi')])) # 3 İlk - 30 Lise - 33 Orta - 71 Universite - 132 Y.lisans
#Gorsellestirme
g <- ggplot(df, aes(egitim.seviyesi))
g + geom_bar()

#1_4 Sigara İcme Frekans Tablosu
print(table(df[,c('sigara.icme')])) # 34 Evet - 236 Hayir 
#Gorsellestirme
g <- ggplot(df, aes(sigara.icme))
g + geom_bar()

#1_5 Yas Uc Grup Frekans Tablosu
print(table(df[,c('yas.uc.grup')])) # <37 (83) - 38-50 (86) - 51+ (79)
#Gorsellestirme
g <- ggplot(df, aes(yas.uc.grup))
g + geom_bar()

#1_6 Olculen Uyku Kalitesi Frekans Tablosu
print(table(df[,c('olculen.uyku.kalitesi')])) # 67 Cokiyi,mukemmel - 36 cok kotu,kotu - 90 iyi - 75 Orta
#Gorsellestirme
g <- ggplot(df, aes(olculen.uyku.kalitesi))
g + geom_bar()

#### Soru_1 TAMAMLANDI ####


#soru-2#
#Medeni Hal ile Egitim Seviyesi degiskenlerine ait capraz tabloyu olusturup iliski var midir yok mudur gucu nedir yorumlayiniz.
mh_es_capraz_tablo = table(df$medeni.hal,df$egitim.seviyesi)
print(mh_es_capraz_tablo)   
#                                       ilk lise orta universite y.lisans
#                             bekar      1    6   11         21       15
#                             bosanmis   0    1    1          7       12
#                             dul        1    2    2          1        1
#                             evli       1   21   19         42      104

#Gorsellestirme 1
es = factor(df$egitim.seviyesi)
t = table(es,veri$medeni.hal)
barplot(t,col = c("red","white","green","blue","yellow"),beside = T,legend = rownames(t))
#Gorsellestirme 2
library(ggplot2)
library(dplyr)
df%>%
  ggplot(aes(x=egitim.seviyesi,fill=medeni.hal))+geom_bar()

# Iliskiyi incelemk icin Ki-Kare Testi yapalim

library(MASS)
chisq.test(mh_es_capraz_tablo)
#X-squared = 34.361, df = 12, p-value = 0.0005915
#     [Chi-squared approximation may be incorrect] 

#Onerilen Parametre ile Test
chisq.test(mh_es_capraz_tablo,simulate.p.value = TRUE)
#X-squared = 34.361, df = NA, p-value = 0.004498

#Hipotezler ;  Ho:Kişinin egitim duzeyi ile medeni hali bagimsizdir.
#              H1:Kişinin egitim duzeyi ile medeni hali bagimsiz degildir.Egitim duzeyi degistikce medeni hal degisir.

# p_value'lar 0.05'ten kucuk oldugu icin Ho Red.

#Iliskinin gucunun belirlenmesi icin Cramer degerini elde edelim.
library(vcd)
istatistik <- assocstats(mh_es_capraz_tablo)
print(istatistik$cramer)
# Cramer degeri [0,1] arasında deger alır 1 e ne kaddar yakınsa o kadar gucludur.
# 0.2063461 degeri 1 e cok uzaktır gucunun fazla olmadigi soylenebilir.


#### Soru_2 TAMAMLANDI ####
            
#Soru_3#
#Medeni Hal ile Uykuya Dalma Guclugu degiskenlerine ait capraz tabloyu olusturup iliski var midir yok mudur gucu nedir yorumlayiniz.

mh_udg_capraz_tablo = table(df$medeni.hal,df$uykuya.dalma.guclugu)
print(mh_udg_capraz_tablo) 
#           evet hayir
#bekar      30    24
#bosanmis    8    13
#dul         5     3
#evli       63   123

#Gorsellestirme 1
es = factor(df$uykuya.dalma.guclugu)
t = table(es,df$medeni.hal)
barplot(t,col = c("green","red"),beside = T,legend = rownames(t))

#Gorsellestirme 2
df%>%
  ggplot(aes(x=medeni.hal,fill=uykuya.dalma.guclugu))+geom_bar()



#Iliskiyi incelemek icin Ki-Kare Testi yapalim
library(MASS)
chisq.test(mh_udg_capraz_tablo)
#X-squared = 10.087, df = 3, p-value = 0.01784
#     [Chi-squared approximation may be incorrect] 

#Onerilen Parametre ile Test
chisq.test(mh_udg_capraz_tablo,simulate.p.value = TRUE)
#X-squared = 10.087, df = NA, p-value = 0.01999

#Hipotezler ;  Ho:Kişinin egitim duzeyi ile medeni hali bagimsizdir.
#              H1:Kişinin egitim duzeyi ile medeni hali bagimsiz degildir.Egitim duzeyi degistikce medeni hal degisir.

# p_value'lar 0.05'ten kucuk oldugu icin Ho Red.

#Iliskinin gucunun belirlenmesi icin Cramer degerini elde edelim.
library(vcd)
istatistik <- assocstats(mh_udg_capraz_tablo)
print(istatistik$cramer)
# Cramer degeri [0,1] arasında deger alır 1 e ne kaddar yakınsa o kadar gucludur.
#  0.1936426 degeri 1 e cok uzaktır gucunun fazla olmadigi soylenebilir.

#### Soru_3 TAMAMLANDI ####


#Soru 4#
#epworth.uyku.olcegi,HADS.anksiyete,HADS.depresyon degiskenlerinin normalligini inceleyin normallestirilebilir mi?
keeps2 = c('epworth.uyku.olcegi','HADS.anksiyete','HADS.depresyon')
soru4.df = df[,keeps2]

#Gorsellestirme
for(i in df[,keeps2]){
  qqnorm(i) 
  qqline(i)
}


#Normallik inceleme
for(j in 1:ncol(soru4.df)) { 
  degisken = soru4.df[,j] 
  degisken.adi = keeps2[j]
  test <- shapiro.test(degisken) 
  if(test$p.value<0.05) { 
    cat(degisken.adi,' dağılımı Normal Dağılım Değildir (p=', 
        test$p.value,")\n") 
  } else { cat(degisken.adi,' dağılımı Normal Dağılımdır (p=', 
               test$p.value,")\n") 
  }
}
#epworth.uyku.olcegi  dağılımı Normal Dağılım Değildir (p= 1.95381e-06 )
#HADS.anksiyete  dağılımı Normal Dağılım Değildir (p= 1.238763e-05 )
#HADS.depresyon  dağılımı Normal Dağılım Değildir (p= 8.569646e-13 )

#Donusumler ile normallestirilebilirler mi inceleyelim

for(i in 1:ncol(soru4.df)) { 
  degisken = soru4.df[,i] 
  degisken.adi = keeps2[i]
  
  degisken = degisken[!is.na(degisken)]

  test <- shapiro.test(degisken) 
  if(test$p.value<0.05) {
      log.d = log(degisken) 
      log.test = shapiro.test(log.d)
      if(!is.nan(log.test$p.value)) { 
        if(log.test$p.value>0.05) { 
          cat(degisken.adi," log dönüşümü ile normal dağılıma sahiptir (p=", log.test$p.value,")\n") 
          next 
        } 
        }
      sqrt.d = sqrt(degisken) 
      sqrt.test = shapiro.test(sqrt.d) 
      if(!is.nan(sqrt.test$p.value)) { 
        if(sqrt.test$p.value>0.05) { 
          cat(degisken.adi," karekök dönüşümü ile normal dağılıma sahiptir(p=" 
              ,sqrt.test$p.value,")\n") 
          next 
        } 
        }
      ustel.d = exp(-degisken) 
      ustel.test = shapiro.test(ustel.d) 
      if(!is.nan(ustel.test$p.value)) { 
        if(ustel.test$p.value>0.05) { 
          cat(degisken.adi," üstel dönüşümü ile normal dağılıma sahiptir (p=", 
              ustel.test$p.value,")\n") 
          next 
        } 
        }
      bx.d = 1/degisken 
      bx.test = shapiro.test(bx.d) 
      if(!is.nan(bx.test$p.value)) { 
        if(bx.test$p.value>0.05) { 
          cat(degisken.adi," 1/x dönüşümü ile normal dağılıma sahiptir (p=", bx.test$p.value,")\n") 
          next 
        } 
        }
      cat(degisken.adi,' dağılımı Donusum ile Normal Dağılıma Donusemez (p=', 
          test$p.value,")\n") 
  } else { 
        cat(degisken.adi,' dağılımı Normal Dağılımdır (p=', test$p.value,")\n") 
  }
  }

#epworth.uyku.olcegi  dağılımı Donusum ile Normal Dağılıma Donusemez (p= 1.95381e-06 )
#HADS.anksiyete  dağılımı Donusum ile Normal Dağılıma Donusemez (p= 1.238763e-05 )
#HADS.depresyon  dağılımı Donusum ile Normal Dağılıma Donusemez (p= 8.569646e-13 )

#### Soru_4 TAMAMLANDI ####
 
#Soru_5#
#Epworth uyku olcegidegeri ortalamasi 12'den, HADS.anksiyete ve HADS.depresyonolcegi skorlarının 10dan buyuk olup olmadigini test edin.

#5_1 epworh.uyku.olcegi ortalamasi 12den buyuk mu?
#Tek Ornek Testi olup Normal dagilim olmadigi icin ve ortalama test edilecegi icin Wilcoxon isaretli sira sayilari testi

epw.test = wilcox.test(df$epworth.uyku.olcegi,alternative = 'g',mu=12) #  >12 test edildigi icin alternative='g'
print(epw.test) 
#V = 748, p-value = 1
#alternative hypothesis: true location is greater than 12
#Ho red. epworth.uyku.olcegi ortalamasi 12 den kucuktur.

#5_2 HADS.anksiyete ortalamasi 10 dan buyuk mu?
hadsanks.test = wilcox.test(df$HADS.anksiyete,alternative = 'g',mu=10)
print(hadsanks.test)
#V = 2326.5, p-value = 1
#alternative hypothesis: true location is greater than 10
#Ho red. HADS.anksiyete ortalamasi 10 den kucuktur.

#5_3 HADS.depresyon ortalamasi 10 dan buyuk mu?
hadsdep.test = wilcox.test(df$HADS.depresyon,alternative = 'g',mu=10)
print(hadsdep.test)
#V = 97.5, p-value = 1
#alternative hypothesis: true location is greater than 10
#Ho red. HADS.depresyon ortalamasi 10 den kucuktur.

#### Soru_5 TAMAMLANDI ####

#Soru_6#
#Hafta ici uyku suresi - Hafta sonu uyku suresi aynı olup olmadigini inceleyin
# NOT : BUNU SOR #########################################

hius = df$hafta.ici.uyku.suresi
hsus = df$hafta.sonu.uyku.suresi
hihs.test = wilcox.test(hius,hsus,paired = TRUE)
print(hihs.test)
#V = 820.5, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0
#Hafta ici uyku suresi ile hafta sonu uyku suresi farklidir.

#### Soru_6 TAMAMLANDI ####

#Soru-7#
#Erkekler ve kadinlar icin epworth.uyku.olcegi,HADS.anksiyete ve HADS.depresyon puan farklarını incleyin.
#Ikiornek KS testi kullandik.

#7_1 cinsiyet-epworth.uyku.olcegi
#Ho : Dagilimlar aynidir.
#H1 : Dagilimlar aynı degildir.

cinsiyet_verisi = df$cinsiyet[!is.na(df$cinsiyet)]
epw_verisi = veri$epworth.uyku.olcegi[!is.na(df$epworth.uyku.olcegi)]

kadin_epw_verisi = epw_verisi[cinsiyet_verisi=='kadin']
erkek_epw_verisi = epw_verisi[cinsiyet_verisi=='erkek']

cin_epw_test = ks.test(erkek_epw_verisi,kadin_epw_verisi)
print(cin_epw_test)
#D = 0.15343, p-value = 0.1033
#alternative hypothesis: two-sided
#p_value > 0.05 old. icin ayni dagilima sahiptir.
if(cin_epw_test$p.value<0.05) { 
  cat("Erkek ve Kadınların epworth.uyku.olcegi puanları aynı dağılıma", "sahip değildir p=", cin_epw_test$p.value,"\n") 
} else { 
    cat("Erkek ve Kadınların epworth.uyku.olcegi puanları aynı dağılıma", "sahiptir p=", cin_epw_test$p.value,"\n") 
}

#7_2 cinsiyet-HADS.anksiyete
cinsiyet_verisi = df$cinsiyet[!is.na(df$cinsiyet)]
hads_aks_verisi = veri$HADS.anksiyete[!is.na(df$HADS.anksiyete)]

kadin_hadsaks_verisi = hads_aks_verisi[cinsiyet_verisi=='kadin']
erkek_hadsaks_verisi = hads_aks_verisi[cinsiyet_verisi=='erkek']

cin_hadsaks_test = ks.test(erkek_hadsaks_verisi,kadin_hadsaks_verisi)
print(cin_hadsaks_test)
#D = 0.073198, p-value = 0.8697
#alternative hypothesis: two-sided
#p_value > 0.05 old. icin ayni dagilima sahiptir.
if(cin_hadsaks_test$p.value<0.05) { 
  cat("Erkek ve Kadınların HADS.anksiyete puanları aynı dağılıma", "sahip değildir p=", cin_hadsaks_test$p.value,"\n") 
} else { 
  cat("Erkek ve Kadınların HADS.anksiyete puanları aynı dağılıma", "sahiptir p=", cin_hadsaks_test$p.value,"\n") 
}

#7_3 cinsiyet-HADS.depresyon

cinsiyet_verisi = df$cinsiyet[!is.na(df$cinsiyet)]
hads_dep_verisi = veri$HADS.depresyon[!is.na(df$HADS.depresyon)]

kadin_hadsdep_verisi = hads_dep_verisi[cinsiyet_verisi=='kadin']
erkek_hadsdep_verisi = hads_dep_verisi[cinsiyet_verisi=='erkek']

cin_hadsdep_test = ks.test(erkek_hadsdep_verisi,kadin_hadsdep_verisi)
print(cin_hadsdep_test)

if(cin_hadsdep_test$p.value<0.05) { 
  cat("Erkek ve Kadınların HADS.depresyon puanları aynı dağılıma", "sahip değildir p=", cin_hadsdep_test$p.value,"\n") 
} else { 
  cat("Erkek ve Kadınların HADS.depresyon puanları aynı dağılıma", "sahiptir p=", cin_hadsdep_test$p.value,"\n") 
}


#### Soru_7 TAMAMLANDI ####



##### SON ####




  
  
  
  
