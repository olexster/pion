library(readr)
library(readxl)
library(xlsx)

# plik<-"~/_budowy/Pulawy azoty Siarczan/obserwacje/pionowosc/K77.xlsx"
plik<-choose.files()
# wyswietal okno z pytaniem o plik
slu <- read_excel(plik, 
                  sheet = "punkty",
                  col_names = c("nr","X", "Y" ,"H"),
                  col_types = c("text", "numeric", "numeric", 
                                "numeric"),skip = 0)
# wczytuje osie konstrukcji
# osie  równolegle  do osi Y
osLit <- read_excel(plik,
           sheet = "osLit",
           col_names = T,
           col_types = c("text", "numeric"),skip = 0)
# osie  równolegle  do osi X
osLicz<-read_excel(plik, 
                   sheet = "osLicz",
                   col_names = T,
                   col_types = c("text", "numeric"),skip = 0)
# osLit <- read_delim("~/roboczy/slupy/osLit.csv",
#                     ";", escape_double = FALSE, locale = locale(decimal_mark = ","),
#                     trim_ws = TRUE)
# osLicz <- read_delim("~/roboczy/slupy/osLicz.csv",
#                      ";", escape_double = FALSE, col_types = cols(os = col_character()), locale = locale(decimal_mark = ","),
#                      trim_ws = TRUE)

# slu<-slu[-which(slu$cod=='wys'),]

slu$H<-slu$H-129.40
slu$osY<-NA
slu$osX<-NA
slu$dX<-slu$dY<-NA
slu$pX<-slu$pY<-NA
slu$he<-NA
slu$hx<-slu$hy<-NA
slu$kr<-NA
slu$pionX<-slu$pionY<-NA

 poziomy<-read_excel(plik, 
                     sheet = "poziomy",
                     col_names = T,
                     col_types = c("text", "numeric"),skip = 0)
slu$poz<-cut(slu$H,
             c(0,c(diff(poziomy$war)/2,0)+poziomy$war),
             labels <-poziomy$nazwa
)



# i<-which(osLit$os=="B")
# which((slu$Y-osLit$wsp[i]<0.5) & (slu$Y-osLit$wsp[i]>-0.5))
# które punkty leza +/- 500mm od osi B

for(os in osLit$os){  # funkcja przypisuje  oznacznia osi do zmiennej "i"
  print(os)
  i<-which(osLit$os==os)
  o<- which(
    (slu$Y-osLit$wsp[i]<0.5) & (slu$Y-osLit$wsp[i]>-0.5)
  )
  slu$osY[o]<-os
  slu$dY[o]<-slu$Y[o]-osLit$wsp[i]
}
for(os in osLicz$os){  # funkcja przypisuje  oznacznia osi do zmiennej "i"
  print(os)
  i<-which(osLicz$os==os)
  o<-which(
    (slu$X-osLicz$wsp[i]<0.5) & (slu$X-osLicz$wsp[i]>-0.5)
  )
  slu$osX[o]<-os
  slu$dX[o]<-slu$X[o]-osLicz$wsp[i]
}
# osiowosc - dodac kolumny z oznaczeniem osi osX osY

# pionowosc

# slu$poz<-cut(slu$H,
#              poziomy,
#              labels <-c(0,
#                         5.500,
#                         9.900,
#                         12.700,
#                         15.500,
#                         20.000,
#                         23.870
#              )
# )
# slu$dX[which((slu$poz==0)&(slu$osY=="B")&(slu$osX==22))]
profil<- read_excel(plik, 
                    sheet = "profil",
                    col_names = c("he","h", "b"),
                    col_types = c("text", "numeric", "numeric"),
                    skip = 1)

# <-data.frame(he=c(200,240,300,400,450,500),
#                    h=c(0.200,0.240,0.300,0.400,0.450,0.500),
#                    b=c(0.200,0.240,0.300,0.300,0.300,0.300))



# /////////////////////////////////////////////////
# poczatek bloku przypisywanie profili do punktów z pomiaru 
# slu$he[which(slu$osX=="25")]<-300
slu$he[which(slu$osX=="E")]<-"A240"
# slu$he[which(slu$osY=="D.5")]<-"HEB300"
slu$he[which(slu$osX=="C")]<-"B450"
# slu$he[which(slu$osY=="A.7")]<-"HEB300"
# slu$he[which(slu$osY=="K1")]<-200

# slu$he[which(slu$osX=="A"|slu$osX=="C"&(
  #   slu$osY=="8"))
  # ]<-"B450"
# slu$he<-"IPE400"
# slu$he[which(slu$osY=="7")]<-"B340"
# slu$he[which(slu$osX=="B" & slu$osY=="8")]<-"B500"
# slu$he[which(slu$osX=="D"|slu$osX=="E")]<-"A240"
slu$he[which((slu$osY==3|
                slu$osY==4|
                slu$osY==5
              )&(
                slu$osX=="B"
                ))]<-"B600"
slu$he[which((slu$osY==2|
                slu$osY==8|
                slu$osY==6
              )&(
                slu$osX=="B"
                ))]<-"B500"
slu$he[which((slu$osY==2|
                slu$osY==3|
                slu$osY==4
              )&(
                slu$osX=="D"
                ))]<-"B340"
slu$he[which((slu$osY==5|
                slu$osY==6|
                slu$osY==8
              )&(
                slu$osX=="D"
                ))]<-"A240"
slu$he[which((slu$osY==2|
                slu$osY==3|
                slu$osY==4|
                slu$osY==5|
                slu$osY==6|
                slu$osY==8
              )&(
                slu$osX=="A"
              ))]<-"B450"
slu$he[which((slu$osX=="A"|slu$osX=="B")& slu$osY==7)]<-"B340"
# koniec bloku przypisywanie profili do punktów z pomiaru 
# /////////////////////////////////////////


slu$osY[which((slu$osX=="A"|
                slu$osX=="B"|
                slu$osX=="C"|
                slu$osX=="D"
              )& slu$osY=="6'")]<-6

# for(i in 1:nrow(slu)){
#   slu$pY[i]<-ifelse(slu$dY[i]>0,
#          slu$dY[i]-profil$b[which(profil$he==slu$he[i])]/2,
#          slu$dY[i]+profil$b[which(profil$he==slu$he[i])]/2
#          )*1000
# }
# for(i in 1:nrow(slu)){
#   slu$pX[i]<-ifelse(slu$dX[i]>0,
#                     slu$dX[i]-profil$h[which(profil$he==slu$he[i])]/2,
#                     slu$dX[i]+profil$h[which(profil$he==slu$he[i])]/2
#   )*1000 
# }
for(i in 1:nrow(slu)){
  slu$hx[i]<-ifelse(slu$dX[i]>0,
                    profil$h[which(profil$he==slu$he[i])]/2,
                    -profil$h[which(profil$he==slu$he[i])]/2
                    # przypisuje dodleglosc naroznika slupa od osi Y
  )
}
for(i in 1:nrow(slu)){
  slu$hy[i]<-ifelse(slu$dY[i]>0,
                    profil$b[which(profil$he==slu$he[i])]/2,
                    -profil$b[which(profil$he==slu$he[i])]/2
                    # przypisuje dodleglosc naroznika slupa od osi X
  )
}
(slu$pX<-(slu$dX-slu$hx)*1000)
(slu$pY<-(slu$dY-slu$hy)*1000)


# write.xlsx()
plik_wyn<-"K77.txt"
cat(c("*********************************","\n"),file = plik_wyn,append = T,sep = "\t"
)
data.frame()
for(licz in levels(as.factor(slu$osX))){
  for(lit in levels(as.factor(slu$osY))){
    cat("os licz",
        licz,#os liczbowa
        "os lit",
        lit,#os literowa
        "\n",
        " X mean", #srednia poprawek 
        mean(slu$pX[which((slu$poz==0)&(slu$osY==lit)&(slu$osX==licz))]),
        " X sd", #odchylenie standardowe sredniej
                sd(slu$pX[which((slu$poz==0)&(slu$osY==lit)&(slu$osX==licz))]),"\n",
        " Y mean",
        mean(slu$pY[which((slu$poz==0)&(slu$osY==lit)&(slu$osX==licz))]),
        " Y sd",
        sd(slu$pY[which((slu$poz==0)&(slu$osY==lit)&(slu$osX==licz))]),
        "\n"
        ,file = plik_wyn,append = T,sep = "\t"
    )
  }
}

for(licz in levels(as.factor(slu$osX))){
  for(lit in levels(as.factor(slu$osY))){
    id_slu<-which((slu$poz==0)&(slu$osY==lit)&(slu$osX==licz))
    print(licz)
    print(lit)
    print(slu$dY[id_slu])
    print(slu$dX[id_slu])
    print(slu$hy[id_slu])
    print(slu$hx[id_slu])
  }
}
slu$dY[which((slu$poz==0)&(slu$osY=="H")&(slu$osX=="23"))]
for(pn in 1:nrow(slu)){ #literki strony slupa
  ifelse(slu$dX[pn]>0,
         ifelse(slu$dY[pn]>0,
                slu$kr[pn]<-"b",
                slu$kr[pn]<-"a"
         ),
         ifelse(slu$dY[pn]>0,
                slu$kr[pn]<-"c",
                slu$kr[pn]<-"d"
         )
  )
}
for(pn in 1:nrow(slu)) {
  slu$pionX[pn]<-slu$dX[pn]-
    mean(dx<-slu$dX[which((slu$poz==0)&
                            (slu$osY==slu$osY[pn])&
                            (slu$osX==slu$osX[pn])&
                            (slu$kr==slu$kr[pn])
    )])
  slu$pionY[pn]<-slu$dY[pn]-
    mean(dy<-slu$dY[which((slu$poz==0)&
                            (slu$osY==slu$osY[pn])&
                            (slu$osX==slu$osX[pn])&
                            (slu$kr==slu$kr[pn])
    )])
  cat("Dx ",dx," Dy ",dy,"\n")
  
}
# (data.frame(tapply(slu$pionY[which((slu$osY=="J")&(slu$osX==""))],
                  # INDEX = slu$poz[which((slu$osY=="B")&(slu$osX=="23"))],
                  # mean))) #obliczna srednie
# sink("wyn.txt",append=TRUE)
cat("*********************************","\n",file = plik_wyn,append = T)
for(licz in levels(as.factor(slu$osX))){
  for(lit in levels(as.factor(slu$osY))){
    # id_slu<-which((slu$poz==0)&(slu$osY==lit)&(slu$osX==licz))
    cat(lit,licz,"\n","X ",tapply(slu$pionX[which((slu$osY==lit)&(slu$osX==licz))],
                                  INDEX = slu$poz[which((slu$osY==lit)&(slu$osX==licz))],
                                  mean),"\n", "Y ", tapply(slu$pionY[which((slu$osY==lit)&(slu$osX==licz))],
                                                           INDEX = slu$poz[which((slu$osY==lit)&(slu$osX==licz))],
                                                           mean),"\n",file = plik_wyn,append = T)
    
  }
}
write.xlsx(x = slu,file = "k77.xlsx",sheetName = "slu",col.names = T,showNA = T)
# sink()

