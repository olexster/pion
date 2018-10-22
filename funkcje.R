library(readr)
slu <- read_delim("~/roboczy/slupy/K 50 2711.cst"," ", escape_double = FALSE, trim_ws = TRUE)

slu$H<-slu$H-130.7
slu$osY<-NA
slu$osX<-NA
slu$dX<-slu$dY<-NA
slu$pX<-slu$pY<-NA
slu$he<-NA
slu$hx<-slu$hy<-NA
slu$kr<-NA
slu$pionX<-slu$pionY<-NA
poziomy<-c(0,
           5.500,
           9.900,
           12.700,
           15.500,
           20.000,
           23.870)
poziomy<-c(0,c(diff(poziomy)/2,0)+poziomy)
slu$poz<-cut(slu$H,
    poziomy,
    labels <-c(0,
               5.500,
               9.900,
               12.700,
               15.500,
               20.000,
               23.870)
    )

osLit <- read_delim("~/roboczy/slupy/osLit.csv",
                    ";", escape_double = FALSE, locale = locale(decimal_mark = ","),
                    trim_ws = TRUE)
osLicz <- read_delim("~/roboczy/slupy/osLicz.csv",
                     ";", escape_double = FALSE, col_types = cols(X3 = col_skip(),
                                                                  X4 = col_skip(), X5 = col_skip(),
                                                                  os = col_character()), locale = locale(decimal_mark = ","),
                     trim_ws = TRUE)
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

slu$poz<-cut(slu$H,
             poziomy,
             labels <-c(0,
                        5.500,
                        9.900,
                        12.700,
                        15.500,
                        20.000,
                        23.870
                        )
             )
# slu$dX[which((slu$poz==0)&(slu$osY=="B")&(slu$osX==22))]

profil<-data.frame(he=c(300,450,500),h=c(0.300,0.450,0.500),b=c(0.300,0.300,0.300))
slu$he[which(slu$osX=="25")]<-300
slu$he[which(slu$osY=="I")]<-300
slu$he[which(slu$osY=="H")]<-300

slu$he[which(slu$osX=="23"&(
               slu$osY=="B"|
               slu$osY=="C"|
               slu$osY=="G"))
       ]<-450
slu$he[which(slu$osX=="22"&(
               slu$osY=="B"|
               slu$osY=="C"|
               slu$osY=="G"))
       
       ]<-450
slu$he[which(slu$osX=="23"&(
               slu$osY=="D"|
               slu$osY=="E"|
               slu$osY=="F"))
       
       ]<-500
slu$he[which(slu$osX=="22"&(
               slu$osY=="D"|
               slu$osY=="E"|
               slu$osY=="F"))
       
       ]<-500
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
  slu$hy[i]<-ifelse(slu$dY[i]>0,
                    profil$b[which(profil$he==slu$he[i])]/2,
                    -profil$b[which(profil$he==slu$he[i])]/2
  )
}
for(i in 1:nrow(slu)){
  slu$hx[i]<-ifelse(slu$dX[i]>0,
                    profil$h[which(profil$he==slu$he[i])]/2,
                    -profil$h[which(profil$he==slu$he[i])]/2
  )
}
(slu$pX<-(slu$dX-slu$hx)*1000)
(slu$pY<-(slu$dY-slu$hy)*1000)

for(licz in levels(as.factor(slu$osX))){
  for(lit in levels(as.factor(slu$osY))){
    cat(
      paste(
        licz,#os liczbowa
        " ",
        lit,#os literowa
        "\n",
        " X mean", #srednia poprawek 
        mean(slu$pX[which((slu$poz==0)&(slu$osY==lit)&(slu$osX==licz))]),
        " X sd", #odchylenie standardowe sredniej
        
        sd(slu$pX[which((slu$poz==0)&(slu$osY==lit)&(slu$osX==licz))]),
        "\n",
        " Y mean",
        mean(slu$pY[which((slu$poz==0)&(slu$osY==lit)&(slu$osX==licz))]),
        " Y sd",
        sd(slu$pY[which((slu$poz==0)&(slu$osY==lit)&(slu$osX==licz))]),
        "\n"
      )
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
data.frame(tapply(slu$pionY[which((slu$osY=="B")&(slu$osX=="23"))],
       INDEX = slu$poz[which((slu$osY=="B")&(slu$osX=="23"))],
      mean)) #obliczna srednie
# sink("wyn.txt",append=TRUE)
for(licz in levels(as.factor(slu$osX))){
  for(lit in levels(as.factor(slu$osY))){
    # id_slu<-which((slu$poz==0)&(slu$osY==lit)&(slu$osX==licz))
    cat(lit,licz,"\n","X ",tapply(slu$pionX[which((slu$osY==lit)&(slu$osX==licz))],
           INDEX = slu$poz[which((slu$osY==lit)&(slu$osX==licz))],
           mean),"\n", "Y ", tapply(slu$pionY[which((slu$osY==lit)&(slu$osX==licz))],
                 INDEX = slu$poz[which((slu$osY==lit)&(slu$osX==licz))],
                 mean),"\n",file = "wyn.txt",append = T)
    
  }
}
# sink()
