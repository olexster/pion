# pn<-66
# 
# slu$dX[which((slu$poz==11.2)&
#                (slu$osY==slu$osY[pn])&
#                (slu$osX==slu$osX[pn])&
#                (slu$kr==slu$kr[pn])
# )]
# length((which((slu$poz==0)&
#         (slu$osY==slu$osY[pn])&
#         (slu$osX==slu$osX[pn])&
#         (slu$kr==slu$kr[pn]))))<1
# 
# 
# (slu$poz==11.2)&
#   (slu$osY==slu$osY[pn])&
#   (slu$osX==slu$osX[pn])&
#   (slu$kr==slu$kr[pn])
# poz<-0
# while (length(which((slu$poz==poz)&
#                     (slu$osY==slu$osY[pn])&
#                     (slu$osX==slu$osX[pn])&
#                     (slu$kr==slu$kr[pn])))<1) {
#   print (poz)
#   poz<-poziomy[which(poziomy==poz)+1]
#     # return(poz)
#   }

for(pn in which(is.na(slu$pionX))) {
  
  slu$pionX[pn]<-slu$dX[pn]-
    mean(dx<-slu$dX[which((slu$poz==26.5)&
                            (slu$osY==slu$osY[pn])&
                            (slu$osX==slu$osX[pn])&
                            (slu$kr==slu$kr[pn])
    )])
  slu$pionY[pn]<-slu$dY[pn]-
    mean(dy<-slu$dY[which((slu$poz==26.5)&
                            (slu$osY==slu$osY[pn])&
                            (slu$osX==slu$osX[pn])&
                            (slu$kr==slu$kr[pn])
    )])
  cat("Dx ",dx," Dy ",dy," poziom ",slu$poz[pn],"\n")
  
}
