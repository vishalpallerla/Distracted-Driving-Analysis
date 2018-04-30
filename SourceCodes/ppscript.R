setwd("F:\\Vishal_UH\\Spring 17\\Statistical Methods\\Stat")
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)
library(xlsx)
library(openxlsx)

pdf("pp_graph.pdf")
func_pp<-function(t,xlim){
fileNamesPD<-list.files(recursive = T,pattern=paste("\\-00",t,".pp","$",sep = ""))
n<-0
for(ef in fileNamesPD){
pd<-read.xlsx(ef,1,startRow=9)
plot(pd[,2],pd[,4],type='l',xlim = c(0,xlim),ylim = c(0.00,0.04),ylab="",xlab="")
box()
#mtext(expression("0.00  0.04"), side=2, line=1)
mtext(expression("PP["~degree~ C^{2} ~ "]"), side=2, line=2)
if(t==1){
  mtext("Perinasal Perspiration[" ~degree~C^{2} ~"] valid signal sets" , side=3, line=1)
  mtext(expression("BL"), side=4, line=1)
}
if(t==2)
  mtext(expression("PD"), side=4, line=1)
if(t==3)
  mtext(expression("RD"), side=4, line=1)
if(t==4)
  mtext(expression("ED"), side=4, line=1)
if(t==5)
  mtext(expression("CD"), side=4, line=1)
if(t==6)
  mtext(expression("MD"), side=4, line=1)
if(t==7)
  mtext(expression("ND"), side=4, line=1)
if(t==8){
  mtext(expression("FD"), side=4, line=1)
  #xlab("Time[s]")
  mtext(expression("Time[s]"), side=1, line=2)
  #mtext(expression("Perinasal NR EDA signals per drive, for all subjects"), side=1, line=2)
}
par(new = TRUE)
n<-n+1
}
text(xlim-15,0.03,paste("n = ",n))
}

#main = expression("Perinasal Perspiration[C"^2),
#graphics.off()
#par("mar")

par(mfrow=c(8,1))
#par(mar=c(2,4,2.7,2))
par(mar = c(2,3.5, 1.5, 2)+0.025, oma=c(2, 0, 1, 0))
func_pp(1,400)
par(new=FALSE)
func_pp(2,600)
par(new=FALSE)
func_pp(3,700)
par(new=FALSE)
func_pp(4,900)
par(new=FALSE)
func_pp(5,900)
par(new=FALSE)
func_pp(6,900)
par(new=FALSE)
func_pp(7,1100)
par(new=FALSE)
func_pp(8,350)
dev.off()