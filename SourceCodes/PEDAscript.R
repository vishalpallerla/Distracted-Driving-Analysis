setwd("F:\\Vishal_UH\\Spring 17\\Statistical Methods\\Stat")
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)
library(xlsx)
library(openxlsx)
library(varhandle)
library(dplyr)
pdf("PEDA4.pdf")
func_pp<-function(t,xl,yx,yy){
  fileNamesPD<-list.files(recursive = T,pattern=paste("\\-00",t,".peda","$",sep = ""))
  n<-0
  
  for(ef in fileNamesPD){
    pd<-read.xlsx2(ef,1,startRow=9)
    time<-as.numeric(unfactor(pd[,2]))
    peda<-as.numeric(unfactor(pd[,3]))
    plot(time,peda,type='l',xlim = c(0,xl), ylim = c(yx,yy),ylab="",xlab="")
    lines(time,peda,type="l")
    box()
    mtext(expression("EDA[k"~Omega~"]"), side=2, line=2)
    if(t==1){
      
      #mtext(expression("BL"), side=4, line=1)
    }
    if(t==2){
      mtext("Palm EDA [k"~Omega~"] raw signal sets" , side=3, line=1)
      #mtext(expression("PD"), side=4, line=1)
    }
    if(t==3)
      #mtext(expression("RD"), side=4, line=1)
      if(t==4)
        #mtext(expression("ND"), side=4, line=1)
        if(t==5)
          #mtext(expression("CD"), side=4, line=1)
          if(t==6)
            #mtext(expression("ED"), side=4, line=1)
            if(t==7)
              #mtext(expression("MD"), side=4, line=1)
              if(t==8){
                mtext(expression("Time[s]"), side=1, line=2)
                #mtext(expression("FD"), side=4, line=1)
              }
    par(new = TRUE)
    n<-n+1
  }
  text(xl-(xl/25),yy-(yy/5),paste("n = ",n))  
}

func_peda_clean<-function(t,xl){
  fileNamesPD<-list.files(recursive = T,pattern=paste("\\-00",t,".peda","$",sep = ""))
  n<-0
  
  for(ef in fileNamesPD){
    pd<-read.xlsx2(ef,1,startRow=9)
    time<-as.numeric(unfactor(pd[,2]))
    peda<-as.numeric(unfactor(pd[,3]))
    pd1<-filter(pd,peda>=4500 | peda<=0)
    if(nrow(pd1)==0){
      plot(time,peda,type='l',xlim = c(0,xl),ylim = c(0,2500),ylab="",xlab="")
      lines(time,peda,type="l")
      box()
      if(t==1){
        
        mtext(expression("BL"), side=4, line=1)
      }
      if(t==2){
        mtext("Palm EDA [k"~Omega~"] valid signal sets" , side=3, line=1)
        mtext(expression("PD"), side=4, line=1)
      }
      if(t==3)
        mtext(expression("RD"), side=4, line=1)
      if(t==4)
        mtext(expression("ND"), side=4, line=1)
      if(t==5)
        mtext(expression("CD"), side=4, line=1)
      if(t==6)
        mtext(expression("ED"), side=4, line=1)
      if(t==7)
        mtext(expression("MD"), side=4, line=1)
      if(t==8){
        mtext(expression("FD"), side=4, line=1)
        mtext(expression("Time[s]"), side=1, line=2)
        #mtext(expression("Perinasal NR EDA signals per drive, for all subjects"), side=1, line=2)
      }
      par(new = TRUE)
      n<-n+1
    }
    else
    {
      write(ef,file = "PEDADiscarded.txt",append = TRUE)
    }
  }
  text(xl-(xl/25),2200,paste("n = ",n))
  #text(xl-(xl/25),100,paste("n = ",n))  
}


#par(mfrow=c(14,2))
layout(matrix(c(1:14), nrow = 7, ncol = 2))
par(mar = c(2,3.5, 1.25, 2)+0.025, oma=c(2, 0, 1.5, 0))
headControl = TRUE
#par(mar=c(2,4,2.7,2))
func_pp(2,600,25,600000)
par(new=FALSE)
func_pp(3,700,-8700000,3000000)
par(new=FALSE)
func_pp(4,900,-8605852,2944641)
par(new=FALSE)
func_pp(5,900,-8605852,2944641)
par(new=FALSE)
func_pp(6,900,8,800000)
par(new=FALSE)
func_pp(7,1000,-8700000,3000000)
par(new=FALSE)
func_pp(8,250,23,800000)
mtext(expression("Time[s]"), side=1, line=2)
par(new=FALSE)
func_peda_clean(2,600)
par(new=FALSE)
func_peda_clean(3,700)
par(new=FALSE)
func_peda_clean(4,900)
par(new=FALSE)
func_peda_clean(5,900)
par(new=FALSE)
func_peda_clean(6,900)
par(new=FALSE)
func_peda_clean(7,1000)
par(new=FALSE)
func_peda_clean(8,250)
dev.off()

