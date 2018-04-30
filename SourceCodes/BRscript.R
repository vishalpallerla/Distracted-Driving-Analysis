setwd("F:\\Vishal_UH\\Spring 17\\Statistical Methods\\Stat")
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)
library(xlsx)
library(openxlsx)
library(varhandle)
library(dplyr)
pdf("BR_cleannnnnn.pdf")
func_pp<-function(t,xl){
  fileNamesPD<-list.files(recursive = T,pattern=paste("\\-00",t,".BR","$",sep = ""))
  n<-0
  
  for(ef in fileNamesPD){
    pd<-read.xlsx2(ef,1,startRow=9)
    time<-as.numeric(unfactor(pd[,2]))
    br<-as.numeric(unfactor(pd[,3]))
    plot(time,br,type='l',xlim = c(0,xl),ylim = c(0,30),ylab="",xlab="")
    lines(time,br,type="l")
    box()
    mtext(expression("BR[bpm]"), side=2, line=2)
    if(t==1){
      
      #mtext(expression("BL"), side=4, line=1)
    }
    if(t==2){
      mtext("Breath  Rate [bpm] raw signal sets" , side=3, line=1)
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
  text(xl-(xl/25),20,paste("n = ",n))  
}

func_br_clean<-function(t,xl){
  fileNamesPD<-list.files(recursive = T,pattern=paste("\\-00",t,".BR","$",sep = ""))
  n<-0
  
  for(ef in fileNamesPD){
    pd<-read.xlsx2(ef,1,startRow=9)
    time<-as.numeric(unfactor(pd[,2]))
    br<-as.numeric(unfactor(pd[,3]))
    pd1<-filter(pd,br>=70|br<=4)
    if(nrow(pd1)==0){
      plot(time,br,type='l',xlim = c(0,xl),ylim = c(5,30),ylab="",xlab="")
      lines(time,br,type="l")
      box()
      #mtext(expression("HR[bpm]"), side=2, line=2)
      if(t==1){
        
        mtext(expression("BL"), side=4, line=1)
      }
      if(t==2){
        mtext("Breath  Rate [bpm] valid signal sets" , side=3, line=1)
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
      write(ef,file = "BRDiscarded.txt",append = TRUE)
    }
  }
  
  text(xl-(xl/25),20,paste("n = ",n))  
}


#par(mfrow=c(14,2))
layout(matrix(c(1:14), nrow = 7, ncol = 2, byrow = FALSE))
par(mar = c(2,3.5, 1, 2)+0.025, oma=c(2, 0, 1, 0))
headControl = TRUE
#par(mar=c(2,4,2.7,2))
func_pp(2,600)
par(new=FALSE)
func_pp(3,700)
par(new=FALSE)
func_pp(4,1000)
par(new=FALSE)
func_pp(5,1000)
par(new=FALSE)
func_pp(6,1000)
par(new=FALSE)
func_pp(7,1000)
par(new=FALSE)
func_pp(8,300)
mtext(expression("Time[s]"), side=1, line=2)
par(new=FALSE)
func_br_clean(2,600)
par(new=FALSE)
func_br_clean(3,700)
par(new=FALSE)
func_br_clean(4,1000)
par(new=FALSE)
func_br_clean(5,1000)
par(new=FALSE)
func_br_clean(6,1000)
par(new=FALSE)
func_br_clean(7,1000)
par(new=FALSE)
func_br_clean(8,300)
dev.off()

