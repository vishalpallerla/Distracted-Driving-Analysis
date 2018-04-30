setwd("F:\\Vishal_UH\\Spring 17\\Statistical Methods\\Stat")
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)
library(xlsx)
library(openxlsx)
library(varhandle)
library(dplyr)
pdf("res_final.pdf")
func_pp<-function(t,xl){
  n<-0
  fileNamesPD<-list.files(recursive = T,pattern=paste("\\-00",t,".res","$",sep = ""))
  for(ef in fileNamesPD){
    pd<-read.xlsx2(ef,1,startRow=9)
    pd1<-mutate(pd,NRSpeed=Speed)
    pd1$Speed[pd1$Speed>-0.1 & pd1$Speed<0.1]<-0
    pd1$Speed[pd1$Speed<= -0.1]<-NA
    write.xlsx(pd1,file=paste("Speed",n,".xlsx"),sheetName="sheet1", 
               col.names=TRUE, row.names=FALSE, append=FALSE)
    pd$Speed[pd$Speed>-0.1 & pd$Speed<0.1]<-0
    pd$Speed[pd$Speed< -0.1]<-NA
    
    pd<-na.omit(pd)
    time<-as.numeric(unfactor(pd[,2]))
    hr<-as.numeric(unfactor(pd[,3]))
    plot(time,hr,type='l',xlim = c(0,xl),ylim = c(0,120),ylab="",xlab="")
    lines(time,hr,type="l")
    box()
    if(t==2){
      mtext("Speed [Km/hr]" , side=3, line=1)
    }
    if(t==8){
      mtext("Time[s]", side=1, line=2)
    }
    par(new = TRUE)
    n<-n+1
  }
}

func_acc<-function(t,xl){
  fileNamesPD<-list.files(recursive = T,pattern=paste("\\-00",t,".res","$",sep = ""))
  n<-0
  for(ef in fileNamesPD){
    pd<-read.xlsx2(ef,1,startRow=9)
    pd1<-mutate(pd,NR=Acceleration)
    pd1$NR[pd$Acceleration<0]<-NA
    write.xlsx(pd1,file=paste("Acceleration",n,".xlsx"),sheetName="sheet1", 
               col.names=TRUE, row.names=FALSE, append=FALSE)
    pd$Acceleration[pd$Acceleration<0]<-NA
    
    pd<-na.omit(pd)
    time<-as.numeric(unfactor(pd[,2]))
    hr<-as.numeric(unfactor(pd[,4]))
    plot(time,hr,type='l',xlim = c(0,xl),ylim = c(0,60),ylab="",xlab="")
    
    lines(time,hr,type="l")
    box()
    if(t==2){
      mtext(expression("Acceleration ["~degree~"]") , side=3, line=1)
    }
    
    if(t==8){
      mtext("Time[s]", side=1, line=2)
    }
    par(new = TRUE)
    n<-n+1
  }
  #text(xl-(xl/100),180,paste("n = ",n))  
}

func_break<-function(t,xl){
  fileNamesPD<-list.files(recursive = T,pattern=paste("\\-00",t,".res","$",sep = ""))
  n<-0
  for(ef in fileNamesPD){
    pd<-read.xlsx2(ef,1,startRow=9)
    pd1<-mutate(pd,NRBreak=names(pd)[5])
    pd1$Braking[pd1$Speed>300]<-300
    write.xlsx(pd1,file=paste("Break",n,".xlsx"),sheetName="sheet1", 
               col.names=TRUE, row.names=FALSE, append=FALSE)
    
    pd$Speed[pd$Speed>300]<-300
    pd<-na.omit(pd)
    time<-as.numeric(unfactor(pd[,2]))
    hr<-as.numeric(unfactor(pd[,5]))
    plot(time,hr,type='l',xlim = c(0,xl),ylim = c(0,300),ylab="",xlab="")
    lines(time,hr,type="l")
    box()
    
    if(t==2){
      mtext("Braking[N]" , side=3, line=1)
      
    }
    
    if(t==8){
      mtext("Time[s]", side=1, line=2)
    }
    par(new = TRUE)
    n<-n+1
  }
  #text(xl-(xl/100),180,paste("n = ",n))  
}

func_steer<-function(t,xl){
  fileNamesPD<-list.files(recursive = T,pattern=paste("\\-00",t,".res","$",sep = ""))
  n<-0
  for(ef in fileNamesPD){
    pd<-read.xlsx2(ef,1,startRow=9)
    time<-as.numeric(unfactor(pd[,2]))
    hr<-as.numeric(unfactor(pd[,6]))
    plot(time,hr,type='l',xlim = c(0,xl),ylim = c(-4,4),ylab="",xlab="")
    lines(time,hr,type="l")
    box()
    if(t==2){
      mtext("Steering[rad]" , side=3, line=1)
    }
    if(t==8){
      mtext("Time[s]", side=1, line=2)
    }
    par(new = TRUE)
    n<-n+1
  }
  #text(xl-(xl/100),180,paste("n = ",n))  
}

func_lane<-function(t,xl){
  fileNamesPD<-list.files(recursive = T,pattern=paste("\\-00",t,".res","$",sep = ""))
  n<-0
  for(ef in fileNamesPD){
    pd<-read.xlsx2(ef,1,startRow=9)
    time<-as.numeric(unfactor(pd[,2]))
    hr<-as.numeric(unfactor(pd$Lane.Position))
    plot(time,hr,type='l',xlim = c(0,xl),ylim = c(-4,4),ylab="",xlab="")
    lines(time,hr,type="l")
    box()
    if(t==2){
      mtext("Lane Position[m]" , side=3, line=1)
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
      mtext("Time[s]", side=1, line=2)
    }
    par(new = TRUE)
    n<-n+1
  }
  #text(xl-(xl/100),180,paste("n = ",n))  
}

layout(matrix(c(1:35), nrow = 7, ncol = 5))
par(mar = c(2,3.5, 1.25, 2)+0.025, oma=c(2, 0, 1.5, 0))
headControl = TRUE
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
par(new=FALSE)
func_acc(2,600)
par(new=FALSE)
func_acc(3,700)
par(new=FALSE)
func_acc(4,1000)
par(new=FALSE)
func_acc(5,1000)
par(new=FALSE)
func_acc(6,1000)
par(new=FALSE)
func_acc(7,1000)
par(new=FALSE)
func_acc(8,300)
par(new=FALSE)
func_break(2,600)
par(new=FALSE)
func_break(3,700)
par(new=FALSE)
func_break(4,1000)
par(new=FALSE)
func_break(5,1000)
par(new=FALSE)
func_break(6,1000)
par(new=FALSE)
func_break(7,1000)
par(new=FALSE)
func_break(8,300)
par(new=FALSE)
func_steer(2,600)
par(new=FALSE)
func_steer(3,700)
par(new=FALSE)
func_steer(4,1000)
par(new=FALSE)
func_steer(5,1000)
par(new=FALSE)
func_steer(6,1000)
par(new=FALSE)
func_steer(7,1000)
par(new=FALSE)
func_steer(8,300)
par(new=FALSE)
func_lane(2,600)
par(new=FALSE)
func_lane(3,700)
par(new=FALSE)
func_lane(4,1000)
par(new=FALSE)
func_lane(5,1000)
par(new=FALSE)
func_lane(6,1000)
par(new=FALSE)
func_lane(7,1000)
par(new=FALSE)
func_lane(8,300)
par(new=FALSE)
dev.off()