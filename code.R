library(plot3D)

simulate.testing=function(size=10000,pos=seq(0.01,0.50,0.01),type="independent",correlation.prob=0){
  best.test.size=rep(0,length(pos))
  best.test.number=rep(0,length(pos))
  for(j in 1:length(pos)){
    if(type=="independent") people=sample(c(1,0),size,replace=T,prob=c(pos[j],1-pos[j]))
    if(type=="bursty") {
      people=sample(c(1,0),1,replace=T,prob=c(pos[j],1-pos[j]))
      target=pos[j]*size
      xs=seq(0,0.25,by=0.001)
      minimize=xs*size*(1+1/(1-(pos[j]+correlation.prob)))
      minimize=(minimize-target)^2
      prob.temp=1.9*xs[which(minimize==min(minimize))]
      
      for(p in 2:size) {
        if(people[p-1]==0) people[p]=sample(c(1,0),1,replace=T,prob=c(prob.temp,1-prob.temp))
        if(people[p-1]==1) people[p]=sample(c(1,0),1,replace=T,prob=c(prob.temp+correlation.prob,1-prob.temp-correlation.prob))
      }
    }
    number.of.tests=rep(0,size)
    for(i in 1:size){
      ntests=0
      count=1
      while(count<size){
        people.sample=people[count:min(size,count+i)]
        if(all(people.sample==0)) ntests=ntests+1
        if(any(people.sample==1)) ntests=ntests+1+i
        count=count+i
      }
      number.of.tests[i]=ntests
    }
    best.test.size[j]=which(number.of.tests==min(number.of.tests))[1]
    best.test.number[j]=number.of.tests[best.test.size[j]]
  }
  return(cbind(best.test.size,best.test.number))
}

independent=simulate.testing()
bursty1=simulate.testing(type="bursty",correlation.prob=0.05)
bursty2=simulate.testing(type="bursty",correlation.prob=0.1)
bursty3=simulate.testing(type="bursty",correlation.prob=0.15)
bursty4=simulate.testing(type="bursty",correlation.prob=0.2)
bursty5=simulate.testing(type="bursty",correlation.prob=0.25)
bursty6=simulate.testing(type="bursty",correlation.prob=0.3)
bursty7=simulate.testing(type="bursty",correlation.prob=0.35)
bursty8=simulate.testing(type="bursty",correlation.prob=0.4)
bursty9=simulate.testing(type="bursty",correlation.prob=0.45)
bursty10=simulate.testing(type="bursty",correlation.prob=0.5)

answer=cbind(independent,bursty1,bursty2,bursty3,bursty4,bursty5,bursty6,bursty7,bursty8,bursty9)
for(i in 1:nrow(answer)){
  for (j in c(1,3,5,7,9,11,13,15,17,19)){
    if(answer[i,j]>25) answer[i,j]=1
  }
}

pdf("/Users/cabdyn/Box Sync/Postdoc/Papers/Batch testing/fig12.pdf")

image2D(answer[,c(1,3,5,7,9,11,13,15,17,19)],xlab="Population prevalence",ylab="Increased change of consecutive infected persons",xaxt="n",yaxt="n",col=gray.colors(14))
#colkey(clab="Optimal batch size",side.clab=4,clim=c(1,14),col=gray.colors(14))
axis(1,at=seq(0,1,length.out = 45),labels=round(seq(0.01,0.45,length.out = 45),2))
axis(2,at=seq(0,1,length.out=10),labels=seq(0,0.45,by=0.05))
dev.off()

pdf("/Users/cabdyn/Box Sync/Postdoc/Papers/Batch testing/fig2.pdf")
image2D(answer[,c(2,4,6,8,10,12,14,16,18,20)]/10000,xlab="Population prevalence",ylab="Increased change of consecutive infected persons",xaxt="n",yaxt="n",col=rev(gray.colors(20)))
axis(1,at=seq(0,1,length.out = 45),labels=round(seq(0.01,0.45,length.out = 45),2))
axis(2,at=seq(0,1,length.out=10),labels=seq(0,0.45,by=0.05))
dev.off()

pdf("/Users/cabdyn/Box Sync/Postdoc/Papers/Batch testing/fig1color.pdf")
image2D(answer[,c(1,3,5,7,9,11,13,15,17,19)],xlab="Population prevalence",ylab="Increased change of consecutive infected persons",xaxt="n",yaxt="n")
axis(1,at=seq(0,1,length.out = 45),labels=round(seq(0.01,0.45,length.out = 45),2))
axis(2,at=seq(0,1,length.out=10),labels=seq(0,0.45,by=0.05))
dev.off()

pdf("/Users/cabdyn/Box Sync/Postdoc/Papers/Batch testing/fig2color.pdf")
image2D(answer[,c(2,4,6,8,10,12,14,16,18,20)]/10000,xlab="Population prevalence",ylab="Increased change of consecutive infected persons",xaxt="n",yaxt="n")
axis(1,at=seq(0,1,length.out = 45),labels=round(seq(0.01,0.45,length.out = 45),2))
axis(2,at=seq(0,1,length.out=10),labels=seq(0,0.45,by=0.05))
dev.off()

