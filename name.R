library(dplyr)

df <- read.table('C:/Users/user/Desktop/names.txt',sep = "\t",header = T)
cat <- c('坡','圳','埤','溝','灣','坑','山','崙','湖','江','頂','湳','溪','潭','水','坪','崎','崁','谷','埔','墘')

res <- vector()
par(usr = c(0,1500,0,0.5))
for (k in c(5)){
  plot.new()
  for (i in c(1:21)){
    zz <- filter(df,grepl(cat[i],df$category1))
    dens <- density(zz[,k])
    par(new=TRUE)
    plot(dens$x,dens$y,type="l",xlab="Value",ylab="Count estimate",
         xlim = c(-200,1000),ylim = c(0,0.005),main = colnames(zz)[k])
    #res <- rbind(res,c(cat[i],as.numeric(sapply(zz[4:10],mean))))
  }
}
write.table(res,'C:/Users/user/Desktop/mean.txt',sep = '\t',col.names = F)

