library(dplyr)
setwd('C:/Users/Guandu/Desktop/Names')
df <- read.table('names_1k.txt',sep = "\t",header = T)
cat <- c('坡','圳','埤','溝','灣','坑','山','崙','湖','江','頂','湳','溪','潭','水','坪','崎','崁','谷','埔','墘')

res <- vector()
for (k in c(13)){
  plot.new()
  for (i in c(1:21)){
    zz <- filter(df,grepl(cat[i],df$category1))
    #dens <- density(zz[,k])
    # par(new=TRUE)
    # plot(ecdf(zz[,k]),verticals=T,do.points=F,main = colnames(zz)[k],xlim = c(0,200),ylim = c(0,1))
    # plot(dens$x,dens$y,type="l",xlab="Value",ylab="Count estimate",
    #      xlim = c(-200,1000),ylim = c(0,0.005),main = colnames(zz)[k])
    res <- rbind(res,c(cat[i],as.numeric(sapply(zz[4:13],mean))))
  }
}
colnames(res) <- c('cat',colnames(df[4:13]))
write.table(res,'mean_1k.txt',sep = '\t',col.names = T)

# mypalette<- colorRampPalette('heat')
# mycolors<- mypalette(21)
mycolors <- heat.colors(21)
ord <- rank(-as.numeric(res[,8]))
mycolors <- mycolors[ord]

symbols(res[,5],res[,10],circles = res[,9],fg="white",bg=mycolors,
        xlab = colnames(res)[5], ylab = colnames(res)[10], 
        main = c(colnames(res)[8],'circle size = roughness order; calculated by mean value'))
text(as.numeric(res[,5]),as.numeric(res[,10]),cat)

pie(rep(1, 21), col = heat.colors(21), main = "heat")



