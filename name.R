library(dplyr)
setwd('C:/Users/Guandu/Desktop/Names')
df <- read.table('names_500m.txt',sep = "\t",header = T)
cat <- c('坡','圳','埤','溝','灣','坑','山','崙','湖','江','頂','湳','溪','潭','水','坪','崎','崁','谷','埔','墘')

##### export mean or median table ####
res <- vector()
for (i in c(1:21)){
  #zz <- filter(df,grepl(cat[i],df$category1)) #multiple words included
  zz <- filter(df,df$category1==cat[i]) #if only the word included
  res <- rbind(res,c(cat[i],as.numeric(sapply(zz[4:13],mean,na.rm = TRUE))))
}
colnames(res) <- c('cat',colnames(df[4:13]))
#write.table(res,'mean_500.txt',sep = '\t',col.names = T)

#### plot for KDE or ECDF ####
for (k in c(4:13)){
  #plot.new()
  for (i in c(19)){
    zz <- filter(df,grepl(cat[i],df$category1))
    # dens <- density(zz[,k])  #KDE
    # par(new=TRUE)
    # plot(dens$x,dens$y,type="l",xlab="Value",ylab="Count estimate",
    #      xlim = c(-200,1000),ylim = c(0,0.005),main = colnames(zz)[k]) #KDE
    plot(ecdf(zz[,k]),verticals=T,do.points=F,main = colnames(zz)[k], xlab = '',
         ) #ECDF xlim = c(0,200),ylim = c(0,1)
  }
}


mycolors <- heat.colors(21)
ord <- rank(-as.numeric(res[,8]))
mycolors <- mycolors[ord]

symbols(res[,5],res[,11],circles = res[,9],fg="grey",bg=mycolors,
        xlab = colnames(res)[5], ylab = colnames(res)[11], 
        main = c(colnames(res)[8],'circle size = roughness order; calculated by mean value'))
text(as.numeric(res[,5]),as.numeric(res[,11]),cat)

#pie(rep(1, 21), col = heat.colors(21), main = "heat") #color wheel



