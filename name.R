library(dplyr)
setwd('C:/Users/Guandu/Desktop/Names')
df <- read.table('names_500m.txt',sep = "\t",header = T)
cat <- c('©Y','¦`','°ñ','·¾','ÆW','§|','¤s','±[','´ò','¦¿','³»','Ùõ','·Ë','¼æ','¤ô','©W','±T','®r','¨¦','®H','áY')

##### export mean or median table ####
res <- vector()
for (i in c(1:21)){
  #zz <- filter(df,grepl(cat[i],df$category1)) #multiple words included
  zz <- filter(df,df$category1==cat[i]) #if only the word included
  res <- rbind(res,c(as.numeric(sapply(zz[4:13],mean,na.rm = TRUE))))
}
res <- data.frame(cat,res)
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


# mycolors <- heat.colors(21)
# ord <- rank(-as.numeric(res[,8]))
# mycolors <- mycolors[ord]
mycolors <- (heat.colors(100))
ord <- round(100*as.numeric(res[,8])/max(as.numeric(res[,8])))
mycolors <- mycolors[ord]

symbols(res[,5],res[,11],circles = res[,9],fg="grey",bg=mycolors,
        xlab = "¶Zªe¹D¥­§¡¤ô¥­¶ZÂ÷(¤½¤Ø)", ylab = "¶Zªe¹D¥­§¡««ª½¶ZÂ÷(¤½¤Ø)") #colnames(res)[5]
text(as.numeric(res[,5]),as.numeric(res[,11]),cat)

legend('topright', legend = c('0.1','0.2','0.3'),pch=1,cex=c(0.5,0.5,0.5))

pie(rep(1, 100), col = heat.colors(100), main = "heat") #color wheel

library(showtext)
library(Cairo)
library(ggplot2)
mycolors <- (heat.colors(100))
font_add("notosanR", "NotoSansCJKtc-Regular.otf")

CairoPDF("TEST.pdf",family = "notosanR")
showtext_begin()
ggplot(res,aes(x=HDIS_MEAN, y=VDIS_MEAN,label = cat,fill = GRA_MEAN))+
  geom_point(shape=21, aes(size=ROUGHNESS_MEAN),show.legend = T)+
  scale_size_continuous(range=c(4,20))+
  scale_fill_gradientn(colours=mycolors,trans = 'reverse')+ #
  geom_text(aes(fontface='bold'))+
  ylim(0,100)+xlim(350,570)+
  xlab('¶Zªe¹D¥­§¡¤ô¥­¶ZÂ÷(¤½¤Ø)')+ylab('¶Zªe¹D¥­§¡««ª½¶ZÂ÷(¤½¤Ø)')+
  labs(fill = '©Y«×(%)', size = '¦aªí²ÊÁW«×(-)')+
  theme_bw()

showtext_end()
dev.off()

