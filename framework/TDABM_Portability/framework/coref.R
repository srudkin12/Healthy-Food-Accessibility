complement <- function(y, rho, x){ 
  if (missing(x)) x <- rnorm(length(y))
  y.perp <- residuals(lm(x ~ y))
  rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)
}

nor<-function(x){ x-min(x)/(max(x)-min(x))}

points_to_balls<-function(l){
  a001<-length(l$landmarks)
  a1<-matrix(0,nrow=a001,ncol=2)
  a1<-as.data.frame(a1)
  names(a1)<-c("pt","ball")
  for(i in 1:a001){
    a<-as.data.frame(l$points_covered_by_landmarks[i])
    names(a)<-"pt"
    a$ball<-i
    a1<-rbind.data.frame(a1,a)
   }
  a1<-a1[2:nrow(a1),]
  return(a1)
}

latout<-function(data,filename){
 if(missing(filename)) filename<-"latout.txt"
 a001<-nrow(data)
 a002<-ncol(data)
 a003<-a002-1
 mat1<-matrix("",nrow=a001,ncol=1)
 mat1[,1]<-data[,1]
 for(i in 1:a001){
   for(k in 2:a003){
     mat1[i,1]<-paste(mat1[i,1],data[i,k],sep="&")
   }
 }
for(i in 1:a001){
 mat1[i,1]<-paste0(mat1[i,1],"&",data[i,a002],"\\","\\")
 }
 write.table(as.data.frame(mat1),filename,sep="\n",row.names=FALSE,quote=FALSE)
} 

# The following two functions are user generated. Any similarity to work by others is unintentional and I welcome the opportunity to credit relevant authors

points_to_balls<-function(l){
  a001<-length(l$landmarks)
  a1<-matrix(0,nrow=a001,ncol=2)
  a1<-as.data.frame(a1)
  names(a1)<-c("pt","ball")
  for(i in 1:a001){
    a<-as.data.frame(l$points_covered_by_landmarks[i])
    names(a)<-"pt"
    a$ball<-i
    a1<-rbind.data.frame(a1,a)
   }
  a1<-a1[2:nrow(a1),]
  return(a1)
}

latout<-function(data,filename){
 if(missing(filename)) filename<-"latout.txt"
 a001<-nrow(data)
 a002<-ncol(data)
 a003<-a002-1
 mat1<-matrix("",nrow=a001,ncol=1)
 mat1[,1]<-data[,1]
 for(i in 1:a001){
   for(k in 2:a003){
     mat1[i,1]<-paste(mat1[i,1],data[i,k],sep="&")
   }
 }
for(i in 1:a001){
 mat1[i,1]<-paste0(mat1[i,1],"&",data[i,a002],"\\","\\")
 }
 write.table(as.data.frame(mat1),filename,sep="\n",row.names=FALSE,quote=FALSE)
} 

# Function for summary statistics tables

# For summary statistics there are four functions depending on the level of detail required

sstatsmat<-function(characteristics,decp){
 if(missing(decp)) decp <- 2
 a001<-ncol(characteristics)
 sstats<-matrix(0,nrow=a001,ncol=5)
 for(i in 1:a001){
  j<-i
  sstats[i,1]<-names(characteristics)[j]
  sstats[i,2]<-round(mean(characteristics[,j]),decp)
  sstats[i,3]<-round(sd(characteristics[,j]),decp)
  sstats[i,4]<-round(min(characteristics[,j]),decp)
  sstats[i,5]<-round(max(characteristics[,j]),decp)
 }
 return(sstats)
}

sstatsmatdet<-function(characteristics,decp){
 if(missing(decp)) decp <- 2
 a001<-ncol(characteristics)
 sstats<-matrix(0,nrow=a001,ncol=8)
 for(i in 1:a001){
  j<-i
  sstats[i,1]<-names(characteristics)[j]
  sstats[i,2]<-round(mean(characteristics[,j]),decp)
  sstats[i,3]<-round(sd(characteristics[,j]),decp)
  sstats[i,4]<-round(min(characteristics[,j]),decp)
  sstats[i,5]<-round(quantile(characteristics[,j],0.25,na.rm=TRUE),decp)
  sstats[i,6]<-round(quantile(characteristics[,j],0.50,na.rm=TRUE),decp)
  sstats[i,7]<-round(quantile(characteristics[,j],0.75,na.rm=TRUE),decp)
  sstats[i,8]<-round(max(characteristics[,j]),decp)
 }
 return(sstats)
}

sstatsmatk<-function(characteristics,decp){
 if(missing(decp)) decp <- 2
 a001<-ncol(characteristics)
 sstats<-matrix(0,nrow=a001,ncol=7)
 for(i in 1:a001){
  j<-i
  sstats[i,1]<-names(characteristics)[j]
  sstats[i,2]<-round(mean(characteristics[,j]),decp)
  sstats[i,3]<-round(sd(characteristics[,j]),decp)
  sstats[i,4]<-round(min(characteristics[,j]),decp)
  sstats[i,5]<-round(max(characteristics[,j]),decp)
  sstats[i,6]<-round(skewness(characteristics[,j]),decp)
  sstats[i,7]<-round(kurtosis(characteristics[,j]),decp)
 }
 return(sstats)
}


sstatsmatdetk<-function(characteristics,decp){
 if(missing(decp)) decp <- 2
 a001<-ncol(characteristics)
 sstats<-matrix(0,nrow=a001,ncol=10)
 for(i in 1:a001){
  j<-i
  sstats[i,1]<-names(characteristics)[j]
  sstats[i,2]<-round(mean(characteristics[,j]),decp)
  sstats[i,3]<-round(sd(characteristics[,j]),decp)
  sstats[i,4]<-round(min(characteristics[,j]),decp)
  sstats[i,5]<-round(quantile(characteristics[,j],0.25),decp)
  sstats[i,6]<-round(quantile(characteristics[,j],0.50),decp)
  sstats[i,7]<-round(quantile(characteristics[,j],0.75),decp)
  sstats[i,8]<-round(max(characteristics[,j]),decp)
#  sstats[i,9]<-round(skewness(characteristics[,j]),decp)
#  sstats[i,10]<-round(kurtosis(characteristics[,j]),decp)
 }
 return(sstats)
}



# Function for correlation matrix

corm<-function(vec,n,spear){
 if(missing(spear)){spear<-"FALSE"}
 if(missing(n)){n<-2}
 a101<-ncol(vec)
 m001a<-matrix(0,nrow=a101,ncol=a101)
 print(paste0("Rows: ",a101," Cols: ",a101))
 for(i in 1:a101){
  for(j in 1:a101){
   a201<-cor(vec[,i],vec[,j])
   a202<-cor(vec[,i],vec[,j],method="spearman")
   m001a[i,j]<-ifelse(i>=j,a201,a202) ### Determines which number to put in the matrix
  }
 }
 for(i in 1:a101){
  for(j in 1:a101){
    m001a[i,j]<-round(m001a[i,j],n)
  }
 }
 a103<-a101-1
 for(i in 1:a103){
  a104<-i+1
  for(j in a104:4){
   m001a[i,j]<-ifelse(spear==TRUE,m001a[i,j],"") 
  }
 }
 for(i in 1:a101){
  m001a[i,i]<-1
 }
 return(m001a)
}

# wss plot for clustering

wssplot <- function(data, nc=15, seed=123){
               wss <- (nrow(data)-1)*sum(apply(data,2,var))
               for (i in 2:nc){
                    set.seed(seed)
                    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
                plot(1:nc, wss, type="b", xlab="Number of groups",
                     ylab="Sum of squares within a group")}

wssplotgg <- function(data,filename, nc=15, seed=123){
               wss <- (nrow(data)-1)*sum(apply(data,2,var))
               for (i in 2:nc){
                    set.seed(seed)
                    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
              wss<-as.data.frame(wss)
              names(wss)<-"wss"
              wss$cl<-seq(1:nrow(wss))
              g<-ggplot(wss,aes(x=cl,y=wss))+
              geom_point(color="blue")+
              geom_line(color="blue")+
              geom_hline(yintercept=0)+
              geom_vline(xintercept=0)+
              labs(x="Clusters",y="Within Sum of Squares") +
              coord_cartesian(xlim=c(0,100)) +
              theme(axis.title.x = element_text(vjust=0,size=100),
              axis.title.y = element_text(vjust=0,size=40,margin=margin(t=0,r=30,b=0,l=0)),
              axis.text = element_text(size=80),
              panel.grid.major = element_line(color="gray10", linetype = "dashed",  linewidth = .5),
              panel.grid.minor = element_line(color="gray90", linetype = "dashed", linewidth = .8))+
              theme_linedraw()

              g

              ggsave(filename, plot=g, height=4, width=4, units="in", dpi=300)

        }
 #               plot(1:nc, wss, type="b", xlab="Number of groups",
 #                    ylab="Sum of squares within a group")}

# User created TDABM plot


# New Plotting Functions


ColorIgraphPlot5a <- function( outputFromBallMapper, showVertexLabels = TRUE , ltext="Coloration", showLegend = FALSE , minimal_ball_radius = 7 , maximal_ball_scale=20, maximal_color_scale=10 , seed_for_plotting = -1 , store_in_file = "" , default_x_image_resolution = 1024 , default_y_image_resolution = 1024 , number_of_colors = 100, minc = -99999, maxc = 99999)
{
  vertices = outputFromBallMapper$vertices
  vertices[,2] <- maximal_ball_scale*vertices[,2]/max(vertices[,2])+minimal_ball_radius
  net = igraph::graph_from_data_frame(outputFromBallMapper$edges,vertices = vertices,directed = F)

  jet.colors <- grDevices::colorRampPalette(c("red","orange","yellow","green","cyan","blue","violet"))
  color_spectrum <- jet.colors( number_of_colors )

  #and over here we map the pallete to the order of values on vertices
  min_ <- ifelse(minc==-99999,min(outputFromBallMapper$coloring),minc)
  max_ <- ifelse(maxc==99999,max(outputFromBallMapper$coloring),maxc)
  
  #In some cases, the maximal values are achieved, and then the balls are not colored. To avoid this situaiton, we shift the values by a bit to get something slightly below min and slightly above max (relative to the distnace between the minimym and the maximum).
  min_ <- min_-0.01*(max_-min_)
  max_ <- max_+0.01*(max_-min_)
  print(max_)
  color <- vector(length = length(outputFromBallMapper$coloring),mode="double")
  for ( i in 1:length( outputFromBallMapper$coloring ) )
  {
    position <- base::max(base::ceiling(number_of_colors*(outputFromBallMapper$coloring[i]-min_)/(max_-min_)),1)
    color[ i ] <- color_spectrum [ position ]
  }
  igraph::V(net)$color <- color

  if ( showVertexLabels == FALSE  )igraph::V(net)$label = NA

  if ( seed_for_plotting != -1 )base::set.seed(seed_for_plotting)

  if ( store_in_file != "" ) grDevices::png(store_in_file, default_x_image_resolution, default_y_image_resolution)

  igraph::V(net)$label.cex = 2 #Change this line if you would like to have labels of different sizes.
 
  par(oma=c(0,0,0,2.5))
  par(mar=c(0,0,0,3.5))
  
  graphics::plot(net,edge.width=5)

  fields::image.plot(legend.only=T, zlim=c(min_,max_), col=color_spectrum, legend.width=1.5, legend.shrink=0.8, axis.args=list(cex.axis=1.5), legend.args=list(text=ltext,side=4, font=1, line=5, cex=1.8))

  if ( store_in_file != "" )grDevices::dev.off()

}#ColorIgraphPlot5a

ColorIgraphPlot5a10 <- function( outputFromBallMapper, showVertexLabels = TRUE , ltext="Coloration", showLegend = FALSE , minimal_ball_radius = 7 , maximal_ball_scale=20, maximal_color_scale=10 , seed_for_plotting = -1 , store_in_file = "" , default_x_image_resolution = 1024 , default_y_image_resolution = 1024 , number_of_colors = 100, minc = -99999, maxc = 99999)
{
  vertices = outputFromBallMapper$vertices
  vertices[,2] <- maximal_ball_scale*vertices[,2]/max(vertices[,2])+minimal_ball_radius
  net = igraph::graph_from_data_frame(outputFromBallMapper$edges,vertices = vertices,directed = F)

  jet.colors <- grDevices::colorRampPalette(c("red","orange","yellow","green","cyan","blue","violet"))
  color_spectrum <- jet.colors( number_of_colors )

  #and over here we map the pallete to the order of values on vertices
  min_ <- ifelse(minc==-99999,min(outputFromBallMapper$coloring),minc)
  max_ <- ifelse(maxc==99999,max(outputFromBallMapper$coloring),maxc)
  
  #In some cases, the maximal values are achieved, and then the balls are not colored. To avoid this situaiton, we shift the values by a bit to get something slightly below min and slightly above max (relative to the distnace between the minimym and the maximum).
  min_ <- min_-0.01*(max_-min_)
  max_ <- max_+0.01*(max_-min_)
  print(max_)
  color <- vector(length = length(outputFromBallMapper$coloring),mode="double")
  for ( i in 1:length( outputFromBallMapper$coloring ) )
  {
    position <- base::max(base::ceiling(number_of_colors*(outputFromBallMapper$coloring[i]-min_)/(max_-min_)),1)
    color[ i ] <- color_spectrum [ position ]
  }
  igraph::V(net)$color <- color

  if ( showVertexLabels == FALSE  )igraph::V(net)$label = NA

  if ( seed_for_plotting != -1 )base::set.seed(seed_for_plotting)

  if ( store_in_file != "" ) grDevices::png(store_in_file, default_x_image_resolution, default_y_image_resolution)

  igraph::V(net)$label.cex = 2 #Change this line if you would like to have labels of different sizes.
 
  par(oma=c(0,0,0,3))
  par(mar=c(0,0,0,3))
  
  graphics::plot(net,edge.width=1.2)

  fields::image.plot(legend.only=T, zlim=c(min_,max_), col=color_spectrum, legend.width=1.5, legend.shrink=0.8, axis.args=list(cex.axis=1.2), legend.args=list(text=ltext,side=4, font=1, line=3, cex=1.4))

  if ( store_in_file != "" )grDevices::dev.off()

}#ColorIgraphPlot5a10

ColorIgraphPlot5a5 <- function( outputFromBallMapper, showVertexLabels = TRUE , ltext="Coloration", showLegend = FALSE , minimal_ball_radius = 7 , maximal_ball_scale=20, maximal_color_scale=10 , seed_for_plotting = -1 , store_in_file = "" , default_x_image_resolution = 1024 , default_y_image_resolution = 1024 , number_of_colors = 100, minc = -99999, maxc = 99999)
{
  vertices = outputFromBallMapper$vertices
  vertices[,2] <- maximal_ball_scale*vertices[,2]/max(vertices[,2])+minimal_ball_radius
  net = igraph::graph_from_data_frame(outputFromBallMapper$edges,vertices = vertices,directed = F)

  jet.colors <- grDevices::colorRampPalette(c("red","orange","yellow","green","cyan","blue","violet"))
  color_spectrum <- jet.colors( number_of_colors )

  #and over here we map the pallete to the order of values on vertices
  min_ <- ifelse(minc==-99999,min(outputFromBallMapper$coloring),minc)
  max_ <- ifelse(maxc==99999,max(outputFromBallMapper$coloring),maxc)
  
  #In some cases, the maximal values are achieved, and then the balls are not colored. To avoid this situaiton, we shift the values by a bit to get something slightly below min and slightly above max (relative to the distnace between the minimym and the maximum).
  min_ <- min_-0.01*(max_-min_)
  max_ <- max_+0.01*(max_-min_)
  print(max_)
  color <- vector(length = length(outputFromBallMapper$coloring),mode="double")
  for ( i in 1:length( outputFromBallMapper$coloring ) )
  {
    position <- base::max(base::ceiling(number_of_colors*(outputFromBallMapper$coloring[i]-min_)/(max_-min_)),1)
    color[ i ] <- color_spectrum [ position ]
  }
  igraph::V(net)$color <- color

  if ( showVertexLabels == FALSE  )igraph::V(net)$label = NA

  if ( seed_for_plotting != -1 )base::set.seed(seed_for_plotting)

  if ( store_in_file != "" ) grDevices::png(store_in_file, default_x_image_resolution, default_y_image_resolution)

  igraph::V(net)$label.cex = 2 #Change this line if you would like to have labels of different sizes.
 
  par(oma=c(0,0,0,4))
  par(mar=c(0,0,0,4))
  
  graphics::plot(net,edge.width=1.2)

  fields::image.plot(legend.only=T, zlim=c(min_,max_), col=color_spectrum, legend.width=1.5, legend.shrink=0.8, axis.args=list(cex.axis=1.6), legend.args=list(text=ltext,side=4, font=1, line=4, cex=1.8))

  if ( store_in_file != "" )grDevices::dev.off()

}#ColorIgraphPlot5a5



bmsplot<-function(x,y,xtit,ytit,filename,z,w){
 
 # X axis from information provided to function

 xax<-x
 xl01<-min(xax)
 xl02<-max(xax)
 xlim=c(xl01,xl02)

 # Y line
 
 yl01<-0
 yl02<-0

 for(i in ncol(y)){
  a001<-min(y[,i]) # Minimum for column i
  a002<-max(y[,i]) # Maximum for column i 
  yl01<-ifelse(yl01<a001,yl01,a001)
  yl02<-ifelse(yl02>a002,yl02,a002)
 }

 ylin1<-y[,1]
 ylin0<-y[,2]
 ylin2<-y[,3]

 df001<-as.data.frame(cbind.data.frame(x,y))
 names(df001)<-c("xax","yax","ylow","yhigh")
 
 if(missing(z)){
   # 2 column ggplot


 g<-ggplot(df001,aes(x=xax,y=yax))+
 geom_ribbon(aes(ymin=ylin0,ymax=ylin2), alpha = .5, fill = "darkseagreen3", color= "transparent")+
 geom_line(color="aquamarine4",lwd=.7) +
 geom_abline(intercept=0,slope=0,color="black",size =1.5,linetype="dotdash")+
 labs(x=xtit,y=ytit) +
 coord_cartesian(xlim=c(xl01,xl02),ylim=c(yl01,yl02),expand=FALSE) +
 theme(axis.title.x = element_text(vjust=0,size=40),
 axis.title.y = element_text(vjust=0,size=40,margin=margin(t=0,r=30,b=0,l=0)),
 axis.text = element_text(size=30),
 panel.grid.major = element_line(color="gray10", linetype = "dashed",  linewidth = .5),
 panel.grid.minor = element_line(color="gray90", linetype = "dashed", linewidth = .8))

 g

 ggsave(filename,width=12,height=8)
 return(print("Only 1 axis included"))
 }
 
 zl01<-0
 zl02<-0

 for(i in ncol(z)){
  a001<-min(z[,i]) # Minimum for column i
  a002<-max(z[,i]) # Maximum for column i 
  zl01<-ifelse(zl01<a001,zl01,a001)
  zl02<-ifelse(zl02>a002,zl02,a002)
 }

 zlin1<-z[,1]
 zlin0<-z[,2]
 zlin2<-z[,3]

 # Overall limits for plot

 plim1<-min(yl01,zl01)
 plim2<-max(yl02,zl02)

 df001<-as.data.frame(cbind.data.frame(x,y,z))
 names(df001)<-c("xax","yax","ylow","yhigh","zax","zlow","zhigh")

 print(head(df001,1))
  

 # 2 column ggplot

 g<-ggplot(df001,aes(x=xax,y=yax))+
 geom_ribbon(aes(ymin=ylin0,ymax=ylin2), alpha = .5, fill = "darkseagreen3", color= "transparent")+
 geom_line(color="aquamarine4",lwd=.7) +
 geom_ribbon(aes(ymin=zlin0,ymax=zlin2), alpha = .5, fill = "firebrick", color= "transparent")+
 geom_line(aes(x=xax,y=zlin1),color="red",lwd=.7) +
 geom_abline(intercept=0,slope=0,color="black",size =1.5,linetype="dotdash")+
 labs(x=xtit,y=ytit) +
  coord_cartesian(xlim=c(xl01,xl02),ylim=c(plim1,plim2),expand=FALSE) +
 theme(axis.title.x = element_text(vjust=0,size=40),
 axis.title.y = element_text(vjust=0,size=40,margin=margin(t=0,r=30,b=0,l=0)),
 axis.text = element_text(size=30),
 panel.grid.major = element_line(color="gray10", linetype = "dashed",  linewidth = .5),
 panel.grid.minor = element_line(color="gray90", linetype = "dashed", linewidth = .8))

 g

 ggsave(filename,width=12,height=8)
 
 if(missing(w)){
 return(NA)
 }

 print("creating 3 axis")

 wl01<-0
 wl02<-0

 for(i in ncol(w)){
  a001<-min(w[,i]) # Minimum for column i
  a002<-max(w[,i]) # Maximum for column i 
  wl01<-ifelse(wl01<a001,wl01,a001)
  wl02<-ifelse(wl02>a002,wl02,a002)
 }

 wlin1<-w[,1]
 wlin0<-w[,2]
 wlin2<-w[,3]

 # Overall limits for plot

 plim1<-min(yl01,zl01,wl01)
 plim2<-max(yl02,zl02,wl02)

 df001<-as.data.frame(cbind.data.frame(x,y,z,w))
 names(df001)<-c("xax","yax","ylow","yhigh","zax","zlow","zhigh","wzx","wlow","whigh")

 # 3 column ggplot

 g<-ggplot(df001,aes(x=xax,y=yax))+
  geom_ribbon(aes(ymin=ylin0,ymax=ylin2), alpha = .5, fill = "darkseagreen3", color= "transparent")+
  geom_line(color="aquamarine4",lwd=.7) +
  geom_ribbon(aes(ymin=zlin0,ymax=zlin2), alpha = .5, fill = "firebrick", color= "transparent")+
  geom_line(aes(x=xax,y=zlin1),color="red",lwd=.7) +
  geom_ribbon(aes(ymin=wlin0,ymax=wlin2), alpha = .5, fill = "blue", color= "transparent")+
  geom_line(aes(x=xax,y=wlin1),color="blue",lwd=.7) +
  geom_abline(intercept=0,slope=0,color="black",size =1.5,linetype="dotdash")+
  labs(x=xtit,y=ytit) +
  coord_cartesian(xlim=c(xl01,xl02),ylim=c(plim1,plim2),expand=FALSE) +
  theme(axis.title.x = element_text(vjust=0,size=40),
  axis.title.y = element_text(vjust=0,size=40,margin=margin(t=0,r=30,b=0,l=0)),
  axis.text = element_text(size=30),
  panel.grid.major = element_line(color="gray10", linetype = "dashed",  linewidth = .5),
  panel.grid.minor = element_line(color="gray90", linetype = "dashed", linewidth = .8))

  g
  
 ggsave(filename,width=12,height=8) 
}

bmsummaryplots<-function(m001s,prefix,colline,poly,tabeps,dp){

epstab<-as.data.frame(tabeps)
names(epstab)<-c("eps")

# Set ggplot theme

theme_set(theme_linedraw())

# Number of balls

ytit<-"Number of Balls"
xtit<-"Ball Radius (Epsilon)"

a001<-max(max(m001s$n975),0)
a002<-min(min(m001s$n025),0)

filename<-paste0(prefix,"balls.png")
filename2<-paste0(prefix,"ballshr.png")
png(filename,width=1000,height=1000)
plot(m001s$eps,m001s$n975,type="l",xlab="Radius (Epsilon)",ylab="Number of Balls",ylim=c(a002,a001),cex.axis=1.5,cex.lab=1.5)
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$n025,rev(m001s$n975)),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
 
}
lines(m001s$eps,m001s$n025)
lines(m001s$eps,m001s$n975)
lines(m001s$eps,m001s$mn,lwd="3")

if(colline=="TRUE"){
 lines(m001s$eps,m001s$n025,col="blue")
 lines(m001s$eps,m001s$n975,col="blue")
 lines(m001s$eps,m001s$mn,lwd="3",col="blue")

}
dev.off()

x<-m001s$eps
y<-as.data.frame(cbind.data.frame(m001s$mn,m001s$n025,m001s$n975))

bmsplot(x,y,xtit,ytit,filename2)


# Ball Coloration

a001<-max(max(m001s$maxc975),0)
a002<-min(min(m001s$minc025),0)

filename<-paste0(prefix,"ballcolours.png")
filename2<-paste0(prefix,"ballcolourshr.png")
png(filename,width=1000,height=1000)
plot(m001s$eps,m001s$minc975,type="l",xlab="Radius (Epsilon)",ylab="Coloration",ylim=c(a002,a001),cex.axis=1.5,cex.lab=1.5)
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$minc025,rev(m001s$minc975)),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$maxc025,rev(m001s$maxc975)),col=adjustcolor("orangered",alpha.f=0.10), border=NA)
}
lines(m001s$eps,m001s$minc025)
lines(m001s$eps,m001s$minc975)
lines(m001s$eps,m001s$mminc,lwd="3")
lines(m001s$eps,m001s$maxc025)
lines(m001s$eps,m001s$maxc975)
lines(m001s$eps,m001s$mmaxc,lwd="3")
if(colline=="TRUE"){
 lines(m001s$eps,m001s$minc025,col="blue")
 lines(m001s$eps,m001s$minc975,col="blue")
 lines(m001s$eps,m001s$mminc,lwd="3",col="blue")
 lines(m001s$eps,m001s$maxc025,col="red")
 lines(m001s$eps,m001s$maxc975,col="red")
 lines(m001s$eps,m001s$mmaxc,lwd="3",col="red")
}
dev.off()

ytit<-"Coloration of Ball"
x<-m001s$eps
y<-as.data.frame(cbind.data.frame(m001s$mminc,m001s$minc025,m001s$minc975))
z<-as.data.frame(cbind.data.frame(m001s$mmaxc,m001s$maxc025,m001s$maxc975))

bmsplot(x,y,xtit,ytit,filename2,z)

# Ball Coloration

a001<-max(max(m001s$tmaxc975),0)
a002<-min(min(m001s$tminc025),0)

filename<-paste0(prefix,"ballcolourst.png")
filename2<-paste0(prefix,"ballcoloursthr.png")
png(filename,width=1000,height=1000)
plot(m001s$eps,m001s$tminc975,type="l",xlab="Radius (Epsilon)",ylab="Coloration",ylim=c(a002,a001),cex.axis=1.5,cex.lab=1.5)
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$tminc025,rev(m001s$tminc975)),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$tmaxc025,rev(m001s$tmaxc975)),col=adjustcolor("orangered",alpha.f=0.10), border=NA)
}
lines(m001s$eps,m001s$tminc025)
lines(m001s$eps,m001s$tminc975)
lines(m001s$eps,m001s$tmincm,lwd="3")
lines(m001s$eps,m001s$tmaxc025)
lines(m001s$eps,m001s$tmaxc975)
lines(m001s$eps,m001s$tmaxcm,lwd="3")
if(colline=="TRUE"){
 lines(m001s$eps,m001s$tminc025,col="blue")
 lines(m001s$eps,m001s$tminc975,col="blue")
 lines(m001s$eps,m001s$tmincm,lwd="3",col="blue")
 lines(m001s$eps,m001s$tmaxc025,col="red")
 lines(m001s$eps,m001s$tmaxc975,col="red")
 lines(m001s$eps,m001s$tmaxcm,lwd="3",col="red")
}
dev.off()

ytit<-"Coloration of Ball"
x<-m001s$eps
y<-as.data.frame(cbind.data.frame(m001s$tmincm,m001s$tminc025,m001s$tminc975))
z<-as.data.frame(cbind.data.frame(m001s$tmaxcm,m001s$tmaxc025,m001s$tmaxc975))

bmsplot(x,y,xtit,ytit,filename2,z)



# Standard deviation of Coloration

a001<-max(max(m001s$sdc975),0)
a002<-min(min(m001s$sdc025),0)

filename<-paste0(prefix,"colsd.png")
png(filename,width=1000,height=1000)
filename2<-paste0(prefix,"colsdhr.png")
plot(m001s$eps,m001s$sdc975,type="l",xlab="Radius (Epsilon)",ylab="Coloration Std Dev",ylim=c(a002,a001))
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$sdc025,rev(m001s$sdc975)),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
 
}
lines(m001s$eps,m001s$sdc025)
lines(m001s$eps,m001s$sdc975)
lines(m001s$eps,m001s$msdc,lwd="3")

if(colline=="TRUE"){
 lines(m001s$eps,m001s$sdc025,col="blue")
 lines(m001s$eps,m001s$sdc975,col="blue")
 lines(m001s$eps,m001s$msdc,lwd="3",col="blue")

}
dev.off()

ytit<-"Std Dev. of Coloration"

x<-m001s$eps
y<-as.data.frame(cbind.data.frame(m001s$msdc,m001s$sdc025,m001s$sdc975))

bmsplot(x,y,xtit,ytit,filename2)

# Standard deviation of Coloration (Threshold)

a001<-max(max(m001s$tsdc975),0)
a002<-min(min(m001s$tsdc025),0)

filename<-paste0(prefix,"colsdt.png")
png(filename,width=1000,height=1000)
filename2<-paste0(prefix,"colsdthr.png")
plot(m001s$eps,m001s$sdc975,type="l",xlab="Radius (Epsilon)",ylab="Coloration Std Dev",ylim=c(a002,a001))
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$tsdc025,rev(m001s$tsdc975)),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
 
}
lines(m001s$eps,m001s$tsdc025)
lines(m001s$eps,m001s$tsdc975)
lines(m001s$eps,m001s$tsdcm,lwd="3")

if(colline=="TRUE"){
 lines(m001s$eps,m001s$tsdc025,col="blue")
 lines(m001s$eps,m001s$tsdc975,col="blue")
 lines(m001s$eps,m001s$tsdcm,lwd="3",col="blue")

}
dev.off()

ytit<-"Std Dev. of Coloration"

x<-m001s$eps
y<-as.data.frame(cbind.data.frame(m001s$tsdcm,m001s$tsdc025,m001s$tsdc975))

bmsplot(x,y,xtit,ytit,filename2)

# Balls bigger than threshold

a001<-max(max(m001s$nthresh975),0)
a002<-min(min(m001s$nthresh025),0)

filename<-paste0(prefix,"nthresh.png")
filename2<-paste0(prefix,"nthreshhr.png")
png(filename,width=1000,height=1000)
plot(m001s$eps,m001s$nthresh975,type="l",xlab="Radius (Epsilon)",ylab="Larger than Threshold",ylim=c(a002,a001))
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$nthresh025,rev(m001s$nthresh975)),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
 
}
lines(m001s$eps,m001s$nthresh025)
lines(m001s$eps,m001s$nthresh975)
lines(m001s$eps,m001s$nthreshm,lwd="3")

if(colline=="TRUE"){
 lines(m001s$eps,m001s$nthresh025,col="blue")
 lines(m001s$eps,m001s$nthresh975,col="blue")
 lines(m001s$eps,m001s$nthreshm,lwd="3",col="blue")

}
dev.off()

ytit<-"Balls Above Threshold"
x<-m001s$eps
y<-as.data.frame(cbind.data.frame(m001s$nthreshm,m001s$nthresh025,m001s$nthresh975))

bmsplot(x,y,xtit,ytit,filename2)



# Ball size 

a001<-max(max(m001s$maxb975),0)
a002<-min(min(m001s$minb025),0)

filename<-paste0(prefix,"ballsize.png")
filename2<-paste0(prefix,"ballsizehr.png")
png(filename,width=1000,height=1000)
plot(m001s$eps,m001s$minb975,type="l",xlab="Radius (Epsilon)",ylab="Ball Size",ylim=c(a002,a001),cex.axis=1.5,cex.lab=1.5)
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$minb025,rev(m001s$minb975)),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
}
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$maxb025,rev(m001s$maxb975)),col=adjustcolor("orangered",alpha.f=0.10), border=NA)
}
lines(m001s$eps,m001s$minb025)
lines(m001s$eps,m001s$minb975)
lines(m001s$eps,m001s$mminb,lwd="3")
lines(m001s$eps,m001s$maxb025)
lines(m001s$eps,m001s$maxb975)
lines(m001s$eps,m001s$mmaxb,lwd="3")
if(colline=="TRUE"){
 lines(m001s$eps,m001s$minb025,col="blue")
 lines(m001s$eps,m001s$minb975,col="blue")
 lines(m001s$eps,m001s$mminb,lwd="3",col="blue")
 lines(m001s$eps,m001s$maxb025,col="red")
 lines(m001s$eps,m001s$maxb975,col="red")
 lines(m001s$eps,m001s$mmaxb,lwd="3",col="red")
}
dev.off()

ytit<-"Number of Points in Balls"
x<-m001s$eps
y<-as.data.frame(cbind.data.frame(m001s$mminb,m001s$minb025,m001s$minb975))
z<-as.data.frame(cbind.data.frame(m001s$mmaxb,m001s$maxb025,m001s$maxb975))

bmsplot(x,y,xtit,ytit,filename2,z)


# Range of Coloration

a001<-max(max(m001s$rangec975),0)
a002<-min(min(m001s$rangec025),0)

filename<-paste0(prefix,"rangec.png")
filename2<-paste0(prefix,"rangechr.png")
png(filename,width=1000,height=1000)
plot(m001s$eps,m001s$rangec975,type="l",xlab="Radius (Epsilon)",ylab="Range of Coloration",ylim=c(a002,a001),cex.axis=1.5,cex.lab=1.5)
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$rangec025,rev(m001s$rangec975)),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
 
}
lines(m001s$eps,m001s$rangec025)
lines(m001s$eps,m001s$rangec975)
lines(m001s$eps,m001s$mrangec,lwd="3")

if(colline=="TRUE"){
 lines(m001s$eps,m001s$rangec025,col="blue")
 lines(m001s$eps,m001s$rangec975,col="blue")
 lines(m001s$eps,m001s$mrangec,lwd="3",col="blue")

}
dev.off()

ytit<-"Range of Ball Coloration"
x<-m001s$eps
y<-as.data.frame(cbind.data.frame(m001s$mrangec,m001s$rangec025,m001s$rangec975))

bmsplot(x,y,xtit,ytit,filename2)

# Range of Coloration (Threshold)

a001<-max(max(m001s$trangec975),0)
a002<-min(min(m001s$trangec025),0)

filename<-paste0(prefix,"trangec.png")
filename2<-paste0(prefix,"trangechr.png")
png(filename,width=1000,height=1000)
plot(m001s$eps,m001s$trangec975,type="l",xlab="Radius (Epsilon)",ylab="Range of Coloration",ylim=c(a002,a001),cex.axis=1.5,cex.lab=1.5)
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$trangec025,rev(m001s$trangec975)),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
 
}
lines(m001s$eps,m001s$trangec025)
lines(m001s$eps,m001s$trangec975)
lines(m001s$eps,m001s$trangecm,lwd="3")

if(colline=="TRUE"){
 lines(m001s$eps,m001s$trangec025,col="blue")
 lines(m001s$eps,m001s$trangec975,col="blue")
 lines(m001s$eps,m001s$trangecm,lwd="3",col="blue")

}
dev.off()

ytit<-"Range of Ball Coloration"
x<-m001s$eps
y<-as.data.frame(cbind.data.frame(m001s$trangecm,m001s$trangec025,m001s$trangec975))

bmsplot(x,y,xtit,ytit,filename2)

# Range of ball size

a001<-max(max(m001s$rangeb975),0)
a002<-min(min(m001s$rangeb025),0)

filename<-paste0(prefix,"rangeb.png")
filename2<-paste0(prefix,"rangebhr.png")
png(filename,width=1000,height=1000)
plot(m001s$eps,m001s$rangeb975,type="l",xlab="Radius (Epsilon)",ylab="Range of Ball Size",ylim=c(a002,a001),cex.axis=1.5,cex.lab=1.5)
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$rangeb025,rev(m001s$rangeb975)),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
 
}
lines(m001s$eps,m001s$rangeb025)
lines(m001s$eps,m001s$rangeb975)
lines(m001s$eps,m001s$mrangeb,lwd="3")

if(colline=="TRUE"){
 lines(m001s$eps,m001s$rangeb025,col="blue")
 lines(m001s$eps,m001s$rangeb975,col="blue")
 lines(m001s$eps,m001s$mrangeb,lwd="3",col="blue")

}
dev.off()

ytit<-"Range of Ball Size"
x<-m001s$eps
y<-as.data.frame(cbind.data.frame(m001s$mrangeb,m001s$rangeb025,m001s$rangeb975))

bmsplot(x,y,xtit,ytit,filename2)

# Zero connected balls

a001<-max(max(m001s$zero975),0)
a002<-min(min(m001s$zero025),0)

filename<-paste0(prefix,"zero.png")
filename2<-paste0(prefix,"zerohr.png")
png(filename,width=1000,height=1000)
plot(m001s$eps,m001s$zero975,type="l",xlab="Radius (Epsilon)",ylab="Zero Connected Balls",ylim=c(a002,a001),cex.axis=1.5,cex.lab=1.5)
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$zero025,rev(m001s$zero975)),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
 
}
lines(m001s$eps,m001s$zero025)
lines(m001s$eps,m001s$zero975)
lines(m001s$eps,m001s$mzero,lwd="3")

if(colline=="TRUE"){
 lines(m001s$eps,m001s$zero025,col="blue")
 lines(m001s$eps,m001s$zero975,col="blue")
 lines(m001s$eps,m001s$mzero,lwd="3",col="blue")

}
dev.off()

ytit<-"Zero Connected Balls"
x<-m001s$eps
y<-as.data.frame(cbind.data.frame(m001s$mzero,m001s$zero025,m001s$zero975))

bmsplot(x,y,xtit,ytit,filename2)

# Average number of connections amongst connected balls

a001<-max(max(m001s$avcon975),0)
a002<-min(min(m001s$avcon025),0)

filename<-paste0(prefix,"avcon.png")
filename2<-paste0(prefix,"avconhr.png")
png(filename,width=1000,height=1000)
plot(m001s$eps,m001s$avcon975,type="l",xlab="Radius (Epsilon)",ylab="Average Connections",ylim=c(a002,a001),cex.axis=1.5,cex.lab=1.5)
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$avcon025,rev(m001s$avcon975)),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
 
}
lines(m001s$eps,m001s$avcon025)
lines(m001s$eps,m001s$avcon975)
lines(m001s$eps,m001s$mavcon,lwd="3")

if(colline=="TRUE"){
 lines(m001s$eps,m001s$avcon025,col="blue")
 lines(m001s$eps,m001s$avcon975,col="blue")
 lines(m001s$eps,m001s$mavcon,lwd="3",col="blue")

}
dev.off()

ytit<-"Average Connections"
x<-m001s$eps
y<-as.data.frame(cbind.data.frame(m001s$mavcon,m001s$avcon025,m001s$avcon975))

bmsplot(x,y,xtit,ytit,filename2)

# Total number of connections

a001<-max(max(m001s$con975),0)
a002<-min(min(m001s$con025),0)

filename<-paste0(prefix,"con.png")
filename2<-paste0(prefix,"conhr.png")
png(filename,width=1000,height=1000)
plot(m001s$eps,m001s$con975,type="l",xlab="Radius (Epsilon)",ylab="Total Connections",ylim=c(a002,a001),cex.axis=1.5,cex.lab=1.5)
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$con025,rev(m001s$con975)),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
 
}
lines(m001s$eps,m001s$con025)
lines(m001s$eps,m001s$con975)
lines(m001s$eps,m001s$mcon,lwd="3")

if(colline=="TRUE"){
 lines(m001s$eps,m001s$con025,col="blue")
 lines(m001s$eps,m001s$con975,col="blue")
 lines(m001s$eps,m001s$mcon,lwd="3",col="blue")

}
dev.off()


ytit<-"Average Connections"
x<-m001s$eps
y<-as.data.frame(cbind.data.frame(m001s$mcon,m001s$con025,m001s$con975))

bmsplot(x,y,xtit,ytit,filename2)

# Standard deviation of connections per ball

a001<-max(max(m001s$sdcon975),0)
a002<-min(min(m001s$sdcon025),0)

filename<-paste0(prefix,"sdcon.png")
filename2<-paste0(prefix,"sdconhr.png")
png(filename,width=1000,height=1000)
plot(m001s$eps,m001s$sdcon975,type="l",xlab="Radius (Epsilon)",ylab="Std Dev of Connections",ylim=c(a002,a001),cex.axis=1.5,cex.lab=1.5)
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$sdcon025,rev(m001s$sdcon975)),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
 
}
lines(m001s$eps,m001s$sdcon025)
lines(m001s$eps,m001s$sdcon975)
lines(m001s$eps,m001s$msdcon,lwd="3")

if(colline=="TRUE"){
 lines(m001s$eps,m001s$sdcon025,col="blue")
 lines(m001s$eps,m001s$sdcon975,col="blue")
 lines(m001s$eps,m001s$msdcon,lwd="3",col="blue")

}
dev.off()

ytit<-"Std Dev Connections"
x<-m001s$eps
y<-as.data.frame(cbind.data.frame(m001s$msdcon,m001s$sdcon025,m001s$sdcon975))

bmsplot(x,y,xtit,ytit,filename2)

# Within ball standard deviation

a001<-max(max(m001s$wsd975),0)
a002<-min(min(m001s$wsd025),0)

filename<-paste0(prefix,"wsd.png")
filename2<-paste0(prefix,"wsdhr.png")
png(filename,width=1000,height=1000)
plot(m001s$eps,m001s$wsd975,type="l",xlab="Radius (Epsilon)",ylab="Within Ball Std Dev",ylim=c(a002,a001),cex.axis=1.5,cex.lab=1.5)
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$wsd025,rev(m001s$wsd975)),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
 
}
lines(m001s$eps,m001s$wsd025)
lines(m001s$eps,m001s$wsd975)
lines(m001s$eps,m001s$wsdm,lwd="3")

if(colline=="TRUE"){
 lines(m001s$eps,m001s$wsd025,col="blue")
 lines(m001s$eps,m001s$wsd975,col="blue")
 lines(m001s$eps,m001s$wsdm,lwd="3",col="blue")

}
dev.off()

ytit<-"Within Ball Std Dev"
x<-m001s$eps
y<-as.data.frame(cbind.data.frame(m001s$wsdm,m001s$wsd025,m001s$wsd975))

bmsplot(x,y,xtit,ytit,filename2)

# Within ball standard deviation minimum and maximum (minimum 5 points)

a001<-max(max(m001s$wsdmax975),0)
a002<-min(min(m001s$wsdmin025),0)

filename<-paste0(prefix,"wsdminmax.png")
filename2<-paste0(prefix,"wsdminmaxhr.png")
png(filename,width=1000,height=1000)
plot(m001s$eps,m001s$wsdmin975,type="l",xlab="Radius (Epsilon)",ylab="Within Ball Std Dev",ylim=c(a002,a001),cex.axis=1.5,cex.lab=1.5)
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$wsdmin025,rev(m001s$wsdmin975)),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
}
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$wsdmax025,rev(m001s$wsdmax975)),col=adjustcolor("orangered",alpha.f=0.10), border=NA)
}
lines(m001s$eps,m001s$wsdmin025)
lines(m001s$eps,m001s$wsdmin975)
lines(m001s$eps,m001s$wsdminm,lwd="3")
lines(m001s$eps,m001s$wsdmax025)
lines(m001s$eps,m001s$wsdmax975)
lines(m001s$eps,m001s$wsdmaxm,lwd="3")
if(colline=="TRUE"){
 lines(m001s$eps,m001s$wsdmin025,col="blue")
 lines(m001s$eps,m001s$wsdmin975,col="blue")
 lines(m001s$eps,m001s$wsdminm,lwd="3",col="blue")
 lines(m001s$eps,m001s$wsdmax025,col="red")
 lines(m001s$eps,m001s$wsdmax975,col="red")
 lines(m001s$eps,m001s$wsdmaxm,lwd="3",col="red")
}
dev.off()

ytit<-"Within Ball Std Dev"
x<-m001s$eps
y<-as.data.frame(cbind.data.frame(m001s$wsdminm,m001s$wsdmin025,m001s$wsdmin975))
z<-as.data.frame(cbind.data.frame(m001s$wsdmaxm,m001s$wsdmax025,m001s$wsdmax975))

bmsplot(x,y,xtit,ytit,filename2,z)

# Within ball Coloration range minimum and maximum (minimum 5 points)

a001<-max(max(m001s$winmax975),0)
a002<-min(min(m001s$winmin025),0)

filename<-paste0(prefix,"winrminmax.png")
filename2<-paste0(prefix,"winrminmaxhr.png")
png(filename,width=1000,height=1000)
plot(m001s$eps,m001s$winmin975,type="l",xlab="Radius (Epsilon)",ylab="Within Ball Range",ylim=c(a002,a001),cex.axis=1.5,cex.lab=1.5)
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$winmin025,rev(m001s$winmin975)),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
}
if(poly=="TRUE"){
 polygon(x=c(m001s$eps,rev(m001s$eps)), y=c(m001s$winmax025,rev(m001s$winmax975)),col=adjustcolor("orangered",alpha.f=0.10), border=NA)
}
lines(m001s$eps,m001s$winmin025)
lines(m001s$eps,m001s$winmin975)
lines(m001s$eps,m001s$winminm,lwd="3")
lines(m001s$eps,m001s$winmax025)
lines(m001s$eps,m001s$winmax975)
lines(m001s$eps,m001s$winmaxm,lwd="3")
if(colline=="TRUE"){
 lines(m001s$eps,m001s$winmin025,col="blue")
 lines(m001s$eps,m001s$winmin975,col="blue")
 lines(m001s$eps,m001s$winminm,lwd="3",col="blue")
 lines(m001s$eps,m001s$winmax025,col="red")
 lines(m001s$eps,m001s$winmax975,col="red")
 lines(m001s$eps,m001s$winmaxm,lwd="3",col="red")
}
dev.off()

ytit<-"Within Ball Range"
x<-m001s$eps
y<-as.data.frame(cbind.data.frame(m001s$winminm,m001s$winmin025,m001s$winmin975))
z<-as.data.frame(cbind.data.frame(m001s$winmaxm,m001s$winmax025,m001s$winmax975))

bmsplot(x,y,xtit,ytit,filename2,z)

###################
# Summary table 1 #
###################

a001<-nrow(epstab)
stab1<-matrix(0,nrow=45,ncol=a001)

m001s$eps<-round(m001s$eps,10)
epstab$eps<-round(epstab$eps,10)

tempa<-merge(m001s,epstab,by="eps")



for(i in 1:a001){
 temp<-tempa[i,]

 stab1[1,i]<-epstab[i,1]
 stab1[2,i]<-round(temp$mn,dp) # Number of balls
 stab1[3,i]<-paste0("(",round(temp$sdn,dp),")")
 stab1[4,i]<-round(temp$mminc,dp) # Minimum Coloration
 stab1[5,i]<-paste0("(",round(temp$sminc,dp),")")
 stab1[6,i]<-round(temp$mmaxc,dp) # Maximum Coloration
 stab1[7,i]<-paste0("(",round(temp$smaxc,dp),")")
 stab1[8,i]<-round(temp$msdc,dp) # Standard deviation of Coloration
 stab1[9,i]<-paste0("(",round(temp$ssdc,dp),")")
 stab1[10,i]<-round(temp$mrangec,dp) # Range Coloration
 stab1[11,i]<-paste0("(",round(temp$srangec,dp),")")
 stab1[12,i]<-round(temp$mminb,dp) # Minimum ball size
 stab1[13,i]<-paste0("(",round(temp$sminb,dp),")")
 stab1[14,i]<-round(temp$mmaxb,dp) # Maximum ball size
 stab1[15,i]<-paste0("(",round(temp$smaxb,dp),")")
 stab1[16,i]<-round(temp$mrangeb,dp) # Range of ball size
 stab1[17,i]<-paste0("(",round(temp$srangeb,dp),")")
 stab1[18,i]<-round(temp$mzero,dp) # Zero connections
 stab1[19,i]<-paste0("(",round(temp$szero,dp),")")
 stab1[20,i]<-round(temp$mavcon,dp) # Average connections
 stab1[21,i]<-paste0("(",round(temp$savcon,dp),")")
 stab1[22,i]<-round(temp$mcon,dp) # Total connections
 stab1[23,i]<-paste0("(",round(temp$scon,dp),")")
 stab1[24,i]<-round(temp$wsdm,dp) # Within ball standard deviation
 stab1[25,i]<-paste0("(",round(temp$wsds,dp),")")
 stab1[26,i]<-round(temp$wsdminm,dp) # Minimum within ball standard deviation
 stab1[27,i]<-paste0("(",round(temp$wsdmins,dp),")")
 stab1[28,i]<-round(temp$wsdmaxm,dp) # Maximum within ball standard deviation
 stab1[29,i]<-paste0("(",round(temp$wsdmaxs,dp),")")
 stab1[30,i]<-round(temp$tmincm,dp) # Minimum Coloration meeting threshold
 stab1[31,i]<-paste0("(",round(temp$tmincs,dp),")")
 stab1[32,i]<-round(temp$tmaxcm,dp) # Maximum Coloration meeting threshold
 stab1[33,i]<-paste0("(",round(temp$tmaxcs,dp),")")
 stab1[34,i]<-round(temp$tsdcm,dp) # Maximum Coloration meeting threshold
 stab1[35,i]<-paste0("(",round(temp$tsdcs,dp),")")
 stab1[36,i]<-round(temp$trangecm,dp) # Maximum Coloration meeting threshold
 stab1[37,i]<-paste0("(",round(temp$trangecs,dp),")")
 stab1[38,i]<-round(temp$winrm,dp) # Within ball range
 stab1[39,i]<-paste0("(",round(temp$winrs,dp),")")
 stab1[40,i]<-round(temp$winsdm,dp) # Standard deviation of within ball ranges
 stab1[41,i]<-paste0("(",round(temp$winsds,dp),")")
 stab1[42,i]<-round(temp$winminm,dp) # Minimum within ball range
 stab1[43,i]<-paste0("(",round(temp$winmins,dp),")")
 stab1[44,i]<-round(temp$winmaxm,dp) # Maximum within ball range
 stab1[45,i]<-paste0("(",round(temp$winmaxs,dp),")")
}



filename2<-paste0(prefix,"sstats1.txt")

latout(stab1,filename2)

# Second table with confidence intervals

a001<-nrow(epstab)
stab1<-matrix(0,nrow=45,ncol=a001)

for(i in 1:a001){
 temp<-tempa[i,]
 stab1[1,i]<-epstab$eps[i]
 stab1[2,i]<-round(temp$mn,dp) # Number of balls
 stab1[3,i]<-paste0("(",round(temp$n025,dp),",",round(temp$n975,dp),")")
 stab1[4,i]<-round(temp$mminc,dp) # Minimum Coloration
 stab1[5,i]<-paste0("(",round(temp$minc025,dp),",",round(temp$minc975,dp),")")
 stab1[6,i]<-round(temp$mmaxc,dp) # Maximum Coloration
 stab1[7,i]<-paste0("(",round(temp$maxc025,dp),",",round(temp$maxc975,dp),")")
 stab1[8,i]<-round(temp$msdc,dp) # Standard deviation of Coloration
 stab1[9,i]<-paste0("(",round(temp$sdc025,dp),",",round(temp$sdc975,dp),")")
 stab1[10,i]<-round(temp$mrangec,dp) # Range Coloration
 stab1[11,i]<-paste0("(",round(temp$rangec025,dp),",",round(temp$rangec975,dp),")")
 stab1[12,i]<-round(temp$mminb,dp) # Minimum ball size
 stab1[13,i]<-paste0("(",round(temp$minb025,dp),",",round(temp$minb975,dp),")")
 stab1[14,i]<-round(temp$mmaxb,dp) # Maximum ball size
 stab1[15,i]<-paste0("(",round(temp$maxb025,dp),",",round(temp$maxb975,dp),")")
 stab1[16,i]<-round(temp$mrangeb,dp) # Range of ball size
 stab1[17,i]<-paste0("(",round(temp$rangeb025,dp),",",round(temp$rangeb975,dp),")")
 stab1[18,i]<-round(temp$mzero,dp) # Zero connections
 stab1[19,i]<-paste0("(",round(temp$zero025,dp),",",round(temp$zero975,dp),")")
 stab1[20,i]<-round(temp$mavcon,dp) # Average connections
 stab1[21,i]<-paste0("(",round(temp$avcon025,dp),",",round(temp$avcon975,dp),")")
 stab1[22,i]<-round(temp$mcon,dp) # Total connections
 stab1[23,i]<-paste0("(",round(temp$con025,dp),",",round(temp$con975,dp),")")
 stab1[24,i]<-round(temp$wsdm,dp) # Within ball standard deviation
 stab1[25,i]<-paste0("(",round(temp$wsd025,dp),",",round(temp$wsd975,dp),")")
 stab1[26,i]<-round(temp$wsdminm,dp) # Minimum within ball standard deviation
 stab1[27,i]<-paste0("(",round(temp$wsdmin025,dp),",",round(temp$wsdmin975,dp),")")
 stab1[28,i]<-round(temp$wsdmaxm,dp) # Maximum within ball standard deviation
 stab1[29,i]<-paste0("(",round(temp$wsdmax025,dp),",",round(temp$wsdmax975,dp),")")
 stab1[30,i]<-round(temp$tmincm,dp) # Minimum Coloration meeting threshold
 stab1[31,i]<-paste0("(",round(temp$tminc025,dp),",",round(temp$tminc975,dp),")")
 stab1[32,i]<-round(temp$tmaxcm,dp) # Maximum Coloration meeting threshold
 stab1[33,i]<-paste0("(",round(temp$tmaxc025,dp),",",round(temp$tmaxc975,dp),")")
 stab1[34,i]<-round(temp$tsdcm,dp) # Maximum Coloration meeting threshold
 stab1[35,i]<-paste0("(",round(temp$tsdc025,dp),",",round(temp$tsdc975,dp),")")
 stab1[36,i]<-round(temp$trangecm,dp) # Maximum Coloration meeting threshold
 stab1[37,i]<-paste0("(",round(temp$trangec025,dp),",",round(temp$trangec975,dp),")")
 stab1[38,i]<-round(temp$winrm,dp) # Within ball range
 stab1[39,i]<-paste0("(",round(temp$winr025,dp),",",round(temp$winr975,dp),")")
 stab1[40,i]<-round(temp$winsdm,dp) # Standard deviation of within ball ranges
 stab1[41,i]<-paste0("(",round(temp$winsd025,dp),",",round(temp$winsd975,dp),")")
 stab1[42,i]<-round(temp$winminm,dp) # Minimum within ball range
 stab1[43,i]<-paste0("(",round(temp$winmin025,dp),",",round(temp$winmin975,dp),")")
 stab1[44,i]<-round(temp$winmaxm,dp) # Maximum within ball range
 stab1[45,i]<-paste0("(",round(temp$winmax025,dp),",",round(temp$winmax975,dp),")")



}

filename2<-paste0(prefix,"sstats2.txt")

latout(stab1,filename2)

}



#####################
# X variation plots #
#####################
xvargraph<-function(xvs01,xdata,epstab,dp,prefix,colline=TRUE,poly=TRUE){

# Drop any missing values

xvs01<-subset(xvs01,is.na(xvs01[,1])==FALSE)

# Identify parameters from data

xvp01<-ncol(xvs01) # Column contain the radius
xvp02<-xvp01-1 # Number of columns containing results
xvp03<-xvp02/4 # Number of metrics considered (each has a mean, sd, q025 and q075 extension)
xvp04<-xvp03/3 # Number of variables given each produces a mean min and max

# To avoid error on final column being epsilon

names(xvs01)[xvp01]<-"eps"

# convert epstab to data frame and name

epstab<-as.data.frame(epstab)
names(epstab)<-c("eps")

# Rounding because of some transcription issues

xvs01$eps<-round(xvs01$eps,10)
epstab$eps<-round(epstab$eps,10)

# Graphs

for(kk in 1:xvp04){
 for(jj in 1:3){
  jj2<-(kk-1)*12+(jj-1)*4+1
  jj3<-jj2+1
  jj4<-jj2+2
  jj5<-jj2+3
  prefix2<-paste0(prefix,"v",kk,"m",jj) # Constructed using prefix then v and number of variable being considered, m and metric number
  filename1<-paste0(prefix2,".png") # PNG output for graph

  a001<-max(max(xvs01[,jj5]),0)
  a002<-min(min(xvs01[,jj4]),0)
  png(filename1,width=1000,height=1000)
  plot(xvs01[,xvp01],xvs01[,jj5],type="l",xlab="Ball Radius (Epsilon)",ylab="Within Variation",ylim=c(a002,a001),cex.axis=1.5,cex.lab=1.5)
  if(poly=="TRUE"){
    polygon(x=c(xvs01[,xvp01],rev(xvs01[,xvp01])), y=c(xvs01[,jj4],rev(xvs01[,jj5])),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
  }
  lines(xvs01[,xvp01],xvs01[,jj5])
  lines(xvs01[,xvp01],xvs01[,jj4])
  lines(xvs01[,xvp01],xvs01[,jj2],lwd="3")
  if(colline=="TRUE"){
   lines(xvs01[,xvp01],xvs01[,jj5],col="blue")
   lines(xvs01[,xvp01],xvs01[,jj4],col="blue")
   lines(xvs01[,xvp01],xvs01[,jj2],lwd="3",col="blue")
  } 
  dev.off()
  filename2<-paste0(prefix2,"hr.png") # PNG output for graph in ggplot style
  ytit<-"Within Variation"
  xtit<-"Ball Radius (Epsilon)"
  x<-xvs01[,xvp01]
  y<-as.data.frame(cbind.data.frame(xvs01[,jj2],xvs01[,jj4],xvs01[,jj5]))
  bmsplot(x,y,xtit,ytit,filename2)
 }
 prefix2<-paste0(prefix,"v",kk)
 kk01<-(kk-1)*12+1
 kk02<-kk01+2 # Mean lower bound
 kk03<-kk01+3 # Mean upper bound
 kk04<-kk01+4 # Minimum average
 kk05<-kk01+6 # Minimum lower bound
 kk06<-kk01+7 # Minimum upper bound
 kk07<-kk01+8 # Maximum average
 kk08<-kk01+10 # Maximum lower bound
 kk09<-kk01+11 # Maximum upper bound
 filename1<-paste0(prefix2,".png") # PNG output for graph
 a001<-max(max(xvs01[,kk09]),0)
 a002<-min(min(xvs01[,kk05]),0)
 png(filename1,width=1000,height=1000)
 plot(xvs01[,xvp01],xvs01[,kk09],type="l",xlab="Radius (Epsilon)",ylab="Within Variation",ylim=c(a002,a001),cex.axis=1.5,cex.lab=1.5)
 lines(xvs01[,xvp01],xvs01[,kk02])
 lines(xvs01[,xvp01],xvs01[,kk03])
 lines(xvs01[,xvp01],xvs01[,kk01],lwd="3")
 lines(xvs01[,xvp01],xvs01[,kk05],lty="dotted")
 lines(xvs01[,xvp01],xvs01[,kk06],lty="dotted")
 lines(xvs01[,xvp01],xvs01[,kk04],lwd="3",lty="dotted")
 lines(xvs01[,xvp01],xvs01[,kk08],lty="dotdash")
 lines(xvs01[,xvp01],xvs01[,kk09],lty="dotdash")
 lines(xvs01[,xvp01],xvs01[,kk07],lwd="3",lty="dotdash")
 if(colline=="TRUE"){
  lines(xvs01[,xvp01],xvs01[,kk02],col="blue")
  lines(xvs01[,xvp01],xvs01[,kk03],col="blue")
  lines(xvs01[,xvp01],xvs01[,kk01],lwd="3",col="blue")
  lines(xvs01[,xvp01],xvs01[,kk05],col="red")
  lines(xvs01[,xvp01],xvs01[,kk06],col="red")
  lines(xvs01[,xvp01],xvs01[,kk04],lwd="3",col="red")
  lines(xvs01[,xvp01],xvs01[,kk08],col="darkgreen")
  lines(xvs01[,xvp01],xvs01[,kk09],col="darkgreen")
  lines(xvs01[,xvp01],xvs01[,kk07],lwd="3",col="darkgreen")
  } 
  if(poly=="TRUE"){
    polygon(x=c(xvs01[,xvp01],rev(xvs01[,xvp01])), y=c(xvs01[,kk02],rev(xvs01[,kk03])),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
    polygon(x=c(xvs01[,xvp01],rev(xvs01[,xvp01])), y=c(xvs01[,kk05],rev(xvs01[,kk06])),col=adjustcolor("red",alpha.f=0.10), border=NA)
    polygon(x=c(xvs01[,xvp01],rev(xvs01[,xvp01])), y=c(xvs01[,kk08],rev(xvs01[,kk09])),col=adjustcolor("green",alpha.f=0.10), border=NA)
  }
 dev.off()
 filename2<-paste0(prefix2,"hr.png") # PNG output for graph in ggplot style
 ytit<-"Within Variation"
 xtit<-"Ball Radius (Epsilon)"
 x<-xvs01[,xvp01]
 y<-as.data.frame(cbind.data.frame(xvs01[,kk01],xvs01[,kk02],xvs01[,kk03]))
 z<-as.data.frame(cbind.data.frame(xvs01[,kk04],xvs01[,kk05],xvs01[,kk06]))
 w<-as.data.frame(cbind.data.frame(xvs01[,kk07],xvs01[,kk08],xvs01[,kk09]))
 bmsplot(x,y,xtit,ytit,filename2,z,w)
}


# Blank tables

xvs02<-merge(xvs01,epstab,by="eps")

xvp05<-nrow(epstab) # Number of epsilon being considered
xvp06<-xvp03*2
xvp06<-xvp06+1 # Two rows for each metric plus top epsilon row
xvtab<-as.data.frame(matrix(0,nrow=xvp06,ncol=xvp05))

for(aa in 1:xvp05){
 temp<-xvs02[aa,]
 for(bb in 1:xvp04){
  bb01<-(bb-1)*12
  bb01<-bb01+2 # column containing mean of mean
  bb02<-bb01+2 # column containing q025 of mean
  bb03<-bb01+3 # column containing q975 of mean
  bb04<-bb01+4 # column containing mean of min
  bb05<-bb01+6 # column containing q025 of min
  bb06<-bb01+7 # column containing q975 of min
  bb07<-bb01+8 # column containing mean of max
  bb08<-bb01+10 # column containing q025 of max
  bb09<-bb01+11 # column containing q975 of max
  aa01<-(bb-1)*6
  aa01<-aa01+2 # Row containing values of mean
  aa02<-aa01+1 # Row to contain confidence interval of mean
  aa03<-aa01+2 # Row containing values of mean
  aa04<-aa01+3 # Row to contain confidence interval of mean
  aa05<-aa01+4 # Row containing values of mean
  aa06<-aa01+5 # Row to contain confidence interval of mean
  xvtab[1,aa]<-temp$eps[1]
  xvtab[aa01,aa]<-round(temp[1,bb01],dp)
  xvtab[aa02,aa]<-paste0("(",round(temp[1,bb02],dp),",",round(temp[1,bb03],dp),")") # Brackets with the 2.5 and 97.5 quantiles 
  xvtab[aa03,aa]<-round(temp[1,bb04],dp)
  xvtab[aa04,aa]<-paste0("(",round(temp[1,bb05],dp),",",round(temp[1,bb06],dp),")") # Brackets with the 2.5 and 97.5 quantiles 
  xvtab[aa05,aa]<-round(temp[1,bb07],dp)
  xvtab[aa06,aa]<-paste0("(",round(temp[1,bb08],dp),",",round(temp[1,bb09],dp),")") # Brackets with the 2.5 and 97.5 quantiles 
  }
 } 

filename2<-paste0(prefix,"abs.txt")
latout(xvtab,filename2)

# Standardisation

sdmat<-as.data.frame(matrix(0,nrow=xvp04,ncol=1))
names(sdmat)<-"sdx"
for(ii in 1:xvp04){
 sdmat$sdx[ii]<-sd(xdata[,ii])
}

xvs01a<-xvs01 # store original

for(ii in 1:xvp04){
 xvp06<-(ii-1)*12+1
 xvp07<-ii*12
 xvs01[,xvp06:xvp07]<-xvs01a[,xvp06:xvp07]/sdmat$sdx[ii]
}

for(kk in 1:xvp04){
 for(jj in 1:3){
  jj2<-(kk-1)*12+(jj-1)*4+1
  jj3<-jj2+1
  jj4<-jj2+2
  jj5<-jj2+3
  prefix2<-paste0(prefix,"v",kk,"m",jj) # Constructed using prefix then v and number of variable being considered, m and metric number
  filename1<-paste0(prefix2,"s.png") # PNG output for graph with S added for standardised version
  a001<-max(max(xvs01[,jj5]),0)
  a002<-min(min(xvs01[,jj4]),0)
  png(filename1,width=1000,height=1000)
  plot(xvs01[,xvp01],xvs01[,jj5],type="l",xlab="Radius (Epsilon)",ylab="Within Variation",ylim=c(a002,a001),cex.axis=1.5,cex.lab=1.5)
  if(poly=="TRUE"){
    polygon(x=c(xvs01[,xvp01],rev(xvs01[,xvp01])), y=c(xvs01[,jj4],rev(xvs01[,jj5])),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
  }
  lines(xvs01[,xvp01],xvs01[,jj5])
  lines(xvs01[,xvp01],xvs01[,jj4])
  lines(xvs01[,xvp01],xvs01[,jj2],lwd="3")
  if(colline=="TRUE"){
   lines(xvs01[,xvp01],xvs01[,jj5],col="blue")
   lines(xvs01[,xvp01],xvs01[,jj4],col="blue")
   lines(xvs01[,xvp01],xvs01[,jj2],lwd="3",col="blue")
  } 
  dev.off()
  filename2<-paste0(prefix2,"shr.png") # PNG output for graph in ggplot style
  ytit<-"Within Variation"
  xtit<-"Ball Radius (Epsilon)"
  x<-xvs01[,xvp01]
  y<-as.data.frame(cbind.data.frame(xvs01[,jj2],xvs01[,jj4],xvs01[,jj5]))
  bmsplot(x,y,xtit,ytit,filename2)
 }
 prefix2<-paste0(prefix,"v",kk)
 kk01<-(kk-1)*12+1
 kk02<-kk01+2 # Mean lower bound
 kk03<-kk01+3 # Mean upper bound
 kk04<-kk01+4 # Minimum average
 kk05<-kk01+6 # Minimum lower bound
 kk06<-kk01+7 # Minimum upper bound
 kk07<-kk01+8 # Maximum average
 kk08<-kk01+10 # Maximum lower bound
 kk09<-kk01+11 # Maximum upper bound
 filename1<-paste0(prefix2,"s.png") # PNG output for graph
 a001<-max(max(xvs01[,kk09]),0)
 a002<-min(min(xvs01[,kk05]),0)
 png(filename1,width=1000,height=1000)
 plot(xvs01[,xvp01],xvs01[,kk09],type="l",xlab="Radius (Epsilon)",ylab="Within Variation",ylim=c(a002,a001),cex.axis=1.5,cex.lab=1.5)
 lines(xvs01[,xvp01],xvs01[,kk02])
 lines(xvs01[,xvp01],xvs01[,kk03])
 lines(xvs01[,xvp01],xvs01[,kk01],lwd="3")
 lines(xvs01[,xvp01],xvs01[,kk05],lty="dotted")
 lines(xvs01[,xvp01],xvs01[,kk06],lty="dotted")
 lines(xvs01[,xvp01],xvs01[,kk04],lwd="3",lty="dotted")
 lines(xvs01[,xvp01],xvs01[,kk08],lty="dotdash")
 lines(xvs01[,xvp01],xvs01[,kk09],lty="dotdash")
 lines(xvs01[,xvp01],xvs01[,kk07],lwd="3",lty="dotdash")
 if(colline=="TRUE"){
  lines(xvs01[,xvp01],xvs01[,kk02],col="blue")
  lines(xvs01[,xvp01],xvs01[,kk03],col="blue")
  lines(xvs01[,xvp01],xvs01[,kk01],lwd="3",col="blue")
  lines(xvs01[,xvp01],xvs01[,kk05],col="red")
  lines(xvs01[,xvp01],xvs01[,kk06],col="red")
  lines(xvs01[,xvp01],xvs01[,kk04],lwd="3",col="red")
  lines(xvs01[,xvp01],xvs01[,kk08],col="darkgreen")
  lines(xvs01[,xvp01],xvs01[,kk09],col="darkgreen")
  lines(xvs01[,xvp01],xvs01[,kk07],lwd="3",col="darkgreen")
  } 
  if(poly=="TRUE"){
    polygon(x=c(xvs01[,xvp01],rev(xvs01[,xvp01])), y=c(xvs01[,kk02],rev(xvs01[,kk03])),col=adjustcolor("dodgerblue",alpha.f=0.10), border=NA)
    polygon(x=c(xvs01[,xvp01],rev(xvs01[,xvp01])), y=c(xvs01[,kk05],rev(xvs01[,kk06])),col=adjustcolor("red",alpha.f=0.10), border=NA)
    polygon(x=c(xvs01[,xvp01],rev(xvs01[,xvp01])), y=c(xvs01[,kk08],rev(xvs01[,kk09])),col=adjustcolor("green",alpha.f=0.10), border=NA)
  }
  abline(h=1,lty="dashed")
  abline(h=2,lty="dashed")
 dev.off()
 filename2<-paste0(prefix2,"shr.png") # PNG output for graph in ggplot style
 ytit<-"Within Variation"
 xtit<-"Ball Radius (Epsilon)"
 x<-xvs01[,xvp01]
 y<-as.data.frame(cbind.data.frame(xvs01[,kk01],xvs01[,kk02],xvs01[,kk03]))
 z<-as.data.frame(cbind.data.frame(xvs01[,kk04],xvs01[,kk05],xvs01[,kk06]))
 w<-as.data.frame(cbind.data.frame(xvs01[,kk07],xvs01[,kk08],xvs01[,kk09]))
 bmsplot(x,y,xtit,ytit,filename2,z,w)
}

# Blank tables

xvp05<-nrow(epstab) # Number of epsilon being considered
xvp06<-xvp03*2
xvp06<-xvp06+1 # Two rows for each metric plus top epsilon row
xvtab<-as.data.frame(matrix(0,nrow=xvp06,ncol=xvp05))

for(aa in 1:xvp05){
 temp<-xvs02[aa,]
 for(bb in 1:xvp04){
  bb01<-(bb-1)*12
  bb01<-bb01+2 # column containing mean of mean
  bb02<-bb01+2 # column containing q025 of mean
  bb03<-bb01+3 # column containing q975 of mean
  bb04<-bb01+4 # column containing mean of min
  bb05<-bb01+6 # column containing q025 of min
  bb06<-bb01+7 # column containing q975 of min
  bb07<-bb01+8 # column containing mean of max
  bb08<-bb01+10 # column containing q025 of max
  bb09<-bb01+11 # column containing q975 of max
  aa01<-(bb-1)*6
  aa01<-aa01+2 # Row containing values of mean
  aa02<-aa01+1 # Row to contain confidence interval of mean
  aa03<-aa01+2 # Row containing values of mean
  aa04<-aa01+3 # Row to contain confidence interval of mean
  aa05<-aa01+4 # Row containing values of mean
  aa06<-aa01+5 # Row to contain confidence interval of mean
  xvtab[1,aa]<-temp$eps[1]
  xvtab[aa01,aa]<-round(temp[1,bb01],dp)
  xvtab[aa02,aa]<-paste0("(",round(temp[1,bb02],dp),",",round(temp[1,bb03],dp),")") # Brackets with the 2.5 and 97.5 quantiles 
  xvtab[aa03,aa]<-round(temp[1,bb04],dp)
  xvtab[aa04,aa]<-paste0("(",round(temp[1,bb05],dp),",",round(temp[1,bb06],dp),")") # Brackets with the 2.5 and 97.5 quantiles 
  xvtab[aa05,aa]<-round(temp[1,bb07],dp)
  xvtab[aa06,aa]<-paste0("(",round(temp[1,bb08],dp),",",round(temp[1,bb09],dp),")") # Brackets with the 2.5 and 97.5 quantiles 
  }
 } 

filename2<-paste0(prefix,"sta.txt")
latout(xvtab,filename2)

}

ColorIgraphPlot5a <- function( outputFromBallMapper, showVertexLabels = TRUE , ltext="Coloration", showLegend = FALSE , minimal_ball_radius = 7 , maximal_ball_scale=20, maximal_color_scale=10 , seed_for_plotting = -1 , store_in_file = "" , default_x_image_resolution = 1024 , default_y_image_resolution = 1024 , number_of_colors = 100, minc = -99999, maxc = 99999)
{
  vertices = outputFromBallMapper$vertices
  vertices[,2] <- maximal_ball_scale*vertices[,2]/max(vertices[,2])+minimal_ball_radius
  net = igraph::graph_from_data_frame(outputFromBallMapper$edges,vertices = vertices,directed = F)

  jet.colors <- grDevices::colorRampPalette(c("red","orange","yellow","green","cyan","blue","violet"))
  color_spectrum <- jet.colors( number_of_colors )

  #and over here we map the pallete to the order of values on vertices
  min_ <- ifelse(minc==-99999,min(outputFromBallMapper$coloring),minc)
  max_ <- ifelse(maxc==99999,max(outputFromBallMapper$coloring),maxc)
  
  #In some cases, the maximal values are achieved, and then the balls are not colored. To avoid this situaiton, we shift the values by a bit to get something slightly below min and slightly above max (relative to the distnace between the minimym and the maximum).
  min_ <- min_-0.01*(max_-min_)
  max_ <- max_+0.01*(max_-min_)
  print(max_)
  color <- vector(length = length(outputFromBallMapper$coloring),mode="double")
  for ( i in 1:length( outputFromBallMapper$coloring ) )
  {
    position <- base::max(base::ceiling(number_of_colors*(outputFromBallMapper$coloring[i]-min_)/(max_-min_)),1)
    color[ i ] <- color_spectrum [ position ]
  }
  igraph::V(net)$color <- color

  if ( showVertexLabels == FALSE  )igraph::V(net)$label = NA

  if ( seed_for_plotting != -1 )base::set.seed(seed_for_plotting)

  if ( store_in_file != "" ) grDevices::png(store_in_file, default_x_image_resolution, default_y_image_resolution)

  igraph::V(net)$label.cex = 2 #Change this line if you would like to have labels of different sizes.
 
  par(oma=c(0,0,0,2.5))
  par(mar=c(0,0,0,3.5))
  
  graphics::plot(net,edge.width=5)

  fields::image.plot(legend.only=T, zlim=c(min_,max_), col=color_spectrum, legend.width=1.5, legend.shrink=0.8, axis.args=list(cex.axis=1.5), legend.args=list(text=ltext,side=4, font=1, line=5, cex=1.8))

  if ( store_in_file != "" )grDevices::dev.off()

  return(net)
}#ColorIgraphPlot5a

