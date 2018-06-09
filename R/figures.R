
source('R/variance.R')
library('effsize')
library(svglite)

# groupcolors <- list( 'young'=c('#ff8200ff','#ff82002f'), 'aging'=c('#c400c4ff','#c400c42f') )

colorset <- list()

colorset[['youngactS']] <- '#e51636ff' # "York red"
colorset[['youngactT']] <- '#e516362f'
colorset[['youngpasS']] <- '#ff8200ff' # orange
colorset[['youngpasT']] <- '#ff82002f'

colorset[['agingactS']] <- '#005de4ff' # blue
colorset[['agingactT']] <- '#005de42f'
colorset[['agingpasS']] <- '#2ab2f2ff' # lighter blue
colorset[['agingpasT']] <- '#2ab2f22f'

colorset[['extra1S']] <- '#c400c4ff' # purple
colorset[['extra1T']] <- '#c400c42f'

plotVarianceMLE <- function(target='notebook') {
  
  if (target == 'svg') {
    svglite(file='doc/fig/Fig2_variance-scatter.svg',width=7,height=7,system_fonts=list(sans='Arial'))
    par(mfrow=c(2,2),mar=c(4.1,4.1,1.1,1.1))
  } else {
    par(mfrow=c(1,2),mar=c(4.1,4.1,1.1,1.1))
  }
  
  plotVariancesScatters(group='young')
  plotVariancesScatters(group='aging')
  # plotVariancesRatios(group='young')
  # plotVariancesRatios(group='aging')
  
  if (target == 'svg') {
    dev.off()
  }
  
}

plotVarianceAge <- function(target='notebook') {
  
  if (target == 'svg') {
    svglite(file='doc/fig/Fig3_variance-age.svg',width=7,height=7,system_fonts=list(sans='Arial'))
    par(mfrow=c(2,2))
  } else {
    par(mfrow=c(1,2))
  }
  
  plotVarianceDensityComparison(task='active',groups=c('young','aging'))
  plotVarianceDensityComparison(task='passive',groups=c('young','aging'))
  
  if (target == 'svg') {
    dev.off()
  }
  
}

plotVarianceRecalibration <- function(target='notebook') {

  if (target == 'svg') {
    svglite(file='doc/fig/Fig4_variance-recalibration.svg',width=7,height=7,system_fonts=list(sans='Arial'))
  }
  
  par(mfrow=c(2,2))
  
  plotVariancesRecalibrationScatter(group='young',task='active')
  plotVariancesRecalibrationScatter(group='aging',task='active')
  plotVariancesRecalibrationScatter(group='young',task='passive')
  plotVariancesRecalibrationScatter(group='aging',task='passive')
  
  if (target == 'svg') {
    dev.off()
  }
  
}

# plotVariancesRatios <- function(group='young') {
#   
#   dfs <- loadVarCIs()
#   
#   df <- dfs[[group]]
#   
#   idx <- order(df$p500)
#   
#   plot(-1000,-1000,main=group,xlim=c(0.5,2.0),ylim=c(0,length(idx)+1),axes=F,xlab='variance: passive / active',ylab='participant')
#   # Xs <- c()
#   for (pp.idx in c(1:length(idx))) {
#     
#     X <- c(df$p025[idx[pp.idx]],df$p975[idx[pp.idx]],df$p975[idx[pp.idx]],df$p025[idx[pp.idx]]) / df$a500[idx[pp.idx]]
#     Y <- c(pp.idx, pp.idx, pp.idx+0.75, pp.idx+0.75)
#     
#     color <- rgb(0.5,0.5,0.5,0.5)
#     if (df$p025[idx[pp.idx]] > df$a500[idx[pp.idx]]) {
#       color <- rgb(0,0,1,1)
#     }
#     if (df$p975[idx[pp.idx]] < df$a500[idx[pp.idx]]) {
#       color <- rgb(1,0,0,1)
#     }
#     
#     # Xs <- c(Xs, X)
#     polygon(X,Y,border=NA,col=color)
#     
#     # lines(rep(df$a500[idx[pp.idx]],2),c(pp.idx,pp.idx+0.75),col=rgb(1,0,0))
#     
#   }
#   # print(range(Xs))
#   lines(c(1,1),c(1,length(idx)+0.75),col=rgb(0,0,0))
#   lines(c(5/6,5/6),c(1,length(idx)+0.75),col=rgb(0.5,0.5,0.5,.5),lty=1)
#   lines(c(6/5,6/5),c(1,length(idx)+0.75),col=rgb(0.5,0.5,0.5,.5),lty=1)
#   
#   axis(side=1,at=c(0.5,5/6,1.0,6/5,1.5,2.0),labels=c('1/2','5/6','1/1','6/5','3/2','2/1'))
#   axis(side=2,at=c(1,20,40,60,80))
#   
# }

plotVariancesScatters <- function(group='young',verbose=FALSE) {
  
  dfs <- loadVarCIs()
  
  df <- dfs[[group]]
  
  idx <- order(df$p500)
  
  # plot(-1000,-1000,log-'xy',main=group,xlim=c(1,250),ylim=c(1,250),axes=F,xlab='active variance',ylab='passive variance',asp=1)
  plot(-1000,-1000,main=group,xlim=c(0,20),ylim=c(0,20),axes=F,xlab=bquote(.('active') ~ sigma),ylab=bquote(.('passive') ~ sigma),asp=1)
  
  logP500 <- sqrt(df$p500)
  logA500 <- sqrt(df$a500)
  
  regmod <- lm(formula = logP500 ~ logA500)
  # lines(c(0,5.5),(c(0,5.5)*coef(regmod)[2])  + coef(regmod)[1], col=rgb(1,0,1), lty=2 )
  
  X <- seq(0,20,.001)
  Yci <- predict( regmod,
                newdata = data.frame( logA500=X ),
                interval = "confidence" )
  Yupr <- Yci[,'upr']
  Ylwr <- Yci[,'lwr']
  
  polyX <- c(X,rev(X))
  polyY <- c(Yupr,rev(Ylwr))
  polygon(polyX,polyY,col=colorset[['extra1T']],border=NA)
  
  abX <- c(0,20)
  inter <- regmod$coefficients[1]
  slope <- regmod$coefficients[2]
  lines(abX,(slope*abX)+inter,col=colorset[['extra1S']],lty=2)
  
  for (pp.idx in c(1:length(idx))) {
    
    # X <- sqrt( c(df$a025[idx[pp.idx]],df$a975[idx[pp.idx]],df$a975[idx[pp.idx]],df$a025[idx[pp.idx]]) )
    # Y <- sqrt( c(df$p025[idx[pp.idx]],df$p025[idx[pp.idx]],df$p975[idx[pp.idx]],df$p975[idx[pp.idx]]) )
    
    color <- rgb(0.5,0.5,0.5)
    if (df$p025[idx[pp.idx]] > df$a500[idx[pp.idx]]) {
      # color <- rgb(0,0,1)
      color <- colorset[[sprintf('%spasS',group)]]
    }
    if (df$p975[idx[pp.idx]] < df$a500[idx[pp.idx]]) {
      # color <- rgb(1,0,0)
      color <- colorset[[sprintf('%sactS',group)]]
    }
    
    # Xs <- c(Xs, X)
    # polygon(X,Y,border=NA,col=color)
    points(sqrt(df$a500[idx[pp.idx]]),sqrt(df$p500[idx[pp.idx]]),col=color)
    
    # lines(rep(df$a500[idx[pp.idx]],2),c(pp.idx,pp.idx+0.75),col=rgb(1,0,0))
    
  }
  # print(range(Xs))
  
  # identity line
  lines(c(0,20),c(0,20),col=rgb(0,0,0),lty=1)
  
  # logarithmic stuff:
  # axis(side=1,at=log(c(1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,200,300)),labels=c('','2','','','','','','','','','20','','','','','','','','','200',''))
  # axis(side=2,at=log(c(1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,200,300)),labels=c('','2','','','','','','','','','20','','','','','','','','','200',''))
  
  # SD axis:
  axis(side=1,at=c(0,5,10,15,20))
  axis(side=2,at=c(0,5,10,15,20))
  
  if(verbose) {
    
    print(summary(regmod))
    
    print(rbind( data.frame(confint(regmod, '(Intercept)', level=0.95)), 
                 data.frame(confint(regmod, 'a500', level=0.95)) ))
    
    print(t.test(log(df$p500),log(df$a500),paired=TRUE))
    
    print(cohen.d(log(df$p500),log(df$a500),paired=TRUE))
    
    print(mean(df$a500))
    print(mean(df$p500))
    
  }
}

plotVarianceDensityComparison <- function(task,groups) {
  
  dfs <- loadVarCIs()
  
  # groupcolors <- list( 'young'=c('#ff8200ff','#ff82002f'), 'aging'=c('#c400c4ff','#c400c42f') )
  
  groupttestdata <- list()
  
  densitylinesY <- list()
  densitylinesX <- list()
  
  medians <- list()
  
  legendcolors <- c()
  
  plot(-1000,-1000,main=sprintf('%s localization',task),xlim=c(0,sqrt(400)),ylim=c(0,0.16),axes=F,xlab=expression(sqrt(sigma^2)),ylab='density')
  
  for (group in groups) {
    
    df <- dfs[[group]]
    
    varname <- sprintf('%s500',substr(task,1,1))
    
    data <- density(sqrt(df[,varname]),from=0, n=1024)
    
    CI <- t.interval(sqrt(df[,varname]))
    idx <- which(data$x > CI[1] & data$x < CI[2])
    
    # X <- c(data$x[idx], rev(data$x[idx]))
    # Y <- c(data$y[idx], rep(0,length(idx)))
    X <- c(data$x, rev(data$x))
    Y <- c(data$y, rep(0,length(data$y)))
    polygon(X,Y,border=NA,col=colorset[[sprintf('%s%sT',group,substr(task,1,3))]])
    
    densitylinesY[[group]] <- data$y
    densitylinesX[[group]] <- data$x
    thismedian <- median(sqrt(df[,varname]))
    x.idx <- which(abs(data$x - thismedian) == min(abs(data$x - thismedian)))
    medians[[group]] <- c(thismedian,data$y[x.idx])
    groupttestdata[[group]] <- sqrt(df[,varname])
    
  }
  
  for (group in groups) {
    
    lines(densitylinesX[[group]], densitylinesY[[group]], col=colorset[[sprintf('%s%sS',group,substr(task,1,3))]], lty=1)
    lines(rep(medians[[group]][1],2),c(0,medians[[group]][2]), lty=2, col=colorset[[sprintf('%s%sS',group,substr(task,1,3))]])
    
    legendcolors <- c(legendcolors,colorset[[sprintf('%s%sS',group,substr(task,1,3))]])
    
  }
  
  legend(10,.15,legend=groups,col=legendcolors,lty=1,bty='n')
  
  # cat(sprintf('\n*** %s - %s, %s\n', task, groups[[1]], groups[[2]]))
  # print(t.test(log(groupttestdata[[groups[1]]]),log(groupttestdata[[groups[2]]]),alternative='l'))
  # 
  # print(cohen.d(log(groupttestdata[[groups[1]]]),log(groupttestdata[[groups[2]]]),alternative='l'))
  
  # axis(side=1,at=sqrt(c(1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,200,300,400)),labels=c('','2','','','','','','','','','20','','','','','','','','','200','',''))
  axis(side=1,at=c(0,5,10,15,20))
  axis(side=2,at=c(0,.05,.1,.15))
  
}

t.interval = function(data, variance = var(data, na.rm=TRUE), conf.level = 0.95) {
  
  z = qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
  
  xbar = mean(data, na.rm=TRUE)
  sdx = sqrt(variance/length(data))
  
  return(c(xbar - z * sdx, xbar + z * sdx))
  
}

plotVariancesRecalibrationScatter <- function(group='young',task='active') {
  
  # load the data for the group:
  locvar   <- read.csv(sprintf('data/%s_varianceCIs.csv',group), stringsAsFactors=F)
  locshift <- read.csv(sprintf('data/%s_rotated_localization.csv',group), stringsAsFactors=F)
  
  # under accepted theory, the variance should predict the shift:
  
  X <- sqrt(locvar[,sprintf('%s500',substr(task,1,1))])
  active_bool <- c('a'=1, 'p'=0)[substr(task,1,1)]
  Y <- locshift$localizationshift_deg[which(locshift$active_bool == active_bool)]
  
  color <- colorset[[sprintf('%s%sS',group,substr(task,1,3))]]
  
  plot(X,Y,main=sprintf('%s - %s',group,task),xlim=c(0,20),ylim=c(-10,30),axes=F,xlab=expression(sqrt(sigma^2)),ylab='localization shift',col=color)
  
  # cat(sprintf('\n***%s - %s\n', group, task))
  regr <- lm(Y~X)
  # print(summary(regr))
  abline(coef=regr$coefficients,col=rgb(0.5,0.5,0.5))
  
  # print(cor.test(X,Y))
  
  axis(side=1,at=c(0,5,10,15,20))
  axis(side=2,at=c(-10,0,10,20,30))
  
}
