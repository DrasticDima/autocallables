library(varbvs)
library(ggplot2)
library(plotly)
library(utils)
library(purrr)
library(tidyverse)
library(matlib)
library(purrr)
library(RQuantLib)

densityPlot<-function(data){
  data=as.data.frame(data)%>%
    setNames("x")
  p1<-ggplot(data, aes((x))) +
    geom_density()
  ggplotly(p1)
}

assets.pathsCustom<-function(mu,spot,dt,cov.matrix,nruns,daily,vols.val,r){
  if (daily==1){
    steps=ceiling(dt[length(dt)]/(1/252))
    dt=rep(1/252,steps)%>%cumsum()
  }
  else{
    steps=length(dt)
    vols.valdt=map(vols.val,~./sqrt(1/diff(c(0,dt))))%>%data.frame()
  }
  # get the number of assets
  nAssets = length(spot)
  # calculate the drift
  #nudt = map(c(dt[1],diff(dt)),~(r-0.5*vols.val^2)*.)%>%unlist()%>%matrix(.,ncol=length(vols.val),byrow = TRUE)
  nudt<-map2(c(dt[1],diff(dt)),t(as.matrix(vols.valdt))%>%data.frame(),~(mu-0.5*.y^2)*.x)%>%unlist()%>%matrix(.,ncol=length(vols.val),byrow = TRUE)
  
  volsdt=map(c(dt[1],diff(dt)),~vols.val*sqrt(.))%>%unlist()%>%matrix(.,ncol=length(vols.val),byrow = TRUE)
  
  volsdt<-vols.valdt
  
  erdt = exp(r*dt)
  
  R=chol(cov.matrix)
  
  paths=list()
  for (idx in 1:nruns){
    # generate uncorrelated random sequence
    x=matrix(rnorm(steps*nAssets),ncol = nAssets) 
    
    cx=x%*%R
    
    path=(rbind(rep(1,nAssets),exp(nudt+volsdt*cx))%>%apply(., 2, cumprod))%*%diag(spot)
    paths[[idx]]=path
    
  }
  
  return(paths)
}

autocallable<-function(paths,t_passed,dt,S0,autocall_strike,cpn_barrier,risk_level,r,reg_cpn,cum_cpn,hasMemory,plot,incl_nom,nominal,risk_level_pay,daily,mem){
  payout=list()
  dt=c(0,as.numeric(dt))
  reg_cpn=0.02
  if (daily==1){
    periods=c(1,ceiling(dt*252))
  }
  else{
    periods=1:(length(dt))
  }
  for (point in paths){
    if (plot==1){
      matplot(t(t(point)/S0), type='l', xlab='dt', ylab='Prices',
              main='Selected Price Paths for Assets')
      matlines ((periods),rep(autocall_strike,length(periods)), type = "l", lty = 1:5, lwd = 3, pch = NULL,
                col = 7)
      matlines ((periods),rep(cpn_barrier,length(periods)), type = "l", lty = 1:5, lwd = 3, pch = NULL,
                col = 8)
      matlines ((periods),rep(risk_level,length(periods)), type = "l", lty = 1:5, lwd = 3, pch = NULL,
                col = 9)
    }
    pay=0
    mem=mem
    below_risk_level_on_exit=1
    counter=2
    for (x in (periods[-1])){
      knocked_structure=TRUE
      knocked_cpn=FALSE
      if (TRUE %in% (point[x,]/S0 < cpn_barrier)){
        knocked_cpn=TRUE
      }
      
      if (TRUE %in% (point[x,]/S0 < autocall_strike[counter])){
        knocked_structure=FALSE
      }
      
      if (!knocked_structure){
        pay = pay+(mem**hasMemory)*reg_cpn*exp(r*-((dt)[counter]+t_passed))*nominal
        mem=1
      }
      else if (!knocked_structure & knocked_cpn){
        mem = mem
      }
      else if (knocked_structure){
        pay=pay+((mem**hasMemory)*reg_cpn+cum_cpn)*exp(r*-((dt)[counter]+t_passed))*nominal
        break
      }
      
      if ((x==(periods[length(periods)])) & (TRUE %in% (point[x,]/S0<risk_level))){
        below_risk_level_on_exit = min(below_risk_level_on_exit,point[x,]/S0)
        mem=0
      }
      
      if ((x==(periods[length(periods)])) & (TRUE %in% ( (point[x,]/S0<cpn_barrier) & (point[x,]/S0>risk_level) ))){
        mem=0
      }
      if ( (x==(periods[length(periods)])) & (TRUE %in% (point[x,]/S0 > 1)) ){
        pay=pay+exp(r*-((dt)[counter]+t_passed))
      }
      counter=min(counter+1,length(dt))
      #print(counter)
    }
    
    if (!knocked_structure){
      if (!incl_nom){
      #print("no nom")
      payout=append(payout,pay)}
    else payout=append(payout,nominal/cpn_barrier*(below_risk_level_on_exit*risk_level_pay*exp(r*-((dt)[counter]+t_passed)))+pay)#extra counter since next period is added at end of loop
    #print("nom")
    }
    else {payout=append(payout,pay)}
  }
  return(payout)
}

data=read.csv2("C:/Users/dmarchenko001/Documents/Projects/Alfa/Autocall/HistPriceGS.csv",fileEncoding="windows-1251")

params=read.csv2("C:/Users/dmarchenko001/Documents/Projects/Alfa/Autocall/paramsGS.csv",fileEncoding="windows-1251")

#Dates 1 (coupon)
couponDates=params$cpn_date%>%unlist()
tVec=map(couponDates,~ 
           tryCatch({as.Date(.,tryFormats = c("%d.%m.%Y"))},error=function(cond) {NA}))%>%unlist(.)%>%is.na() #r stupid cant unlist dates so this workaround
couponDates<-couponDates[!tVec]%>%as.Date(.,tryFormats = c("%d.%m.%Y"))
curr_date<-params$Current_date[1]%>%as.Date(.,tryFormats = c("%d.%m.%Y")) 
dates=sort(c(curr_date,couponDates[curr_date < couponDates]))
diff(dates)/252
businessDays<-map2(dates[-length(dates)],dates[-1],~ businessDaysBetween(calendar="Russia", from = .x, to = .y,
                                                                         includeFirst = 1, includeLast = 1))%>%unlist()
dt<-cumsum(businessDays)/252


spot<-c(35.39,
        141.57,
        3.42,
        123.18)


prices=data[c(2,3,4,5)]


prices2=prices[1:(min(which(prices==0L))-1),]
returns2=log(prices2[-dim(prices2)[1],]/prices2[-1,])
corrmat2=cor(returns2)
vols2=map(returns2,~sd(.))%>%unlist()*sqrt(12) #Yearly

returns=log(prices[-dim(prices)[1],]/prices[-1,])
corrmat=cor(returns)

corrmat[1,]=corrmat2[1,]
corrmat[,1]=corrmat2[,1]

vols=map(returns,~sd(.))%>%unlist()*sqrt(12) #Yearly
vols[1]=vols2[1]

mean.vec2 <- as.numeric(colMeans(returns2))
mean.vec <- as.numeric(colMeans(returns))
mean.vec[1]<-mean.vec2[1]

mem=1
cpn=0.08
nruns=10000
cpnBarrier=0.7
daily=0
r=0.04


TopBarrier<-strsplit(params$CallBarrier,"%")%>%unlist()%>%as.numeric()/100
TopBarrier<-TopBarrier[(length(TopBarrier)-length(dt)):length(TopBarrier)]

s0<-c(256.79,562.34,34.39,356.78)
paths=assets.pathsCustom(mean.vec,spot,dt,corrmat,nruns=10000,daily=0,vols,r)

(TRUE %in% (paths[[1]][9,]>s0))

#write.csv2(paths,"C:/Users/dmarchenko001/Documents/Projects/Alfa/Autocall/check3.csv")
cum_cpn=1
payouts=autocallable(paths,0,dt,s0,TopBarrier,cpnBarrier,cpnBarrier,r,cpn,cum_cpn,hasMemory=1,plot=0,incl_nom=1,nominal=1,risk_level_pay=1,daily=0,mem)
mean(unlist(payouts))

sort(table(unlist(payouts)),decreasing=TRUE)[1:5]
