plot.qrmix=function(x,data=NULL,type=c(1,2,3),lwd=2,bw="SJ",adjust=2,...)
{
  model=x
  if(is.null(data))data=model$xy
  if(is.null(data))stop("The user needs to provide the data if the qrmix object doesn't include it")
  par(ask=T)
  k=model$k
  formula=(model$call)$formula
  x=model.frame(formula=formula,data=data)


  totals=table(model$clusters)
  perc=totals/sum(totals)

  ycluster=split(x[,1],model$clusters)
  ddAll=lapply(ycluster,density,bw=bw,adjust=adjust)
  ddAllY=lapply(ddAll, `[`, 'y')
  maxY=max(apply(matrix(unlist(ddAllY), ncol = k, byrow = FALSE),2,max)*perc)


  if(1 %in% type)
  {
  plot(density(x[,1]),main="Density by cluster",xlab=names(x)[1],type="n",ylim=c(0,maxY))

    for(i in 1:k) {
     lines(ddAll[[i]]$x,perc[i]*ddAll[[i]]$y,col=rainbow(k)[i],lwd=lwd)
      }
  }

#################### Second group of graphs

  ## to change the names of the clusters in the boxplots
  cl_temp=as.factor(model$clusters)
  cl_text="levels(cl_temp)=c(\"cl1\""
  for (i in 2:k)
  {
    cl_text=paste(cl_text,",\"cl",i,"\"",sep='')
  }
  cl_text=paste(cl_text,")",sep='')
  eval(parse(text=cl_text))
  #######


  if(2 %in% type)
  {

  fitMethod=(model$call)$fitMethod
  if(is.null(fitMethod))fitMethod="lm"
  Ntau=(model$call)$Ntau
  if(is.null(Ntau))Ntau=50
  tau=(1:(Ntau-1))/Ntau
  qregs = rq(formula,tau,data)
  flag=0 # 1 means there are no numeric covariates
  if(dim(x)[2]==2){if(is.numeric(x[,2])){
    numx=as.data.frame(x[,2])
    names(numx)=names(x)[2]
  }else{
    flag=1
    factx=as.data.frame(x[,2])
    names(factx)=names(x)[2]
  }
  }else{
    whichcont=sapply(x[,-1], is.numeric)
    if(sum(whichcont)>0){
      numx=as.data.frame(x[,-1][,whichcont])
      names(numx)=names(x[,-1])[whichcont]
      if(sum(whichcont)<(dim(x)[2]-1)){
        factx=as.data.frame(x[,-1][,!whichcont])
        names(factx)=names(x[,-1])[!whichcont]
        }
    }else{
        flag=1  #all factors
        factx=as.data.frame(x[,-1])
        names(factx)=names(x[,-1])
        }
  }

  if(flag==0){
    for(i in 1:dim(numx)[2])
    {
      plot(numx[,i],x[,1],col="skyblue4",pch=20,xlab=names(numx)[i],ylab=all.vars(formula)[1])

      for(j in 1:(Ntau-1))
      {
        interc=mean(qregs$fitted.values[,j]-(qregs$coefficients[names(numx)[i],j])*numx[,i])
        abline(c(interc,qregs$coef[names(numx)[i],j]),lwd=0.5,col="darkolivegreen2",lty=1)
      }
      if(fitMethod!="rq")
      {
        for(j in 1:k)
        {
          interc1=mean(model$fitted.values[model$clusters==j]-(model$coefficients[names(numx)[i],j])*(numx[model$clusters==j,i]))
          abline(c(interc1,model$coefficients[names(numx)[i],j]),col="deepskyblue",lwd=2,lty=2)

          interc2=mean(qregs$fitted.values[,which(tau==model$quantiles[j])]-(qregs$coefficients[names(numx)[i],which(tau==model$quantiles[j])])*numx[,i])
          abline(c(interc2,qregs$coefficients[names(numx)[i],which(tau==model$quantiles[j])]),col="violetred",lwd=2)
        }
      }else{
        interc2=mean(qregs$fitted.values[,which(tau==model$quantiles[j])]-(qregs$coefficients[names(numx)[i],which(tau==model$quantiles[j])])*numx[,i])
        abline(c(interc2,qregs$coefficients[names(numx)[i],which(tau==model$quantiles[j])]),col="violetred",lwd=2)
      }
    }
  }


  if(exists("factx")){
    for(l in 1:dim(factx)[2]){
      boxplot(x[,1]~cl_temp*factx[,l],border=rainbow(k),xlab=paste("cluster*",names(factx)[l],sep=''),ylab=all.vars(formula)[1])
    }

  }

} # end of if

#################### Third type of graph

if(3 %in% type)
{
  boxplot(model$residuals~cl_temp,border=rainbow(k),xlab="Cluster",ylab="Residuals")
 } # end of if

par(ask=FALSE)
}

