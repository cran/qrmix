HuberF=function(r,c=1.345)
{
  c=abs(c)
  if(abs(r)<=c){
    return((1/2)*r^2)
  }else{
    return(c*(abs(r)-(1/2)*c))
  }
}

Huber=Vectorize(HuberF)

BisquareF=function(r,c=4.685)
{
  c=abs(c)
  if(abs(r)<=c){
    return((1-(1-(r/c)^2)^3))
  }else{
    return(1)
  }
}

Bisquare=Vectorize(BisquareF)

Squared=function(r)return(r^2)
Absolute=function(r)return(abs(r))

qrmixFit= function(formula,data=list(),k,Ntau=50,alpha=0.03,lossFn="Squared",fitMethod="lm",xy=TRUE,...){

  if(missing(k))stop("The user must provide the number of clusters.")
  x=model.frame(formula=formula,data=data) #this rearranges the data so that the response variables is in the first column


  if(is.factor(x[,1]))stop("Response variable should be numeric.")
  if(alpha*k>=.5)stop("alpha should be smaller than 1/(2k).")
  if(Ntau<2*k)stop("Ntau should be greater or equal than 2k.")


  if(identical(lossFn,Squared)|identical(lossFn,Absolute)|identical(lossFn,Bisquare)|identical(lossFn,Huber))stop("lossFn should be enclosed by quotation marks.")
  if(identical(fitMethod,lm)|identical(fitMethod,rlm)|identical(fitMethod,rq))stop("fitMethod should be enclosed by quotation marks.")


  if(!is.null(lossFn)&!identical(lossFn,"Squared")&!identical(lossFn,"Absolute")&!identical(lossFn,"Bisquare")&!identical(lossFn,"Huber"))stop("This is not an available loss function.")
  if(!is.null(fitMethod)&!identical(fitMethod,"lm")&!identical(fitMethod,"rlm")&!identical(fitMethod,"rq"))stop("This is not an available fitting method.")



  tau=(1:(Ntau-1))/Ntau
  xlm3 = rq(formula,tau,data=x)
  xpred = predict(xlm3)
  xres0 = abs(residuals(xlm3))

  resi=NULL


  text1="for(j1 in tau[tau>=alpha & tau<=(1/2-(k-1)*alpha)])
  {
  v[1]=j1"
  if(k>2){
    for (i in 2:(k-1))
    {
      text1=paste(text1,"\nfor(j",i," in tau[tau>=(2*(-1)^",i,"*sum(aux[1:(",i,"-1)]*v[1:(",i,"-1)])+alpha) & tau<=(1/2+(-1)^",i,"*sum(aux[1:(",i,"-1)]*v[1:(",i,"-1)])-alpha)])
                  {
                  v[",i,"]=j",i,sep='')
                  }
                  }
  text1=paste(text1,"\nfor(j",k," in tau[tau>=(2*(-1)^",k,"*sum(aux[1:(",k,"-1)]*v[1:(",k,"-1)])+alpha) & tau<=(1-alpha)])
              {
              v[",k,"]=j",k,"
              ind=which(tau %in% v)
              tt=apply(xres0[,ind],1,min)
              resi=rbind(resi,c(ind,sum(",lossFn,"(tt))))
              i2=i2+1
              }",sep='')
  for (i in 1:(k-1))
  {
    text1=paste(text1,"\n}",sep='')
  }

  v=rep(NA,k)
  aux=rep(c(1,-1),length=k-1)
  i2=0
  eval(parse(text=text1)) #this gives resi: indexes of elements in tau and mininum sum of squared residuals

  #####

  kk=which.min(resi[,k+1]) #selects the combination with min sum of squared residuals->optional different loss function
  tt=apply(xres0[,resi[kk,1:k]],1,min)  #gets the residual for each observation
  i1=apply(xres0[,resi[kk,1:k]]==tt,1,which.max)#classifies the observations as 1,2,3,...by giving the column number
  xres1=array(0, dim=c(dim(xres0)[1],k))
  qts=tau[resi[kk,1:k]]

  ######## Refit the model with lm, or rlm (M or MM)

  if(fitMethod!="rq")
  {

  texta=paste("q1=",fitMethod,"(formula,data=x[i1==1,],...)
              xres1[i1==1,1]=abs(q1$res)
              xres1[i1!=1,1]=abs(x[i1!=1,1]-predict(q1,x[i1!=1,]))\n",sep='') #first column is "y"
  for(i in 2:k)
  {
    texta=paste(texta,"q",i,"=",fitMethod,"(formula,data=x[i1==",i,",],...)
                xres1[i1==",i,",",i,"]=abs(q",i,"$res)
                xres1[i1!=",i,",",i,"]=abs(x[i1!=",i,",1]-predict(q",i,",x[i1!=",i,",]))\n",sep='') #first column is "y"
  }


  eval(parse(text=texta))


  tt=apply(xres1,1,min)
  i11=apply(xres1==tt,1,which.max)

  bbtext="cbind(q1$coef"
  for (i in 2:k)
  {
    bbtext=paste(bbtext,",q",i,"$coef",sep="")
  }
  bbtext=paste(bbtext,")",sep="")
  bb=eval(parse(text=bbtext))#coefficients of the robust regression
} ##end of if (for the refitting part)
############################################


  if(fitMethod=="rq")
  {
    out=list(coefficients=coef(xlm3)[,resi[kk,1:k]],clusters=i1,quantiles=qts)
  }else{
    out=list(coefficients=bb,clusters=i11,quantiles=qts)
  }

  if(xy==TRUE)out$xy=x

  out$k=k
  out
}#end of qrmf function
