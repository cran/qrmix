summary.qrmix=function(object,fitMethod=NULL,data=NULL,...)
{
  if(is.null(data))data=object$xy
  if(is.null(data))stop("The user needs to provide the data if the qrmix object doesn't include it.")


  k=object$k
  formula=(object$call)$formula
  data=model.frame(formula=formula,data=data)
  n=dim(data)[1]

  smry=list()



  if(identical(fitMethod,lm)|identical(fitMethod,rlm)|identical(fitMethod,rq))stop("fitMethod should be enclosed by quotation marks.")

  if(!is.null(fitMethod)&!identical(fitMethod,"lm")&!identical(fitMethod,"rlm")&!identical(fitMethod,"rq"))stop("This is not an available refitting method.")

  ##########same fitting method than in original call
  same=0
  if(is.null(fitMethod)){
    fitMethod=(object$call)$fitMethod
    same=1
  }
  if(is.null(fitMethod))fitMethod="lm"
  if(is.null(object$call$fitMethod))
  {
    if(fitMethod=="lm")same=1
  }else{
    if(object$call$fitMethod==fitMethod)same=1
  }
  ##############


  xlm3=rq(formula,tau=object$quantiles,data)
  xres0 = abs(residuals(xlm3))
  tt=apply(xres0,1,min)
  i1=apply(xres0==tt,1,which.max)
  if(fitMethod=="rq")
  {
    res=rep(0,n)
    for (j in 1:n)
    {
      res[j]=xlm3$res[j,i1[j]]
    }
    smry$quantreg=summary(xlm3,...)
  }else{


    form1=deparse(formula)
    xres1=array(0, dim=c(n,k))
    for (i in 1:k)
    {
      qi=NULL
      eval(parse(text=paste("qi=",fitMethod,"(",form1,",data=data[i1==",i,",])",sep='')))
      smry[[i]]=summary(qi,...)

      xres1[i1==i,i]=(qi$res)
      xres1[i1!=i,i]=(data[i1!=i,1]-predict(qi,data[i1!=i,]))

    }

    tt=apply(abs(xres1),1,min)
    i11=apply(abs(xres1)==tt,1,which.max)
    res=rep(0,n)
    for (j in 1:n)
    {
      res[j]=xres1[j,i11[j]]
    }


    names(smry)=paste0("cluster",seq_along(smry))
  }


  smry$residuals=res
  if(fitMethod=="rq")
  {
      smry$clusters=i1
  }else{
    smry$clusters=i11
  }


  smry$call=object$call
  smry$fitMethod=fitMethod
  smry$quantiles=object$quantiles
  class(smry)="summary.qrmix"
  smry
}



