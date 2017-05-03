predict.qrmix = function(object,newdata=NULL,type="clusters",...)
{
  if(is.null(newdata))
  {
    if(type=="clusters")
    {
      ypred = object$clusters
    }else if(type=="fitted.values"){
      ypred = object$fitted.values
    }else if(type=="residuals"){
        ypred = object$residuals}else{stop("Invalid type.")}
  }else{
    mf=try(model.frame((object$call)$formula,data=newdata),silent=TRUE)
    if(class(mf)!="data.frame")stop("User must provide the outcome variable and all the predictors.")

    x=as.matrix(model.matrix(attr(mf,"terms"),data=mf))
    y=as.numeric(model.response(mf))


    n=dim(x)[1]
    k=dim(object$coefficients)[2]
    ypredCl = matrix(0,nrow=n,ncol=k)
    for (i in 1:k)
    {
      ypredCl[,i]=x%*%object$coefficients[,i]
    }
    resiCl=abs(y-ypredCl)
    resipred=apply(resiCl,1,min)
    cl=apply(resiCl==resipred,1,which.max)

    if(type=="clusters")
    {
      ypred=cl
    }else if(type=="yhat"){
      ypred=rep(0,n)
      for (j in 1:n)
      {
        ypred[j]=ypredCl[j,cl[j]]
      }
    }else if(type=="residuals"){
      ypred=rep(0,n)
      for (j in 1:n)
      {
        ypred[j]=y[j]-ypredCl[j,cl[j]]
      }
    }else{stop("Invalid type.")}

    }

  ypred
}


