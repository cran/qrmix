qrmix.default = function(formula, data=list(), ...)
{

  fit = qrmixFit(formula, data, ...)

  mf=model.frame(formula,data=data)
  x=as.matrix(model.matrix(attr(mf,"terms"),data=mf))
  y=as.numeric(model.response(mf))

  if(dim(x)[2]==1){fit$fitted.values=as.vector(x*fit$coefficients[,fit$clusters])
  }else{fit$fitted.values=as.vector(apply(x*t(fit$coefficients[,fit$clusters]),1,sum))}

  fit$residuals=y-fit$fitted.values
  fit$call=match.call()

  class(fit)="qrmix"
  fit
}

