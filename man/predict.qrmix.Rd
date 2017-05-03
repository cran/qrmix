\name{predict.qrmix}
\alias{predict.qrmix}

\title{Predict Method for qrmix Fits

}
\description{
Obtains clusters, predictions, or residuals from a fitted qrmix object.

}
\usage{
\method{predict}{qrmix}(object, newdata = NULL, type = "clusters", ...)
}

\arguments{
  \item{object}{
a fitted object of class \code{"qrmix"}.
}
  \item{newdata}{
optional data frame for which clusters, predictions, or residuals will be obtained from the qrmix fitted object. If omitted, the training values will be used.
}
  \item{type}{the type of prediction. \code{type = "clusters"} (default value) for predicted clusters, \code{"yhat"} for the response predicted value corresponding to the predicted cluste, \code{"residuals"} for the residuals corresponding to the response predicted values.

}
  \item{...}{other argumets passed to other methods.

}

}

\value{
A vector with predicted clusters, responses, or residuals, depending on \code{type}.

}

\examples{

data(blood.pressure)

set.seed(8)
sampleInd = sort(sample(1:500, 400))
bpSample1 = blood.pressure[sampleInd,]
bpSample2 = blood.pressure[-sampleInd,]

mod1 = qrmix(bmi ~ ., data = bpSample1, k = 3)

#Cluster assigned to the training values
predict(mod1)

#Residuals corresponding to the response predicted values from mod1 for new data
predict(mod1, newdata = bpSample2, type = "residuals")

}

