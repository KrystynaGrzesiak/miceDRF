# miceDRF: imputation with mice DRF and I-Score


## Installation

Run the following code in R console to download the latest development version from Github:

```R
if (!require(devtools)) {
  install.packages('devtools')
}
devtools::install_github("KrystynaGrzesiak/miceDRF")

library(miceDRF)
library(mice)

n<-200
d<-5
X<-matrix(runif(n*d), nrow=n, ncol=d)


pmiss<-0.2

X.NA<-apply(X,2, function(x) {
  U<-runif(length(x))
  ifelse(U <= pmiss, rep(NA, length(x)),x)
})

imp<-mice(X.NA, m=1, method="DRF")
Ximp<-mice::complete(imp)

```


