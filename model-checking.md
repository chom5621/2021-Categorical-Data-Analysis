Categorical Data Analysis CHAPTER 5
================
Jo, Munsun
2021 11 27

``` r
setwd("/Users/chom5/Desktop/범주형")
```

## Goodness of fit test (grouped data)

### AZT data

``` r
AZT <- data.frame(race=c("W","W","B","B"),
                  azt=c("Y","N","Y","N"),
                  Yes=c(14,32,11,12),
                  No=c(93,81,52,43))
AZT
```

    ##   race azt Yes No
    ## 1    W   Y  14 93
    ## 2    W   N  32 81
    ## 3    B   Y  11 52
    ## 4    B   N  12 43

``` r
azt.logit <- glm(cbind(Yes,No) ~ race + azt , data=AZT, family=binomial)

sum(residuals(azt.logit, type="pearson")^2) # Pearson's X^2 statistic
```

    ## [1] 1.391026

``` r
sum(residuals(azt.logit, type="deviance")^2) # deviance G^2 statistic
```

    ## [1] 1.38353

### Marijuana data

``` r
Marijuana <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Marijuana.dat", header=TRUE)
Marijuana
```

    ##    race gender yes  no
    ## 1 white female 420 620
    ## 2 white   male 483 579
    ## 3 other female  25  55
    ## 4 other   male  32  62

``` r
fit <- glm(yes/(yes+no) ~ gender + race, weights=yes+no, family=binomial, data=Marijuana)

sum(residuals(fit, type="pearson")^2) # Pearson's X^2 statistic
```

    ## [1] 0.05806229

``` r
sum(residuals(fit, type="deviance")^2) # deviance G^2 statistic
```

    ## [1] 0.05798151

## Goodness of fit test (continuous variable)

### Crab data

``` r
load("glm_data.RData")
head(crab)
```

    ##   Color Spine Width Satellite Weight
    ## 1     3     3  28.3         8   3050
    ## 2     4     3  22.5         0   1550
    ## 3     2     1  26.0         9   2300
    ## 4     4     3  24.8         0   2100
    ## 5     4     3  26.0         4   2600
    ## 6     3     3  23.8         0   2100

## Setup variables

``` r
Y<-crab$Satellite
Y[crab$Satellite>0]<-1

C<-relevel( factor(crab$Color-1), ref="4") 

C4 <- rep(0, length(crab$Color))
C4[which(C==4)] <- 1

S<-relevel( factor(crab$Spine), ref="3") 

crabs <- data.frame(crab, Y=Y, C=C, S=S, C4=C4)
head(crabs)
```

    ##   Color Spine Width Satellite Weight Y C S C4
    ## 1     3     3  28.3         8   3050 1 2 3  0
    ## 2     4     3  22.5         0   1550 0 3 3  0
    ## 3     2     1  26.0         9   2300 1 1 1  0
    ## 4     4     3  24.8         0   2100 0 3 3  0
    ## 5     4     3  26.0         4   2600 1 3 3  0
    ## 6     3     3  23.8         0   2100 0 2 3  0

``` r
crab.fit<-glm(Y~Width, family=binomial, data=crabs)

Y.hat<-predict(crab.fit, type="response")
plot(Y.hat~Y)

library(ResourceSelection)
```

    ## Warning: package 'ResourceSelection' was built under R version 4.0.5

    ## ResourceSelection 0.3-5   2019-07-22

![](model-checking_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# Hosmer-Lemeshow test
hl <- hoslem.test(Y, fitted(crab.fit), g=10)
hl
```

    ## 
    ##  Hosmer and Lemeshow goodness of fit (GOF) test
    ## 
    ## data:  Y, fitted(crab.fit)
    ## X-squared = 4.3855, df = 8, p-value = 0.8208

``` r
cbind(hl$observed,hl$expected)
```

    ##               y0 y1      yhat0     yhat1
    ## [0.129,0.362] 14  5 13.6106317  5.389368
    ## (0.362,0.458] 10  8 10.3756848  7.624315
    ## (0.458,0.527]  5 10  7.4488451  7.551155
    ## (0.527,0.605] 10  9  8.0174489 10.982551
    ## (0.605,0.652]  5 11  5.9045476 10.095452
    ## (0.652,0.716]  7 11  5.7010064 12.298994
    ## (0.716,0.785]  4 12  3.9407017 12.059298
    ## (0.785,0.842]  4 16  3.7332657 16.266734
    ## (0.842,0.888]  3 15  2.3498224 15.650178
    ## (0.888,0.987]  0 14  0.9180457 13.081954
