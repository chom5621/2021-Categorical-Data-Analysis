Categorical Data Analysis CHAPTER 5
================
Jo, Munsun
2021 11 30

## Sparse data

### Infinite Parameter Estimates in logistic regression

#### Complete separation: perfect discrimination

``` r
x<-c(1,2,3,4,5,6); y<-c(1,1,1,0,0,0)
fit<-glm(y~x, family=binomial(link=logit))
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
summary(fit)
```

    ## 
    ## Call:
    ## glm(formula = y ~ x, family = binomial(link = logit))
    ## 
    ## Deviance Residuals: 
    ##          1           2           3           4           5           6  
    ##  2.110e-08   2.110e-08   1.052e-05  -1.052e-05  -2.110e-08  -2.110e-08  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)    165.32  407521.43       0        1
    ## x              -47.23  115264.41       0        1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 8.3178e+00  on 5  degrees of freedom
    ## Residual deviance: 2.2152e-10  on 4  degrees of freedom
    ## AIC: 4
    ## 
    ## Number of Fisher Scoring iterations: 25

#### Quasi-Completle separation: at least one estimate infinite

``` r
x2<-c(1,2,3,4,5,6,3.5,3.5); y2<-c(1,1,1,0,0,0,1,0)
fit2<-glm(y2~x2, family=binomial(link=logit))
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
summary(fit2)
```

    ## 
    ## Call:
    ## glm(formula = y2 ~ x2, family = binomial(link = logit))
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.17741  -0.00002   0.00000   0.00002   1.17741  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)   137.32   54599.64   0.003    0.998
    ## x2            -39.23   15599.90  -0.003    0.998
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 11.0904  on 7  degrees of freedom
    ## Residual deviance:  2.7726  on 6  degrees of freedom
    ## AIC: 6.7726
    ## 
    ## Number of Fisher Scoring iterations: 21

## Endometrial cancer data (Heinze and Shemper 2002)

``` r
Endo <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Endometrial.dat",  header=TRUE)
head(Endo)
```

    ##   NV PI   EH HG
    ## 1  0 13 1.64  0
    ## 2  0 16 2.26  0
    ## 3  0  8 3.14  0
    ## 4  0 34 2.68  0
    ## 5  0 20 1.28  0
    ## 6  0  5 2.31  0

  - NV = neovasculation (binary)
  - PI = pulsatility index of arteria uterina
  - EH = endometrium height
  - HG: Grade

<!-- end list -->

``` r
xtabs(~NV + HG, data=Endo)
```

    ##    HG
    ## NV   0  1
    ##   0 49 17
    ##   1  0 13

``` r
fit <- glm(HG ~ NV + PI + EH, family=binomial, data=Endo)
summary(fit)
```

    ## 
    ## Call:
    ## glm(formula = HG ~ NV + PI + EH, family = binomial, data = Endo)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -1.50137  -0.64108  -0.29432   0.00016   2.72777  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)    4.30452    1.63730   2.629 0.008563 ** 
    ## NV            18.18556 1715.75089   0.011 0.991543    
    ## PI            -0.04218    0.04433  -0.952 0.341333    
    ## EH            -2.90261    0.84555  -3.433 0.000597 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 104.903  on 78  degrees of freedom
    ## Residual deviance:  55.393  on 75  degrees of freedom
    ## AIC: 63.393
    ## 
    ## Number of Fisher Scoring iterations: 17

``` r
logLik(fit)
```

    ## 'log Lik.' -27.69663 (df=4)

``` r
library(car)
```

    ## Warning: package 'car' was built under R version 4.0.5

    ## Loading required package: carData

``` r
Anova(fit)
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: HG
    ##    LR Chisq Df Pr(>Chisq)    
    ## NV   9.3576  1   0.002221 ** 
    ## PI   0.9851  1   0.320934    
    ## EH  19.7606  1  8.777e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# install.packages("profileModel")
library(profileModel)
```

    ## Warning: package 'profileModel' was built under R version 4.0.5

``` r
confintModel(fit, objective="ordinaryDeviance", method="zoom", endpoint.tolerance = 1e-08)
```

    ## Preliminary iteration .... Done
    ## 
    ## Profiling for parameter (Intercept) ... Done
    ## Profiling for parameter NV ... Done
    ## Profiling for parameter PI ... Done
    ## Profiling for parameter EH ... Done
    ## Zooming for parameter (Intercept) ...
    ## Zooming for parameter NV ...
    ## Zooming for parameter PI ...
    ## Zooming for parameter EH ...

    ##                  Lower       Upper
    ## (Intercept)  1.4327458  7.95477715
    ## NV           1.2841117         Inf
    ## PI          -0.1370768  0.03818467
    ## EH          -4.7859125 -1.43638895
    ## attr(,"fitted object")
    ## fit

``` r
# install.packages("detectseparation")
library(detectseparation)
```

    ## Warning: package 'detectseparation' was built under R version 4.0.5

``` r
endo.fit <- glm(HG ~ NV + PI + EH, family=binomial,data=Endo)

plot(check_infinite_estimates(endo.fit))
```

![](Endometrial-cancer-data_sparse-data_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
glm(HG ~ NV + PI + EH, family=binomial,data=Endo, method="detect_separation")
```

    ## Implementation: ROI | Solver: lpsolve 
    ## Separation: TRUE 
    ## Existence of maximum likelihood estimates
    ## (Intercept)          NV          PI          EH 
    ##           0         Inf           0           0 
    ## 0: finite value, Inf: infinity, -Inf: -infinity
