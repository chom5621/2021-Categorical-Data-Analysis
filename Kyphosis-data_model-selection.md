Categorical Data Analysis HW4
================
Jo, Munsun
2021 12 5

#### 1\)

``` r
# (a)
LI <- c(8,8,10,10,12,12,12,14,14,14,16,16,16,18,20,20,20,22,22,24,26,28,32,34,38,38,38)
y <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,0,0,1,1,0,1,1,1,0)
fit <- glm(y ~ LI, family=binomial)
summary(fit)
```

    ## 
    ## Call:
    ## glm(formula = y ~ LI, family = binomial)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.9448  -0.6465  -0.4947   0.6571   1.6971  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept) -3.77714    1.37862  -2.740  0.00615 **
    ## LI           0.14486    0.05934   2.441  0.01464 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 34.372  on 26  degrees of freedom
    ## Residual deviance: 26.073  on 25  degrees of freedom
    ## AIC: 30.073
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# (b)
quantile(LI)
```

    ##   0%  25%  50%  75% 100% 
    ##    8   13   18   25   38

``` r
# (e) 방법2
library(mfx)
```

    ## Warning: package 'mfx' was built under R version 4.0.5

    ## Loading required package: sandwich

    ## Warning: package 'sandwich' was built under R version 4.0.5

    ## Loading required package: lmtest

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: MASS

    ## Loading required package: betareg

    ## Warning: package 'betareg' was built under R version 4.0.5

``` r
logitmfx(fit, atmean=FALSE, data=data.frame(LI))
```

    ## Call:
    ## logitmfx(formula = fit, data = data.frame(LI), atmean = FALSE)
    ## 
    ## Marginal Effects:
    ##       dF/dx Std. Err.     z P>|z|
    ## LI 0.022584  0.014722 1.534 0.125

``` r
# 방법3
predict3 <- predict(fit, data.frame(LI=LI), type='response')
#predict3

rate <- fit$coefficients[2]*predict3*(1-predict3)
#rate
mean(rate)
```

    ## [1] 0.02258365

#### 7\) Kyphosis data

``` r
# (a)
Kyphosis <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Kyphosis.dat", header=TRUE)

fit2 <- glm(y ~ x, family=binomial, data=Kyphosis)
summary(fit2)
```

    ## 
    ## Call:
    ## glm(formula = y ~ x, family = binomial, data = Kyphosis)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3126  -1.0907  -0.9482   1.2170   1.4052  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)
    ## (Intercept) -0.572693   0.602395  -0.951    0.342
    ## x            0.004296   0.005849   0.734    0.463
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 55.051  on 39  degrees of freedom
    ## Residual deviance: 54.504  on 38  degrees of freedom
    ## AIC: 58.504
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# (b)
plot(jitter(y, 0.1) ~ x, data=Kyphosis, ylab="Kyphosis")
```

![](Kyphosis-data_model-selection_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# (c)
fit3 <- glm(y ~ x + I(x^2), family=binomial, data=Kyphosis)
summary(fit3)
```

    ## 
    ## Call:
    ## glm(formula = y ~ x + I(x^2), family = binomial, data = Kyphosis)
    ## 
    ## Deviance Residuals: 
    ##    Min      1Q  Median      3Q     Max  
    ## -1.482  -1.009  -0.507   1.012   1.788  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept) -2.0462547  0.9943478  -2.058   0.0396 *
    ## x            0.0600398  0.0267808   2.242   0.0250 *
    ## I(x^2)      -0.0003279  0.0001564  -2.097   0.0360 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 55.051  on 39  degrees of freedom
    ## Residual deviance: 48.228  on 37  degrees of freedom
    ## AIC: 54.228
    ## 
    ## Number of Fisher Scoring iterations: 4
