Categorical Data Analysis HW4
================
Jo, Munsun
2021 12 6

#### 9\) Crab data

``` r
setwd("/Users/chom5/Desktop/범주형")
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

``` r
Y<-crab$Satellite
Y[crab$Satellite>0]<-1

color <- crab$Color - 1
```

``` r
# (a)
# nominal factor - Color
fit <- glm(Y ~ factor(color), family=binomial)
summary(fit)
```

    ## 
    ## Call:
    ## glm(formula = Y ~ factor(color), family = binomial)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6651  -1.3370   0.7997   0.7997   1.5134  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)      1.0986     0.6667   1.648   0.0994 .
    ## factor(color)2  -0.1226     0.7053  -0.174   0.8620  
    ## factor(color)3  -0.7309     0.7338  -0.996   0.3192  
    ## factor(color)4  -1.8608     0.8087  -2.301   0.0214 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 225.76  on 172  degrees of freedom
    ## Residual deviance: 212.06  on 169  degrees of freedom
    ## AIC: 220.06
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
C <- relevel(factor(color), ref="4")

fit1 <- glm(Y ~ C, family=binomial)
summary(fit1)
```

    ## 
    ## Call:
    ## glm(formula = Y ~ C, family = binomial)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6651  -1.3370   0.7997   0.7997   1.5134  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -0.7621     0.4577  -1.665 0.095910 .  
    ## C1            1.8608     0.8087   2.301 0.021393 *  
    ## C2            1.7382     0.5123   3.393 0.000692 ***
    ## C3            1.1299     0.5509   2.051 0.040289 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 225.76  on 172  degrees of freedom
    ## Residual deviance: 212.06  on 169  degrees of freedom
    ## AIC: 220.06
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# (b)
library(car)
```

    ## Warning: package 'car' was built under R version 4.0.5

    ## Loading required package: carData

``` r
Anova(fit1)
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: Y
    ##   LR Chisq Df Pr(>Chisq)   
    ## C   13.698  3   0.003347 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# (c)
fit2 <- glm(Y ~ color, family=binomial)
summary(fit2)
```

    ## 
    ## Call:
    ## glm(formula = Y ~ color, family = binomial)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.9103  -1.2719   0.8142   0.8142   1.3937  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   2.3635     0.5551   4.257 2.07e-05 ***
    ## color        -0.7147     0.2095  -3.412 0.000645 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 225.76  on 172  degrees of freedom
    ## Residual deviance: 213.30  on 171  degrees of freedom
    ## AIC: 217.3
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# (d)
anova(fit2, fit1, test="LRT")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Y ~ color
    ## Model 2: Y ~ C
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1       171     213.30                     
    ## 2       169     212.06  2    1.237   0.5387

``` r
# (e)
weight <- crab$Weight

fit3 <- glm(Y ~ color + weight, family=binomial)

# Standardized coefficient
# 방법1
library(lm.beta)
```

    ## Warning: package 'lm.beta' was built under R version 4.0.3

``` r
lm.beta(fit3)
```

    ## 
    ## Call:
    ## glm(formula = Y ~ color + weight, family = binomial)
    ## 
    ## Standardized Coefficients::
    ## (Intercept)       color      weight 
    ##    0.000000   -0.857400    1.983404

``` r
# 방법2
(sd(color)/sd(Y))*fit3$coef[2] # color
```

    ##   color 
    ## -0.8574

``` r
(sd(weight)/sd(Y))*fit3$coef[3] # weight
```

    ##   weight 
    ## 1.983404

``` r
1.983404/0.857400
```

    ## [1] 2.313277

``` r
# 방법3
X <- data.frame(color, weight)
X_sc <- scale(X, center=TRUE, scale=TRUE)
fit4 <- glm(Y ~ X_sc, family=binomial); fit4$coef
```

    ## (Intercept)   X_sccolor  X_scweight 
    ##   0.7429828  -0.4123381   0.9538525

``` r
# 방법4
sd(color)*fit3$coef[2] # color
```

    ##      color 
    ## -0.4123381

``` r
sd(weight)*fit3$coef[3] # weight
```

    ##    weight 
    ## 0.9538525

``` r
0.9538525/0.4123381
```

    ## [1] 2.313278
