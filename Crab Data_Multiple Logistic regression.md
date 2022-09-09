Categorical Data Analysis CHAPTER 5
================
Jo, Munsun
2021 11 24

``` r
setwd("/Users/chom5/Desktop/범주형")
load("glm_data.RData")
```

## crab data

``` r
head(crab)
```

    ##   Color Spine Width Satellite Weight
    ## 1     3     3  28.3         8   3050
    ## 2     4     3  22.5         0   1550
    ## 3     2     1  26.0         9   2300
    ## 4     4     3  24.8         0   2100
    ## 5     4     3  26.0         4   2600
    ## 6     3     3  23.8         0   2100

\[logit[P(Y = 1)] = α + β_1width + β_2C1 + β_3C2 + β_4C3\]

C1, C2, C3 : color indicator (medium light; medium; medium dark)

### Binary response model

``` r
Y<-crab$Satellite
Y[crab$Satellite>0]<-1
```

종속변수는 0 혹은 1의 값을 취하는 이항변수 Satellite이다.

### Predictor 1 : Width (continuous)

``` r
X<-crab$Width
```

설명변수 너비는 연속형 변수이다.

### Predictor 2: Color

``` r
# 방법 2: 순서가 있는 연속형

color <- crab$Color-1
```

``` r
# 방법 1: 수준이 4개인 범주형

C<-relevel( factor(color), ref="4")  # changing the reference level equal to 4 (Darkest)
```

``` r
# 방법 3: 수준이 2개인 범주형 (어두우면 1, 밝으면 0)

C4 <- rep(0, length(color))

C4[which(color==4)] <- 1

C4 <- relevel( factor(C4), ref="1")  # changing the reference level equal to 1 (Dark)
```

#### For LRT of each coefficient

``` r
library(car)
```

    ## Warning: package 'car' was built under R version 4.0.5

    ## Loading required package: carData

#### Model fitting - logistic regression

``` r
# 방법 1

fit1 <- glm(Y~X+C, family=binomial)

summary(fit1)
```

    ## 
    ## Call:
    ## glm(formula = Y ~ X + C, family = binomial)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.1124  -0.9848   0.5243   0.8513   2.1413  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -12.7151     2.7617  -4.604 4.14e-06 ***
    ## X             0.4680     0.1055   4.434 9.26e-06 ***
    ## C1            1.3299     0.8525   1.560   0.1188    
    ## C2            1.4023     0.5484   2.557   0.0106 *  
    ## C3            1.1061     0.5921   1.868   0.0617 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 225.76  on 172  degrees of freedom
    ## Residual deviance: 187.46  on 168  degrees of freedom
    ## AIC: 197.46
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
Anova(fit1)  # LRT test for each coefficient
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: Y
    ##   LR Chisq Df Pr(>Chisq)    
    ## X  24.6038  1  7.041e-07 ***
    ## C   6.9956  3    0.07204 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# 방법 2

fit2 <- glm(Y~X+color, family=binomial)

summary(fit2)
```

    ## 
    ## Call:
    ## glm(formula = Y ~ X + color, family = binomial)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.1692  -0.9889   0.5429   0.8700   1.9742  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -10.0708     2.8068  -3.588 0.000333 ***
    ## X             0.4583     0.1040   4.406 1.05e-05 ***
    ## color        -0.5090     0.2237  -2.276 0.022860 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 225.76  on 172  degrees of freedom
    ## Residual deviance: 189.12  on 170  degrees of freedom
    ## AIC: 195.12
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
Anova(fit2)  # LRT test for each coefficient
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: Y
    ##       LR Chisq Df Pr(>Chisq)    
    ## X      24.1767  1  8.789e-07 ***
    ## color   5.3315  1    0.02094 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# 방법 3

fit3 <- glm(Y~X+C4, family=binomial)

summary(fit3)
```

    ## 
    ## Call:
    ## glm(formula = Y ~ X + C4, family = binomial)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0821  -0.9932   0.5274   0.8606   2.1553  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -12.9795     2.7272  -4.759 1.94e-06 ***
    ## X             0.4782     0.1041   4.592 4.39e-06 ***
    ## C40           1.3005     0.5259   2.473   0.0134 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 225.76  on 172  degrees of freedom
    ## Residual deviance: 187.96  on 170  degrees of freedom
    ## AIC: 193.96
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
Anova(fit3)  # LRT test for each coefficient
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: Y
    ##    LR Chisq Df Pr(>Chisq)    
    ## X   26.8351  1  2.216e-07 ***
    ## C4   6.4948  1    0.01082 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### Model comparison - LRT for H0: fit2 vs. H1: fit1

``` r
anova(fit2, fit1, test="LRT")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Y ~ X + color
    ## Model 2: Y ~ X + C
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1       170     189.12                     
    ## 2       168     187.46  2   1.6641   0.4351

#### Model comparison - LRT for H0: fit3 vs. H1: fit1

``` r
anova(fit3, fit1, test="LRT")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Y ~ X + C4
    ## Model 2: Y ~ X + C
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1       170     187.96                     
    ## 2       168     187.46  2  0.50085   0.7785

### 방법 4: Interaction between Width(X) and Color(C4)

``` r
fit4 <- glm(Y~X+C4+X:C4, family=binomial)

summary(fit4)
```

    ## 
    ## Call:
    ## glm(formula = Y ~ X + C4 + X:C4, family = binomial)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.1366  -0.9344   0.4996   0.8554   1.7753  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -5.8538     6.6939  -0.874    0.382
    ## X             0.2004     0.2617   0.766    0.444
    ## C40          -6.9578     7.3182  -0.951    0.342
    ## X:C40         0.3217     0.2857   1.126    0.260
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 225.76  on 172  degrees of freedom
    ## Residual deviance: 186.79  on 169  degrees of freedom
    ## AIC: 194.79
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
Anova(fit4)  # LRT for each coefficient
```

    ## Analysis of Deviance Table (Type II tests)
    ## 
    ## Response: Y
    ##      LR Chisq Df Pr(>Chisq)    
    ## X     26.8351  1  2.216e-07 ***
    ## C4     6.4948  1    0.01082 *  
    ## X:C4   1.1715  1    0.27909    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(fit3, fit4, test="LRT")  # LRT for H0: fit3 vs. H1:fit4
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Y ~ X + C4
    ## Model 2: Y ~ X + C4 + X:C4
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1       170     187.96                     
    ## 2       169     186.79  1   1.1715   0.2791
