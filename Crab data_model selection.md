Categorical Data Analysis CHAPTER 5
================
Jo, Munsun
2021 11 25

``` r
setwd("/Users/chom5/Desktop/범주형")
load("glm_data.RData")
```

### crab data

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

The variables are

  - **Color** color. The colors given in Agresti are “light medium”,
    “medium”, “dark medium”, and “dark”. Here they are abbreviated to
    light, medium, dark, and darker, respectively.

  - **Spine** spine condition. The conditions given in Agresti are “both
    good”, “one worn or broken”, and “both worn or broken”. Here they
    are abbreviated to good, middle, bad, respectively.

  - **Width** carapace width in centimeters

  - **Satellite** number of satellites, which males clustering around
    the female in addition to the male with which she is breeding.

  - **Weight** weight in grams.

<https://cran.r-project.org/web/packages/glmbb/glmbb.pdf>

### Binary response model

``` r
# 종속변수
Y<-crab$Satellite
Y[crab$Satellite>0]<-1 # Satellites가 있으면 1, 없으면 0

# 설명변수
C <- relevel( factor(crab$Color-1), ref="4") # Color 밝음, 중간 밝음, 어두움, 매우 어두움

C4 <- rep(0, length(crab$Color))
C4[which(C==4)] <- 1
## C4 <- relevel( factor(C4), ref="1")  # Color 어두우면 1, 밝으면 0

S <- relevel( factor(crab$Spine), ref="3") # Spine 둘 다 괜찮음, 마모되거나 부서짐, 마모되고 부서짐
```

``` r
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

### Model fitting

``` r
crab.main <-glm(Y~C+S+Weight+Width, family=binomial, data=crabs)
summary(crab.main)
```

    ## 
    ## Call:
    ## glm(formula = Y ~ C + S + Weight + Width, family = binomial, 
    ##     data = crabs)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.1977  -0.9424   0.4849   0.8491   2.1198  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept) -9.2733924  3.8377613  -2.416  0.01568 * 
    ## C1           1.6086658  0.9355326   1.720  0.08552 . 
    ## C2           1.5057635  0.5666668   2.657  0.00788 **
    ## C3           1.1198016  0.5932901   1.887  0.05910 . 
    ## S1          -0.4002868  0.5027043  -0.796  0.42588   
    ## S2          -0.4962677  0.6291609  -0.789  0.43024   
    ## Weight       0.0008258  0.0007038   1.173  0.24069   
    ## Width        0.2631279  0.1952986   1.347  0.17788   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 225.76  on 172  degrees of freedom
    ## Residual deviance: 185.20  on 165  degrees of freedom
    ## AIC: 201.2
    ## 
    ## Number of Fisher Scoring iterations: 4

### Multicolinearity - Weight and Width

``` r
cor(crabs$Weight, crabs$Width)
```

    ## [1] 0.8868715

``` r
plot(crabs$Weight, crabs$Width, xlab="Weight", ylab="Width")
```

![](범주형-AIC-backward-selection_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
drop1(crab.main) # Criterion: AIC, so drop Weight!
```

    ## Single term deletions
    ## 
    ## Model:
    ## Y ~ C + S + Weight + Width
    ##        Df Deviance    AIC
    ## <none>      185.20 201.20
    ## C       3   192.80 202.80
    ## S       2   186.21 198.21
    ## Weight  1   186.61 200.61
    ## Width   1   187.00 201.00

### Full Model fitting

``` r
crab.full <-glm(Y~C*S*Width, family=binomial, data=crabs)
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
summary(crab.full)
```

    ## 
    ## Call:
    ## glm(formula = Y ~ C * S * Width, family = binomial, data = crabs)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.1128  -0.8860   0.4891   0.7637   1.8594  
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##               Estimate Std. Error z value Pr(>|z|)
    ## (Intercept) -4.073e+00  7.266e+00  -0.561    0.575
    ## C1           6.059e+02  4.790e+05   0.001    0.999
    ## C2          -4.996e+00  8.992e+00  -0.556    0.578
    ## C3          -1.465e+01  9.836e+00  -1.490    0.136
    ## S1           1.118e+00  9.035e+04   0.000    1.000
    ## S2          -5.223e+02  4.411e+04  -0.012    0.991
    ## Width        1.352e-01  2.833e-01   0.477    0.633
    ## C1:S1       -6.090e+02  4.875e+05  -0.001    0.999
    ## C2:S1       -1.038e+00  9.035e+04   0.000    1.000
    ## C3:S1        3.617e+01  9.489e+03   0.004    0.997
    ## C1:S2       -6.095e+01  2.410e+04  -0.003    0.998
    ## C2:S2        5.364e+02  4.411e+04   0.012    0.990
    ## C3:S2       -8.750e+01  1.123e+04  -0.008    0.994
    ## C1:Width    -2.418e+01  1.857e+04  -0.001    0.999
    ## C2:Width     2.573e-01  3.477e-01   0.740    0.459
    ## C3:Width     6.122e-01  3.857e-01   1.587    0.112
    ## S1:Width    -7.475e-01  3.534e+03   0.000    1.000
    ## S2:Width     2.405e+01  2.078e+03   0.012    0.991
    ## C1:S1:Width  2.506e+01  1.890e+04   0.001    0.999
    ## C2:S1:Width  7.196e-01  3.534e+03   0.000    1.000
    ## C3:S1:Width         NA         NA      NA       NA
    ## C1:S2:Width         NA         NA      NA       NA
    ## C2:S2:Width -2.467e+01  2.078e+03  -0.012    0.991
    ## C3:S2:Width         NA         NA      NA       NA
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 225.76  on 172  degrees of freedom
    ## Residual deviance: 170.44  on 152  degrees of freedom
    ## AIC: 212.44
    ## 
    ## Number of Fisher Scoring iterations: 17

``` r
step(crab.full, direction="backward") # Backward selection with AIC
```

    ## Start:  AIC=212.44
    ## Y ~ C * S * Width
    ## 
    ##             Df Deviance    AIC
    ## - C:S:Width  3   173.67 209.67
    ## <none>           170.44 212.44
    ## 
    ## Step:  AIC=209.67
    ## Y ~ C + S + Width + C:S + C:Width + S:Width
    ## 
    ##           Df Deviance    AIC
    ## - C:S      6   181.56 205.56
    ## - S:Width  2   173.68 205.68
    ## - C:Width  3   177.34 207.34
    ## <none>         173.67 209.67
    ## 
    ## Step:  AIC=205.56
    ## Y ~ C + S + Width + C:Width + S:Width
    ## 
    ##           Df Deviance    AIC
    ## - S:Width  2   181.64 201.64
    ## - C:Width  3   186.41 204.41
    ## <none>         181.56 205.56
    ## 
    ## Step:  AIC=201.64
    ## Y ~ C + S + Width + C:Width
    ## 
    ##           Df Deviance    AIC
    ## - S        2   183.08 199.08
    ## - C:Width  3   186.61 200.61
    ## <none>         181.64 201.64
    ## 
    ## Step:  AIC=199.08
    ## Y ~ C + Width + C:Width
    ## 
    ##           Df Deviance    AIC
    ## - C:Width  3   187.46 197.46
    ## <none>         183.08 199.08
    ## 
    ## Step:  AIC=197.46
    ## Y ~ C + Width
    ## 
    ##         Df Deviance    AIC
    ## <none>       187.46 197.46
    ## - C      3   194.45 198.45
    ## - Width  1   212.06 220.06

    ## 
    ## Call:  glm(formula = Y ~ C + Width, family = binomial, data = crabs)
    ## 
    ## Coefficients:
    ## (Intercept)           C1           C2           C3        Width  
    ##     -12.715        1.330        1.402        1.106        0.468  
    ## 
    ## Degrees of Freedom: 172 Total (i.e. Null);  168 Residual
    ## Null Deviance:       225.8 
    ## Residual Deviance: 187.5     AIC: 197.5

### Final Model by backward selection with AIC

``` r
crab.final <- glm(Y~C+Width, family=binomial, data=crabs)

summary(crab.final)
```

    ## 
    ## Call:
    ## glm(formula = Y ~ C + Width, family = binomial, data = crabs)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.1124  -0.9848   0.5243   0.8513   2.1413  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -12.7151     2.7617  -4.604 4.14e-06 ***
    ## C1            1.3299     0.8525   1.560   0.1188    
    ## C2            1.4023     0.5484   2.557   0.0106 *  
    ## C3            1.1061     0.5921   1.868   0.0617 .  
    ## Width         0.4680     0.1055   4.434 9.26e-06 ***
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
crab.dark <- update(crab.final, .~ I(C!="4")+Width, data=crabs)

crab.dark2 <- glm(Y~C4+Width, family=binomial, data=crabs)

anova(crab.dark, crab.final, test = "Chisq" )
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: Y ~ I(C != "4") + Width
    ## Model 2: Y ~ C + Width
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1       170     187.96                     
    ## 2       168     187.46  2  0.50085   0.7785

``` r
summary(crab.dark)
```

    ## 
    ## Call:
    ## glm(formula = Y ~ I(C != "4") + Width, family = binomial, data = crabs)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0821  -0.9932   0.5274   0.8606   2.1553  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -12.9795     2.7272  -4.759 1.94e-06 ***
    ## I(C != "4")TRUE   1.3005     0.5259   2.473   0.0134 *  
    ## Width             0.4782     0.1041   4.592 4.39e-06 ***
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

### Full Model fitting with BIC

``` r
n <- length(Y)
step(crab.full, direction="backward", k=log(n)) # Backward selection with BIC
```

    ## Start:  AIC=278.66
    ## Y ~ C * S * Width
    ## 
    ##             Df Deviance    AIC
    ## - C:S:Width  3   173.67 266.43
    ## <none>           170.44 278.66
    ## 
    ## Step:  AIC=266.43
    ## Y ~ C + S + Width + C:S + C:Width + S:Width
    ## 
    ##           Df Deviance    AIC
    ## - C:S      6   181.56 243.40
    ## - C:Width  3   177.34 254.63
    ## - S:Width  2   173.68 256.13
    ## <none>         173.67 266.43
    ## 
    ## Step:  AIC=243.4
    ## Y ~ C + S + Width + C:Width + S:Width
    ## 
    ##           Df Deviance    AIC
    ## - C:Width  3   186.41 232.79
    ## - S:Width  2   181.64 233.17
    ## <none>         181.56 243.40
    ## 
    ## Step:  AIC=232.79
    ## Y ~ C + S + Width + S:Width
    ## 
    ##           Df Deviance    AIC
    ## - S:Width  2   186.61 222.69
    ## - C        3   194.13 225.05
    ## <none>         186.41 232.79
    ## 
    ## Step:  AIC=222.68
    ## Y ~ C + S + Width
    ## 
    ##         Df Deviance    AIC
    ## - S      2   187.46 213.22
    ## - C      3   194.43 215.04
    ## <none>       186.61 222.69
    ## - Width  1   208.83 239.75
    ## 
    ## Step:  AIC=213.22
    ## Y ~ C + Width
    ## 
    ##         Df Deviance    AIC
    ## - C      3   194.45 204.76
    ## <none>       187.46 213.22
    ## - Width  1   212.06 232.67
    ## 
    ## Step:  AIC=204.76
    ## Y ~ Width
    ## 
    ##         Df Deviance    AIC
    ## <none>       194.45 204.76
    ## - Width  1   225.76 230.91

    ## 
    ## Call:  glm(formula = Y ~ Width, family = binomial, data = crabs)
    ## 
    ## Coefficients:
    ## (Intercept)        Width  
    ##    -12.3508       0.4972  
    ## 
    ## Degrees of Freedom: 172 Total (i.e. Null);  171 Residual
    ## Null Deviance:       225.8 
    ## Residual Deviance: 194.5     AIC: 198.5
