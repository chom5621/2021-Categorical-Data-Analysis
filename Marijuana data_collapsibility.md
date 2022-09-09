Categorical Data Analysis CHAPTER 7
================
Jo, Munsun
2021 12 4

``` r
setwd("/Users/chom5/Desktop/범주형")
```

## 7.4 Independence Graphs and Collapsibility

### Marijuana data

  - Model Building for Student Drug Use

<!-- end list -->

``` r
load("glm_data.RData")
head(drug3) # Alcohol, Cigarette, Marijuana, Race, Gender
```

    ##   A C M R G count
    ## 1 1 1 1 1 1   405
    ## 2 1 1 1 2 1    23
    ## 3 1 2 1 1 1    13
    ## 4 1 2 1 2 1     2
    ## 5 2 1 1 1 1     1
    ## 6 2 1 1 2 1     0

``` r
drug3
```

    ##    A C M R G count
    ## 1  1 1 1 1 1   405
    ## 2  1 1 1 2 1    23
    ## 3  1 2 1 1 1    13
    ## 4  1 2 1 2 1     2
    ## 5  2 1 1 1 1     1
    ## 6  2 1 1 2 1     0
    ## 7  2 2 1 1 1     1
    ## 8  2 2 1 2 1     0
    ## 9  1 1 2 1 1   268
    ## 10 1 1 2 2 1    23
    ## 11 1 2 2 1 1   218
    ## 12 1 2 2 2 1    19
    ## 13 2 1 2 1 1    17
    ## 14 2 1 2 2 1     1
    ## 15 2 2 2 1 1   117
    ## 16 2 2 2 2 1    12
    ## 17 1 1 1 1 2   453
    ## 18 1 1 1 2 2    30
    ## 19 1 2 1 1 2    28
    ## 20 1 2 1 2 2     1
    ## 21 2 1 1 1 2     1
    ## 22 2 1 1 2 2     1
    ## 23 2 2 1 1 2     1
    ## 24 2 2 1 2 2     0
    ## 25 1 1 2 1 2   228
    ## 26 1 1 2 2 2    19
    ## 27 1 2 2 1 2   201
    ## 28 1 2 2 2 2    18
    ## 29 2 1 2 1 2    17
    ## 30 2 1 2 2 2     8
    ## 31 2 2 2 1 2   133
    ## 32 2 2 2 2 2    17

``` r
# attach(drug3)
A<-factor(drug3$A); C<-factor(drug3$C); M<-factor(drug3$M); R<-factor(drug3$R); G<-factor(drug3$G)

A<-relevel(A, ref=2); C<-relevel(C, ref=2); M<-relevel(M, ref=2)
```

``` r
# homogeneous association model
homo.assoc.acmgr <- glm(drug3$count~ A + C + M + R + G + A:C + A:M + A:R
+ A:G + C:M + C:R + C:G + M:R + M:G + R:G, family=poisson())
summary(homo.assoc.acmgr)
```

    ## 
    ## Call:
    ## glm(formula = drug3$count ~ A + C + M + R + G + A:C + A:M + A:R + 
    ##     A:G + C:M + C:R + C:G + M:R + M:G + R:G, family = poisson())
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3745  -0.4087  -0.1185   0.2518   2.3241  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  4.72566    0.08823  53.564  < 2e-16 ***
    ## A1           0.66348    0.10594   6.263 3.78e-10 ***
    ## C1          -1.84508    0.17463 -10.566  < 2e-16 ***
    ## M1          -5.45639    0.47983 -11.371  < 2e-16 ***
    ## R2          -2.09519    0.19591 -10.695  < 2e-16 ***
    ## G2           0.17126    0.11363   1.507  0.13177    
    ## A1:C1        2.05380    0.17453  11.768  < 2e-16 ***
    ## A1:M1        2.99068    0.46496   6.432 1.26e-10 ***
    ## A1:R2       -0.50734    0.22374  -2.267  0.02336 *  
    ## A1:G2       -0.24833    0.13531  -1.835  0.06646 .  
    ## C1:M1        2.85956    0.16425  17.409  < 2e-16 ***
    ## C1:R2        0.13544    0.20375   0.665  0.50622    
    ## C1:G2       -0.10958    0.11089  -0.988  0.32310    
    ## M1:R2       -0.37309    0.19685  -1.895  0.05805 .  
    ## M1:G2        0.31765    0.10155   3.128  0.00176 ** 
    ## R2:G2        0.14556    0.15913   0.915  0.36034    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 4818.05  on 31  degrees of freedom
    ## Residual deviance:   15.34  on 16  degrees of freedom
    ## AIC: 183.58
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
M6 <- glm(drug3$count ~ A + C + M + R + G + A:C + A:M + C:M + A:G + A:R + G:M + G:R, family=poisson()) # C1:R2, C1:G2, M1:R2 제외
summary(M6)
```

    ## 
    ## Call:
    ## glm(formula = drug3$count ~ A + C + M + R + G + A:C + A:M + C:M + 
    ##     A:G + A:R + G:M + G:R, family = poisson())
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5357  -0.5772  -0.0178   0.5134   2.4676  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  4.73072    0.08760  54.005  < 2e-16 ***
    ## A1           0.69028    0.10323   6.687 2.28e-11 ***
    ## C1          -1.88667    0.16270 -11.596  < 2e-16 ***
    ## M1          -5.46426    0.47842 -11.421  < 2e-16 ***
    ## R2          -2.06959    0.19300 -10.723  < 2e-16 ***
    ## G2           0.15883    0.11264   1.410  0.15854    
    ## A1:C1        2.05453    0.17406  11.803  < 2e-16 ***
    ## A1:M1        3.00592    0.46484   6.467 1.00e-10 ***
    ## C1:M1        2.84789    0.16384  17.382  < 2e-16 ***
    ## A1:G2       -0.29229    0.12768  -2.289  0.02207 *  
    ## A1:R2       -0.59346    0.19268  -3.080  0.00207 ** 
    ## M1:G2        0.26929    0.09039   2.979  0.00289 ** 
    ## R2:G2        0.12619    0.15866   0.795  0.42644    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 4818.051  on 31  degrees of freedom
    ## Residual deviance:   19.909  on 19  degrees of freedom
    ## AIC: 182.15
    ## 
    ## Number of Fisher Scoring iterations: 5
