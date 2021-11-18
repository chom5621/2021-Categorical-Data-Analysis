Categorical Data Analysis EXERCISE
================
Jo, Munsun
2021 11 18

## Framingham data (from Kaggle website)

### Heart Disease and Blood Pressure

#### Response variable:

  - 10 year risk of coronary heart disease CHD (1:Yes / 2:No)

#### Explanatory variables:

  - Demographic: sex, age

  - Behavioral: currentSmoker, cigsPerDay

  - Medical (binary, historical): BPMeds (blood pressure medication);
    prevalentStroke (stroke);

  - prevalentHyp (hypertensive); diabetes Medical (continuous, current):
    totChol (total cholesterol level); sysBP (systolic BP); diaBP
    (diastolic BP); BMI; heartrate; glucose

<!-- end list -->

``` r
# 데이터 불러오기
setwd("/Users/chom5/Desktop/범주형")
fram <- read.csv("pr1_framingham.csv", header=TRUE)

# 데이터 확인
head(fram[ , 1:6])
```

    ##   male age education currentSmoker cigsPerDay BPMeds
    ## 1    1  39         4             0          0      0
    ## 2    0  46         2             0          0      0
    ## 3    1  48         1             1         20      0
    ## 4    0  61         3             1         30      0
    ## 5    0  46         3             1         23      0
    ## 6    0  43         2             0          0      0

``` r
head(fram[ , 7:10])
```

    ##   prevalentStroke prevalentHyp diabetes totChol
    ## 1               0            0        0     195
    ## 2               0            0        0     250
    ## 3               0            0        0     245
    ## 4               0            1        0     225
    ## 5               0            0        0     285
    ## 6               0            1        0     228

``` r
head(fram[ , c(1:10)])
```

    ##   male age education currentSmoker cigsPerDay BPMeds prevalentStroke
    ## 1    1  39         4             0          0      0               0
    ## 2    0  46         2             0          0      0               0
    ## 3    1  48         1             1         20      0               0
    ## 4    0  61         3             1         30      0               0
    ## 5    0  46         3             1         23      0               0
    ## 6    0  43         2             0          0      0               0
    ##   prevalentHyp diabetes totChol
    ## 1            0        0     195
    ## 2            0        0     250
    ## 3            0        0     245
    ## 4            1        0     225
    ## 5            0        0     285
    ## 6            1        0     228

``` r
str(fram)
```

    ## 'data.frame':    4240 obs. of  16 variables:
    ##  $ male           : int  1 0 1 0 0 0 0 0 1 1 ...
    ##  $ age            : int  39 46 48 61 46 43 63 45 52 43 ...
    ##  $ education      : int  4 2 1 3 3 2 1 2 1 1 ...
    ##  $ currentSmoker  : int  0 0 1 1 1 0 0 1 0 1 ...
    ##  $ cigsPerDay     : int  0 0 20 30 23 0 0 20 0 30 ...
    ##  $ BPMeds         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ prevalentStroke: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ prevalentHyp   : int  0 0 0 1 0 1 0 0 1 1 ...
    ##  $ diabetes       : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ totChol        : int  195 250 245 225 285 228 205 313 260 225 ...
    ##  $ sysBP          : num  106 121 128 150 130 ...
    ##  $ diaBP          : num  70 81 80 95 84 110 71 71 89 107 ...
    ##  $ BMI            : num  27 28.7 25.3 28.6 23.1 ...
    ##  $ heartRate      : int  80 95 75 65 85 77 60 79 76 93 ...
    ##  $ glucose        : int  77 76 70 103 85 99 85 78 79 88 ...
    ##  $ TenYearCHD     : int  0 0 0 1 0 0 1 0 0 0 ...

``` r
# Convert categorical variables to factors
fram$BPMeds = as.factor(fram$BPMeds)
fram$prevalentStroke = as.factor(fram$prevalentStroke)
fram$prevalentHyp = as.factor(fram$prevalentHyp)
fram$diabetes = as.factor(fram$diabetes)
fram$TenYearCHD = as.factor(fram$TenYearCHD)
```

``` r
# Correlation plot
fram_new = na.omit(fram)
str(fram_new)
```

    ## 'data.frame':    3658 obs. of  16 variables:
    ##  $ male           : int  1 0 1 0 0 0 0 0 1 1 ...
    ##  $ age            : int  39 46 48 61 46 43 63 45 52 43 ...
    ##  $ education      : int  4 2 1 3 3 2 1 2 1 1 ...
    ##  $ currentSmoker  : int  0 0 1 1 1 0 0 1 0 1 ...
    ##  $ cigsPerDay     : int  0 0 20 30 23 0 0 20 0 30 ...
    ##  $ BPMeds         : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ prevalentStroke: Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ prevalentHyp   : Factor w/ 2 levels "0","1": 1 1 1 2 1 2 1 1 2 2 ...
    ##  $ diabetes       : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ totChol        : int  195 250 245 225 285 228 205 313 260 225 ...
    ##  $ sysBP          : num  106 121 128 150 130 ...
    ##  $ diaBP          : num  70 81 80 95 84 110 71 71 89 107 ...
    ##  $ BMI            : num  27 28.7 25.3 28.6 23.1 ...
    ##  $ heartRate      : int  80 95 75 65 85 77 60 79 76 93 ...
    ##  $ glucose        : int  77 76 70 103 85 99 85 78 79 88 ...
    ##  $ TenYearCHD     : Factor w/ 2 levels "0","1": 1 1 1 2 1 1 2 1 1 1 ...
    ##  - attr(*, "na.action")= 'omit' Named int [1:582] 15 22 27 34 37 43 50 55 71 73 ...
    ##   ..- attr(*, "names")= chr [1:582] "15" "22" "27" "34" ...

``` r
library(corrplot)
```

    ## corrplot 0.91 loaded

``` r
fram_new_num = subset(fram_new[c(2,5,10:15)])
corrplot(cor(fram_new_num))
```

![](Framingham-data--from-Kaggle-website-_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# model selection - logistic model
fram_new_1 = glm(TenYearCHD ~ ., data = fram_new, family = binomial)
summary(fram_new_1)
```

    ## 
    ## Call:
    ## glm(formula = TenYearCHD ~ ., family = binomial, data = fram_new)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.9582  -0.5939  -0.4264  -0.2829   2.8409  
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -8.328186   0.715449 -11.641  < 2e-16 ***
    ## male              0.555279   0.109033   5.093 3.53e-07 ***
    ## age               0.063515   0.006679   9.509  < 2e-16 ***
    ## education        -0.047767   0.049395  -0.967  0.33353    
    ## currentSmoker     0.071601   0.156752   0.457  0.64783    
    ## cigsPerDay        0.017914   0.006238   2.872  0.00408 ** 
    ## BPMeds1           0.162496   0.234326   0.693  0.48802    
    ## prevalentStroke1  0.693660   0.489569   1.417  0.15652    
    ## prevalentHyp1     0.234208   0.138026   1.697  0.08973 .  
    ## diabetes1         0.039167   0.315506   0.124  0.90120    
    ## totChol           0.002332   0.001127   2.070  0.03850 *  
    ## sysBP             0.015403   0.003808   4.044 5.24e-05 ***
    ## diaBP            -0.004159   0.006438  -0.646  0.51831    
    ## BMI               0.006672   0.012758   0.523  0.60097    
    ## heartRate        -0.003246   0.004211  -0.771  0.44082    
    ## glucose           0.007127   0.002234   3.190  0.00142 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3121.2  on 3657  degrees of freedom
    ## Residual deviance: 2754.5  on 3642  degrees of freedom
    ## AIC: 2786.5
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
fram_new_2 <- glm(TenYearCHD ~ . - diabetes, data = fram_new, family = binomial)
summary(fram_new_2)
```

    ## 
    ## Call:
    ## glm(formula = TenYearCHD ~ . - diabetes, family = binomial, data = fram_new)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.9647  -0.5938  -0.4267  -0.2827   2.8422  
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -8.344657   0.703109 -11.868  < 2e-16 ***
    ## male              0.555642   0.108995   5.098 3.43e-07 ***
    ## age               0.063534   0.006677   9.515  < 2e-16 ***
    ## education        -0.047839   0.049394  -0.969  0.33279    
    ## currentSmoker     0.071479   0.156739   0.456  0.64836    
    ## cigsPerDay        0.017908   0.006238   2.871  0.00409 ** 
    ## BPMeds1           0.162916   0.234290   0.695  0.48683    
    ## prevalentStroke1  0.693140   0.489495   1.416  0.15677    
    ## prevalentHyp1     0.234439   0.138011   1.699  0.08938 .  
    ## totChol           0.002334   0.001127   2.071  0.03831 *  
    ## sysBP             0.015407   0.003808   4.046 5.22e-05 ***
    ## diaBP            -0.004180   0.006435  -0.649  0.51605    
    ## BMI               0.006771   0.012734   0.532  0.59488    
    ## heartRate        -0.003249   0.004211  -0.772  0.44039    
    ## glucose           0.007309   0.001683   4.342 1.41e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3121.2  on 3657  degrees of freedom
    ## Residual deviance: 2754.5  on 3643  degrees of freedom
    ## AIC: 2784.5
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
# criterion for model selection - AIC
step(fram_new_1, direction="backward")
```

    ## Start:  AIC=2786.48
    ## TenYearCHD ~ male + age + education + currentSmoker + cigsPerDay + 
    ##     BPMeds + prevalentStroke + prevalentHyp + diabetes + totChol + 
    ##     sysBP + diaBP + BMI + heartRate + glucose
    ## 
    ##                   Df Deviance    AIC
    ## - diabetes         1   2754.5 2784.5
    ## - currentSmoker    1   2754.7 2784.7
    ## - BMI              1   2754.8 2784.8
    ## - diaBP            1   2754.9 2784.9
    ## - BPMeds           1   2754.9 2784.9
    ## - heartRate        1   2755.1 2785.1
    ## - education        1   2755.4 2785.4
    ## - prevalentStroke  1   2756.4 2786.4
    ## <none>                 2754.5 2786.5
    ## - prevalentHyp     1   2757.3 2787.3
    ## - totChol          1   2758.7 2788.7
    ## - cigsPerDay       1   2762.6 2792.6
    ## - glucose          1   2765.0 2795.0
    ## - sysBP            1   2770.8 2800.8
    ## - male             1   2780.6 2810.6
    ## - age              1   2848.0 2878.0
    ## 
    ## Step:  AIC=2784.49
    ## TenYearCHD ~ male + age + education + currentSmoker + cigsPerDay + 
    ##     BPMeds + prevalentStroke + prevalentHyp + totChol + sysBP + 
    ##     diaBP + BMI + heartRate + glucose
    ## 
    ##                   Df Deviance    AIC
    ## - currentSmoker    1   2754.7 2782.7
    ## - BMI              1   2754.8 2782.8
    ## - diaBP            1   2754.9 2782.9
    ## - BPMeds           1   2755.0 2783.0
    ## - heartRate        1   2755.1 2783.1
    ## - education        1   2755.4 2783.4
    ## - prevalentStroke  1   2756.4 2784.4
    ## <none>                 2754.5 2784.5
    ## - prevalentHyp     1   2757.4 2785.4
    ## - totChol          1   2758.7 2786.7
    ## - cigsPerDay       1   2762.6 2790.6
    ## - sysBP            1   2770.8 2798.8
    ## - glucose          1   2773.3 2801.3
    ## - male             1   2780.7 2808.7
    ## - age              1   2848.2 2876.2
    ## 
    ## Step:  AIC=2782.7
    ## TenYearCHD ~ male + age + education + cigsPerDay + BPMeds + prevalentStroke + 
    ##     prevalentHyp + totChol + sysBP + diaBP + BMI + heartRate + 
    ##     glucose
    ## 
    ##                   Df Deviance    AIC
    ## - BMI              1   2754.9 2780.9
    ## - diaBP            1   2755.1 2781.1
    ## - BPMeds           1   2755.2 2781.2
    ## - heartRate        1   2755.3 2781.3
    ## - education        1   2755.7 2781.7
    ## - prevalentStroke  1   2756.6 2782.6
    ## <none>                 2754.7 2782.7
    ## - prevalentHyp     1   2757.6 2783.6
    ## - totChol          1   2758.9 2784.9
    ## - sysBP            1   2771.1 2797.1
    ## - glucose          1   2773.5 2799.5
    ## - cigsPerDay       1   2776.6 2802.6
    ## - male             1   2780.8 2806.8
    ## - age              1   2848.2 2874.2
    ## 
    ## Step:  AIC=2780.93
    ## TenYearCHD ~ male + age + education + cigsPerDay + BPMeds + prevalentStroke + 
    ##     prevalentHyp + totChol + sysBP + diaBP + heartRate + glucose
    ## 
    ##                   Df Deviance    AIC
    ## - diaBP            1   2755.3 2779.3
    ## - BPMeds           1   2755.4 2779.4
    ## - heartRate        1   2755.5 2779.5
    ## - education        1   2756.0 2780.0
    ## - prevalentStroke  1   2756.9 2780.9
    ## <none>                 2754.9 2780.9
    ## - prevalentHyp     1   2757.9 2781.9
    ## - totChol          1   2759.2 2783.2
    ## - sysBP            1   2771.3 2795.3
    ## - glucose          1   2774.0 2798.0
    ## - cigsPerDay       1   2776.6 2800.6
    ## - male             1   2781.2 2805.2
    ## - age              1   2848.3 2872.3
    ## 
    ## Step:  AIC=2779.27
    ## TenYearCHD ~ male + age + education + cigsPerDay + BPMeds + prevalentStroke + 
    ##     prevalentHyp + totChol + sysBP + heartRate + glucose
    ## 
    ##                   Df Deviance    AIC
    ## - BPMeds           1   2755.8 2777.8
    ## - heartRate        1   2755.9 2777.9
    ## - education        1   2756.4 2778.4
    ## - prevalentStroke  1   2757.2 2779.2
    ## <none>                 2755.3 2779.3
    ## - prevalentHyp     1   2758.0 2780.0
    ## - totChol          1   2759.6 2781.6
    ## - glucose          1   2774.7 2796.7
    ## - cigsPerDay       1   2777.2 2799.2
    ## - sysBP            1   2778.5 2800.5
    ## - male             1   2781.3 2803.3
    ## - age              1   2855.3 2877.3
    ## 
    ## Step:  AIC=2777.78
    ## TenYearCHD ~ male + age + education + cigsPerDay + prevalentStroke + 
    ##     prevalentHyp + totChol + sysBP + heartRate + glucose
    ## 
    ##                   Df Deviance    AIC
    ## - heartRate        1   2756.4 2776.4
    ## - education        1   2756.9 2776.9
    ## <none>                 2755.8 2777.8
    ## - prevalentStroke  1   2757.9 2777.9
    ## - prevalentHyp     1   2758.7 2778.7
    ## - totChol          1   2760.2 2780.2
    ## - glucose          1   2775.3 2795.3
    ## - cigsPerDay       1   2777.7 2797.7
    ## - sysBP            1   2780.4 2800.4
    ## - male             1   2781.5 2801.5
    ## - age              1   2856.1 2876.1
    ## 
    ## Step:  AIC=2776.45
    ## TenYearCHD ~ male + age + education + cigsPerDay + prevalentStroke + 
    ##     prevalentHyp + totChol + sysBP + glucose
    ## 
    ##                   Df Deviance    AIC
    ## - education        1   2757.5 2775.5
    ## <none>                 2756.4 2776.4
    ## - prevalentStroke  1   2758.7 2776.7
    ## - prevalentHyp     1   2759.3 2777.3
    ## - totChol          1   2760.7 2778.7
    ## - glucose          1   2775.5 2793.5
    ## - cigsPerDay       1   2777.7 2795.7
    ## - sysBP            1   2780.5 2798.5
    ## - male             1   2783.5 2801.5
    ## - age              1   2858.7 2876.7
    ## 
    ## Step:  AIC=2775.5
    ## TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + 
    ##     totChol + sysBP + glucose
    ## 
    ##                   Df Deviance    AIC
    ## <none>                 2757.5 2775.5
    ## - prevalentStroke  1   2759.8 2775.8
    ## - prevalentHyp     1   2760.3 2776.3
    ## - totChol          1   2761.5 2777.5
    ## - glucose          1   2776.7 2792.7
    ## - cigsPerDay       1   2779.0 2795.0
    ## - sysBP            1   2782.3 2798.3
    ## - male             1   2784.4 2800.4
    ## - age              1   2864.5 2880.5

    ## 
    ## Call:  glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + 
    ##     prevalentHyp + totChol + sysBP + glucose, family = binomial, 
    ##     data = fram_new)
    ## 
    ## Coefficients:
    ##      (Intercept)              male               age        cigsPerDay  
    ##        -8.745885          0.553297          0.065411          0.019579  
    ## prevalentStroke1     prevalentHyp1           totChol             sysBP  
    ##         0.751698          0.225762          0.002257          0.014218  
    ##          glucose  
    ##         0.007317  
    ## 
    ## Degrees of Freedom: 3657 Total (i.e. Null);  3649 Residual
    ## Null Deviance:       3121 
    ## Residual Deviance: 2757  AIC: 2775
