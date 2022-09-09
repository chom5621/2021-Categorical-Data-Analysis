Statistical Graphics with Big Data EXERCISE\#2
================
Jo, Munsun
2021 11 19

### 1\. R에 내장되어 있는 mtcars를 자료를 이용

#### Problem 1

``` r
?mtcars
```

    ## starting httpd help server ... done

?mtcars 코드를 이용하여 R에 내장되어 있는 mtcars 자료에 대한 설명을 구했고, 변수들의 설명은 다음과 같습니다.

\[, 1\] mpg Miles/(US) gallon

\[, 2\] cyl Number of cylinders

\[, 3\] disp Displacement (cu.in.)

\[, 4\] hp Gross horsepower

\[, 5\] drat Rear axle ratio

\[, 6\] wt Weight (1000 lbs)

\[, 7\] qsec 1/4 mile time

\[, 8\] vs Engine (0 = V-shaped, 1 = straight)

\[, 9\] am Transmission (0 = automatic, 1 = manual)

\[,10\] gear Number of forward gears

\[,11\] carb Number of carburetors

#### Problem 2

By above code, mtcars is a data frame with 32 observations on 11
(numeric) variables.

  - observation의 개수, 즉 데이터의 행(row) 개수는 32개이고

  - variables의 개수, 즉 데이터의 열(column) 개수는 11개이므로

mtcars 자료의 개수는 32개입니다.

변수의 개수는 \#1에서 설명한 것과 같이 variables의 개수인 11개입니다.

#### Problem 3

``` r
getwd()
```

    ## [1] "C:/Users/chom5/Desktop"

``` r
setwd("C:/Temp")
write.csv(mtcars,file="mtcars.csv")
```

getwd()를 통해 현재 작업디렉토리를 확인하고, setwd()를 통해 작업디렉토리를 “C:/Temp”로 지정했습니다.

그리고 write.csv()를 이용하여 “mtcars.csv” 파일을 생성했습니다.

#### Problem 4

``` r
mtcars.read <- read.csv("C:/Temp/mtcars.csv")
```

생성한 mtcars.csv 파일을 read.csv()를 통해 읽어들여 mtcars.read라는 이름의 object를 만들었습니다.

#### Problem 5

``` r
head(mtcars)
```

    ##                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
    ## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
    ## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
    ## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
    ## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
    ## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
    ## Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

``` r
head(mtcars.read)
```

    ##                   X  mpg cyl disp  hp drat    wt  qsec vs am gear carb
    ## 1         Mazda RX4 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
    ## 2     Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
    ## 3        Datsun 710 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
    ## 4    Hornet 4 Drive 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
    ## 5 Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
    ## 6           Valiant 18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

head()를 통해 mtcars와 mtcars.read의 처음 6줄씩을 비교해보았습니다.

차이점으로는 mtcars는 데이터셋 그대로 표현된 것과 달리,

mtcars.read는 가장 왼쪽에 1, 2, 3, …으로 observation 값이 카운트되고 observation 값들이
X라는 변수에 담아졌다는 것입니다.

#### Problem 6

``` r
isAuto <- as.logical(mtcars$am)
isAuto
```

    ##  [1]  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [13] FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
    ## [25] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE

transmission이 automatic인지 아닌지를 TRUE/FALSE로 나타내는 논리벡터 isAuto로 나타내기 위해

0과 1로 표현된 mtcars$am이라는 숫자벡터를

as.logical()이라는 강제형변환에 넣었습니다.

#### Problem 7

``` r
m <- mean(mtcars$mpg)
mpgR <- ifelse(mtcars$mpg>=m,100,0)
```

mpg값이 평균보다 크거나 같으면 100, 작으면 0의 값을 갖는 새로운 숫자벡터 mpgR을 만들었습니다.

먼저 mean()을 이용하여 mtcars$mpg의 평균 m을 구했습니다.

그리고 ifelse()를 이용하여 평균보다 크거나 같으면 100, 작으면 0의 값을 가지는 새로운 숫자벡터 mpgR을 구했습니다.
