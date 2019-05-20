homework
================
xiaolin
2019/5/13

``` r
library(ggplot2)
df=read.csv("E:/BDA/group_homework/customer_backup.csv",sep=",",header = TRUE,encoding = "utf-8")
```

加载数据集为data.frame类型的df

``` r
class(df)
```

    ## [1] "data.frame"

``` r
dim(df)
```

    ## [1] 10281    27

``` r
head(df)
```

    ##   锘縞ustomer_id account_num     lname   fname mi            address1
    ## 1              1 87462024688    Nowmer   Sheri A.    2433 Bailey Road
    ## 2              2 87470586299   Whelply Derrick I.  2219 Dewing Avenue
    ## 3              3 87475757600     Derry  Jeanne        7640 First Ave.
    ## 4              4 87500482201    Spence Michael J.       337 Tosca Way
    ## 5              5 87514054179 Gutierrez    Maya        8668 Via Neruda
    ## 6              6 87517782449   Damstra  Robert F. 1619 Stillman Court
    ##       city state_province postal_code country customer_region_id
    ## 1 Tlaxiaco         Oaxaca       15057  Mexico                 30
    ## 2    Sooke             BC       17172  Canada                101
    ## 3 Issaquah             WA       73980     USA                 21
    ## 4  Burnaby             BC       74674  Canada                 92
    ## 5   Novato             CA       57355     USA                 42
    ## 6 Lynnwood             WA       90792     USA                 75
    ##         phone1       phone2  birthdate marital_status yearly_income_from
    ## 1 271-555-9715 119-555-1969 1961-08-26              M          30000.00 
    ## 2 211-555-7669 807-555-9033 1915-07-03              S          70000.00 
    ## 3 656-555-2272 221-555-2493 1910-06-21              M          50000.00 
    ## 4 929-555-7279 272-555-2844 1969-06-20              M          10000.00 
    ## 5 387-555-7172 260-555-6936 1951-05-10              S          30000.00 
    ## 6 922-555-5465 333-555-5915 1942-10-08              S          70000.00 
    ##   yearly_income_to gender total_children num_children_at_home
    ## 1            50000      F              4                    2
    ## 2            90000      M              1                    0
    ## 3            70000      F              1                    1
    ## 4            30000      M              4                    4
    ## 5            50000      F              3                    0
    ## 6            90000      F              3                    0
    ##             education date_accnt_opened member_card     occupation
    ## 1 Partial High School        1991-09-10      Bronze Skilled Manual
    ## 2 Partial High School        1993-03-11      Bronze   Professional
    ## 3    Bachelors Degree        1991-06-11      Bronze   Professional
    ## 4 Partial High School        1994-05-21      Normal Skilled Manual
    ## 5     Partial College        1992-08-21      Silver         Manual
    ## 6    Bachelors Degree        1992-04-05      Bronze   Professional
    ##   houseowner num_cars_owned        fullname
    ## 1          Y              4    Sheri Nowmer
    ## 2          N              3 Derrick Whelply
    ## 3          Y              2    Jeanne Derry
    ## 4          N              2  Michael Spence
    ## 5          N              3  Maya Gutierrez
    ## 6          Y              3  Robert Damstra

``` r
tail(df)
```

    ##       锘縞ustomer_id account_num   lname   fname mi              address1
    ## 10276          10276 87412865200 Bouvier Shaneen        9234 Carmel Drive
    ## 10277          10277 87439274191    Ross    Fran    5603 Blackridge Drive
    ## 10278          10278 87448420500 Calahoo  Myreda        263 La Orinda Pl.
    ## 10279          10279 87453135848   Ayers    Mary V.           6885 Auburn
    ## 10280          10280 87458639740  Aiello  Ernest J.      5077 Bannock Ct.
    ## 10281          10281 87460163235 Cartney  Samuel K.    4609 Parkway Drive
    ##                city state_province postal_code country customer_region_id
    ## 10276      Victoria             BC       21802  Canada                  6
    ## 10277   Lake Oswego             OR       52724     USA                 64
    ## 10278  N. Vancouver             BC       71758  Canada                 97
    ## 10279 Lincoln Acres             CA       42550     USA                 10
    ## 10280      Puyallup             WA       27746     USA                 86
    ## 10281     Vancouver             BC       63699  Canada                  5
    ##             phone1       phone2  birthdate marital_status
    ## 10276 451-555-1043 455-555-1950 1932-05-08              M
    ## 10277 570-555-6278 682-555-9700 1974-02-09              M
    ## 10278 205-555-4571 516-555-5021 1926-12-08              M
    ## 10279 586-555-3093 766-555-2399 1913-05-18              S
    ## 10280 602-555-5694 560-555-9344 1968-09-06              M
    ## 10281 106-555-2844 833-555-5351 1914-07-06              S
    ##       yearly_income_from yearly_income_to gender total_children
    ## 10276          30000.00             50000      M              0
    ## 10277          90000.00            110000      M              4
    ## 10278          30000.00             50000      F              0
    ## 10279         130000.00            150000      M              3
    ## 10280           150000 +               NA      F              5
    ## 10281          50000.00             70000      F              5
    ##       num_children_at_home           education date_accnt_opened
    ## 10276                    0     Partial College        1993-03-09
    ## 10277                    3 Partial High School        1991-03-14
    ## 10278                    0     Partial College        1992-03-20
    ## 10279                    0 Partial High School        1991-11-22
    ## 10280                    2  High School Degree        1991-05-26
    ## 10281                    0    Bachelors Degree        1993-07-22
    ##       member_card   occupation houseowner num_cars_owned        fullname
    ## 10276      Bronze Professional          N              3 Shaneen Bouvier
    ## 10277      Bronze   Management          N              3       Fran Ross
    ## 10278      Bronze Professional          N              2  Myreda Calahoo
    ## 10279      Bronze   Management          Y              2      Mary Ayers
    ## 10280      Golden Professional          Y              2   Ernest Aiello
    ## 10281      Bronze   Management          N              2  Samuel Cartney

看一下数据的大小和基本结构

``` r
summary(df)
```

    ##  锘縞ustomer_id   account_num             lname          fname     
    ##  Min.   :    1   Min.   :1.001e+10   Smith   : 118   John   : 181  
    ##  1st Qu.: 2571   1st Qu.:2.836e+10   Wilson  :  51   Robert : 169  
    ##  Median : 5141   Median :4.849e+10   Brown   :  48   James  : 152  
    ##  Mean   : 5141   Mean   :5.032e+10   Williams:  46   Mary   : 151  
    ##  3rd Qu.: 7711   3rd Qu.:7.088e+10   Anderson:  43   Michael: 122  
    ##  Max.   :10281   Max.   :9.996e+10   Clark   :  41   David  : 111  
    ##                                      (Other) :9934   (Other):9395  
    ##        mi                       address1              city     
    ##         :4336   1218 Woodside Court :    2   Richmond   : 191  
    ##  A.     : 712   1314 Greenview Court:    2   Shawnee    : 111  
    ##  L.     : 684   3747 Likins Avenue  :    2   Downey     : 110  
    ##  M.     : 605   4041 Jennifer Way   :    2   Lebanon    : 108  
    ##  J.     : 533   4143 Smith Lane     :    2   Lemon Grove: 107  
    ##  C.     : 518   419 River Ash Court :    2   Acapulco   : 106  
    ##  (Other):2893   (Other)             :10269   (Other)    :9548  
    ##    state_province  postal_code      country     customer_region_id
    ##  CA       :4222   Min.   : 1374   Canada:1717   Min.   :  1.00    
    ##  WA       :2086   1st Qu.:32760   Mexico:1205   1st Qu.: 28.00    
    ##  BC       :1717   Median :55288   USA   :7359   Median : 55.00    
    ##  OR       :1051   Mean   :55313                 Mean   : 55.07    
    ##  DF       : 347   3rd Qu.:78209                 3rd Qu.: 82.00    
    ##  Zacatecas: 191   Max.   :99993                 Max.   :109.00    
    ##  (Other)  : 667                                                   
    ##           phone1               phone2           birthdate    
    ##  100-555-1488:    1   100-555-1972:    1   1927-05-04:    5  
    ##  100-555-2960:    1   100-555-2602:    1   1933-03-03:    5  
    ##  100-555-4026:    1   100-555-3511:    1   1944-10-18:    5  
    ##  100-555-5329:    1   100-555-4129:    1   1960-05-22:    5  
    ##  100-555-6151:    1   100-555-4746:    1   1960-07-22:    5  
    ##  100-555-7044:    1   100-555-6187:    1   1912-05-04:    4  
    ##  (Other)     :10275   (Other)     :10275   (Other)   :10252  
    ##  marital_status  yearly_income_from yearly_income_to gender  
    ##  M:5142         30000.00  :3327     Min.   : 30000   F:5097  
    ##  S:5139         10000.00  :2222     1st Qu.: 50000   M:5184  
    ##                 50000.00  :1845     Median : 50000           
    ##                 70000.00  :1207     Mean   : 65735           
    ##                 130000.00 : 506     3rd Qu.: 90000           
    ##                 110000.00 : 493     Max.   :150000           
    ##                 (Other)   : 681     NA's   :223              
    ##  total_children  num_children_at_home               education   
    ##  Min.   :0.000   Min.   :0.0000       Bachelors Degree   :2619  
    ##  1st Qu.:1.000   1st Qu.:0.0000       Graduate Degree    : 539  
    ##  Median :3.000   Median :0.0000       High School Degree :3039  
    ##  Mean   :2.503   Mean   :0.8146       Partial College    : 990  
    ##  3rd Qu.:4.000   3rd Qu.:1.0000       Partial High School:3094  
    ##  Max.   :5.000   Max.   :5.0000                                 
    ##                                                                 
    ##   date_accnt_opened member_card            occupation   houseowner
    ##  1991-07-14:   19   Bronze:5703   Clerical      : 205   N:4094    
    ##  1991-03-04:   18   Golden:1198   Management    :1461   Y:6187    
    ##  1991-05-04:   18   Normal:2420   Manual        :2583             
    ##  1991-09-16:   18   Silver: 960   Professional  :3382             
    ##  1991-02-10:   17                 Skilled Manual:2650             
    ##  1991-09-08:   17                                                 
    ##  (Other)   :10174                                                 
    ##  num_cars_owned           fullname    
    ##  Min.   :0.000   Andrew Bell  :    3  
    ##  1st Qu.:1.000   Heather Smith:    3  
    ##  Median :2.000   Mary Smith   :    3  
    ##  Mean   :2.223   Richard Smith:    3  
    ##  3rd Qu.:3.000   Robert Clark :    3  
    ##  Max.   :4.000   Robert Cooper:    3  
    ##                  (Other)      :10263

看一下数据的基本特征

``` r
attach(df)
df$account_num=as.character(account_num)
df$birthdate=as.Date(birthdate)
df$date_accnt_opened=as.Date(date_accnt_opened)
detach(df)
```

修改下数据结构

``` r
summary(df)
```

    ##  锘縞ustomer_id  account_num             lname          fname     
    ##  Min.   :    1   Length:10281       Smith   : 118   John   : 181  
    ##  1st Qu.: 2571   Class :character   Wilson  :  51   Robert : 169  
    ##  Median : 5141   Mode  :character   Brown   :  48   James  : 152  
    ##  Mean   : 5141                      Williams:  46   Mary   : 151  
    ##  3rd Qu.: 7711                      Anderson:  43   Michael: 122  
    ##  Max.   :10281                      Clark   :  41   David  : 111  
    ##                                     (Other) :9934   (Other):9395  
    ##        mi                       address1              city     
    ##         :4336   1218 Woodside Court :    2   Richmond   : 191  
    ##  A.     : 712   1314 Greenview Court:    2   Shawnee    : 111  
    ##  L.     : 684   3747 Likins Avenue  :    2   Downey     : 110  
    ##  M.     : 605   4041 Jennifer Way   :    2   Lebanon    : 108  
    ##  J.     : 533   4143 Smith Lane     :    2   Lemon Grove: 107  
    ##  C.     : 518   419 River Ash Court :    2   Acapulco   : 106  
    ##  (Other):2893   (Other)             :10269   (Other)    :9548  
    ##    state_province  postal_code      country     customer_region_id
    ##  CA       :4222   Min.   : 1374   Canada:1717   Min.   :  1.00    
    ##  WA       :2086   1st Qu.:32760   Mexico:1205   1st Qu.: 28.00    
    ##  BC       :1717   Median :55288   USA   :7359   Median : 55.00    
    ##  OR       :1051   Mean   :55313                 Mean   : 55.07    
    ##  DF       : 347   3rd Qu.:78209                 3rd Qu.: 82.00    
    ##  Zacatecas: 191   Max.   :99993                 Max.   :109.00    
    ##  (Other)  : 667                                                   
    ##           phone1               phone2        birthdate         
    ##  100-555-1488:    1   100-555-1972:    1   Min.   :1910-01-06  
    ##  100-555-2960:    1   100-555-2602:    1   1st Qu.:1928-01-22  
    ##  100-555-4026:    1   100-555-3511:    1   Median :1945-06-16  
    ##  100-555-5329:    1   100-555-4129:    1   Mean   :1945-06-10  
    ##  100-555-6151:    1   100-555-4746:    1   3rd Qu.:1962-09-13  
    ##  100-555-7044:    1   100-555-6187:    1   Max.   :1980-12-24  
    ##  (Other)     :10275   (Other)     :10275                       
    ##  marital_status  yearly_income_from yearly_income_to gender  
    ##  M:5142         30000.00  :3327     Min.   : 30000   F:5097  
    ##  S:5139         10000.00  :2222     1st Qu.: 50000   M:5184  
    ##                 50000.00  :1845     Median : 50000           
    ##                 70000.00  :1207     Mean   : 65735           
    ##                 130000.00 : 506     3rd Qu.: 90000           
    ##                 110000.00 : 493     Max.   :150000           
    ##                 (Other)   : 681     NA's   :223              
    ##  total_children  num_children_at_home               education   
    ##  Min.   :0.000   Min.   :0.0000       Bachelors Degree   :2619  
    ##  1st Qu.:1.000   1st Qu.:0.0000       Graduate Degree    : 539  
    ##  Median :3.000   Median :0.0000       High School Degree :3039  
    ##  Mean   :2.503   Mean   :0.8146       Partial College    : 990  
    ##  3rd Qu.:4.000   3rd Qu.:1.0000       Partial High School:3094  
    ##  Max.   :5.000   Max.   :5.0000                                 
    ##                                                                 
    ##  date_accnt_opened    member_card            occupation   houseowner
    ##  Min.   :1990-01-01   Bronze:5703   Clerical      : 205   N:4094    
    ##  1st Qu.:1991-06-27   Golden:1198   Management    :1461   Y:6187    
    ##  Median :1992-07-01   Normal:2420   Manual        :2583             
    ##  Mean   :1992-06-26   Silver: 960   Professional  :3382             
    ##  3rd Qu.:1993-06-27                 Skilled Manual:2650             
    ##  Max.   :1994-12-27                                                 
    ##                                                                     
    ##  num_cars_owned           fullname    
    ##  Min.   :0.000   Andrew Bell  :    3  
    ##  1st Qu.:1.000   Heather Smith:    3  
    ##  Median :2.000   Mary Smith   :    3  
    ##  Mean   :2.223   Richard Smith:    3  
    ##  3rd Qu.:3.000   Robert Clark :    3  
    ##  Max.   :4.000   Robert Cooper:    3  
    ##                  (Other)      :10263

重新查看下数据的基本特征

``` r
plot(df$gender)
```

![](xaiolin_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

顾客性别分布描述

``` r
ggplot(data=df)+geom_bar(mapping = aes(gender,fill=gender))
```

![](xaiolin_files/figure-gfm/unnamed-chunk-7-1.png)<!-- --> 尝试ggplot2

``` r
ggplot(data = df) + 
  geom_bar(mapping = aes(x = gender, fill = education))
```

![](xaiolin_files/figure-gfm/unnamed-chunk-8-1.png)<!-- --> 尝试ggplot2

``` r
ggplot(data = df) + 
  geom_bar(mapping = aes(x = gender, fill = occupation))
```

![](xaiolin_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggplot(data = df, mapping = aes(x = gender, colour = education)) + 
  geom_bar(fill = NA, position = "identity")
```

![](xaiolin_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
ggplot(data = df) + 
  geom_bar(mapping = aes(x = gender, fill = country), position = "dodge")
```

![](xaiolin_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

``` r
plot(df$marital_status)
```

![](xaiolin_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> 顾客婚姻状况描述

``` r
plot(df$occupation)
```

![](xaiolin_files/figure-gfm/unnamed-chunk-11-1.png)<!-- --> 顾客职业分布描述

``` r
plot(df$yearly_income_from)
```

![](xaiolin_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
plot(df$yearly_income_to)
```

![](xaiolin_files/figure-gfm/unnamed-chunk-12-2.png)<!-- --> 顾客收入水平描述
