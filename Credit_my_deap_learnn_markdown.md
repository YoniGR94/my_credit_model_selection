Different Techniches to Analyze Credit Balance
================

How good can we predict Credit balance?

What is the best way to check it?

In order to race our models, we will use the Credit data from ILSR
package.

About this data:

“A simulated data set containing information on ten thousand customers.
The aim here is to predict which customers will default on their credit
card debt.”\*

<font size="2">\*[RDocumentation](https://www.rdocumentation.org/packages/ISLR/versions/1.2/topics/Credit)</font>

# prologue

## set Data

First, let’s set the Environment

Libraries:

``` r
library(easypackages)
library(tidymodels)
library(tidyverse)
#collect data:
library(ISLR)
attach(Credit)
#random Forest
library(randomForest)
library(doParallel)
#other models
libraries('glmnet','Rcmdr','MASS')
#neural network
libraries('tensorflow','keras')
#install_tensorflow() #install_keras() #install_minicomda
#data tools:
libraries('dplyr','tidyr')
libraries('Matrix','scales')
#visual
libraries('hrbrthemes','viridis')
libraries('knitr','kableExtra','sjPlot')
```

Now we will see the data’s structure

6 top rows of our table:

``` r
head(Credit[,-1],6) %>%
  kbl() %>%
  kable_material(c("striped", "hover"))
```

<table class=" lightable-material lightable-striped lightable-hover" style="font-family: &quot;Source Sans Pro&quot;, helvetica, sans-serif; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
Income
</th>
<th style="text-align:right;">
Limit
</th>
<th style="text-align:right;">
Rating
</th>
<th style="text-align:right;">
Cards
</th>
<th style="text-align:right;">
Age
</th>
<th style="text-align:right;">
Education
</th>
<th style="text-align:left;">
Gender
</th>
<th style="text-align:left;">
Student
</th>
<th style="text-align:left;">
Married
</th>
<th style="text-align:left;">
Ethnicity
</th>
<th style="text-align:right;">
Balance
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
14.891
</td>
<td style="text-align:right;">
3606
</td>
<td style="text-align:right;">
283
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
Male
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Yes
</td>
<td style="text-align:left;">
Caucasian
</td>
<td style="text-align:right;">
333
</td>
</tr>
<tr>
<td style="text-align:right;">
106.025
</td>
<td style="text-align:right;">
6645
</td>
<td style="text-align:right;">
483
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
82
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
Female
</td>
<td style="text-align:left;">
Yes
</td>
<td style="text-align:left;">
Yes
</td>
<td style="text-align:left;">
Asian
</td>
<td style="text-align:right;">
903
</td>
</tr>
<tr>
<td style="text-align:right;">
104.593
</td>
<td style="text-align:right;">
7075
</td>
<td style="text-align:right;">
514
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
71
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
Male
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Asian
</td>
<td style="text-align:right;">
580
</td>
</tr>
<tr>
<td style="text-align:right;">
148.924
</td>
<td style="text-align:right;">
9504
</td>
<td style="text-align:right;">
681
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
Female
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Asian
</td>
<td style="text-align:right;">
964
</td>
</tr>
<tr>
<td style="text-align:right;">
55.882
</td>
<td style="text-align:right;">
4897
</td>
<td style="text-align:right;">
357
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
68
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:left;">
Male
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Yes
</td>
<td style="text-align:left;">
Caucasian
</td>
<td style="text-align:right;">
331
</td>
</tr>
<tr>
<td style="text-align:right;">
80.180
</td>
<td style="text-align:right;">
8047
</td>
<td style="text-align:right;">
569
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
Male
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
No
</td>
<td style="text-align:left;">
Caucasian
</td>
<td style="text-align:right;">
1151
</td>
</tr>
</tbody>
</table>

Here a histogram of he credit balance:

``` r
Credit %>%
  ggplot(aes(x=Balance))+
  geom_histogram(color= alpha("black", 0.7),fill= alpha("darkgreen", 0.8) )+
  geom_vline(xintercept= mean(Balance), color= "red",lty= "dashed")+
  theme_bw()+labs(title = "Balance Histogram")+ theme(plot.title = element_text(size=12,hjust = 0.5,face = "bold"))
```

![](Credit_my_deap_learnn_markdown_files/figure-gfm/histogram%20Balance-1.png)<!-- -->
As we can see, $Balance$ is not normal, and has a right tail.

Trying to predict the Balance, one cal ask about race and/or gender
bias. Does this factor alone predict the result? and age?

``` r
Credit %>%
  ggplot(aes(y=Balance, x=Age,fill= factor(Ethnicity) ))+
  geom_point(size= 0.7)+
  geom_smooth(method = "glm", level= 0.9, color= "black")+ylim(min(Balance), max(1500))+
  labs(title = "Balance GLM by Age & Ethnicity")+ theme(plot.title = element_text(size=12,hjust = 0.5,face = "bold"))
```

![](Credit_my_deap_learnn_markdown_files/figure-gfm/visual%20trend-1.png)<!-- -->

``` r
Credit %>%
  ggplot(aes(y=Balance, x=Ethnicity,fill= Ethnicity))+
  geom_boxplot(size= 0.7)+labs(x = "")
```

![](Credit_my_deap_learnn_markdown_files/figure-gfm/visual%20trend-2.png)<!-- -->

and what if we will predict by race & gender?

``` r
Credit %>%
  ggplot(aes(y=Balance,x= factor(Ethnicity),fill=factor(Gender) ))+
  geom_point(size= 0.7)+
  geom_boxplot(size= 0.7)+scale_fill_brewer(palette="Dark2")+
  labs(x= "", title = "Balance by Gender & Ethnicity")+theme(plot.title = element_text(size=12,hjust = 0.5,face = "bold"))
```

![](Credit_my_deap_learnn_markdown_files/figure-gfm/plot%20age%20race-1.png)<!-- -->
Again, there is no clear effect.

One can ask, can all of the weak predictors create a good one together?

So let’s start!

We will ad some variables.

1.  High_deg= $T$ if got more than 12 years of education
2.  Age_2= $Age^2$ in order to allaw parabolic age effect
3.  Bride= interaction of gender ans marriage

also, we will set seed and sample train & test.

``` r
Credit<- Credit%>%
  mutate(High_deg= Education>=13,
  Age_2= Age^2,
  Bride= (Gender== 'Female')&(Married== 'Yes'))
```

``` r
credit_split<- initial_split(Credit, prop = 0.8)
credit_train_split<- training(credit_split)
credit_test_split<-  testing(credit_split)

dolche_de_leche<- function(df) {
  recipe(data=  df, Balance~.) %>% 
  update_role(Balance, new_role = "outcome") %>%
  step_novel    (all_nominal(), -all_outcomes(),  new_level = "the_rest")%>% 
  step_unknown  (all_nominal(), -all_outcomes(),  new_level= "step_unknown" )%>%
  step_other    (all_nominal(), -all_outcomes(), other = 'step_other', threshold = 10)%>%
  step_nzv      (all_numeric(), -all_outcomes(),freq_cut = 99/1) %>% 
  step_normalize(all_numeric(), -all_outcomes())
  }

dolche_credit_train<-credit_train_split%>% dolche_de_leche()%>% prep(credit_train_split)%>% bake(credit_train_split) 
dolche_credit_test<- credit_train_split%>% dolche_de_leche()%>% prep(credit_train_split)%>% bake(credit_test_split)
```

Finally, we can predict with our models

# Models

**draft: predict if Credit is 0.**

This part is unfinished. here I create logistic prediction of Balance=0
in order to use in as another variable.

``` r
log_data<- dolche_credit_train %>% 
  mutate(Balance =  (Balance==0) %>% as.factor())

log_fit <- 
  logistic_reg(mode = "classification") %>%
  set_engine(engine = "glm") %>% 
  fit(Balance ~ ., data =log_data)

log_pred<- predict(log_fit, new_data = dolche_credit_test)

dolche_credit_train<- cbind(dolche_credit_train,log_data$Balance)
dolche_credit_test<- cbind(dolche_credit_test,log_pred )
```

## Linear

### Classic LM

Assumption: knowing X matrix, Y distributed normal:
$(Y|X) \sim N(BX,\sigma^2)$

``` r
lm_par <- linear_reg() %>% set_mode('regression') %>% 
  set_engine("lm")
lm_fit <- lm_par %>% fit(Balance ~ . , dolche_credit_train)
lm_pred<- predict(lm_fit, new_data = dolche_credit_test)
# abs(Credit$Balance[testid] - lmpred)

delta_lm<- lm_pred-dolche_credit_test$Balance
delta_lm %>% abs() %>% unlist() %>% mean(na.rm= T)
```

    ## [1] 85.0616

``` r
cbind(lm_pred,dolche_credit_test$Balance) %>%
  rename(pred= 1, true= 2) %>% 
  ggplot(aes(y=pred, x= true))+
  geom_point()+geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Credit_my_deap_learnn_markdown_files/figure-gfm/LM-1.png)<!-- -->

Now, what is our best predictors, if we filter our variables and prevent
over fitting?

### step wise method

Adding each time variable according to AIC method.

``` r
lmfit <- lm(Balance ~ ., data = credit_train_split)

null_model <- lm(Balance ~1, data =  credit_train_split) #null model for starting Stepwise
step_fit <- stepAIC (null_model,k= 2, direction = "forward",scope = list(lower= formula(null_model),upper= formula(lmfit) ) )
```

    ## Start:  AIC=3902.83
    ## Balance ~ 1
    ## 
    ##             Df Sum of Sq      RSS    AIC
    ## + Rating     1  45580094 17405260 3493.3
    ## + Limit      1  45115746 17869608 3501.7
    ## + Income     1  10706343 52279011 3845.2
    ## + Student    1   3917140 59068214 3884.3
    ## + Cards      1    470757 62514597 3902.4
    ## <none>                   62985354 3902.8
    ## + Bride      1    221571 62763784 3903.7
    ## + High_deg   1    210989 62774366 3903.8
    ## + Gender     1    201786 62783569 3903.8
    ## + Married    1    119889 62865466 3904.2
    ## + Age        1     73531 62911823 3904.5
    ## + Age_2      1     71247 62914107 3904.5
    ## + Education  1     37502 62947852 3904.6
    ## + ID         1      3455 62981899 3904.8
    ## + Ethnicity  2     85850 62899504 3906.4
    ## 
    ## Step:  AIC=3493.27
    ## Balance ~ Rating
    ## 
    ##             Df Sum of Sq      RSS    AIC
    ## + Income     1   9283193  8122067 3251.4
    ## + Student    1   4941183 12464078 3388.4
    ## + Age_2      1    670299 16734961 3482.7
    ## + Age        1    569607 16835653 3484.6
    ## + Cards      1    151139 17254121 3492.5
    ## + Married    1    113815 17291445 3493.2
    ## <none>                   17405260 3493.3
    ## + Gender     1    106808 17298453 3493.3
    ## + Limit      1     18721 17386539 3494.9
    ## + ID         1     12387 17392873 3495.0
    ## + Ethnicity  2    119407 17285853 3495.1
    ## + Bride      1      6010 17399250 3495.2
    ## + Education  1      2897 17402363 3495.2
    ## + High_deg   1      1051 17404210 3495.2
    ## 
    ## Step:  AIC=3251.37
    ## Balance ~ Rating + Income
    ## 
    ##             Df Sum of Sq     RSS    AIC
    ## + Student    1   4871925 3250142 2960.3
    ## + Married    1    109447 8012620 3249.0
    ## + Age_2      1     65190 8056877 3250.8
    ## + Age        1     58924 8063142 3251.0
    ## + Limit      1     51027 8071040 3251.4
    ## <none>                   8122067 3251.4
    ## + Gender     1     34248 8087819 3252.0
    ## + High_deg   1     33709 8088358 3252.0
    ## + Education  1     14262 8107805 3252.8
    ## + ID         1     11358 8110708 3252.9
    ## + Bride      1      2043 8120024 3253.3
    ## + Cards      1      1251 8120816 3253.3
    ## + Ethnicity  2     47982 8074085 3253.5
    ## 
    ## Step:  AIC=2960.28
    ## Balance ~ Rating + Income + Student
    ## 
    ##             Df Sum of Sq     RSS    AIC
    ## + Limit      1    151670 3098472 2947.0
    ## + Age_2      1     24313 3225829 2959.9
    ## + Age        1     23860 3226283 2959.9
    ## <none>                   3250142 2960.3
    ## + Married    1     12173 3237970 2961.1
    ## + Cards      1      7480 3242662 2961.6
    ## + High_deg   1      5024 3245119 2961.8
    ## + ID         1       106 3250036 2962.3
    ## + Gender     1        66 3250077 2962.3
    ## + Education  1        46 3250096 2962.3
    ## + Bride      1        19 3250123 2962.3
    ## + Ethnicity  2     14907 3235236 2962.8
    ## 
    ## Step:  AIC=2946.99
    ## Balance ~ Rating + Income + Student + Limit
    ## 
    ##             Df Sum of Sq     RSS    AIC
    ## + Cards      1    125019 2973453 2935.8
    ## + Age_2      1     21947 3076525 2946.7
    ## + Age        1     21020 3077452 2946.8
    ## <none>                   3098472 2947.0
    ## + Married    1      7147 3091325 2948.2
    ## + High_deg   1      1553 3096919 2948.8
    ## + Education  1      1430 3097042 2948.8
    ## + Gender     1       529 3097943 2948.9
    ## + ID         1        97 3098375 2949.0
    ## + Bride      1        54 3098418 2949.0
    ## + Ethnicity  2     11272 3087200 2949.8
    ## 
    ## Step:  AIC=2935.81
    ## Balance ~ Rating + Income + Student + Limit + Cards
    ## 
    ##             Df Sum of Sq     RSS    AIC
    ## + Age        1   23449.6 2950004 2935.3
    ## + Age_2      1   23122.8 2950331 2935.3
    ## <none>                   2973453 2935.8
    ## + Married    1    5251.9 2968201 2937.2
    ## + High_deg   1    1632.8 2971821 2937.6
    ## + ID         1    1556.2 2971897 2937.6
    ## + Education  1    1505.8 2971948 2937.7
    ## + Gender     1     394.0 2973059 2937.8
    ## + Bride      1      79.8 2973374 2937.8
    ## + Ethnicity  2    7789.1 2965664 2939.0
    ## 
    ## Step:  AIC=2935.28
    ## Balance ~ Rating + Income + Student + Limit + Cards + Age
    ## 
    ##             Df Sum of Sq     RSS    AIC
    ## <none>                   2950004 2935.3
    ## + Married    1    7522.4 2942481 2936.5
    ## + ID         1    2318.9 2947685 2937.0
    ## + High_deg   1    1553.5 2948450 2937.1
    ## + Education  1    1485.7 2948518 2937.1
    ## + Gender     1     394.2 2949610 2937.2
    ## + Age_2      1      23.5 2949980 2937.3
    ## + Bride      1       0.2 2950003 2937.3
    ## + Ethnicity  2    6588.7 2943415 2938.6

``` r
step_pred <- predict(step_fit , credit_test_split)
err_step<- abs(credit_test_split$Balance - step_pred)
mean(abs(err_step))
```

    ## [1] 84.84623

the chosen model is
$$Balance ~ Rating + Income + Student + Limit + Cards$$ and we get sd of
80.94

lets see the regression vs the step wise. it is clear that the AIC
method choose only the variables with P-value \< 5 %

``` r
tab_model(lmfit,step_fit, show.ci= F,show.se = T,show.loglik= T)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
Balance
</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
Balance
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Estimates
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
std. Error
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Estimates
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
std. Error
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">
p
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-455.85
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
70.46
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-500.97
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
27.30
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
ID
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.02
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.05
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.670
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Income
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-7.95
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.27
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-7.94
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.26
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Limit
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.19
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.19
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Rating
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.21
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.54
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.026</strong>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.15
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.53
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
<strong>0.032</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Cards
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
17.35
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
4.75
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
17.17
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
4.67
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Age
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.69
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.14
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.746
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.51
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.32
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
0.116
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Education
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-3.92
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
3.19
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.220
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Gender \[Female\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-18.67
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
18.29
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.308
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Student \[Yes\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
425.14
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
18.93
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
426.72
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
18.48
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Married \[Yes\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-22.85
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
16.36
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.163
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Ethnicity \[Asian\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
11.03
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
15.76
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.485
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Ethnicity \[Caucasian\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
5.71
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
13.62
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.675
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
High degTRUE
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
24.26
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
21.06
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.250
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Age 2
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.00
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.02
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.934
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
BrideTRUE
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
26.05
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
23.21
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.262
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">
320
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">
320
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
R<sup>2</sup> / R<sup>2</sup> adjusted
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.954 / 0.952
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.953 / 0.952
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
log-Likelihood
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
-1912.257
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
-1914.700
</td>
</tr>
</table>

### lasso

Assumption

$(Y|X) \sim N(BX,\sigma^2)$ , like LM.

But this time we use shrinkage method in order to reduce variance & over
fitting. so our minimizing function define as

$RSS+ \lambda {\Sigma}_{j=1}^p |\beta_j|$

when $p=length( \beta)$ and $\lambda$ is a hyper parameter.

This time, we need to set our hipper parameter, $\lambda$ that lead to
the minimum mean cross-validated error\*

<font size="2"> \*[see
also](https://cran.r-project.org/web/packages/glmnet/vignettes/glmnet.pdf)
</font>

``` r
lasso_par <- linear_reg(mixture = 1,penalty = 0.01) %>% set_mode('regression') %>% 
  set_engine("glmnet")
lasso_fit <- lm_par %>% fit(Balance ~ . , dolche_credit_train)
lasso_pred<- predict(lasso_fit, new_data = dolche_credit_test)
# abs(Credit$Balance[testid] - lmpred)

delta_lasso<- lasso_pred-dolche_credit_test$Balance
delta_lasso %>% abs() %>% unlist() %>% mean(na.rm= T)
```

    ## [1] 85.0616

``` r
cbind(lasso_pred,dolche_credit_test$Balance) %>%
  rename(pred= 1, true= 2) %>% 
  ggplot(aes(y=pred, x= true))+
  geom_point()+geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Credit_my_deap_learnn_markdown_files/figure-gfm/LM%20Lasso-1.png)<!-- -->

## Trees

The main algorithm in random forest, adaboost, etc is splitting the data
each time into two samples, in the most effective way by reevaluating
the error function.

Mathematically, the tree assume a model of form

$f(x)= \sum^M_{m=1} c_m*I(x \in R_m) +\epsilon$

while $M$ is the numbers of groups, $R_m$ is the specific group & $c_m$
is the parameter of the model.

For a small tree,this is a very weak learner, but it can be used to
create deeper learning. A complicated tree can lead to over-feeting.

### Random Forest

A mean of n-tree

``` r
rf_s<- rand_forest(mode = "regression", trees = 1500, min_n = 5)%>%
  set_mode("regression") %>% set_engine("randomForest")

rf_fit<- rf_s %>% fit(Balance ~ . , dolche_credit_train)
rf_pred <- predict(rf_fit, new_data = dolche_credit_test)

delta_rf<- rf_pred-dolche_credit_test$Balance
delta_rf %>% abs() %>% unlist() %>% mean(na.rm= T)

cbind(rf_pred,dolche_credit_test$Balance) %>%
  rename(pred= 1, true= 2) %>% 
  ggplot(aes(y=pred, x= true))+
  geom_point()+geom_smooth()
```

------------------------------------------------------------------------

``` r
rf_spec <- rand_forest(mode = "regression",
  trees = 800,
  min_n = tune(), mtry = tune() ) %>% 
  set_engine("randomForest") %>% set_mode("regression")
rf_spec
```

    ## Random Forest Model Specification (regression)
    ## 
    ## Main Arguments:
    ##   mtry = tune()
    ##   trees = 800
    ##   min_n = tune()
    ## 
    ## Computational engine: randomForest

``` r
rf_grid <- grid_latin_hypercube(
  min_n(),
  finalize(mtry(), dolche_credit_train),
  size = 6)
rf_grid
```

    ## # A tibble: 6 x 2
    ##   min_n  mtry
    ##   <int> <int>
    ## 1    39    10
    ## 2    28    14
    ## 3     8     4
    ## 4    17     3
    ## 5    13     7
    ## 6    25    10

``` r
rf_wf <- workflow() %>%
  add_formula(Balance ~ .) %>%
  add_model(rf_spec)

vb_folds_rf <- vfold_cv(dolche_credit_train, strata = Balance, v= 5)
```

``` r
set.seed(234)
rf_res <- tune_grid(
  rf_wf,
  resamples = vb_folds_rf,
  grid = rf_grid,
  control = control_grid(save_pred = TRUE) )
```

``` r
rf_res %>% #???
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  #select(mean, mtry:tree_depth) %>%
  pivot_longer(mtry:min_n,
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")+theme_linedraw()
```

![](Credit_my_deap_learnn_markdown_files/figure-gfm/randomForest%20to%20tune%20res-1.png)<!-- -->

``` r
ggsave("photo_graph/myplot_rf.png")

show_best(rf_res, "rmse")# %>% select(-.estimator,-n,-.metric)
```

    ## # A tibble: 5 x 8
    ##    mtry min_n .metric .estimator  mean     n std_err .config             
    ##   <int> <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>               
    ## 1    14    28 rmse    standard    148.     5    7.23 Preprocessor1_Model2
    ## 2     7    13 rmse    standard    149.     5    5.49 Preprocessor1_Model5
    ## 3    10    25 rmse    standard    152.     5    5.90 Preprocessor1_Model6
    ## 4    10    39 rmse    standard    165.     5    5.88 Preprocessor1_Model1
    ## 5     4     8 rmse    standard    172.     5    5.29 Preprocessor1_Model3

``` r
best_tune_rf <- select_best(rf_res, "rmse")
write_csv(best_tune_rf,"data/best_tune_rf.csv") #in any case, I save them to cut reproducing
```

![](myplot.png)

``` r
best_tune_rf<- read_csv("data/best_tune_rf.csv")
```

    ## Rows: 1 Columns: 3
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (1): .config
    ## dbl (2): mtry, min_n
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
YG_tuned_boost <- rand_forest(mode = "regression", trees = 1500, min_n = best_tune_rf$min_n, mtry =best_tune_rf$mtry,
                           )%>%
  set_mode("regression") %>% set_engine("randomForest")

mod_boost_final<- YG_tuned_boost %>% fit(Balance~ ., data= dolche_credit_train)
rf_pred<-   mod_boost_final %>% predict(new_data= dolche_credit_test)%>% as.data.frame()

delta_rf<- rf_pred-dolche_credit_test$Balance
delta_rf %>% abs() %>% unlist() %>% mean(na.rm= T)
```

    ## [1] 85.98553

``` r
cbind(rf_pred,dolche_credit_test$Balance) %>%
  rename(pred= 1, true= 2) %>% 
  ggplot(aes(y=pred, x= true))+
  geom_point()+geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Credit_my_deap_learnn_markdown_files/figure-gfm/print%20fit%20randomForest-1.png)<!-- -->
\_\_\_

### XGBoost

Algorithm of gradient boosting trees

``` r
xgb_s<-boost_tree(mode = "regression",
                    trees= 1500, min_n= 5) %>%
  set_engine('xgboost') %>% set_mode("regression")
  
xgb_fit<- xgb_s %>% fit(Balance ~ . , dolche_credit_train)
#rf_pred <- predict(rf_fit, new_data = dolche_credit_test)

pred_xgb<-  predict(xgb_fit, new_data = dolche_credit_test)
err_xgb<- pred_xgb-dolche_credit_test$Balance
err_xgb %>% abs() %>% unlist() %>% mean(na.rm= T)


cbind(pred_xgb,dolche_credit_test$Balance) %>%
  rename(pred= 1, true= 2) %>% 
  ggplot(aes(y=pred, x= true))+
  geom_point()+geom_smooth()
```

------------------------------------------------------------------------

``` r
xgb_spec <- boost_tree(mode = "regression",
  trees = 800, tree_depth = tune(),sample_size = 0.4,
  min_n = tune(), mtry = tune() ) %>% 
  set_engine("xgboost") %>% set_mode("regression")
xgb_spec
```

    ## Boosted Tree Model Specification (regression)
    ## 
    ## Main Arguments:
    ##   mtry = tune()
    ##   trees = 800
    ##   min_n = tune()
    ##   tree_depth = tune()
    ##   sample_size = 0.4
    ## 
    ## Computational engine: xgboost

``` r
xgb_grid <- grid_latin_hypercube(
  min_n(),tree_depth(),
  finalize(mtry(), dolche_credit_train),
  size = 6)
xgb_grid
```

    ## # A tibble: 6 x 3
    ##   min_n tree_depth  mtry
    ##   <int>      <int> <int>
    ## 1    20          9     8
    ## 2    22         11     4
    ## 3    14         13     2
    ## 4     4          6    13
    ## 5    34          6     9
    ## 6    33          2    11

``` r
xgb_wf <- workflow() %>%
  add_formula(Balance ~ .) %>%
  add_model(xgb_spec)

vb_folds_xgb <- vfold_cv(dolche_credit_train, strata = Balance, v= 5)
```

``` r
set.seed(234)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = vb_folds_xgb,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE) )
```

    ## Warning: package 'xgboost' was built under R version 4.1.3

``` r
#abc<- 
xgb_res %>% #???
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  #select(mean, mtry:tree_depth) %>%
  pivot_longer(mtry:tree_depth,
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")+theme_linedraw()
```

![](Credit_my_deap_learnn_markdown_files/figure-gfm/xgboost%20to%20tune%20res-1.png)<!-- -->

``` r
ggsave("photo_graph/myplot_xgb.png")

show_best(xgb_res, "rmse")# %>% select(-.estimator,-n,-.metric)
```

    ## # A tibble: 5 x 9
    ##    mtry min_n tree_depth .metric .estimator  mean     n std_err .config         
    ##   <int> <int>      <int> <chr>   <chr>      <dbl> <int>   <dbl> <chr>           
    ## 1    13     4          6 rmse    standard    129.     5   13.4  Preprocessor1_M~
    ## 2     2    14         13 rmse    standard    195.     5   14.0  Preprocessor1_M~
    ## 3     8    20          9 rmse    standard    225.     5    8.18 Preprocessor1_M~
    ## 4     4    22         11 rmse    standard    237.     5    4.23 Preprocessor1_M~
    ## 5    11    33          2 rmse    standard    240.     5   11.2  Preprocessor1_M~

``` r
best_tune_XGB <- select_best(xgb_res, "rmse")
write_csv(best_tune_XGB,"data/best_tune_XGB.csv") #in any case, I save them to cut reproducing
```

![](myplot.png)

``` r
best_tune_XGB<- read_csv("data/best_tune_XGB.csv")
```

    ## Rows: 1 Columns: 4
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (1): .config
    ## dbl (3): mtry, min_n, tree_depth
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
YG_tuned_boost <- boost_tree(mode = "regression", trees = 1500, min_n = best_tune_XGB$min_n, mtry =best_tune_XGB$mtry,
                           tree_depth= best_tune_XGB$tree_depth )%>%
  set_mode("regression") %>% set_engine("xgboost")

mod_boost_final<- YG_tuned_boost %>% fit(Balance~ ., data= dolche_credit_train)
pred_xgb<-   mod_boost_final %>% predict(new_data= dolche_credit_test)%>% as.data.frame()
err_xgb<- pred_xgb-dolche_credit_test$Balance
err_xgb %>% abs() %>% unlist() %>% mean(na.rm= T)
```

    ## [1] 56.34975

``` r
cbind(pred_xgb,dolche_credit_test$Balance) %>%
  rename(pred= 1, true= 2) %>% 
  ggplot(aes(y=pred, x= true))+
  geom_point()+geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Credit_my_deap_learnn_markdown_files/figure-gfm/print%20fit%20XGBoost-1.png)<!-- -->
\_\_\_

``` r
norm_y<- 2*y/(max(y)+min(y))-1

adb_s<-randomForest(formula= Balance ~ ., data = Credit[-testid , ],
                    ntree= 1500, mtry= 14, na.action = na.omit, importance= T )

pred_abs <- predict(adb_s , Credit[testid , ], s = "lambda.min")
pred_abs<- head(as.vector(pred_abs),133)
err_adb<- abs(y[testid] - pred_abs)
mean(abs(err_adb))
```

## Neural Network

Creating of network of nonlinear function and weights, that evaluate the
prediction.

This method is the hardest to present due to the complexitivity of the
net.

### set seed

we set our net using 2 layers of relu and then a dropout

``` r
credit_nn_split<- initial_split(dolche_credit_train, prop = 0.8)
credit_nn_train<- training(credit_nn_split)
credit_nn_test<-  testing(credit_nn_split)

x <- model.matrix(Balance ~ . - 1, data = credit_nn_train)
x_test<- model.matrix(Balance ~ . - 1, data = credit_nn_test)
y <- credit_nn_train$Balance
y_test<- credit_nn_test$Balance

modnn <- keras_model_sequential () %>%
  layer_dense(units = 50, activation = "relu",
              input_shape = ncol(x)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 20, activation = 'relu') %>%
  layer_dense(units = 4, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = 'relu')
```

    ## Loaded Tensorflow version 2.9.1

``` r
modnn %>% compile(loss = "mse",
                  optimizer = optimizer_rmsprop (),
                  metrics = list("mean_absolute_error")
                  )
modnn
```

    ## Model: "sequential"
    ## ________________________________________________________________________________
    ##  Layer (type)                       Output Shape                    Param #     
    ## ================================================================================
    ##  dense_3 (Dense)                    (None, 50)                      1050        
    ##  dropout_1 (Dropout)                (None, 50)                      0           
    ##  dense_2 (Dense)                    (None, 20)                      1020        
    ##  dense_1 (Dense)                    (None, 4)                       84          
    ##  dropout (Dropout)                  (None, 4)                       0           
    ##  dense (Dense)                      (None, 1)                       5           
    ## ================================================================================
    ## Total params: 2,159
    ## Trainable params: 2,159
    ## Non-trainable params: 0
    ## ________________________________________________________________________________

Using the net:

``` r
mod_Credit <- modnn %>% fit(
  x, y, epochs = 1500, batch_size = 32,
  validation_data = list(x_test, y_test))
```

### Keras result

``` r
mod_Credit
```

    ## 
    ## Final epoch (plot to see history):
    ##                    loss: 416,868
    ##     mean_absolute_error: 484.1
    ##                val_loss: 417,168
    ## val_mean_absolute_error: 479.6

``` r
plot(mod_Credit)+theme_gray()+
  theme(plot.caption = element_text(size = 6,hjust= 0),
        legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right")#,
```

![](Credit_my_deap_learnn_markdown_files/figure-gfm/Keras%20result-1.png)<!-- -->

``` r
    #legend.margin = margin(6, 6, 6, 6))

dolche_credit_test_nn<- model.matrix(Balance ~ . - 1, data = dolche_credit_test)
y <- dolche_credit_test$Balance

nnpred <- predict(modnn ,dolche_credit_test_nn ) %>% as.data.frame()
err_nn<- abs(y_test - nnpred)
err_nn %>% unlist() %>% mean(na.rm= T)
```

    ## [1] 484.1669

## Sum all result

We used the same seed to test all methods, so now we can compare the
error of each data.

-   Each time this scipt were running, we got different result, due to
    randomness of $testid$ , and of the deep learning models

``` r
modl_nam<-c('Balance', "Linear", "Lasso", "Random_forrest","xgboost","Neural_network")
order_script<- order(modl_nam[-1])+1

my_pred<- data.frame(cbind(dolche_credit_test$Balance,lm_pred,lasso_pred,rf_pred,pred_xgb,nnpred)) %>%  #pred data frame
   `colnames<-`(modl_nam) %>% 
  pivot_longer(cols = 2:6, names_to = "Model")

colnames(my_pred)[3]<- "Predict"

my_err<- my_pred %>% mutate(Delta= Predict- Balance) %>% 
  group_by(Model) %>% summarise_at(2, mean)

#caption- var of the models alphabetically

script_base<- map2_chr(my_err$Model, my_err$Predict, function(x,y) {paste0(x, " is ", round(y,3) )})
script<- "SD: "
for (i in script_base) {
  script<- paste(script, ",", i)}
script<- str_remove(script, ", ")

my_pred %>%
  ggplot( aes(x= Model ,y=Balance-Predict, fill= Model)) +
  geom_violin()+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.45) +
  theme_dark() +
  theme(
    legend.position="none",
    plot.title = element_text(size=12,hjust = 0.5,face = "bold"),
    plot.caption = element_text(size = 8,hjust= 0),
    axis.title = element_text(size = 8))+ ylab("Error")+
  labs(title = "Error Violin",caption = script)
```

![](Credit_my_deap_learnn_markdown_files/figure-gfm/sum%20pred-1.png)<!-- -->

``` r
my_err %>% 
  ggplot(aes(x= reorder(Model, -Predict), y= Predict, fill= Predict))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = round(Predict,2))  , vjust = +1.2, color= "White")
```

![](Credit_my_deap_learnn_markdown_files/figure-gfm/barplot-1.png)<!-- -->

# Discussion

If the winner of the error in prediction were not that clear, our $H_0$
would will be the linear model, since it’s prediction is the easiest to
understand and present.

Amazingly, <b>the LM did nothing compare to the NN</b>, with only almost
quarter of the linear’s error.

At the same time, Lasso(var 81) did only slightly better then the liner,
both better than the ADB(var 85).

Another clear effect is this of the ADB comparing to the RF(var 101),
which had the worse prediction variance. The effect if weights is the
main advantage of ADB over RF.

To sum it up, though rerunning of this script might create a different
result, this modeling comparing to data frame prediction show us how
<b>using weights or regulation in models can get better models comparing
to the same model. In contrast, some time different models create worse
prediction than the unregulated ones</b>, like RF and LM. The most
complex model, NN, overcome all models, and known to have huge
potential, as long as understanding the effect of each variable is not
needed.

.
