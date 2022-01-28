Different Techniches to Analyze Credit Balance
================

how good can we predict Credit balance?

In order to race our models, we will use the Credit data from ILSR
package.

About this data:

“A simulated data set containing information on ten thousand customers.
The aim here is to predict which customers will default on their credit
card debt.”\*

\*[RDocumentation](https://www.rdocumentation.org/packages/ISLR/versions/1.2/topics/Credit)

# set Data

first, let’s set the Environment

``` r
#collect data:
library(ISLR)
attach(Credit)

#neural network
library(tensorflow)
#install_tensorflow()
library(keras)
#install_keras()
#install_minicomda

#data tools:
library(dplyr)
library(tidyr)
library(Matrix)

#visual
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(knitr)
library(kableExtra)
library(scales)
library(jtools)

#random Forest
library(randomForest)

#other models
library(glmnet)
```

now we will see the data’s structure

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

``` r
Credit %>%
  ggplot(aes(x=Balance))+
  geom_histogram(color= alpha("black", 0.7),fill= "green" )+
  geom_vline(xintercept= mean(Balance), color= "blue",lty= "dashed")+
  theme_linedraw()+labs( x= "Balance in $10,000")
```

![](Credit_my_deap_learnn_markdown_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->
As we can see, *B**a**l**a**n**c**e* is not normal, due to a dense 0
value

``` r
Credit %>%
  ggplot(aes(y=Balance, x=Age, ))+
  geom_point(y=Balance, x=Age, size= 0.7)+#,fill= Ethnicity)
  geom_smooth(method = "glm")+ylim(min(Balance), max(Balance))
```

![](Credit_my_deap_learnn_markdown_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
Credit %>%
  ggplot(aes(y=Balance, x=Ethnicity,fill= Ethnicity))+
  geom_boxplot(size= 0.7)+labs(x = "")
```

![](Credit_my_deap_learnn_markdown_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

we will ad some variables

``` r
#set data to Train & Test
Credit <- na.omit(Credit)
n <- nrow(Credit)
set.seed (13)
ntest <- trunc(n / 3)
testid <- sample (1:n, ntest)

#manipulate data:
Credit<- Credit %>%
  #mutate(Married= (ifelse(Married== "Yes" ,T,F)))%>%
  mutate(High_deg= Education>=13)%>%
  mutate(Age_2= Age^2)%>%
  mutate(Bride= (Gender== 'Female')&(Married== 'Yes'))%>%
  select(-ID)
```

# Models

# Linear

# LM

assumption

(*Y*\|*X*) ∼ *N*(*B**X*,*ϵ*)

``` r
lmfit <- lm(Balance ~ ., data = Credit[-testid , ])
lmpred <- predict(lmfit , Credit[testid , ])
err_lm<- (abs(Credit$Balance[testid] - lmpred))
mean(abs(err_lm))
```

    ## [1] 81.98279

``` r
#summary(lmfit)
export_summs(lmfit)
```

    ## Registered S3 methods overwritten by 'broom':
    ##   method            from  
    ##   tidy.glht         jtools
    ##   tidy.summary.glht jtools

               ─────────────────────────────────────────────────
                                                Model 1         
                                       ─────────────────────────
                 (Intercept)                       -384.03 ***  
                                                    (78.39)     
                 Income                              -7.55 ***  
                                                     (0.30)     
                 Limit                                0.17 ***  
                                                     (0.04)     
                 Rating                               1.40 *    
                                                     (0.60)     
                 Cards                               17.55 **   
                                                     (5.50)     
                 Age                                 -3.52      
                                                     (2.58)     
                 Education                           -4.55      
                                                     (3.61)     
                 GenderFemale                        -4.60      
                                                    (20.49)     
                 StudentYes                         421.42 ***  
                                                    (21.75)     
                 MarriedYes                          -3.29      
                                                    (18.24)     
                 EthnicityAsian                       7.39      
                                                    (17.56)     
                 EthnicityCaucasian                  11.48      
                                                    (15.01)     
                 High_degTRUE                        23.35      
                                                    (23.50)     
                 Age_2                                0.03      
                                                     (0.02)     
                 BrideTRUE                          -10.57      
                                                    (25.99)     
                                       ─────────────────────────
                 N                                  267         
                 R2                                   0.95      
               ─────────────────────────────────────────────────
                 *** p < 0.001; ** p < 0.01; * p < 0.05.        

Column names: names, Model 1

# lasso

\*(use vectors)

assumption

(*Y*\|*X*) ∼ *N*(*B**X*,*ϵ*), like LM.

but this time we use shrinkage method in order to reduce variance & over
fitting. so our minimizing function define as

$RSS+ \\lambda {\\Si3gma}\_{j=1}^p \|\\beta_j\|$

when *p* = *l**e**n**g**t**h*(*β*) and *λ* is a hyper parameter.

``` r
x <- scale(model.matrix(Balance ~ . - 1, data = Credit))
y <- Credit$Balance

#glmnet
cvfit <- cv.glmnet(x[-testid , ], y[-testid],
                     type.measure = "mae")
cpred <- predict(cvfit , x[testid , ], s = "lambda.min")
err_cp<- abs(y[testid] - cpred)
mean(abs(err_cp))
```

    ## [1] 81.0397

# Trees

The main algorithm in randomforest, adaboost, etc is splitting the data
each time into two samples, in the most effective way by reevaluating
the error function. This is a very weak learner, but it can be used to
create deeper learning.

# Random Forest

A mean of n-tree

``` r
rf_s<- randomForest(formula= Balance ~ ., data = Credit[-testid , ],
                    ntree= 1500, mtry= 6, na.action = na.omit )

pred_s<- predict(rf_s,newdata = Credit[-testid , ] )
rf_pred <- predict(rf_s , Credit[testid , ], s = "lambda.min")
err_rf<- y[testid] - rf_pred
mean(abs(err_rf))
```

    ## [1] 101.1024

# Adaboost

A mean of n-tree with weights

``` r
norm_y<- 2*y/(max(y)+min(y))-1

adb_s<-randomForest(formula= Balance ~ ., data = Credit[-testid , ],
                    ntree= 1500, mtry= 14, na.action = na.omit, importance= T )
```

    ## Warning in randomForest.default(m, y, ...): invalid mtry: reset to within valid
    ## range

``` r
pred_abs <- predict(adb_s , Credit[testid , ], s = "lambda.min")
pred_abs<- head(as.vector(pred_abs),133)
err_adb<- y[testid] - pred_abs
mean(abs(err_adb))
```

    ## [1] 85.32504

# Neural Network

Creating of network of nonlinear function and weights, that evaluate the
prediction.

this method is the hardest to present due to the coplexativity of the
net.

# set seed

we set our net using 50 nodes of relu, dropout and 20 nodes of relu.

``` r
modnn <- keras_model_sequential () %>%
  layer_dense(units = 50, activation = "relu",
              input_shape = ncol(x)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 20, activation = 'relu') %>%
  #layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1)
```

    ## Loaded Tensorflow version 2.7.0

``` r
modnn %>% compile(loss = "mse",
                  optimizer = optimizer_rmsprop (),
                  metrics = list("mean_absolute_error")
                  )
modnn
```

    ## Model
    ## Model: "sequential"
    ## ________________________________________________________________________________
    ##  Layer (type)                       Output Shape                    Param #     
    ## ================================================================================
    ##  dense_2 (Dense)                    (None, 50)                      800         
    ##                                                                                 
    ##  dropout (Dropout)                  (None, 50)                      0           
    ##                                                                                 
    ##  dense_1 (Dense)                    (None, 20)                      1020        
    ##                                                                                 
    ##  dense (Dense)                      (None, 1)                       21          
    ##                                                                                 
    ## ================================================================================
    ## Total params: 1,841
    ## Trainable params: 1,841
    ## Non-trainable params: 0
    ## ________________________________________________________________________________

Using the net:

``` r
mod_Credit <- modnn %>% fit(
  x[-testid , ], y[-testid], epochs = 1500, batch_size = 32,
  validation_data = list(x[testid , ], y[testid ])
)
```

Keras result

``` r
#show Keras result
summary(mod_Credit)
```

    ##         Length Class  Mode
    ## params  3      -none- list
    ## metrics 4      -none- list

``` r
plot(mod_Credit)+theme_gray()+
  theme(plot.caption = element_text(size = 6,hjust= 0),
        legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right")#,
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Credit_my_deap_learnn_markdown_files/figure-gfm/Keras%20result-1.png)<!-- -->

``` r
    #legend.margin = margin(6, 6, 6, 6))

nnpred <- predict(modnn , x[testid , ])
err_nn<- y[testid] - nnpred
mean(abs(err_nn))
```

    ## [1] 16.73089

# sum result

we used the same seed to test all methods, so now we can compare the
error of each data

``` r
#data of all prediction
my_pred<- data.frame(c(lmpred,cpred,rf_pred,pred_abs,nnpred))
m<- length(testid)
modl_nam<-c("Linear", "Lasso", "Random_forrest","Adaboost","Neural_network")
my_pred$Model<- c(rep(modl_nam[1],m),rep(modl_nam[2],m),
                   rep(modl_nam[3],m),rep(modl_nam[4],m),rep(modl_nam[5],m) )
my_pred$Balance<-rep(y[testid],5)
colnames(my_pred)[1]<- "Predict"

all_sd<- c(err_lm,err_cp,err_rf,err_adb,err_nn)
#caption
order_script<- order(modl_nam)
script<- paste("sd of models: ",modl_nam[order_script[1]], "is", round(all_sd[order_script[1]],3))
for (i in order_script[-1]) {
  script<- paste(script, ", ", modl_nam[i], "is", round(all_sd[i],3) )}

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
