# Credit Balance Prediction Analysis


## Table of Contents

- [Overview](#overview)
- [Setup](#setup)
- [Result](#result)
- [Project Contents](#project-contents)
- [Graphs](#graphs)

## Overview
This repository contains a comprehensive analysis of credit balance prediction using multiple statistical methods and machine learning approaches.

The project explores the efficacy of different modeling techniques in predicting credit card balances, addressing an important question in financial analytics.

## Setup

#### Using multiple models to evaluate Credit balance

The goal of this analysis is to test different approaches to predict numeric prediction with data frame.

I used some methods I was taught the University, especially from the book [An Introduction to Statistical Learning](https://www.statlearning.com/)

The data is Credit from the [ILSR package](https://www.rdocumentation.org/packages/ISLR/versions/1.2/topics/Credit)

This project will show which of the models are better: Those who are based on linear regression,
or those who use randomness and deep learning.

#### The models

* Linear- Regression

* Lasso- Regulated linear regression

* Random Forrest- Average of multiple trees

* xgboost- Algorithm of gradient boosting trees

* Neural network- machine learning using multiple layers


#### Tools & Technologies Used

* R for data analysis and modeling

* tidyverse for data manipulation

* tidymodels for machine learning workflows

* Keras/TensorFlow for neural network implementation

* Various statistical packages (glmnet, randomForest, etc.)

## Result

### Key Findings

* Neural Network models significantly outperformed traditional approaches for this dataset

* Feature importance varied substantially between different modeling techniques

* Testing of more complicated models not necessarily give better result than classic regression in predictive performance

### Future Work

Potential extensions of this project could include

* Ensemble modeling approaches

* More extensive hyperparameter optimization

* Application of more advanced deep learning architectures

## Project Contents

👓 Minimal code presentation- [present me](https://github.com/YoniGR94/my_credit_model_selection/blob/main/presentme.md)

👓 Recommended- see html presentation- [RMarkdown](https://github.com/YoniGR94/my_credit_model_selection/blob/main/Credit_my_deap_learnn_markdown.md)

👓 Read code- [RMarkdown script](https://github.com/YoniGR94/my_credit_model_selection/blob/main/Credit_my_deap_learnn_markdown.Rmd)

👓 Copy plots and png- [figure gfm](https://github.com/YoniGR94/my_credit_model_selection/tree/main/Credit_my_deap_learnn_markdown_files/figure-gfm)

### Graphs

The main result will be discussed,
and also those graph which show the evaluation of each model

Error distribution by model:

![](https://github.com/YoniGR94/my_credit_model_selection/blob/main/Credit_my_deap_learnn_markdown_files/figure-gfm/sum%20pred-1.png?raw=true)

Average error by model:

![](https://github.com/YoniGR94/my_credit_model_selection/blob/main/Credit_my_deap_learnn_markdown_files/figure-gfm/barplot-1.png?raw=true)

![](https://img.shields.io/github/commit-activity/m/YoniGR94/my_credit_model_selection?label=commited&logo=git&style=plastic)
