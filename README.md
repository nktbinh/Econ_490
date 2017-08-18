# Predicting Problem Drinking using Socio-Economic Factors

Based on the most recent available public data from the National Survey of Drug Use and Health (http://www.icpsr.umich.edu/icpsrweb/DSDR/studies/36361). This is a relatively large data set with 3,148 variables and 55,271 observations. I did the data cleaning manually to shift the focus to a few outcomes and predictors of interest.

This is my second attempt at understanding and analyzing the causal/inferencial relationship between problem drinking and socio-economic factors given in the data set. My first attempt was during the course of my final Economic Seminar at the University of British Columbia, which resulted in a final paper titled "Studying Differences in Outcomes in Response to Different Indicators of Alcoholism for Different Gender Groups". The main analysis of the paper was done using STATA (I also included the .do file for your reference). This was my first exposure to statistical analysis so I have to admit I did not know what I was doing for the most part. In additions, the data poses a significant challenge due to its massive size and the number of different measurement of core substances modules and their treatments, as well as other important indicators of mental illnesses.

This project is an ongoing process. Current version of the report can be found [here](http://rpubs.com/nktbinh214/299780).

Future additions:

- _Random Forest_ and _Bagging_ under **Tree-Based** Methods

- _Imputation_ using **MLR/mice** packages

- _Boosting_ using **xgboost/caret** packages

