Diabetes Risk Assessment Models
================
Coots.
January 10, 2023

``` r
data <- readRDS("/Users/madisoncoots/Documents/harvard/research/race-diabetes/data/data.rds")
```

## Aggarwal et al. (2022) model

``` r
aggarwal_model_formula <- diabetes ~ race + ridageyr + bmxbmi +
  I(bmxbmi^2) + race:ridageyr + race:bmxbmi +
  race:I(bmxbmi^2) + ridageyr:bmxbmi + race:ridageyr:bmxbmi

aggarwal_model <- glm(aggarwal_model_formula, 
                         data = data, 
                         family = "binomial",
                         weights = round(wtmec8yr/1000))

summary(aggarwal_model)
```

    ## 
    ## Call:
    ## glm(formula = aggarwal_model_formula, family = "binomial", data = data, 
    ##     weights = round(wtmec8yr/1000))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -8.9980  -1.4636  -0.9059  -0.4061  23.1657  
    ## 
    ## Coefficients:
    ##                                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                           -1.291e+01  3.749e-01 -34.428  < 2e-16
    ## raceAsian American                    -5.997e+00  1.359e+00  -4.414 1.01e-05
    ## raceBlack American                     1.911e+00  7.291e-01   2.620 0.008782
    ## raceHispanic American                  2.167e+00  7.405e-01   2.927 0.003422
    ## ridageyr                               6.392e-02  4.736e-03  13.497  < 2e-16
    ## bmxbmi                                 3.278e-01  1.740e-02  18.840  < 2e-16
    ## I(bmxbmi^2)                           -3.033e-03  2.146e-04 -14.134  < 2e-16
    ## raceAsian American:ridageyr            1.061e-01  1.569e-02   6.760 1.38e-11
    ## raceBlack American:ridageyr            2.124e-02  9.378e-03   2.264 0.023549
    ## raceHispanic American:ridageyr         3.302e-02  9.043e-03   3.651 0.000261
    ## raceAsian American:bmxbmi              3.401e-01  7.410e-02   4.590 4.43e-06
    ## raceBlack American:bmxbmi             -6.191e-02  3.443e-02  -1.798 0.072154
    ## raceHispanic American:bmxbmi          -1.232e-01  3.612e-02  -3.411 0.000647
    ## raceAsian American:I(bmxbmi^2)        -3.631e-03  1.028e-03  -3.532 0.000413
    ## raceBlack American:I(bmxbmi^2)         6.533e-04  4.318e-04   1.513 0.130236
    ## raceHispanic American:I(bmxbmi^2)      1.801e-03  4.579e-04   3.933 8.38e-05
    ## ridageyr:bmxbmi                        1.151e-04  1.400e-04   0.823 0.410691
    ## raceAsian American:ridageyr:bmxbmi    -3.327e-03  5.510e-04  -6.038 1.56e-09
    ## raceBlack American:ridageyr:bmxbmi    -6.298e-04  2.796e-04  -2.252 0.024311
    ## raceHispanic American:ridageyr:bmxbmi -6.753e-04  2.730e-04  -2.473 0.013395
    ##                                          
    ## (Intercept)                           ***
    ## raceAsian American                    ***
    ## raceBlack American                    ** 
    ## raceHispanic American                 ** 
    ## ridageyr                              ***
    ## bmxbmi                                ***
    ## I(bmxbmi^2)                           ***
    ## raceAsian American:ridageyr           ***
    ## raceBlack American:ridageyr           *  
    ## raceHispanic American:ridageyr        ***
    ## raceAsian American:bmxbmi             ***
    ## raceBlack American:bmxbmi             .  
    ## raceHispanic American:bmxbmi          ***
    ## raceAsian American:I(bmxbmi^2)        ***
    ## raceBlack American:I(bmxbmi^2)           
    ## raceHispanic American:I(bmxbmi^2)     ***
    ## ridageyr:bmxbmi                          
    ## raceAsian American:ridageyr:bmxbmi    ***
    ## raceBlack American:ridageyr:bmxbmi    *  
    ## raceHispanic American:ridageyr:bmxbmi *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 128038  on 17709  degrees of freedom
    ## Residual deviance: 103815  on 17690  degrees of freedom
    ##   (717 observations deleted due to missingness)
    ## AIC: 103855
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
summary(readRDS("/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/models/final_paper_model.rda"))
```

    ## 
    ## Call:
    ## glm(formula = final_model_formula, family = "binomial", data = regression_data, 
    ##     weights = round(wtmec8yr/1000))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -8.9980  -1.4636  -0.9059  -0.4061  23.1657  
    ## 
    ## Coefficients:
    ##                                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                           -1.291e+01  3.749e-01 -34.428  < 2e-16
    ## raceAsian American                    -5.997e+00  1.359e+00  -4.414 1.01e-05
    ## raceBlack American                     1.911e+00  7.291e-01   2.620 0.008782
    ## raceHispanic American                  2.167e+00  7.405e-01   2.927 0.003422
    ## ridageyr                               6.392e-02  4.736e-03  13.497  < 2e-16
    ## bmxbmi                                 3.278e-01  1.740e-02  18.840  < 2e-16
    ## I(bmxbmi^2)                           -3.033e-03  2.146e-04 -14.134  < 2e-16
    ## raceAsian American:ridageyr            1.061e-01  1.569e-02   6.760 1.38e-11
    ## raceBlack American:ridageyr            2.124e-02  9.378e-03   2.264 0.023549
    ## raceHispanic American:ridageyr         3.302e-02  9.043e-03   3.651 0.000261
    ## raceAsian American:bmxbmi              3.401e-01  7.410e-02   4.590 4.43e-06
    ## raceBlack American:bmxbmi             -6.191e-02  3.443e-02  -1.798 0.072154
    ## raceHispanic American:bmxbmi          -1.232e-01  3.612e-02  -3.411 0.000647
    ## raceAsian American:I(bmxbmi^2)        -3.631e-03  1.028e-03  -3.532 0.000413
    ## raceBlack American:I(bmxbmi^2)         6.533e-04  4.318e-04   1.513 0.130236
    ## raceHispanic American:I(bmxbmi^2)      1.801e-03  4.579e-04   3.933 8.38e-05
    ## ridageyr:bmxbmi                        1.151e-04  1.400e-04   0.823 0.410691
    ## raceAsian American:ridageyr:bmxbmi    -3.327e-03  5.510e-04  -6.038 1.56e-09
    ## raceBlack American:ridageyr:bmxbmi    -6.298e-04  2.796e-04  -2.252 0.024311
    ## raceHispanic American:ridageyr:bmxbmi -6.753e-04  2.730e-04  -2.473 0.013395
    ##                                          
    ## (Intercept)                           ***
    ## raceAsian American                    ***
    ## raceBlack American                    ** 
    ## raceHispanic American                 ** 
    ## ridageyr                              ***
    ## bmxbmi                                ***
    ## I(bmxbmi^2)                           ***
    ## raceAsian American:ridageyr           ***
    ## raceBlack American:ridageyr           *  
    ## raceHispanic American:ridageyr        ***
    ## raceAsian American:bmxbmi             ***
    ## raceBlack American:bmxbmi             .  
    ## raceHispanic American:bmxbmi          ***
    ## raceAsian American:I(bmxbmi^2)        ***
    ## raceBlack American:I(bmxbmi^2)           
    ## raceHispanic American:I(bmxbmi^2)     ***
    ## ridageyr:bmxbmi                          
    ## raceAsian American:ridageyr:bmxbmi    ***
    ## raceBlack American:ridageyr:bmxbmi    *  
    ## raceHispanic American:ridageyr:bmxbmi *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 128038  on 17709  degrees of freedom
    ## Residual deviance: 103815  on 17690  degrees of freedom
    ##   (717 observations deleted due to missingness)
    ## AIC: 103855
    ## 
    ## Number of Fisher Scoring iterations: 7

## Age and BMI model

``` r
age_and_bmi_formula <- diabetes ~ ridageyr + bmxbmi 

age_and_bmi_model <- glm(age_and_bmi_formula, 
                         data = data, 
                         family = "binomial",
                         weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints

summary(age_and_bmi_model)
```

    ## 
    ## Call:
    ## glm(formula = age_and_bmi_formula, family = "binomial", data = data, 
    ##     weights = round(wtmec8yr/1000))
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -10.4361   -1.3032   -0.8043   -0.4175   21.7847  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -8.7565921  0.0549972 -159.22   <2e-16 ***
    ## ridageyr     0.0668992  0.0006466  103.46   <2e-16 ***
    ## bmxbmi       0.1075971  0.0011777   91.36   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 133864  on 18426  degrees of freedom
    ## Residual deviance: 111362  on 18424  degrees of freedom
    ## AIC: 111368
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
summary(readRDS("/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/models/age_and_bmi_model.rda"))
```

    ## 
    ## Call:
    ## glm(formula = age_and_bmi_formula, family = "binomial", data = regression_data, 
    ##     weights = round(wtmec8yr/1000))
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -10.4361   -1.3032   -0.8043   -0.4175   21.7847  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -8.7565921  0.0549972 -159.22   <2e-16 ***
    ## ridageyr     0.0668992  0.0006466  103.46   <2e-16 ***
    ## bmxbmi       0.1075971  0.0011777   91.36   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 133864  on 18426  degrees of freedom
    ## Residual deviance: 111362  on 18424  degrees of freedom
    ## AIC: 111368
    ## 
    ## Number of Fisher Scoring iterations: 5

## Social determinants model

``` r
social_determinants_model_formula <- diabetes ~ ridageyr + bmxbmi + income + health_insurance + educ + food_security

social_determinants_model <- glm(social_determinants_model_formula, 
                         data = data, 
                         family = "binomial",
                         weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints

summary(social_determinants_model)
```

    ## 
    ## Call:
    ## glm(formula = social_determinants_model_formula, family = "binomial", 
    ##     data = data, weights = round(wtmec8yr/1000))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -9.6054  -1.3937  -0.8150  -0.4071  21.8859  
    ## 
    ## Coefficients:
    ##                                              Estimate Std. Error z value
    ## (Intercept)                                -8.4762224  0.0865910 -97.888
    ## ridageyr                                    0.0685249  0.0006965  98.383
    ## bmxbmi                                      0.1064977  0.0012321  86.436
    ## income$10,000 to $14,999                    0.0102479  0.0705726   0.145
    ## income$100,000 and Over                    -0.4642824  0.0658206  -7.054
    ## income$15,000 to $19,999                   -0.2131579  0.0710019  -3.002
    ## income$20,000 and Over                      0.0447486  0.0743001   0.602
    ## income$20,000 to $24,999                    0.2019943  0.0683452   2.956
    ## income$25,000 to $34,999                   -0.2891486  0.0669319  -4.320
    ## income$35,000 to $44,999                   -0.2211302  0.0671095  -3.295
    ## income$45,000 to $54,999                   -0.4306054  0.0691826  -6.224
    ## income$5,000 to $9,999                     -0.1705606  0.0779675  -2.188
    ## income$55,000 to $64,999                    0.0251865  0.0685328   0.368
    ## income$65,000 to $74,999                   -0.0920035  0.0698808  -1.317
    ## income$75,000 to $99,999                   -0.2061955  0.0671374  -3.071
    ## incomeUnder $20,000                        -0.1579269  0.1105413  -1.429
    ## health_insuranceYes                         0.1417324  0.0247747   5.721
    ## educCollege graduate or above              -0.5737435  0.0318157 -18.033
    ## educHigh school graduate/GED or equivalent -0.2726825  0.0299791  -9.096
    ## educLess than 9th grade                     0.3587863  0.0392822   9.134
    ## educMore than high school                  -1.1655427  0.3564049  -3.270
    ## educSome college or AA degree              -0.2779488  0.0290828  -9.557
    ## food_securityOften worried                  0.2880551  0.0309198   9.316
    ## food_securitySometimes worried              0.2241514  0.0235095   9.534
    ##                                            Pr(>|z|)    
    ## (Intercept)                                 < 2e-16 ***
    ## ridageyr                                    < 2e-16 ***
    ## bmxbmi                                      < 2e-16 ***
    ## income$10,000 to $14,999                   0.884544    
    ## income$100,000 and Over                    1.74e-12 ***
    ## income$15,000 to $19,999                   0.002681 ** 
    ## income$20,000 and Over                     0.546995    
    ## income$20,000 to $24,999                   0.003122 ** 
    ## income$25,000 to $34,999                   1.56e-05 ***
    ## income$35,000 to $44,999                   0.000984 ***
    ## income$45,000 to $54,999                   4.84e-10 ***
    ## income$5,000 to $9,999                     0.028700 *  
    ## income$55,000 to $64,999                   0.713238    
    ## income$65,000 to $74,999                   0.187980    
    ## income$75,000 to $99,999                   0.002132 ** 
    ## incomeUnder $20,000                        0.153100    
    ## health_insuranceYes                        1.06e-08 ***
    ## educCollege graduate or above               < 2e-16 ***
    ## educHigh school graduate/GED or equivalent  < 2e-16 ***
    ## educLess than 9th grade                     < 2e-16 ***
    ## educMore than high school                  0.001074 ** 
    ## educSome college or AA degree               < 2e-16 ***
    ## food_securityOften worried                  < 2e-16 ***
    ## food_securitySometimes worried              < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 127767  on 17224  degrees of freedom
    ## Residual deviance: 103776  on 17201  degrees of freedom
    ##   (1202 observations deleted due to missingness)
    ## AIC: 103824
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
summary(readRDS("/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/models/social_determinants_model.rda"))
```

    ## 
    ## Call:
    ## glm(formula = social_determinants_model_formula, family = "binomial", 
    ##     data = regression_data, weights = round(wtmec8yr/1000))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -9.6054  -1.3937  -0.8150  -0.4071  21.8859  
    ## 
    ## Coefficients:
    ##                                              Estimate Std. Error z value
    ## (Intercept)                                -8.4762224  0.0865910 -97.888
    ## ridageyr                                    0.0685249  0.0006965  98.383
    ## bmxbmi                                      0.1064977  0.0012321  86.436
    ## income$10,000 to $14,999                    0.0102479  0.0705726   0.145
    ## income$100,000 and Over                    -0.4642824  0.0658206  -7.054
    ## income$15,000 to $19,999                   -0.2131579  0.0710019  -3.002
    ## income$20,000 and Over                      0.0447486  0.0743001   0.602
    ## income$20,000 to $24,999                    0.2019943  0.0683452   2.956
    ## income$25,000 to $34,999                   -0.2891486  0.0669319  -4.320
    ## income$35,000 to $44,999                   -0.2211302  0.0671095  -3.295
    ## income$45,000 to $54,999                   -0.4306054  0.0691826  -6.224
    ## income$5,000 to $9,999                     -0.1705606  0.0779675  -2.188
    ## income$55,000 to $64,999                    0.0251865  0.0685328   0.368
    ## income$65,000 to $74,999                   -0.0920035  0.0698808  -1.317
    ## income$75,000 to $99,999                   -0.2061955  0.0671374  -3.071
    ## incomeUnder $20,000                        -0.1579269  0.1105413  -1.429
    ## health_insuranceYes                         0.1417324  0.0247747   5.721
    ## educCollege graduate or above              -0.5737435  0.0318157 -18.033
    ## educHigh school graduate/GED or equivalent -0.2726825  0.0299791  -9.096
    ## educLess than 9th grade                     0.3587863  0.0392822   9.134
    ## educMore than high school                  -1.1655427  0.3564049  -3.270
    ## educSome college or AA degree              -0.2779488  0.0290828  -9.557
    ## food_securityOften worried                  0.2880551  0.0309198   9.316
    ## food_securitySometimes worried              0.2241514  0.0235095   9.534
    ##                                            Pr(>|z|)    
    ## (Intercept)                                 < 2e-16 ***
    ## ridageyr                                    < 2e-16 ***
    ## bmxbmi                                      < 2e-16 ***
    ## income$10,000 to $14,999                   0.884544    
    ## income$100,000 and Over                    1.74e-12 ***
    ## income$15,000 to $19,999                   0.002681 ** 
    ## income$20,000 and Over                     0.546995    
    ## income$20,000 to $24,999                   0.003122 ** 
    ## income$25,000 to $34,999                   1.56e-05 ***
    ## income$35,000 to $44,999                   0.000984 ***
    ## income$45,000 to $54,999                   4.84e-10 ***
    ## income$5,000 to $9,999                     0.028700 *  
    ## income$55,000 to $64,999                   0.713238    
    ## income$65,000 to $74,999                   0.187980    
    ## income$75,000 to $99,999                   0.002132 ** 
    ## incomeUnder $20,000                        0.153100    
    ## health_insuranceYes                        1.06e-08 ***
    ## educCollege graduate or above               < 2e-16 ***
    ## educHigh school graduate/GED or equivalent  < 2e-16 ***
    ## educLess than 9th grade                     < 2e-16 ***
    ## educMore than high school                  0.001074 ** 
    ## educSome college or AA degree               < 2e-16 ***
    ## food_securityOften worried                  < 2e-16 ***
    ## food_securitySometimes worried              < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 127767  on 17224  degrees of freedom
    ## Residual deviance: 103776  on 17201  degrees of freedom
    ##   (1202 observations deleted due to missingness)
    ## AIC: 103824
    ## 
    ## Number of Fisher Scoring iterations: 6

## Biological determinants model

``` r
biological_determinants_formula <- diabetes ~ ridageyr + bmxbmi + gender +
  pad660 + pad615 + whd140 + bmxwt + bmxht + bmxwaist + 
  increased_diabetes_risk + relatives_had_diabetes + felt_depressed + feels_at_risk_diabetes

biological_determinants_model <- glm(biological_determinants_formula, 
                         data = data, 
                         family = "binomial",
                         weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints

summary(biological_determinants_model)
```

    ## 
    ## Call:
    ## glm(formula = biological_determinants_formula, family = "binomial", 
    ##     data = data, weights = round(wtmec8yr/1000))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -4.5718  -0.3186  -0.1559  -0.0750  13.6721  
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                    -4.704e+01  1.119e+01  -4.204 2.62e-05 ***
    ## ridageyr                        4.079e-02  8.420e-03   4.845 1.27e-06 ***
    ## bmxbmi                          3.722e-01  1.610e-01   2.312 0.020772 *  
    ## genderWoman                     1.327e-02  3.231e-01   0.041 0.967250    
    ## pad660                          4.331e-04  3.031e-04   1.429 0.153089    
    ## pad615                          6.186e-04  1.472e-04   4.203 2.63e-05 ***
    ## whd140                          4.397e-04  6.058e-05   7.259 3.90e-13 ***
    ## bmxwt                          -1.219e-01  5.082e-02  -2.398 0.016490 *  
    ## bmxht                           1.769e-01  6.315e-02   2.801 0.005093 ** 
    ## bmxwaist                        7.455e-02  2.000e-02   3.727 0.000193 ***
    ## increased_diabetes_riskYes      1.882e+00  2.516e-01   7.482 7.31e-14 ***
    ## relatives_had_diabetesYes       1.590e+00  2.968e-01   5.355 8.56e-08 ***
    ## felt_depressedNearly every day -1.402e+01  4.812e+02  -0.029 0.976751    
    ## felt_depressedNot at all        2.560e-01  6.092e-01   0.420 0.674297    
    ## felt_depressedSeveral days      8.446e-01  6.318e-01   1.337 0.181243    
    ## feels_at_risk_diabetesYes      -1.305e+00  2.346e-01  -5.563 2.66e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1521.2  on 1000  degrees of freedom
    ## Residual deviance: 1013.0  on  985  degrees of freedom
    ##   (17426 observations deleted due to missingness)
    ## AIC: 1045
    ## 
    ## Number of Fisher Scoring iterations: 16

``` r
summary(readRDS("/Users/madisoncoots/Documents/harvard/research/race-diabetes/race-and-diabetes-risk/models/biological_determinants_model.rda"))
```

    ## 
    ## Call:
    ## glm(formula = biological_determinants_formula, family = "binomial", 
    ##     data = regression_data_no_na, weights = round(wtmec8yr/1000))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -4.5718  -0.3186  -0.1559  -0.0750  13.6721  
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                    -4.704e+01  1.119e+01  -4.204 2.62e-05 ***
    ## ridageyr                        4.079e-02  8.420e-03   4.845 1.27e-06 ***
    ## bmxbmi                          3.722e-01  1.610e-01   2.312 0.020772 *  
    ## genderWoman                     1.327e-02  3.231e-01   0.041 0.967250    
    ## pad660                          4.331e-04  3.031e-04   1.429 0.153089    
    ## pad615                          6.186e-04  1.472e-04   4.203 2.63e-05 ***
    ## whd140                          4.397e-04  6.058e-05   7.259 3.90e-13 ***
    ## bmxwt                          -1.219e-01  5.082e-02  -2.398 0.016490 *  
    ## bmxht                           1.769e-01  6.315e-02   2.801 0.005093 ** 
    ## bmxwaist                        7.455e-02  2.000e-02   3.727 0.000193 ***
    ## increased_diabetes_riskYes      1.882e+00  2.516e-01   7.482 7.31e-14 ***
    ## relatives_had_diabetesYes       1.590e+00  2.968e-01   5.355 8.56e-08 ***
    ## felt_depressedNearly every day -1.402e+01  4.812e+02  -0.029 0.976751    
    ## felt_depressedNot at all        2.560e-01  6.092e-01   0.420 0.674297    
    ## felt_depressedSeveral days      8.446e-01  6.318e-01   1.337 0.181243    
    ## feels_at_risk_diabetesYes      -1.305e+00  2.346e-01  -5.563 2.66e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1521.2  on 1000  degrees of freedom
    ## Residual deviance: 1013.0  on  985  degrees of freedom
    ## AIC: 1045
    ## 
    ## Number of Fisher Scoring iterations: 16
