Fijian Earthquakes
================
Jay Lee
9/7/2017

### Earthquake Detection

1.  

![](Lab_2_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-1.png)

The data shows a strong, positive, linear relationship.

1.  If there was no relationship between these two values, we would expect the slope of the model to be 0 and the y-intercept to be $\\bar{y}$.

2.  

<!-- -->

    ## 
    ## Call:
    ## lm(formula = stations ~ mag, data = quakes)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -48.871  -7.102  -0.474   6.783  50.244 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -180.4243     4.1899  -43.06   <2e-16 ***
    ## mag           46.2822     0.9034   51.23   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 11.5 on 998 degrees of freedom
    ## Multiple R-squared:  0.7245, Adjusted R-squared:  0.7242 
    ## F-statistic:  2625 on 1 and 998 DF,  p-value: < 2.2e-16

![](Lab_2_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png)
