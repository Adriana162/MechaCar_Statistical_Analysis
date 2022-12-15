### MechaCar Statistical Analysis

##Linear Regression to Predict MPG 
The goal of this analysis is to design linear model that predicts the mpg of MechaCar prototypes using several variable. The dataset collected on each car are 
vehicle length, vehicle weight, spoiler angle, drivetrain, and ground clearance. First, the file that included the data was imported and read in as a dataframe. 
Then the `lm()` function is used to perform the multiple linear regression. In the `lm()` function all variables are passed in like so `lm(mpg ,.,). Then we perform 
a summary with the `summary()' funtion, to determine the p-value and r-squared value. 

Results:
![lm_summary.PNG](/resources/lm_summary.PNG)

The drivetrain, spoiler angle, and vehicle weight are the variables which provided a non-random amount of variance to the mpg values in the dataset.
The slope of the linear model is not considered to be zero because the p-value of our linear regression analysis is 5.35 x 10^-11, which is
much smaller than the assumed significance value of 0.05%. Therefore, there is sufficiant evidence to reject the null hypothesis, which means 
that the slope of the linear model is not zero.
This model is more effective in predicting the mpg of Mechacar prototypes because the r-sqaured value is 0.7149. Which means about 71% variability 
of the variables is explained using this model. Although the r-squared value is high, there could be other variables used to give more significant value. 

##Summary Statistics on Suspension Coils
The design specification for the MechaCar suspension coils is a variance that does not exceed 100 pounds per square inch(PSI). 
The current manufacturing data meets this specification in total as the variance is equal to 62.29356. 

Total Summary:
![total_summary.PNG](/resources/total_summary.PNG)

The manufacturing data does not meet the specification of 100 PSI when looking at the total of each lot individually. This is
because the variance of lot 3 is equal to 170.2861224.

Lot Summary:
![lot_summary.PNG](/resources/lot_summary.PNG)

##T-Tests on Suspension Coils
In this section, t-tests are used to determine if all manufacturing lots and each lot individually are statistically different from the population
mean of 1,500 pounds per square inch.


The first test is for all manufacturing slots like so `t.test(suspension_table$PSI,mu=1500)`. The PSI variable of the suspension table is used as the x value and
the mu(true value of the mean) is set to 1500. The p-value is 0.06 which is greater than the assumed signigicance level of 0.05% so there is not enough evidence to 
reject the null hypothesis, making it statistically similar.

The test for lot1 shows the p-value is 1 which is higher than 0.05% so it is not satistically different. The p-value for lot2 is 0.61 which is statistically similar.
Lastly, the p-value for lot3 is 0.04 which is less than 0.05% so there is sufficiant evidence to reject the null hypothesis. 

##Study Design: MechaCar vs Competition 
This section is a design of a statistical study to compare performance of MechaCar vehicles against vehicles from other manufacturers.
The metrics that are going to be looked at are horsepower, fuel efficiency, cost and safety rating. 

The null hypothesis is that there is no statistical difference between sample A (MechaCar) and sample B (Competition).

The statistical test I would use is the two-sample t-test because we are focusing on just the two samples (sample A vs. sample B). 

That data that is needed to run the tests are mileage, price, horsepower ratio and saftey rating from the Insurance Institute for Highway Safety.
The data will be grouped by price ranges to compare if there is a statistical difference in these groups when looking at our metrics. 