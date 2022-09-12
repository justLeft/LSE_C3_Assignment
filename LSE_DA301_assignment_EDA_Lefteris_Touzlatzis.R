## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 4. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 5. Include your insights and observations.

###############################################################################

# Load and explore the data

# Install and import Tidyverse.
library(tidyverse)

# Import the data set.
sales <- read.csv(file.choose(), header=T)

# Print the data frame.
sales
view(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales1 <- select(sales, -Ranking, -Year, -Genre, -Publisher)

# View the data frame.
head(sales1)

# View the descriptive statistics.
summary(sales1)
view(sales1)

################################################################################

# Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(EU_Sales, NA_Sales, colour=Platform, data=sales1)
qplot(EU_Sales, Global_Sales, colour=Platform, data=sales1)
qplot(NA_Sales, Global_Sales, colour=Platform, data=sales1)


## 2b) Histograms
# Create histograms.
plot(hist(sales1$EU_Sales))
plot(hist(sales1$NA_Sales))
plot(hist(sales1$Global_Sales))

## 2c) Boxplots
# Create boxplots.
qplot(EU_Sales, data=sales1, geom='boxplot')
qplot(NA_Sales, data=sales1, geom='boxplot')
qplot(Global_Sales, data=sales1, geom='boxplot')
qplot(Global_Sales, Platform, data=sales1, geom='boxplot')

###############################################################################

# 4. Observations and insights

## Scaterplots show that products can have different popularity between
## NA and EU as they are scattered, while global sales are more affected by NA
## as the relationship looks more linear. Few outliers can be noticed.
## Frequency distributions of sales are skewed on the right and boxplots 
## show a lot of outliers.



###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 3. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 4. Include your insights and observations.

################################################################################

# 2. Load and explore the data

# Import the libraries
# The whole tidyverse library.
library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(skimr)
library(DataExplorer)

# View data frame created in Week 4.
View(sales1)

# Check output: Determine the min, max, and mean values, descriptive statistics.
skim(sales1)

# View column names
colnames(sales1)

###############################################################################

# 3. Determine the impact on sales per product_id.

## 3a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
product_sales <- sales1 %>% group_by(Product) %>%
  summarise(NA_Sales=sum(NA_Sales),
            EU_Sales=sum(EU_Sales),
            Global_Sales=sum(Global_Sales),
            .groups='drop')
# Create a summary of the new data frame.
view(product_sales)
summary(product_sales)
skim(product_sales)

# 4. Create plots to review and determine insights into the data set.
## Scatterplots
qplot(EU_Sales, NA_Sales, colour=Product, data=product_sales)
qplot(EU_Sales, Global_Sales, colour=Product, data=product_sales)
qplot(NA_Sales, Global_Sales, colour=Product, data=product_sales)

## Histograms
hist(product_sales$EU_Sales)
hist(product_sales$NA_Sales)
hist(product_sales$Global_Sales)

## Boxplots
boxplot(product_sales$EU_Sales)
boxplot(product_sales$NA_Sales)
boxplot(product_sales$Global_Sales)

# 5. Determine the normality of the data set.

## 5a) Create Q-Q Plots
# Global Sales plots
qqnorm(product_sales$Global_Sales)
qqline(product_sales$Global_Sales) 

# NA Plots.
qqnorm(product_sales$NA_Sales)
qqline(product_sales$NA_Sales) 

# EU Plots.
qqnorm(product_sales$EU_Sales)
qqline(product_sales$EU_Sales)

## 5b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages('moments') 
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(product_sales$Global_Sales)
shapiro.test(product_sales$NA_Sales)
shapiro.test(product_sales$EU_Sales)

## 5c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(product_sales$Global_Sales)
kurtosis(product_sales$Global_Sales)

## 5d) Determine correlation
# As the datapoints are not normally distributed, correlation is not considered
round (cor(product_sales),
       digits=2)

###############################################################################

# 6. Plot the data
# Create plots to gain insights into data.

# view dataset and data types
view(product_sales)
str(product_sales)

# Order based on GLobal Sales
product_sales <- product_sales[order(-product_sales$Global_Sales),]

# Plot relationship between Product and Sales
# Global Sales plot
ggplot(data=product_sales,
       mapping=aes(x=Product, y=Global_Sales)) +
  geom_point(color='red',
             alpha=.5,
             size=3) +  
  geom_smooth(method='lm')

# NA Sales plot
ggplot(data=product_sales,
       mapping=aes(x=Product, y=NA_Sales)) +
  geom_point(color='red',
             alpha=.5,
             size=3) +  
  geom_smooth(method='lm')

# EU Sales plot
ggplot(data=product_sales,
       mapping=aes(x=Product, y=EU_Sales)) +
  geom_point(color='red',
             alpha=.5,
             size=3) +  
  geom_smooth(method='lm')

###############################################################################

## 7. Observations and insights
# Data points in Q-Q plots deviate from the straight line, indicating that 
# we cannot assume normality. This is also confirmed by the Shapiro-Wilk 
# normality tests, where p-value is much lower than significance level 5%. 
# For that reason, I didn't proceed with the correlation analysis.
# Plotting the relationship between the Product and Sales, a worth mentioning 
# trend is observed; the products with lower id tend to have higher sales.


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
view(product_sales)

# Determine a summary of the data frame.
skim(product_sales)
summary(product_sales)

###############################################################################

# 2. Create a simple linear regression models

# Determine the correlation between columns
cor(product_sales)

## Linear regression model NA Sales to Global Sales.
model_Gl_NA <- lm(Global_Sales~NA_Sales, data=product_sales)

# View the model.
model_Gl_NA

# View more outputs for the model - the full regression table.
summary(model_Gl_NA)

# View residuals on a plot.
plot(model_Gl_NA$residuals)

# Plot the relationship between Global and NA Sales.
plot(product_sales$NA_Sales, product_sales$Global_Sales)
#Line-of-best-fit.
abline(coefficients(model_Gl_NA))

## Linear regression model EU Sales to Global Sales.
model_Gl_EU <- lm(Global_Sales~EU_Sales, data=product_sales)

# View the model.
model_Gl_EU

# View more outputs for the model - the full regression table.
summary(model_Gl_EU)

# View residuals on a plot.
plot(model_Gl_EU$residuals)

# Plot the relationship between Global and EU Sales.
plot(product_sales$EU_Sales, product_sales$Global_Sales)
#Line-of-best-fit.
abline(coefficients(model_Gl_NA))

###############################################################################

# 3. Create a multiple linear regression model

## Linear regression model to predict Global Sales using NA and EU Sales.
model_sales <- lm(Global_Sales~NA_Sales+EU_Sales, 
                  data=product_sales)

# View the model.
model_sales

# Multiple linear regression model.
summary(model_sales)

# View residuals on a plot.
plot(model_sales$residuals)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.



###############################################################################

# 5. Observations and insights
# Your observations and insights here...


###############################################################################
###############################################################################




