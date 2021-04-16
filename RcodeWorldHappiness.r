# Set working directory
setwd('D:/LUMS Semester Resources/Senior Year/LUMS Semester 8 Resources/Math-231 (Stats)/Project')


# Imports
library(tidyverse)
library('dplyr')
library('tidyverse')
library('ggplot2')
library(corrplot)


# Reading datasets
df_2015 <- read.csv("Dataset/2015.csv")
df_2016 <- read.csv("Dataset/2016.csv")
df_2017 <- read.csv("Dataset/2017.csv")
df_2018 <- read.csv("Dataset/2018.csv")
df_2019 <- read.csv("Dataset/2019.csv")


# Printing first 6 entries of each dataset
head(df_2015)
head(df_2016)
head(df_2017)
head(df_2018)
head(df_2019)


# Printing dimensions (rows x cols)
cat("Dimensions of 2015 dataset: ", dim(df_2015), "\n")
cat("Dimensions of 2016 dataset: ", dim(df_2016), "\n")
cat("Dimensions of 2017 dataset: ", dim(df_2017), "\n")
cat("Dimensions of 2018 dataset: ", dim(df_2018), "\n")
cat("Dimensions of 2019 dataset: ", dim(df_2019), "\n")


# Printing columns of each dataset
colnames(df_2015)
colnames(df_2016)
colnames(df_2017)
colnames(df_2018)
colnames(df_2019)


# Preprocessing and data cleaning (Method -> Phase 1)

# 2019 dataset
# Renaming columns for ease of access
df_2019 <- df_2019 %>% 
    rename(
        "rank" = "Overall.rank",
        "country" = "Country.or.region",
        "gdp" = "GDP.per.capita",
        "family" = "Social.support",
        "health" = "Healthy.life.expectancy",
        "freedom" = "Freedom.to.make.life.choices",
        "corruption" = "Perceptions.of.corruption"
    )
# Lower casing column names
names(df_2019) <- tolower(names(df_2019))
# Checking for na values
sapply(df_2019, function(x) sum(is.na(x)))
# Checking data types
str(df_2019)
# Sampling 5 values from processed dataframe
df_2019[sample(nrow(df_2019), 5), ]

# 2018 dataset
# Renaming columns for ease of access
df_2018 <- df_2018 %>% 
    rename(
        "rank" = "Overall.rank",
        "country" = "Country.or.region",
        "gdp" = "GDP.per.capita",
        "family" = "Social.support",
        "health" = "Healthy.life.expectancy",
        "freedom" = "Freedom.to.make.life.choices",
        "corruption" = "Perceptions.of.corruption"
    )
# Lower casing column names
names(df_2018) <- tolower(names(df_2018))
# Checking for na values
sapply(df_2018, function(x) sum(is.na(x)))
# Checking data types
str(df_2018)
# Changing data type from chr to numeric
df_2018$corruption <- as.numeric(df_2018$corruption)
# Checking which row has na value. We cannot drop this row as this country data is important (will handle later)
df_2018[rowSums(is.na(df_2018)) > 0,]
# Sampling 5 values from processed dataframe
df_2018[sample(nrow(df_2018), 5), ]

# 2017 dataset
# Renaming columns for ease of access
df_2017 <- df_2017 %>% 
    rename(
        "rank" = "Happiness.Rank",
        "score" = "Happiness.Score",
        "gdp" = "Economy..GDP.per.Capita.",
        "health" = "Health..Life.Expectancy.",
        "corruption" = "Trust..Government.Corruption.",
        "dystopia" = "Dystopia.Residual"
    )
# Lower casing column names
names(df_2017) <- tolower(names(df_2017))
# Checking for na values
sapply(df_2017, function(x) sum(is.na(x)))
# Checking data types
str(df_2017)
# Dropping extra columns which are not present in 2018/2019 dataset (Whisker.high, Whisker.low, and Dystopia.Residual)
df_2017 = subset(df_2017, select = -c(4, 5, 12))
# Sampling 5 values from processed dataframe
df_2017[sample(nrow(df_2017), 5), ]

# 2016 dataset
# Renaming columns for ease of access
df_2016 <- df_2016 %>% 
    rename(
        "rank" = "Happiness.Rank",
        "score" = "Happiness.Score",
        "gdp" = "Economy..GDP.per.Capita.",
        "health" = "Health..Life.Expectancy.",
        "corruption" = "Trust..Government.Corruption.",
        "dystopia" = "Dystopia.Residual"
    )
# Lower casing column names
names(df_2016) <- tolower(names(df_2016))
# Checking for na values
sapply(df_2016, function(x) sum(is.na(x)))
# Checking data types
str(df_2016)
# Dropping extra columns which are not present in 2018/2019 dataset (Region, Lower.Confidence.Interval, Upper.Confidence.Interval, and Dystopia.Residual)
df_2016 = subset(df_2016, select = -c(2, 5, 6, 13))
# Sampling 5 values from processed dataframe
df_2016[sample(nrow(df_2016), 5), ]

# 2015 dataset
# Renaming columns for ease of access
df_2015 <- df_2015 %>% 
    rename(
        "rank" = "Happiness.Rank",
        "score" = "Happiness.Score",
        "gdp" = "Economy..GDP.per.Capita.",
        "health" = "Health..Life.Expectancy.",
        "corruption" = "Trust..Government.Corruption.",
        "dystopia" = "Dystopia.Residual"
    )
# Lower casing column names
names(df_2015) <- tolower(names(df_2015))
# Checking for na values
sapply(df_2015, function(x) sum(is.na(x)))
# Checking data types
str(df_2015)
# Dropping extra columns which are not present in 2018/2019 dataset (Region, Standard.Error, and Dystopia.Residual)
df_2015 = subset(df_2015, select = -c(2, 5, 12))
# Sampling 5 values from processed dataframe
df_2015[sample(nrow(df_2015), 5), ]


# Printing updated columns of each dataset
colnames(df_2015)
colnames(df_2016)
colnames(df_2017)
colnames(df_2018)
colnames(df_2019)


# Adding year column to all datasets
df_2015$year = 2015
df_2016$year = 2016
df_2017$year = 2017
df_2018$year = 2018
df_2019$year = 2019


# Merging all dataframes into one
df <- rbind(df_2015, df_2016, df_2017, df_2018, df_2019)
cat("Dimensions of combined dataset: ", dim(df), "\n")
colnames(df)
# Converting column values of country to lower case
df$country = tolower(df$country)
sapply(df, function(x) sum(is.na(x))) # shows 1 na value in corruption column
str(df)
df[sample(nrow(df), 5), ]


# Imputing na value with mean
df$corruption[is.na(df$corruption)] <- mean(df$corruption, na.rm=TRUE)
# No missing/na values are present in dataset now
sapply(df, function(x) sum(is.na(x)))


# Printing Summary
summary(df)


# Exploratory Data Analysis (EDA) -> Phase 2

# Correlation Analysis (What variables give a higher happiness score)
# Excluding "country" and "year" columns
df_corr = df[, c(2:9)]
head(df_corr)
corr <- cor(df_corr[, -1], method="pearson")
corr
subset(corr, select = c(score))
corrplot(corr, method = "circle")
corrplot(corr, method = "number")


# Regression Analysis -> Phase 3

# 4a: Multi-Linear regression with all the variables
multiple.regression <- lm(score ~ gdp + family + health + freedom + corruption + generosity, data=df[, c(2:9)])
summary(multiple.regression)

# 4b: Diagnostics
# plot(multiple.regression) -> uncomment and run to see plots

# 4c: Distribution of data
distrib <- function(data, main, xlab) {
    boxplot(data, main=main, xlab=xlab, col="blue", border="brown", horizontal=TRUE)
}
par(mar=c(1, 1, 1, 1))
distrib(df$score, "Distribution of score", "score")
distrib(df$gdp, "Distribution of gdp", "gdp")
distrib(df$family, "Distribution of family", "family")
distrib(df$health, "Distribution of health", "health")
distrib(df$freedom, "Distribution of freedom", "freedom")
distrib(df$corruption, "Distribution of corruption", "corruption")
distrib(df$generosity, "Distribution of generosity", "generosity")	





