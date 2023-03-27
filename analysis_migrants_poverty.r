
#install package # comment out control + shift + c

# install.packages("sys")
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("gapminder")
# install.packages("psych")

  
library(psych)
library(gapminder)
#loading package/ Dependencies 
library(tidyverse) #loads ggplot2, dplyr, tidyr, readr.. among others.

library(sys)        # Helps us create paths
#library(readr)     # Helps us read our csv files
library(readxl)     # Helps us read excel files
#library(dplyr)     # Manipulate data sets
#library(tidyr)     # Cleans the data, goal: 1 column/1 variable,
#                                            1 row/1 observation,
#                                            1 cell/1 value 


## Path Locations
path_raw_data_folder <- file.path("C:", "Users", "Martin", "Desktop", "R_projects", "raw_data")
      # Dataset with cases filed (historical)
path_asylum_workload <- file.path(path_raw_data_folder, "Asylum_Workload_Top20_Nations_FY14-21.xlsx")
      # Dataset with poverty indicators (historical)
path_poverty_lvl_hist <- file.path(path_raw_data_folder, "pip_dataset.csv")

#Importing data

asylum_data <- read_excel(path_asylum_workload, col_names = FALSE)

# Open a different tab with your data fully loaded to visually inspect
#View(asylum_data)

#Eliminate rows that are NOT observations
asylum_data <- asylum_data[-(27:32),]
asylum_data <- asylum_data[-1,]

#View(asylum_data)

#forward fill years
ff <- data.frame(t(asylum_data[1,2:49]))
colnames(ff) <- c("year")
ff <- ff %>%
  fill(year, .direction = "down")
asylum_data[1,2:49] <- t(ff)

colnames(asylum_data)[2:49] = c(t(ff))

# fix double columns 
asylum_data <- asylum_data[-1,]


# We are going to focus only on cases filed 
columns_to_keep <- append(1, seq(2,44,6))

asylum_data <- asylum_data[columns_to_keep]

# Get rid off unnecessary rows (total, all other nationalities, etc..)
asylum_data <- asylum_data[-24,]
asylum_data <- asylum_data[-1:-3,]

#Name "country" column
colnames(asylum_data)[1] <- "country"

#Columns with quantitative data are in the wrong format 

asylum_data[,2:9] <- sapply(asylum_data[,2:9], as.numeric)

## We pivot the data so that can create a new column year 
asylum_x_country <- asylum_data %>%
  pivot_longer(cols = colnames(asylum_data)[-1], names_to = 'year', values_to = 'Cases filed')
View(asylum_x_country)

# Rename our column 
colnames(asylum_x_country)[3] <- "cases_filed"
asylum_x_country$year <- as.numeric(asylum_x_country$year)
str(asylum_x_country)

# Make a unique list of countries in the dataset 
# We will need this to filter our next dataset
unique_countries <- c(unique(asylum_x_country$country, incomparables = FALSE))
print(unique_countries)

#####################Second data set#############################


## Bring in a dataset with poverty indicators 

poverty_data <- read_csv(path_poverty_lvl_hist, col_names = TRUE)
View(poverty_data)

## Filter only the countries that we have caseload data for 
## and for the years that we have caseload data for 

poverty_data <- poverty_data %>% 
  filter(country %in% unique_countries)
poverty_data <- poverty_data %>%
  filter(year > 2013)
View(poverty_data)

# Dataset contains alot of columns with different macroeconomic measurements
# We want to focus on poverty indicators plus median income & gini for control
str(poverty_data)

# Identify column with NAs and how any NAs they have
column_w_nas = c()
for (i in 1:length(colnames(poverty_data))){
  if (sum(is.na(poverty_data[,i])) > 0){
    column_w_nas <- append(column_w_nas, colnames(poverty_data)[i])
    print(paste("columnn ", as.character(i),
                colnames(poverty_data)[i], " NAs:"))
    print(sum(is.na(poverty_data[,i])))
  }
}

# Since we have a large amount of variables to test
# with some columns similar to the ones we identified
# we are removing the entire columns in column_w_nas

poverty_data <- poverty_data %>% select(- column_w_nas)

## filter our all columns with poverty variables +gini +median income:

poverty_data <- poverty_data %>%
  select("country","year","median", "headcount_ratio_1000" ,
         "gini", "decile1_avg")
# headcount_ratio_1000	= % of pop in houses with income PP below $10/day
## percentage of poor population

# median = The level of income or expenditure per day
# gini = inequality between 0 and 1, higher indicates greater inequality.

# decile1_avg	= mean income per day within poorest tenth of the pop
## How poor are their poor 

str(poverty_data)

## Take out repeats (Different reporting sources) 
poverty_data <- poverty_data %>%
  distinct(country, year, .keep_all = TRUE)


## Join the Data asylum_x_country

df <- poverty_data %>% left_join(asylum_x_country, 
                              by=c('country',
                                   'year'))
colnames(df)[3] = "median_income_per_day"
## Summarize data

# Mean cases_filed in a year for all years available +
# Total/average amount of cases filed by year 

by_year <- df %>%
  group_by(year) %>%
  summarise(sum_cases_filed = sum(cases_filed))

ggplot(by_year, aes(x = year, y= sum_cases_filed)) +
geom_line()

# # # # # 
by_country <- df %>%
  filter(country %in% c("Brazil","Colombia","Ukraine", "Russia")) %>%
  group_by(country, year) %>%
  summarise(mean_cases_filed = mean(cases_filed))

ggplot(by_country, aes(x = year, y = mean_cases_filed)) +
geom_line() +
facet_wrap(~ country)

## median spending per day in year 2018 for certain nations
df_sum <- df %>%
  filter(country %in% c("Brazil","Colombia","Ukraine", "Russia") &
           year == 2020)
ggplot(df_sum, aes(x = country, y = median_income_per_day)) +
  geom_col()



## exploratory data analysis 
describe(df)

cor_df <- cor(df[,2:7])
View(cor_df)


# model 
df_lm <- lm(cases_filed ~ median_income_per_day + gini +
              headcount_ratio_1000 + decile1_avg, data = df)
summary(df_lm)

# median income seems to explain cases_filed the best 
df_lm <- lm(cases_filed ~ median_income_per_day + gini, data = df)
summary(df_lm)

