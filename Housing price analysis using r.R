library(tidyverse)
library(ggmap)


##### ##### ##### ##### Load the dataset
##### ##### ##### ##### Explore the Dataset  
# id: Unique ID for each home sold
# date: Date of the home sale
# price: Price of each home sold
# bedrooms: Number of bedrooms
# bathrooms: Number of bathrooms, where .5 accounts for a room with a toilet but no shower
# sqft_living: Square footage of the house interior living space
# sqft_lot: Square footage of the land space
# floors: Number of floors
# waterfront: Whether the house was overlooking the waterfront or not
# view: An index of how good the view of the property was (0-4)
# condition: An index on the condition of the apartment (1-5)
# grade: An index on the quality of building construction and design (1-13),
# sqft_above: The square footage of the interior housing space that is above ground level
# sqft_basement: The square footage of the interior housing space that is below ground level
# yr_built: The year the house was initially built
# yr_renovated: The year of the house’s last renovation
# zipcode: Zipcode area the house is in
# lat: Latitude
# long: Longitude
# sqft_living15: The square footage of interior housing living space for the nearest 15 neighbors
# sqft_lot15: The square footage of the land lots of the nearest 15 neighbors


https://www.kaggle.com/datasets/harlfoxem/housesalesprediction

df <- read_csv("kc_house_data.csv")


##### ##### ##### ##### How many rows and columns do we have?
dim(df)


##### ##### ##### ##### Do we have NAs? 
colSums(is.na(df))


##### ##### ##### ##### Should we drop any columns?


df <- subset(df, select = -c(id, date, zipcode, lat, long))



##### ##### ##### ##### Should we change the default data type?
sapply(df, class)

df$waterfront <- as.factor(df$waterfront)


##### ##### ##### ##### Do we have duplicates?  If we do, let's drop them

sum(duplicated(df))

df <- df[!duplicated(df), ]


##### ##### ##### ##### Plot the distribution of house prices

ggplot(df, aes(x = price)) +
  geom_histogram(binwidth = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of  Prices",
       x = "Price",
       y = "Frequency") +
  theme_minimal()


ggplot(df, aes(x = price)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of  Prices",
       x = "Price",
       y = "Frequency") +
  theme_minimal()




##### ##### ##### ##### Do we have outliers?

ggplot(df, aes(y = price)) +
  geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of House Prices",
       y = "Price") +
  theme_minimal()

##### ##### ##### ##### If we have outliers are they in the top range (expensive) or bottom range (cheap)?

##### ##### ##### ##### What is the proportion of homes that are in the top 3% of  prices?
top_3_percent_threshold <- quantile(df$price, 0.97)
print(top_3_percent_threshold)

percentage_top_3_percent <- mean(df$price >= top_3_percent_threshold) * 100
print(percentage_top_3_percent)

num_records_top_3_percent <- sum(df$price >= top_3_percent_threshold)
print(num_records_top_3_percent)


##### ##### ##### ##### Let's remove the top 3% of records before training our model
df <- df[df$price < top_3_percent_threshold, ]


##### ##### ##### ##### Let's plot the distribution of home prices after removing outliers
ggplot(df, aes(x = price)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of  Prices",
       x = "Price",
       y = "Frequency") +
  theme_minimal()


ggplot(df, aes(x = price)) +
  geom_histogram(bins = 20, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of  Prices",
       x = "Price",
       y = "Frequency") +
  theme_minimal()



##### ##### ##### ##### Generate the feature correlation plot
install.packages("corrplot")
library(corrplot)

correlation_matrix <- cor(df)
corrplot(correlation_matrix, method = "circle", type = "upper", 
         order = "hclust", tl.cex = 0.7)

# we need to do this only for numeric columns
numeric_cols <- sapply(df, is.numeric)
df_numeric <- df[, numeric_cols]
correlation_matrix <- cor(df_numeric)
corrplot(correlation_matrix, method = "circle", type = "upper", 
         order = "hclust")

# Construct a linear regression model to predict house prices using 
#  4 features that are strongly correlated with house prices: 
#  sqft_living, grade, sqft_above, and sqft_living15.

lr1 <-lm(price ~ sqft_living + grade + sqft_above + sqft_living15,
         data = df)
summary(lr1)

#What R-squared value did you get?

# Construct another linear regression model to predict house prices using
# all available features

lr2 <- lm(price ~ ., data = df)
summary(lr2)


#What R-squared value did you get?


# Which model resulted in the better R-squared value?



# Split the data into train/test

train_indices <- sample(nrow(df), 0.8 * nrow(df))  # 80% of rows for training
train_df <- df[train_indices, ]
test_df <- df[-train_indices, ]


# Train a linear regression on training data
# Predict house prices on testing data

model <- lm(price ~ ., data = train_df)
predictions <- predict(model, newdata = test_df)


# Calculate RMSE 
install.packages("Metrics")
library(Metrics)

rmse_score <- rmse(test_df$price, predictions)
print(rmse_score)


# Is this a good or bad value?
mean(df$price)



