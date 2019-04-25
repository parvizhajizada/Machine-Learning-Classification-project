# load necessary packages ----
library(readr)
library(tibble)
library(corrplot)
library(ggplot2)

# load data
wdbc <- read_csv("wdbc.csv")

# Rename columns
names(wdbc) <- c("radius1", "texture1", "perimeter1", "area1", "smoothness1", "compactness1", "concavity1", "concave_points1", "symmetry1", "fractal_dimension1",
                 "radius2", "texture2", "perimeter2", "area2", "smoothness2", "compactness2", "concavity2", "concave_points2", "symmetry2", "fractal_dimension2",
                 "radius3", "texture3", "perimeter3", "area3", "smoothness3", "compactness3", "concavity3", "concave_points3", "symmetry3", "fractal_dimension3",
                 "class")

# Factorize target variable with in a more intiutive manner (m is malignent, b is benign)
wdbc$class <- factor(wdbc$class, levels = c(1, 2), labels = c("b", "m"))

# Exploratory data analysis-----
# Checing for NA
any(is.na(wdbc)) # not any NA

# Checing summary statistics
summary(wdbc) 

# Checking structure of the data
str(wdbc) # all explanatory variables are numeric and response variable is binary

#convert as data frame
wdbc <- as.data.frame(wdbc)
#simple plot for each column for quick checking

for (i in seq(1, length(wdbc),1)){
  if (class(wdbc[,i]) == 'numeric'){
    hist(wdbc[,i], main= names(wdbc)[i])
  } else if (class(wdbc[,i]) == 'factor'){
    barplot(table(wdbc[,i]), main= names(wdbc)[i])
  } else { 
    print('there is nothing to plot')
    }
}

  barplot(table(wdbc$class))
# Checking for potential imbalance in response variable
table(wdbc["class"]) # looks like balanced 
prop.table(table(wdbc$class)) # looks like balanced 

# checking for outliers
cooksd <- cooks.distance(glm(class ~ ., 
                             family = "binomial", 
                             data = wdbc))
rownames(wdbc[cooksd > 4*mean(cooksd, na.rm=T), ])
plot(wdbc$radius1, wdbc$class)

# checking for multicollinearity 
correlation <- cor(wdbc[,-c(31)])
mean((correlation > 0.5 | correlation < -0.5))
mean((correlation > 0.7 | correlation < -0.7))
mean((correlation > 0.9 | correlation < -0.9))

# Feature engineering can be considered if the performance of the final model is not satisfactory

# Modelling ----
# split data into training and test sets----
#set seed 123
set.seed(123)
#stratified division with 80/20
which_train <- createDataPartition(wdbc$class, 
                                   p = 0.8, 
                                   list = FALSE) 
# lets use these indices to divide data into two samples

wdbc_train <- wdbc[which_train,]
wdbc_test <- wdbc[-which_train,]



# Objective is to minimize type 2 error (cost of missing a person with malignant tumor)
# We have a binary classification problem
# 
corrplot(cor(wdbc[,-c(31)]), method = "ellipse", type = "upper") 

ggplot(wdbc, aes(x = class, fill = class)) +
  geom_bar(stat = "count", positin = "dodge") +
  ggtitle("Distribution of Bening/Malignant Tumor")

ggplot(wdbc,aes(x=smoothness1,fill=class))+geom_density(alpha=0.25)+
  xlab(label = "Radius1")+
  ggtitle("Distribution of Radius1")

####linear
#train
#predict
#model valuation

#####glm
#train
#predict
#model valuation


#####linear discriminant analysis
#train
#predict
#model valuation

####kNN
#train
#predict
#model valuation

####SVM
#train
#predict
#model valuation