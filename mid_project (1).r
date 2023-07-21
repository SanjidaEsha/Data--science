library(ggplot2)

Mydata<-read.csv("D:/University/11th semester/data mining/fake_bills.csv",header = TRUE,sep = ";")
Mydata

#structure of data frame
str(Mydata)

# Check for missing values
colSums(is.na(Mydata))


discard_instance = Mydata[complete.cases(Mydata$is_genuine,Mydata$diagonal,Mydata$height_left,Mydata$height_right,
                                         Mydata$margin_low,Mydata$margin_up,Mydata$length),]
discard_instance

Mean<-as.data.frame(Mydata)
Mean$margin_low[is.na(Mean$margin_low)]=mean(Mean$margin_low,na.rm = TRUE)
Mean

Median<-as.data.frame(Mydata)
Median$margin_low[is.na(Median$margin_low)]=median(Median$margin_low,na.rm = TRUE)
Median


#library(DescTools)
#mode<-as.data.frame(Mydata)
#mode$margin_low[is.na(mode$margin_low)]=Mode(mode$margin_low,na.rm = TRUE)
#mode

# Detect outliers in numeric variables only
outliers <- sapply(Mydata[, sapply(Mydata, is.numeric)], function(x) {boxplot.stats(x)$out})

# Print the number of outliers detected for each variable
colSums(sapply(outliers, function(x) Mydata %in% x))

#normalization
normali <- function(x) {(x-min(x))/(max(x)-min(x))}
fake_bills_norm <- as.data.frame(lapply(discard_instance[, c(2,3,4,5,6,7)], normali))
fake_bills_norm

set.seed(123)
random <- sample(1:nrow(fake_bills_norm), 0.8 * nrow(fake_bills_norm))
train_data <- fake_bills_norm[random, ]
test_data <- fake_bills_norm[-random, ]

# Extract the target variable (is_genuine)
train_target <- discard_instance$is_genuine[random]
test_target <- discard_instance$is_genuine[-random]

# Define the number of neighbors (k)
k <- 3
k <- 5

library(class)
# Perform KNN classification with Euclidean distance
knn_predictions_euclidean <- knn(train_data, test_data, train_target, k)

confusion = table(knn_predictions_euclidean,test_target)
confusion

sum(diag(confusion))/nrow(test_data)

# Preprocess data for Manhattan distance
attr(train_data, "nn.method") <- "manhattan"

# Perform KNN classification with Manhattan distance
knn_predictions_manhattan <- knn(train_data, test_data, train_target, k)

confusion_manhattan = table(knn_predictions_manhattan,test_target)
confusion_manhattan

sum(diag(confusion))/nrow(test_data)

# Preprocess data for Maximum distance
attr(train_data, "nn.method") <- "maximum"

# Perform KNN classification with Maximum distance
knn_predictions_maximum <- knn(train_data, test_data, train_target, k)

confusion_maximum = table(knn_predictions_maximum,test_target)
confusion_maximum

sum(diag(confusion))/nrow(test_data)



