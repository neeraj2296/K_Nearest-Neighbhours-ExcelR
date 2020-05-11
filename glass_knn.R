
glass <- read.csv(file.choose())
View(glass)
colnames(glass)

table(glass$Type)

str(glass)

# recode diagnosis as a factor
glass$Type <- factor(glass$Type, levels = c("1", "2","3","5","6","7"),labels = c("glass 1", "glass 2","glass 3", "glass 4","glass 5", "glass 6"))

round(prop.table(table(glass$Type)) * 100, digits = 2)

# summarize any three numeric features
summary(glass)

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#normalize(glass)
glass_n <- as.data.frame(lapply(glass[,1:9], normalize))

# confirm that normalization worked
summary(glass_n$RI)

# create training and test data
glass_train <- glass_n[1:150, ]
glass_test <- glass_n[151:214, ]

# create labels for training and test data

glass_train_labels <- glass[1:150, 10]
#wbcd_train_labels <- wbcd_train_labels[["diagnosis"]] 

glass_test_labels <- glass[151:214, 10]
#wbcd_test_labels <- wbcd_test_labels[["diagnosis"]]
#---- Training a model on the data ----

# load the "class" library
#install.packages("class")
library(class)

glass_test_pred <- knn(train = glass_train, test = glass_test,
                      cl = glass_train_labels, k=21)

##--------Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = glass_test_labels, y = glass_test_pred,
           prop.chisq=FALSE)
