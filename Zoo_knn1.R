#Loading the data set
zoo <- read.csv(file.choose())
View(zoo)
#Removing unecassary columns from the data set
zu <- zoo[,2:18]  
str(zu)
#factorising all the varaibles.
zu$hair <- as.factor(zu$hair)
zu$feathers <- as.factor(zu$feathers)
zu$eggs <- as.factor(zu$eggs)
zu$milk <- as.factor(zu$milk)
zu$airborne <- as.factor(zu$airborne)
zu$aquatic <- as.factor(zu$aquatic)
zu$predator <- as.factor(zu$predator)
zu$toothed <- as.factor(zu$toothed)
zu$backbone <- as.factor(zu$backbone)
zu$breathes <- as.factor(zu$breathes)
zu$venomous <- as.factor(zu$venomous)
zu$fins <- as.factor(zu$fins)
zu$legs <- as.factor(zu$legs)
zu$tail <- as.factor(zu$tail)
zu$domestic <- as.factor(zu$domestic)
zu$catsize <- as.factor(zu$catsize)
zu$type <- as.factor(zu$type)



# Data partition
set.seed(123)
ind <- sample(2,nrow(zu), replace = T, prob = c(0.7,0.3))
train <- zu[ind==1,]
test <- zu[ind==2,]

# KNN Model 

train_control <- trainControl(method = "repeatedcv", number = 10,repeats = 3
                          # classprobs are needed when u want to select ROC for optimal K Value
)
set.seed(222)
fit <- train(type ~., data = train, method = 'knn', tuneLength = 20,
             trControl = train_control, preProc = c("center","scale"))

# default metric is accuracy but if u want to use ROC, then mention the same
# Model Performance :
fit # the optimum value for k should be 7

#Visualizing the output
plot(fit)
varImp(fit)
#predicting using the data set's clusters.
pred <- predict(fit, test )
confusionMatrix(pred, test$type)
