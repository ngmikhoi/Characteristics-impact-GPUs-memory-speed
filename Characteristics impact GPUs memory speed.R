#####LIBRARY#####
library(ggplot2) #help plot predicted and actual values plot, scatter plot
library(randomForest) #library for Random Forest Regression
library(corrplot) #library to create correlation plot

#####3. DATA PREPROCESSING#####
data <- read.csv("All_GPUs.csv") #Read file csv All_GPUs.csv and store into variable name "data"
head(data) #have the first view of All_GPUs data set

#variable name selected_data store the data set that contains only columns that we will work.
selected_data <- data[, c("Architecture", "Core_Speed", "Memory", "Memory_Bandwidth", "Memory_Speed", "Memory_Bus", "Memory_Type", "Process")]
head(selected_data) #have the first view of selected_data

#we can clearly see that in our data column, there are many symbols stand for NA values, so we store them into missingSign
missingSign <- c(""," ","\n","\n-", "\n- ")

#count the number of NA values of each column
apply(selected_data == missingSign, 2, sum)

#view the row locations of NA values of each column
apply(selected_data == missingSign, 2, which)

#compute the probability of NA value appearance of each column
apply(selected_data == missingSign, 2, mean)

#Store data into new variable name newData and then make cells contains missingSign become NA to easily treat in next steps
newData = selected_data
for(ch in missingSign){
  newData[newData == ch] = NA
}

#due to the low probabilities of NA values, so we decide to delete row contains NA values using na.omit(data_set)
newData <- na.omit(newData)

#At this step, we delete the unit in each cell, and make them become numeric type.
newData$Core_Speed <- as.numeric(sub(" MHz", "", newData$Core_Speed))

newData$Memory <- as.numeric(sub(" MB", "", newData$Memory))

newData$Memory_Bandwidth <- as.numeric(sub("GB/sec","",newData$Memory_Bandwidth))

newData$Memory_Bus <- as.numeric(sub(" Bit", "", newData$Memory_Bus))

newData$Memory_Speed <- as.numeric(sub(" MHz", "", newData$Memory_Speed))

newData$Process <- as.numeric(sub("nm", "", newData$Process))

apply(is.na(newData), 2, sum)

summary(newData)

# Change memory_type into numeric type using factor
newData$Memory_Type <- factor(newData$Memory_Type, levels = unique(newData$Memory_Type))
newData$Memory_Type_Num <- as.numeric(newData$Memory_Type)

#Change Architecture into numeric type using factor
newData$Architecture <- factor(newData$Architecture, levels = unique(newData$Architecture))
newData$Architecture_Num <- as.numeric(newData$Architecture)

#####4. DESCRIPTIVE STATISTICS#####
#draw histogram
hist(newData$Memory_Speed, xlab = "Memory_Speed", main = "Histogram of Memory_Speed"
     , col = "orange")

hist(newData$Memory_Bandwidth, xlab = "Memory_Bandwidth", main = "Histogram of Memory_Bandwidth"
     , col = "orange")

hist(newData$Memory_Bus, xlab = "Memory_Bus", main = "Histogram of Memory_Bus"
     , col = "orange")

hist(newData$Memory, xlab = "Memory", main = "Histogram of Memory"
     , col = "orange")

hist(newData$Core_Speed, xlab = "Core_Speed", main = "Histogram of Core_Speed"
     , col = "orange")

hist(newData$Process, xlab = "Process", main = "Histogram of Process"
     , col = "orange")


#draw boxplot
#core_speed
boxplot(newData$Core_Speed, main = "Boxplot of Core_Speed")

#memory
boxplot(newData$Memory, main = "Boxplot of Memory")

#Memory_Bandwidth
boxplot(newData$Memory_Bandwidth, main = "Boxplot of Memory_Bandwidth")

#Memory_Speed
boxplot(newData$Memory_Speed,  main = "Boxplot of Memory_Speed")

#Memory_Bus
boxplot(newData$Memory_Bus,  main = "Boxplot of Memory_Bus")

#Process
boxplot(newData$Process, main = "Boxplot of Process")

#Boxplot about Memory_Speed and Memory_Type_Num
boxplot(newData$Memory_Speed ~ newData$Memory_Type_Num,
        xlab = "Memory_Type_Number", 
        ylab = "Memory_Speed")      

#Boxplot about Memory_Speed and Process
boxplot(newData$Memory_Speed ~ newData$Process,
        xlab = "Process",  
        ylab = "Memory_Speed")       

#draw correlation plot
#Removed 2 character distortions
newData <- newData[, !names(newData) %in% c("Architecture", "Memory_Type")]
my_colors <- colorRampPalette(c("blue", "green", "red"))(10)  #creates a color palette consisting of 10 colors ranging from blue to red
corrplot(cor(newData), method = "number", col = my_colors, tl.cex = 0.8, number.cex = 0.8)
#The parameter method = "number" specifies that the value of each cell in the plot will be the correlation value. The col parameter is used to specify the color palette for the plot. #tl.cex and number.cex are parameters for adjusting the size of labels on the plot and numeric values, respectively.

#draw scatter plot
#Core_Speed and Memory_Speed
#shape=1 is circle
ggplot(newData, aes(x = Core_Speed, y = Memory_Speed)) +
  geom_point(shape = 1, color = "green", size = 2, alpha = 0.7) +  #shape=1 is circle
  labs( x = "Core_Speed",
        y = "Memory_Speed") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  
        axis.title = element_text(size = 12),    
        axis.text = element_text(size = 10))     

#2: Memory and Memory_Speed
#shape=1 is circle
ggplot(newData, aes(x = Memory, y = Memory_Speed)) +
  geom_point(shape = 1, color = "green", size = 2, alpha = 0.7) +  #shape=1 is circle
  labs( x = "Memory",
        y = "Memory_Speed") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  
        axis.title = element_text(size = 12),    
        axis.text = element_text(size = 10))     

#3: Memory_Bandwidth and Memory_Speed
#shape=1 is circle
ggplot(newData, aes(x = Memory_Bandwidth, y = Memory_Speed)) +
  geom_point(shape = 1, color = "purple", size = 2, alpha = 0.7) +  
  labs( x = "Memory_Bandwidth",
        y = "Memory_Speed") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  
        axis.title = element_text(size = 12),    
        axis.text = element_text(size = 10))     

#4: Memory_Bus and Memory_Speed
#shape=1 is circle
ggplot(newData, aes(x = Memory_Bus, y = Memory_Speed)) +
  geom_point(shape = 1, color = "black", size = 2, alpha = 0.7) +  
  labs( x = "Memory_Bus",
        y = "Memory_Speed") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  
        axis.title = element_text(size = 12),    
        axis.text = element_text(size = 10))     

#5: Architecture and Memory_Speed
#shape=1 is circle
ggplot(newData, aes(x = Architecture_Num, y = Memory_Speed)) +
  geom_point(shape = 1, color = "red", size = 2, alpha = 0.7) +  
  labs( x = "Architecture",
        y = "Memory_Speed") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  
        axis.title = element_text(size = 12),    
        axis.text = element_text(size = 10))

#####NOTE BEFORE SWITCH TO NEXT SECTION#####
#In the next step, we store again Architecture_Num into new column name Architecture and delete Architecture_Num, and so to Memory_Type.

newData$Memory_Type <- newData$Memory_Type_Num
newData$Memory_Type_Num <- NULL

newData$Architecture <- newData$Architecture_Num
newData$Architecture_Num <- NULL


#####5.1. DATA PREPARATION#####
#seed(123) to make a random generation homogeneous among all our tests. Whenever we run the cell, we could get consistent results of the split
set.seed(123)
#This function allows us to randomly select a subset of our dataset without replacement, ensuring each element is chosen only once. Its output consists of the indices of 70 randomly selected elements.
train <- sample(1:nrow(newData), nrow(newData) * 0.7)
#train_data stores the data for training
train_data <- newData[train,]
#test_data stores the data for testing
test_data <- newData[-train,]

#####5.2. LINEAR REGRESSION MODEL#####
###TRAINING###
#we first build the linear regression for Memory_Speed with all other features
lr_model <- lm(Memory_Speed ~ Architecture + Core_Speed + Memory + Memory_Bandwidth + Memory_Bus + Memory_Type + Process, data = train_data)
summary(lr_model)

#Linear Regression for Memory_Speed with all other features except Memory
lr_model <- lm(Memory_Speed ~ Architecture + Core_Speed + Memory_Bandwidth + Memory_Bus + Memory_Type + Process, data = train_data)
summary(lr_model)

#draw scale location plot for linear regression model
plot(lr_model, which = 3)
#draw Q-Q plot for linear regression model
plot(lr_model, which = 2)

###TESTING###
#get predicted values and store them into predicted_value variable
predicted_value <- predict(lr_model, newdata = test_data)
#Combine predicted value and real value into a table with 2 columns Predicted and Real
predicted_real_value_table <- data.frame(Predicted = predicted_value, Real = test_data$Memory_Speed)
#print table
print(predicted_real_value_table)

#Compute accuracy of the model
accuracy <- sum(1 - abs(predicted_real_value_table$Predicted - predicted_real_value_table$Real) / predicted_real_value_table$Real) / nrow(predicted_real_value_table)
#Compute MAE of the model
mae <- sum(abs(predicted_real_value_table$Predicted - predicted_real_value_table$Real)) / nrow(predicted_real_value_table)

#print the accuracy and MAE of model
print(paste("Accuracy:", accuracy))
print(paste("MAE:", mae))

#plot predict comparing plot for linear regression model
ggplot(predicted_real_value_table, aes(x = Real, y = Predicted)) +
  geom_point(shape = 1, color = "blue") +
  labs(x = "Real Values", y = "Predicted Values") +
  geom_abline(intercept = 0, slope = 1, color = "black")


#####6.2. RANDOM FOREST MODEL#####
###TRAINING###
#rft_model store the random forest tree regression model for Memory_Speed with respect to other features except Memory
rft_model <- randomForest(Memory_Speed ~ Architecture + Core_Speed + Memory_Bandwidth + Memory_Bus + Memory_Type + Process, data = train_data)
print(rft_model)
###TESTING###
#Create a table to store predicted values and actual values. 2 columns are Predicted and Real
predicted_real_value_table_rf <- data.frame(Predicted = predict(rft_model, newdata = test_data), Real = test_data$Memory_Speed)
#print the table
print(predicted_real_value_table_rf)

#Compute accuracy, MAE and R-square of the model
accuracy <- sum(1 - abs(predicted_real_value_table_rf$Predicted - predicted_real_value_table_rf$Real) / predicted_real_value_table_rf$Real) / nrow(predicted_real_value_table_rf)
mae <- sum(abs(predicted_real_value_table_rf$Predicted - predicted_real_value_table_rf$Real)) / nrow(predicted_real_value_table_rf)
r2 <- cor(predicted_real_value_table_rf$Real, predicted_real_value_table_rf$Predicted)^2

#Print accuracy, MAE and R-Square
print(paste("Accuracy:", accuracy))
print(paste("MAE:", mae))
print(paste("R-squared:", r2))

#Plot the Q-Q plot for Random Forest model
residuals <- predicted_real_value_table_rf$Real - predicted_real_value_table_rf$Predicted
qqnorm(residuals)
qqline(residuals)

#plot predict comparing plot for random forest regression model
ggplot(predicted_real_value_table_rf, aes(x = Real, y = Predicted)) +
  geom_point(shape = 1, color = "blue") +
  labs(x = "Real Values", y = "Predicted Values") +
  geom_abline(intercept = 0, slope = 1, color = "black")

#Plot the numbers of trees by mean squared error
plot(rft_model)


#plot predict values versus actual values of 2 models
ggplot() +
  geom_point(data = predicted_real_value_table_rf, aes(x = Real, y = Predicted, color = "Random Forest"), shape = 1) +
  geom_point(data = predicted_real_value_table, aes(x = Real, y = Predicted, color = "Linear Regression"), shape = 1) +
  labs(x = "Real Values", y = "Predicted Values") + geom_abline(intercept = 0, slope = 1, color = "black")



