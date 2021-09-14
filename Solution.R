
###-------------------------#Q1----------------------------###
##a

###Set the working Directory
setwd("<working directory>")
dir()

###Read each data file
df1 = read.table("A.data", sep = ",")
df2 = read.table("B.data", sep = ",")
df3 = read.table("C.data", sep = ",")
df4 = read.table("D.data", sep = ",")

###Bind all 4 data files together
datafile <- rbind(df1,df2,df3,df4)
dim(datafile)

###Save the combined dataset as a csv file
write.csv(datafile,"<working directory>datafile.csv")


###Update the column names to meaningful names
colnames(datafile) <- c("age", "sex", "cp", "restbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")
datafile


### Checking for missing values
is.null(datafile)
head(datafile)
tail(datafile)

###Replace the ? with NA so that the is.na functions can be used
datafile[datafile == "?"]<-NA
tail(datafile)


###to find the percentage of missing values in each column
colMeans(is.na(datafile))*100


### drop ca nd thal columns from the dataset
datafile_1 = subset(datafile, select = -c(ca,thal))
datafile_1


###Check data types before mean imputation
summary(datafile_1)

###Change data type to numeric
datafile_1$restbps <- as.numeric(as.character(datafile_1$restbps))  # Convert one variable to numeric
datafile_1$chol <- as.numeric(as.character(datafile_1$chol))  # Convert one variable to numeric
datafile_1$thalach <- as.numeric(as.character(datafile_1$thalach))
datafile_1$oldpeak <- as.numeric(as.character(datafile_1$oldpeak))


sapply(datafile_1, class)


###Mean imputation of the continuous variables
datafile_1$restbps[is.na(datafile_1$restbps)] <- mean(datafile_1$restbps, na.rm = TRUE)
datafile_1$chol[is.na(datafile_1$chol)] <- mean(datafile_1$chol, na.rm = TRUE)
datafile_1$thalach[is.na(datafile_1$thalach)] <- mean(datafile_1$thalach, na.rm = TRUE)
datafile_1$oldpeak[is.na(datafile_1$oldpeak)] <- mean(datafile_1$oldpeak, na.rm = TRUE)


tail(datafile_1)
colMeans(is.na(datafile_1))*100

###For knn imputation, the data type should be numeric
datafile_1$fbs <- as.numeric(as.character(datafile_1$fbs))
datafile_1$restecg <- as.numeric(as.character(datafile_1$restecg))
datafile_1$exang <- as.numeric(as.character(datafile_1$exang))
datafile_1$slope <- as.numeric(as.character(datafile_1$slope))

tail(datafile_1)
sapply(datafile_1, class)

###KNN Imputation for categorical data

library(VIM)  #KNN Resides in VIM library
datafile_2 <- kNN(datafile_1, variable = c("fbs", "restecg", "exang", "slope"), k = 5)

summary(datafile_2, class)
tail(datafile_2)

###Remove the additional columns comes with KNN imputation. 

datafile_2 = subset(datafile_2, select = -c(fbs_imp,restecg_imp,exang_imp,slope_imp))
datafile_2



###Outliers
#IQR = Q3 - Q1
#Max = Q3 + 1.5IQR
#Min = Q1 - 1.5IQR





###Check the outliers of continuous variables in a univariate manner
boxplot(datafile_2$age, ylab = "age")
boxplot(datafile_2$restbps, ylab = "restbps")
boxplot(datafile_2$chol, ylab = "chol")
boxplot(datafile_2$thalach, ylab = "thalach")
boxplot(datafile_2$oldpeak, ylab = "oldpeak")

boxplot.stats(datafile_2$restbps)$out


###Remove outliers in each continuous variable

# find the index of outliers from restbps
datafile_3 <- datafile_2

datafile_3 <- datafile_3[-which(datafile_3$restbps %in% boxplot.stats(datafile_3$restbps)$out),]

# find the index of outliers from chol
datafile_3 <- datafile_3[-which(datafile_3$chol %in% boxplot.stats(datafile_3$chol)$out),]

# find the index of outliers from thalach
datafile_3 <- datafile_3[-which(datafile_3$thalach %in% boxplot.stats(datafile_3$thalach)$out),]

# find the index of outliers from oldpeak
datafile_3 <- datafile_3[-which(datafile_3$oldpeak %in% boxplot.stats(datafile_3$oldpeak)$out),]




###Check the outliers of continuous variables in a univariate manner
boxplot(datafile_3$age, ylab = "age")
boxplot(datafile_3$restbps, ylab = "restbps")
boxplot(datafile_3$chol, ylab = "chol")
boxplot(datafile_3$thalach, ylab = "thalach")
boxplot(datafile_3$oldpeak, ylab = "oldpeak")


### Data Exploration
dim(datafile_3)
names(datafile_3)
str(datafile_3)
attributes(datafile_3)

####Pie chart - use to visualize categorical frequencies

#####sex
par(mfrow = c(1,2))
pie(table(datafile_3$sex))
pie(table(datafile_3$sex),labels = paste(round(prop.table(table(datafile_3$sex))*100), "%", sep = ""),
    main = "sex")

#####Slope
par(mfrow = c(1,2))
pie(table(datafile_3$slope))
pie(table(datafile_3$slope),labels = paste(sep = " ", round(prop.table(table(datafile_3$slope))*100), "%"),
     main = "Slope")

#####restecg
par(mfrow = c(1,2))
pie(table(datafile_3$restecg))
pie(table(datafile_3$restecg),labels = paste(round(prop.table(table(datafile_3$restecg))*100), "%", sep = ""),
    main = "restecg")

#####cp
par(mfrow = c(1,2))
pie(table(datafile_3$cp))
pie(table(datafile_3$cp),labels = paste(round(prop.table(table(datafile_3$cp))*100), "%", sep = ""),
    main = "cp")


###Density of Age

par(mfrow = c(1,3))

plot(density(datafile_3$age))
plot(density(datafile_3$restbps))
plot(density(datafile_3$chol))

###Histogram
par(mfrow = c(1,3))

hist(datafile_3$age)
hist(datafile_3$restbps)
hist(datafile_3$chol)

##---------------------Q1 (b)-----------------------------------------------------------##
##Cluster Analysis
####K-means  Clustering

#### Find the most viable number of clusters
pkgs <- c("factoextra",  "NbClust")
install.packages(pkgs)

dev.off() 

###datafile_quant has only the quantitative data
datafile_quant <- subset(datafile_3, select = -c(sex,cp,fbs,restecg,exang,slope,num))
datafile_quant
# Elbow method
fviz_nbclust(datafile_quant, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(datafile_quant, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(datafile_quant, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

###NbClust() function: 30 indices for choosing the best number of clusters
NbClust(data = datafile_quant, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "kmeans")


head(datafile_quant)

###K-Means Clustering
datafile_quant_Cluster <- kmeans(datafile_quant[, 0:5], 2)
datafile_quant_Cluster

library(cluster)
clusplot(datafile_quant, datafile_quant_Cluster$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)


###Hierarchical clustering
# Compute dissimilarity matrix
res.dist <- dist(datafile_quant, method = "euclidean")

# Compute hierarchical clustering
res.hc <- hclust(res.dist, method = "ward.D")
plot(res.hc) # display dendogram


groups <- cutree(res.hc, k=2) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(res.hc, k=2, border="red") 

# Visualize
plot(res.hc, cex = 0.5)


###----------------------Q1 (C)----------------------#
##Removing the 2,3,4 valuse from variable 14
library(dplyr)
datafile_4 = filter(datafile_3, num != 2 & num != 3 & num != 4)
dim(datafile_4)

##Splitting the dataset in 80:20 ratio
set.seed(100) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 80% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(datafile_4), size = floor(.80*nrow(datafile_4)), replace = F)
train_data <- datafile_4[sample, ]
test_data  <- datafile_4[-sample, ]

dim(datafile_4)
dim(train_data)
dim(test_data)

##Logistic regression -> is a Generalized Linear Regression(GLM)
mlr = glm(train_data$num~age + sex + cp + restbps + chol + fbs + restecg + thalach + exang + oldpeak + slope,
          data = train_data, family = binomial(link = logit)) #Logit means log of odds

###Identify the significant variables
summary(mlr)

##Logistic regression with the identified significant variables
final_LogisticRModel = glm(train_data$num~sex + cp + exang + oldpeak + slope, 
                           data = train_data, family = binomial(link = logit)) #Logit means log of odds

summary(final_LogisticRModel)

###---(ii)----###
install.packages("klaR")
install.packages("caret")
library(caret)
library(klaR)
x_test <- test_data[,1:11]
y_test <- test_data[,12]

predictions <- predict(final_LogisticRModel, x_test)
predictions <- as.data.frame(predictions)
predictions


##------------------Q1 (d)----------------------------------##
#	Hosmer-Lemeshow H statistic to check the validity of predictive model.

dev.off()
install.packages("BiocManager")
BiocManager::install("limma")
install.packages("MKmisc")
library(MKmisc)

HLgof.test(fit = fitted(final_LogisticRModel), obs = train_data$num)

##pval = 0.9492
##pval > 0.05 ; WE DO NOT REJECT H0
## The model is ADEQUATE

#------------------------------------------------------------------------------------------------#
#-----------------------------------Q2-----------------------------------------------------------#
#------------------------------------------------------------------------------------------------#


data(iris)
head(iris)
str(iris)
summary(iris)
is.na(iris)
dim(iris)

###Introduce NAs to iris dataset randomly
iris_with_na <- as.data.frame(lapply(iris, function(cc) cc[ sample(c(TRUE, NA), prob = c(0.98, 0.02), size = length(cc), replace = TRUE) ]))
summary(iris_with_na)
iris_with_na = filter(iris_with_na, Species != "setosa")

##Abalone dataset introduced with random NA values
datafile_abalone = read.csv("<working directory>abalone.csv", header = T)
datafile_abalone_with_na <- as.data.frame(lapply(datafile_abalone, function(cc) cc[ sample(c(TRUE, NA), prob = c(0.999, 0.001), size = length(cc), replace = TRUE) ]))
summary(datafile_abalone_with_na)


dataset_analyser <- function(df, responseVariable){
  a <- head(df) 

  ### Handle possible NuLL values
  df[df == "?" | df == " " | df == ""] <- NA
  

  ### Identify Quantitative and Qualitative variables
  quant <- select_if(df, is.numeric)
  qual <- df %>% select_if(~!is.numeric(.))
  
  ### Mean of quantitative variables
  mean_result <- colMeans(quant, na.rm = TRUE)
  
  ### Median of quantitative variables
  
  library(matrixStats)
  Mat1 = data.matrix(quant)
  Median <- colMedians(Mat1, na.rm = TRUE, hasNA = TRUE, keep.names=TRUE)
  Variarble <- c(colnames(Mat1))
  median_result4 <- data.frame(Variarble,Median)
  
  
  ### Mode of quantitative variables
  quant_for_mode <- quant
  
  getmode <- function(v) {
    uniq_value <- unique(v)
    uniq_value[which.max(tabulate(match(v, uniq_value)))]
  }
  
  
  mode_result <- sapply(quant_for_mode, getmode)
  
  ### Count the missing values in each variable
  na_count <-sapply(df, function(y) sum(length(which(is.na(y)))))
  
  
  ###Mean Imputation for numeric variables
  quant_imp <- quant
  
  
  ####Check each column for NA values and if found, replace with the mean(NA neglected) of the same column.
  for(i in 1:ncol(quant_imp)) {
    quant_imp[ , i][is.na(quant_imp[ , i])] <- mean(quant_imp[ , i], na.rm=TRUE)
  }
  
  quant_na_count_after_imp <- sapply(quant_imp, function(y) sum(length(which(is.na(y)))))
  
  ###Mode imputation for categorical variables
  qual_imp <- qual
  
  ####Check each column for NA values and if found, call getMode() and replace with the Mode of the same column.
  
  for(i in 1:ncol(qual_imp)) {
    qual_imp[ , i][is.na(qual_imp[ , i])] <- getmode(qual_imp[ , i])
  }
  
  qual_na_count_after_imp <- sapply(qual_imp, function(y) sum(length(which(is.na(y)))))
  
  ###Find univariate outliers for each variable
 # datafile <- cbind(quant_imp,qual_imp)
#  dim(datafile)
  
  
  outlier_results <- list()
  for(i in 1:ncol(quant_imp)) {
    #print(boxplot.stats(quant_imp[, i]))
    boxplot(quant_imp[, 2], ylab = colnames(quant_imp[i]))
    outliers <- boxplot.stats(quant_imp[, i])$out
    
    outlier_results[i] = capture.output(cat("Univariate Outliers of ",colnames(quant_imp[i])," are :",outliers))
  }
  
  
  #### Generate histograms, box plots and pie charts
  ####Histograms for Quantitative variables
  for(i in 1:ncol(quant_imp)) {
    bar_name <- colnames(quant_imp)[i]
    png(file = paste("C:\\Users\\<working directory>\\OneDrive\\Semester 02\\Data Analysis\\Assignment\\graphs\\", 
                     bar_name , "_bar.png", sep=""))
    hist(quant_imp[, i],main = bar_name, xlab= bar_name)
    dev.off()
  }
  
  ####Box plots for Quantitative variables
  for(i in 1:ncol(quant_imp)) {
    bar_name <- colnames(quant_imp)[i]
    png(file = paste("C:\\Users\\<working directory>\\OneDrive\\Semester 02\\Data Analysis\\Assignment\\graphs\\",
                     bar_name , "_box.png", sep=""))
    boxplot(quant_imp[, i], ylab = bar_name)
    dev.off()
  }
  
  ####Pie Charts for Qualitative variables
  for(i in 1:ncol(qual_imp)) {
    bar_name <- colnames(qual_imp)[i]
    png(file = paste("C:\\Users\\<working directory>\\OneDrive\\Semester 02\\Data Analysis\\Assignment\\graphs\\",
                     bar_name, "_pie.png", sep=""))
    pie(table(qual_imp[,i]))
    dev.off()
  }

  
  ##Generate the output of the function
  newList <- list("data file head" = a,"Quantitative VAriables" = names(quant), "Qualitative Variables" = names(qual),
                  "Mean of Quantitative variables" = mean_result, "Median of Quantitative variables" = median_result4,
                  "Mode of Quantitative variables" = mode_result, "Missing Value count in each variable" = na_count,
                  "Missing Value count of quantitative variables after imputation" = quant_na_count_after_imp,
                  "Missing Value count of qualitative variables after imputation" = qual_na_count_after_imp, "Outliers" = outlier_results)
  return(newList)
}




#RUN THE FUNCTION
dataset_analyser(datafile_abalone_with_na,Rings)
sapply(iris, class)




##---------------------------------------------------------------------##
##----------------------------QUESTION 3-------------------------------##
##---------------------------------------------------------------------##
###Set working directory
setwd("C:\\Users\\<working directory>\\OneDrive\\Semester 02\\Data Analysis\\Assignment")

###Read the covid data file
covid_data <- read.csv(file="owid-covid-data.csv", header = T)
covid_data

###Keep only the specified columns
covid_data <- subset(covid_data, select = c('iso_code', 'location', 'date', 'total_cases'))

###Select the countries to compare sri Lanka with
asia <- c("LKA","SGP", "CHN")

###filter the dataset to have only the 3 selected country data
covid_data_asia <- covid_data[covid_data$iso_code %in% asia,]
tail(covid_data_asia)

###Specify a date range to plot the line graph

covid_data_asia_range <- subset(covid_data_asia, date >"2021-03-22" & date < "2021-04-10" )


##Plot the graphs
library(ggplot2)
ggplot(data=covid_data_asia_range, aes(x=date, y=total_cases, group=iso_code, color=iso_code))+geom_line()
+ ggtitle("Standing of Sri Lanka in Total Covid cases compared to SIngapore and China")


