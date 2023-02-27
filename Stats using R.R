
#1 : Data set:
  #Data Set impored from kaggle.
  #The data consists of Marks of students including their study time & number of courses. 
  
  library(tidyverse)
  #Reading the CSV File
  dataset <- read.csv("E:\\Files\\Student_Marks.csv")
  names(dataset) <- make.names(names(dataset ), unique=TRUE)
  # 1. Loading 
  data(dataset)
  # 2. Print only top few values of the data
  head(dataset) 
  # Check the dimensions of the data.
  dim(dataset)
  #priting the summary
  print(summary(dataset))
  
  
  #Converting integer values to numeric:
  dataset$number_courses=as.numeric(dataset$number_courses)
  
#3 Basic Plots:
  attach(dataset)
  
  #boxplot for Marks vs time studied:
  boxplot(dataset$Marks ~dataset$time_study, main="Fig.-1: Boxplot of Marks vs time studied", col= rainbow(5))
  #Boxplot of Course vs time studied:
  boxplot(dataset$number_courses ~dataset$time_study, main="Fig.-1: Boxplot of Course vs time studied", col= rainbow(5))
  
  ggplot(data = dataset, aes(x = dataset$time_study, y = dataset$Marks)) +geom_line(color="blue")
  
  ggplot(data = dataset, mapping = aes(x = dataset$number_courses, y = dataset$Marks)) +geom_point(alpha = 0.1,color = "blue")
  
  ggplot(data = dataset, mapping = aes(x = time_study, y = Marks)) +geom_point(alpha = 0.1,color = "blue")
  
  
#4 Correlation:
  library(corrplot)
  cor.mat.dataset = cor(dataset)
  corrplot(cor.mat.dataset)
  corrplot(cor.mat.dataset, method='number')
  corrplot(cor.mat.dataset,method = 'square', order = 'FPC', type = 'lower', diag = FALSE)
  
  cor(dataset$Marks , dataset$time_study)
  #r=0.9422539 hence positive correlation between marks and time studied
  
#5 Confidence Intervals:
  library(Rmisc)
  CI(dataset$Marks, ci = 0.95)
  #mean = 24.41769
  #21.57506<Mean<27.26032
  
  CI(dataset$time_study, ci = 0.90)
  #mean = 4.077140
  #3.683143<Mean<4.471137
  
#6 Hypothesis Testing:
  library(stats)
  
  # One sample t-test
  # to test: Is the mean value of Sales is equal to 24 or not?
  t.test(dataset$Marks, mu = 24)
  #Null Hypothesis : True mean is 24
  #alternative hypothesis: true mean is not equal to 24
  #t = 0.29156, df = 99, p-value = 0.7712
  
  t.test(dataset$time_study, mu =4 , alternative = 'greater') 
  #t = 0.32509, df = 99, p-value = 0.3729
  #Null Hypothesis : True mean is less than 4
  #alternative hypothesis: true mean is greater than 4
  
  
  t.test(dataset$time_study, mu =4 , alternative = 'less')
  #t = 0.32509, df = 99, p-value = 0.6271
  #Null Hypothesis : True mean is greater than 4
  #alternative hypothesis: true mean is less than 4
  
  

#7 Chi-square test:
   
  chisq.test(dataset$Marks , dataset$time_study)
  #P value = 0.239
  #P value is greater than 0.05 
  
  chisq.test(dataset$number_courses , dataset$time_study)
  #P value = 0.4288
  # P value is greater than 0.05 
  
  chisq.test(dataset$number_courses , dataset$Marks)
  #P value = 0.4288
  #P value is greater than 0.05 
  
#8 Analysis of Variance:
  
  boxplot(dataset$Marks ~dataset$time_study, col= rainbow(5))
  model1 <- aov(dataset$Marks ~dataset$time_study)
  summary(model1)
  
  # F value = 775.8
  
  model2 <- aov(dataset$number_courses ~dataset$Marks)
  summary(model2)
  
  #F value = 20.67
  
  # The F-statistic value tells us whether the result is good or not.
  #The F ratio is the ratio of two mean square values. 
  #If the null hypothesis is true, you expect F to have a value close to 1.0 most of the time. 
  #A large F ratio means that the variation among group means is more than you'd expect to see by chance. 
  
#9 Linear and Multiple Regression Models:
  
  fit.LR <- lm(dataset$Marks ~dataset$time_study)
  summary(fit.LR)
  
  
  
  fit.MR <- lm(dataset$Marks ~dataset$time_study)
  summary(fit.MR)
  

  cor(x=dataset$Marks , y=dataset$time_study) 
  
  #p-value: 0.00000000000000022
  

  
  anova(fit.LR)
  # The F-statistic value tells us whether the result is good or not.
  #The F ratio is the ratio of two mean square values. 
  #If the null hypothesis is true, you expect F to have a value close to 1.0 most of the time. 
  #A large F ratio means that the variation among group means is more than you'd expect to see by chance.
  #In this case the F value is 775.8 which is extremely large.
  
  
  