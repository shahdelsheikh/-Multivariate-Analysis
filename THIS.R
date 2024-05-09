library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
library(corrplot) #plotting correlation matrices
library(GPArotation) #methods for factor rotation
library(nFactors)  #methods for determining the number of factors
#### Data prepraration stage #############################
mvsample<- read.csv("mv_sample_final.csv")
#Removing Missing Values
head(mvsample)
unique(mvsample$Student)

sapply(mvsample, unique)

#Visualizing categorical variables
mvsample$Gender <- as.factor(mvsample$Gender)
barplot(table(mvsample$Gender), main = "Gender Distribution", col = 'lightblue')

mvsample$Student <- as.factor(mvsample$Student)
barplot(table(mvsample$Student), main = "Student Distribution", col = 'pink')

library(dplyr)

#recoding the variable 
mvsample$Gender <- as.factor(mvsample$Gender)
mvsample$Student <- as.factor(mvsample$Student)


#Standardiznig the variables that will be included only
Data_1 <- mvsample[, sapply(mvsample, is.numeric)]
Data_2 <- mvsample[, -c(1:14,39:41)]

Data_Standardized <- scale(Data_2)
summary(Data_Standardized)

#########################################################################################
###############FACTOR ANALYSIS#################

#Determing number of factors 
#parallel analysis for number of factors 
fa.parallel(cor(Data_Standardized), fm="miners", fa="fa")
#Parrallel Analysis Suggested that number of factors is equal 4
#Using scree plot
pca_result<-principal(Data_Standardized)$values
plot(principal(Data_Standardized)$values, type = "b", col="darkred", pch="*", ylab = 
       "Eigenvalues", main = "Scree Plot of Eigenvalues")
principal(Data_Standardized)

#We'll move forward with the analysis having 4 factors using principal factor method
factor_model <- fa(Data_Standardized, nfactors = 4, rotate = "none")
factor_model$loadings
factm<-as.data.frame(unclass(factor_model$loadings))
factm
#factor analysis using principal component method 
pc <- principal(Data_Standardized, nfactors = 4, rotate = 'none')
pc$loadings
pc$values  ##eigen values
pc$uniquenesses
pc.ob <- principal(r =Data_Standardized, nfactors = 4, rotate = "oblimin")  #oblimin rotation
pc.ob$uniquenesses
pc.ob$communality
#loadings overlap so we'll apply rotation

#with rotation
factor_rotation <- fa(Data_Standardized, nfactors = 4,rotate = "promax")
factor_rotation$loadings
#rotation with varimax is the easier to be interpreted so we'll most probably use it
factor_rotation_var <- fa(Data_Standardized, nfactors = 4,rotate = "varimax")
factor_rotation_var$loadings
f1<-as.data.frame(unclass(factor_rotation_var$loadings))
rotated_factors <- factor_rotation_var$rotated.fa

correlation_matrix <- cor(rotated_factors)  #it didn't run
loadings_matrix <- f1
colnames(loadings_matrix)= c( "Openness","Transendence", "Interpersonel", "Restraint")

dim(loadings_matrix)
loadings_matrix <- as.matrix(loadings_matrix)


factor_scores <- Data_Standardized %*% loadings_matrix
factor <- as.data.frame(factor_scores)
#communalities
l2= f1^2
h2= data.frame(apply(l2,1,sum))
colnames(h2) = c("Communalities")
h2
colnames(factor_rotation_var$scores)= c( "Openness","Transendence", "Interpersonel", "Restraint")
factors = data.frame(factor_rotation_var$scores)

factors
data.frame(cor(factors))
#uncorrelated atwaka3 bas recheck
#La ta3leq hasebloko ento el ta3leq 


#data validation  
#one way is to divide the data into two parts and compare the results
library(caret)
library(caTools)

set.seed(123)
sample <- sample(nrow(Data_Standardized), nrow(Data_Standardized)*0.7)
train <- Data_Standardized[sample, ]
test <- Data_Standardized[-sample, ]

train_factor_rotation_var <- fa(train, nfactors = 4, rotate = "varimax")

# Step 4: Apply Factor Loadings to the Test Set
loadings_matrix <- as.data.frame(unclass(train_factor_rotation_var$loadings))
colnames(loadings_matrix) <- c("Openness", "Transcendence", "Interpersonal", "Restraint")

test <- as.matrix(test)
test[, colnames(loadings_matrix)] <- lapply(test[, colnames(loadings_matrix)], as.numeric)

# Apply Factor Loadings to the Test Set
test_factor_scores <- test %*% loadings_matrix

# Communalities for the Test Set
h2_test <- rowSums(loadings_matrix^2)
colnames(h2_test) <- c("Communalities")

# Display test factor scores and communalities
print(test_factor_scores)
print(data.frame(h2_test))

test_factor_scores <- test %*% loadings_matrix

# Communalities for the Test Set
h2_test <- rowSums(loadings_matrix^2)
colnames(h2_test) <- c("Communalities")

# Display test factor scores and communalities
print(test_factor_scores)
print(data.frame(h2_test))
###########################################################
#########CLUSTER ANALYSIS######################
library(stats)
library(factoextra)
library(cluster)
#recheck
pca= prcomp(mvsample[,-c(1:14,39:41)], scale= TRUE)
barplot(pca$x[1:40,1])
#the plot suggests we have either 2 groups (one positive and the other negative) OR that we have 4 groups (1 positive, 1 negative, 1 low negative, 1 low positive)
#we'll try both 

###################RECHECK benhot eh fl argument bta3 data= f fviz 3shan kolhom shaklhom wehsh 

#we'll start by having 2 clusters
set.seed(123)
clusters_2<- kmeans(factors,centers=2, nstart= 25)
means= clusters_2$centers
t(means)

fviz_cluster(clusters_2, data =factors)
cluster_assignments <- clusters_2$cluster
cluster_counts <- table(cluster_assignments)



# 3 clusters
set.seed(123)
clusters_3<- kmeans(factors,centers=3, nstart = 25)
means= clusters_3$centers #cluster center
t(means) #transpose the cluster center
fviz_cluster(clusters_3, data = factors)
#There are either 3 or 2 clusters bas msh 4 khalessss
####################################################
###################DISCRIMINANT ANALYSIS##########################
#we'll consider having 2 clusters first
#Split the data into train and test
library(caret)
set.seed(123)
clusters <- clusters_2$cluster
factor$cluster <- clusters
factor$cluster <- as.numeric(factor$cluster)

#We need to check the three assumptions 
#check normality assumption for the three clusters 
library(MVN)
mvn(data = factors[,1:4],mvnTest = 'mardia')
factors
#the results show that MVN assumption is not satisfied w bardo beywarena el univariate normality la'ena ely fehom moshkla homa Interpersonal and Openness
group1 <- as.data.frame(factors[factor$cluster==1,1:4])
group2 <- as.data.frame(factors[factor$cluster==2,1:4])

mvn(data = group1[,c(1,2,3,4)],mvnTest = 'mardia') 
mvn(data = group2[,c(1,2,3,4)],mvnTest = 'mardia')
#bardo MVN not satisfied  but since n is large we can assume that we have approx MVN using CLT 

#covs are equal assumption
library(candisc)
factors = data.frame(factors,clusters_2$cluster)
varcovequal <- BoxM(data = factors[, c(1:4)], group = factors$clusters_2.cluster)
summary(varcovequal) #pvalue small fa reject covariance kman haha 

#check equality of means 
#3andna el covs not equal w unknown fa nestakhdm hotelling
means<-TwoSamplesHT2(factors[,c(1:4)], factors$clusters_2.cluster)$p.value
summary(means)
#p value is very small REJECT H0 fa kda el assumption satisfied

# we'll perform quadratic discriminant analysis since the covs are not equal 
#first we'll split the data into train and test
split_index <- createDataPartition(factor$cluster, p = 0.7, list = FALSE) #70% training
train_data <- factors[split_index, ]
test_data <- factors[-split_index, ]

library(MASS)
library(caret)
factor_3 <- factor(factors$clusters_2.cluster)
class(factors$clusters_2.cluster)

qda_model <- qda(clusters_2.cluster ~., data = train_data)

predicted <- predict(qda_model, newdata = test_data[, -5])
xtabs(~predicted$class + test_data$clusters_2.cluster)



###########################################################################
##################MULTIVARIATE REGRESSION##################
#Load necessary packages
library('devtools')
library('sjPlot')
library(MVN)
library(car)

#Data preparations
str(mvsample)
is.factor(mvsample$Gender) #TRUE

mvsample$Student <- as.factor(mvsample$Student)
mvsample$Sons <- as.factor(mvsample$Sons)

#Model 1
mva <- lm(cbind(DASS_21,GHQ_12,SEC)~Age+Gender+Work+Student
              +Sons+Openness+Restraint+Transcendence+Interpersonal, data=mvsample)
summary(mva)

## residuals for Ys
head(resid(mva))

## fitted values for Ys
head(fitted(mva))

## residual standard error
sigma(mva)
library(biotools)

## Anova for multivariate model
anova(mva)

#TYPE 2 ANOVA
Anova(mva)

model <- manova(cbind(DASS_21,GHQ_12,SEC)~Age+Gender+Work+Student
        +Sons+Openness+Restraint+Transcendence+Interpersonal, data=mvsample)
summary(model)
vcov(model)
mvn(residuals(mva))


#Model 2
mva2 <- update(mva, . ~ . - Student)
summary(mva2)
anova(mva2)
mvn(rstandard(mva2), mvnTest = "hz", univariatePlot = "histogram")

#Model 3
mva3 <- lm(cbind(log(DASS_21_1),GHQ_12,SEC)~Age+Gender+Work
           +Openness+Restraint+Transcendence, data=mvsample)
Anova(mva3)
mvn(rstandard(mva3), mvnTest = "hz", univariatePlot = "histogram")

#Testing multivariate normal assumption
mv= mvn(mvsample[,c("DASS_21", "GHQ_12", "SEC")], mvnTest= "mardia", univariatePlot="histogram") #DASS_21 is skewed so it needs a transformation
mv$multivariateNormality#pvalue is small so we reject MVN assumption
mv$univariateNormality
formattable::formattable(as.data.frame.matrix(mv$multivariateNormality), align = c("l"))
formattable::formattable(as.data.frame.matrix(mv$univariateNormality), align = c("l"))
formattable::formattable(as.data.frame.matrix(mv$Descriptives), align = c("l"))

#QQ plot
qqnorm(cbind(mvsample$DASS_21, mvsample$GHQ_12, mvsample$SEC))

#For standardized residuals
qqnorm(rstandard(mva))
qqline(rstandard(mva),lty=2 , col="red")
mvn(rstandard(mva), mvnTest = "hz")

#Normality of residuals
residuals_mva <- residuals(mva)
mvn(residuals_mva, mvnTest = "hz")
qqPlot(residuals_mva, main = "Q-Q Plot of Residuals")

###############################################################################
#we'll use the log fn to transform DASS_21
#Add constant, doesn't change dist (linear fn) & transforms zeros 
mvsample$DASS_21_1 = mvsample$DASS_21 +1 
colnames(mvsample)
mlm = lm(cbind(log(DASS_21_1),GHQ_12,SEC)~Age+Gender+Work+Student
         +Sons+Openness+Restraint+Transcendence+Interpersonal, data=mvsample)
summary(mlm)
anova(mlm)
Anova(mlm)

#Testing multivariate normal assumption
mvm= mvn(cbind(log(mvsample$DASS_21_1),mvsample$GHQ_12,mvsample$SEC), mvnTest= "hz", univariatePlot="histogram")
mvm$multivariateNormality
mvm$univariateNormality

#QQ Plot
qqnorm(cbind(log(mvsample$DASS_21_1), mvsample$GHQ_12, mvsample$SEC))
qqnorm(cbind(1/(mvsample$DASS_21_1), mvsample$GHQ_12, mvsample$SEC))

#For standardized residuals
qqnorm(rstandard(mlm))
qqline(rstandard(mlm),lty=2 , col="red")
mvn(rstandard(mlm), mvnTest = "mardia", univariatePlot = "histogram")

#Normality of residuals
residuals_mlm <- residuals(mlm)
mvn(residuals_mlm, mvnTest = "mardia")
################################################################################
#DASS_21 quadratic transformation
quadratic = lm(cbind(I(DASS_21)^2,GHQ_12,SEC)~Age+Gender+Work+Student
         +Sons+Openness+Restraint+Transcendence+Interpersonal, data=mvsample)
summary(quadratic)

#QQ using standardized residuals
qqnorm(rstandard(quadratic))
qqline(rstandard(quadratic),lty=2 , col="red")
mvn(rstandard(quadratic), mvnTest = "mardia", univariatePlot = "histogram")

#QQ
qqnorm(cbind(I(mvsample$DASS_21)^2, mvsample$GHQ_12, mvsample$SEC)) 
################################################################################
#DASS_21_1 inverse transformation
inverted = lm(cbind((1/DASS_21_1),GHQ_12,SEC)~Age+Gender+Work+Student
               +Sons+Openness+Restraint+Transcendence+Interpersonal, data=mvsample)
summary(inverted)
qqnorm(rstandard(inverted))
qqline(rstandard(inverted),lty=2 , col="red")
mvn(rstandard(inverted), mvnTest = "mardia", univariatePlot = "histogram")
mvn(cbind((1/mvsample$DASS_21_1),mvsample$GHQ_12,mvsample$SEC),mvnTest = "mardia", univariatePlot = "histogram")
qqnorm(cbind((1/mvsample$DASS_21_1), mvsample$GHQ_12, mvsample$SEC)) 
################################################################################
#Tansforming x --> Logarithmic transformation
mva_log <- lm(cbind(DASS_21,GHQ_12,SEC)~log(Age)+Gender+log(Work)+ Student
          +Sons+log(Openness)+log(Restraint)+log(Transcendence)+log(Interpersonal), data=mvsample)

#Normality of residuals
residuals_log <- residuals(mva_log)
qqPlot(residuals_log)
mvn(residuals_log, mvnTest = "mardia")

################################################################################
mva_log2 <- lm(cbind(log(DASS_21_1),GHQ_12,SEC)~log(Age)+Gender+log(Work)+ Student
              +Sons+log(Openness)+log(Restraint)+log(Transcendence)+log(Interpersonal), data=mvsample)
#Normality of residuals
residuals_log2 <- residuals(mva_log2)
qqPlot(residuals_log2)
mvn(residuals_log2, mvnTest = "mardia")

