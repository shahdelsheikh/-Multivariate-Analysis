# Multivariate Analysis (COVID-19 Analytics) using R.
## Introduction
The COVID-19 pandemic has posed significant challenges to mental well-being due to prolonged periods of lockdown and isolation. This project aims to investigate the protective influence of character strengths on mental health and self-efficacy during lockdowns. Drawing from a previous study, we conducted an in-depth analysis using a multivariate approach to gain a deeper understanding of the interplay between character strengths and mental health outcomes. We have 849 observations.

## Methodology
The data preparation step involved removing a single missing value. We employed various statistical techniques, including factor analysis, cluster analysis, discriminant analysis, and multivariate regression analysis. 
### Factor Analysis 
It was performed to uncover the latent structure within the dataset, determining the number of factors to retain, estimating factor loadings, and conducting factor rotation. We decided that we have 4 factors namely (Openness, Transcendence, Interpersonal, and Restraint).
### Cluster Analysis 
Cluster analysis was utilized to identify patterns among individuals with different strengths. By setting the k-means algorithm to initiate with 25 random starts, we indicated that utilizing 2 clusters only. By using the mean of the clusters, we reached that there exists 2 clusters namely (Character Strengths Emphasized and Character Strengths Deemphasized.
### Discriminant Analysis 
Discriminant Analysis was used as a validation technique for the results of the cluster analysis and we examined assumptions such as normality, equal covariances, and equality of means to determine whether to apply linear or quadratic discriminant analysis. After checking the conditions, quadratic discriminant analysis was chosen as the suitable one. Afterwards, we splitted the data into train and test and constructed a classification table to determine the percantages of correct classification for both Character Strengths Emphasized and Character Strengths Deemphasized.
### Multivariate Regression Analysis
It explored the relationships between character strengths and mental health outcomes.
