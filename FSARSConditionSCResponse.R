##Louis Zielinski
##louis.zielinski@marquette.edu
##Marquette University
##FSARS Prediction Analyses

##clear your working directory
rm(list=ls())
##packages
library(readxl)
library(cluster)
library(dplyr)
library(tidyverse)
library(haven)
library(ggplot2)
library(e1071)
library(tseries)    
library(forecast)
 


##Files##
data <- read_xlsx("C:/Users/zieli/OneDrive/Desktop/Data Intelligence/Data sheets/FSARS Prediction Data/FSARS-v2-Bayesian Modeling-Final.xlsx")
data <- data %>%
  filter(SCR.Response <= 8)
View(data)

  ##Data is formatted for within subjects to use later
  ##Mean skin conductance response to help decide what is a norm
  data <- data[,-c(4)]
  mean(data[["SCR.Response"]], na.rm = TRUE)
  
  ##Grouping data by ID and removing all IDs that have all 0.000 values
  data <- data %>%
    group_by(ID) %>%
    filter(any(SCR.Response != 0)) %>%
    ungroup()
  
  
  ##FSARS Self-Report Response Scales
  Questionnaires <- read_sav("C:/Users/zieli/OneDrive/Desktop/Data Intelligence/Data sheets/FSARS Prediction Data/FSARS Merged V2 101-269.sav")
  View(Questionnaires)
  ##Keeping columns with validated scales & dropping the rest
  Questionnaires <- Questionnaires[c("ID", "PCL","CAPS5","CTQ","CTQ_EA","IUS","IUS.Pro", "IUS.Inh", "AUDIT","No.EDA", "MINI.MDD.current","MINI.MDD.past","MINI.MDD.recurrent","SSRI", "Age","Gender","Race.di")]
  ##Replacing the NA's with mean of each column
  Questionnaires <- Questionnaires %>%
    mutate(across(where(is.numeric),
      ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  ##Remove values with no EDA data since no comparison can be made
  Questionnaires <- Questionnaires[Questionnaires$No.EDA != 1, ]
  Questionnaires <- Questionnaires %>%
    select(,-`No.EDA`)
  
  
##Similarity measures##
  ##Remove ID from impactng analysis
  Qd <- daisy(select(Questionnaires, -ID), metric = "gower")
  Qd
  ##AGNES
  mResult <- agnes(Qd, method = "ward")
  mResult
  #Plotting (banner plot and dendrogram)
  plot(mResult)
  abline(h = 0.6, lty = 2, lwd = 2)
  mResult
  ##Dotted line at 6 clusters on the banner plot
  par(mfrow = c(2, 1))
  plot(mResult, which.plots = 1)  # banner plot
  abline(v = 0.6, lty = 2, lwd = 2)
  
  
  ##6 Clusters
  mClusters <- cutree(mResult, k = 6)
  ##append the clusters
  Questionnaires <- data.frame(Questionnaires, mClusters)  
  ##see summary statistics and counts
  summary(subset(Questionnaires, mClusters ==1))
  summary(subset(Questionnaires, mClusters ==2))
  summary(subset(Questionnaires, mClusters ==3))
  summary(subset(Questionnaires, mClusters ==4))
  summary(subset(Questionnaires, mClusters ==5))
  summary(subset(Questionnaires, mClusters ==6))
  summary(as.factor(mClusters))
  
  ##Dataframes of each cluster
  cluster1 <- Questionnaires[mClusters == 1, ]
  cluster2 <- Questionnaires[mClusters == 2, ]
  cluster3 <- Questionnaires[mClusters == 3, ]
  cluster4 <- Questionnaires[mClusters == 4, ]
  cluster5 <- Questionnaires[mClusters == 5, ]
  cluster6 <- Questionnaires[mClusters == 6, ]
  
  ##Cluster means
  cluster_means <- Questionnaires %>%
    group_by(mClusters) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)
      ),.groups = "drop")
  cluster_means
  #C1: Medium PTSD, 2nd uncertainty and anxiety, 2nd alc use
  #C2: Safety, Lowest PTSD, 3rd alc use, youngest
  #C3: Medium PTSD, 20% MDD (2nd), MDD past highest, lowest SSRI use, 
  #C4: Least to medium averages. all women
  #C5: Highest PTSD,uncertainty,anxiety, MDD current past recurrent, highest SSRI, smallest sample
  #C6: Med PTSD, 1st in alc use, lowest SSRI use
  
                  ##Could predict cluster of future observations based on response types with the following naive Bayes
                  library(caret)
                  set.seed(123) 
                  train_index <- createDataPartition(Questionnaires$mClusters,  
                    p = 0.7,         # 70% training, 30% testing
                    list = FALSE )
                  training_data <- Questionnaires[train_index, ]
                  test_data  <- Questionnaires[-train_index, ]
                  nb_model <- naiveBayes(mClusters ~ PCL + CAPS5 + CTQ + CTQ_EA + IUS + IUS.Pro + IUS.Inh + AUDIT + MINI.MDD.current + MINI.MDD.past + MINI.MDD.recurrent + SSRI + Age + Gender + Race.di,
                    data = training_data)
                  pred_class <- predict(nb_model, newdata = test_data)
                  accuracy <- mean(pred_class == test_data$mClusters)
                  accuracy
                  
                  

  
###Comparing clusters to patterns###
  
  ##filtering observances from participants in cluster1
  data_cluster1 <- data %>%
    filter(ID %in% cluster1$ID)
  ##Keeping outcome possibilities
  data_cluster1_outcomeN2 <- data_cluster1 %>%
    filter(Outcome == -2)
  data_cluster1_outcomeN1 <- data_cluster1 %>%
    filter(Outcome == -1)
  data_cluster1_outcome0 <- data_cluster1 %>%
    filter(Outcome == 0)
  data_cluster1_outcome1 <- data_cluster1 %>%
    filter(Outcome == 1)
  data_cluster1_outcome2 <- data_cluster1 %>%
    filter(Outcome == 2)
  #trendlines of "SCR.Response" against "CondTrial" for each "Outcome" option in the df
  #leoss for non-linear data
  #fear stimulus for those in cluster 1
  summary(data_cluster1_outcomeN2$SCR.Response)
  ggplot(data_cluster1_outcomeN2, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #fear.safety stimulus for those in cluster 1
  summary(data_cluster1_outcomeN1$SCR.Response)
  ggplot(data_cluster1_outcomeN1, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #safety stimulus for those in cluster 1
  summary(data_cluster1_outcome0$SCR.Response)
  ggplot(data_cluster1_outcome0, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #reward.safety stimulus for those in cluster 1
  summary(data_cluster1_outcome1$SCR.Response)
  ggplot(data_cluster1_outcome1, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #reward stimulus for those in cluster 1
  summary(data_cluster1_outcome2$SCR.Response)
  ggplot(data_cluster1_outcome2, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  
  ##filtering observances from participants in cluster2
  data_cluster2 <- data %>%
    filter(ID %in% cluster2$ID)
  ##Keeping outcome possibilities
  data_cluster2_outcomeN2 <- data_cluster2 %>%
    filter(Outcome == -2)
  data_cluster2_outcomeN1 <- data_cluster2 %>%
    filter(Outcome == -1)
  data_cluster2_outcome0 <- data_cluster2 %>%
    filter(Outcome == 0)
  data_cluster2_outcome1 <- data_cluster2 %>%
    filter(Outcome == 1)
  data_cluster2_outcome2 <- data_cluster2 %>%
    filter(Outcome == 2)
  #trendlines of "SCR.Response" against "CondTrial" for each "Outcome" option in the df
  #fear stimulus for those in cluster 2
  summary(data_cluster2_outcomeN2$SCR.Response)
  ggplot(data_cluster2_outcomeN2, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #fear.safety stimulus for those in cluster 2
  summary(data_cluster2_outcomeN1$SCR.Response)
  ggplot(data_cluster2_outcomeN1, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #safety stimulus for those in cluster 2
  summary(data_cluster2_outcome0$SCR.Response)
  ggplot(data_cluster2_outcome0, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #reward.safety stimulus for those in cluster 2
  summary(data_cluster2_outcome1$SCR.Response)
  ggplot(data_cluster2_outcome1, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #reward stimulus for those in cluster 2
  summary(data_cluster2_outcome2$SCR.Response)
  ggplot(data_cluster2_outcome2, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  
  ##filtering observances from participants in cluster3
  data_cluster3 <- data %>%
    filter(ID %in% cluster3$ID)
  ##Keeping outcome possibilities
  data_cluster3_outcomeN2 <- data_cluster3 %>%
    filter(Outcome == -2)
  data_cluster3_outcomeN1 <- data_cluster3 %>%
    filter(Outcome == -1)
  data_cluster3_outcome0 <- data_cluster3 %>%
    filter(Outcome == 0)
  data_cluster3_outcome1 <- data_cluster3 %>%
    filter(Outcome == 1)
  data_cluster3_outcome2 <- data_cluster3 %>%
    filter(Outcome == 2)
  #trendlines of "SCR.Response" against "CondTrial" for each "Outcome" option in the df
  #fear stimulus for those in cluster 3
  summary(data_cluster3_outcomeN2$SCR.Response)
  ggplot(data_cluster3_outcomeN2, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #fear.safety stimulus for those in cluster 3
  summary(data_cluster3_outcomeN1$SCR.Response)
  ggplot(data_cluster3_outcomeN1, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #safety stimulus for those in cluster 3
  summary(data_cluster3_outcome0$SCR.Response)
  ggplot(data_cluster3_outcome0, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #reward.safety stimulus for those in cluster 3
  summary(data_cluster3_outcome1$SCR.Response)
  ggplot(data_cluster3_outcome1, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #reward stimulus for those in cluster 3
  summary(data_cluster3_outcome2$SCR.Response)
  ggplot(data_cluster3_outcome2, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  
  ##filtering observances from participants in cluster4
  data_cluster4 <- data %>%
    filter(ID %in% cluster4$ID)
  ##Keeping outcome possibilities
  data_cluster4_outcomeN2 <- data_cluster4 %>%
    filter(Outcome == -2)
  data_cluster4_outcomeN1 <- data_cluster4 %>%
    filter(Outcome == -1)
  data_cluster4_outcome0 <- data_cluster4 %>%
    filter(Outcome == 0)
  data_cluster4_outcome1 <- data_cluster4 %>%
    filter(Outcome == 1)
  data_cluster4_outcome2 <- data_cluster4 %>%
    filter(Outcome == 2)
  #trendlines of "SCR.Response" against "CondTrial" for each "Outcome" option in the df
  #fear stimulus for those in cluster 4
  summary(data_cluster4_outcomeN2$SCR.Response)
  ggplot(data_cluster4_outcomeN2, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #fear.safety stimulus for those in cluster 4
  summary(data_cluster4_outcomeN1$SCR.Response)
  ggplot(data_cluster4_outcomeN1, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #safety stimulus for those in cluster 4
  summary(data_cluster4_outcome0$SCR.Response)
  ggplot(data_cluster4_outcome0, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #reward.safety stimulus for those in cluster 4
  summary(data_cluster4_outcome1$SCR.Response)
  ggplot(data_cluster4_outcome1, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #reward stimulus for those in cluster 4
  summary(data_cluster4_outcome2$SCR.Response)
  ggplot(data_cluster4_outcome2, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  
  ##filtering observances from participants in cluster5
  data_cluster5 <- data %>%
    filter(ID %in% cluster5$ID)
  ##Keeping outcome possibilities
  data_cluster5_outcomeN2 <- data_cluster5 %>%
    filter(Outcome == -2)
  data_cluster5_outcomeN1 <- data_cluster5 %>%
    filter(Outcome == -1)
  data_cluster5_outcome0 <- data_cluster5 %>%
    filter(Outcome == 0)
  data_cluster5_outcome1 <- data_cluster5 %>%
    filter(Outcome == 1)
  data_cluster5_outcome2 <- data_cluster5 %>%
    filter(Outcome == 2)
  #trendlines of "SCR.Response" against "CondTrial" for each "Outcome" option in the df
  #fear stimulus for those in cluster 5
  summary(data_cluster5_outcomeN2$SCR.Response)
  ggplot(data_cluster5_outcomeN2, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #fear.safety stimulus for those in cluster 5
  summary(data_cluster5_outcomeN1$SCR.Response)
  ggplot(data_cluster5_outcomeN1, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #safety stimulus for those in cluster 5
  summary(data_cluster5_outcome0$SCR.Response)
  ggplot(data_cluster5_outcome0, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #reward.safety stimulus for those in cluster 5
  summary(data_cluster5_outcome1$SCR.Response)
  ggplot(data_cluster5_outcome1, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #reward stimulus for those in cluster 5
  summary(data_cluster5_outcome2$SCR.Response)
  ggplot(data_cluster5_outcome2, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  
  ##filtering observances from participants in cluster6
  data_cluster6 <- data %>%
    filter(ID %in% cluster6$ID)
  ##Keeping outcome possibilities
  data_cluster6_outcomeN2 <- data_cluster6 %>%
    filter(Outcome == -2)
  data_cluster6_outcomeN1 <- data_cluster6 %>%
    filter(Outcome == -1)
  data_cluster6_outcome0 <- data_cluster6 %>%
    filter(Outcome == 0)
  data_cluster6_outcome1 <- data_cluster6 %>%
    filter(Outcome == 1)
  data_cluster6_outcome2 <- data_cluster6 %>%
    filter(Outcome == 2)
  #trendlines of "SCR.Response" against "CondTrial" for each "Outcome" option in the df
  #fear stimulus for those in cluster 5
  summary(data_cluster6_outcomeN2$SCR.Response)
  ggplot(data_cluster6_outcomeN2, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #fear.safety stimulus for those in cluster 5
  summary(data_cluster6_outcomeN1$SCR.Response)
  ggplot(data_cluster6_outcomeN1, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #safety stimulus for those in cluster 5
  summary(data_cluster6_outcome0$SCR.Response)
  ggplot(data_cluster6_outcome0, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #reward.safety stimulus for those in cluster 5
  summary(data_cluster6_outcome1$SCR.Response)
  ggplot(data_cluster6_outcome1, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  #reward stimulus for those in cluster 5
  summary(data_cluster6_outcome2$SCR.Response)
  ggplot(data_cluster6_outcome2, aes(x = CondTrial, y = SCR.Response)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    theme_minimal()
  
  ##Merge cluster number into a df column
  data <- data %>%
    left_join(Questionnaires %>% select(ID, mClusters),
      by = "ID")
  

####Forecasting####
  ###A time series model of cluster, stimuli presented, trial number with SCR output
  ts_df_outcome <- data %>%
    group_by(mClusters, Outcome, SubTrial) %>%
    summarise( SCR.Response = mean(SCR.Response, na.rm = TRUE),
      .groups = "drop" ) %>%
    filter(SubTrial %in% 1:40)
  
    ##Cluster 1##
  ##Filtering for fear.safety responses of cluster 1
  df_c1_o1 <- ts_df_outcome %>%
    filter(mClusters == 1, Outcome == -1) %>%
    arrange(SubTrial)
  ##Time series and Holt Exponential in model 2 fear.safety response
  scr_ts_c1_o1 <- ts(df_c1_o1$SCR.Response, start = 1, frequency = 1)
  scr_ts_c1_o1 <- log1p(scr_ts_c1_o1)
  holt_c1_o1 <- holt(scr_ts_c1_o1)
  ##Plotting fitted model
  plot(holt_c1_o1,
       main = "Holt Model: Cluster 1, Fear-Safety",
       xlab = "SubTrial",
       ylab = "SCR.Response")
  
      ##Cluster 2##
  ##Filtering for fear.safety responses of cluster 2
  df_c2_o1 <- ts_df_outcome %>%
    filter(mClusters == 2, Outcome == -1) %>%
    arrange(SubTrial)
  ##Time series and Holt Exponential in cluster 2 fear.safety response
  scr_ts_c2_o1 <- ts(df_c2_o1$SCR.Response, start = 1, frequency = 1)
  scr_ts_c2_o1 <- log1p(scr_ts_c2_o1)
  holt_c2_o1 <- holt(scr_ts_c2_o1)
  ##Plotting fitted model
  plot(holt_c2_o1,
       main = "Holt Model: Cluster 2, Fear-Safety",
       xlab = "SubTrial",
       ylab = "SCR.Response")
  
    ##Cluster 3##
  ##Filtering for fear.safety responses of cluster 3
  df_c3_o1 <- ts_df_outcome %>%
    filter(mClusters == 3, Outcome == -1) %>%
    arrange(SubTrial)
  ##Time series and Holt Exponential in cluster 3 fear.safety response
  scr_ts_c3_o1 <- ts(df_c3_o1$SCR.Response, start = 1, frequency = 1)
  scr_ts_c3_o1 <- log1p(scr_ts_c3_o1)
  holt_c3_o1 <- holt(scr_ts_c3_o1)
  ##Plotting fitted model
  plot(holt_c3_o1,
       main = "Holt Model: Cluster 3, Fear-Safety",
       xlab = "SubTrial",
       ylab = "SCR.Response")
  
    ##Cluster 4##
  ##Filtering for fear.safety responses of cluster 4
  df_c4_o1 <- ts_df_outcome %>%
    filter(mClusters == 4, Outcome == -1) %>%
    arrange(SubTrial)
  ##Time series and Holt Exponential in cluster 4 fear.safety response
  scr_ts_c4_o1 <- ts(df_c4_o1$SCR.Response, start = 1, frequency = 1)
  scr_ts_c4_o1 <- log1p(scr_ts_c4_o1)
  holt_c4_o1 <- holt(scr_ts_c4_o1)
  ##Plotting fitted model
  plot(holt_c4_o1,
       main = "Holt Model: Cluster 4, Fear-Safety",
       xlab = "SubTrial",
       ylab = "SCR.Response")
  
    ##Cluster 5##
  ##Filtering for fear.safety responses of cluster 5
  df_c5_o1 <- ts_df_outcome %>%
    filter(mClusters == 5, Outcome == -1) %>%
    arrange(SubTrial)
  ##Time series and Holt Exponential in cluster 5 fear.safety response
  scr_ts_c5_o1 <- ts(df_c5_o1$SCR.Response, start = 1, end = 40, frequency = 1)
  scr_ts_c5_o1 <- log1p(scr_ts_c5_o1)
  holt_c5_o1 <- holt(scr_ts_c5_o1)
  ##Plotting fitted model
  plot(holt_c5_o1,
       main = "Holt Model: Cluster 5, Fear-Safety",
       xlab = "SubTrial",
       ylab = "SCR.Response")
  
  ##Cluster 6##
  ##Filtering for fear.safety responses of cluster 6
  df_c6_o1 <- ts_df_outcome %>%
    filter(mClusters == 6, Outcome == -1) %>%
    arrange(SubTrial)
  ##Time series and Holt Exponential in cluster 6 fear.safety response
  scr_ts_c6_o1 <- ts(df_c6_o1$SCR.Response, start = 1, end = 40, frequency = 1)
  scr_ts_c6_o1 <- log1p(scr_ts_c6_o1)
  holt_c6_o1 <- holt(scr_ts_c6_o1)
  c6_o1_forecast <- forecast(holt_c6_o1, h = 10)
  ##Plotting fitted model
  plot(holt_c6_o1,
       main = "Holt Model: Cluster 6, Fear-Safety",
       xlab = "SubTrial",
       ylab = "SCR.Response")
  
  ##Holt Models for all cluster and outcomes
  holt_models <- data %>%
    group_by(mClusters, Outcome, SubTrial) %>%
    summarise( SCR.Response = mean(SCR.Response, na.rm = TRUE),
      .groups = "drop") %>%
    split(list(.$mClusters, .$Outcome)) %>%
    lapply(function(df) {
      ts_obj <- ts(df$SCR.Response, start = 1, frequency = 1)
      holt(ts_obj)})
  holt_models[["1.1"]]   # cluster 1, outcome -1
  holt_models[["2.1"]]   # cluster 2, outcome -1
  holt_models[["3.1"]]   # cluster 3, outcome -1
  holt_models[["4.1"]]   # cluster 4, outcome -1
  holt_models[["5.1"]]   # cluster 5, outcome -1
  holt_models[["6.1"]]   # cluster 6, outcome -1
  ##Training and testing
  df <- data %>%
    filter(mClusters == 6, Outcome == -1) %>%
    group_by(SubTrial) %>%
    summarise(SCR.Response = mean(SCR.Response, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(SubTrial)
  n <- nrow(df)
  train_ts <- ts(log1p(df$SCR.Response[1:floor(n * 0.75)]), start = 1)
  test_ts  <- ts(log1p(df$SCR.Response[(floor(n * 0.75)+1):n]), start = floor(n * 0.75)+1)
  fit <- holt(train_ts)
  fc  <- forecast(fit, h = length(test_ts))
  ##Accuracy
  accuracy(fc, test_ts)
  
  
  
  
  
  
  
  
  
#######Bayesian Hierarchical Time‑Series##########(da bwead and bwutta)
  ##SCR log transformed
  data_bayes <- data %>%
    filter(SubTrial >= 1, SubTrial <= 40) %>%
    mutate( SCR_log = log1p(SCR.Response),
      Outcome = factor(Outcome),
      cluster = factor(mClusters),
      ID = factor(ID))

  
  ##Hierarchical time series model with autocorrelation across trials
  library(brms)
  model_bayes_ar <- brm(
    SCR_log ~ SubTrial * Outcome * cluster +
      (1 + SubTrial | ID),
    data = data_bayes,
    family = gaussian(),
    autocor = cor_ar(form = ~ SubTrial | ID, p = 1),
    chains = 4,
    cores = 4,
    iter = 4000,
    control = list(adapt_delta = 0.95) )
  summary(model_bayes_ar)
  View(model_bayes_ar)
  ##Posterior predictive check
  pp_check(model_bayes_ar)
  ##Outcome Specific Slope
  fixef(model_bayes_ar)
  parnames(model_bayes_ar)
  library(tidybayes)
  slopes <- model_bayes_ar %>%
    gather_draws( b_SubTrial,
      `b_SubTrial:OutcomeM1`,
      `b_SubTrial:Outcome0`,
      `b_SubTrial:Outcome1`,
      `b_SubTrial:Outcome2`) %>%
    mutate(Outcome = case_when(
        .variable == "b_SubTrial" ~ "-2",
        .variable == "b_SubTrial:Outcome-1" ~ "-1",
        .variable == "b_SubTrial:Outcome0"  ~ "0",
        .variable == "b_SubTrial:Outcome1"  ~ "1",
        .variable == "b_SubTrial:Outcome2"  ~ "2"))
    slopes
    ##learning slopes by Stimulus type
    slopes_summary <- slopes %>%
      group_by(Outcome) %>%
      summarise(mean_slope = mean(.value),
        lower = quantile(.value, 0.025),
        upper = quantile(.value, 0.975),
        .groups = "drop" )
    
    ##Plot learning rates
    ggplot(slopes_summary, aes(x = Outcome, y = mean_slope)) +
      geom_point(size = 3) +
      geom_errorbar(
        aes(ymin = lower, ymax = upper),
        width = 0.15) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      theme_minimal() + labs( x = "Outcome",
        y = "Learning slope (SubTrial)",
        title = "Outcome-Specific learning rates (posterior means ± 95% CI)" )
    
  ##Analyze by Cluster
  


##see summary statistics and counts
summary(subset(data, mClusters ==1))
summary(subset(data, mClusters ==2))
summary(subset(data, mClusters ==3))
summary(subset(data, mClusters ==4))
summary(subset(data, mClusters ==5))
summary(subset(data, mClusters ==6))
summary(as.factor(mClusters))
