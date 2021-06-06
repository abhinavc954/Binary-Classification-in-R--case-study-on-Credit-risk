## 1. Set project workspace folder in computer. 
    setwd("E:\\NMIMS\\Trimester - 3\\Enterprise Systems\\Project")

## 2. Loading package to read in excel input file
    library(xlsx)
    input<-read.xlsx("DataSource.xlsx",1)
  # Creating a copy of input file
    copy<- input

## 3. Looking at data structure and summary statististics of each variable
    str(copy)
    summary(copy)
    attach(copy)
  # Check for NA/null values in summary statistics
  # No null values found, Data is clean
  # Exploring Categorical Variables( available in summary)
    table(Creditability)
    table(Purpose)
    table(Personal_Profile)

## 4. Exploring and creating distribution charts for Numerical Variables
    summary(Duration_months)
  # Standard deviation
    sd(Duration_months) 
    sd(Credit_Amt)
    sd(Age)
    summary(Age)
  # Histograms
    hist(Age)  
    hist(Credit_Amt)
    hist(Duration_months)
  # Boxplots
    library(ggplot2)
    ggplot(data=input, aes(x="Entire Dataset",y=Duration_months))+geom_boxplot()
    ggplot(data=input, aes(x="Entire Dataset",y=Age))+geom_boxplot()
    ggplot(data=input, aes(x="Entire Dataset",y=Credit_Amt))+geom_boxplot()
    
## 5. Creating crosstables to understand attribute relationship with outcome variable
    library(gmodels)
  # Crosstable function gives crosstabel and chi-square value for 2 variables selected.
    CrossTable(Creditability, Account_Balance, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
    CrossTable(Creditability, Credit_History, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
    CrossTable(Creditability, Purpose, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

  # Many categorical vars have several levels, causing 0 values in Cross tables & a warning message
  # while calculating Chi-square values
  # Hence ,REgrouping classes of each categorical variables into shorter groups, 
  #  and recoding into numerical values for modelling purpose.
    require(plyr)
    require(car)
    attach(copy)
    copy$Account_Balance<-revalue(copy$Account_Balance,c("A13"="3","A14"="3","A11"="1","A12"="2")) # some balance
    copy$Credit_History<-revalue(copy$Credit_History,c("A30"="1","A31"="1","A32"="2","A33"="3","A34"="3"))
    copy$Purpose<-recode(copy$Purpose,"c('A46','A43','A44','A45')='3';c('A40','A48','A49','A410')='4';c('A41')='1';c('A42')='2'")
    copy$Savings_Accnt_Bonds<-revalue(copy$Savings_Accnt_Bonds,c("A61"="1","A62"="2","A63"="3","A64"="3","A65"="4"))
    copy$Present_Employment_Since<-revalue(copy$Present_Employment_Since,c("A71"="1","A72"="1","A73"="2","A74"="3","A75"="4"))
    copy$Personal_Profile<-revalue(copy$Personal_Profile,c("A91"="1","A93"="2","A92"="1","A94"="3"))
    copy$Other_Debtors_Gurantors<-revalue(copy$Other_Debtors_Gurantors,c("A101"="1","A102"="2","A103"="2"))
    copy$Property<-revalue(copy$Property,c("A121"="1","A122"="2","A123"="3","A124"="4"))
    copy$Other_Installment_Plans<-revalue(copy$Other_Installment_Plans,c("A141"="1","A142"="1","A143"="2"))
    copy$Housing<-revalue(copy$Housing,c("A151"="1","A152"="2","A153"="3"))
    copy$Job<-revalue(copy$Job,c("A171"="1","A172"="1","A173"="2","A174"="3"))
    copy$Telephone<-revalue(copy$Telephone,c("A191"="1","A192"="2"))
    copy$Foreign_Worker<-revalue(copy$Foreign_Worker,c("A201"="1","A202"="2"))
    copy$Num_of_existing_credits<-revalue(as.factor(copy$Num_of_existing_credits),c("1"="1","2"="2","3"="2","4"="2"))
  # No change in remaining 3 categorical variables

  # Creating crosstables again to verify.
    CrossTable(copy$Creditability, copy$Account_Balance, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
    CrossTable(copy$Creditability, copy$Credit_History, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
    CrossTable(copy$Creditability, copy$Purpose, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
    CrossTable(copy$Creditability, copy$Savings_Accnt_Bonds, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
    CrossTable(copy$Creditability, copy$Present_Employment_Since, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
    CrossTable(copy$Creditability, copy$Installment_Rate, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
    CrossTable(copy$Creditability, copy$Personal_Profile, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
    CrossTable(copy$Creditability, copy$Other_Debtors_Gurantors, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
    CrossTable(copy$Creditability, copy$Present_Residence_Since, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)


## Note: We will test 3 models, namely logistic regression, decision tree and random forest
##       for this classification problem.

## 6.  Out of 20 predictor variables, some may be good predictors while some may be bad.
##    Hence we use Chi-square test of independence(for categorical) & t-test(for numerical) for measuring dependence between outcome
##    variable and each predictor.
  #Chi square test of independence between Creditability and nominal predictor variables
  # Identified index of categorical varibles and capturing chisq p-value via a for loop
    l<-c(1,3,4,6,7,8,9,10,11,12,14,15,16,17,18,19,20)
    c<-NA
    for(i in l)
    {
      a<-(chisq.test(table(copy[,21],copy[,i])))$p.value
      c<-rbind(a,c)
    }
    
  # T-Test for association between creditability and numerical vars
    t.test(copy$Credit_Amt~copy$Creditability)
    t.test(copy$Age~copy$Creditability)
    t.test(copy$Duration_months~copy$Creditability)

## 7. LOGISTIC REGRESSION MODEL
  
  # splitting into training and test data , 50-50 split
    #set.seed(101)
    #indexes50 = sample(1:nrow(copy), size=0.5*nrow(copy))

    #train50<-copy[indexes50,]
    #test50<-copy[-indexes50,]
  # 7.1. Fitting the model using only the variables found significant in chisq and t test at 10%

    model50 <- glm(Creditability ~ Account_Balance + Duration_months +Credit_Amt+Credit_History
                   +Age+Purpose+Savings_Accnt_Bonds+Present_Employment_Since+Personal_Profile
                   +Property+Other_Installment_Plans+Housing+Foreign_Worker, 
                   family=binomial, data = train50)
    summary(model50) 
    #AIC for model50= 523.51 
  # 7.2. removing variables beyond 10% significance and running again
    model50_1<-glm(Creditability ~ Account_Balance + Duration_months +Credit_Amt+Credit_History
                   +Age+Purpose+Savings_Accnt_Bonds+Present_Employment_Since+Personal_Profile
                   +Other_Installment_Plans, 
                   family=binomial, data = train50)
    summary(model50_1) 
    # AIC for model50_1 = 518.62

  # 7.3. removing vars beyond 5% significance
    model50_2<-glm(Creditability ~ Account_Balance + Duration_months +Credit_Amt+Credit_History
                   +Purpose+Savings_Accnt_Bonds+Present_Employment_Since+Personal_Profile
                   +Other_Installment_Plans, 
                   family=binomial, data = train50)
    summary(model50_2) 
    # AIC = 520.36

  ##** AIC value of model should be minimum.
  ##** Final Model - model50_1 is chosen as it has the lowest AIC value.

  # 7.4. Predicting on test data with 50% threshold
  #Subsetting test data with predictors identified in final model.    
    test50_1<-test50[,c(1,2,5,3,4,6,7,9,14,13)]
    pred<-predict(model50_1,test50_1,type = "response")
  # Assigning 1(credit worthy) to outcomes with probability>=0.5
    pred1<-rep(0,500)
    for(i in 1:500)
    {
      pred1[i]<-ifelse(pred[i]>=0.5,1,0)
    }
    table(pred1)
    # Checking Accuracy via Confusion Matrix
    table(test50$Creditability,pred1)

  # 7.5. Predicting on test data with 75% threshold
    pred2<-rep(0,500)
    for(i in 1:500)
    {
      pred2[i]<-ifelse(pred[i]>=0.75,1,0)
    }
    table(pred2)
    # Confusion Matrix
    table(test50$Creditability,pred2)
    #ROCR Curve
    library(ROCR)
    ROCRpred <- prediction(pred1, test50$Creditability)
    ROCRperf <- performance(ROCRpred, 'tpr','fpr')
    plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))


## 8. DECISION TREE MODEL-

# 8.1 CART tree based on Gini Index
    library(rpart)
    fit<-rpart(Creditability ~ Account_Balance+Duration_months+Credit_History
              +Purpose+Credit_Amt+Savings_Accnt_Bonds+Present_Employment_Since+Installment_Rate
              +Personal_Profile+Other_Debtors_Gurantors+Present_Residence_Since+Property
              +Age+Num_of_existing_credits+Housing+Other_Installment_Plans+Job
              +Num_of_people_being_liable_to_provide_maintenance_for+Telephone+Foreign_Worker, data=train50, method="class")
    
    summary(fit)
  # Noting down variable importance
    fit$variable.importance
  # Plotting the tree
    library(rpart.plot)
    rpart.plot(fit,type=4, extra=101,box.palette="GnBu",branch.lty=3,shadow.col="gray",cex=0.46)

  # Prediction on test dataset
    dtpred<-predict(fit,test50,type="class")
  # Confusion Matrix at 50% threshold
    table(dtpred,as.factor(test50$Creditability))
    
# 8.2.Pruning the tree


    fit1<-prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
    library(rpart.plot)
    rpart.plot(fit,type=4, extra=101,box.palette="GnBu",branch.lty=3,shadow.col="gray",cex=0.46)
    
    # Prediction on test dataset
    dtpred1<-predict(fit1,test50,type="class")
    # Confusion Matrix
    table(dtpred1,as.factor(test50$Creditability))
    
    
# 8.3. Using C5.0 package- information gain(entropy)
    library(C50)
    train50_1<-train50
    train50_1$Creditability<-as.factor(train50_1$Creditability)
    tree_mod <- C5.0(x = train50_1[,1:20], y = train50_1$Creditability,trials=10) # boosting
    tree_mod
    summary(tree_mod)
    # Prediction on test data
    cpred<-predict(tree_mod,newdata=test50[,1:20],type = "class")
    #Confusion matrix
    table(cpred,test50$Creditability)
    

## 9. RANDOM FOREST-

    library(randomForest)
    rf50 <- randomForest(Creditability ~., data = train50_1, ntree=200, importance=T, proximity=T)
    #Error Plot
    plot(rf50, main="")
    rf50
    #Prediction om test set
    Test50_rf_pred <- predict(rf50, test50, type="class")
    table(Test50_rf_pred, test50$Creditability)
    # Variable Importance
    importance(rf50)
    varImpPlot(rf50,  main="", cex=0.8)



