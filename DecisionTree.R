library(rpart)
library(caret)
library(rpart.plot)
library(dplyr)
library(xlsx)
library(data.tree)
library(caTools)
library(ElemStatLearn)
library(ggplot2)
library(xgboost)

#Read the xlsx
df <- read.xlsx("OrderVol_Prediction.xlsx",1,header= TRUE)


#column drops
drops <- c("Australian.Dollar..t.2.")
df <- df[, ! names(df) %in% drops, drop = F]



#Add the other lags
df$Brasil.Government.Expenditure..t.1.<-lag(df$Brasil.Government.Expenditure..t.0.,1)
df$Brasil.Government.Expenditure..t.1.[1] <- 80
df$Brasil.Government.Expenditure..t.3.<-lag(df$Brasil.Government.Expenditure..t.2.,1)
df$Brasil.Government.Expenditure..t.3.[1]<-70

df$China.Government.Expenditure..t.1.<- lead(df$China.Government.Expenditure..t.2.)
df$China.Government.Expenditure..t.0.<- lead(df$China.Government.Expenditure..t.1.)
df$China.Government.Expenditure..t.4.<- lag(df$China.Government.Expenditure..t.3.,1)
df$China.Government.Expenditure..t.4.[1]<-1168

df$Russia.Government.Expenditure..t.1.<- lead(df$Russia.Government.Expenditure..t.2.)
df$Russia.Government.Expenditure..t.0.<- lead(df$Russia.Government.Expenditure..t.1.)

df$India.Government.Expenditure..t.1.<- lead(df$India.Government.Expenditure..t.2.)
df$India.Government.Expenditure..t.0.<- lead(df$India.Government.Expenditure..t.1.)

df$South.Korea.Industrial.Production..t.1.<- lead(df$South.Korea.Industrial.Production..t.2.)
df$South.Korea.Industrial.Production..t.0.<- lead(df$South.Korea.Industrial.Production..t.1.)


lagdrops <- c("Brasil.Government.Expenditure..t.1.","Brasil.Government.Expenditure..t.2.", "Brasil.Government.Expenditure..t.3.",
                        "Brasil.Government.Expenditure..t.4.",
                        "Brasil.Industrial.Production..t.1.",  
                        "China.Government.Expenditure..t.0.", 
                        "China.Government.Expenditure..t.1.", 
                         "China.Government.Expenditure..t.5.",
                        "China.Government.Expenditure..t.3.",
                        "India.Government.Expenditure..t.0.",
                        "India.Government.Expenditure..t.2.",
                        "India.Government.Expenditure..t.3.",
                        "Russia.Government.Expenditure..t.0.",
                        "Russia.Government.Expenditure..t.2.",
                        "Russia.Government.Expenditure..t.3.",
                        "South.Korea.Industrial.Production..t.0.",
                        "South.Korea.Industrial.Production..t.2.", "Swedish.Unemployment.rate.Index...t.0.6 ")


df <- df[, ! names(df) %in% lagdrops, drop = F]



#Variance Check #no need for now

# apply(df, 2, var)  
# apply(df, 1, var)  

#MissRatio

MissRatio <- function(x){
    sum(is.na(x))/length(x)*100
}

apply(df, 2, FUN=MissRatio) #column based missing ratio
apply(df, 1, FUN=MissRatio) # rowbased missing ratio

#Check data classes

as.data.frame(sapply(df,class))
df$South.Korea.Industrial.Production..t.1.= as.factor(df$South.Korea.Industrial.Production..t.1.)
df$Applied.Ind.Tech.Capex..t.5.=as.factor(df$Applied.Ind.Tech.Capex..t.5.)
df$Swedish.Unemployment.rate.Index...t.0.=as.factor(df$Swedish.Unemployment.rate.Index...t.0.)
dfshort<- df[c(1:53),]


#Train and Test set
set.seed(123)
ind<- sample(2, nrow(dfshort), replace=TRUE, prob=c(0.8, 0.2))
train<- dfshort[ind==1,]
test<- dfshort[ind==2,]

    
    
    
#######################    
#ANALYSIS WITH ALL DATA#
########################

#CART Regression Tree
# Train and test tree 
    tree_g <- rpart(Order.Volume ~ ., train, method = "anova")
    pred_g <- predict(tree_g, test, type = "vector")
    conf_g <- table(test$Order.Volume, pred_g)
    acc_g <- sum(diag(conf_g)) / sum(conf_g)

    library(rattle)
    fancyRpartPlot(tree_g) 

    print(tree_g)


    
    
    
    #Check the complexity parameters
    printcp(tree_g)
    tree_g$variable.importance
    
  

    #Tuning http://topepo.github.io/caret/train-models-by-tag.html
    
    ControlMethodcart <- trainControl(method = "cv", number = 2) # we will do 2 Cross validation
    
    ParameterGridCart <- expand.grid(cp=0.01)
    
    OptimumModelCart1 = caret::train(Order.Volume ~ .,
                                     data = train,
                                     method = "rpart",
                                     trControl = ControlMethodcart,
                                     tuneGrid = ParameterGridCart)
    
    print(OptimumModelCart1)
    
    ParameterGridCart <- expand.grid(cp=0.0)
    
    OptimumModelCart2 = caret::train(Order.Volume ~ .,
                                     data = train,
                                     method = "rpart",
                                     trControl = ControlMethodcart,
                                     tuneGrid = ParameterGridCart)
    
    print(OptimumModelCart2)
    importance <- varImp(OptimumModelCart2)
    
    MAE(test$Order.Volume, pred_tunedtree )
    MSE(test$Order.Volume, pred_tunedtree )
    RMSE(test$Order.Volume, pred_tunedtree )
    

    
    prunedtree<-prune(tree_g, cp=0.21155)
    pred_prunedtree <- predict(prunedtree, test, type = "vector")
    conf_prunedtree <- table(test$Order.Volume, pred_prunedtree)
    acc_prunedtree <- sum(diag(conf_prunedtree)) / sum(conf_prunedtree)
    acc_prunedtree
    
    fancyRpartPlot(prunedtree)
    fancyRpartPlot(tree_g_tuned)
    
    #Tunning tries
    tree_g_tuned <- rpart(Order.Volume ~ ., train, method = "anova", control = rpart.control(minsplit = 21, minbucket=7, cp=0.01))
    pred_tunedtree <- predict(tree_g_tuned, test, type = "vector")
    conf_tuned <- table(test$Order.Volume, pred_tunedtree)
    acc_tuned <- sum(diag(conf_tuned)) / sum(conf_tuned)
    acc_tuned
    
    
    tree_g_tuned$variable.importance
    
    #Error check
    
    MAE<- function(actual,predicted) {
        mean(abs(actual - predicted))
    }
    
    
    
    MSE<- function(actual, predicted){
        mean((actual - predicted))^2
    }
    
    
    
    RMSE = function(actual, predicted){
        sqrt(mean((predicted - actual)^2))
    }
    MAE(test$Order.Volume, pred_prunedtree )
    MSE(test$Order.Volume, pred_prunedtree )
    RMSE(test$Order.Volume, pred_prunedtree )
    
    
    MAE(test$Order.Volume, pred_g )
    MSE(test$Order.Volume, pred_g )
    RMSE(test$Order.Volume, pred_g )
    
    MAE(test$Order.Volume, pred_tunedtree )
    MSE(test$Order.Volume, pred_tunedtree )
    RMSE(test$Order.Volume, pred_tunedtree )
    MAPE<- function(actual, predicted) {
        mean(abs((actual-predicted)/actual) * 100)}
    
    MAPE(test$Order.Volume, pred_tunedtree)
    

    
    rpart.control(minsplit = 20, minbucket = round(7), cp = 0.01,
                  maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                  surrogatestyle = 0, maxdepth = 30)
    
    
    
    #Prune the tree https://www.youtube.com/watch?v=LziT4fJDB4I
    library(tree)
    
    tree_model<- tree(Order.Volume~., train)
    tree_model
    plot(tree_model)
    text(tree_model,pretty=0)
    
    cvs<-cv.tree(tree_model)
    plot(cvs$size, cvs$dev, type="b")
    which.min(cvs$dev)
    cvs$size[6]
    
    pruned_model<- prune.tree(tree_model, best=4)
    
    text(tree_model,pretty=0)
    
    
    #XGBOOST tree
    
    #Check Balance
    dfshort %>%  group_by(Order.Volume) %>% summarise(number=n())
    hist(dfshort$Order.Volume)
    
    # 
    # #xgboost
    # library(xgboost)
    # data(train, package= 'xgboost')
    # data(dfshort.test, package= 'xgboost')
    # train <- dfshort.train
    # test <- dfshort.test
    # bst <- xgboost(data = train$data, label = train$label, max_depth = 2, eta = 1,
    #                nrounds = 2, objective = "binary:logistic")
    # 
    #Tunning
    set.seed(123)
    library(caret)
    
    ControlMethod <- trainControl(method = "cv", number = 5) # we will do 5 Cross validation
    
    ParameterGrid1 <- expand.grid(eta = 0.1,
                                 nrounds = seq(10, 200, 20),
                                 max_depth = 5,
                                 min_child_weight = 1,
                                 gamma = 0,
                                 subsample = 0.8,
                                 colsample_bytree = 0.8)
    
    OptimumModel1 = caret::train(Order.Volume ~ .,
                                 data = train,
                                 method = "xgbTree",
                                 trControl = ControlMethod,
                                 tuneGrid = ParameterGrid1)
    
    print(OptimumModel1)
    
    
    
    ParameterGrid2 <- expand.grid(eta = 0.1,
                                 nrounds = 190,
                                 max_depth = seq(1, 10, 2),
                                 min_child_weight = seq(1, 5, 2),
                                 gamma = 0,
                                 subsample = 0.8,
                                 colsample_bytree = 0.8)
    
    OptimumModel2 <- caret::train(Order.Volume ~ .,
                                 data = train,
                                 method = "xgbTree",
                                 trControl = ControlMethod,
                                 tuneGrid = ParameterGrid2)
    
    print(OptimumModel2)
    
    
    ParameterGrid3 <- expand.grid(eta = 0.1,
                                 nrounds = 190,
                                 max_depth = 7,
                                 min_child_weight = 5,
                                 gamma = seq(0.1, 0.5, 0.1),
                                 subsample = 0.8,
                                 colsample_bytree = 0.8)
    
    OptimumModel3 <- caret::train(Order.Volume ~ .,
                                 data = train,
                                 method = "xgbTree",
                                 trControl = ControlMethod,
                                 tuneGrid = ParameterGrid3)
    
    print(OptimumModel3)
    
    ParameterGrid4 = expand.grid(eta = 0.1,
                                 nrounds = 190,
                                 max_depth = 7,
                                 min_child_weight = 5,
                                 gamma = 0.1,
                                 subsample = seq(0.1, 1, 0.2),
                                 colsample_bytree = seq(0.1, 1, 0.2))
    
    OptimumModel4 = caret::train(Order.Volume ~ .,
                                 data = train,
                                 method = "xgbTree",
                                 trControl = ControlMethod,
                                 tuneGrid = ParameterGrid4)
    
    
    print(OptimumModel4)
    
    ParameterGrid5 = expand.grid(eta = c(0.1, 0.05, 0.01),
                                 nrounds = c(200, 500, 800, 1100),
                                 max_depth = 7,
                                 min_child_weight = 5,
                                 gamma = 0.1,
                                 subsample = 0.7,
                                 colsample_bytree = 0.1)
    
    OptimumModel5 = caret::train(Order.Volume ~ .,
                                 data = train,
                                 method = "xgbTree",
                                 trControl = ControlMethod,
                                 tuneGrid = ParameterGrid5)
    
    print(OptimumModel5)
    
    
    modelxgboost <- xgboost(data=train$data, label)
 
    
    plot(OptimumModel5)
    
    
    #importance
    importance <- varImp(OptimumModel5)
    
    train<-matrix(train)
    
    #Plot
    xgb.plot.tree(OptimumModel5)
    
    xgboost(data = train$data, label=train$Order.Volume, nrounds = 200,
            max_depth = 7,
            min_child_weight = 5,
            gamma = 0.1,
            subsample = 0.9,
            colsample_bytree = 0.1, objective = "binary:logistic")
    
    xgb.plot.tree(model=OptimumModel5)
    
    xgb.plot.multi.trees(model = OptimumModel5)
    
    rpart.plot(OptimumModel5, type=3, digits=3, fallen.leaves = TRUE)
    
    
    #TEST
    Pxgboost <- predict(OptimumModel5,newdata = test)
    
    
   
    MAE(test$Order.Volume, Pxgboost)
    
    
    
    
    #CART
    m2 <- rpart(Order.Volume~., data = dfm2, method='anova')
    rpart.plot(m2, type=3, digits=3, fallen.leaves = TRUE)
    
    m3 <- rpart(Order.Volume~., data= dfshort, method = 'anova')
    
    #importance check
    
    m2$variable.importance
    m3$variable.importance
    
    #prediction
    p1 <- predict(m2,dfm2[c(54:60),])
    p2 <- predict(m2,dfm2[c(1:53),])
    
    
    #Error check
    printcp(m2)
    printcp(m3)
    MAE<- function(actual,predicted) {
        mean(abs(actual - predicted))
    }
    
    MAE(tests$Order.Volume, p2)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
#Random forest for all data
    
    set.seed(123)
    ind<- sample(2, nrow(df), replace=TRUE, prob=c(0.8, 0.2))
    train<- df[ind==1,]
    test<- df[ind==2,]
    
    library(randomForest)
    set.seed(222)
    
    model<- randomForest(Order.Volume~., data=train, ntree=300, 
                         mtry=8,
                         importance=TRUE,
                         proximity=TRUE,
                         na.action=na.exclude)
    print(model)
    plot(model)
    
    #Prediction in RF
    library(caret)
    p1<- predict(model,train, na.action=na.exclude)
    p2<- predict(model,test)
    
    #variable Importance
    varImpPlot(model)
    
   
    #ANALIYSIS WITH OLS RESULTS
    
    treedata <-train %>% select(Order.Volume,
                                Finland.unemployment.rate.Index...t.4.,
                                South.Korea.Industrial.Production..t.1., 
                                Duke.Energy.Corp..t.5., 
                                Brasil.Government.Expenditure..t.0., India.Government.Expenditure..t.1.)
    
    
    treedata<- mutate(treedata, Order.Volume=factor(Order.Volume), 
                      Finland.unemployment.rate.Index...t.4.= as.numeric(Finland.unemployment.rate.Index...t.4.),
                      Duke.Energy.Corp..t.5.= as.numeric(Duke.Energy.Corp..t.5.),
                      Brasil.Government.Expenditure..t.0.=as.numeric(Brasil.Government.Expenditure..t.0.),
                      India.Government.Expenditure..t.1.= as.numeric(India.Government.Expenditure..t.1.))
    
    
    #Data preparation
    treedata$South.Korea.Industrial.Production..t.1.= as.factor(treedata$South.Korea.Industrial.Production..t.1.)
    treedata$Order.Volume=as.numeric(treedata$Order.Volume)
    str(treedata)
    
    
    
    
    #CART Regression Tree
    
    m1 <- rpart(Order.Volume~., data = treedata, method='anova')
    rpart.plot(m1, type=3, digits=3, fallen.leaves = TRUE)
    p1 <- predict(m1,treedata[c(54:60),])
    p2 <- predict(m1,treedata[c(1:53),])
    tests <- treedata[c(1:53),]
    
    #importance check
    
    m1$variable.importance
    
    
    
    
    
    
    
    
    #PCA
    
    pca <- prcomp(dfshort, scale=TRUE, center = TRUE)
    
    summary(pca)
    str(pca)
    
    
    plot(pca$x[,1], pca$x[,2])
    
    
    library("factoextra")
    eig.val <- get_eigenvalue(pca)
    eig.val
    
    
    
    pca.var <-pca$sdev^2
    pca.var.per<- round(pca.var/sum(pca.var)*100, 1)
    barplot(pca.var.per, main="Scree Plot", xlab = "Principal Component", ylab="Percent Variation")
    plot(pca.var.per, type = "l", xlab = "Principal Component", ylab="Percent Variation")
    pca.data <- data.frame(rownames(pca$x),
                           + pca$x[,1],
                           + pca$x[,2])
    
    
    #Plot drawing
    install.packages("devtools")
    
    library(devtools)
    
    install_github("vqv/ggbiplot")
    
    library(ggbiplot)
    
    #biplot
    fviz_pca(pca)
    
    # Contribution of variables
    var<-get_pca_var(pca)
    head(var$contrib)
    
    
    predictdata <- df[c(54:60),]
    
    
    df2<-cbind(dfshort, pca$x[,1:4])
    ggplot(data=df2, aes(PC1,PC2)) +
        stat_ellipse(geom = "polygon", col="black", alpha=0.5) +
        geom_point(shape=21, col="black")
    
    
    
    library("factoextra")
    # Compute hierarchical clustering and cut into 4 clusters
    res <- hcut(dfshort, k = 4, stand = TRUE)
    # Visualize
    fviz_dend(res, rect = TRUE, cex = 0.5,
              k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"))
    


