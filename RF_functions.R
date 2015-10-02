require(randomForest)
require(doParallel)
require(foreach)
require(ggplot2)
require(caret)
par_forest = function(x,y,ntree=1000,proximity=proximity,importance=importance,n_cluster=8,...){
          #start time
          strt = Sys.time()
          #loop
          rf = foreach(ntree=rep(ntree %/% n_cluster,n_cluster), .combine=randomForest::combine,.packages='randomForest') %dopar% {
            randomForest(x=x,y=y,ntree=ntree,proximity=proximity,importance=importance) 
          }
          print(Sys.time()-strt)
          
          rf
}

call_rf_classify = function(feature,x=x,y=y,ntree=ntree,proximity=proximity,importance=importance,scale_by_TIV=0,scale_by_col=0,center_by_col=0,control=0){
  print(feature)
        if (feature=="_all") 
        {
          df1 = df[11:length(df)]
        }  
        else
        {
          df1 = df[grep(feature,names(df))]    
        }
        
        
        if (feature=="_volume" && scale_by_TIV==1)
        {
          TIV = df1$EstimatedTotalIntraCranialVol_volume
          df1 = as.data.frame(sweep(data.matrix(df1),1,TIV,'/'))
          df1 = df1[-length(df1)]
        }
        
        if(scale_by_col==1){
          df1 = scale(df1)
        } else if(center_by_col==1){
          df1 = scale(df1,scale=TRUE)
        }
        
        ## Splitting Data
        trainIndex = createDataPartition(as.factor(df$ADOS), p=0.7,list = FALSE) # as.factor makes sure that the subjects are evenly sampled from each ADOS score
        Train = df1[trainIndex,]
        Test = df1[-trainIndex,]
        y = df$ADOS[trainIndex]
        ## Call RF
        rf<-par_forest(x=Train,y=y,ntree=ntree,proximity=proximity,importance=importance,scale_by_TIV=scale_by_TIV, scale_by_col=scale_by_col,center_by_col=center_by_col,control=control) #control=1 : control data is also present
           # y=NULL or y=df$ADOS for regression 
        threshold = mean(rf$predicted,na.rm=TRUE)
        ## Prediction on Test data
       predicted = predict(rf,Test)
        
        ## Plots
        varImpPlot(rf)
        df_test = data.frame(xid=1:nrow(Test),true=df$ADOS[-trainIndex],predicted=predicted,control=as.factor(df$control[-trainIndex]))
        df_train = data.frame(xid=1:nrow(Train),true=df$ADOS[trainIndex],predicted=rf$predicted,control=as.factor(df$control[trainIndex]))
   
        
#         print(dff)
        p = ggplot(data=df_train,aes(x=xid,shape=control))+geom_point(aes(y=true,col='blue'))+geom_point(aes(y=predicted,col='red'))+
        theme_bw(base_size=15)+scale_colour_manual(values=c('blue','red'),labels=c("true","predicted"))+
        xlab("Train Subjects")+ylab("ADOS score (OOB Prediction")+ggtitle(feature)+geom_abline(intercept= threshold,slope=0)
          print(p)  
        
        p = ggplot(data=df_test,aes(x=xid,shape=control))+geom_point(aes(y=true),col='blue')+geom_point(aes(y=predicted),col='red')+
          theme_bw(base_size=15)+scale_colour_manual(values=c('blue','red'),labels=c("true","predicted"))+
          xlab("Test Subjects")+ylab("ADOS score")+ggtitle(feature)+geom_abline(intercept= threshold,slope=0)
        print(p)
  

    ## Classification
     print(threshold)
      predicted_class = (predicted<threshold)

      cf = confusionMatrix(df_test$control,as.numeric(predicted_class))
      print(cf)

rf
}



