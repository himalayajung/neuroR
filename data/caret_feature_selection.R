require(caret)
require(randomForest)
require(doParallel)
require(foreach)

source('../Rfunctions.R') # R helper functions

## Settings
set.seed(123)
# brain morphometric properties
features=list('_volume','_area','_thickness$','_thicknessstd','_foldind','_meancurv','_gauscurv','_all')
# features=list('_volume')
scale_by_TIV=1 # should you normalize the volume features by TIV??
data_splitting=0

## Data
df=read.csv('../FS.csv',row.names=1)
df_imputed=read.csv('../FS_imputed.csv',row.names=1)

df=df[df$sex==1,] # select only male subjects
df_imputed=df_imputed[df_imputed$sex==1,]
df$ADOS=df_imputed$ADOS
df$control=as.factor(df$control)

df=df[df$site=="KKI",] # choose individual site if necessary
df_imputed=df_imputed[df_imputed$site=="KKI",]
phenotype=df_imputed[c("age","sex","VIQ","PIQ","FIQ")]

## setup parallel backend to use multiple processors
n_cluster=22
cl = makeCluster(n_cluster)
registerDoParallel(cl)
# Loop along the features
for (feature in features){
  
  print(paste('************************',feature,'************************'))
  # creating feature specific data
  if (feature=="_all") {
    d = df[11:length(df)]
    col_vol = grep("_volume",names(d))  
    TIV = df$EstimatedTotalIntraCranialVol_volume
    d[col_vol] = as.data.frame(sweep(data.matrix(d[col_vol]),1,TIV,'/'))
    d=d[-length(d)]
  } else{
    d=df[grep(feature,names(df))]    }
    
  if (feature=="_volume" && scale_by_TIV==1){
    TIV = d$EstimatedTotalIntraCranialVol_volume
    d = as.data.frame(sweep(data.matrix(d),1,TIV,'/'))
    d = d[-length(d)]}
  ## Preprocessing
  # Remove near zero variance predictors
  nzv = nearZeroVar(d)
  if(length(nzv)>0){
    d = d[, -nzv]}
  # Identifying Correlated Predictors
  highlyCor = findCorrelation(cor(d),cutoff=0.99)
  if(length(highlyCor)>0){
    d = d[,-highlyCor]}
  # Linear Dependencies  --commented for individual sites
  #             comboInfo  =  findLinearCombos(as.matrix(d))
  #             print(comboInfo)
  #             if(length(comboInfo$remove)>0){
  #               d=d[,-comboInfo$remove]}
  ## Data Splitting
  if(data_splitting==1){
    trainIndex  =  createDataPartition(as.factor(df$control), p=0.8,list = FALSE) # as.factor makes sure that the subjects are evenly sampled from each ADOS score
    Train = d[trainIndex,]
    Test = d[-trainIndex,]
    yTrain = df$control[trainIndex]
    yTest = df$control[-trainIndex]} else{
      Train = d
      yTrain = df$control
    }

# define the control using a random forest selection function
control  =  rfeControl(functions=rfFuncs, method = "LOOCV")
# run the RFE algorithm
rfProfile =  rfe(Train, yTrain, sizes=10, rfeControl=control)
# save(rfProfile,file=paste0('feature_selection/kki/','RFE',feature,'_male.Rdata'))

# list the chosen features
print(predictors(rfProfile))
cat('Resamples')
print(head(rfProfile$resample))
# plot the results
p = plot(rfProfile, type=c("g", "o"),main=feature)
print(p)

}
stopCluster(cl) 

