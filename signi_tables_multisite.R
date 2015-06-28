# ouputs brain morphometric features  that are significantly different between groups (control vs. patient) for each site of ABIDE datset

create_signi_tables = function(site)
{
    # print(site)
    library(matlab)
    library(stringr)
    library(xtable)
    # tables = "/Volumes/admi-1/Projects/ABIDE_Comparison_pipelines/segmentation_analysis/FAST_segmentation_tables" # where seg. tables are stored

    imp_stats = c('GMvolume','totalvolume','area','intensity_mean','thickness_mean','curvature_gauss','curvature_mean','foldingIndex')
    # imp_stats = 'intensity_mean'
    # stat = imp_stats[1]
    # lof = dir(tables,paste(stat,'.csv',sep = ''))
    # dframe = read.csv(fullfile(tables,lof[1]),row.names = 1)


    df = data.frame(matrix(nrow = 40+50-10,ncol = length(imp_stats)))
    colnames(df) = imp_stats
    # print(site)

    for (s in 1:length(imp_stats))
    {
        stat = imp_stats[s]
        lof = dir(tables,paste(stat,'.csv',sep = ''))
        print(stat)
        print(lof)
        for (i in 1:length(lof)) # files belonging to the stat
        {
          # print(lof[i])
          d = read.csv(fullfile(tables,lof[i]),row.names = 1)
          if (ncol(d)> = 50) {OFFSET = 0} else {OFFSET = 50-5}
          print(ncol(d))
          print(OFFSET)
          print(colnames(d))
          col_names = colnames(d)
          # print(head(dframe))
          dframe = d[d['site'] =  = site,] # using only the rows belonging to the site passed to the function
          if (site =  = 'ALL') dframe = d

          # It is recommended to demean the covariates in applications of linear model
          age = dframe$age
          age = scale(age,center = TRUE,scale = FALSE)
          TIV = dframe$TIV
          TIV = scale(TIV,center = TRUE,scale = FALSE) 
      autism = as.factor(dframe$autism)

          # sex = scale(dframe$sex,center = TRUE,scale = FALSE) 
      sex = as.factor(dframe$sex)
          
        NCOL = ncol(dframe)
        print(NCOL)
        if (NCOL>50) NCOL = 50
        print('*****')
        print(NCOL)
          for (j in 6:NCOL) # goes through different structures
          {
             ## REGRESSION
               if (length(unique(sex)) =  = 1)
          {lm.model = lm(dframe[,j]~autism+age+TIV)} # since contrasts can be applied only to factors with 2 or more levels

          else
          {lm.model = lm(dframe[,j]~autism+age+TIV+sex)}

              result = coef(summary(lm.model))
              p = result[2,4]
              slope = result[2,1]
              if(is.na(p)) p = 1
              df[j-5+OFFSET,stat] = p*sign(slope)

              if (p<0.05) 
              {
                  # df[j,stat] = gsub("_volume|_area|_mean|_thickness|_gauscurv|_meancurv|_foldind","",col_names[j])
                  # print('******')
              }
          }
          
        }
      
      }
      return(df)


        # df = df[apply(df,1,function(x) any(!is.na(x))),] # remove the rows which have all NAs
        # print.xtable(xtable(df,label = site,caption = site),size = 'small',include.rownames = FALSE,add.to.row = list(list(-1),"\\rowcolor[gray]{.8} "),scalebox = 0.55)
        # print(xtable(t(df)))
        print(df)

    # dd  =  Reduce(function(...) merge(..., all = T), d)
    # dd = dd[apply(dd,1,function(x) any(!is.na(x))),] # remove the rows which have all NAs
     # print.xtable(xtable(dd,label = site,caption = site),size = 'tiny',include.rownames = FALSE,add.to.row = list(list(-1),"\\rowcolor[gray]{.8} "))

    
}
#######
    tables = "/Volumes/admI/Projects/ABIDE_Comparison_pipelines/segmentation_analysis/FreeSurfer_segmentation_tables"
    imp_stats = c('GMvolume','totalvolume','area','intensity_mean','thickness_mean','curvature_gauss','curvature_mean','foldingIndex')

ABIDE = read.csv('ABIDE.csv',row.names = 1)

sites = as.list(as.character(unique(ABIDE$site)))
sites = c(sites,'ALL')

# sites = 'NYU'
file.create('FS.tex')
lof = dir(tables,'\\.csv')
dframe_aparc = read.csv(fullfile(tables,lof[2]),row.names = 1)
dframe_aseg = read.csv(fullfile(tables,lof[16]),row.names = 1)

NROW = length(colnames(dframe))    

# for ( i in 1:length(lof)){
#   print(lof[i])
#  print(length(colnames(dframe)))
# }
allframes = lapply(sites,create_signi_tables)
frame = do.call(cbind,allframes)
# lof = dir(tables,paste(stat,'.csv',sep = ''))
col_names_aparc = colnames(dframe_aparc)
col_names_aseg = colnames(dframe_aseg)

rownames(frame) = c(col_names_aseg[-c(1:5)],col_names_aparc[-c(1:5)])
write.csv(frame,file = 'FS.csv',row.names = TRUE)


library(xlsx)
wb = createWorkbook()
for (i in 1:length(imp_stats))
{
  d = data.frame(matrix(nrow = nrow(frame)))
  rownames(d) = rownames(frame)
  for (j in seq(i,length(sites)*length(imp_stats),by = length(imp_stats)))
  {
    print(j)
  d = cbind(d,frame[,j])
  }
  print(imp_stats[i])
  d = d[,-1]
  # rownames(d) = gsub('.csv','',lof)
  colnames(d) = sites
   print(d)
 # write.csv(d,file = paste('FIRST_',imp_cols[i],'.csv',sep = ''),row.names = TRUE)
sheet = createSheet(wb,sheetName = imp_stats[i])
addDataFrame(d,sheet)
}
saveWorkbook(wb,"FS.xlsx")













