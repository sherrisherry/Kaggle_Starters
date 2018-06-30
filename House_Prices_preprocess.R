# read in the training and test datasets without converting 'NA's to missing values.
train<-read.csv('House_Prices\train.csv', na.strings = "", strip.white = TRUE)
test<-read.csv('House_Prices\test.csv', na.strings = "", strip.white = TRUE)
# exploring the training set
summary(train)
sum(is.na(train))
# making sure whether time series analysis is necessary
summary(train$YrSold)
summary(test$YrSold)
# filling the target columns of the test set with NA then combining test and training sets
test$SalePrice<-NA
df<-rbind(train,test)
rm(train,test)
df$MSSubClass<-NULL # drop MSSubClass because it duplicates with YearBuilt and HouseStyle
sort(apply(is.na(df[,-ncol(df)]),2,sum),decreasing = TRUE) # counting the number of missing values in each predictor and sort them in descending order. only work when target variable is the last column
# in the 'NAisNoApp' columns, NA means this attribute doesn't apply to them, not missing.
NAisNoApp<-c('Alley', 'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2', 'FireplaceQu', 'GarageType', 'GarageFinish', 'GarageQual', 'GarageCond', 'PoolQC', 'Fence', 'MiscFeature')
for(i in NAisNoApp){levels(df[,i])[levels(df[,i])=="NA"] <- "NoApp"}
# write the dataset into a csv file then read this file back to df to reconstruct df
write.csv(df,file = 'data.csv', row.names = FALSE)
df<-read.csv('data.csv', na.strings = "NA", strip.white = TRUE)
sort(apply(is.na(df[,-ncol(df)]),2,sum),decreasing = TRUE) # check the number of missing values in each predictor.
sort(apply(is.na(df[is.na(df$SalePrice),]),2,sum),decreasing = TRUE) # check the number of missing values in each predictor in the test set.
# Most of the missing values are in the test set so removing incomplete rows is not feasible even if they are only a small portion of the dataset.
df[is.na(df$BsmtFinSF1),]
# NAs in 'BsmtFinSF1','BsmtFinSF2','BsmtUnfSF',and 'TotalBsmtSF' actually mean 0
df[df$BsmtCond=='NoApp',][1:3,]
df[is.na(df$BsmtFinSF1),c('BsmtFinSF1','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF')]<-0
df[is.na(df$BsmtFullBath),c('BsmtFullBath','BsmtHalfBath')]<-0
# binning 'GarageYrBlt'
summary(df[is.na(df$GarageYrBlt),])
plot(SalePrice~GarageYrBlt,data = df[!is.na(df$SalePrice),])
df[is.na(df$GarageYrBlt),'GarageYrBlt']<-0
df[df$GarageYrBlt<1945&df$GarageYrBlt>10,'GarageYrBlt']<-1
df[df$GarageYrBlt>=1945&df$GarageYrBlt<1985,'GarageYrBlt']<-2
df[df$GarageYrBlt>=1985,'GarageYrBlt']<-3
# the NAs in the following columns were not imputed correctly
ls<-c('GarageYrBlt','GarageFinish','GarageQual','GarageCond')
df[df$GarageType!='NoApp'&df$GarageYrBlt==0,ls]<-NA
# create a map to encode ordinal columns by sequential integers
data.map<-function(x,common=0,message=TRUE){
# summarizes the data frame 'x'
# returns a list of factor_cols, factor_levels, char_cols, and num_cols.
# common: a non-negative numerical parameter, if 2 factorial columns share more than 'common' levels, they share the same scheme. 0 means all the levels should be the same.
factor_cols<-list() # a list of the sets of names of factorial columns. if 2 factorial columns share the same scheme, they are in the same set. the name of a set is the same as the 1st element of the set.
factor_levels<-list() # a list of the schemes of factorial columns. the name of a scheme is the same as the name of its corresponding set.
char_cols<-c() # a set of the names of char columns.
num_cols<-c() # a set of the names of numerical columns.
for(i in colnames(x)){
  switch(class(x[,i]), # r switch doesn't need break to branch out
         factor={
           l<-levels(x[,i])
           if(!length(factor_cols)){
             factor_levels[[i]]<-l
             factor_cols[[i]]<-i
           }
           else{
             for(j in 1:length(factor_levels)){
               if(ifelse(common,sum(l %in% factor_levels[[j]])>common,all(l==factor_levels[[j]]))){
                 factor_cols[[j]]<-append(factor_cols[[j]],i)
                 if(!common)factor_levels[[j]]<-union(l,factor_levels[[j]])
                 j<--5
                 break}
             }
             if(j!=-5){
               factor_cols[[i]]<-i
               factor_levels[[i]]<-l
             }
           }},
         character={char_cols<-append(char_cols,i)},
         {num_cols<-append(num_cols,i)})
  if(message)cat(paste(i,class(x[,i]),'factors:',length(factor_cols),'chars:',length(char_cols),'nums:',length(num_cols),"\n",sep = ' '))
}
return(list(factor_cols=factor_cols,factor_levels=factor_levels,char_cols=char_cols,num_cols=num_cols))
}
data_map<-data.map(df[,-ncol(df)],common=2)
factor_cols<-data_map$factor_cols
factor_levels<-data_map$factor_levels
num_cols<-data_map$num_cols
rm(data_map)
# impute NAs in factorial columns by the mode of corresponding columns
ls<-unlist(factor_cols)
for(i in 1:length(ls))df[is.na(df[,ls[i]]),ls[i]]<-names(which.max(table(df[!is.na(df$SalePrice),ls[i]])))
# impute NAs in numerical columns by the median of corresponding columns
ls<-num_cols
for(i in 1:length(ls))df[is.na(df[,ls[i]]),ls[i]]<-median(df[!is.na(df$SalePrice),ls[i]],na.rm = TRUE)

# encode ordinal columns by sequential integers
encode.ordinal<-function(x,ls,none='',out.int=FALSE,full_print=TRUE,log=FALSE){
  # returns an encoded obj.
  # x is the ordinal factorial data frame to be encoded.
  # ls is an ordered list of the levels, ordered low to high.
  # none is the 'none'-but-not-'NA' level, always encoded to 0, levels from low to high encoded from 1 increasing by 1.
  # out.int: whether to convert encoded obj to integers.
  ## Only set to TRUE when no NA in the obj because NAs in x causes error when converting to integers.
  ## by default, the encoded obj is factorial, which will be numerical once saved then reloaded.
  # full_print: when set to FALSE, only print minimum information.
  # log is the parameters for sink(). if FALSE, no log file.
  if(is.null(dim(x)))stop('data frame degraded to vector, use df[ , , drop=FALSE]')
  if(full_print)print(summary(x))
  for(i in 1:ncol(x)){
    for(j in 1:length(ls))levels(x[,i])[levels(x[,i])==ls[j]] <- j
    levels(x[,i])[levels(x[,i])==none] <- 0
  }
  cat(paste('coded',i,'cols',j,'levels','\n'))
  if(full_print)print(summary(x)) # compare this to the summary before encoding to check whether encoding has run correctly.
  if(out.int){
	for(i in 1:ncol(x)){
		x[,i]<-as.character(x[,i])
		x[,i]<-as.integer(x[,i])}
	if(full_print)print(summary(x))
  }
  if(is.list(log)){
	sink(file=log$file,append = log$append, split = log$split) # divert output to file
	cat('Columns:\n\t')
	cat(colnames(x),sep=', ')
	cat('\nScheme:\n')
	encode<-0:length(ls)
	names(encode)<-c(none,ls)
	print(encode)
	cat('\n')
	sink() # divert output back to console
	}
  return(x)
}
summary(factor_cols)
# ExterQual includes 10 columns. in this context it is likely to be a group of scoring.
factor_levels
# we can see that ExterQual is ordinal and we also find some other ordinal columns.
log_para<-list(file='recipe.txt',append = TRUE, split = FALSE)
i<-'ExterQual'
df[,factor_cols[[i]]]<-encode.ordinal(df[,factor_cols[[i]]],ls=c('Po','Fa','TA','Gd','Ex'),none = 'NoApp',log=log_para)
factor_levels[[i]]<-NULL
factor_cols[[i]]<-NULL
i<-'BsmtFinType1'
df[,factor_cols[[i]]]<-encode.ordinal(df[,factor_cols[[i]]],ls=c("Unf","LwQ","Rec","BLQ","ALQ","GLQ"),none = 'NoApp',log=log_para)
factor_levels[[i]]<-NULL
factor_cols[[i]]<-NULL
i<-'BsmtExposure'
df[,factor_cols[[i]]]<-encode.ordinal(df[,factor_cols[[i]],drop=FALSE],ls=c("No","Mn","Av","Gd"),none = 'NoApp',log=log_para) # ensure input x doesn't degrade to vector by 'drop=FALSE'
factor_levels[[i]]<-NULL
factor_cols[[i]]<-NULL
i<-'GarageFinish'
df[,factor_cols[[i]]]<-encode.ordinal(df[,factor_cols[[i]],drop=FALSE],ls=c("Unf","RFn","Fin"),none = 'NoApp',log=log_para)
factor_levels[[i]]<-NULL
factor_cols[[i]]<-NULL
i<-'LandSlope'
df[,factor_cols[[i]]]<-encode.ordinal(df[,factor_cols[[i]],drop=FALSE],ls=c("Gtl","Mod","Sev"),log=log_para)
factor_levels[[i]]<-NULL
factor_cols[[i]]<-NULL
i<-'PavedDrive'
df[,factor_cols[[i]]]<-encode.ordinal(df[,factor_cols[[i]],drop=FALSE],ls=c("N","P","Y"),log=log_para)
factor_levels[[i]]<-NULL
factor_cols[[i]]<-NULL
i<-'Utilities'
df[,factor_cols[[i]]]<-encode.ordinal(df[,factor_cols[[i]],drop=FALSE],ls=c('ELO','NoSeWa','NoSewr','AllPub'),log=log_para) # in dataset only "AllPub" "NoSeWa", with 2 NAs
factor_levels[[i]]<-NULL
factor_cols[[i]]<-NULL
# bin categories of the same level together before encoding 'Functional'
levels(df$Functional)[levels(df$Functional)=="Maj2"] <- "Maj1"
levels(df$Functional)[levels(df$Functional)=="Min2"] <- "Min1"
i<-'Functional'
df[,factor_cols[[i]]]<-encode.ordinal(df[,factor_cols[[i]],drop=FALSE],ls=c("Sev","Maj1","Mod","Min1","Typ"),log=log_para)
factor_levels[[i]]<-NULL
factor_cols[[i]]<-NULL
# find all the 2-level columns
ls<-lapply(factor_levels,length)
ls<-as.data.frame(ls) # list can be converted only to data frame
colnames(ls[,ls==2])
i<-c(factor_cols$Street,factor_cols$CentralAir)
# encode all the 2-level columns
encode.binary<-function(x,out.int=FALSE,full_print=TRUE,log=FALSE){
  # encode 2-level columns to 0 and 1
  if(is.null(dim(x)))stop('data frame degraded to vector, use df[ , , drop=FALSE]')
  if(full_print)print(summary(x))
  if(is.list(log)){
  map<-data.map(x,message=FALSE)
  cols<-map$factor_cols
  lvs<-map$factor_levels
  rm(map)
  for(i in 1:length(cols)){
    y<-x[,cols[[i]],drop=FALSE]
	for(j in 1:ncol(y))levels(y[,j])<-c(0,1)
	x[,cols[[i]]]<-y
	cat(paste('coded',j,'cols','\n'))
	sink(file=log$file,append = log$append, split = log$split) # divert output to file
	cat('Columns:\n\t')
	cat(cols[[i]],sep=', ')
	cat('\nScheme:\n')
	encode<-c(0,1)
	names(encode)<-lvs[[i]]
	print(encode)
	cat('\n')
	sink() # divert output back to console
	}
  rm(y)
  }
  else{
  for(j in 1:ncol(x))levels(x[,j])<-c(0,1)
  cat(paste('coded',j,'cols','\n'))
  }
  if(full_print)print(summary(x)) # compare this to the summary before encoding to check whether encoding has run correctly.
  if(out.int){
	for(i in 1:ncol(x)){
		x[,i]<-as.character(x[,i])
		x[,i]<-as.integer(x[,i])}
	if(full_print)print(summary(x))
  }
  return(x)
}
df[,i]<-encode.binary(df[,i,drop=FALSE],log=log_para)
write.csv(df,file = 'data.csv', row.names = FALSE)