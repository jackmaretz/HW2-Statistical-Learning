library(wrassp) 
#library(help = wrassp)
library(tuneR)
#Read the .au file 

# read.AsspDataObj ------------------------------------------------------------------------

temp = list.files(pattern = "*.au")

create_empty_table <- function(num_rows, num_cols) {
  frame <- data.frame(matrix(NA, nrow = num_rows, ncol = num_cols))
  return(frame)
}



# creating empty df -------------------------------------------------------
mat <- matrix(, nrow = 150, ncol = 5128)
suppressPackageStartupMessages( require(signal, quietly = TRUE) )

for (file in 1:length(temp)) {
  x = read.AsspDataObj(temp[file])
  #print(x)
  y = Wave(as.numeric(x$audio), samp.rate = rate.AsspDataObj(x), bit = 16)
  yDown = downsample(y,samp.rate = 11025)
  #print(file)
  #####
  fs = yDown@samp.rate
  winsize <- 2048
  hopsize <- 512
  nfft <- 2048
  noverlap <- winsize - hopsize
  #go
  #print(yDown@left[1:3])
  
  sp <-specgram(x=yDown@left[1:330000],n=nfft,Fs=fs, window = winsize,overlap = noverlap)
  #print(sp$S[1,1])
  #take a look
  # print(dim(sp$S))
  # print(round(head(sp$f),2)) # *frequency* indices corresponding to the *rows* of S 
  # print(round(head(sp$t),2))
  # 
  # freq band selection -----------------------------------------------------
  
  nb = 2^3
  lowB =100
  eps <- .Machine$double.eps
  
  # numb sec analyzez window ------------------------------------------------
  
  ntm <- ncol(sp$S)
  corrtime <- 15
  
  # Energy of bands  ------------------------------------------------------------------
 
  fco <- round( c(0, lowB*(fs/2/lowB)^((0:(nb - 1))/(nb - 1)))/fs*nfft ) 
  energy <- matrix(0, nb, ntm) 
  for (tm in 1:ntm) { 
    for (i in 1:nb) { 
      lower_bound <- 1 + fco[i] 
      upper_bound <- min( c( 1 + fco[i + 1], nrow(sp$S) ) ) 
      energy[i, tm] <- sum( abs(sp$S[ lower_bound:upper_bound, tm ])^2 )
    } 
  }
  energy[energy < eps] <- eps 
  energy = 10*log10(energy)
  feat.vec <- c(energy)  
 
  #print(c('file',(file),'feat',feat.vec[1:2]))
  mat[file,] <- feat.vec
  #print('mat',head(mat[file,]))
}
  
# 
# # what to do with every au ------------------------------------------------
# 
# 
# str(f1.au)
# f1 = f1.au@left 
# f1.dwn = downsample(f1.au,samp.rate = 11025)
# 
# 
# # Feature extraction ------------------------------------------------------
# 
#suppressPackageStartupMessages( require(signal, quietly = TRUE) )
# 
# # setup of short time fourier transform -----------------------------------
# fs = f1.dwn@samp.rate
# winsize <- 2048
# hopsize <- 512
# nfft <- 2048
# noverlap <- winsize - hopsize
# #go
# sp <-  specgram(x=f1.dwn@left,n=nfft,Fs=fs, window = winsize,overlap = noverlap)
# 
# #take a look
# class(sp)
# names(sp)
# #sp
# dim(sp$S)
# 
# # freq band selection -----------------------------------------------------
# 
# nb= 2^3
# lowB=100
# eps <- .Machine$double.eps
# 
# # numb sec analyzez window ------------------------------------------------
# 
# ntm <- ncol(sp$S)
# corrtime <- 15
# 
# 
# # energy ------------------------------------------------------------------
# 
# # Energy of bands 
# fco <- round( c(0, lowB*(fs/2/lowB)^((0:(nb-1))/(nb-1)))/fs*nfft ) 
# energy <- matrix(0, nb, ntm) 
# for (tm in 1:ntm){ 
#   for (i in 1:nb){ 
#     lower_bound <- 1 + fco[i] 
#     upper_bound <- min( c( 1 + fco[i + 1], nrow(sp$S) ) ) 
#     energy[i, tm] <- sum( abs(sp$S[ lower_bound:upper_bound, tm ])^2 )
#   } 
# }
# energy[energy < eps] <- eps 
# energy = 10*log10(energy)
# energy[,1:3]
# ##
# dim(energy)
# feat.vec <- c(energy)
# head(feat.vec)
label = read.table('Labels.txt')
data=as.data.frame(mat)
data=cbind(label,data)
write.csv(data, file = "database.csv")


# spitting rtain test -----------------------------------------------------
dim(data) 

#Sample Indexes
indexes = sample(1:nrow(data), size=0.5*nrow(data))

# Split data
test = data[indexes,]
dim(test)  # 6 11
train = data[-indexes,]
dim(train) # 26 11




# MODEL SVM ---------------------------------------------------------------



library(e1071)
svm_model <- svm(x~ ., data=train, method='C-classification', kernel='linear')
svm_model
#training set predictions
pred_train <-predict(svm_model,train)
mean(pred_train==train$x)
#test set predictions
pred_test <-predict(svm_model,test)
mean(pred_test==test$x)



# model svm CARET ---------------------------------------------------------

library(caret)
ctrl <- trainControl(method = "repeatedcv", repeats = 10)
mod <- train(x~., data=train, method = "svmLinear", trControl = ctrl)
pred_train <-predict(mod,train)
mean(pred_train==train$x)
pred_test <-predict(mod,test[,-1])
mean(pred_test==test$x)


# model RANDOM FOREST -----------------------------------------------------
library(ranger)
modRF <-  ranger(x~.,data=train,num.trees = 500,importance = 'permutation')
modRF
p16<-predict(modRF,data=test)
preds=p16$predictions
variabiliImp=order(importance(modRF))[1:50]

mean(preds==test$x)



# train ridotto -----------------------------------------------------------

trainReduced= train[,c(1,variabiliImp)]
modRfReduced <-  ranger(x~.,data=trainReduced,num.trees = 200)
modRfReduced
p16<-predict(modRfReduced,data=test)
preds=p16$predictions
mean(preds==test$x)


# XGBOOOOOST --------------------------------------------------------------
#::install_github("Laurae2/Laurae")
library(Laurae)
dtrain <-
  xgb.DMatrix(data = Laurae::DT2mat(X[1:39943,]), label = y)
dtest <-
  xgb.DMatrix(data = Laurae::DT2mat(X[-c(1:39943),]), label = yv)

set.seed(11111)
cv <-
  xgb.cv(
    params = list(
      nthread = 4,
      # More threads if you feel so
      eta = 0.10,
      max_depth = 6,
      booster = "gbtree",colsample_bytree=0.6
    ),
    objective = "binary:logistic",
    eval_metric = "error",
    nrounds = 10000,
    early_stopping_rounds = 50,
    data = dtrain,
    nfold = 3,
    verbose = 1
  )
)[3] / 60);cvtempo




# xgb ---------------------------------------------------------------------
xg
xgb <- xgboost(data = data.matrix(train[,-1]), 
               label = x, 
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softmax",
               num_class = 12,
               nthread = 3
)
