#CREACI?N DE LA NUEVA BASE DE DATOS

####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                           SET PATHS                                    ####
path_data <- 'C:/Users/Tester/Desktop/TFG'
#path_src <- 'N:/UDMTD16/code/src'
#path_data <- 'N:/UDMTD16/data'
#path_data <- 'N:/UDMTD/UDMTD16/data'
#path_src  <- 'N:/UDMTD/UDMTD16/code/src'

####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                  LOAD PACKAGES AND FUNCTIONS                           ####
library(data.table)
library(pROC)
library(ggplot2)
#library(viridis) #customize ggplots
#library(gridExtra) #customize ggroc
library(latex2exp)
#library(ROCR)
library(matrixStats)
library(randomForest)
library(ranger)

####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                  LOAD PACKAGES AND FUNCTIONS                           ####

target_vars <- c("CLASE_AS", "CNAE_Div_AS", "A10_i")


####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
####                  READ RDS                                              ####

tru.dt <- readRDS(file = file.path(path_data, 'FF_2011.dt.rds'))
raw.dt <- readRDS(file = file.path(path_data, 'FG_2011.dt.rds'))

raw.dt[, cod := paste(CODSEC, VIV, HOGAR, NORDEN_ID, sep = "")]
tru.dt[, cod := paste(CODSEC, VIV, HOGAR, NORDEN_ID, sep = "")]

varNames_all    <- names(raw.dt)
varNames_ID     <- c('cod', 'CCAA', 'ESTRATO', "SEXOa", "EDADa", "PROXY_0", 'FACTORADULTO')
varNames_target <- c( "F9", "F18", "F8_2", "F17a_2", "F17m_2", "F7_2", 
                      "F16a_2", "F16m_2", "A10_i", 
                      "CNO_Sub_AS", "CNO_SPl_AS", "CNO_GPl_AS",  
                      "CNAE_Gru_AS", "CNAE_Div_AS", "CNAE_Sec_AS", 
                      "SitProf_AS", "CLASE_AS", "A7_2a", "D28", "ACTIVa")

raw.dt <- raw.dt[, c(varNames_ID, varNames_target), with = FALSE]
setnames(raw.dt, varNames_target, paste0(varNames_target, '_raw'))

tru.dt <- tru.dt[, c('cod', varNames_target), with = FALSE]
setnames(tru.dt, varNames_target, paste0(varNames_target, '_true'))


dat.dt <- merge(raw.dt, tru.dt, by = 'cod')


regressors_vars <- c(setdiff(varNames_ID, 'cod'), paste0(varNames_target, '_raw'))


for (i in target_vars){
  
  r <- paste(i, "_raw", sep = "")
  t <- paste(i, "_true", sep = "")
  targetName <- paste0('target_', i)
  set(dat.dt, which(is.na(dat.dt[[r]])), r, '*')
  set(dat.dt, which(is.na(dat.dt[[t]])), t, '*')
  dat.dt[, (targetName) := ifelse(get(r) == get(t), 0, 1)]
  
}

dat.dt[
  , target := (rowSums2(as.matrix(.SD)) > 0) * 1L, .SDcols = paste0('target_', target_vars)]

data_rf.dt <- copy(raw.dt)[
  dat.dt[, c('cod', 'target')], on = 'cod'][
  , c('target', regressors_vars), with = FALSE]

data_rf.dt[is.na(data_rf.dt)] <- '*'

train_index <- sample(1:nrow(data_rf.dt), nrow(data_rf.dt) * 0.8)
train_rf.dt <- data_rf.dt[train_index, ]
test_rf.dt  <- data_rf.dt[-train_index, ]


fit=ranger(formula = target ~ ., data = train_rf.dt, num.trees = 1000, importance = "impurity")
fit$variable.importance
#ggplot2
importance.dt <- data.table(importance = fit$variable.importance, variable = names(fit$variable.importance))
ggplot(importance.dt, aes(x = reorder(variable, importance, sum), y = importance)) + 
  geom_col()+
  coord_flip() +
  theme_bw()

values_mtry  <- seq.int(from = 1, to = length(regressors_vars), length.out = 4)
values_trees <- c(500, 750, 1000, 1250, 1500, 1750)
error        <- matrix(nrow = length(values_mtry), ncol = length(values_trees))

#tuneRanger(target ~ ., measure = NULL, iters = 70, iters.warmup = 30,
#           time.budget = NULL, num.threads = NULL, num.trees = 1000,
#           parameters = list(replace = FALSE, respect.unordered.factors ="order"))

#data_rf.task <- makeClassifTask(data= data_rf, target = "target")
#result_tune <- tuneRanger(data_rf.task, num.trees = 1000)



for(i in 1:length(values_mtry)){
  for(j in 1:length(values_trees)){
    pred     <- ranger(formula = target~., 
                       data = train_rf.dt, 
                       num.trees = values_trees[j], 
                       mtry = values_mtry[i])
    error[i,j] <- pred$r.squared
  }
}

max_col <- max.col(error)
max_row <- which.max(c(error[1,max_col[1]], error[2,max_col[2]], error[3,max_col[3]], error[4,max_col[4]]))
t <- proc.time()
model_rf <- ranger(formula = target ~ ., 
                   data = train_rf.dt, 
                   num.trees = values_trees[max_col[max_row]],
                   mtry = values_mtry[max_row])

result_pred   <- predict(fit, test_rf.dt)
pred_rf_test <- result_pred$predictions
tim <- proc.time() - t

#MODELO OPTIMO PARA TARGET_VARS = C("CLASE_AS", "CNAE_Div_AS")
#1500 ARBOLES
#MTRY = 9.333
#TIEMPO system = 0.25
#R squared (OOB): 0.2490863

#MODELO OPTIMO PARA TARGET_VARS = C("CLASE_AS", "CNAE_Div_AS", "A10_i"):
#1250 ARBOLES
#MTRY = 9.333
#TIEMPO system = 0.16
#R squared (OOB): 0.2855142

#MODELO OPTIMO PARA TARGET_VARS = C("CLASE_AS", "CNAE_Div_AS", "D28"):
#PROBLEMA: Unsupported type of dependent variable

#MODELO OPTIMO PARA TARGET_VARS = C("CLASE_AS", "CNAE_Div_AS", "D28", "A10_i"):
#PROBLEMA: Unsupported type of dependent variable

#curva ROC
#################### Curvas ROC ######################


curvaROC1 <- roc(response = test_rf.dt[, target],
                 predictor =   pred_rf_test)
AUC1 <- auc(curvaROC1)

#roc.list <- list(curvaROC1, curvaROC2, curvaROC3)#comparar diferentes roc
#names(roc.list) <- c("ProbError", "MomError", "DesignWeight")

ggroc(curvaROC1, size = 1.6, color = "gray22") +
  geom_abline(slope = 1, intercept = 1, linetype = 'dotted', size = 1.1, color = "gray22") +
  annotate("text", x = 0.25, y = 0.05, label = as.character(round(AUC1,2))) +
  labs(title = paste0('ROC curves for CLASE_AS, A10_i and CNAE_Div_AS'),
       color = 'Variable', linetype = 'Variable') +
  theme_bw() +
  theme(plot.title   = element_text(hjust = 0.5, size = 16, color = "darkgreen"),
        axis.title   = element_text(hjust = 0.5, size = 12, color = "green4", face = "bold")) 

#ggplot(data.frame(x = test_rf.dt$target, y = pred_rf_test), aes(x = x, y = y)) +
#  geom_point() +
#  geom_abline(slope = 1, intercept = 0) +
#  labs(title = 'Train Set', x = 'True value', y = 'Predicted value') +
#  theme_bw()
