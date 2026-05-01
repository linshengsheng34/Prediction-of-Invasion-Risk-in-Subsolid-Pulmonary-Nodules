
library(randomForest)
library(DMwR)
library(shapviz)
library(pbapply)
library(rlang)
library(tidyverse)
library(reshape2)
library(openxlsx)
library(DALEX)
library(readr)
library(gbm) 
library(dplyr)
library(caret)
library(ggplot2)
library(pROC)
library(rms)
library(rmda)
library(dcurves)
library(Hmisc)
library(ResourceSelection)
library(DynNom)
library(survey)
library(caret)
library(foreign)
library(plotROC)
library(survival)
library(shapper)
library(iml)
library(e1071)
library(ROCR)
library(corrplot)
library(lattice)
library(Formula)
library(SparseM)
library(survival)
library(riskRegression)
library(pheatmap)
library(fastshap)
library(naivebayes)
library(ingredients)
library(mlr3)
library(table1)
library(tableone)
library(adabag)
library(RColorBrewer)
library(VIM)
library(mice)
library(autoReg)
library(cvms)
library(tibble)
library(plotROC)
library(pROC)
library(ggplot2)
library(cvms)
library(tibble)
library(corrplot)
library(data.table)
library(pheatmap)
library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)
library(ROSE)
library(scales)
library(plotROC)
library(pROC)
library(ggplot2)
library(kernelshap)  
library(gridExtra)

data=read.csv("data.csv",header = T,encoding = "GBK")
colnames(data)

data$Result = factor(data$Result,levels = c(0,1),labels = c('No','Yes'))   
data$Margin = factor(data$Margin,levels = c(0,1),labels = c('Well_defined','Ill_defined'))
data$Spiculation = factor(data$Spiculation,levels = c(0,1),labels = c('No','Yes'))
data$Lobulation = factor(data$Lobulation,levels = c(0,1),labels = c('No','Yes'))
data$Pleural_indentation = factor(data$Pleural_indentation,levels = c(0,1),labels = c('No','Yes'))
data$Vascular_convergence_sign = factor(data$Vascular_convergence_sign,levels = c(0,1),labels = c('No','Yes'))

traindata <- data[1:586, ]      
valid <- data[587:833, ]     


nrow(traindata)  
nrow(valid)  

x = traindata  

x1 = colnames(x[,7:ncol(x)])


x2 = colnames(x[,2:6])

CreateTableOne(data=x)

myVars = colnames(x[,2:ncol(x)])

catVars = colnames(x[,2:6])


colnames(x)

var=c("Result",
      "Margin","Spiculation","Lobulation","Pleural_indentation","Vascular_convergence_sign","Long_diameter","Solid_component_proportion","Skewness","Entropy")


data=read.csv("data.csv",header = T,encoding = "GBK")
colnames(data)

data$Result = factor(data$Result,levels = c(0,1),labels = c('No','Yes'))
traindata <- data[1:586, ]      
testdata <- data[587:833, ]     
dev = traindata
vad = testdata

dev = dev[,var]
vad = vad[,var]
dev$Result = factor(as.character(dev$Result))

models = c("glm","svmRadial","gbm","nnet","rf","xgbTree","AdaBoost.M1")

models_names = list(Logistic="glm",SVM="svmRadial",GBM="gbm",NeuralNetwork="nnet",RandomForest="rf",Xgboost="xgbTree",Adaboost="AdaBoost.M1")


glm.tune.grid = NULL
svm.tune.grid = expand.grid(
  sigma = c(0.001, 0.005, 0.01, 0.05, 0.1),
  C = c(0.1, 1, 5, 10, 50)
)
gbm.tune.grid = expand.grid(
  n.trees = c(500, 1000),
  interaction.depth = c(2, 3, 5),
  shrinkage = c(0.01, 0.05),
  n.minobsinnode = 5
)
nnet.tune.grid = expand.grid(
  size = c(3, 5, 7),
  decay = c(0.001, 0.01, 0.1)
)
rf.tune.grid = expand.grid(mtry = c(2,3,4,5))

xgb.tune.grid = expand.grid(
  nrounds = c(200, 500),
  max_depth = c(3, 5),
  eta = c(0.01, 0.05),
  gamma = c(0, 0.1),
  colsample_bytree = 0.8,
  min_child_weight = c(1, 3),
  subsample = 0.8
)

ada.tune.grid = expand.grid(
  mfinal = c(50, 100, 150),
  maxdepth = c(1, 2, 3),
  coeflearn = c("Zhu","Breiman")
)

Tune_table = list(glm = glm.tune.grid,
                  svmRadial = svm.tune.grid,
                  gbm = gbm.tune.grid,
                  nnet = nnet.tune.grid,
                  rf = rf.tune.grid,
                  xgbTree = xgb.tune.grid,
                  
                  AdaBoost.M1 = ada.tune.grid
)


train_probe = data.frame(Result = dev$Result)
test_probe = data.frame(Result = vad$Result)

importance = list()

ML_calss_model = list()
set.seed(123)
train.control <- trainControl(method = 'repeatedcv',
                              number = 10, 
                              repeats = 5, 
                              classProbs = TRUE, 
                              summaryFunction = twoClassSummary)

pb = txtProgressBar(min = 0, max = length(models), style = 3)
for (i in seq_along(models)) {
  model <- models[i]
  model_name <- names(models_names)[which(models_names == model)]  
  set.seed(52)
  fit = train(Result~.,
              data = dev,
              tuneGrid = Tune_table[[model]],
              metric='ROC',
              method= model,
              trControl=train.control)
  
  train_Pro = predict(fit, newdata = dev, type = 'prob')
  test_Pro = predict(fit, newdata = vad, type = 'prob')
  
  train_probe[[model_name]] <- train_Pro$Yes
  test_probe[[model_name]] <- test_Pro$Yes
  
  ML_calss_model[[model_name]] = fit  
  importance[[model_name]] = varImp(fit, scale = TRUE)  
  
  setTxtProgressBar(pb, i)
}

close(pb)  


models_names = list(Logistic="glm",SVM="svmRadial",GBM="gbm",NeuralNetwork="nnet",RandomForest="rf",
                    Xgboost="xgbTree",Adaboost="AdaBoost.M1")

Train = train_probe
Test = test_probe

cutpoint = 3  

datalist = list(Train= train_probe,
                Test= test_probe)


for (newdata_tt in names(datalist)) {
  
  newdata = datalist[[newdata_tt]]
  formula = as.formula(paste0("Result ~ ", paste(colnames(newdata)[2:ncol(newdata)], collapse = " + ")))
  trellis.par.set(caretTheme())
  cal_obj = calibration(formula, data = newdata, class = 'Yes', cuts = cutpoint)
  caldata <- as.data.frame(cal_obj$data)
  caldata <- na.omit(caldata)
  
  y_true = ifelse(newdata$Result == "Yes", 1, 0)
  
  stat_df = data.frame(Model = character(), Brier = numeric(), Slope = numeric(), Intercept = numeric())
  
  for (model_name in names(models_names)) {
    if (!model_name %in% colnames(newdata)) next
    
    p = newdata[[model_name]]
    p[is.na(p)] = 0
    
    brier = mean((p - y_true)^2)
    
    fit_lm = lm(y_true ~ p)
    slope = coef(fit_lm)[2]
    intercept = coef(fit_lm)[1]
    
    stat_df = rbind(stat_df, data.frame(
      Model = model_name,
      Brier = round(brier, 3),
      Slope = round(slope, 3),
      Intercept = round(intercept, 3)
    ))
  }
  
  legend_labels <- paste0(
    stat_df$Model,
    "  B:", stat_df$Brier,
    "  S:", stat_df$Slope,
    "  I:", stat_df$Intercept
  )
  names(legend_labels) <- stat_df$Model
  
  annotate_df <- stat_df
  annotate_df$label <- paste0(
    stat_df$Model,
    "\nSlope=", stat_df$Slope,
    "\nIntercept=", stat_df$Intercept
  )
  
   Calibrat_plot <- ggplot(caldata, aes(x = midpoint, y = Percent, group = calibModelVar, color = calibModelVar)) +
    geom_point(size = 1.2) +
    geom_line(linewidth = 0.7) +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dotdash") +
    
    ggtitle("Calibration curve") +
    xlab("Predicted Probability") +
    ylab("Observed Event Rate") +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
      axis.text = element_text(size = 12, face = "bold"),
      
      legend.text = element_text(size = 14, face = "bold"),  
      legend.title = element_text(size = 12, face = "bold"),
     
      legend.position = c(0.68, 0.17),
      legend.background = element_blank(),
      axis.title = element_text(size = 12, face = "bold"),
      panel.border = element_rect(color = "black", size = 1)
    ) +
    scale_color_discrete(
      name = "Model  Brier  Slope  Intercept",
      labels = legend_labels
    )
  
  pdf(paste0(newdata_tt, "_Calibration_with_slope_intercept.pdf"), 7, 7, family = "serif")
  print(Calibrat_plot)
  dev.off()
  
  write.csv(stat_df, paste0(newdata_tt, "_calibration_stats.csv"), row.names = FALSE)
  
  if (!exists("best_thresholds")) {
    best_thresholds = list()
  }
  
  ROC_list = list()
  ROC_label = list()
  AUC_metrics = data.frame()
  Evaluation_metrics = data.frame(Model = NA,Threshold=NA,Accuracy=NA,Sensitivity=NA,Specificity=NA,Precision=NA,F1=NA)
  cm_plot_list = list()
  
  for (model_name in names(models_names)) {
    ROC = roc(response=newdata$Result, predictor=newdata[,model_name])
    AUC = round(auc(ROC),3)
    CI = ci.auc(ROC)
    label = paste0(model_name, " (AUC=",sprintf("%0.3f", AUC), ",95%CI:",sprintf("%0.3f", CI[1]),"-",sprintf("%0.3f", CI[3]),")")
    
    if (newdata_tt == "Train") {
      bestp = ROC$thresholds[which.max(ROC$sensitivities + ROC$specificities - 1)]
      best_thresholds[[model_name]] = bestp
    } else {
      bestp = best_thresholds[[model_name]]
    }
    
    predlab = as.factor(ifelse(newdata[,model_name] > bestp,"Yes","No"))
    index_table = confusionMatrix(data = predlab, reference = newdata$Result, positive = "Yes", mode="everything")
    mydata = data.frame(reference=newdata$Result,prediction=predlab)
    mytibble = as.tibble(table(mydata))
    
    confusion_plot = plot_confusion_matrix(
      mytibble, target_col = "reference", prediction_col = "prediction",
      counts_col = "n", add_counts = TRUE, add_normalized = TRUE,
      palette = "Blues", theme_fn = ggplot2::theme_minimal
    )+
      ggtitle("") +
      xlab(model_name) +  
      theme(
        plot.title = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold", hjust = 0.5) 
      )
    cm_plot_list[[model_name]] = confusion_plot
    
    Evaluation_metrics = rbind(Evaluation_metrics,
                               data.frame(
                                 Model = model_name, Threshold = round(bestp,3),
                                 Accuracy = sprintf("%0.3f",index_table[["overall"]][["Accuracy"]]),
                                 Sensitivity = sprintf("%0.3f",index_table[["byClass"]][["Sensitivity"]]),
                                 Specificity = sprintf("%0.3f",index_table[["byClass"]][["Specificity"]]),
                                 Precision = sprintf("%0.3f",index_table[["byClass"]][["Precision"]]),
                                 F1 = sprintf("%0.3f",index_table[["byClass"]][["F1"]])
                               ))
    ROC_label[[model_name]] = label
    ROC_list[[model_name]] = ROC
  }
  
  while(length(cm_plot_list) < 7){ cm_plot_list[[length(cm_plot_list)+1]] = grid::nullGrob() }
  pdf(paste0(newdata_tt,"_All_CM.pdf"),11,5)
  grid.arrange(grobs = cm_plot_list, nrow = 2, ncol = 4)
  dev.off()
  Evaluation_metrics = Evaluation_metrics[-1,]
  write.csv(Evaluation_metrics, paste0(newdata_tt,"_Evaluation_metrics.csv"), row.names = FALSE)
  
  ROC_plot=pROC::ggroc(ROC_list,size=1.0,legacy.axes = T)+theme_bw()+
    labs(title = ' ROC curve')+
    theme(plot.title = element_text(hjust = 0.5,size = 15,face="bold"),
          axis.text=element_text(size=12,face="bold"),
          legend.title = element_blank(),
          legend.text = element_text(size=14,face="bold"),
          legend.position=c(0.65,0.18),
          legend.background = element_blank(),
          axis.title.y = element_text(size=12,face="bold"),
          axis.title.x = element_text(size=12,face="bold"),
          panel.border = element_rect(color="black",size=1),
          panel.background = element_blank())+
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),colour='grey',linetype = 'dotdash')+
    scale_colour_discrete(breaks=c(names(models_names)), labels=c(ROC_label))
  pdf(paste0(newdata_tt,"_ROC.pdf"),7,7,family = "serif")
  print(ROC_plot)
  dev.off()
  
  dca_data = newdata
  dca_data$Result=ifelse(dca_data$Result=="Yes",1,0)
  DCA_list = list()
  for (model_name in names(models_names)) {
    dca_formula = as.formula(paste("Result ~", model_name))
    set.seed(123)
    dca_curvers = decision_curve(dca_formula, data = dca_data, study.design = "cohort", bootstraps = 50)
    DCA_list[[model_name]] = dca_curvers
  }
  dca = setNames(DCA_list, names(DCA_list))
  pdf(paste0(newdata_tt,"DCA.pdf"),7,7,family = "serif")
  plot_decision_curve(dca, curve.names = c(names(models_names)),
                      cost.benefit.axis = F, confidence.intervals = "none", lwd = 2, legend.position ="bottomleft")+
    theme(plot.title = element_text(hjust = 0.5,size = 15,face="bold"),
          axis.text=element_text(size=12,face="bold"), legend.title = element_blank(),
          legend.text = element_text(size=16,face="bold"), legend.background = element_blank(),
          axis.title.y = element_text(size=12,face="bold"), axis.title.x = element_text(size=12,face="bold"),
          panel.border = element_rect(color="black",size=1), panel.background = element_blank())
  
  ）
  title("Decision curve", cex.main=1.5, font=2, col="black")
  dev.off()
  write.csv(dca_data,paste0(newdata_tt,"_PRplot.csv"),row.names = F)
}

library(PredictABEL)

y_train <- as.integer(train_probe$Result == "Yes")
y_test  <- as.integer(test_probe$Result  == "Yes")
p_rf_train  <- train_probe$RandomForest
p_xgb_train <- train_probe$Xgboost
p_rf_test   <- test_probe$RandomForest
p_xgb_test  <- test_probe$Xgboost

calc_nri_idi <- function(y, p_std, p_new, dataset_label) {
  
  
  res <- improveProb(
    x1    = p_std,   
    x2    = p_new,   
    y     = y        
  )
  
  
  metrics <- c("NRI", "NRI+", "NRI-", "IDI")
  
  estimates <- c(res$nri,   res$nri.ev,  res$nri.ne,  res$idi)
  ses       <- c(res$se.nri, res$se.nri.ev, res$se.nri.ne, res$se.idi)
  
  z_vals <- estimates / ses
  p_vals <- 2 * pnorm(-abs(z_vals))
  ci_low <- estimates - 1.96 * ses
  ci_up  <- estimates + 1.96 * ses
  
  data.frame(
    Dataset  = dataset_label,
    Metric   = metrics,
    Estimate = round(estimates, 4),
    CI_Lower = round(ci_low,    4),
    CI_Upper = round(ci_up,     4),
    Z_value  = round(z_vals,    4),
    P_value  = formatC(p_vals, format = "e", digits = 3),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}


result_train <- calc_nri_idi(y_train, p_rf_train, p_xgb_train, "Train")
result_test  <- calc_nri_idi(y_test,  p_rf_test,  p_xgb_test,  "Test")
result_all   <- rbind(result_train, result_test)

print(result_all, row.names = FALSE)

write.csv(result_all, "RF_vs_XGB_NRI_IDI.csv", row.names = FALSE)



####################SHAP

names(models_names)

best_Model_key <- "RandomForest" 
rf_model <- ML_calss_model[[best_Model_key]]

pred_fun <- function(model, newdata) {
  predict(model, newdata, type = "prob")[, "Yes"]
}

X <- dev[, -1]
set.seed(123)
bg_X <- X[sample(1:nrow(X), min(100, nrow(X))), ]


explain_kernel <- kernelshap(
  object = rf_model,
  X = X, 
  pred_fun = pred_fun,
  bg_X = bg_X,
  parallel = FALSE 
)

shap_value <- shapviz(explain_kernel, X_pred = X, interactions = TRUE)


pdf(paste0("SHAP_", best_Model_key, "_importance_beeswarm.pdf"), 8, 5)
sv_importance(shap_value, kind = "beeswarm", size = 0.8,
              viridis_args = list(begin = 0.25, end = 0.85, option = "B"),
              show_numbers = F) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 14))
dev.off()


pdf(paste0("SHAP_", best_Model_key, "_importance_bar.pdf"), 8, 5)
sv_importance(shap_value, kind = "bar", fill = "#2E8B57", show_numbers = F) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(face = "bold", size = 14))
dev.off()


target_row <- 1 
pdf(paste0("SHAP_", best_Model_key, "_waterfall.pdf"), width = 8, height = 6)
sv_waterfall(shap_value, row_id =24, fill_colors = c("#f7d13d", "#a52c60")) +
  theme_bw(base_size = 16) +
  theme(text = element_text(face = "bold", colour = "black"))
dev.off()


vars <- c("Margin", "Spiculation", "Lobulation", "Pleural_indentation", 
          "Vascular_convergence_sign", "Long_diameter", "Solid_component_proportion", 
          "Skewness", "Entropy")

plots <- lapply(vars, function(v) {
  sv_dependence(shap_value, v = v, size = 0.8, alpha = 0.7) +
    scale_color_viridis_c(option = "viridis", name = v) +
    theme_bw() +
    theme(legend.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 11, face = "bold"))
})

pdf(paste0("SHAP_", best_Model_key, "_dependence_gradient.pdf"), 16, 10)
do.call(gridExtra::grid.arrange, c(plots, ncol = 3))
dev.off()

####################################################################

saveRDS(rf_model, "rf_model.rds")
saveRDS(dev[, -1], "train_template.rds")

