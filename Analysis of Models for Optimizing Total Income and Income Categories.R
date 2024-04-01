library(readxl)
library(caret)
library(dplyr)
library(ggplot2)
library(plotly)
library(caret)
library(randomForest)
#install.packages("Metrics")
library(Metrics)
library(e1071)
#install.packages("xgboost")
library(xgboost)

#Read data excel
data <- read_excel("D:/Downloads/dataset_sml_2023.xlsx")
View(data)

#Mengambil data subdistrict 184 dan subdistrict 185
filtered_data <- data[data$subdistrict %in% c('subdistrict_184', 'subdistrict_185'), ]
View(filtered_data)

#Mengecek jumlah dari masing-masing subdistrict
table(filtered_data$subdistrict)

#Cek summary dan tipe data
summary(filtered_data)
str(filtered_data)

#mengecek missing value
missing_values <- sapply(filtered_data, function(x) sum(is.na(x)))
View(missing_values)

#mengecek variabel dengan tipe character
lapply(filtered_data, class)
names(lapply(filtered_data, class)[sapply(lapply(filtered_data, class), function(x) x == "character")])
filtered_data$revenue_category
filtered_data$subdistrict

#Mengkodekan tipe data kategori
datafix <- filtered_data %>%
  mutate(revenue_category = recode(revenue_category,
                                           "Low" = 1,
                                           "Medium" = 2,
                                           "High" = 3))
datafix <- datafix %>%
  mutate(subdistrict = recode(subdistrict,
                                   "subdistrict_184" = 1,
                                   "subdistrict_185" = 2))

#Membuang variabel dt_id
datafix <- datafix[, -which(names(datafix) == "dt_id")]
View(datafix)

#===========================================NOMOR 1===============================================#

##=========================================SUBDISTRICT 184========================================##
#Mengambil data subdistrict 184
datasub184 <- datafix[datafix$subdistrict %in% c(1), ]
View(datasub184)
dim(datasub184)
datasub184$subdistrict

#Membuang variabel subdistrict
datasub184 <- datasub184[, -which(names(datasub184) == "subdistrict")]

#Mengidentifikasi dan menghapus variabel yang varians mendekati nol
nearzerovar184 <- nearZeroVar(datasub184);nearzerovar184
names(datasub184)[nearzerovar184]
datafil184 <- datasub184[, -nearzerovar184]

#Cek korelasi setelah penghapusan variabel yang variansnya mendekati nol
cor184 <- cor(datafil184);View(cor184)

#Uji Univariat (Variabel target = revenue_total)
##Mengambil dan menampilkan variabel yang berkorelasi tinggi terhadap revenue_total dengan korelasi di atas 0,7
highcor184 <- which(abs(cor184["revenue_total", ])>0.7);highcor184
print(names(datafil184)[highcor184])

##Mengambil data dari variabel yang memiliki korelasi tinggi
highdata184 <- datafil184[, highcor184 ]; highdata184

##Cek Variabel yang signifikan terhadap revenue_total pada subdistrict 184
unianalysis184 <- lapply(names(highdata184), function(var) {
  if (var != "target_variable") {
    unitest_result184 <- t.test(highdata184[[var]], highdata184$revenue_total)
    return(data.frame(variable = var, p_value =  unitest_result184$p.value))
  }
})

dataresult184 <- do.call(rbind,unianalysis184)
dataresult184$Signifikansi <- ifelse(dataresult184$p_value < 0.05, "Signifikan", "Tidak Signifikan");dataresult184

#Data Baru Subdistrict184 (variabel target = revenue_total)
datafix184 <- highdata184; View(datafix184)
dim(datafix184)

#membagi data menjadi training dan testing (target variabel = revenue_total)
jumlah184 <- round(nrow(datafix184) * 0.8)
indeks184 <- sample(1:nrow(datafix184), jumlah184)
training184 <- datafix184[indeks184, ]
testing184 <- datafix184[-indeks184, ]

dim(training184)
dim(testing184)

#Uji Univariat (Variabel target = revenue_category)
##Mengambil dan menampilkan variabel yang berkorelasi tinggi terhadap revenue_category dengan korelasi di atas 0,7
highcor_cat184 <- which(abs(cor184["revenue_category", ])>0.7);highcor_cat184
print(names(datafil184)[highcor_cat184])

##Mengambil data dari variabel yang memiliki korelasi tinggi
highdata_cat184 <- datafil184[, highcor_cat184 ]; highdata_cat184

##Cek Variabel yang signifikan terhadap revenue_category pada subdistrict 184
unianalysis_cat184 <- lapply(names(highdata_cat184), function(var) {
  if (var != "target_variable") {
    kruskal_result_cat184 <- kruskal.test(highdata_cat184[[var]] ~ highdata_cat184$revenue_category)
    return(data.frame(variable = var, p_value = kruskal_result_cat184$p.value))
  }
})

dataresult_cat184 <- do.call(rbind,unianalysis_cat184)
dataresult_cat184$Signifikansi <- ifelse(dataresult_cat184$p_value < 0.05, "Signifikan", "Tidak Signifikan");dataresult_cat184

#Data Baru Subdistrict184 (variabel target = revenue_total)
datafix_cat184 <- highdata_cat184; View(datafix_cat184)
dim(datafix_cat184)

#membagi data menjadi training dan testing (target variabel = revenue_category)
jumlah_cat184 <- round(nrow(datafix_cat184) * 0.8)
indeks_cat184 <- sample(1:nrow(datafix_cat184), jumlah_cat184)
training_cat184 <- datafix_cat184[indeks_cat184, ]
testing_cat184 <- datafix_cat184[-indeks_cat184, ]

dim(training_cat184)
dim(testing_cat184)

##=========================================SUBDISTRICT 185========================================##
##Mengambil data subdistrict 185
datasub185 <- datafix[datafix$subdistrict %in% c(2), ]
View(datasub185)
datasub185
datasub185$subdistrict

#Membuang variabel subdistrict
datasub185 <- datasub185[, -which(names(datasub185) == "subdistrict")]

#Mengidentifikasi dan menghapus variabel yang varians mendekati nol
nearzerovar185 <- nearZeroVar(datasub185);nearzerovar185
names(datasub185)[nearzerovar185]
datafil185 <- datasub185[, -nearzerovar185]

#Cek korelasi setelah penghapusan variabel yang variansnya mendekati nol
cor185 <- cor(datafil185);View(cor185)

#Uji Univariat (Variabel target = revenue_total)
##Mengambil dan menampilkan variabel yang berkorelasi tinggi terhadap revenue_total dengan korelasi di atas 0,7
highcor185 <- which(abs(cor185["revenue_total", ])>0.7);highcor185
print(names(datafil185)[highcor185])

##Mengambil data dari variabel yang memiliki korelasi tinggi
highdata185 <- datafil185[, highcor185 ]; highdata185

##Cek Variabel yang signifikan terhadap revenue_total pada subdistrict 185
unianalysis185 <- lapply(names(highdata185), function(var) {
  if (var != "target_variable") {
    unitest_result185 <- t.test(highdata185[[var]], highdata185$revenue_total)
    return(data.frame(variable = var, p_value =  unitest_result185$p.value))
  }
})

dataresult185 <- do.call(rbind,unianalysis185)
dataresult185$Signifikansi <- ifelse(dataresult185$p_value < 0.05, "Signifikan", "Tidak Signifikan");dataresult185

#Data Baru Subdistrict184 (variabel target = revenue_total)
datafix185 <- highdata185; View(datafix185)
dim(datafix185)

#membagi data menjadi training dan testing (target variabel = revenue_total)
jumlah185 <- round(nrow(datafix185) * 0.8)
indeks185 <- sample(1:nrow(datafix185), jumlah185)
training185 <- datafix185[indeks185, ]
testing185 <- datafix185[-indeks185, ]

dim(training185)
dim(testing185)

#Uji Univariat (Variabel target = revenue_category)
##Mengambil dan menampilkan variabel yang berkorelasi tinggi terhadap revenue_category dengan korelasi di atas 0,7
highcor_cat185 <- which(abs(cor185["revenue_category", ])>0.7);highcor_cat185
print(names(datafil184)[highcor_cat185])

##Mengambil data dari variabel yang memiliki korelasi tinggi
highdata_cat185 <- datafil185[, highcor_cat185 ]; highdata_cat185

##Cek Variabel yang signifikan terhadap revenue_category pada subdistrict 184
unianalysis_cat185 <- lapply(names(highdata_cat185), function(var) {
  if (var != "target_variable") {
    kruskal_result_cat185 <- kruskal.test(highdata_cat185[[var]] ~ highdata_cat185$revenue_category)
    return(data.frame(variable = var, p_value = kruskal_result_cat185$p.value))
  }
})

dataresult_cat185 <- do.call(rbind,unianalysis_cat185)
dataresult_cat185$Signifikansi <- ifelse(dataresult_cat185$p_value < 0.05, "Signifikan", "Tidak Signifikan");dataresult_cat185

#Data Baru Subdistrict184 (variabel target = revenue_total)
datafix_cat185 <- highdata_cat185; View(datafix_cat185)
dim(datafix_cat185)

#membagi data menjadi training dan testing (target variabel = revenue_category)
jumlah_cat185 <- round(nrow(datafix_cat185) * 0.8)
indeks_cat185 <- sample(1:nrow(datafix_cat185), jumlah_cat185)
training_cat185 <- datafix_cat185[indeks_cat185, ]
testing_cat185 <- datafix_cat185[-indeks_cat185, ]

dim(training_cat185)
dim(testing_cat185)

#===========================================Nomor 2===============================================
##=========================================SUBDISTRICT 184========================================##

#Melakukan Normalisasi data training
normalize <- function(x) {
  min_val <- min(x)
  max_val <- max(x)
  
  if (min_val == max_val) {
    return(rep(0, length(x)))
  } else {
    return((x - min_val) / (max_val - min_val))
  }
}

normdata_train184 <- as.data.frame(lapply(training184, normalize))

print("Data Sebelum Normalisasi:")
training184
print("Data Sesudah Normalisasi:")
normdata_train184

#=====Metode Random Forest=====
target184 <- normdata_train184$revenue_total
data184 <- normdata_train184[, -which(names(normdata_train184) %in% c("revenue_total", "revenue_category"))]
set.seed(123)  
rf_model184 <- randomForest(x = data184, y = target184, ntree = 500, method = "rf")
rf_model184

#Cek importance variabel
importance184 <- importance(rf_model184)
importance184
importance184 <- data.frame(importance184)

#Membuat plot importance variabel
varImpPlot(rf_model184, color = "darkblue", cex = 1)

#Mengurutkan importance variabel dari besar ke kecil
Best184 <- arrange(importance184, desc(IncNodePurity))

#Mengecek variabel yang lebih dari Threshold
significant_variables184 <- Best184 %>%
  filter(IncNodePurity > 0.1)
significant_variables184

#=====Extreme Gradient Boosting (XGBoost)=====
target184 <- normdata_train184$revenue_total
data184 <- normdata_train184[, -which(names(normdata_train184) %in% c("revenue_total", "revenue_category"))]
xgb_model184 <- xgboost(data = as.matrix(data184), label = target184, nrounds = 500, objective = "reg:squarederror")
xgb_model184

#Cek importance variabel
importance_xgb184 <- xgb.importance(model = xgb_model184)
importance_xgb184
importance_xgb184 <- data.frame(importance_xgb184)

#Mengurutkan importance variabel dari besar ke kecil
Bestxgb184 <- arrange(importance_xgb184, desc(Gain))
Bestxgb184

#Membuat scatter plot dengan urutan dari variabel Gain terbesar ke terkecil
scatter_plot1 <- ggplot(Bestxgb184, aes(x = Gain, y = reorder(Feature, Gain))) +
  geom_point(color = "darkblue", size = 3) +
  labs(title = "XGBoost Feature Importance - Scatter Plot",
       x = "Gain",
       y = "Feature") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  scale_y_discrete(limits = rev(Bestxgb184$Feature)) +  
  xlim(c(0, max(Bestxgb184$Gain) * 1.2))
scatter_plot1

#Convert dari ggplot ke plotly agar lebih interaktif
plotly_ggplot1 <- ggplotly(scatter_plot1)
plotly_ggplot1

#Mengecek variabel yang lebih dari Threshold
significant_variablesxgb184 <- Bestxgb184 %>%
  filter(Gain > 0.001)
significant_variablesxgb184

##=========================================SUBDISTRICT 185========================================##

#Melakukan Normalisasi data training
normdata_train185 <- as.data.frame(lapply(training185, normalize))

print("Data Sebelum Normalisasi:")
training185
print("Data Sesudah Normalisasi:")
normdata_train185

#=====Metode Random Forest=====
target185 <- normdata_train185$revenue_total
data185 <- normdata_train185[, -which(names(normdata_train185) %in% c("revenue_total", "revenue_category"))]
set.seed(123)  
rf_model185 <- randomForest(x = data185, y = target185, ntree = 500)
rf_model185

#Mencari Future Important
importance185 <- importance(rf_model185)
importance185
importance185 <- data.frame(importance185)
varImpPlot(rf_model185,color = "darkblue", cex = 1)

Best185 <- arrange(importance185, desc(IncNodePurity))

#Mengecek variabel yang lebih dari Threshold
significant_variables185 <- Best185 %>%
  filter(IncNodePurity > 0.1)
significant_variables185

#=====Metode Extreme Gradient Boosting (XGBoost)=====
target185 <- normdata_train185$revenue_total
data185 <- normdata_train185[, -which(names(normdata_train185) %in% c("revenue_total", "revenue_category"))]
xgb_model185 <- xgboost(data = as.matrix(data185), label = target185, nrounds = 500, objective = "reg:squarederror")
xgb_model185

#Mencari Future Important
importance_xgb185 <- xgb.importance(model = xgb_model185)
importance_xgb185
importance_xgb185 <- data.frame(importance_xgb185)

Bestxgb185 <- arrange(importance_xgb185, desc(Gain))
Bestxgb185

#Membuat scatter plot dengan urutan dari variabel Gain terbesar ke terkecil
scatter_plot2 <- ggplot(Bestxgb185, aes(x = Gain, y = reorder(Feature, Gain))) +
  geom_point(color = "darkblue", size = 3) +
  labs(title = "XGBoost Feature Importance - Scatter Plot",
       x = "Gain",
       y = "Feature") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  scale_y_discrete(limits = rev(Bestxgb185$Feature)) +  
  xlim(c(0, max(Bestxgb185$Gain) * 1.2))
scatter_plot2

#Convert dari ggplot ke plotly agar lebih interaktif
plotly_ggplot2 <- ggplotly(scatter_plot2)
plotly_ggplot2

significant_variablesxgb185 <- Bestxgb185 %>%
  filter(Gain > 0.001)
significant_variablesxgb185

#===========================================Nomor 3===============================================
##=========================================SUBDISTRICT 184========================================##

#=====Metode Random Forest=====
#MSE data training
train_predictions184 <- predict(rf_model184, newdata = data184)
train_mse184 <- mean((target184 - train_predictions184)^2)
train_rmse184 <- sqrt(mean((target184 - train_predictions184)^2))
print(paste("Mean Squared Error (MSE) data training:", train_mse184))
print(paste("Root Mean Squared Error (RMSE) data training:", train_rmse184))

#Plot hasil prediksi vs. observasi pada data training
plot(train_predictions184, target184, 
     main = "Prediction vs. Observation Training Data Subdistrict 184 (Revenue_Total)", 
     xlab = "Predicted", ylab = "Observed",abline(0, 1, col = "red"))
     
#Melakukan Normalisasi data testing
normdata_test184 <- as.data.frame(lapply(testing184, normalize))
print("Data Sebelum Normalisasi:")
testing184
print("Data Sesudah Normalisasi:")
normdata_test184

#Validasi data testing
validation_target184 <- normdata_test184$revenue_total
validation_predictors184 <- normdata_test184[, -which(names(normdata_test184) %in% c("revenue_total", "revenue_category"))]
validation_predictions184 <- predict(rf_model184, newdata = validation_predictors184)
mse184 <- mean((validation_target184 - validation_predictions184)^2)
rmse184 <- sqrt(mean((validation_target184 - validation_predictions184)^2))
print(paste("Mean Squared Error (MSE) Validasi:", mse184))
print(paste("Root Mean Squared Error (RMSE) Validasi:", rmse184))

#Plot hasil prediksi vs. observasi pada data training
plot(validation_predictions184, validation_target184, 
     main = "Prediction vs. Observation Testing Data Subdistrict 184 (Revenue_Total)", 
     xlab = "Predicted", ylab = "Observed",abline(lm(validation_target184 ~ validation_predictions184), col = "blue"))

#=====Metode Extreme Gradient Boosting (XGBoost)=====
#MSE data training
trainx_predictions184 <- predict(xgb_model184, newdata = as.matrix(data184))
trainx_mse184 <- mean((target184 - trainx_predictions184)^2)
trainx_rmse184 <- sqrt(mean((target184 - trainx_predictions184)^2))
print(paste("Mean Squared Error (MSE) data training:", trainx_mse184))
print(paste("Root Mean Squared Error (RMSE) data training:", trainx_rmse184))

#Plot hasil prediksi vs. observasi pada data training
plot(trainx_predictions184, target184, 
     main = "Prediction vs. Observation Training Data Subdistrict 184 (Revenue_Total)", 
     xlab = "Predicted", ylab = "Observed",abline(0, 1, col = "red"))

#Validasi data testing
validationx_target184 <- normdata_test184$revenue_total
validationx_predictors184 <- normdata_test184[, -which(names(normdata_test184) %in% c("revenue_total", "revenue_category"))]
validationx_predictions184 <- predict(xgb_model184, newdata = as.matrix(validationx_predictors184))
msex184 <- mean((validationx_target184 - validationx_predictions184)^2)
rmsex184 <- sqrt(mean((validationx_target184 - validationx_predictions184)^2))
print(paste("Mean Squared Error (MSE) Validasi:", msex184))
print(paste("Root Mean Squared Error (RMSE) Validasi:", rmsex184))

#Plot hasil prediksi vs. observasi pada data testing
plot(validationx_predictions184, validationx_target184, 
     main = "Prediction vs. Observation Testing Data Subdistrict 184 (Revenue_Total)", 
     xlab = "Predicted", ylab = "Observed",abline(lm(validationx_target184 ~ validationx_predictions184), col = "blue"))

##=========================================SUBDISTRICT 185========================================##
#=====Metode Random Forest=====
#MSE data training
train_predictions185 <- predict(rf_model185, newdata = data185)
train_mse185 <- mean((target185 - train_predictions185)^2)
train_rmse185 <- sqrt(mean((target185 - train_predictions185)^2))
print(paste("Mean Squared Error (MSE) data training:", train_mse185))
print(paste("Root Mean Squared Error (RMSE) data training:", train_rmse185))

#Plot hasil prediksi vs. observasi pada data training
plot(train_predictions185, target185, 
     main = "Prediction vs. Observation Training Data Subdistrict 185 (Revenue_Total)", 
     xlab = "Predicted", ylab = "Observed",abline(0, 1, col = "red"))

#Melakukan Normalisasi data testing
normdata_test185 <- as.data.frame(lapply(testing185, normalize))
print("Data Sebelum Normalisasi:")
testing185
print("Data Sesudah Normalisasi:")
normdata_test185

#Validasi data testing
validation_target185 <- normdata_test185$revenue_total
validation_predictors185 <- normdata_test185[, -which(names(normdata_test185) %in% c("revenue_total", "revenue_category"))]
validation_predictions185 <- predict(rf_model185, newdata = validation_predictors185)
mse185 <- mean((validation_target185 - validation_predictions185)^2)
rmse185 <- sqrt(mean((validation_target185 - validation_predictions185)^2))
print(paste("Mean Squared Error (MSE) Validasi:", mse185))
print(paste("Root Mean Squared Error (RMSE) Validasi:", rmse185))

#Plot hasil prediksi vs. observasi pada data testing
plot(validation_predictions185, validation_target185, 
     main = "Prediction vs. Observation Testing Data Subdistrict 185 (Revenue_Total)", 
     xlab = "Predicted", ylab = "Observed",abline(lm(validation_target185 ~ validation_predictions185), col = "blue"))

#=====Metode Extreme Gradient Boosting (XGBoost)=====
#MSE data training
trainx_predictions185 <- predict(xgb_model185, newdata = as.matrix(data185))
trainx_mse185 <- mean((target185 - trainx_predictions185)^2)
trainx_rmse185 <- sqrt(mean((target185 - trainx_predictions185)^2))
print(paste("Mean Squared Error (MSE) data training:", trainx_mse185))
print(paste("Root Mean Squared Error (RMSE) data training:", trainx_rmse185))

#Plot hasil prediksi vs. observasi pada data training
plot(trainx_predictions185, target185, 
     main = "Prediction vs. Observation Training Data Subdistrict 185 (Revenue_Total)", 
     xlab = "Predicted", ylab = "Observed",abline(0, 1, col = "red"))

#Validasi data testing
validationx_target185 <- normdata_test185$revenue_total
validationx_predictors185 <- normdata_test185[, -which(names(normdata_test185) %in% c("revenue_total", "revenue_category"))]
validationx_predictions185 <- predict(xgb_model185, newdata = as.matrix(validationx_predictors185))
msex185 <- mean((validationx_target185 - validationx_predictions185)^2)
rmsex185 <- sqrt(mean((validationx_target185 - validationx_predictions185)^2))
print(paste("Mean Squared Error (MSE) Validasi:", msex185))
print(paste("Root Mean Squared Error (RMSE) Validasi:", rmsex185))

#Plot hasil prediksi vs. observasi pada data testing
plot(validationx_predictions185, validationx_target185, 
     main = "Prediction vs. Observation Testing Data Subdistrict 185 (Revenue_Total)", 
     xlab = "Predicted", ylab = "Observed",abline(lm(validationx_target185 ~ validationx_predictions185), col = "blue"))

#===========================================Nomor 4===============================================

##=========================================SUBDISTRICT 184========================================##
#Melakukan Normalisasi data training
normalize <- function(x) {
  min_val <- min(x)
  max_val <- max(x)
  
  if (min_val == max_val) {
    return(rep(0, length(x)))
  } else {
    return((x - min_val) / (max_val - min_val))
  }
}

normdata_train_cat184 <- as.data.frame(lapply(training_cat184, normalize))

print("Data Sebelum Normalisasi:")
training_cat184
print("Data Sesudah Normalisasi:")
normdata_train_cat184


#=====Metode Random Forest=====
target_cat184 <- normdata_train_cat184$revenue_category
data_cat184 <- normdata_train_cat184[, -which(names(normdata_train_cat184) %in% c("revenue_category", "revenue_total"))]
set.seed(123)  
rf_model_cat184 <- randomForest(x = data_cat184, y = target_cat184, ntree = 500)
rf_model_cat184

importance_cat184 <- importance(rf_model_cat184)

importance_cat184
importance_cat184 <- data.frame(importance_cat184)
varImpPlot(rf_model_cat184,color = "darkblue", cex = 1)

Best_cat184 <- arrange(importance_cat184, desc(IncNodePurity))

significant_variables_cat184 <- Best_cat184 %>%
  filter(IncNodePurity > 0.1)
significant_variables_cat184

#=====Extreme Gradient Boosting (XGBoost)=====
targetx_cat184 <- normdata_train_cat184$revenue_category
datax_cat184 <- normdata_train_cat184[, -which(names(normdata_train_cat184) %in% c("revenue_category", "revenue_total"))]

tuned_params <- list(
  objective = "multi:softprob",
  max_depth = 6,
  eta = 0.3,
  subsample = 0.8,
  colsample_bytree = 0.8,
  num_class = length(unique(targetx_cat184))
)

xgb_model_cat184 <- xgboost(data = as.matrix(datax_cat184), label = as.integer(targetx_cat184), nrounds = 500, params = tuned_params)
xgb_model_cat184

importance_xgb_cat184 <- xgb.importance(model = xgb_model_cat184)
importance_xgb_cat184
importance_xgb_cat184 <- data.frame(importance_xgb_cat184)

Bestxgb_cat184 <- arrange(importance_xgb_cat184, desc(Gain))
Bestxgb_cat184

#Membuat scatter plot dengan urutan dari variabel Gain terbesar ke terkecil
scatter_plot3 <- ggplot(Bestxgb_cat184, aes(x = Gain, y = reorder(Feature, Gain))) +
  geom_point(color = "darkblue", size = 3) +
  labs(title = "XGBoost Feature Importance - Scatter Plot",
       x = "Gain",
       y = "Feature") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  scale_y_discrete(limits = rev(Bestxgb_cat184$Feature)) +  
  xlim(c(0, max(Bestxgb_cat184$Gain) * 1.2))
scatter_plot3

#Convert dari ggplot ke plotly agar lebih interaktif
plotly_ggplot3 <- ggplotly(scatter_plot3)
plotly_ggplot3

significant_variablesxgb_cat184 <- Bestxgb_cat184 %>%
  filter(Gain > 0.001)
significant_variablesxgb_cat184

##=========================================SUBDISTRICT 185========================================##
#Melakukan normalisasi data training
normdata_train_cat185 <- as.data.frame(lapply(training_cat185, normalize))

print("Data Sebelum Normalisasi:")
training_cat185
print("Data Sesudah Normalisasi:")
normdata_train_cat185

#=====Metode Random Forest=====
target_cat185 <- normdata_train_cat185$revenue_category
data_cat185 <- normdata_train_cat185[, -which(names(normdata_train_cat185) %in% c("revenue_category", "revenue_total"))]
set.seed(123)  
rf_model_cat185 <- randomForest(x = data_cat185, y = target_cat185, ntree = 500)
rf_model_cat185

importance_cat185 <- importance(rf_model_cat185)
importance_cat185
importance_cat185 <- data.frame(importance_cat185)
varImpPlot(rf_model_cat185, color = "darkblue", cex = 1)

Best_cat185 <- arrange(importance_cat185, desc(IncNodePurity))

significant_variables_cat185 <- Best_cat185 %>%
  filter(IncNodePurity > 0.1)
significant_variables_cat185

#=====Extreme Gradient Boosting (XGBoost)=====
targetx_cat185 <- normdata_train_cat185$revenue_category
datax_cat185 <- normdata_train_cat185[, -which(names(normdata_train_cat185) %in% c("revenue_category", "revenue_total"))]

tuned_params <- list(
  objective = "multi:softprob",
  max_depth = 6,
  eta = 0.3,
  subsample = 0.8,
  colsample_bytree = 0.8,
  num_class = length(unique(targetx_cat185))
)

xgb_model_cat185 <- xgboost(data = as.matrix(datax_cat185), label = as.integer(targetx_cat185), nrounds = 500, params = tuned_params)
xgb_model_cat185

importance_xgb_cat185 <- xgb.importance(model = xgb_model_cat185)
importance_xgb_cat185
importance_xgb_cat185 <- data.frame(importance_xgb_cat185)

Bestxgb_cat185 <- arrange(importance_xgb_cat185, desc(Gain))
Bestxgb_cat185

#Membuat scatter plot dengan urutan dari variabel Gain terbesar ke terkecil
scatter_plot4 <- ggplot(Bestxgb_cat185, aes(x = Gain, y = reorder(Feature, Gain))) +
  geom_point(color = "darkblue", size = 3) +
  labs(title = "XGBoost Feature Importance - Scatter Plot",
       x = "Gain",
       y = "Feature") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  scale_y_discrete(limits = rev(Bestxgb_cat185$Feature)) +  
  xlim(c(0, max(Bestxgb_cat185$Gain) * 1.2))
scatter_plot4

#Convert dari ggplot ke plotly agar lebih interaktif
plotly_ggplot4 <- ggplotly(scatter_plot4)
plotly_ggplot4

significant_variablesxgb_cat185 <- Bestxgb_cat185 %>%
  filter(Gain > 0.001)
significant_variablesxgb_cat185

#===========================================Nomor 5===============================================
##=========================================SUBDISTRICT 184========================================##

#=====Metode Random Forest=====
#Confussion Matrix, F1-score, Precision, dan Accuracy data training

##Membuat data frame untuk klasifikasi, yakni mengubah revenue_category menjadi faktor
fac_cat184 <- normdata_train_cat184 
fac_cat184$revenue_category <- as.factor(fac_cat184$revenue_category)

##Memisahkan target dan prediktor untuk revenue_category
fac_target_cat184 <- fac_cat184$revenue_category
fac_cat184 <- fac_cat184[, -which(names(fac_cat184) %in% c("revenue_category", "revenue_total"))]

#Prediksi data training
train_predictions_cat184 <- predict(rf_model_cat184, newdata = fac_cat184)

##Buat threshold value
threshold_low <- 0.25
threshold_high <- 0.75

##Konversi prediksi menjadi nilai kategorikal
train_predictions_cat184_categorical <- cut(train_predictions_cat184, 
                                            breaks = c(-Inf, threshold_low, threshold_high, Inf), 
                                            labels = c(0, 0.5, 1))

##Membuat Confusion matrix
conf_matrix184 <- table(train_predictions_cat184_categorical, fac_target_cat184)
conf_matrix184

##Menghitung Akurasi, Presisi, Recall, and F1-score pada data Training
accuracy_train_cat184 <- sum(train_predictions_cat184_categorical == fac_target_cat184) / length(fac_target_cat184)
precision_train_cat184 <- conf_matrix184[3, 3] / sum(conf_matrix184[, 3])
recall_train_cat184 <- conf_matrix184[3, 3] / sum(conf_matrix184[3, ])
f1score_train_cat184 <- 2 * (precision_train_cat184 * recall_train_cat184) / (precision_train_cat184 + recall_train_cat184)

cat("Accuracy Data Training Subdistrict 184 Metode Random Forest:", accuracy_train_cat184 , "\n")
cat("Precision Data Training Subdistrict 184 Metode Random Forest:", precision_train_cat184, "\n")
cat("Recall Data Training for Subdistrict 184 Metode Random Forest:", recall_train_cat184, "\n")
cat("F1-score Data Training for Subdistrict 184 Metode Random Forest:", f1score_train_cat184, "\n")

#Confussion Matrix, F1-score, Precision, dan Accuracy data testing

##Melakukan Normalisasi data testing
normdata_test_cat184 <- as.data.frame(lapply(testing_cat184, normalize))
print("Data Sebelum Normalisasi:")
testing184
print("Data Sesudah Normalisasi:")
normdata_test_cat184

##Membuat data frame untuk klasifikasi, yakni mengubah revenue_category menjadi faktor (data testing)
fact_cat184 <- normdata_test_cat184
fact_cat184$revenue_category <- as.factor(fact_cat184$revenue_category)

##Memisahkan target dan prediktor untuk revenue_category
fact_target_cat184 <- fact_cat184$revenue_category
fact_cat184 <- fact_cat184[, -which(names(fact_cat184) %in% c("revenue_category", "revenue_total"))]

# Prediksi pada data testing yang sudah dinormalisasi
test_predictions_cat184 <- predict(rf_model_cat184, fact_cat184)


##Buat threshold value
threshold_low <- 0.25
threshold_high <- 0.75

##Konversi prediksi menjadi nilai kategorikal
test_predictions_cat184_categorical <- cut(test_predictions_cat184, 
                                            breaks = c(-Inf, threshold_low, threshold_high, Inf), 
                                            labels = c(0, 0.5, 1))

##Membuat Confusion matrix
conf_matrix_test184 <- table(test_predictions_cat184_categorical, fact_target_cat184)
conf_matrix_test184

##Menghitung Akurasi, Presisi, Recall, and F1-score pada data Training
accuracy_test_cat184 <- sum(test_predictions_cat184_categorical == fact_target_cat184) / length(fact_target_cat184)
precision_test_cat184 <- conf_matrix_test184[3, 3] / sum(conf_matrix_test184[, 3])
recall_test_cat184 <- conf_matrix_test184[3, 3] / sum(conf_matrix_test184[3, ])
f1score_test_cat184 <- 2 * (precision_test_cat184 * recall_test_cat184) / (precision_test_cat184 + recall_test_cat184)

cat("Accuracy Data Testing Subdistrict 184 Metode Random Forest:", accuracy_test_cat184 , "\n")
cat("Precision Data Testing Subdistrict 184 Metode Random Forest:", precision_test_cat184, "\n")
cat("Recall Data Testing for Subdistrict 184 Metode Random Forest:", recall_test_cat184, "\n")
cat("F1-score Data Testing for Subdistrict 184 Metode Random Forest:", f1score_test_cat184, "\n")

#Cek Overfitting
accuracy_train184 <- sum(diag(conf_matrix184)) / sum(conf_matrix184)
accuracy_test184 <- sum(diag(conf_matrix_test184)) / sum(conf_matrix_test184)

if (accuracy_train184 == 1) {
  print("Model is overfitting on training data")
} else {
  print("Model is not overfitting on training data")
}

if (accuracy_test184 < accuracy_train184) {
  print("Model may be overfitting on testing data")
} else {
  print("Model is not overfitting on testing data")
}

#=====Extreme Gradient Boosting (XGBoost)=====

#Prediksi data training
trainx_predictions_cat184 <- predict(xgb_model_cat184, newdata = as.matrix(fac_cat184))

##Buat threshold value
threshold_low <- 0.0025
threshold_high <- 0.0075

##Konversi prediksi menjadi nilai kategorikal
trainx_predictions_cat184_categorical <- cut(trainx_predictions_cat184, 
                                            breaks = c(-Inf, threshold_low, threshold_high, Inf), 
                                            labels = c(0, 0.5, 1))
#Menyetarakan length
length_train <- length(trainx_predictions_cat184_categorical)
length_fac_train <- length(fac_target_cat184)

if (length_train > length_fac_train) {
  trainx_predictions_cat184_categorical <- trainx_predictions_cat184_categorical[1:length_fac_train]
} else if (length_fac_train > length_train) {
  fac_target_cat184 <- fac_target_cat184[1:length_train]
}

## Membuat Confusion matrix
confx_matrix184 <- table(trainx_predictions_cat184_categorical, fac_target_cat184)
confx_matrix184

##Menghitung Akurasi, Presisi, Recall, and F1-score pada data Training
accuracyx_train_cat184 <- sum(trainx_predictions_cat184_categorical == fac_target_cat184) / length(fac_target_cat184)
precisionx_train_cat184 <- confx_matrix184[3, 3] / sum(confx_matrix184[, 3])
recallx_train_cat184 <- confx_matrix184[3, 3] / sum(confx_matrix184[3, ])
f1scorex_train_cat184 <- 2 * (precisionx_train_cat184 * recallx_train_cat184) / (precisionx_train_cat184 + recallx_train_cat184)

cat("Accuracy Data Training Subdistrict 184 Metode XGBoost:", accuracyx_train_cat184 , "\n")
cat("Precision Data Training Subdistrict 184 Metode XGBoost:", precisionx_train_cat184, "\n")
cat("Recall Data Training for Subdistrict 184 Metode XGBoost:", recallx_train_cat184, "\n")
cat("F1-score Data Training for Subdistrict 184 Metode XGBoost:", f1scorex_train_cat184, "\n")

#Confussion Matrix, F1-score, Precision, dan Accuracy data testing

##Membuat data frame untuk klasifikasi, yakni mengubah revenue_category menjadi faktor (data testing)
fact_cat184 <- normdata_test_cat184
fact_cat184$revenue_category <- as.factor(fact_cat184$revenue_category)

##Memisahkan target dan prediktor untuk revenue_category
fact_target_cat184 <- fact_cat184$revenue_category
fact_cat184 <- fact_cat184[, -which(names(fact_cat184) %in% c("revenue_category", "revenue_total"))]

# Prediksi pada data testing yang sudah dinormalisasi
testx_predictions_cat184 <- predict(xgb_model_cat184, newdata = as.matrix(fact_cat184))

##Buat threshold value
threshold_low <- 0.0025
threshold_high <- 0.0075

##Konversi prediksi menjadi nilai kategorikal
testx_predictions_cat184_categorical <- cut(testx_predictions_cat184, 
                                           breaks = c(-Inf, threshold_low, threshold_high, Inf), 
                                           labels = c(0, 0.5, 1))

#Menyetarakan length
length_test <- length(testx_predictions_cat184_categorical)
length_fac_test <- length(fact_target_cat184)

if (length_test > length_fac_test) {
  testx_predictions_cat184_categorical <- testx_predictions_cat184_categorical[1:length_fac_test]
} else if (length_fac_test > length_test) {
  fact_target_cat184 <- fact_target_cat184[1:length_test]
}

# Membuat Confusion matrix
confx_matrix_test184 <- table(testx_predictions_cat184_categorical, as.matrix(fact_target_cat184))
confx_matrix_test184

##Menghitung Akurasi, Presisi, Recall, and F1-score pada data Training
accuracyx_test_cat184 <- sum(testx_predictions_cat184_categorical == fact_target_cat184) / length(fact_target_cat184)
precisionx_test_cat184 <- confx_matrix_test184[3, 3] / sum(confx_matrix_test184[, 3])
recallx_test_cat184 <- confx_matrix_test184[3, 3] / sum(confx_matrix_test184[3, ])
f1scorex_test_cat184 <- 2 * (precisionx_test_cat184 * recallx_test_cat184) / (precisionx_test_cat184 + recallx_test_cat184)

cat("Accuracy Data Testing Subdistrict 184 Metode XGBoost:", accuracyx_test_cat184 , "\n")
cat("Precision Data Testing Subdistrict 184 Metode XGBoost:", precisionx_test_cat184, "\n")
cat("Recall Data Testing for Subdistrict 184 Metode XGBoost:", recallx_test_cat184, "\n")
cat("F1-score Data Testing for Subdistrict 184 Metode XGBoost:", f1scorex_test_cat184, "\n")


#Cek Overfitting
accuracyx_train184 <- sum(diag(confx_matrix184)) / sum(confx_matrix184)
accuracyx_test184 <- sum(diag(confx_matrix_test184)) / sum(confx_matrix_test184)

if (accuracyx_train184 == 1) {
  print("Model is overfitting on training data")
} else {
  print("Model is not overfitting on training data")
}

if (accuracyx_test184 < accuracyx_train184) {
  print("Model may be overfitting on testing data")
} else {
  print("Model is not overfitting on testing data")
}

##=========================================SUBDISTRICT 185========================================##

#=====Metode Random Forest=====
#Confussion Matrix, F1-score, Precision, dan Accuracy data training

##Membuat data frame untuk klasifikasi, yakni mengubah revenue_category menjadi faktor
fac_cat185 <- normdata_train_cat185 
fac_cat185$revenue_category <- as.factor(fac_cat185$revenue_category)

##Memisahkan target dan prediktor untuk revenue_category
fac_target_cat185 <- fac_cat185$revenue_category

fac_cat185 <- fac_cat185[, -which(names(fac_cat185) %in% c("revenue_category", "revenue_total"))]

#Prediksi data training
train_predictions_cat185 <- predict(rf_model_cat185, newdata = fac_cat185)

##Buat threshold value
threshold_low <- 0.25
threshold_high <- 0.75

##Konversi prediksi menjadi nilai kategorikal
train_predictions_cat185_categorical <- cut(train_predictions_cat185, 
                                            breaks = c(-Inf, threshold_low, threshold_high, Inf), 
                                            labels = c(0, 0.5, 1))

##Membuat Confusion matrix
conf_matrix185 <- table(train_predictions_cat185_categorical, fac_target_cat185)
conf_matrix185

##Menghitung Akurasi, Presisi, Recall, and F1-score pada data Training
accuracy_train_cat185 <- sum(train_predictions_cat185_categorical == fac_target_cat185) / length(fac_target_cat185)
precision_train_cat185 <- conf_matrix185[3, 3] / sum(conf_matrix185[, 3])
recall_train_cat185 <- conf_matrix185[3, 3] / sum(conf_matrix185[3, ])
f1score_train_cat185 <- 2 * (precision_train_cat185 * recall_train_cat185) / (precision_train_cat185 + recall_train_cat185)

cat("Accuracy Data Training Subdistrict 185 Metode Random Forest:", accuracy_train_cat185 , "\n")
cat("Precision Data Training Subdistrict 185 Metode Random Forest:", precision_train_cat185, "\n")
cat("Recall Data Training for Subdistrict 185 Metode Random Forest:", recall_train_cat185, "\n")
cat("F1-score Data Training for Subdistrict 185 Metode Random Forest:", f1score_train_cat185, "\n")

#Confussion Matrix, F1-score, Precision, dan Accuracy data testing

##Melakukan Normalisasi data testing
normdata_test_cat185 <- as.data.frame(lapply(testing_cat185, normalize))
print("Data Sebelum Normalisasi:")
testing185
print("Data Sesudah Normalisasi:")
normdata_test_cat185

##Membuat data frame untuk klasifikasi, yakni mengubah revenue_category menjadi faktor (data testing)
fact_cat185 <- normdata_test_cat185 
fact_cat185$revenue_category <- as.factor(fact_cat185$revenue_category)

##Memisahkan target dan prediktor untuk revenue_category
fact_target_cat185 <- fact_cat185$revenue_category
fact_cat185 <- fact_cat185[, -which(names(fact_cat185) %in% c("revenue_category", "revenue_total"))]

# Prediksi pada data testing yang sudah dinormalisasi
test_predictions_cat185 <- predict(rf_model_cat185, fact_cat185)

##Buat threshold value
threshold_low <- 0.25
threshold_high <- 0.75

##Konversi prediksi menjadi nilai kategorikal
test_predictions_cat185_categorical <- cut(test_predictions_cat185, 
                                           breaks = c(-Inf, threshold_low, threshold_high, Inf), 
                                           labels = c(0, 0.5, 1))

##Membuat Confusion matrix
conf_matrix_test185 <- table(test_predictions_cat185_categorical, fact_target_cat185)
conf_matrix_test185

##Menghitung Akurasi, Presisi, Recall, and F1-score pada data Training
accuracy_test_cat185 <- sum(test_predictions_cat185_categorical == fact_target_cat185) / length(fact_target_cat185)
precision_test_cat185 <- conf_matrix_test185[3, 3] / sum(conf_matrix_test185[, 3])
recall_test_cat185 <- conf_matrix_test185[3, 3] / sum(conf_matrix_test185[3, ])
f1score_test_cat185 <- 2 * (precision_test_cat185 * recall_test_cat185) / (precision_test_cat185 + recall_test_cat185)

cat("Accuracy Data Testing Subdistrict 185 Metode Random Forest:", accuracy_test_cat185 , "\n")
cat("Precision Data Testing Subdistrict 185 Metode Random Forest:", precision_test_cat185, "\n")
cat("Recall Data Testing for Subdistrict 185 Metode Random Forest:", recall_test_cat185, "\n")
cat("F1-score Data Testing for Subdistrict 185 Metode Random Forest:", f1score_test_cat185, "\n")

#Cek Overfitting
accuracy_train185 <- sum(diag(conf_matrix185)) / sum(conf_matrix185)
accuracy_test185 <- sum(diag(conf_matrix_test185)) / sum(conf_matrix_test185)

if (accuracy_train185 == 1) {
  print("Model is overfitting on training data")
} else {
  print("Model is not overfitting on training data")
}

if (accuracy_test185 < accuracy_train185) {
  print("Model may be overfitting on testing data")
} else {
  print("Model is not overfitting on testing data")
}

#=====Extreme Gradient Boosting (XGBoost)=====

#Prediksi data training
trainx_predictions_cat185 <- predict(xgb_model_cat185, newdata = as.matrix(fac_cat185))

##Buat threshold value
threshold_low <- 0.0025
threshold_high <- 0.0075

##Konversi prediksi menjadi nilai kategorikal
trainx_predictions_cat185_categorical <- cut(trainx_predictions_cat185, 
                                             breaks = c(-Inf, threshold_low, threshold_high, Inf), 
                                             labels = c(0, 0.5, 1))
#Menyetarakan length
length_train <- length(trainx_predictions_cat185_categorical)
length_fac_train <- length(fac_target_cat185)

if (length_train > length_fac_train) {
  trainx_predictions_cat185_categorical <- trainx_predictions_cat185_categorical[1:length_fac_train]
} else if (length_fac_train > length_train) {
  fac_target_cat185 <- fac_target_cat185[1:length_train]
}

## Membuat Confusion matrix
confx_matrix185 <- table(trainx_predictions_cat185_categorical, fac_target_cat185)
confx_matrix185

##Menghitung Akurasi, Presisi, Recall, and F1-score pada data Training
accuracyx_train_cat185 <- sum(trainx_predictions_cat185_categorical == fac_target_cat185) / length(fac_target_cat185)
precisionx_train_cat185 <- confx_matrix185[3, 3] / sum(confx_matrix185[, 3])
recallx_train_cat185 <- confx_matrix185[3, 3] / sum(confx_matrix185[3, ])
f1scorex_train_cat185 <- 2 * (precisionx_train_cat185 * recallx_train_cat185) / (precisionx_train_cat185 + recallx_train_cat185)

cat("Accuracy Data Training Subdistrict 185 Metode XGBoost:", accuracyx_train_cat185 , "\n")
cat("Precision Data Training Subdistrict 185 Metode XGBoost:", precisionx_train_cat185, "\n")
cat("Recall Data Training for Subdistrict 185 Metode XGBoost:", recallx_train_cat185, "\n")
cat("F1-score Data Training for Subdistrict 185 Metode XGBoost:", f1scorex_train_cat185, "\n")

#Confussion Matrix, F1-score, Precision, dan Accuracy data testing

##Membuat data frame untuk klasifikasi, yakni mengubah revenue_category menjadi faktor (data testing)
fact_cat185 <- normdata_test_cat185
fact_cat185$revenue_category <- as.factor(fact_cat185$revenue_category)

##Memisahkan target dan prediktor untuk revenue_category
fact_target_cat185 <- fact_cat185$revenue_category
fact_cat185 <- fact_cat185[, -which(names(fact_cat185) %in% c("revenue_category", "revenue_total"))]

# Prediksi pada data testing yang sudah dinormalisasi
testx_predictions_cat185 <- predict(xgb_model_cat185, newdata = as.matrix(fact_cat185))

##Buat threshold value
threshold_low <- 0.0025
threshold_high <- 0.0075

##Konversi prediksi menjadi nilai kategorikal
testx_predictions_cat185_categorical <- cut(testx_predictions_cat185, 
                                            breaks = c(-Inf, threshold_low, threshold_high, Inf), 
                                            labels = c(0, 0.5, 1))

#Menyetarakan length
length_test <- length(testx_predictions_cat185_categorical)
length_fac_test <- length(fact_target_cat185)

if (length_test > length_fac_test) {
  testx_predictions_cat185_categorical <- testx_predictions_cat185_categorical[1:length_fac_test]
} else if (length_fac_test > length_test) {
  fact_target_cat185 <- fact_target_cat185[1:length_test]
}

# Membuat Confusion matrix
confx_matrix_test185 <- table(testx_predictions_cat185_categorical, as.matrix(fact_target_cat185))
confx_matrix_test185

##Menghitung Akurasi, Presisi, Recall, and F1-score pada data Training
accuracyx_test_cat185 <- sum(testx_predictions_cat185_categorical == fact_target_cat185) / length(fact_target_cat185)
precisionx_test_cat185 <- confx_matrix_test185[3, 3] / sum(confx_matrix_test185[, 3])
recallx_test_cat185 <- confx_matrix_test185[3, 3] / sum(confx_matrix_test185[3, ])
f1scorex_test_cat185 <- 2 * (precisionx_test_cat185 * recallx_test_cat185) / (precisionx_test_cat185 + recallx_test_cat185)

cat("Accuracy Data Testing Subdistrict 185 Metode XGBoost:", accuracyx_test_cat185 , "\n")
cat("Precision Data Testing Subdistrict 185 Metode XGBoost:", precisionx_test_cat185, "\n")
cat("Recall Data Testing for Subdistrict 185 Metode XGBoost:", recallx_test_cat185, "\n")
cat("F1-score Data Testing for Subdistrict 185 Metode XGBoost:", f1scorex_test_cat185, "\n")

#Cek Overfitting
accuracyx_train185 <- sum(diag(confx_matrix185)) / sum(confx_matrix185)
accuracyx_test185 <- sum(diag(confx_matrix_test185)) / sum(confx_matrix_test185)

if (accuracyx_train185 == 1) {
  print("Model is overfitting on training data")
} else {
  print("Model is not overfitting on training data")
}

if (accuracyx_test185 < accuracyx_train185) {
  print("Model may be overfitting on testing data")
} else {
  print("Model is not overfitting on testing data")
}

#===========================================Nomor 6===============================================
##=========================================SUBDISTRICT 184========================================##
#Variabel Target revenue_total
significant_variables184
#Variabel Target revenue_category
significant_variables_cat184

##=========================================SUBDISTRICT 185========================================##
#Variabel Target revenue_total
significant_variables185
#Variabel Target revenue_category
significant_variables_cat185

#===========================================Nomor 7===============================================
##=========================================REVENUE_TOTAL========================================##

#menggunakan data awal yaitu datafix
View(datafix)

#Mengidentifikasi dan menghapus variabel yang varians mendekati nol
nearzerovar <- nearZeroVar(datafix);nearzerovar
datafil <- datafix[, -nearzerovar]

#Cek korelasi setelah penghapusan variabel yang variansnya mendekati nol
corfix <- cor(datafil);View(corfix)

#Uji Univariat (Variabel target = revenue_total)
##Mengambil dan menampilkan variabel yang berkorelasi tinggi terhadap revenue_total dengan korelasi di atas 0,7
highcor <- which(abs(corfix["revenue_total", ])>0.7);highcor
print(names(datafil)[highcor])

##Mengambil data dari variabel yang memiliki korelasi tinggi
highdata<- datafil[, highcor]; highdata

##Cek Variabel yang signifikan terhadap revenue_total
unianalysis <- lapply(names(highdata), function(var) {
  if (var != "target_variable") {
    unitest_result <- t.test(highdata[[var]], highdata$revenue_total)
    return(data.frame(variable = var, p_value =  unitest_result$p.value))
  }
})

dataresult <- do.call(rbind,unianalysis)
dataresult$Signifikansi <- ifelse(dataresult$p_value < 0.05, "Signifikan", "Tidak Signifikan");dataresult

#Data Baru dari keselurahan data (variabel target = revenue_total)
datafix1 <- highdata; View(datafix1)
dim(datafix1)

#membagi data menjadi training dan testing (variabel target = revenue_total)
jumlah <- round(nrow(datafix1) * 0.8)
indeks <- sample(1:nrow(datafix1), jumlah)
training <- datafix1[indeks, ]
testing <- datafix1[-indeks, ]

dim(training)
dim(testing)

#Melakukan Normalisasi data training
normdata_train <- as.data.frame(lapply(training, normalize))

print("Data Sebelum Normalisasi:")
training
print("Data Sesudah Normalisasi:")
normdata_train

#=====Metode Random Forest=====
target <- normdata_train$revenue_total
data1 <- normdata_train[, -which(names(normdata_train) %in% c("revenue_total", "revenue_category"))]
set.seed(123)  
rf_model <- randomForest(x = data1, y = target, ntree = 500, method = "rf")
rf_model

importance <- importance(rf_model)
importance
importance <- data.frame(importance)
varImpPlot(rf_model)

Best <- arrange(importance, desc(IncNodePurity))

significant_variables <- Best %>%
  filter(IncNodePurity > 0.1)
significant_variables

#=====Extreme Gradient Boosting (XGBoost)=====
target <- normdata_train$revenue_total
data1 <- normdata_train[, -which(names(normdata_train) %in% c("revenue_total", "revenue_category"))]
xgb_model <- xgboost(data = as.matrix(data1), label = target, nrounds = 500, objective = "reg:squarederror")
xgb_model

importance_xgb <- xgb.importance(model = xgb_model)
importance_xgb
importance_xgb <- data.frame(importance_xgb)

Bestxgb <- arrange(importance_xgb, desc(Gain))
Bestxgb

#Membuat scatter plot dengan urutan dari variabel Gain terbesar ke terkecil
scatter_plot5 <- ggplot(Bestxgb, aes(x = Gain, y = reorder(Feature, Gain))) +
  geom_point(color = "darkblue", size = 3) +
  labs(title = "XGBoost Feature Importance - Scatter Plot",
       x = "Gain",
       y = "Feature") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  scale_y_discrete(limits = rev(Bestxgb$Feature)) +  
  xlim(c(0, max(Bestxgb$Gain) * 1.2))
scatter_plot5

significant_variablesxgb <- Bestxgb %>%
  filter(Gain > 0.001)
significant_variablesxgb

#Mencari MSE (variabel target = revenue_total)

#=====Metode Random Forest=====
#MSE data training
train_predictions <- predict(rf_model, newdata = data1)
train_mse <- mean((target - train_predictions)^2)
train_rmse <- sqrt(mean((target - train_predictions)^2))
print(paste("Mean Squared Error (MSE) data training:", train_mse))
print(paste("Root Mean Squared Error (RMSE) data training:", train_rmse))

#Plot hasil prediksi vs. observasi pada data training
plot(train_predictions, target, 
     main = "Prediction vs. Observation Training Data (Revenue_Total)", 
     xlab = "Predicted", ylab = "Observed",abline(0, 1, col = "red"))

#Melakukan Normalisasi data testing
normdata_test <- as.data.frame(lapply(testing, normalize))
print("Data Sebelum Normalisasi:")
testing
print("Data Sesudah Normalisasi:")
normdata_test

#Validasi data testing
validation_target <- normdata_test$revenue_total
validation_predictors <- normdata_test[, -which(names(normdata_test) %in% c("revenue_total", "revenue_category"))]
validation_predictions <- predict(rf_model, newdata = validation_predictors)
mse <- mean((validation_target - validation_predictions)^2)
rmse <- sqrt(mean((validation_target - validation_predictions)^2))
print(paste("Mean Squared Error (MSE) Validasi:", mse))
print(paste("Root Mean Squared Error (RMSE) Validasi:", rmse))

#Plot hasil prediksi vs. observasi pada data training
plot(validation_predictions, validation_target, 
     main = "Prediction vs. Observation Testing Data (Revenue_Total)", 
     xlab = "Predicted", ylab = "Observed",abline(lm(validation_target ~ validation_predictions), col = "blue"))

#=====Metode Extreme Gradient Boosting (XGBoost)=====
#MSE data training
trainx_predictions <- predict(xgb_model, newdata = as.matrix(data1))
trainx_mse <- mean((target - trainx_predictions)^2)
trainx_rmse <- sqrt(mean((target - trainx_predictions)^2))
print(paste("Mean Squared Error (MSE) data training:", trainx_mse))
print(paste("Root Mean Squared Error (RMSE) data training:", trainx_rmse))

#Plot hasil prediksi vs. observasi pada data training
plot(trainx_predictions, target, 
     main = "Prediction vs. Observation Training Data (Revenue_Total)", 
     xlab = "Predicted", ylab = "Observed",abline(0, 1, col = "red"))

#Validasi data testing
validationx_target <- normdata_test$revenue_total
validationx_predictors <- normdata_test[, -which(names(normdata_test) %in% c("revenue_total", "revenue_category"))]
validationx_predictions <- predict(xgb_model, newdata = as.matrix(validationx_predictors))
msex <- mean((validationx_target - validationx_predictions)^2)
rmsex <- sqrt(mean((validationx_target - validationx_predictions)^2))
print(paste("Mean Squared Error (MSE) Validasi:", msex))
print(paste("Root Mean Squared Error (RMSE) Validasi:", rmsex))

#Plot hasil prediksi vs. observasi pada data testing
plot(validationx_predictions, validationx_target, 
     main = "Prediction vs. Observation Training Data Subdistrict 184 (Revenue_Total)", 
     xlab = "Predicted", ylab = "Observed",abline(lm(validationx_target184 ~ validationx_predictions184), col = "blue"))

##=========================================REVENUE_CATEGORY========================================##
#Uji Univariat (Variabel target = revenue_category)
##Mengambil dan menampilkan variabel yang berkorelasi tinggi terhadap revenue_category dengan korelasi di atas 0,7
highcor_cat <- which(abs(cor184["revenue_category", ])>0.7);highcor_cat
print(names(datafil)[highcor_cat])

##Mengambil data dari variabel yang memiliki korelasi tinggi
highdata_cat <- datafil[, highcor_cat]; highdata_cat

##Cek Variabel yang signifikan terhadap revenue_total pada subdistrict 184
unianalysis_cat <- lapply(names(highdata_cat), function(var) {
  if (var != "target_variable") {
    kruskal_result_cat <- kruskal.test(highdata_cat[[var]] ~ highdata_cat$revenue_category)
    return(data.frame(variable = var, p_value = kruskal_result_cat$p.value))
  }
})

dataresult_cat <- do.call(rbind,unianalysis_cat)
dataresult_cat$Signifikansi <- ifelse(dataresult_cat$p_value < 0.05, "Signifikan", "Tidak Signifikan");dataresult_cat

#Data Baru dari keselurahan data (variabel target = revenue_category)
datafix2 <- highdata_cat; View(datafix2)
dim(datafix2)

#membagi data menjadi training dan testing (variabel target = revenue_category)
jumlah_cat <- round(nrow(datafix2) * 0.8)
indeks_cat <- sample(1:nrow(datafix2), jumlah_cat)
training_cat <- datafix2[indeks_cat, ]
testing_cat <- datafix2[-indeks_cat, ]

dim(training_cat)
dim(testing_cat)

#Melakukan Normalisasi data training
normdata_train_cat <- as.data.frame(lapply(training_cat, normalize))

print("Data Sebelum Normalisasi:")
training_cat
print("Data Sesudah Normalisasi:")
normdata_train_cat

#=====Metode Random Forest=====
target_cat <- normdata_train_cat$revenue_category
data_cat <- normdata_train_cat[, -which(names(normdata_train_cat) %in% c("revenue_category", "revenue_total"))]
set.seed(123)  
rf_model_cat <- randomForest(x = data_cat, y = target_cat, ntree = 500)
rf_model_cat

importance_cat <- importance(rf_model_cat)
importance_cat
importance_cat <- data.frame(importance_cat)
varImpPlot(rf_model_cat)

Best_cat <- arrange(importance_cat, desc(IncNodePurity))

significant_variables_cat <- Best_cat %>%
  filter(IncNodePurity > 0.1)
significant_variables_cat

#=====Extreme Gradient Boosting (XGBoost)=====
target_cat <- normdata_train_cat$revenue_category
data_cat <- normdata_train_cat[, -which(names(normdata_train_cat) %in% c("revenue_category", "revenue_total"))]

tuned_params <- list(
  objective = "multi:softprob",
  max_depth = 6,
  eta = 0.3,
  subsample = 0.8,
  colsample_bytree = 0.8,
  num_class = length(unique(target_cat))
)

xgb_model_cat <- xgboost(data = as.matrix(data_cat), label = as.integer(target_cat), nrounds = 500, params = tuned_params)
xgb_model_cat


importance_xgb_cat <- xgb.importance(model = xgb_model_cat)
importance_xgb_cat
importance_xgb_cat <- data.frame(importance_xgb_cat)

Bestxgb_cat <- arrange(importance_xgb_cat, desc(Gain))
Bestxgb_cat

scatter_plot6 <- ggplot(Bestxgb_cat, aes(x = Gain, y = reorder(Feature, Gain))) +
  geom_point(color = "darkblue", size = 3) +
  labs(title = "XGBoost Feature Importance - Scatter Plot",
       x = "Gain",
       y = "Feature") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  scale_y_discrete(limits = rev(Bestxgb_cat$Feature)) +  
  xlim(c(0, max(Bestxgb_cat$Gain) * 1.2))
scatter_plot6

significant_variablesxgb_cat <- Bestxgb_cat %>%
  filter(Gain > 0.001)
significant_variablesxgb_cat

#Mencari MSE (variabel target = revenue_category)

#=====Metode Random Forest=====
#Confussion Matrix, F1-score, Precision, dan Accuracy data training

##Membuat data frame untuk klasifikasi, yakni mengubah revenue_category menjadi faktor
fac_cat <- normdata_train_cat
fac_cat$revenue_category <- as.factor(fac_cat$revenue_category)

##Memisahkan target dan prediktor untuk revenue_category
fac_target_cat <- fac_cat$revenue_category
fac_cat <- fac_cat[, -which(names(fac_cat) %in% c("revenue_category", "revenue_total"))]

train_predictions_cat <- predict(rf_model_cat, newdata = fac_cat)

##Buat threshold value
threshold_low <- 0.25
threshold_high <- 0.75

##Konversi prediksi menjadi nilai kategorikal
train_predictions_cat_categorical <- cut(train_predictions_cat, 
                                            breaks = c(-Inf, threshold_low, threshold_high, Inf), 
                                            labels = c(0, 0.5, 1))

##Membuat Confusion matrix
conf_matrix <- table(train_predictions_cat_categorical, fac_target_cat)
conf_matrix

##Menghitung Akurasi, Presisi, Recall, and F1-score pada data Training
accuracy_train_cat <- sum(train_predictions_cat_categorical == fac_target_cat) / length(fac_target_cat)
precision_train_cat <- conf_matrix[3, 3] / sum(conf_matrix[, 3])
recall_train_cat <- conf_matrix[3, 3] / sum(conf_matrix[3, ])
f1score_train_cat <- 2 * (precision_train_cat * recall_train_cat) / (precision_train_cat + recall_train_cat)

cat("Accuracy Data Training Metode Random Forest:", accuracy_train_cat , "\n")
cat("Precision Data Training  Metode Random Forest:", precision_train_cat, "\n")
cat("Recall Data Training for Metode Random Forest:", recall_train_cat, "\n")
cat("F1-score Data Training for Metode Random Forest:", f1score_train_cat, "\n")


#Confussion Matrix, F1-score, Precision, dan Accuracy data testing

##Melakukan Normalisasi data testing
normdata_test_cat <- as.data.frame(lapply(testing_cat, normalize))
print("Data Sebelum Normalisasi:")
testing
print("Data Sesudah Normalisasi:")
normdata_test_cat

target_cat_test <- normdata_test_cat[, -which(names(normdata_test_cat) %in% c("revenue_category", "revenue_total"))]

##Membuat data frame untuk klasifikasi, yakni mengubah revenue_category menjadi faktor (data testing)
fact_cat <- normdata_test_cat
fact_cat$revenue_category <- as.factor(fact_cat$revenue_category)

##Memisahkan target dan prediktor untuk revenue_category
fact_target_cat <- fact_cat$revenue_category
fact_cat <- fact_cat[, -which(names(fact_cat) %in% c("revenue_category", "revenue_total"))]

# Prediksi pada data testing yang sudah dinormalisasi
test_predictions_cat <- predict(rf_model_cat, fact_cat)

##Buat threshold value
threshold_low <- 0.25
threshold_high <- 0.75

##Konversi prediksi menjadi nilai kategorikal
test_predictions_cat_categorical <- cut(test_predictions_cat, 
                                           breaks = c(-Inf, threshold_low, threshold_high, Inf), 
                                           labels = c(0, 0.5, 1))

##Membuat Confusion matrix
conf_matrix_test <- table(test_predictions_cat_categorical, fact_target_cat)
conf_matrix_test

##Menghitung Akurasi, Presisi, Recall, and F1-score pada data Training
accuracy_test_cat <- sum(test_predictions_cat_categorical == fact_target_cat) / length(fact_target_cat)
precision_test_cat <- conf_matrix_test[3, 3] / sum(conf_matrix_test[, 3])
recall_test_cat <- conf_matrix_test[3, 3] / sum(conf_matrix_test[3, ])
f1score_test_cat <- 2 * (precision_test_cat * recall_test_cat) / (precision_test_cat + recall_test_cat)

cat("Accuracy Data Testing Metode Random Forest:", accuracy_test_cat , "\n")
cat("Precision Data Testing Metode Random Forest:", precision_test_cat, "\n")
cat("Recall Data Testing for Metode Random Forest:", recall_test_cat, "\n")
cat("F1-score Data Testing for  Metode Random Forest:", f1score_test_cat, "\n")

#Cek Overfitting
if (conf_matrix_test[1, 1] == sum(target_cat_test) && conf_matrix_test[2, 2] == sum(target_cat_test) && conf_matrix_test[3, 3] == sum(target_cat_test )) {
  print("Model is overfitting")
} else {
  print("Model is not overfitting")
}


#=====Extreme Gradient Boosting (XGBoost)=====

#Prediksi data training
trainx_predictions_cat <- predict(xgb_model_cat, newdata = as.matrix(fac_cat))

##Buat threshold value
threshold_low <- 0.0025
threshold_high <- 0.0075

##Konversi prediksi menjadi nilai kategorikal
trainx_predictions_cat_categorical <- cut(trainx_predictions_cat, 
                                             breaks = c(-Inf, threshold_low, threshold_high, Inf), 
                                             labels = c(0, 0.5, 1))
#Menyetarakan length
length_train <- length(trainx_predictions_cat_categorical)
length_fac_train <- length(fac_target_cat)

if (length_train > length_fac_train) {
  trainx_predictions_cat_categorical <- trainx_predictions_cat_categorical[1:length_fac_train]
} else if (length_fac_train > length_train) {
  fac_target_cat <- fac_target_cat[1:length_train]
}

## Membuat Confusion matrix
confx_matrix <- table(trainx_predictions_cat_categorical, fac_target_cat)
confx_matrix

##Menghitung Akurasi, Presisi, Recall, and F1-score pada data Training
accuracyx_train_cat <- sum(trainx_predictions_cat_categorical == fac_target_cat) / length(fac_target_cat)
precisionx_train_cat <- confx_matrix[3, 3] / sum(confx_matrix[, 3])
recallx_train_cat <- confx_matrix[3, 3] / sum(confx_matrix[3, ])
f1scorex_train_cat <- 2 * (precisionx_train_cat * recallx_train_cat) / (precisionx_train_cat + recallx_train_cat)

cat("Accuracy Data Training Metode XGBoost:", accuracyx_train_cat, "\n")
cat("Precision Data Training Metode XGBoost:", precisionx_train_cat, "\n")
cat("Recall Data Training Metode XGBoost:", recallx_train_cat, "\n")
cat("F1-score Data Training Metode XGBoost:", f1scorex_train_cat, "\n")

#Confussion Matrix, F1-score, Precision, dan Accuracy data testing

##Membuat data frame untuk klasifikasi, yakni mengubah revenue_category menjadi faktor (data testing)
fact_cat <- normdata_test_cat
fact_cat$revenue_category <- as.factor(fact_cat$revenue_category)

##Memisahkan target dan prediktor untuk revenue_category
fact_target_cat <- fact_cat$revenue_category
fact_cat <- fact_cat184[, -which(names(fact_cat) %in% c("revenue_category", "revenue_total"))]

# Prediksi pada data testing yang sudah dinormalisasi
testx_predictions_cat <- predict(xgb_model_cat, newdata = as.matrix(fact_cat))

##Buat threshold value
threshold_low <- 0.0025
threshold_high <- 0.0075

##Konversi prediksi menjadi nilai kategorikal
testx_predictions_cat_categorical <- cut(testx_predictions_cat, 
                                            breaks = c(-Inf, threshold_low, threshold_high, Inf), 
                                            labels = c(0, 0.5, 1))

#Menyetarakan length
length_test <- length(testx_predictions_cat_categorical)
length_fac_test <- length(fact_target_cat)

if (length_test > length_fac_test) {
  testx_predictions_cat_categorical <- testx_predictions_cat_categorical[1:length_fac_test]
} else if (length_fac_test > length_test) {
  fact_target_cat <- fact_target_cat[1:length_test]
}

# Membuat Confusion matrix
confx_matrix_test <- table(testx_predictions_cat_categorical, as.matrix(fact_target_cat))
confx_matrix_test

##Menghitung Akurasi, Presisi, Recall, and F1-score pada data Training
accuracyx_test_cat <- sum(testx_predictions_cat_categorical == fact_target_cat) / length(fact_target_cat)
precisionx_test_cat <- confx_matrix_test[3, 3] / sum(confx_matrix_test[, 3])
recallx_test_cat <- confx_matrix_test[3, 3] / sum(confx_matrix_test[3, ])
f1scorex_test_cat <- 2 * (precisionx_test_cat * recallx_test_cat) / (precisionx_test_cat + recallx_test_cat)

cat("Accuracy Data Testing Metode XGBoost:", accuracyx_test_cat, "\n")
cat("Precision Data Testing Metode XGBoost:", precisionx_test_cat, "\n")
cat("Recall Data Testing for Metode XGBoost:", recallx_test_cat, "\n")
cat("F1-score Data Testing forMetode XGBoost:", f1scorex_test_cat, "\n")