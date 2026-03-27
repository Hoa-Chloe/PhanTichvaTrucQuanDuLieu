# Cài đặt nếu chưa có
# install.packages(c("tidyverse", "caret", "pROC", "rsample", "yardstick"))

library(tidyverse)
library(caret)
library(pROC)

medical <- read.csv("D:/R/labs/dataset/medical_care.csv", stringsAsFactors = TRUE)
dim(medical)
head(medical)

# Kiểm tra phân phối biến mục tiêu
table(medical$UCURNINS)
prop.table(table(medical$UCURNINS))

set.seed(42)

# Tạo chỉ số cho tập training (70%)
train_idx <- createDataPartition(medical$UCURNINS, p = 0.7, list = FALSE)

train_data <- medical[train_idx, ]
test_data  <- medical[-train_idx, ]

cat("Kích thước tập train:", nrow(train_data), "\n")
cat("Kích thước tập test: ", nrow(test_data),  "\n")

# Kiểm tra tỷ lệ lớp trong từng tập
prop.table(table(train_data$UCURNINS))
prop.table(table(test_data$UCURNINS))

set.seed(NULL)  # Bỏ seed cố định để thấy sự ngẫu nhiên

auc_scores <- numeric(20)

for (i in 1:20) {
  idx   <- createDataPartition(medical$UCURNINS, p = 0.7, list = FALSE)
  train <- medical[idx, ]
  test  <- medical[-idx, ]
  
  m   <- glm(UCURNINS ~ UMARSTAT + USATMED + URELATE + REGION + FHOSP +
               FDENT + FEMER + FDOCT + UIMMSTAT + UAGE + U_FTPT +
               U_WKSLY + U_USHRS + UBRACE + GENDER + UEDUC3,
             data = train, family = binomial())
  
  p   <- predict(m, newdata = test, type = "response")
  roc_i <- roc(test$UCURNINS, p, levels = c("No", "Yes"), quiet = TRUE)
  auc_scores[i] <- auc(roc_i)
}

cat("AUC min:", round(min(auc_scores),  4), "\n")
cat("AUC max:", round(max(auc_scores),  4), "\n")
cat("AUC sd: ", round(sd(auc_scores),   4), "\n")

# Trực quan hóa
hist(auc_scores, main = "Phân phối AUC — Holdout (20 lần)",
     xlab = "AUC", col = "steelblue", border = "white")

# Cấu hình 10-Fold CV
ctrl_10fold <- trainControl(
  method          = "cv",
  number          = 10,
  classProbs      = TRUE,          # Cần thiết để tính AUC
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

set.seed(123)
model_cv <- train(
  UCURNINS ~ UMARSTAT + USATMED + URELATE + REGION + FHOSP +
    FDENT + FEMER + FDOCT + UIMMSTAT + UAGE + U_FTPT +
    U_WKSLY + U_USHRS + UBRACE + GENDER + UEDUC3,
  data      = medical,
  method    = "glm",
  family    = "binomial",
  trControl = ctrl_10fold,
  metric    = "ROC"
)

# Xem kết quả CV
print(model_cv)
cat("AUC trung bình (10-Fold CV):", round(model_cv$results$ROC, 4), "\n")

library(pROC)

set.seed(456)
k    <- 10
folds <- createFolds(medical$UCURNINS, k = k, list = TRUE)

auc_train <- numeric(k)
auc_val   <- numeric(k)

for (i in seq_len(k)) {
  val_idx  <- folds[[i]]
  train_cv <- medical[-val_idx, ]
  val_cv   <- medical[val_idx, ]
  
  m <- glm(UCURNINS ~ UMARSTAT + USATMED + URELATE + REGION + FHOSP +
             FDENT + FEMER + FDOCT + UIMMSTAT + UAGE + U_FTPT +
             U_WKSLY + U_USHRS + UBRACE + GENDER + UEDUC3,
           data = train_cv, family = binomial())
  
  p_train <- predict(m, newdata = train_cv, type = "response")
  p_val   <- predict(m, newdata = val_cv,   type = "response")
  
  auc_train[i] <- auc(roc(train_cv$UCURNINS, p_train,
                          levels = c("No","Yes"), quiet = TRUE))
  auc_val[i]   <- auc(roc(val_cv$UCURNINS,   p_val,
                          levels = c("No","Yes"), quiet = TRUE))
  
  cat(sprintf("Fold %2d | Train AUC: %.4f | Val AUC: %.4f\n",
              i, auc_train[i], auc_val[i]))
}

cat("\n--- Tổng kết ---\n")
cat("Train AUC trung bình:", round(mean(auc_train), 4), "\n")
cat("Val   AUC trung bình:", round(mean(auc_val),   4), "\n")
cat("Chênh lệch (overfit?):", round(mean(auc_train) - mean(auc_val), 4), "\n")

# Cấu hình 10-Fold CV lặp 5 lần (= 50 lần đánh giá tổng cộng)
ctrl_repeated <- trainControl(
  method          = "repeatedcv",
  number          = 10,
  repeats         = 5,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

set.seed(789)
model_rep_cv <- train(
  UCURNINS ~ UMARSTAT + USATMED + URELATE + REGION + FHOSP +
    FDENT + FEMER + FDOCT + UIMMSTAT + UAGE + U_FTPT +
    U_WKSLY + U_USHRS + UBRACE + GENDER + UEDUC3,
  data      = medical,
  method    = "glm",
  family    = "binomial",
  trControl = ctrl_repeated,
  metric    = "ROC"
)

cat("AUC trung bình (Repeated 10-Fold CV):", round(model_rep_cv$results$ROC, 4), "\n")

set.seed(NULL)

results_comparison <- data.frame()

for (run in 1:10) {
  for (k_val in c(5, 10)) {
    folds_k  <- createFolds(medical$UCURNINS, k = k_val, list = TRUE)
    auc_k    <- numeric(k_val)
    
    for (i in seq_len(k_val)) {
      val_idx  <- folds_k[[i]]
      train_k  <- medical[-val_idx, ]
      val_k    <- medical[val_idx,  ]
      
      m <- glm(UCURNINS ~ UMARSTAT + USATMED + URELATE + REGION + FHOSP +
                 FDENT + FEMER + FDOCT + UIMMSTAT + UAGE + U_FTPT +
                 U_WKSLY + U_USHRS + UBRACE + GENDER + UEDUC3,
               data = train_k, family = binomial())
      
      p     <- predict(m, newdata = val_k, type = "response")
      auc_k[i] <- auc(roc(val_k$UCURNINS, p,
                          levels = c("No","Yes"), quiet = TRUE))
    }
    
    results_comparison <- rbind(results_comparison,
                                data.frame(run = run, k = paste0(k_val, "-Fold"),
                                           auc = mean(auc_k)))
  }
}

# Tổng kết theo từng loại K-Fold
results_comparison %>%
  group_by(k) %>%
  summarise(
    mean_auc = round(mean(auc), 4),
    sd_auc   = round(sd(auc),   4),
    .groups  = "drop"
  )

# Kiểm tra tỷ lệ lớp trong từng fold (có stratify)
set.seed(42)
folds_strat <- createFolds(medical$UCURNINS, k = 5, list = TRUE)

for (i in seq_len(5)) {
  fold_labels <- medical$UCURNINS[folds_strat[[i]]]
  pct_yes     <- round(mean(fold_labels == "Yes") * 100, 1)
  cat(sprintf("Fold %d — tỷ lệ 'Yes': %s%%\n", i, pct_yes))
}

titanic <- read.csv("D:/R/labs/dataset/titanic.csv", stringsAsFactors = TRUE)

# Tiền xử lý cơ bản
titanic_clean <- titanic %>%
  select(survived, pclass, sex, age, sibsp, parch, fare) %>%
  mutate(survived = factor(survived, levels = c(0, 1),
                           labels = c("No", "Yes"))) %>%
  drop_na()

cat("Tỷ lệ sống sót:\n")
print(prop.table(table(titanic_clean$survived)))

# So sánh CV thường vs Stratified
ctrl_normal <- trainControl(
  method = "cv", number = 5,
  classProbs = TRUE, summaryFunction = twoClassSummary
)

ctrl_strat <- trainControl(
  method = "cv", number = 5,
  classProbs = TRUE, summaryFunction = twoClassSummary,
  sampling = NULL  # createFolds trong caret đã stratify theo mặc định
)

set.seed(42)
model_titanic <- train(
  survived ~ pclass + sex + age + sibsp + parch + fare,
  data      = titanic_clean,
  method    = "glm",
  family    = "binomial",
  trControl = ctrl_normal,
  metric    = "ROC"
)

cat("AUC Titanic (5-Fold CV):", round(model_titanic$results$ROC, 4), "\n")

# LOO-CV với caret (chỉ dùng cho tập dữ liệu nhỏ!)
# Với medical_care (35,000 dòng) — KHÔNG khuyến nghị LOO

# Dùng tập nhỏ hơn để minh họa
german <- read.csv("D:/R/labs/dataset/german1.csv")
german$target <- factor(german$target)

ctrl_loo <- trainControl(
  method          = "LOOCV",
  classProbs      = TRUE,
  summaryFunction = twoClassSummary
)

set.seed(42)
# Lấy mẫu nhỏ để demo (LOO rất chậm trên tập lớn)
german_small <- german[1:200, ]

model_loo <- train(
  target ~ .,
  data      = german_small,
  method    = "glm",
  family    = "binomial",
  trControl = ctrl_loo,
  metric    = "ROC"
)

cat("AUC (LOO-CV, 200 obs):", round(model_loo$results$ROC, 4), "\n")

ctrl_boot <- trainControl(
  method          = "boot",
  number          = 50,            # 50 lần bootstrap
  classProbs      = TRUE,
  summaryFunction = twoClassSummary
)

set.seed(42)
model_boot <- train(
  survived ~ pclass + sex + age + sibsp + parch + fare,
  data      = titanic_clean,
  method    = "glm",
  family    = "binomial",
  trControl = ctrl_boot,
  metric    = "ROC"
)

cat("AUC (Bootstrap, 50 reps):", round(model_boot$results$ROC, 4), "\n")

# Chọn số láng giềng tối ưu cho KNN (dùng wines dataset)
wines <- read.csv("D:/R/labs/dataset/wines.csv", stringsAsFactors = TRUE)
wines$type <- factor(wines$type)

# Chuẩn hóa dữ liệu
preproc  <- preProcess(wines[, -which(names(wines) == "type")],
                       method = c("center", "scale"))
wines_sc <- predict(preproc, wines)

ctrl_tune <- trainControl(
  method          = "cv",
  number          = 5,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary
)

set.seed(42)
model_knn <- train(
  type ~ . - quality,
  data      = wines_sc,
  method    = "knn",
  trControl = ctrl_tune,
  metric    = "ROC",
  tuneGrid  = data.frame(k = c(3, 5, 7, 9, 11, 15, 21))
)

# Xem kết quả theo từng giá trị k
print(model_knn$results[, c("k", "ROC", "Sens", "Spec")])

# Vẽ đồ thị
plot(model_knn, main = "Chọn k tối ưu cho KNN qua 5-Fold CV")
cat("k tối ưu:", model_knn$bestTune$k, "\n")
cat("AUC tối đa:", round(max(model_knn$results$ROC), 4), "\n")

set.seed(42)

# Bước 1: Tách ra 20% làm test set (giữ lại hoàn toàn)
test_final_idx  <- createDataPartition(titanic_clean$survived, p = 0.2, list = FALSE)
test_final      <- titanic_clean[test_final_idx, ]
data_for_cv     <- titanic_clean[-test_final_idx, ]

cat("Train+Val set:", nrow(data_for_cv), "| Test set:", nrow(test_final), "\n")

# Bước 2: Dùng CV trên phần còn lại để chọn mô hình tốt nhất
ctrl_tvt <- trainControl(
  method          = "cv",
  number          = 5,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary
)

model_final <- train(
  survived ~ pclass + sex + age + sibsp + parch + fare,
  data      = data_for_cv,
  method    = "glm",
  family    = "binomial",
  trControl = ctrl_tvt,
  metric    = "ROC"
)

cat("AUC trong CV (validation):", round(model_final$results$ROC, 4), "\n")

# Bước 3: Đánh giá cuối cùng trên test set (chỉ chạy 1 lần duy nhất!)
pred_final <- predict(model_final, newdata = test_final, type = "prob")[, "Yes"]
roc_final  <- roc(test_final$survived, pred_final,
                  levels = c("No", "Yes"), quiet = TRUE)

cat("AUC trên TEST SET (đánh giá cuối):", round(auc(roc_final), 4), "\n")

