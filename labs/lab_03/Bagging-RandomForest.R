install.packages(c("randomForest", "ranger", "caret",
                   "vip", "pROC", "tidyverse", "patchwork"))

library(tidyverse)
library(randomForest)  # Package cổ điển, dễ học
library(ranger)        # Nhanh hơn, dùng cho dữ liệu lớn
library(caret)         # Unified interface (nhất quán Bài 17)
library(vip)           # Visualize feature importance
library(pROC)
library(patchwork)     # Ghép nhiều biểu đồ

library(rpart)

titanic <- read.csv("D:/R/labs/dataset/titanic.csv", stringsAsFactors = TRUE)
titanic_clean <- titanic %>%
  select(survived, pclass, sex, age, sibsp, parch, fare) %>%
  mutate(survived = factor(survived, levels = c(0, 1),
                           labels = c("No", "Yes"))) %>%
  drop_na()

set.seed(42)
n <- nrow(titanic_clean)
B <- 200  # Số cây

# Ma trận lưu dự đoán: mỗi cột là một cây
bag_preds <- matrix(NA, nrow = n, ncol = B)

for (b in 1:B) {
  # Bước 1: Bootstrap sample (lấy mẫu CÓ hoàn lại)
  boot_idx  <- sample(1:n, size = n, replace = TRUE)
  boot_data <- titanic_clean[boot_idx, ]
  
  # Bước 2: Huấn luyện Decision Tree trên bootstrap sample
  tree_b <- rpart(survived ~ ., data = boot_data, method = "class")
  
  # Bước 3: Dự đoán trên toàn bộ dữ liệu gốc
  bag_preds[, b] <- as.integer(
    predict(tree_b, newdata = titanic_clean, type = "class") == "Yes"
  )
}

# Bước 4: Majority vote — nếu > 50% cây nói "Yes" → dự đoán "Yes"
vote_pct  <- rowMeans(bag_preds)
bag_final <- factor(ifelse(vote_pct >= 0.5, "Yes", "No"))

# So sánh với cây đơn lẻ
tree_single <- rpart(survived ~ ., data = titanic_clean, method = "class")
pred_single <- predict(tree_single, type = "class")

acc_single  <- mean(pred_single == titanic_clean$survived)
acc_bagging <- mean(bag_final   == titanic_clean$survived)

cat(sprintf("Accuracy — Cây đơn lẻ:      %.4f\n", acc_single))
cat(sprintf("Accuracy — Bagging (%d cây): %.4f\n", B, acc_bagging))

# Theo dõi accuracy của Bagging khi tăng dần số cây
acc_by_ntree <- numeric(B)

for (b in 1:B) {
  vote_b          <- rowMeans(bag_preds[, 1:b, drop = FALSE])
  pred_b          <- factor(ifelse(vote_b >= 0.5, "Yes", "No"))
  acc_by_ntree[b] <- mean(pred_b == titanic_clean$survived)
}

data.frame(ntree = 1:B, accuracy = acc_by_ntree) %>%
  ggplot(aes(x = ntree, y = accuracy)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_hline(yintercept = acc_single, color = "tomato",
             linetype = "dashed", linewidth = 1) +
  annotate("text", x = 150, y = acc_single - 0.005,
           label = "Cây đơn lẻ", color = "tomato") +
  labs(title = "Accuracy của Bagging theo số cây — Titanic",
       x = "Số cây (B)", y = "Accuracy") +
  theme_minimal()

german <- read.csv("german1.csv")
german$target <- factor(german$target, levels = c(1, 2),
                        labels = c("Good", "Bad"))

# Chia train/test bằng caret
set.seed(42)
train_idx <- createDataPartition(german$target, p = 0.8, list = FALSE)
g_train   <- german[train_idx, ]
g_test    <- german[-train_idx, ]

cat("Train:", nrow(g_train), "| Test:", nrow(g_test), "\n")
prop.table(table(g_train$target))

p <- ncol(g_train) - 1  # Số biến đầu vào

set.seed(42)
rf_model <- randomForest(
  target      ~ .,
  data        = g_train,
  ntree       = 500,
  mtry        = floor(sqrt(p)),  # ≈ sqrt(p) cho phân loại
  importance  = TRUE,            # Bật để tính feature importance
  keep.forest = TRUE
)

print(rf_model)

# Dự đoán nhãn
pred_class <- predict(rf_model, newdata = g_test, type = "class")

# Dự đoán xác suất (để tính AUC)
pred_prob  <- predict(rf_model, newdata = g_test, type = "prob")[, "Bad"]

# Confusion Matrix
cm <- confusionMatrix(pred_class, g_test$target, positive = "Bad")
print(cm)

# AUC
roc_rf <- roc(g_test$target, pred_prob,
              levels = c("Good", "Bad"), quiet = TRUE)
cat("\nAUC (Random Forest):", round(auc(roc_rf), 4), "\n")

# So sánh với Decision Tree từ Bài 18
tree_dt  <- rpart(target ~ ., data = g_train, method = "class")
pred_dt  <- predict(tree_dt, newdata = g_test, type = "prob")[, "Bad"]
roc_dt   <- roc(g_test$target, pred_dt,
                levels = c("Good", "Bad"), quiet = TRUE)

cat("AUC (Decision Tree):", round(auc(roc_dt), 4), "\n")
cat("Cải thiện:          ", round(auc(roc_rf) - auc(roc_dt), 4), "\n")

# err.rate: ma trận lưu OOB error theo từng cây
oob_df <- data.frame(
  ntree = 1:500,
  OOB   = rf_model$err.rate[, "OOB"],
  Good  = rf_model$err.rate[, "Good"],
  Bad   = rf_model$err.rate[, "Bad"]
)

oob_df %>%
  pivot_longer(-ntree, names_to = "Type", values_to = "Error") %>%
  ggplot(aes(x = ntree, y = Error, color = Type)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("OOB"  = "black",
                                "Good" = "steelblue",
                                "Bad"  = "tomato")) +
  labs(title    = "OOB Error theo số cây — German Credit",
       subtitle  = "OOB Error hội tụ sau ~100–150 cây",
       x = "Số cây (ntree)", y = "Error Rate",
       color = "Loại lỗi") +
  theme_minimal()

# Tìm ntree tối thiểu mà OOB error đã ổn định
oob_stable <- which(abs(diff(oob_df$OOB, lag = 50)) < 0.001)[1] + 50
cat("OOB error ổn định sau khoảng:", oob_stable, "cây\n")

# --- MDI: Mean Decrease Gini ---
importance_mdi <- as.data.frame(importance(rf_model, type = 2)) %>%
  rownames_to_column("variable") %>%
  rename(MDI = MeanDecreaseGini) %>%
  arrange(desc(MDI))

# --- MDA: Mean Decrease Accuracy (Permutation) ---
importance_mda <- as.data.frame(importance(rf_model, type = 1)) %>%
  rownames_to_column("variable") %>%
  rename(MDA = MeanDecreaseAccuracy) %>%
  arrange(desc(MDA))

# Vẽ song song để so sánh
p1 <- ggplot(importance_mdi %>% head(15),
             aes(x = reorder(variable, MDI), y = MDI)) +
  geom_col(fill = "steelblue") + coord_flip() +
  labs(title = "MDI (Mean Decrease Gini)", x = NULL, y = "Importance") +
  theme_minimal()

p2 <- ggplot(importance_mda %>% head(15),
             aes(x = reorder(variable, MDA), y = MDA)) +
  geom_col(fill = "tomato") + coord_flip() +
  labs(title = "MDA (Permutation Importance)", x = NULL, y = "Importance") +
  theme_minimal()

p1 + p2 +
  plot_annotation(title = "Feature Importance — German Credit Random Forest")

rank_compare <- importance_mdi %>%
  mutate(rank_MDI = row_number()) %>%
  left_join(
    importance_mda %>% mutate(rank_MDA = row_number()),
    by = "variable"
  ) %>%
  select(variable, rank_MDI, rank_MDA) %>%
  mutate(rank_diff = abs(rank_MDI - rank_MDA)) %>%
  arrange(rank_MDI)

print(head(rank_compare, 10))
cat("\nBiến có thứ hạng chênh lệch nhiều nhất:\n")
print(rank_compare %>% arrange(desc(rank_diff)) %>% head(5))

ctrl_rf <- trainControl(
  method          = "cv",
  number          = 5,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Thử nhiều giá trị mtry
tune_grid <- expand.grid(mtry = c(2, 3, 5, 7, 10, 15, 20))

set.seed(42)
rf_caret <- train(
  target    ~ .,
  data      = g_train,
  method    = "rf",
  trControl = ctrl_rf,
  tuneGrid  = tune_grid,
  metric    = "ROC",
  ntree     = 300
)

# Kết quả theo từng mtry
print(rf_caret$results[, c("mtry", "ROC", "Sens", "Spec")])
plot(rf_caret, main = "Chọn mtry tối ưu — 5-Fold CV (caret)")

cat("mtry tối ưu:", rf_caret$bestTune$mtry, "\n")
cat("AUC tốt nhất:", round(max(rf_caret$results$ROC), 4), "\n")

wines <- read.csv("wines.csv", stringsAsFactors = TRUE)

# Dự đoán quality (biến liên tục)
wines_reg <- wines %>% select(-type)

# Chia train/test bằng caret
set.seed(42)
w_idx   <- createDataPartition(wines_reg$quality, p = 0.8, list = FALSE)
w_train <- wines_reg[w_idx, ]
w_test  <- wines_reg[-w_idx, ]

# Regression Random Forest
set.seed(42)
rf_reg <- randomForest(
  quality    ~ .,
  data       = w_train,
  ntree      = 300,
  mtry       = floor(ncol(w_train) / 3),  # p/3 cho hồi quy
  importance = TRUE
)

print(rf_reg)

# Đánh giá
pred_rf_reg <- predict(rf_reg, newdata = w_test)

rmse_rf <- sqrt(mean((w_test$quality - pred_rf_reg)^2))
mae_rf  <- mean(abs(w_test$quality - pred_rf_reg))
r2_rf   <- 1 - sum((w_test$quality - pred_rf_reg)^2) /
  sum((w_test$quality - mean(w_test$quality))^2)

# So sánh với Regression Tree (Bài 18) và Linear Regression
lm_wine   <- lm(quality ~ ., data = w_train)
pred_lm   <- predict(lm_wine, newdata = w_test)
rmse_lm   <- sqrt(mean((w_test$quality - pred_lm)^2))
r2_lm     <- 1 - sum((w_test$quality - pred_lm)^2) /
  sum((w_test$quality - mean(w_test$quality))^2)

tree_reg  <- rpart(quality ~ ., data = w_train, method = "anova")
pred_tree <- predict(tree_reg, newdata = w_test)
rmse_tree <- sqrt(mean((w_test$quality - pred_tree)^2))
r2_tree   <- 1 - sum((w_test$quality - pred_tree)^2) /
  sum((w_test$quality - mean(w_test$quality))^2)

# Bảng so sánh
data.frame(
  Model = c("Linear Regression", "Decision Tree", "Random Forest"),
  RMSE  = round(c(rmse_lm, rmse_tree, rmse_rf), 4),
  R2    = round(c(r2_lm,   r2_tree,   r2_rf),   4)
) %>%
  arrange(RMSE) %>%
  print()

data.frame(
  actual    = w_test$quality,
  predicted = pred_rf_reg
) %>%
  ggplot(aes(x = actual, y = predicted)) +
  geom_jitter(alpha = 0.3, color = "steelblue", width = 0.1) +
  geom_abline(slope = 1, intercept = 0, color = "red",
              linetype = "dashed", linewidth = 1) +
  labs(title    = "Predicted vs Actual — Random Forest Regression (Wines)",
       subtitle  = paste0("RMSE = ", round(rmse_rf, 4),
                          " | R² = ", round(r2_rf, 4)),
       x = "Actual Quality", y = "Predicted Quality") +
  theme_minimal()

titanic_clean <- titanic %>%
  select(survived, pclass, sex, age, sibsp, parch, fare) %>%
  mutate(survived = factor(survived, levels = c(0, 1),
                           labels = c("No", "Yes"))) %>%
  drop_na()

set.seed(42)
tit_idx   <- createDataPartition(titanic_clean$survived, p = 0.8, list = FALSE)
tit_train <- titanic_clean[tit_idx, ]
tit_test  <- titanic_clean[-tit_idx, ]

ctrl_comp <- trainControl(
  method = "cv", number = 5,
  classProbs = TRUE, summaryFunction = twoClassSummary
)

# Decision Tree (Bài 18)
set.seed(42)
m_tree <- train(survived ~ ., data = tit_train, method = "rpart",
                trControl = ctrl_comp, metric = "ROC",
                tuneGrid = expand.grid(cp = c(0, 0.001, 0.01, 0.05)))

# Bagging (treebag = Bagging với Decision Tree)
set.seed(42)
m_bag  <- train(survived ~ ., data = tit_train, method = "treebag",
                trControl = ctrl_comp, metric = "ROC")

# Random Forest
set.seed(42)
m_rf   <- train(survived ~ ., data = tit_train, method = "rf",
                trControl = ctrl_comp, metric = "ROC",
                tuneGrid = expand.grid(mtry = c(2, 3, 4)),
                ntree = 300)

# So sánh CV
results_tit <- resamples(list(
  DecisionTree = m_tree,
  Bagging      = m_bag,
  RandomForest = m_rf
))

summary(results_tit, metric = "ROC")
dotplot(results_tit, metric = "ROC",
        main = "So sánh AUC (5-Fold CV) — Titanic")

# Đánh giá trên test set
get_auc_test <- function(model, test, truth_col, pos_level) {
  p   <- predict(model, newdata = test, type = "prob")[, pos_level]
  lvl <- levels(test[[truth_col]])
  round(auc(roc(test[[truth_col]], p, levels = lvl, quiet = TRUE)), 4)
}

cat("\n--- AUC trên Test Set ---\n")
cat("Decision Tree:", get_auc_test(m_tree, tit_test, "survived", "Yes"), "\n")
cat("Bagging:      ", get_auc_test(m_bag,  tit_test, "survived", "Yes"), "\n")
cat("Random Forest:", get_auc_test(m_rf,   tit_test, "survived", "Yes"), "\n")

medical <- read.csv("medical_care.csv", stringsAsFactors = TRUE)

medical_clean <- medical %>%
  select(UCURNINS, UMARSTAT, USATMED, REGION, FHOSP, FDENT, FEMER,
         FDOCT, UIMMSTAT, UAGE, U_FTPT, U_WKSLY, UBRACE, GENDER, UEDUC3) %>%
  drop_na() %>%
  mutate(UCURNINS = factor(UCURNINS))

# Chia train/test bằng caret
set.seed(42)
med_idx   <- createDataPartition(medical_clean$UCURNINS, p = 0.8, list = FALSE)
med_train <- medical_clean[med_idx, ]
med_test  <- medical_clean[-med_idx, ]

# ranger: nhanh hơn randomForest, hỗ trợ parallel
set.seed(42)
rf_ranger <- ranger(
  UCURNINS    ~ .,
  data        = med_train,
  num.trees   = 300,
  mtry        = 4,
  importance  = "impurity",                        # MDI
  probability = TRUE,                              # Trả về xác suất
  num.threads = parallel::detectCores() - 1,       # Dùng đa nhân
  verbose     = FALSE
)

cat("OOB Prediction Error:", round(rf_ranger$prediction.error, 4), "\n")

# Dự đoán trên test set
pred_ranger <- predict(rf_ranger, data = med_test)$predictions[, "Yes"]
roc_med     <- roc(med_test$UCURNINS, pred_ranger,
                   levels = c("No", "Yes"), quiet = TRUE)
cat("AUC (Random Forest — Medical):", round(auc(roc_med), 4), "\n")

# So sánh với Logistic Regression (Bài 17)
lr_med  <- glm(UCURNINS ~ UMARSTAT + USATMED + REGION + FHOSP + FDENT +
                 FEMER + FDOCT + UIMMSTAT + UAGE + U_FTPT + U_WKSLY +
                 UBRACE + GENDER + UEDUC3,
               data = med_train, family = binomial())
pred_lr <- predict(lr_med, newdata = med_test, type = "response")
roc_lr  <- roc(med_test$UCURNINS, pred_lr,
               levels = c("No", "Yes"), quiet = TRUE)

# So sánh với Decision Tree (Bài 18)
tree_med <- rpart(UCURNINS ~ ., data = med_train, method = "class")
pred_dt  <- predict(tree_med, newdata = med_test, type = "prob")[, "Yes"]
roc_dt   <- roc(med_test$UCURNINS, pred_dt,
                levels = c("No", "Yes"), quiet = TRUE)

cat("\n--- So sánh AUC trên Test Set (Medical Care) ---\n")
cat("Logistic Regression (Bài 17):", round(auc(roc_lr),  4), "\n")
cat("Decision Tree (Bài 18):      ", round(auc(roc_dt),  4), "\n")
cat("Random Forest (Bài 19):      ", round(auc(roc_med), 4), "\n")

# Feature Importance từ ranger
imp_ranger <- data.frame(
  variable   = names(rf_ranger$variable.importance),
  importance = rf_ranger$variable.importance
) %>%
  arrange(desc(importance))

ggplot(imp_ranger, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance (MDI) — Random Forest (Medical Care)",
       x = NULL, y = "Mean Decrease Impurity") +
  theme_minimal()

# Benchmark tốc độ trên Medical Care
cat("--- Benchmark tốc độ ---\n")

# randomForest (single thread)
t1 <- system.time(
  randomForest(UCURNINS ~ ., data = med_train,
               ntree = 100, importance = FALSE)
)

# ranger (multi thread)
t2 <- system.time(
  ranger(UCURNINS ~ ., data = med_train,
         num.trees = 100, num.threads = parallel::detectCores() - 1,
         verbose = FALSE)
)

cat(sprintf("randomForest: %.1f giây\n", t1["elapsed"]))
cat(sprintf("ranger:       %.1f giây\n", t2["elapsed"]))
cat(sprintf("ranger nhanh hơn: %.1fx\n", t1["elapsed"] / t2["elapsed"]))

