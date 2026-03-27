install.packages(c("rpart", "rpart.plot", "caret", "pROC", "tidyverse", "vip"))

library(tidyverse)
library(rpart)        # Xây dựng Decision Tree (CART)
library(rpart.plot)   # Visualize cây đẹp hơn
library(caret)        # Tuning + CV (nhất quán Bài 17)
library(pROC)
library(vip)

titanic <- read.csv("D:/R/labs/dataset/titanic.csv", stringsAsFactors = TRUE)

titanic_clean <- titanic %>%
  select(survived, pclass, sex, age, sibsp, parch, fare) %>%
  mutate(survived = factor(survived, levels = c(0, 1),
                           labels = c("No", "Yes"))) %>%
  drop_na()

set.seed(42)
train_idx  <- createDataPartition(titanic_clean$survived, p = 0.8, list = FALSE)
tit_train  <- titanic_clean[train_idx, ]
tit_test   <- titanic_clean[-train_idx, ]

cat("Train:", nrow(tit_train), "| Test:", nrow(tit_test), "\n")
prop.table(table(tit_train$survived))

set.seed(42)
tree_default <- rpart(
  survived ~ .,
  data   = tit_train,
  method = "class"    # "class" cho phân loại, "anova" cho hồi quy
)

# Visualize
rpart.plot(
  tree_default,
  type    = 4,         # Kiểu vẽ (0–5, type=4 hiển thị rõ nhất)
  extra   = 104,       # Hiển thị xác suất và % quan sát
  under   = TRUE,
  fallen.leaves = TRUE,
  main    = "Decision Tree — Titanic (Mặc định)"
)

# Xem cấu trúc dạng text
print(tree_default)

# Xem bảng cp (complexity parameter)
printcp(tree_default)
plotcp(tree_default)   # Vẽ cross-validation error theo cp

# Dự đoán nhãn
pred_class <- predict(tree_default, newdata = tit_test, type = "class")

# Dự đoán xác suất (để tính AUC)
pred_prob <- predict(tree_default, newdata = tit_test, type = "prob")[, "Yes"]

# Confusion matrix
cm <- confusionMatrix(pred_class, tit_test$survived, positive = "Yes")
print(cm)

# AUC
roc_tree <- roc(tit_test$survived, pred_prob,
                levels = c("No", "Yes"), quiet = TRUE)
cat("AUC (Decision Tree mặc định):", round(auc(roc_tree), 4), "\n")

# Huấn luyện cây ở nhiều độ sâu khác nhau
depths   <- 1:10
acc_train <- numeric(length(depths))
acc_test  <- numeric(length(depths))

for (i in seq_along(depths)) {
  tree_d <- rpart(survived ~ ., data = tit_train, method = "class",
                  control = rpart.control(maxdepth = depths[i], cp = 0))
  
  acc_train[i] <- mean(predict(tree_d, tit_train, type = "class") == tit_train$survived)
  acc_test[i]  <- mean(predict(tree_d, tit_test,  type = "class") == tit_test$survived)
}

# Visualize bias-variance tradeoff
data.frame(depth = depths, Train = acc_train, Test = acc_test) %>%
  pivot_longer(-depth, names_to = "Set", values_to = "Accuracy") %>%
  ggplot(aes(x = depth, y = Accuracy, color = Set, linetype = Set)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Train" = "steelblue", "Test" = "tomato")) +
  labs(title  = "Bias–Variance Tradeoff — Decision Tree (Titanic)",
       subtitle = "Train Accuracy vs Test Accuracy theo độ sâu cây",
       x = "Độ sâu cây (maxdepth)", y = "Accuracy") +
  theme_minimal()

# Xây cây đầy đủ (cp = 0)
set.seed(42)
tree_full <- rpart(survived ~ ., data = tit_train, method = "class",
                   control = rpart.control(cp = 0, minsplit = 2))

# Bảng cp và CV error
cp_table <- as.data.frame(tree_full$cptable)
print(cp_table)

# Vẽ CV error theo cp
plotcp(tree_full, main = "Cross-Validation Error theo Complexity Parameter (cp)")

# Tìm cp tối ưu: cp có xerror nhỏ nhất
best_cp <- cp_table$CP[which.min(cp_table$xerror)]
cat("cp tối ưu:", round(best_cp, 6), "\n")

# Cắt tỉa cây
tree_pruned <- prune(tree_full, cp = best_cp)

rpart.plot(tree_pruned, type = 4, extra = 104, fallen.leaves = TRUE,
           main = paste("Cây sau Pruning (cp =", round(best_cp, 4), ")"))

# So sánh kết quả
pred_pruned <- predict(tree_pruned, tit_test, type = "class")
acc_full    <- mean(predict(tree_full,   tit_test, type = "class") == tit_test$survived)
acc_pruned  <- mean(pred_pruned == tit_test$survived)

cat(sprintf("Accuracy — Cây đầy đủ:  %.4f\n", acc_full))
cat(sprintf("Accuracy — Sau pruning: %.4f\n", acc_pruned))

ctrl <- trainControl(
  method          = "cv",
  number          = 10,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Grid search cp
cp_grid <- expand.grid(cp = c(0, 0.001, 0.005, 0.01, 0.02, 0.05, 0.1))

set.seed(42)
tree_caret <- train(
  survived ~ .,
  data      = tit_train,
  method    = "rpart",
  trControl = ctrl,
  tuneGrid  = cp_grid,
  metric    = "ROC"
)

# Kết quả theo từng cp
print(tree_caret$results[, c("cp", "ROC", "Sens", "Spec")])
plot(tree_caret, main = "Chọn cp tối ưu qua 10-Fold CV (caret)")

cat("cp tối ưu (caret):", tree_caret$bestTune$cp, "\n")
cat("AUC tốt nhất:     ", round(max(tree_caret$results$ROC), 4), "\n")

# Vẽ cây sau khi pruning (dùng kết quả từ caret)
best_tree_model <- tree_caret$finalModel
rpart.plot(
  best_tree_model,
  type          = 4,
  extra         = 104,
  fallen.leaves = TRUE,
  box.palette   = "RdYlGn",
  shadow.col    = "gray",
  main          = "Decision Tree tối ưu — Titanic"
)

# Feature Importance
importance_df <- data.frame(
  variable   = names(best_tree_model$variable.importance),
  importance = best_tree_model$variable.importance
) %>%
  arrange(desc(importance))

ggplot(importance_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance — Decision Tree (Titanic)",
       x = NULL, y = "Importance (Gini Decrease)") +
  theme_minimal()

wines <- read.csv("wines.csv", stringsAsFactors = TRUE)

# Dùng các biến hóa học để dự đoán quality
wines_reg <- wines %>% select(-type)

# Chia train/test bằng caret
set.seed(42)
w_idx   <- createDataPartition(wines_reg$quality, p = 0.8, list = FALSE)
w_train <- wines_reg[w_idx, ]
w_test  <- wines_reg[-w_idx, ]

# Regression Tree (method = "anova")
tree_reg <- rpart(
  quality ~ .,
  data    = w_train,
  method  = "anova",
  control = rpart.control(cp = 0.01)
)

rpart.plot(
  tree_reg,
  type          = 4,
  extra         = 101,    # Hiển thị số quan sát tại mỗi nút
  fallen.leaves = TRUE,
  main          = "Regression Tree — Wine Quality"
)

# Đánh giá
pred_reg <- predict(tree_reg, newdata = w_test)

rmse_tree <- sqrt(mean((w_test$quality - pred_reg)^2))
mae_tree  <- mean(abs(w_test$quality - pred_reg))
r2_tree   <- 1 - sum((w_test$quality - pred_reg)^2) /
  sum((w_test$quality - mean(w_test$quality))^2)

cat(sprintf("RMSE: %.4f\n", rmse_tree))
cat(sprintf("MAE:  %.4f\n", mae_tree))
cat(sprintf("R²:   %.4f\n", r2_tree))

# So sánh với Linear Regression
lm_wine <- lm(quality ~ ., data = w_train)
pred_lm  <- predict(lm_wine, newdata = w_test)

rmse_lm <- sqrt(mean((w_test$quality - pred_lm)^2))
r2_lm   <- 1 - sum((w_test$quality - pred_lm)^2) /
  sum((w_test$quality - mean(w_test$quality))^2)

cat(sprintf("\nSo sánh:\n"))
cat(sprintf("  Regression Tree — RMSE: %.4f | R²: %.4f\n", rmse_tree, r2_tree))
cat(sprintf("  Linear Regression—RMSE: %.4f | R²: %.4f\n", rmse_lm,   r2_lm))

# Cây đầy đủ rồi tìm cp tối ưu
set.seed(42)
tree_reg_full <- rpart(quality ~ ., data = w_train, method = "anova",
                       control = rpart.control(cp = 0))

plotcp(tree_reg_full, main = "CV Error theo cp — Wine Quality")

best_cp_reg     <- tree_reg_full$cptable[which.min(tree_reg_full$cptable[, "xerror"]), "CP"]
tree_reg_pruned <- prune(tree_reg_full, cp = best_cp_reg)

pred_pruned_reg <- predict(tree_reg_pruned, newdata = w_test)
rmse_pruned     <- sqrt(mean((w_test$quality - pred_pruned_reg)^2))

cat(sprintf("RMSE — Cây đầy đủ:  %.4f\n",
            sqrt(mean((w_test$quality - predict(tree_reg_full, w_test))^2))))
cat(sprintf("RMSE — Sau pruning: %.4f\n", rmse_pruned))

german <- read.csv("german1.csv")
german$target <- factor(german$target, levels = c(1, 2),
                        labels = c("Good", "Bad"))

# Chia train/test bằng caret
set.seed(42)
g_idx   <- createDataPartition(german$target, p = 0.8, list = FALSE)
g_train <- german[g_idx, ]
g_test  <- german[-g_idx, ]

cat("German — Train:", nrow(g_train), "| Test:", nrow(g_test), "\n")

ctrl_compare <- trainControl(
  method = "cv", number = 10,
  classProbs = TRUE, summaryFunction = twoClassSummary
)

# Logistic Regression
set.seed(42)
m_lr <- train(target ~ ., data = g_train, method = "glm",
              family = "binomial", trControl = ctrl_compare, metric = "ROC")

# Decision Tree (tuning cp)
set.seed(42)
m_tree <- train(target ~ ., data = g_train, method = "rpart",
                trControl = ctrl_compare, metric = "ROC",
                tuneGrid = expand.grid(cp = c(0, 0.001, 0.005, 0.01, 0.02, 0.05)))

# Tổng hợp kết quả CV
results <- resamples(list(
  LogisticRegression = m_lr,
  DecisionTree       = m_tree
))

summary(results, metric = "ROC")
dotplot(results, metric = "ROC",
        main = "So sánh AUC (10-Fold CV) — German Credit")

# Đánh giá trên test set
get_auc <- function(model, test, truth) {
  p       <- predict(model, newdata = test, type = "prob")[, "Bad"]
  roc_obj <- roc(test[[truth]], p, levels = c("Good", "Bad"), quiet = TRUE)
  round(auc(roc_obj), 4)
}

cat("AUC trên TEST SET:\n")
cat("  Logistic Regression:", get_auc(m_lr,   g_test, "target"), "\n")
cat("  Decision Tree:      ", get_auc(m_tree, g_test, "target"), "\n")
# Dự đoán type (red/white) — ranh giới trong không gian hóa học
wines_clf       <- wines %>% select(-quality)
wines_clf$type  <- factor(wines_clf$type)

set.seed(42)
wc_idx   <- createDataPartition(wines_clf$type, p = 0.8, list = FALSE)
wc_train <- wines_clf[wc_idx, ]
wc_test  <- wines_clf[-wc_idx, ]

ctrl_w <- trainControl(method = "cv", number = 5,
                       classProbs = TRUE, summaryFunction = twoClassSummary)

set.seed(42)
m_lr_w <- train(type ~ ., data = wc_train, method = "glm",
                family = "binomial", trControl = ctrl_w, metric = "ROC")

set.seed(42)
m_tree_w <- train(type ~ ., data = wc_train, method = "rpart",
                  trControl = ctrl_w, metric = "ROC",
                  tuneGrid = expand.grid(cp = c(0, 0.001, 0.005, 0.01)))

cat("\n=== Wines — Phân loại red/white ===\n")
cat("Logistic Regression AUC (CV):",
    round(max(m_lr_w$results$ROC), 4), "\n")
cat("Decision Tree AUC (CV):      ",
    round(max(m_tree_w$results$ROC), 4), "\n")

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

# Decision Tree với cp được tuned
ctrl_med <- trainControl(
  method = "cv", number = 5,
  classProbs = TRUE, summaryFunction = twoClassSummary
)

set.seed(42)
tree_med <- train(
  UCURNINS ~ .,
  data      = med_train,
  method    = "rpart",
  trControl = ctrl_med,
  metric    = "ROC",
  tuneGrid  = expand.grid(cp = c(0, 0.0001, 0.0005, 0.001, 0.005, 0.01))
)

cat("cp tối ưu (Medical):", tree_med$bestTune$cp, "\n")
cat("AUC (CV):           ", round(max(tree_med$results$ROC), 4), "\n")

# Test set
pred_med <- predict(tree_med, newdata = med_test, type = "prob")[, "Yes"]
roc_med  <- roc(med_test$UCURNINS, pred_med, levels = c("No", "Yes"), quiet = TRUE)
cat("AUC (Test set):     ", round(auc(roc_med), 4), "\n")

# So sánh với Logistic Regression
lr_med <- glm(UCURNINS ~ UMARSTAT + USATMED + REGION + FHOSP + FDENT +
                FEMER + FDOCT + UIMMSTAT + UAGE + U_FTPT + U_WKSLY +
                UBRACE + GENDER + UEDUC3,
              data = med_train, family = binomial())

pred_lr_med <- predict(lr_med, newdata = med_test, type = "response")
roc_lr_med  <- roc(med_test$UCURNINS, pred_lr_med,
                   levels = c("No", "Yes"), quiet = TRUE)

cat("\n--- So sánh trên Test Set (Medical) ---\n")
cat("Logistic Regression AUC:", round(auc(roc_lr_med), 4), "\n")
cat("Decision Tree AUC:      ", round(auc(roc_med),    4), "\n")

# Vẽ cây tốt nhất
rpart.plot(tree_med$finalModel, type = 4, extra = 104, fallen.leaves = TRUE,
           main = "Decision Tree tối ưu — Medical Care",
           box.palette = "RdBu")

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

# Decision Tree với cp được tuned
ctrl_med <- trainControl(
  method = "cv", number = 5,
  classProbs = TRUE, summaryFunction = twoClassSummary
)

set.seed(42)
tree_med <- train(
  UCURNINS ~ .,
  data      = med_train,
  method    = "rpart",
  trControl = ctrl_med,
  metric    = "ROC",
  tuneGrid  = expand.grid(cp = c(0, 0.0001, 0.0005, 0.001, 0.005, 0.01))
)

cat("cp tối ưu (Medical):", tree_med$bestTune$cp, "\n")
cat("AUC (CV):           ", round(max(tree_med$results$ROC), 4), "\n")

# Test set
pred_med <- predict(tree_med, newdata = med_test, type = "prob")[, "Yes"]
roc_med  <- roc(med_test$UCURNINS, pred_med, levels = c("No", "Yes"), quiet = TRUE)
cat("AUC (Test set):     ", round(auc(roc_med), 4), "\n")

# So sánh với Logistic Regression
lr_med <- glm(UCURNINS ~ UMARSTAT + USATMED + REGION + FHOSP + FDENT +
                FEMER + FDOCT + UIMMSTAT + UAGE + U_FTPT + U_WKSLY +
                UBRACE + GENDER + UEDUC3,
              data = med_train, family = binomial())

pred_lr_med <- predict(lr_med, newdata = med_test, type = "response")
roc_lr_med  <- roc(med_test$UCURNINS, pred_lr_med,
                   levels = c("No", "Yes"), quiet = TRUE)

cat("\n--- So sánh trên Test Set (Medical) ---\n")
cat("Logistic Regression AUC:", round(auc(roc_lr_med), 4), "\n")
cat("Decision Tree AUC:      ", round(auc(roc_med),    4), "\n")

# Vẽ cây tốt nhất
rpart.plot(tree_med$finalModel, type = 4, extra = 104, fallen.leaves = TRUE,
           main = "Decision Tree tối ưu — Medical Care",
           box.palette = "RdBu")

summary_results <- data.frame(
  Dataset  = c("Titanic", "German Credit", "Wines (type)", "Medical Care"),
  AUC_Tree = c(
    round(max(tree_caret$results$ROC), 4),
    round(max(m_tree$results$ROC),     4),
    round(max(m_tree_w$results$ROC),   4),
    round(max(tree_med$results$ROC),   4)
  ),
  AUC_LR = c(
    round(max(m_lr$results$ROC),   4),
    round(max(m_lr$results$ROC),   4),
    round(max(m_lr_w$results$ROC), 4),
    NA
  )
)

print(summary_results)


