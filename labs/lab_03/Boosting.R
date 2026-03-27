install.packages(c("gbm", "xgboost", "caret", "pROC", "tidyverse", "vip"))

library(tidyverse)
library(gbm)        # Gradient Boosting Machine
library(xgboost)    # XGBoost
library(caret)      # Unified interface (nhất quán Bài 17–19)
library(pROC)
library(vip)

german <- read.csv("german1.csv")
german$target <- factor(german$target, levels = c(1, 2),
                        labels = c("Good", "Bad"))

set.seed(42)
train_idx <- createDataPartition(german$target, p = 0.8, list = FALSE)
g_train   <- german[train_idx, ]
g_test    <- german[-train_idx, ]

cat("Train:", nrow(g_train), "| Test:", nrow(g_test), "\n")

# AdaBoost dùng caret với method = "adaboost"
# install.packages("adabag") nếu chưa có
ctrl <- trainControl(
  method          = "cv",
  number          = 5,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary
)

set.seed(42)
m_ada <- train(
  target ~ .,
  data      = g_train,
  method    = "adaboost",
  trControl = ctrl,
  metric    = "ROC",
  tuneGrid  = expand.grid(
    nIter  = c(50, 100, 150, 200),
    method = "Adaboost.M1"
  )
)

print(m_ada$results[, c("nIter", "ROC")])
cat("Số vòng tối ưu:", m_ada$bestTune$nIter, "\n")
cat("AUC (CV):      ", round(max(m_ada$results$ROC), 4), "\n")

# Đánh giá trên test set
pred_ada <- predict(m_ada, newdata = g_test, type = "prob")[, "Bad"]
roc_ada  <- roc(g_test$target, pred_ada,
                levels = c("Good", "Bad"), quiet = TRUE)
cat("AUC (Test):    ", round(auc(roc_ada), 4), "\n")

library(rpart)

# Dùng biến số để đơn giản
g_num <- german %>%
  select(where(is.numeric), target) %>%
  mutate(y = ifelse(target == "Bad", 1, -1)) %>%
  select(-target)

n <- nrow(g_num)
w <- rep(1/n, n)  # Trọng số ban đầu bằng nhau

set.seed(42)
M_demo <- 5  # Demo 5 vòng

for (m in 1:M_demo) {
  # Huấn luyện cây nông (decision stump: depth = 1)
  tree_m <- rpart(y ~ . - y, data = g_num, method = "anova",
                  weights = w,
                  control = rpart.control(maxdepth = 1, cp = 0))
  
  pred_m  <- sign(predict(tree_m, g_num))
  correct <- (pred_m == g_num$y)
  
  # Weighted error
  eps_m   <- sum(w[!correct]) / sum(w)
  alpha_m <- 0.5 * log((1 - eps_m) / eps_m)
  
  # Cập nhật trọng số
  w <- w * exp(-alpha_m * g_num$y * pred_m)
  w <- w / sum(w)  # Chuẩn hóa
  
  cat(sprintf("Vòng %d | eps=%.4f | alpha=%.4f | w_max=%.4f | w_min=%.6f\n",
              m, eps_m, alpha_m, max(w), min(w)))
}

titanic <- read.csv("titanic.csv", stringsAsFactors = TRUE)

titanic_clean <- titanic %>%
  select(survived, pclass, sex, age, sibsp, parch, fare) %>%
  mutate(
    survived = as.integer(survived),       # gbm cần 0/1 numeric
    sex      = as.integer(sex == "female")
  ) %>%
  drop_na()

# Chia train/test bằng caret
set.seed(42)
tit_idx   <- createDataPartition(titanic_clean$survived, p = 0.8, list = FALSE)
tit_train <- titanic_clean[tit_idx, ]
tit_test  <- titanic_clean[-tit_idx, ]

# Huấn luyện GBM
set.seed(42)
gbm_model <- gbm(
  survived ~ .,
  data              = tit_train,
  distribution      = "bernoulli",  # Phân loại nhị phân
  n.trees           = 500,
  shrinkage         = 0.05,         # Learning rate η
  interaction.depth = 3,            # Độ sâu mỗi cây
  n.minobsinnode    = 10,
  bag.fraction      = 0.8,          # Stochastic GBM
  cv.folds          = 5,            # Cross-validation để tìm ntree tối ưu
  verbose           = FALSE
)

# Tìm số cây tối ưu qua CV
best_ntree <- gbm.perf(gbm_model, method = "cv", plot.it = TRUE)
cat("Số cây tối ưu (CV):", best_ntree, "\n")

# Dự đoán với số cây tối ưu
pred_gbm <- predict(gbm_model, newdata = tit_test,
                    n.trees = best_ntree, type = "response")

roc_gbm <- roc(tit_test$survived, pred_gbm, quiet = TRUE)
cat("AUC (GBM):", round(auc(roc_gbm), 4), "\n")

imp_gbm <- summary(gbm_model, n.trees = best_ntree, plotit = FALSE)

ggplot(imp_gbm, aes(x = reorder(var, rel.inf), y = rel.inf)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance — GBM (Titanic)",
       x = NULL, y = "Relative Influence (%)") +
  theme_minimal()

plot(gbm_model, i.var = "age",    n.trees = best_ntree,
     main = "Partial Dependence: Age vs P(survived)")

plot(gbm_model, i.var = "pclass", n.trees = best_ntree,
     main = "Partial Dependence: Pclass vs P(survived)")

# Chuẩn bị dữ liệu: chuyển sang matrix số
X_train <- model.matrix(target ~ . - 1, data = g_train)
X_test  <- model.matrix(target ~ . - 1, data = g_test)

y_train <- as.integer(g_train$target == "Bad")  # 1 = Bad, 0 = Good
y_test  <- as.integer(g_test$target  == "Bad")

# Tạo DMatrix — định dạng tối ưu của XGBoost
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

# Tham số XGBoost
params <- list(
  objective        = "binary:logistic",
  eval_metric      = "auc",
  eta              = 0.05,
  max_depth        = 4,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  lambda           = 1,    # L2 regularization
  alpha            = 0     # L1 regularization
)

# Huấn luyện với Early Stopping
set.seed(42)
xgb_model <- xgb.train(
  params                = params,
  data                  = dtrain,
  nrounds               = 500,
  watchlist             = list(train = dtrain, test = dtest),
  early_stopping_rounds = 30,
  print_every_n         = 50,
  verbose               = 1
)

cat("\nSố vòng tối ưu (Early Stopping):", xgb_model$best_iteration, "\n")
cat("AUC tốt nhất (validation):      ", round(xgb_model$best_score, 4), "\n")

pred_xgb <- predict(xgb_model, newdata = dtest)

roc_xgb <- roc(y_test, pred_xgb, quiet = TRUE)
cat("AUC (XGBoost, test):", round(auc(roc_xgb), 4), "\n")

# Confusion matrix
pred_class_xgb <- factor(ifelse(pred_xgb >= 0.5, "Bad", "Good"),
                         levels = c("Good", "Bad"))
confusionMatrix(pred_class_xgb, g_test$target, positive = "Bad")

# XGBoost cung cấp 3 loại importance:
# Gain: tổng mức giảm loss (tương tự MDI)
# Cover: số quan sát bị ảnh hưởng
# Frequency: tần suất biến được chọn để phân chia
imp_xgb <- xgb.importance(model = xgb_model)
print(head(imp_xgb, 10))

xgb.plot.importance(imp_xgb, top_n = 15,
                    main = "Feature Importance — XGBoost (German Credit)")

ctrl_xgb <- trainControl(
  method          = "cv",
  number          = 5,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter     = FALSE
)

xgb_grid <- expand.grid(
  nrounds          = c(100, 200, 300),
  eta              = c(0.01, 0.05, 0.1),
  max_depth        = c(3, 4, 6),
  gamma            = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample        = 0.8
)

set.seed(42)
m_xgb <- train(
  target    ~ .,
  data      = g_train,
  method    = "xgbTree",
  trControl = ctrl_xgb,
  tuneGrid  = xgb_grid,
  metric    = "ROC"
)

cat("Tham số tốt nhất:\n")
print(m_xgb$bestTune)
cat("AUC tốt nhất (CV):", round(max(m_xgb$results$ROC), 4), "\n")

wines <- read.csv("wines.csv", stringsAsFactors = TRUE)
wines$type <- factor(wines$type)

# Chia train/test bằng caret
set.seed(42)
w_idx   <- createDataPartition(wines$type, p = 0.8, list = FALSE)
w_train <- wines[w_idx, ]
w_test  <- wines[-w_idx, ]

# Chuẩn bị matrix cho XGBoost (bỏ quality, chỉ dùng biến hóa học)
X_w_train <- model.matrix(type ~ . - quality - 1, data = w_train)
X_w_test  <- model.matrix(type ~ . - quality - 1, data = w_test)

y_w_train <- as.integer(w_train$type == "red")
y_w_test  <- as.integer(w_test$type  == "red")

dw_train <- xgb.DMatrix(data = X_w_train, label = y_w_train)
dw_test  <- xgb.DMatrix(data = X_w_test,  label = y_w_test)

# Tuning XGBoost với caret
ctrl_w <- trainControl(
  method          = "cv",
  number          = 5,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter     = FALSE
)

xgb_grid_w <- expand.grid(
  nrounds          = c(100, 200),
  eta              = c(0.05, 0.1),
  max_depth        = c(3, 4, 6),
  gamma            = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample        = 0.8
)

set.seed(42)
m_xgb_w <- train(
  type      ~ . - quality,
  data      = w_train,
  method    = "xgbTree",
  trControl = ctrl_w,
  tuneGrid  = xgb_grid_w,
  metric    = "ROC"
)

cat("Tham số tốt nhất (Wines):\n")
print(m_xgb_w$bestTune)
cat("AUC tốt nhất (CV):", round(max(m_xgb_w$results$ROC), 4), "\n")

# Đánh giá trên test set
pred_xgb_w <- predict(m_xgb_w, newdata = w_test, type = "prob")[, "red"]
roc_xgb_w  <- roc(w_test$type, pred_xgb_w,
                  levels = c("white", "red"), quiet = TRUE)
cat("AUC (XGBoost Wines, test):", round(auc(roc_xgb_w), 4), "\n")

# Minh họa overfitting với GBM khi không có early stopping
set.seed(42)
gbm_overfit <- gbm(
  survived ~ .,
  data              = tit_train,
  distribution      = "bernoulli",
  n.trees           = 1000,
  shrinkage         = 0.1,
  interaction.depth = 5,
  bag.fraction      = 1.0,   # Không stochastic
  cv.folds          = 0,     # Không CV
  verbose           = FALSE
)

# Tính AUC train và test theo từng số cây
ntree_seq   <- seq(10, 1000, by = 10)
auc_train_v <- numeric(length(ntree_seq))
auc_test_v  <- numeric(length(ntree_seq))

for (i in seq_along(ntree_seq)) {
  p_tr <- predict(gbm_overfit, tit_train,
                  n.trees = ntree_seq[i], type = "response")
  p_te <- predict(gbm_overfit, tit_test,
                  n.trees = ntree_seq[i], type = "response")
  
  auc_train_v[i] <- auc(roc(tit_train$survived, p_tr, quiet = TRUE))
  auc_test_v[i]  <- auc(roc(tit_test$survived,  p_te, quiet = TRUE))
}

# Vẽ đồ thị
data.frame(ntree = ntree_seq,
           Train = auc_train_v,
           Test  = auc_test_v) %>%
  pivot_longer(-ntree, names_to = "Set", values_to = "AUC") %>%
  ggplot(aes(x = ntree, y = AUC, color = Set)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = ntree_seq[which.max(auc_test_v)],
             linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("Train" = "steelblue", "Test" = "tomato")) +
  labs(title    = "Overfitting trong GBM — Titanic",
       subtitle  = "Đường đứt: số cây tối ưu cho test AUC",
       x = "Số cây (n.trees)", y = "AUC") +
  theme_minimal()

best_ntree_manual <- ntree_seq[which.max(auc_test_v)]
cat("Số cây cho test AUC cao nhất:", best_ntree_manual, "\n")
cat("Train AUC tại điểm đó:       ",
    round(auc_train_v[which.max(auc_test_v)], 4), "\n")
cat("Test AUC cao nhất:            ", round(max(auc_test_v), 4), "\n")

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

# Chuẩn bị matrix cho XGBoost
X_med_train <- model.matrix(UCURNINS ~ . - 1, data = med_train)
X_med_test  <- model.matrix(UCURNINS ~ . - 1, data = med_test)
y_med_train <- as.integer(med_train$UCURNINS == "Yes")
y_med_test  <- as.integer(med_test$UCURNINS  == "Yes")

dmed_train <- xgb.DMatrix(data = X_med_train, label = y_med_train)
dmed_test  <- xgb.DMatrix(data = X_med_test,  label = y_med_test)

params_med <- list(
  objective        = "binary:logistic",
  eval_metric      = "auc",
  eta              = 0.05,
  max_depth        = 4,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  lambda           = 1,
  nthread          = parallel::detectCores() - 1
)

set.seed(42)
xgb_med <- xgb.train(
  params                = params_med,
  data                  = dmed_train,
  nrounds               = 300,
  watchlist             = list(train = dmed_train, test = dmed_test),
  early_stopping_rounds = 30,
  print_every_n         = 50,
  verbose               = 1
)

pred_xgb_med <- predict(xgb_med, newdata = dmed_test)
roc_xgb_med  <- roc(y_med_test, pred_xgb_med, quiet = TRUE)
cat("AUC — XGBoost (Medical):", round(auc(roc_xgb_med), 4), "\n")

# So sánh với các bài trước
lr_med   <- glm(UCURNINS ~ ., data = med_train, family = binomial())
pred_lr  <- predict(lr_med, newdata = med_test, type = "response")
roc_lr   <- roc(med_test$UCURNINS, pred_lr,
                levels = c("No", "Yes"), quiet = TRUE)

tree_med  <- rpart(UCURNINS ~ ., data = med_train, method = "class")
pred_dt   <- predict(tree_med, newdata = med_test, type = "prob")[, "Yes"]
roc_dt    <- roc(med_test$UCURNINS, pred_dt,
                 levels = c("No", "Yes"), quiet = TRUE)

library(ranger)
rf_med    <- ranger(UCURNINS ~ ., data = med_train, num.trees = 300,
                    probability = TRUE, verbose = FALSE)
pred_rf   <- predict(rf_med, data = med_test)$predictions[, "Yes"]
roc_rf    <- roc(med_test$UCURNINS, pred_rf,
                 levels = c("No", "Yes"), quiet = TRUE)

cat("\n===== So sánh AUC — Medical Care =====\n")
cat("Logistic Regression (Bài 17):", round(auc(roc_lr),      4), "\n")
cat("Decision Tree       (Bài 18):", round(auc(roc_dt),      4), "\n")
cat("Random Forest       (Bài 19):", round(auc(roc_rf),      4), "\n")
cat("XGBoost             (Bài 20):", round(auc(roc_xgb_med), 4), "\n")

ctrl_all <- trainControl(
  method          = "cv",
  number          = 5,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

set.seed(42)

# Decision Tree (Bài 18)
m_tree <- train(target ~ ., g_train, method = "rpart",
                trControl = ctrl_all, metric = "ROC",
                tuneGrid = expand.grid(cp = c(0.001, 0.01, 0.05)))

# Random Forest (Bài 19)
m_rf   <- train(target ~ ., g_train, method = "rf",
                trControl = ctrl_all, metric = "ROC",
                tuneGrid = expand.grid(mtry = c(3, 5, 7)),
                ntree = 200)

# GBM
m_gbm  <- train(target ~ ., g_train, method = "gbm",
                trControl = ctrl_all, metric = "ROC",
                tuneGrid  = expand.grid(
                  n.trees           = c(100, 200),
                  interaction.depth = c(2, 3),
                  shrinkage         = c(0.05, 0.1),
                  n.minobsinnode    = 10
                ),
                verbose = FALSE)

# XGBoost
m_xgb  <- train(target ~ ., g_train, method = "xgbTree",
                trControl = ctrl_all, metric = "ROC",
                tuneGrid  = expand.grid(
                  nrounds          = c(100, 200),
                  eta              = c(0.05, 0.1),
                  max_depth        = c(3, 4),
                  gamma            = 0,
                  colsample_bytree = 0.8,
                  min_child_weight = 1,
                  subsample        = 0.8
                ),
                verbose = FALSE)

# Logistic Regression (baseline từ Bài 17)
m_lr   <- train(target ~ ., g_train, method = "glm",
                family = "binomial",
                trControl = ctrl_all, metric = "ROC")

# Tổng hợp kết quả CV
results_all <- resamples(list(
  LogisticReg  = m_lr,
  DecisionTree = m_tree,
  RandomForest = m_rf,
  GBM          = m_gbm,
  XGBoost      = m_xgb
))

summary(results_all, metric = "ROC")

dotplot(results_all, metric = "ROC",
        main = "So sánh AUC (5-Fold CV) — German Credit\nBài 17–20")

bwplot(results_all, metric = "ROC",
       main = "Phân phối AUC (5-Fold CV)")

# Đánh giá trên test set
get_test_auc <- function(model, test_data, truth, pos) {
  p   <- predict(model, newdata = test_data, type = "prob")[, pos]
  lvl <- levels(test_data[[truth]])
  round(auc(roc(test_data[[truth]], p, levels = lvl, quiet = TRUE)), 4)
}

cat("\n===== AUC trên TEST SET — German Credit =====\n")
cat("Logistic Regression (Bài 17):", get_test_auc(m_lr,   g_test, "target", "Bad"), "\n")
cat("Decision Tree       (Bài 18):", get_test_auc(m_tree, g_test, "target", "Bad"), "\n")
cat("Random Forest       (Bài 19):", get_test_auc(m_rf,   g_test, "target", "Bad"), "\n")
cat("GBM                 (Bài 20):", get_test_auc(m_gbm,  g_test, "target", "Bad"), "\n")
cat("XGBoost             (Bài 20):", get_test_auc(m_xgb,  g_test, "target", "Bad"), "\n")

# Dùng learning rate = 0.05, tìm n.trees qua CV
# Cần target dạng numeric 0/1 cho gbm
g_train_num <- g_train %>%
  mutate(target_num = as.integer(target == "Bad")) %>%
  select(-target)

set.seed(42)
gbm_step1 <- gbm(target_num ~ ., data = g_train_num,
                 distribution = "bernoulli",
                 n.trees = 1000, shrinkage = 0.05,
                 interaction.depth = 4, cv.folds = 5,
                 verbose = FALSE)
best_n <- gbm.perf(gbm_step1, method = "cv")
cat("Số cây tối ưu với eta=0.05:", best_n, "\n")

ctrl_step2 <- trainControl(method = "cv", number = 5,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
grid_step2 <- expand.grid(
  n.trees           = best_n,               # Cố định từ bước 1
  interaction.depth = c(2, 3, 4, 5),
  shrinkage         = 0.05,
  n.minobsinnode    = c(5, 10, 20)
)

set.seed(42)
m_step2 <- train(target ~ ., g_train, method = "gbm",
                 trControl = ctrl_step2, metric = "ROC",
                 tuneGrid = grid_step2, verbose = FALSE)
print(m_step2$bestTune)

grid_step4 <- expand.grid(
  n.trees           = best_n * 2,
  interaction.depth = m_step2$bestTune$interaction.depth,
  shrinkage         = 0.025,
  n.minobsinnode    = m_step2$bestTune$n.minobsinnode
)
# bag.fraction chỉ có thể truyền qua argument trực tiếp trong gbm(),
# không qua tuneGrid của caret — cần huấn luyện trực tiếp:
set.seed(42)
gbm_step4 <- gbm(target_num ~ ., data = g_train_num,
                 distribution = "bernoulli",
                 n.trees           = best_n * 2,
                 shrinkage         = 0.025,
                 interaction.depth = m_step2$bestTune$interaction.depth,
                 n.minobsinnode    = m_step2$bestTune$n.minobsinnode,
                 bag.fraction      = 0.8,
                 cv.folds          = 5,
                 verbose           = FALSE)
best_n4 <- gbm.perf(gbm_step4, method = "cv")
cat("Số cây tối ưu (bước 4):", best_n4, "\n")

