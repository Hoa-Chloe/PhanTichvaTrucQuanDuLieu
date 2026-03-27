install.packages(c("caret", "caretEnsemble", "pROC",
                   "tidyverse", "ranger", "xgboost", "rpart"))

library(tidyverse)
library(caret)
library(caretEnsemble)  # Stacking với caret
library(pROC)
library(ranger)
library(xgboost)
library(rpart)

titanic <- read.csv("D:/R/labs/dataset/titanic.csv", stringsAsFactors = TRUE)
titanic_clean <- titanic %>%
  select(survived, pclass, sex, age, sibsp, parch, fare) %>%
  mutate(survived = factor(survived, levels = c(0, 1),
                           labels = c("No", "Yes"))) %>%
  drop_na()

set.seed(42)
n   <- nrow(titanic_clean)
idx <- sample(1:n, n)

# Chia 3 phần: 60% train base, 20% blend, 20% test
train_end <- floor(0.6 * n)
blend_end <- floor(0.8 * n)

base_train <- titanic_clean[idx[1:train_end], ]
blend_set  <- titanic_clean[idx[(train_end + 1):blend_end], ]
test_set   <- titanic_clean[idx[(blend_end + 1):n], ]

cat("Base train:", nrow(base_train),
    "| Blend:", nrow(blend_set),
    "| Test:", nrow(test_set), "\n")

# Base learner 1: Logistic Regression
bl_lr <- glm(survived ~ ., data = base_train, family = binomial())

# Base learner 2: Decision Tree
bl_tree <- rpart(survived ~ ., data = base_train, method = "class",
                 control = rpart.control(cp = 0.01))

# Base learner 3: Random Forest
bl_rf <- ranger(survived ~ ., data = base_train,
                num.trees = 200, probability = TRUE, verbose = FALSE)

# Base learner 4: XGBoost
X_base      <- model.matrix(survived ~ . - 1, data = base_train)
y_base      <- as.integer(base_train$survived == "Yes")
dtrain_base <- xgb.DMatrix(X_base, label = y_base)

bl_xgb <- xgb.train(
  params  = list(objective = "binary:logistic", eta = 0.05,
                 max_depth = 3, eval_metric = "auc"),
  data    = dtrain_base, nrounds = 100, verbose = 0
)

p_lr_blend   <- predict(bl_lr, newdata = blend_set, type = "response")
p_tree_blend <- predict(bl_tree, newdata = blend_set,
                        type = "prob")[, "Yes"]
p_rf_blend   <- predict(bl_rf, data = blend_set)$predictions[, "Yes"]

X_blend      <- model.matrix(survived ~ . - 1, data = blend_set)
p_xgb_blend  <- predict(bl_xgb, xgb.DMatrix(X_blend))

# Tạo meta-features matrix
meta_train <- data.frame(
  p_lr   = p_lr_blend,
  p_tree = p_tree_blend,
  p_rf   = p_rf_blend,
  p_xgb  = p_xgb_blend,
  y      = blend_set$survived
)

cat("Tương quan giữa các base learner:\n")
print(round(cor(meta_train[, 1:4]), 3))

meta_model <- glm(y ~ p_lr + p_tree + p_rf + p_xgb,
                  data = meta_train, family = binomial())

cat("\n--- Hệ số Meta-Model ---\n")
print(round(coef(meta_model), 4))
cat("\n(Hệ số dương lớn → base learner đó được 'tin tưởng' nhiều hơn)\n")

p_lr_test   <- predict(bl_lr,   newdata = test_set, type = "response")
p_tree_test <- predict(bl_tree, newdata = test_set, type = "prob")[, "Yes"]
p_rf_test   <- predict(bl_rf,   data = test_set)$predictions[, "Yes"]

X_test_mat  <- model.matrix(survived ~ . - 1, data = test_set)
p_xgb_test  <- predict(bl_xgb, xgb.DMatrix(X_test_mat))

meta_test <- data.frame(
  p_lr   = p_lr_test,
  p_tree = p_tree_test,
  p_rf   = p_rf_test,
  p_xgb  = p_xgb_test
)

p_stack_test <- predict(meta_model, newdata = meta_test, type = "response")

# Hàm tiện lợi tính AUC
get_auc <- function(prob, truth) {
  round(auc(roc(truth, prob, quiet = TRUE)), 4)
}

cat("\n===== AUC trên Test Set (Titanic) =====\n")
cat("Logistic Regression:", get_auc(p_lr_test,    test_set$survived), "\n")
cat("Decision Tree:      ", get_auc(p_tree_test,  test_set$survived), "\n")
cat("Random Forest:      ", get_auc(p_rf_test,    test_set$survived), "\n")
cat("XGBoost:            ", get_auc(p_xgb_test,   test_set$survived), "\n")
cat("----------------------------------\n")
cat("Blending (Stack):   ", get_auc(p_stack_test, test_set$survived), "\n")

german <- read.csv("german1.csv")
german$target <- factor(german$target, levels = c(1, 2),
                        labels = c("Good", "Bad"))

set.seed(42)
g_idx   <- createDataPartition(german$target, p = 0.8, list = FALSE)
g_train <- german[g_idx, ]
g_test  <- german[-g_idx, ]

K       <- 5
n_train <- nrow(g_train)
set.seed(42)
fold_id <- sample(rep(1:K, length.out = n_train))

# Ma trận lưu OOF predictions
oof_preds <- matrix(NA, nrow = n_train, ncol = 3,
                    dimnames = list(NULL, c("p_lr", "p_rf", "p_xgb")))

cat("Xây OOF predictions qua", K, "folds...\n")

for (k in 1:K) {
  cat(sprintf("  Fold %d/%d\n", k, K))
  
  val_idx  <- which(fold_id == k)
  tr_fold  <- g_train[-val_idx, ]
  val_fold <- g_train[val_idx, ]
  
  # Base learner 1: Logistic Regression
  bl1 <- glm(target ~ ., data = tr_fold, family = binomial())
  oof_preds[val_idx, "p_lr"] <- predict(bl1, val_fold, type = "response")
  
  # Base learner 2: Random Forest
  bl2 <- ranger(target ~ ., data = tr_fold, num.trees = 200,
                probability = TRUE, verbose = FALSE)
  oof_preds[val_idx, "p_rf"] <- predict(bl2,
                                        data = val_fold)$predictions[, "Bad"]
  
  # Base learner 3: XGBoost
  X_tr  <- model.matrix(target ~ . - 1, data = tr_fold)
  y_tr  <- as.integer(tr_fold$target == "Bad")
  X_val <- model.matrix(target ~ . - 1, data = val_fold)
  
  bl3 <- xgb.train(
    params  = list(objective = "binary:logistic", eta = 0.05,
                   max_depth = 3, eval_metric = "auc"),
    data    = xgb.DMatrix(X_tr, label = y_tr),
    nrounds = 100, verbose = 0
  )
  oof_preds[val_idx, "p_xgb"] <- predict(bl3, xgb.DMatrix(X_val))
}

cat("OOF predictions hoàn thành!\n")

# OOF AUC của từng base learner
cat("\n--- OOF AUC (ước lượng performance trên training data) ---\n")
for (col in colnames(oof_preds)) {
  a <- auc(roc(g_train$target, oof_preds[, col],
               levels = c("Good", "Bad"), quiet = TRUE))
  cat(sprintf("  %s: %.4f\n", col, round(a, 4)))
}

meta_df_train <- data.frame(oof_preds, y = g_train$target)

# Meta 1: Logistic Regression
meta_lr <- glm(y ~ ., data = meta_df_train, family = binomial())

cat("\n--- Hệ số Meta-Model (Logistic Regression) ---\n")
print(round(coef(meta_lr), 4))

# Train lại base learners trên TOÀN BỘ g_train
cat("\nTrain lại base learners trên toàn bộ training data...\n")

final_lr  <- glm(target ~ ., data = g_train, family = binomial())
final_rf  <- ranger(target ~ ., data = g_train, num.trees = 300,
                    probability = TRUE, verbose = FALSE)

X_full    <- model.matrix(target ~ . - 1, data = g_train)
y_full    <- as.integer(g_train$target == "Bad")
final_xgb <- xgb.train(
  params  = list(objective = "binary:logistic", eta = 0.05,
                 max_depth = 3, eval_metric = "auc"),
  data    = xgb.DMatrix(X_full, label = y_full),
  nrounds = 100, verbose = 0
)

# Dự đoán trên test set
X_test_g <- model.matrix(target ~ . - 1, data = g_test)

p_lr_te  <- predict(final_lr,  newdata = g_test, type = "response")
p_rf_te  <- predict(final_rf,  data = g_test)$predictions[, "Bad"]
p_xgb_te <- predict(final_xgb, xgb.DMatrix(X_test_g))

meta_test_g <- data.frame(p_lr = p_lr_te, p_rf = p_rf_te,
                          p_xgb = p_xgb_te)

p_stack_te <- predict(meta_lr, newdata = meta_test_g, type = "response")
p_avg_te   <- rowMeans(meta_test_g)

cat("\n===== AUC trên Test Set (German Credit) =====\n")
cat("Logistic Regression:   ", get_auc(p_lr_te,    g_test$target), "\n")
cat("Random Forest:         ", get_auc(p_rf_te,    g_test$target), "\n")
cat("XGBoost:               ", get_auc(p_xgb_te,   g_test$target), "\n")
cat("Simple Average:        ", get_auc(p_avg_te,   g_test$target), "\n")
cat("OOF Stacking (LR meta):", get_auc(p_stack_te, g_test$target), "\n")

titanic_f <- titanic %>%
  select(survived, pclass, sex, age, sibsp, parch, fare) %>%
  mutate(survived = factor(survived, levels = c(0, 1),
                           labels = c("No", "Yes"))) %>%
  drop_na()

set.seed(42)
tit_idx   <- createDataPartition(titanic_f$survived, p = 0.8, list = FALSE)
tit_train <- titanic_f[tit_idx, ]
tit_test  <- titanic_f[-tit_idx, ]

# QUAN TRỌNG: tất cả base learners PHẢI dùng cùng CV folds
# để OOF predictions nhất quán
ctrl_stack <- trainControl(
  method          = "cv",
  number          = 5,
  savePredictions = "final",    # Bắt buộc để caretEnsemble lấy OOF preds
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  index           = createFolds(tit_train$survived, k = 5)  # Cùng folds!
)

# Danh sách base learners
set.seed(42)
base_models <- caretList(
  survived ~ .,
  data       = tit_train,
  trControl  = ctrl_stack,
  metric     = "ROC",
  methodList = c("glm", "rpart", "rf"),
  tuneList   = list(
    xgb = caretModelSpec(
      method   = "xgbTree",
      tuneGrid = expand.grid(
        nrounds = 100, eta = 0.05, max_depth = 3,
        gamma = 0, colsample_bytree = 0.8,
        min_child_weight = 1, subsample = 0.8
      )
    ),
    knn = caretModelSpec(
      method   = "knn",
      tuneGrid = data.frame(k = 7)
    )
  )
)

# AUC từng base learner qua CV
cat("=== AUC từng base learner (5-Fold CV) ===\n")
for (nm in names(base_models)) {
  auc_cv <- max(base_models[[nm]]$results$ROC, na.rm = TRUE)
  cat(sprintf("  %-12s: %.4f\n", nm, round(auc_cv, 4)))
}

# Kiểm tra tương quan giữa các base learners
modelCor(resamples(base_models))

# Meta-model 1: Logistic Regression (đơn giản, ít overfit)
set.seed(42)
stack_lr <- caretStack(
  base_models,
  method    = "glm",
  metric    = "ROC",
  trControl = trainControl(
    method = "cv", number = 5,
    classProbs = TRUE, summaryFunction = twoClassSummary
  )
)

cat("\n--- Hệ số Meta-Model (Logistic Regression) ---\n")
print(coef(stack_lr$ens_model$finalModel))

# Meta-model 2: Random Forest (phi tuyến)
set.seed(42)
stack_rf <- caretStack(
  base_models,
  method    = "rf",
  metric    = "ROC",
  trControl = trainControl(
    method = "cv", number = 5,
    classProbs = TRUE, summaryFunction = twoClassSummary
  ),
  tuneGrid = data.frame(mtry = c(2, 3, 4))
)

p_stack_lr <- predict(stack_lr, newdata = tit_test, type = "prob")[, "Yes"]
p_stack_rf <- predict(stack_rf, newdata = tit_test, type = "prob")[, "Yes"]

cat("\n===== AUC trên Test Set (Titanic) =====\n")
for (nm in names(base_models)) {
  p <- predict(base_models[[nm]], newdata = tit_test, type = "prob")[, "Yes"]
  cat(sprintf("  %-20s: %.4f\n", nm, get_auc(p, tit_test$survived)))
}
cat(sprintf("  %-20s: %.4f\n", "Stack (LR meta)",
            get_auc(p_stack_lr, tit_test$survived)))
cat(sprintf("  %-20s: %.4f\n", "Stack (RF meta)",
            get_auc(p_stack_rf, tit_test$survived)))

wines <- read.csv("wines.csv", stringsAsFactors = TRUE)
wines_clf <- wines %>%
  select(-quality) %>%
  mutate(type = factor(type))

set.seed(42)
w_idx   <- createDataPartition(wines_clf$type, p = 0.8, list = FALSE)
w_train <- wines_clf[w_idx, ]
w_test  <- wines_clf[-w_idx, ]

ctrl_w <- trainControl(
  method          = "cv",
  number          = 5,
  savePredictions = "final",
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  index           = createFolds(w_train$type, k = 5)
)

set.seed(42)
base_wines <- caretList(
  type ~ .,
  data       = w_train,
  trControl  = ctrl_w,
  metric     = "ROC",
  methodList = c("glm", "rf"),
  tuneList   = list(
    xgb = caretModelSpec(
      method   = "xgbTree",
      tuneGrid = expand.grid(
        nrounds = 100, eta = 0.05, max_depth = 4,
        gamma = 0, colsample_bytree = 0.8,
        min_child_weight = 1, subsample = 0.8
      )
    ),
    knn = caretModelSpec(
      method   = "knn",
      tuneGrid = data.frame(k = 5)
    )
  )
)

# AUC từng base learner
cat("=== AUC từng base learner — Wines (5-Fold CV) ===\n")
for (nm in names(base_wines)) {
  auc_cv <- max(base_wines[[nm]]$results$ROC, na.rm = TRUE)
  cat(sprintf("  %-12s: %.4f\n", nm, round(auc_cv, 4)))
}

# Meta-model
set.seed(42)
stack_wines <- caretStack(
  base_wines,
  method    = "glm",
  metric    = "ROC",
  trControl = trainControl(
    method = "cv", number = 5,
    classProbs = TRUE, summaryFunction = twoClassSummary
  )
)

# Đánh giá
p_stack_wines <- predict(stack_wines, newdata = w_test,
                         type = "prob")[, "red"]
roc_stack_w   <- roc(w_test$type, p_stack_wines,
                     levels = c("white", "red"), quiet = TRUE)

cat("\n===== AUC trên Test Set (Wines) =====\n")
for (nm in names(base_wines)) {
  p <- predict(base_wines[[nm]], newdata = w_test, type = "prob")[, "red"]
  cat(sprintf("  %-12s: %.4f\n", nm,
              round(auc(roc(w_test$type, p,
                            levels = c("white","red"),
                            quiet = TRUE)), 4)))
}
cat(sprintf("  %-12s: %.4f\n", "Stacking",
            round(auc(roc_stack_w), 4)))

wines_reg <- wines %>% select(-type)

set.seed(42)
wr_idx   <- createDataPartition(wines_reg$quality, p = 0.8, list = FALSE)
wr_train <- wines_reg[wr_idx, ]
wr_test  <- wines_reg[-wr_idx, ]

# OOF Stacking cho hồi quy
K_r     <- 5
n_wr    <- nrow(wr_train)
set.seed(42)
fold_r  <- sample(rep(1:K_r, length.out = n_wr))

oof_reg <- matrix(NA, nrow = n_wr, ncol = 3,
                  dimnames = list(NULL, c("p_lm", "p_rf", "p_xgb")))

for (k in 1:K_r) {
  val_idx  <- which(fold_r == k)
  tr_fold  <- wr_train[-val_idx, ]
  val_fold <- wr_train[val_idx, ]
  
  # LM
  bl_lm <- lm(quality ~ ., data = tr_fold)
  oof_reg[val_idx, "p_lm"] <- predict(bl_lm, val_fold)
  
  # Random Forest
  bl_rf_r <- ranger(quality ~ ., data = tr_fold,
                    num.trees = 200, verbose = FALSE)
  oof_reg[val_idx, "p_rf"] <- predict(bl_rf_r,
                                      data = val_fold)$predictions
  
  # XGBoost
  X_tr_r  <- model.matrix(quality ~ . - 1, data = tr_fold)
  y_tr_r  <- tr_fold$quality
  X_val_r <- model.matrix(quality ~ . - 1, data = val_fold)
  
  bl_xgb_r <- xgb.train(
    params  = list(objective = "reg:squarederror", eta = 0.05,
                   max_depth = 4),
    data    = xgb.DMatrix(X_tr_r, label = y_tr_r),
    nrounds = 100, verbose = 0
  )
  oof_reg[val_idx, "p_xgb"] <- predict(bl_xgb_r, xgb.DMatrix(X_val_r))
}

# Meta-model: Linear Regression
meta_reg <- lm(quality ~ p_lm + p_rf + p_xgb,
               data = data.frame(oof_reg, quality = wr_train$quality))

# Train lại trên full training data
final_lm_r  <- lm(quality ~ ., data = wr_train)
final_rf_r  <- ranger(quality ~ ., data = wr_train,
                      num.trees = 300, verbose = FALSE)
X_wr_full   <- model.matrix(quality ~ . - 1, data = wr_train)
final_xgb_r <- xgb.train(
  params  = list(objective = "reg:squarederror", eta = 0.05,
                 max_depth = 4),
  data    = xgb.DMatrix(X_wr_full, label = wr_train$quality),
  nrounds = 100, verbose = 0
)

# Dự đoán trên test set
X_wr_test <- model.matrix(quality ~ . - 1, data = wr_test)

p_lm_te  <- predict(final_lm_r,  newdata = wr_test)
p_rf_te  <- predict(final_rf_r,  data = wr_test)$predictions
p_xgb_te <- predict(final_xgb_r, xgb.DMatrix(X_wr_test))

meta_test_r  <- data.frame(p_lm = p_lm_te, p_rf = p_rf_te,
                           p_xgb = p_xgb_te)
p_stack_r    <- predict(meta_reg, newdata = meta_test_r)

# So sánh RMSE
rmse <- function(actual, pred) sqrt(mean((actual - pred)^2))
r2   <- function(actual, pred) {
  1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2)
}

cat("===== Hồi quy Wine Quality =====\n")
cat(sprintf("Linear Regression — RMSE: %.4f | R2: %.4f\n",
            rmse(wr_test$quality, p_lm_te),
            r2(wr_test$quality, p_lm_te)))
cat(sprintf("Random Forest     — RMSE: %.4f | R2: %.4f\n",
            rmse(wr_test$quality, p_rf_te),
            r2(wr_test$quality, p_rf_te)))
cat(sprintf("XGBoost           — RMSE: %.4f | R2: %.4f\n",
            rmse(wr_test$quality, p_xgb_te),
            r2(wr_test$quality, p_xgb_te)))
cat(sprintf("OOF Stacking      — RMSE: %.4f | R2: %.4f\n",
            rmse(wr_test$quality, p_stack_r),
            r2(wr_test$quality, p_stack_r)))

medical <- read.csv("medical_care.csv", stringsAsFactors = TRUE)

medical_clean <- medical %>%
  select(UCURNINS, UMARSTAT, USATMED, REGION, FHOSP, FDENT, FEMER,
         FDOCT, UIMMSTAT, UAGE, U_FTPT, U_WKSLY, UBRACE, GENDER, UEDUC3) %>%
  drop_na() %>%
  mutate(UCURNINS = factor(UCURNINS))

set.seed(42)
med_idx   <- createDataPartition(medical_clean$UCURNINS, p = 0.8, list = FALSE)
med_train <- medical_clean[med_idx, ]
med_test  <- medical_clean[-med_idx, ]

# caretEnsemble trên Medical Care
ctrl_med <- trainControl(
  method          = "cv",
  number          = 5,
  savePredictions = "final",
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  index           = createFolds(med_train$UCURNINS, k = 5)
)

set.seed(42)
base_med <- caretList(
  UCURNINS ~ .,
  data       = med_train,
  trControl  = ctrl_med,
  metric     = "ROC",
  methodList = c("glm"),
  tuneList   = list(
    rf = caretModelSpec(
      method   = "rf",
      tuneGrid = data.frame(mtry = 4),
      ntree    = 200
    ),
    xgb = caretModelSpec(
      method   = "xgbTree",
      tuneGrid = expand.grid(
        nrounds = 200, eta = 0.05, max_depth = 4,
        gamma = 0, colsample_bytree = 0.8,
        min_child_weight = 1, subsample = 0.8
      )
    )
  )
)

set.seed(42)
stack_med <- caretStack(
  base_med,
  method    = "glm",
  metric    = "ROC",
  trControl = trainControl(
    method = "cv", number = 5,
    classProbs = TRUE, summaryFunction = twoClassSummary
  )
)

p_stack_med <- predict(stack_med, newdata = med_test,
                       type = "prob")[, "Yes"]
roc_stack_med <- roc(med_test$UCURNINS, p_stack_med,
                     levels = c("No", "Yes"), quiet = TRUE)

cat("\n========================================\n")
cat("  TỔNG KẾT BÀI 17–21: Medical Care\n")
cat("========================================\n")
cat("Bài 17 — Logistic Regression: (xem lại Bài 17)\n")
cat("Bài 18 — Decision Tree:       (xem lại Bài 18)\n")
cat("Bài 19 — Random Forest:       (xem lại Bài 19)\n")
cat("Bài 20 — XGBoost:             (xem lại Bài 20)\n")
cat(sprintf("Bài 21 — Stacking:            %.4f\n",
            round(auc(roc_stack_med), 4)))

# Biểu đồ so sánh AUC — German Credit
results_summary <- data.frame(
  Model = c("Logistic Reg\n(Bài 17)", "Decision Tree\n(Bài 18)",
            "Random Forest\n(Bài 19)", "XGBoost\n(Bài 20)",
            "Simple Average", "OOF Stacking\n(Bài 21)"),
  AUC   = c(
    get_auc(p_lr_te,    g_test$target),
    get_auc(predict(rpart(target ~ ., g_train, method = "class"),
                    g_test, type = "prob")[, "Bad"], g_test$target),
    get_auc(p_rf_te,    g_test$target),
    get_auc(p_xgb_te,   g_test$target),
    get_auc(p_avg_te,   g_test$target),
    get_auc(p_stack_te, g_test$target)
  ),
  Group = c("Single Model", "Single Model", "Ensemble",
            "Ensemble",     "Ensemble",     "Ensemble")
)

ggplot(results_summary,
       aes(x = reorder(Model, AUC), y = AUC, fill = Group)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(AUC, 4)), hjust = -0.1, size = 3.5) +
  coord_flip(ylim = c(min(results_summary$AUC) - 0.02, 1.0)) +
  scale_fill_manual(values = c("Single Model" = "steelblue",
                               "Ensemble"     = "tomato")) +
  labs(title    = "So sánh AUC — Bài 17 đến Bài 21 (German Credit)",
       subtitle  = "Đỏ = Ensemble methods, Xanh = Single models",
       x = NULL, y = "AUC (Test Set)", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Kiểm tra tương quan giữa OOF predictions
# Muốn: tương quan thấp (< 0.9) → các mô hình học khác nhau
cor_matrix <- cor(oof_preds)
print(round(cor_matrix, 3))

