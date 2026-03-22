# ==========================================================
# WATER POTABILITY - LOGISTIC REGRESSION (FINAL FIXED)
# ==========================================================

# 1. Load thư viện
library(tidyverse)
library(caret)
library(pROC)
library(readr)
library(ggplot2)

# 2. Đọc dữ liệu
water_potability <- read_csv("D:/R/labs/data/water_potability.csv")

# 3. Xử lý NA
water_potability <- water_potability %>%
  mutate(across(everything(), ~replace_na(., median(., na.rm = TRUE))))

# 4. Convert target
water_potability$Potability <- as.factor(water_potability$Potability)
levels(water_potability$Potability) <- c("Not_Potable", "Potable")

# 5. Chia dữ liệu
set.seed(42)
train_index <- createDataPartition(water_potability$Potability, p=0.7, list=FALSE)
train_data <- water_potability[train_index, ]
test_data  <- water_potability[-train_index, ]

# ==========================================================
# 🔥 6. CÂN BẰNG DỮ LIỆU
# ==========================================================
train_balanced <- upSample(
  x = train_data[, -ncol(train_data)],
  y = train_data$Potability
)
colnames(train_balanced)[ncol(train_balanced)] <- "Potability"

cat("Phân phối sau khi cân bằng:\n")
print(prop.table(table(train_balanced$Potability)) * 100)

# ==========================================================
# 🔥 7. CHUẨN HÓA (KHÔNG LÀM MẤT DÒNG)
# ==========================================================
preproc <- preProcess(train_balanced[, -ncol(train_balanced)], 
                      method = c("center", "scale"))

# Áp dụng đúng cách
train_scaled <- train_balanced
train_scaled[, -ncol(train_scaled)] <- predict(preproc, train_balanced[, -ncol(train_balanced)])

test_scaled <- test_data
test_scaled[, -ncol(test_scaled)] <- predict(preproc, test_data[, -ncol(test_data)])

# ==========================================================
# 8. TRAIN MODEL
# ==========================================================
model <- glm(Potability ~ ., 
             data = train_scaled, 
             family = binomial(link="logit"))

cat("\nTóm tắt model:\n")
print(summary(model))

# ==========================================================
# 9. PREDICT
# ==========================================================
probabilities <- predict(model, newdata=test_scaled, type="response")

# ==========================================================
# 🔥 10. ROC + THRESHOLD TỐI ƯU
# ==========================================================
roc_obj <- roc(test_scaled$Potability, probabilities)
auc_value <- auc(roc_obj)

best_thresh <- coords(roc_obj, "best", ret="threshold")
best_thresh <- as.numeric(best_thresh)

cat("\nThreshold tối ưu:", best_thresh, "\n")

# ==========================================================
# 11. CLASSIFICATION
# ==========================================================
predicted_classes <- ifelse(probabilities > best_thresh, "Potable", "Not_Potable")
predicted_classes <- factor(predicted_classes, levels=c("Not_Potable", "Potable"))

# ==========================================================
# 12. CONFUSION MATRIX
# ==========================================================
conf_matrix <- confusionMatrix(predicted_classes, test_scaled$Potability, positive="Potable")

cat("\nConfusion Matrix:\n")
print(conf_matrix$table)

cat("\nĐánh giá model:\n")
cat("Accuracy:", round(conf_matrix$overall["Accuracy"], 4), "\n")
cat("Recall (Sensitivity):", round(conf_matrix$byClass["Sensitivity"], 4), "\n")
cat("Specificity:", round(conf_matrix$byClass["Specificity"], 4), "\n")
cat("F1 Score:", round(conf_matrix$byClass["F1"], 4), "\n")
cat("AUC:", round(auc_value, 4), "\n")

# ==========================================================
# 13. VẼ ROC
# ==========================================================
plot(roc_obj, main="ROC Curve - Water Potability", col="blue", lwd=2)
abline(a=0, b=1, lty=2, col="red")

# ==========================================================
# 14. VẼ CONFUSION MATRIX
# ==========================================================
cm_data <- as.data.frame(conf_matrix$table)

cm_data <- cm_data %>%
  group_by(Reference) %>%
  mutate(Prop = Freq / sum(Freq))

ggplot(data = cm_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = Freq), color = "white", size = 8, fontface = "bold") +
  geom_text(aes(label = sprintf("(%.1f%%)", Prop*100)), 
            color = "white", size = 4, vjust = 3) +
  scale_fill_gradient(low = "#d1e9ff", high = "#004085") +
  labs(
    title = "Confusion Matrix - Water Potability",
    subtitle = "Logistic Regression (Improved)",
    x = "Actual",
    y = "Predicted",
    fill = "Quantity"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 13)
  )

