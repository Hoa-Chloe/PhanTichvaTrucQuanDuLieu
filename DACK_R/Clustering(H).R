# ==========================================================
# 1. CÀI ĐẶT VÀ KHỞI TẠO THƯ VIỆN (Tổng hợp)
# ==========================================================
# install.packages(c("tidyverse", "cluster", "factoextra", "dbscan", "mclust", "clusterSim", "gridExtra"))

library(tidyverse)
library(cluster)
library(factoextra)
library(dbscan)
library(mclust)
library(clusterSim)
library(gridExtra)

# ==========================================================
# 2. LOAD VÀ LÀM SẠCH DỮ LIỆU 
# ==========================================================
data <- read.csv("D:/R/DACK_R/wine-clustering.csv") 

# Lưu lại số dòng ban đầu 
original_rows <- nrow(data)
original_na <- sum(is.na(data))

cat("--- TRẠNG THÁI DỮ LIỆU TRƯỚC KHI LÀM SẠCH ---\n")
cat("Số dòng ban đầu:", original_rows, "\n")
cat("Số giá trị NA:", original_na, "\n")
cat("Số dòng trùng lặp:", sum(duplicated(data)), "\n")

# A. Xử lý giá trị thiếu
data <- na.omit(data)

# B. Xử lý trùng lặp
data <- data %>% distinct()
rows_after_dist <- nrow(data)

# C. Xử lý Outliers (IQR Method)
clean_outliers <- function(df) {
  df_numeric <- df %>% select_if(is.numeric)
  outlier_mask <- rep(FALSE, nrow(df))
  for (col in colnames(df_numeric)) {
    Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    outlier_mask <- outlier_mask | (df[[col]] < (Q1 - 1.5 * IQR) | df[[col]] > (Q3 + 1.5 * IQR))
  }
  return(df[!outlier_mask, ])
}

data <- clean_outliers(data)
final_rows <- nrow(data)

cat("\n--- BÁO CÁO SAU KHI LÀM SẠCH ---\n")
cat("Số dòng NA bị loại:", original_na, "\n")
cat("Số dòng trùng bị loại:", original_rows - original_na - rows_after_dist, "\n")
cat("Số Outliers bị loại:", rows_after_dist - final_rows, "\n")
cat("Số dòng còn lại (Dataframe sạch):", final_rows, "\n")
cat("------------------------------------------\n")

# ==========================================================
# 3. TIỀN XỬ LÝ VÀ PCA (Kết hợp)
# ==========================================================
data_num <- data %>% select_if(is.numeric)
data_scaled <- scale(data_num)

pca <- prcomp(data_scaled)
pca_data <- as.data.frame(pca$x[,1:2])
colnames(pca_data) <- c("PC1", "PC2")

# ==========================================================
# 4. TÌM SỐ CỤM TỐI ƯU
# ==========================================================
p_elbow <- fviz_nbclust(data_scaled, kmeans, method = "wss") + ggtitle("Elbow Method")
p_sil <- fviz_nbclust(data_scaled, kmeans, method = "silhouette") + ggtitle("Silhouette Method")
grid.arrange(p_elbow, p_sil, ncol = 2)

# ==========================================================
# 5. THỰC HIỆN PHÂN CỤM
# ==========================================================
set.seed(123)

# --- A. K-MEANS ---
kmeans_model <- kmeans(data_scaled, centers = 3)
pca_data$kmeans <- as.factor(kmeans_model$cluster)

p1 <- fviz_cluster(kmeans_model, data = data_scaled, geom = "point", 
                   ellipse.type = "norm", label = FALSE, 
                   ggtheme = theme_minimal(), main = "K-Means Clustering")

sil_km <- mean(silhouette(kmeans_model$cluster, dist(data_scaled))[,3])
db_km <- index.DB(data_scaled, kmeans_model$cluster)$DB
print(p1)
# --- B. HIERARCHICAL CLUSTERING ---
dist_matrix <- dist(data_scaled)
hc_model <- hclust(dist_matrix, method = "ward.D")
hc_clusters <- cutree(hc_model, k = 3)
pca_data$hc <- as.factor(hc_clusters)

p2 <- fviz_cluster(list(data = data_scaled, cluster = hc_clusters), 
                   geom = "point", ellipse.type = "norm", label = FALSE, 
                   ggtheme = theme_minimal(), main = "Hierarchical Clustering")

sil_hc <- mean(silhouette(hc_clusters, dist_matrix)[,3])
db_hc <- index.DB(data_scaled, hc_clusters)$DB
print(p2)
# --- C. DBSCAN ---
dbscan_model <- dbscan(data_scaled, eps = 2, minPts = 4)
pca_data$dbscan <- as.factor(dbscan_model$cluster)

p3 <- fviz_cluster(list(data = data_scaled, cluster = dbscan_model$cluster), 
                   geom = "point", label = FALSE, 
                   ggtheme = theme_minimal(), main = "DBSCAN (0 is Noise)")

valid_idx <- dbscan_model$cluster != 0
if(sum(valid_idx) > 1){
  sil_db_val <- mean(silhouette(dbscan_model$cluster[valid_idx], dist(data_scaled[valid_idx,]))[,3])
} else {
  sil_db_val <- NA
}
print(p3)
# --- D. GMM (Gaussian Mixture Model) ---
gmm_model <- Mclust(data_scaled)
pca_data$gmm <- as.factor(gmm_model$classification)

p4 <- fviz_cluster(list(data = data_scaled, cluster = gmm_model$classification), 
                   geom = "point", ellipse.type = "norm", label = FALSE, 
                   ggtheme = theme_minimal(), main = "GMM Clustering")

sil_gmm <- mean(silhouette(gmm_model$classification, dist(data_scaled))[,3])
db_gmm <- index.DB(data_scaled, gmm_model$classification)$DB
print(p4)
# ==========================================================
# 6. TỔNG HỢP VÀ SO SÁNH (Kết hợp)
# ==========================================================
# Hiển thị bảng so sánh chỉ số
results <- data.frame(
  Method = c("KMeans", "Hierarchical", "DBSCAN", "GMM"),
  Silhouette = c(sil_km, sil_hc, sil_db_val, sil_gmm),
  DB_Index = c(db_km, db_hc, NA, db_gmm)
)

print("--- BẢNG SO SÁNH CHỈ SỐ ĐÁNH GIÁ ---")
print(results)

# Hiển thị lưới biểu đồ so sánh các thuật toán
grid.arrange(p1, p2, p3, p4, ncol = 2)

