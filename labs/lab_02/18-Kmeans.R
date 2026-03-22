set.seed(123)

# Tạo 3 nhóm dữ liệu rõ ràng
group1 <- data.frame(x = rnorm(50, 2, 0.5), y = rnorm(50, 2, 0.5))
group2 <- data.frame(x = rnorm(50, 8, 0.6), y = rnorm(50, 3, 0.6))
group3 <- data.frame(x = rnorm(50, 5, 0.5), y = rnorm(50, 7, 0.5))

all_data <- rbind(group1, group2, group3)
true_labels <- c(rep(1, 50), rep(2, 50), rep(3, 50))

par(mfrow = c(1, 2))

# Trước clustering
plot(all_data$x, all_data$y, pch = 19, col = "gray", cex = 1.2,
     xlab = "Feature 1", ylab = "Feature 2",
     main = "TRƯỚC clustering\n(Không có nhãn)")

# Sau clustering
plot(all_data$x, all_data$y, pch = 19, cex = 1.2,
     col = c("red", "blue", "green")[true_labels],
     xlab = "Feature 1", ylab = "Feature 2",
     main = "SAU clustering\n(Máy tự tìm 3 nhóm)")

legend("topright", legend = c("Cụm 1", "Cụm 2", "Cụm 3"),
       col = c("red", "blue", "green"), pch = 19, cex = 0.9)

par(mfrow = c(1, 1))

set.seed(42)

# Tạo dữ liệu khách hàng
customers <- data.frame(
  Age = c(rnorm(70, 25, 4), rnorm(60, 45, 5), rnorm(70, 65, 6)),
  Income = c(rnorm(70, 30, 8), rnorm(60, 70, 10), rnorm(70, 45, 8)),
  Spending = c(rnorm(70, 20, 5), rnorm(60, 80, 12), rnorm(70, 40, 8))
)

# K-Means
km <- kmeans(customers, centers = 3, nstart = 25)
customers$Cluster <- km$cluster

# Visualization
par(mfrow = c(1, 2))

plot(customers$Age, customers$Income,
     col = c("red", "blue", "green")[customers$Cluster],
     pch = 19, cex = 1.3,
     xlab = "Tuổi", ylab = "Thu nhập (triệu/tháng)",
     main = "Age vs Income")
points(km$centers[, 1:2], pch = 4, cex = 3, lwd = 3)

plot(customers$Income, customers$Spending,
     col = c("red", "blue", "green")[customers$Cluster],
     pch = 19, cex = 1.3,
     xlab = "Thu nhập", ylab = "Chi tiêu",
     main = "Income vs Spending")
points(km$centers[, 2:3], pch = 4, cex = 3, lwd = 3)

par(mfrow = c(1, 1))

# Thống kê từng cụm
cluster_summary <- data.frame(
  Cum = 1:3,
  So_luong = as.numeric(table(customers$Cluster)),
  Tuoi_TB = tapply(customers$Age, customers$Cluster, mean),
  Thu_nhap_TB = tapply(customers$Income, customers$Cluster, mean),
  Chi_tieu_TB = tapply(customers$Spending, customers$Cluster, mean)
)

# Làm tròn
cluster_summary[, 3:5] <- round(cluster_summary[, 3:5], 1)

cluster_summary

set.seed(42)

# Tạo dữ liệu mẫu
data_points <- data.frame(
  x = c(rnorm(30, 2, 0.5), rnorm(30, 8, 0.6), rnorm(30, 5, 0.5)),
  y = c(rnorm(30, 2, 0.5), rnorm(30, 3, 0.6), rnorm(30, 7, 0.5))
)

par(mfrow = c(1, 3))

# Cách 1: Random
set.seed(123)
random_idx <- sample(1:nrow(data_points), 3)
plot(data_points$x, data_points$y, pch = 19, col = "gray", cex = 1.2,
     main = "Cách 1: Random\nChọn ngẫu nhiên 3 điểm",
     xlab = "X", ylab = "Y")
points(data_points$x[random_idx], data_points$y[random_idx], 
       pch = 8, cex = 3, lwd = 3, col = "red")
text(data_points$x[random_idx], data_points$y[random_idx] + 0.5, 
     labels = c("C1", "C2", "C3"), col = "red", font = 2)

# Cách 2: K-Means++ (mô phỏng - chọn xa nhau)
c1_idx <- sample(1:nrow(data_points), 1)
dist_to_c1 <- sqrt((data_points$x - data_points$x[c1_idx])^2 + 
                     (data_points$y - data_points$y[c1_idx])^2)
c2_idx <- which.max(dist_to_c1)
dist_to_c2 <- sqrt((data_points$x - data_points$x[c2_idx])^2 + 
                     (data_points$y - data_points$y[c2_idx])^2)
min_dist <- pmin(dist_to_c1, dist_to_c2)
c3_idx <- which.max(min_dist)
kmpp_idx <- c(c1_idx, c2_idx, c3_idx)

plot(data_points$x, data_points$y, pch = 19, col = "gray", cex = 1.2,
     main = "Cách 2: K-Means++\nChọn thông minh (xa nhau)",
     xlab = "X", ylab = "Y")
points(data_points$x[kmpp_idx], data_points$y[kmpp_idx], 
       pch = 8, cex = 3, lwd = 3, col = "blue")
text(data_points$x[kmpp_idx], data_points$y[kmpp_idx] + 0.5, 
     labels = c("C1", "C2", "C3"), col = "blue", font = 2)

# Cách 3: Random Partition
set.seed(456)
random_clusters <- sample(1:3, nrow(data_points), replace = TRUE)
centers_rp <- aggregate(data_points, by = list(random_clusters), mean)[, -1]

plot(data_points$x, data_points$y, pch = 19, col = "gray", cex = 1.2,
     main = "Cách 3: Random Partition\nGán ngẫu nhiên → Tính centroid",
     xlab = "X", ylab = "Y")
points(centers_rp$x, centers_rp$y, 
       pch = 8, cex = 3, lwd = 3, col = "green")
text(centers_rp$x, centers_rp$y + 0.5, 
     labels = c("C1", "C2", "C3"), col = "green", font = 2)

par(mfrow = c(1, 1))

# Giả sử có 1 điểm và 3 centroids
point <- c(x = 5, y = 5)
centroid1 <- c(x = 2, y = 2)
centroid2 <- c(x = 8, y = 3)
centroid3 <- c(x = 5, y = 7)

# Tính khoảng cách
d1 <- sqrt(sum((point - centroid1)^2))
d2 <- sqrt(sum((point - centroid2)^2))
d3 <- sqrt(sum((point - centroid3)^2))

# Kết quả
data.frame(
  Centroid = c("C1", "C2", "C3"),
  Toa_do = c("(2, 2)", "(8, 3)", "(5, 7)"),
  Khoang_cach = round(c(d1, d2, d3), 2),
  Gan_cum = c(ifelse(d1 == min(c(d1, d2, d3)), "✓", ""),
              ifelse(d2 == min(c(d1, d2, d3)), "✓", ""),
              ifelse(d3 == min(c(d1, d2, d3)), "✓", ""))
)

# Sử dụng K-Means++ centroids
km_init <- kmeans(data_points, centers = data_points[kmpp_idx, ], 
                  algorithm = "Lloyd", iter.max = 1)

par(mfrow = c(1, 2))

# Trước assignment
plot(data_points$x, data_points$y, pch = 19, col = "gray", cex = 1.5,
     main = "TRƯỚC Assignment\nCác điểm chưa có nhãn",
     xlab = "X", ylab = "Y")
points(data_points$x[kmpp_idx], data_points$y[kmpp_idx], 
       pch = 8, cex = 3, lwd = 3, col = "black")

# Sau assignment
plot(data_points$x, data_points$y, pch = 19, cex = 1.5,
     col = c("red", "blue", "green")[km_init$cluster],
     main = "SAU Assignment\nMỗi điểm gán vào cụm gần nhất",
     xlab = "X", ylab = "Y")
points(km_init$centers[, 1], km_init$centers[, 2], 
       pch = 4, cex = 3, lwd = 3, col = "black")

par(mfrow = c(1, 1))

# Giả sử Cụm 1 có 3 điểm
cluster1_points <- data.frame(
  x = c(2.1, 2.5, 1.8),
  y = c(2.3, 1.9, 2.1)
)

cluster1_points

# Tính centroid mới
new_centroid <- colMeans(cluster1_points)

data.frame(
  Thanh_phan = c("μ_x", "μ_y"),
  Cong_thuc = c("(2.1 + 2.5 + 1.8) / 3", "(2.3 + 1.9 + 2.1) / 3"),
  Ket_qua = round(new_centroid, 2)
)

par(mfrow = c(1, 2))

# Trước update (centroids cũ)
plot(data_points$x, data_points$y, pch = 19, cex = 1.5,
     col = c("red", "blue", "green")[km_init$cluster],
     main = "TRƯỚC Update\nCentroids ở vị trí cũ",
     xlab = "X", ylab = "Y")
points(km_init$centers[, 1], km_init$centers[, 2], 
       pch = 8, cex = 3, lwd = 3, col = "black")

# Sau update (centroids mới)
km_update <- kmeans(data_points, centers = data_points[kmpp_idx, ], 
                    algorithm = "Lloyd", iter.max = 2)

plot(data_points$x, data_points$y, pch = 19, cex = 1.5,
     col = c("red", "blue", "green")[km_update$cluster],
     main = "SAU Update\nCentroids di chuyển về trung tâm",
     xlab = "X", ylab = "Y")
points(km_update$centers[, 1], km_update$centers[, 2], 
       pch = 4, cex = 3, lwd = 3, col = "black")

# Vẽ mũi tên di chuyển
arrows(km_init$centers[, 1], km_init$centers[, 2],
       km_update$centers[, 1], km_update$centers[, 2],
       col = "purple", lwd = 2, length = 0.15)

par(mfrow = c(1, 1))

set.seed(42)

# Chạy từng iteration
iterations <- list()
current_centers <- data_points[sample(1:nrow(data_points), 3), ]

for (iter in 1:6) {
  km_iter <- kmeans(data_points, centers = current_centers, 
                    algorithm = "Lloyd", iter.max = 1)
  iterations[[iter]] <- km_iter
  current_centers <- km_iter$centers
}

# Vẽ 6 iterations
par(mfrow = c(3, 2))

for (i in 1:6) {
  plot(data_points$x, data_points$y, pch = 19, cex = 1.3,
       col = c("red", "blue", "green")[iterations[[i]]$cluster],
       main = paste("Iteration", i, 
                    "\nWSS =", round(iterations[[i]]$tot.withinss, 1)),
       xlab = "X", ylab = "Y")
  points(iterations[[i]]$centers[, 1], iterations[[i]]$centers[, 2], 
         pch = 4, cex = 3, lwd = 3, col = "black")
  
  # Vẽ mũi tên di chuyển (trừ iteration 1)
  if (i > 1) {
    arrows(iterations[[i-1]]$centers[, 1], 
           iterations[[i-1]]$centers[, 2],
           iterations[[i]]$centers[, 1], 
           iterations[[i]]$centers[, 2],
           col = "purple", lwd = 1.5, length = 0.1)
  }
}

par(mfrow = c(1, 1))

data.frame(
  Iteration = 1:6,
  WSS = sapply(iterations, function(x) round(x$tot.withinss, 2))
)

km_final <- kmeans(data_points, centers = 3, nstart = 25)

plot(data_points$x, data_points$y, pch = 19, cex = 1.5,
     col = c("red", "blue", "green")[km_final$cluster],
     main = "Hàm mục tiêu: Tối thiểu hóa khoảng cách",
     xlab = "X", ylab = "Y")
points(km_final$centers[, 1], km_final$centers[, 2], 
       pch = 4, cex = 3, lwd = 3, col = "black")

# Vẽ khoảng cách từ 15 điểm ngẫu nhiên
set.seed(789)
sample_pts <- sample(1:nrow(data_points), 15)
for (i in sample_pts) {
  cluster <- km_final$cluster[i]
  segments(data_points$x[i], data_points$y[i],
           km_final$centers[cluster, 1],
           km_final$centers[cluster, 2],
           col = c("red", "blue", "green")[cluster],
           lty = 2, lwd = 1.5)
}

wss_by_cluster <- sapply(1:3, function(k) {
  cluster_points <- data_points[km_final$cluster == k, ]
  centroid <- km_final$centers[k, ]
  sum(rowSums((cluster_points - matrix(rep(centroid, each = nrow(cluster_points)), 
                                       ncol = 2))^2))
})

data.frame(
  Cum = 1:3,
  So_diem = as.numeric(table(km_final$cluster)),
  WSS = round(wss_by_cluster, 2)
)

set.seed(123)

par(mfrow = c(2, 3))

# Chạy 6 lần với seed khác nhau
wss_results <- numeric(6)

for (i in 1:6) {
  set.seed(i * 100)
  km_temp <- kmeans(data_points, centers = 3, nstart = 1)
  wss_results[i] <- km_temp$tot.withinss
  
  plot(data_points$x, data_points$y, pch = 19, cex = 1.2,
       col = c("red", "blue", "green")[km_temp$cluster],
       main = paste("Lần", i, "- WSS =", round(km_temp$tot.withinss, 0)),
       xlab = "X", ylab = "Y")
  points(km_temp$centers[, 1], km_temp$centers[, 2], 
         pch = 4, cex = 2.5, lwd = 2.5, col = "black")
}

par(mfrow = c(1, 1))

# nstart = 1 (chạy 1 lần)
km_nstart1 <- kmeans(data_points, centers = 3, nstart = 1)

# nstart = 25 (chạy 25 lần, chọn tốt nhất)
km_nstart25 <- kmeans(data_points, centers = 3, nstart = 25)

data.frame(
  Phuong_phap = c("nstart = 1", "nstart = 25"),
  WSS = c(round(km_nstart1$tot.withinss, 2), 
          round(km_nstart25$tot.withinss, 2)),
  Ghi_chu = c("Có thể bị local minimum", "Chọn kết quả tốt nhất")
)

set.seed(42)

# Dữ liệu khách hàng
customers <- data.frame(
  CustomerID = 1:200,
  Age = c(rnorm(70, 25, 4), rnorm(60, 45, 5), rnorm(70, 65, 6)),
  Income = c(rnorm(70, 30, 8), rnorm(60, 70, 10), rnorm(70, 45, 8)),
  Spending = c(rnorm(70, 20, 5), rnorm(60, 80, 12), rnorm(70, 40, 8))
)

# Xem dữ liệu mẫu
head(customers)

summary(customers[, 2:4])

# K-Means với K = 3
km_customers <- kmeans(customers[, 2:4], centers = 3, nstart = 25)

# Kích thước các cụm
table(km_customers$cluster)

# Centroids
round(km_customers$centers, 2)

customers$Cluster <- as.factor(km_customers$cluster)

par(mfrow = c(2, 2))

# Age vs Income
plot(customers$Age, customers$Income,
     col = c("red", "blue", "green")[customers$Cluster],
     pch = 19, cex = 1.2,
     xlab = "Tuổi", ylab = "Thu nhập (triệu/tháng)",
     main = "Age vs Income")
points(km_customers$centers[, 1:2], pch = 4, cex = 3, lwd = 3)
legend("topright", legend = paste("Cụm", 1:3),
       col = c("red", "blue", "green"), pch = 19, cex = 0.8)

# Age vs Spending
plot(customers$Age, customers$Spending,
     col = c("red", "blue", "green")[customers$Cluster],
     pch = 19, cex = 1.2,
     xlab = "Tuổi", ylab = "Chi tiêu (triệu/tháng)",
     main = "Age vs Spending")

# Income vs Spending
plot(customers$Income, customers$Spending,
     col = c("red", "blue", "green")[customers$Cluster],
     pch = 19, cex = 1.2,
     xlab = "Thu nhập", ylab = "Chi tiêu",
     main = "Income vs Spending")
points(km_customers$centers[, 2:3], pch = 4, cex = 3, lwd = 3)

# Cluster sizes
barplot(table(customers$Cluster), 
        names.arg = paste("Cụm", 1:3),
        col = c("red", "blue", "green"),
        main = "Kích thước các cụm",
        ylab = "Số khách hàng")

par(mfrow = c(1, 1))

cluster_summary <- data.frame(
  Cum = 1:3,
  So_luong = as.numeric(table(customers$Cluster)),
  Tuoi_TB = tapply(customers$Age, customers$Cluster, mean),
  Thu_nhap_TB = tapply(customers$Income, customers$Cluster, mean),
  Chi_tieu_TB = tapply(customers$Spending, customers$Cluster, mean)
)

cluster_summary[, 3:5] <- round(cluster_summary[, 3:5], 1)

cluster_summary

set.seed(42)

# Dữ liệu khách hàng
customers <- data.frame(
  Age = c(rnorm(70, 25, 4), rnorm(60, 45, 5), rnorm(70, 65, 6)),
  Income = c(rnorm(70, 30, 8), rnorm(60, 70, 10), rnorm(70, 45, 8))
)

par(mfrow = c(2, 3))

for (k in 2:7) {
  km <- kmeans(customers, centers = k, nstart = 25)
  plot(customers$Age, customers$Income,
       col = rainbow(k)[km$cluster],
       pch = 19, cex = 1.2,
       xlab = "Tuổi", ylab = "Thu nhập",
       main = paste("K =", k))
  points(km$centers, pch = 4, cex = 2.5, lwd = 2.5)
}

par(mfrow = c(1, 1))

par(mfrow = c(1, 3))

# K quá nhỏ
km2 <- kmeans(customers, centers = 2, nstart = 25)
plot(customers$Age, customers$Income,
     col = c("red", "blue")[km2$cluster],
     pch = 19, cex = 1.3,
     main = "K=2: UNDERFITTING\nCụm không đồng nhất",
     xlab = "Tuổi", ylab = "Thu nhập")
points(km2$centers, pch = 4, cex = 3, lwd = 3)

# K vừa phải
km3 <- kmeans(customers, centers = 3, nstart = 25)
plot(customers$Age, customers$Income,
     col = c("red", "blue", "green")[km3$cluster],
     pch = 19, cex = 1.3,
     main = "K=3: GOOD FIT\nCụm rõ ràng",
     xlab = "Tuổi", ylab = "Thu nhập")
points(km3$centers, pch = 4, cex = 3, lwd = 3)

# K quá lớn
km7 <- kmeans(customers, centers = 7, nstart = 25)
plot(customers$Age, customers$Income,
     col = rainbow(7)[km7$cluster],
     pch = 19, cex = 1.3,
     main = "K=7: OVERFITTING\nQuá phức tạp",
     xlab = "Tuổi", ylab = "Thu nhập")
points(km7$centers, pch = 4, cex = 3, lwd = 3)

par(mfrow = c(1, 1))

# Dữ liệu nhỏ
small_data <- data.frame(
  x = c(1, 2, 2, 8, 9, 9),
  y = c(1, 1, 2, 8, 8, 9)
)

small_data

# K = 2
km_small <- kmeans(small_data, centers = 2, nstart = 25)

# Clusters
km_small$cluster

# K = 2
km_small <- kmeans(small_data, centers = 2, nstart = 25)

# Clusters
km_small$cluster

# WSS
km_small$tot.withinss

# Tính WSS cho K = 1 đến 10
wss_values <- sapply(1:10, function(k) {
  kmeans(customers, centers = k, nstart = 25)$tot.withinss
})

# Vẽ biểu đồ
plot(1:10, wss_values, 
     type = "b", pch = 19, col = "blue", lwd = 2, cex = 1.5,
     xlab = "Số cụm K", ylab = "WSS",
     main = "Elbow Method")

grid()

# Đánh dấu elbow
points(3, wss_values[3], col = "red", pch = 19, cex = 3)
text(3, wss_values[3] + 2000, "Elbow\nK = 3", col = "red", font = 2)

# Giá trị WSS
text(1:10, wss_values + 1500, round(wss_values, 0), cex = 0.8, col = "darkblue")

data.frame(
  K = 1:10,
  WSS = round(wss_values, 0),
  Giam = c(NA, round(-diff(wss_values), 0)),
  Giam_pct = c(NA, round(-diff(wss_values)/wss_values[-10]*100, 1))
)

par(mfrow = c(1, 2))

# WSS
plot(1:10, wss_values, type = "b", pch = 19, col = "blue", lwd = 2,
     xlab = "K", ylab = "WSS", main = "Tại sao gọi là 'Elbow'?")
points(3, wss_values[3], col = "red", pch = 19, cex = 3)

# % giảm
pct_decrease <- c(NA, -diff(wss_values)/wss_values[-10]*100)
plot(2:10, pct_decrease[-1], type = "b", pch = 19, col = "darkgreen", lwd = 2,
     xlab = "K", ylab = "% Giảm WSS", main = "Tốc độ giảm WSS")
abline(h = 10, col = "red", lty = 2)

par(mfrow = c(1, 1))

# Dữ liệu khó xác định elbow
set.seed(123)
difficult <- data.frame(x = rnorm(200, 5, 3), y = rnorm(200, 5, 3))
wss_diff <- sapply(1:10, function(k) {
  kmeans(difficult, centers = k, nstart = 25)$tot.withinss
})

par(mfrow = c(1, 2))

plot(1:10, wss_values, type = "b", pch = 19, col = "blue", lwd = 2,
     main = "Elbow RÕ RÀNG", xlab = "K", ylab = "WSS")
points(3, wss_values[3], col = "red", pch = 19, cex = 2)

plot(1:10, wss_diff, type = "b", pch = 19, col = "blue", lwd = 2,
     main = "Elbow KHÔNG RÕ", xlab = "K", ylab = "WSS")

par(mfrow = c(1, 1))

# Dữ liệu đơn giản
example <- data.frame(
  x = c(2, 2.5, 2.2,  8, 8.5, 8.3),
  y = c(2, 2.3, 1.8,  8, 8.2, 7.9)
)
ex_clusters <- c(1, 1, 1, 2, 2, 2)

plot(example$x, example$y,
     col = c("red", "blue")[ex_clusters],
     pch = 19, cex = 3,
     main = "Tính Silhouette cho điểm i",
     xlab = "X", ylab = "Y", xlim = c(0, 10), ylim = c(0, 10))

# Điểm i
i <- 1
points(example$x[i], example$y[i], pch = 1, cex = 4, lwd = 3)
text(example$x[i], example$y[i] - 0.7, "i", font = 2, cex = 1.5)

# KC đến cùng cụm (a)
for (j in which(ex_clusters == 1 & (1:6) != i)) {
  segments(example$x[i], example$y[i], example$x[j], example$y[j],
           col = "red", lwd = 2)
}

# KC đến cụm khác (b)
for (j in which(ex_clusters == 2)) {
  segments(example$x[i], example$y[i], example$x[j], example$y[j],
           col = "blue", lwd = 2, lty = 2)
}

text(2.5, 3, "a(i)", col = "red", font = 2)
text(5, 5, "b(i)", col = "blue", font = 2)

i <- 1
same <- which(ex_clusters == 1 & (1:6) != i)
other <- which(ex_clusters == 2)

a_i <- mean(sqrt((example$x[i] - example$x[same])^2 + 
                   (example$y[i] - example$y[same])^2))
b_i <- mean(sqrt((example$x[i] - example$x[other])^2 + 
                   (example$y[i] - example$y[other])^2))
s_i <- (b_i - a_i) / max(a_i, b_i)

data.frame(
  a_i = round(a_i, 2),
  b_i = round(b_i, 2),
  s_i = round(s_i, 3)
)

library(cluster)

# Tính Silhouette cho K = 2 đến 10
sil_scores <- sapply(2:10, function(k) {
  km <- kmeans(customers, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(customers))
  mean(sil[, 3])
})

# Vẽ biểu đồ
plot(2:10, sil_scores, 
     type = "b", pch = 19, col = "blue", lwd = 2, cex = 1.5,
     xlab = "K", ylab = "Silhouette Score",
     main = "Silhouette Method", ylim = c(0, max(sil_scores) + 0.1))

grid()

# Best K
best_k <- which.max(sil_scores) + 1
points(best_k, max(sil_scores), col = "red", pch = 19, cex = 3)
text(best_k, max(sil_scores) + 0.06, paste("Best K =", best_k), 
     col = "red", font = 2)

# Ngưỡng
abline(h = 0.7, col = "darkgreen", lty = 2)
abline(h = 0.5, col = "orange", lty = 2)
abline(h = 0.3, col = "red", lty = 2)

text(9, 0.75, ">0.7: Rất tốt", col = "darkgreen", cex = 0.9)
text(9, 0.55, "0.5-0.7: Tốt", col = "orange", cex = 0.9)
text(9, 0.35, "0.3-0.5: TB", col = "red", cex = 0.9)

data.frame(
  K = 2:10,
  Silhouette = round(sil_scores, 4),
  Danh_gia = ifelse(sil_scores > 0.7, "Rất tốt",
                    ifelse(sil_scores > 0.5, "Tốt", "Trung bình"))
)

km3 <- kmeans(customers, centers = 3, nstart = 25)
sil3 <- silhouette(km3$cluster, dist(customers))

plot(sil3, col = c("red", "blue", "green"),
     main = paste("Silhouette Plot (K=3)\nAvg =", round(mean(sil3[, 3]), 3)),
     border = NA) 

par(mfrow = c(1, 2))

# Elbow
plot(1:10, wss_values, type = "b", pch = 19, col = "blue", lwd = 2,
     main = "Elbow Method", xlab = "K", ylab = "WSS")
points(3, wss_values[3], col = "red", pch = 19, cex = 2)

# Silhouette
plot(2:10, sil_scores, type = "b", pch = 19, col = "blue", lwd = 2,
     main = "Silhouette Method", xlab = "K", ylab = "Score")
points(best_k, max(sil_scores), col = "red", pch = 19, cex = 2)

par(mfrow = c(1, 1))

set.seed(42)

# Dữ liệu nhỏ (6 điểm)
small_data <- data.frame(
  x = c(1, 2, 2, 8, 9, 9),
  y = c(1, 1, 2, 8, 8, 9),
  label = paste("P", 1:6, sep="")
)

par(mfrow = c(2, 3))

# Bước 0: Mỗi điểm là 1 cụm
plot(small_data$x, small_data$y, pch = 19, cex = 2,
     col = rainbow(6), xlim = c(0, 10), ylim = c(0, 10),
     xlab = "X", ylab = "Y",
     main = "Bước 0: 6 cụm\nMỗi điểm 1 cụm")
text(small_data$x, small_data$y + 0.5, small_data$label, font = 2)

# Bước 1: Gộp P1 và P2 (gần nhất)
colors1 <- c(1, 1, 2, 3, 4, 5)
plot(small_data$x, small_data$y, pch = 19, cex = 2,
     col = rainbow(5)[colors1], xlim = c(0, 10), ylim = c(0, 10),
     xlab = "X", ylab = "Y",
     main = "Bước 1: 5 cụm\nGộp P1, P2")
text(small_data$x, small_data$y + 0.5, small_data$label, font = 2)
lines(small_data$x[1:2], small_data$y[1:2], lwd = 2, col = rainbow(5)[1])

# Bước 2: Gộp {P1,P2} và P3
colors2 <- c(1, 1, 1, 2, 3, 4)
plot(small_data$x, small_data$y, pch = 19, cex = 2,
     col = rainbow(4)[colors2], xlim = c(0, 10), ylim = c(0, 10),
     xlab = "X", ylab = "Y",
     main = "Bước 2: 4 cụm\nGộp {P1,P2}, P3")
text(small_data$x, small_data$y + 0.5, small_data$label, font = 2)
lines(small_data$x[1:3], small_data$y[1:3], lwd = 2, col = rainbow(4)[1])

# Bước 3: Gộp P4 và P5
colors3 <- c(1, 1, 1, 2, 2, 3)
plot(small_data$x, small_data$y, pch = 19, cex = 2,
     col = rainbow(3)[colors3], xlim = c(0, 10), ylim = c(0, 10),
     xlab = "X", ylab = "Y",
     main = "Bước 3: 3 cụm\nGộp P4, P5")
text(small_data$x, small_data$y + 0.5, small_data$label, font = 2)
lines(small_data$x[1:3], small_data$y[1:3], lwd = 2, col = rainbow(3)[1])
lines(small_data$x[4:5], small_data$y[4:5], lwd = 2, col = rainbow(3)[2])

# Bước 4: Gộp {P4,P5} và P6
colors4 <- c(1, 1, 1, 2, 2, 2)
plot(small_data$x, small_data$y, pch = 19, cex = 2,
     col = rainbow(2)[colors4], xlim = c(0, 10), ylim = c(0, 10),
     xlab = "X", ylab = "Y",
     main = "Bước 4: 2 cụm\nGộp {P4,P5}, P6")
text(small_data$x, small_data$y + 0.5, small_data$label, font = 2)
lines(small_data$x[1:3], small_data$y[1:3], lwd = 2, col = rainbow(2)[1])
lines(small_data$x[4:6], small_data$y[4:6], lwd = 2, col = rainbow(2)[2])

# Bước 5: Gộp 2 cụm lớn
colors5 <- rep(1, 6)
plot(small_data$x, small_data$y, pch = 19, cex = 2,
     col = "purple", xlim = c(0, 10), ylim = c(0, 10),
     xlab = "X", ylab = "Y",
     main = "Bước 5: 1 cụm\nGộp hết")
text(small_data$x, small_data$y + 0.5, small_data$label, font = 2)

par(mfrow = c(1, 1))

par(mfrow = c(2, 2))

# Tạo 2 cụm mẫu
cluster_A <- data.frame(x = c(1, 2, 1.5), y = c(1, 1.5, 2))
cluster_B <- data.frame(x = c(8, 9, 8.5), y = c(8, 8.5, 9))

# Single Linkage
plot(c(cluster_A$x, cluster_B$x), c(cluster_A$y, cluster_B$y),
     col = c(rep("red", 3), rep("blue", 3)), pch = 19, cex = 2,
     xlab = "X", ylab = "Y", main = "Single Linkage\n(Min distance)")
# Vẽ khoảng cách min
min_dist <- which.min(dist(rbind(cluster_A, cluster_B)))
segments(cluster_A$x[1], cluster_A$y[1], 
         cluster_B$x[1], cluster_B$y[1],
         col = "green", lwd = 3)
text(5, 5, "MIN", col = "green", font = 2, cex = 1.5)

# Complete Linkage
plot(c(cluster_A$x, cluster_B$x), c(cluster_A$y, cluster_B$y),
     col = c(rep("red", 3), rep("blue", 3)), pch = 19, cex = 2,
     xlab = "X", ylab = "Y", main = "Complete Linkage\n(Max distance)")
# Vẽ khoảng cách max
segments(cluster_A$x[1], cluster_A$y[1], 
         cluster_B$x[3], cluster_B$y[3],
         col = "orange", lwd = 3)
text(5, 5, "MAX", col = "orange", font = 2, cex = 1.5)

# Average Linkage
plot(c(cluster_A$x, cluster_B$x), c(cluster_A$y, cluster_B$y),
     col = c(rep("red", 3), rep("blue", 3)), pch = 19, cex = 2,
     xlab = "X", ylab = "Y", main = "Average Linkage\n(Avg all pairs)")
# Vẽ tất cả khoảng cách
for(i in 1:3) {
  for(j in 1:3) {
    segments(cluster_A$x[i], cluster_A$y[i],
             cluster_B$x[j], cluster_B$y[j],
             col = "purple", lwd = 1, lty = 2)
  }
}
text(5, 5, "AVG", col = "purple", font = 2, cex = 1.5)

# Ward
plot(c(cluster_A$x, cluster_B$x), c(cluster_A$y, cluster_B$y),
     col = c(rep("red", 3), rep("blue", 3)), pch = 19, cex = 2,
     xlab = "X", ylab = "Y", main = "Ward's Method\n(Min increase WSS)")
# Vẽ centroids
points(mean(cluster_A$x), mean(cluster_A$y), pch = 4, cex = 3, lwd = 3)
points(mean(cluster_B$x), mean(cluster_B$y), pch = 4, cex = 3, lwd = 3)
text(5, 5, "WSS", col = "darkred", font = 2, cex = 1.5)

par(mfrow = c(1, 1))

# Dùng dữ liệu customers từ trước
set.seed(42)
customers_hc <- data.frame(
  Age = c(rnorm(70, 25, 4), rnorm(60, 45, 5), rnorm(70, 65, 6)),
  Income = c(rnorm(70, 30, 8), rnorm(60, 70, 10), rnorm(70, 45, 8))
)

# Hierarchical clustering
dist_matrix <- dist(customers_hc)
hc_complete <- hclust(dist_matrix, method = "complete")
hc_average <- hclust(dist_matrix, method = "average")
hc_ward <- hclust(dist_matrix, method = "ward.D2")

par(mfrow = c(3, 1))

# Complete linkage
plot(hc_complete, main = "Dendrogram - Complete Linkage", 
     xlab = "", sub = "", labels = FALSE, hang = -1)
rect.hclust(hc_complete, k = 3, border = "red")
abline(h = 50, col = "blue", lty = 2, lwd = 2)
text(100, 53, "Cắt ở đây → K=3", col = "blue", font = 2)

# Average linkage
plot(hc_average, main = "Dendrogram - Average Linkage", 
     xlab = "", sub = "", labels = FALSE, hang = -1)
rect.hclust(hc_average, k = 3, border = "red")

# Ward
plot(hc_ward, main = "Dendrogram - Ward's Method", 
     xlab = "", sub = "", labels = FALSE, hang = -1)
rect.hclust(hc_ward, k = 3, border = "red")

par(mfrow = c(1, 1))

# Tính WSS cho mỗi K
wss_hc <- sapply(2:10, function(k) {
  clusters <- cutree(hc_ward, k = k)
  sum(sapply(1:k, function(i) {
    cluster_data <- customers_hc[clusters == i, ]
    if(nrow(cluster_data) > 1) {
      centroid <- colMeans(cluster_data)
      sum(rowSums((cluster_data - matrix(rep(centroid, each = nrow(cluster_data)), 
                                         ncol = 2))^2))
    } else {
      0
    }
  }))
})

plot(2:10, wss_hc, type = "b", pch = 19, col = "blue", lwd = 2, cex = 1.5,
     xlab = "Số cụm K", ylab = "WSS",
     main = "Elbow Method cho Hierarchical Clustering")
grid()
points(3, wss_hc[2], col = "red", pch = 19, cex = 3)
text(3, wss_hc[2] + 1000, "K = 3", col = "red", font = 2)

# Chọn K = 3
clusters_hc <- cutree(hc_ward, k = 3)

par(mfrow = c(2, 2))

# Visualization
plot(customers_hc$Age, customers_hc$Income,
     col = c("red", "blue", "green")[clusters_hc],
     pch = 19, cex = 1.2,
     xlab = "Tuổi", ylab = "Thu nhập",
     main = "Hierarchical Clustering\n(Ward's Method, K=3)")

# So sánh với K-Means
km_compare <- kmeans(customers_hc, centers = 3, nstart = 25)
plot(customers_hc$Age, customers_hc$Income,
     col = c("red", "blue", "green")[km_compare$cluster],
     pch = 19, cex = 1.2,
     xlab = "Tuổi", ylab = "Thu nhập",
     main = "K-Means\n(K=3)")
points(km_compare$centers, pch = 4, cex = 3, lwd = 3)

# Dendrogram
plot(hc_ward, main = "Dendrogram", labels = FALSE, hang = -1)
rect.hclust(hc_ward, k = 3, border = "red")

# Cluster sizes
barplot(table(clusters_hc), 
        names.arg = paste("Cụm", 1:3),
        col = c("red", "blue", "green"),
        main = "Kích thước các cụm",
        ylab = "Số điểm")

par(mfrow = c(1, 1))

data.frame(
  Cum = 1:3,
  So_luong = as.numeric(table(clusters_hc)),
  Tuoi_TB = round(tapply(customers_hc$Age, clusters_hc, mean), 1),
  Thu_nhap_TB = round(tapply(customers_hc$Income, clusters_hc, mean), 1)
)

set.seed(42)

# Tạo dữ liệu mẫu
example_points <- data.frame(
  x = c(2, 2.5, 2.2, 2.8, 2.1,  8, 8.3,  5),
  y = c(2, 2.3, 1.8, 2.5, 2.4,  8, 8.2,  5)
)

# eps = 1, minPts = 3
eps <- 1
minPts <- 3

plot(example_points$x, example_points$y, pch = 19, cex = 3,
     col = "gray", xlim = c(0, 10), ylim = c(0, 10),
     xlab = "X", ylab = "Y",
     main = paste("DBSCAN: eps =", eps, ", minPts =", minPts))

# Vẽ vòng tròn eps cho điểm 1 (core)
theta <- seq(0, 2*pi, length.out = 100)
lines(example_points$x[1] + eps*cos(theta), 
      example_points$y[1] + eps*sin(theta),
      col = "red", lwd = 2)
points(example_points$x[1], example_points$y[1], pch = 19, cex = 3, col = "red")
text(example_points$x[1], example_points$y[1] - 0.3, "CORE", col = "red", font = 2)

# Vẽ vòng tròn eps cho điểm 6 (border)
lines(example_points$x[6] + eps*cos(theta), 
      example_points$y[6] + eps*sin(theta),
      col = "blue", lwd = 2)
points(example_points$x[6], example_points$y[6], pch = 19, cex = 3, col = "blue")
text(example_points$x[6], example_points$y[6] - 0.3, "CORE", col = "blue", font = 2)

# Điểm 7 là border (gần điểm 6)
points(example_points$x[7], example_points$y[7], pch = 19, cex = 3, col = "lightblue")
text(example_points$x[7], example_points$y[7] - 0.3, "BORDER", col = "blue", font = 2)

# Điểm 8 là noise
points(example_points$x[8], example_points$y[8], pch = 4, cex = 3, lwd = 3, col = "black")
text(example_points$x[8], example_points$y[8] + 0.5, "NOISE", col = "black", font = 2)

legend("topright", 
       legend = c("Core Point", "Border Point", "Noise Point"),
       col = c("red", "lightblue", "black"),
       pch = c(19, 19, 4), cex = 1)

library(dbscan)

set.seed(123)
data_dbscan <- data.frame(
  x = c(rnorm(50, 2, 0.5), rnorm(50, 8, 0.6), rnorm(50, 5, 0.5)),
  y = c(rnorm(50, 2, 0.5), rnorm(50, 3, 0.6), rnorm(50, 7, 0.5))
)

par(mfrow = c(2, 3))

# Thử nghiệm eps
db1 <- dbscan(data_dbscan, eps = 0.3, minPts = 5)
plot(data_dbscan, col = db1$cluster + 1, pch = 19, cex = 1.2,
     main = "eps = 0.3, minPts = 5\nQuá nhỏ → Nhiều noise")
points(data_dbscan[db1$cluster == 0, ], col = "black", pch = 4, cex = 1.5, lwd = 2)

db2 <- dbscan(data_dbscan, eps = 0.8, minPts = 5)
plot(data_dbscan, col = db2$cluster + 1, pch = 19, cex = 1.2,
     main = "eps = 0.8, minPts = 5\nVừa phải")
points(data_dbscan[db2$cluster == 0, ], col = "black", pch = 4, cex = 1.5, lwd = 2)

db3 <- dbscan(data_dbscan, eps = 2.0, minPts = 5)
plot(data_dbscan, col = db3$cluster + 1, pch = 19, cex = 1.2,
     main = "eps = 2.0, minPts = 5\nQuá lớn → 1 cụm")
points(data_dbscan[db3$cluster == 0, ], col = "black", pch = 4, cex = 1.5, lwd = 2)

# Thử nghiệm minPts
db4 <- dbscan(data_dbscan, eps = 0.8, minPts = 3)
plot(data_dbscan, col = db4$cluster + 1, pch = 19, cex = 1.2,
     main = "eps = 0.8, minPts = 3\nNhiều cụm nhỏ")
points(data_dbscan[db4$cluster == 0, ], col = "black", pch = 4, cex = 1.5, lwd = 2)

db5 <- dbscan(data_dbscan, eps = 0.8, minPts = 5)
plot(data_dbscan, col = db5$cluster + 1, pch = 19, cex = 1.2,
     main = "eps = 0.8, minPts = 5\nVừa phải")
points(data_dbscan[db5$cluster == 0, ], col = "black", pch = 4, cex = 1.5, lwd = 2)

db6 <- dbscan(data_dbscan, eps = 0.8, minPts = 10)
plot(data_dbscan, col = db6$cluster + 1, pch = 19, cex = 1.2,
     main = "eps = 0.8, minPts = 10\nNhiều noise")
points(data_dbscan[db6$cluster == 0, ], col = "black", pch = 4, cex = 1.5, lwd = 2)

par(mfrow = c(1, 1))

data.frame(
  Setting = c("eps=0.3, minPts=5", "eps=0.8, minPts=5", "eps=2.0, minPts=5",
              "eps=0.8, minPts=3", "eps=0.8, minPts=5", "eps=0.8, minPts=10"),
  So_cum = c(max(db1$cluster), max(db2$cluster), max(db3$cluster),
             max(db4$cluster), max(db5$cluster), max(db6$cluster)),
  So_noise = c(sum(db1$cluster == 0), sum(db2$cluster == 0), sum(db3$cluster == 0),
               sum(db4$cluster == 0), sum(db5$cluster == 0), sum(db6$cluster == 0))
)

set.seed(42)

# Dữ liệu nhỏ
small_db <- data.frame(
  x = c(1, 1.5, 1.2, 2, 2.3,  8, 8.5, 8.2, 9,  5),
  y = c(1, 1.3, 0.8, 1.5, 1.8,  8, 8.3, 7.8, 8.5,  5)
)

eps_val <- 1
minPts_val <- 3

par(mfrow = c(3, 2))

# Bước 1: Tất cả điểm chưa gán
plot(small_db$x, small_db$y, pch = 19, cex = 2, col = "gray",
     xlim = c(0, 10), ylim = c(0, 10),
     main = "Bước 1: Chưa gán nhãn",
     xlab = "X", ylab = "Y")
text(small_db$x, small_db$y + 0.4, 1:10, font = 2)

# Bước 2: Chọn điểm 1, tìm láng giềng
plot(small_db$x, small_db$y, pch = 19, cex = 2, col = "gray",
     xlim = c(0, 10), ylim = c(0, 10),
     main = paste("Bước 2: Điểm 1 có", sum(dist(rbind(small_db[1,], small_db))[-1] < eps_val), "láng giềng"),
     xlab = "X", ylab = "Y")
theta <- seq(0, 2*pi, length.out = 100)
lines(small_db$x[1] + eps_val*cos(theta), 
      small_db$y[1] + eps_val*sin(theta), col = "red", lwd = 2)
points(small_db$x[1], small_db$y[1], pch = 19, cex = 2, col = "red")

# Bước 3: Tạo Cụm 1
cluster_labels <- rep(0, 10)
cluster_labels[c(1,2,3,4,5)] <- 1
plot(small_db$x, small_db$y, pch = 19, cex = 2, 
     col = ifelse(cluster_labels == 1, "red", "gray"),
     xlim = c(0, 10), ylim = c(0, 10),
     main = "Bước 3: Cụm 1 (điểm 1-5)",
     xlab = "X", ylab = "Y")

# Bước 4: Chọn điểm 6
plot(small_db$x, small_db$y, pch = 19, cex = 2, 
     col = ifelse(cluster_labels == 1, "red", "gray"),
     xlim = c(0, 10), ylim = c(0, 10),
     main = "Bước 4: Điểm 6 có láng giềng",
     xlab = "X", ylab = "Y")
lines(small_db$x[6] + eps_val*cos(theta), 
      small_db$y[6] + eps_val*sin(theta), col = "blue", lwd = 2)
points(small_db$x[6], small_db$y[6], pch = 19, cex = 2, col = "blue")

# Bước 5: Tạo Cụm 2
cluster_labels[c(6,7,8,9)] <- 2
plot(small_db$x, small_db$y, pch = 19, cex = 2, 
     col = c("gray", "red", "blue")[cluster_labels + 1],
     xlim = c(0, 10), ylim = c(0, 10),
     main = "Bước 5: Cụm 2 (điểm 6-9)",
     xlab = "X", ylab = "Y")

# Bước 6: Điểm 10 là Noise
plot(small_db$x, small_db$y, pch = ifelse(cluster_labels == 0, 4, 19), 
     cex = 2, lwd = 2,
     col = c("black", "red", "blue")[cluster_labels + 1],
     xlim = c(0, 10), ylim = c(0, 10),
     main = "Bước 6: Điểm 10 = NOISE",
     xlab = "X", ylab = "Y")
text(small_db$x[10], small_db$y[10] + 0.5, "NOISE", col = "black", font = 2)

par(mfrow = c(1, 1))

# Tính k-NN distance
k <- 5  # minPts
knn_dist <- kNNdist(data_dbscan, k = k)
knn_sorted <- sort(knn_dist)

plot(knn_sorted, type = "l", lwd = 2, col = "blue",
     xlab = "Điểm (sắp xếp)", 
     ylab = paste(k, "-NN Distance"),
     main = "k-NN Distance Graph\nTìm eps tối ưu")
grid()

# Đánh dấu elbow
abline(h = 0.8, col = "red", lty = 2, lwd = 2)
text(100, 0.9, "eps ≈ 0.8", col = "red", font = 2)

# Vùng tăng đột ngột
points(130, knn_sorted[130], col = "red", pch = 19, cex = 2)

# Tạo dữ liệu hình dạng phức tạp
set.seed(123)

# Hình vòng tròn
theta_vals <- seq(0, 2*pi, length.out = 100)
circle_data <- data.frame(
  x = 5 + 3*cos(theta_vals) + rnorm(100, 0, 0.2),
  y = 5 + 3*sin(theta_vals) + rnorm(100, 0, 0.2)
)

# Điểm trung tâm
center_data <- data.frame(
  x = rnorm(50, 5, 0.3),
  y = rnorm(50, 5, 0.3)
)

# Noise
noise_data <- data.frame(
  x = runif(20, 0, 10),
  y = runif(20, 0, 10)
)

complex_data <- rbind(circle_data, center_data, noise_data)

par(mfrow = c(1, 2))

# K-Means (THẤT BẠI)
km_complex <- kmeans(complex_data, centers = 2, nstart = 25)
plot(complex_data$x, complex_data$y,
     col = c("red", "blue")[km_complex$cluster],
     pch = 19, cex = 1,
     main = "K-Means: THẤT BẠI\nKhông nhận dạng được hình vòng",
     xlab = "X", ylab = "Y")
points(km_complex$centers, pch = 4, cex = 3, lwd = 3)

# DBSCAN (THÀNH CÔNG)
db_complex <- dbscan(complex_data, eps = 0.5, minPts = 5)
plot(complex_data$x, complex_data$y,
     col = db_complex$cluster + 1, 
     pch = ifelse(db_complex$cluster == 0, 4, 19),
     cex = 1, lwd = 2,
     main = "DBSCAN: THÀNH CÔNG\nNhận dạng 2 cụm + noise",
     xlab = "X", ylab = "Y")

par(mfrow = c(1, 1))

# Dữ liệu có outliers
set.seed(456)
normal_pts <- data.frame(
  x = rnorm(100, 5, 1),
  y = rnorm(100, 5, 1)
)
outliers_pts <- data.frame(
  x = c(0, 10, 0, 10),
  y = c(0, 0, 10, 10)
)
data_with_outliers <- rbind(normal_pts, outliers_pts)

par(mfrow = c(1, 2))

# K-Means bị ảnh hưởng
km_out <- kmeans(data_with_outliers, centers = 2, nstart = 25)
plot(data_with_outliers$x, data_with_outliers$y,
     col = c("red", "blue")[km_out$cluster],
     pch = 19, cex = 1.2,
     main = "K-Means\nBị ảnh hưởng bởi outliers",
     xlab = "X", ylab = "Y")
points(km_out$centers, pch = 4, cex = 3, lwd = 3)

# DBSCAN loại bỏ outliers
db_out <- dbscan(data_with_outliers, eps = 0.5, minPts = 5)
plot(data_with_outliers$x, data_with_outliers$y,
     col = ifelse(db_out$cluster == 0, "black", "red"),
     pch = ifelse(db_out$cluster == 0, 4, 19),
     cex = 1.2, lwd = 2,
     main = "DBSCAN\nTự động phát hiện outliers",
     xlab = "X", ylab = "Y")

par(mfrow = c(1, 1))

# Dùng dữ liệu khách hàng
set.seed(42)
customers_db <- data.frame(
  Age = c(rnorm(70, 25, 4), rnorm(60, 45, 5), rnorm(70, 65, 6)),
  Income = c(rnorm(70, 30, 8), rnorm(60, 70, 10), rnorm(70, 45, 8))
)

# Tìm eps tối ưu
knn_dist_cust <- kNNdist(customers_db, k = 5)
eps_optimal <- 6  # Từ k-NN graph

# DBSCAN
db_customers <- dbscan(customers_db, eps = eps_optimal, minPts = 5)

par(mfrow = c(2, 2))

# k-NN distance graph
plot(sort(knn_dist_cust), type = "l", lwd = 2, col = "blue",
     main = "k-NN Distance\nChọn eps ≈ 6",
     xlab = "Điểm", ylab = "5-NN Distance")
abline(h = eps_optimal, col = "red", lty = 2, lwd = 2)

# DBSCAN result
plot(customers_db$Age, customers_db$Income,
     col = db_customers$cluster + 1,
     pch = ifelse(db_customers$cluster == 0, 4, 19),
     cex = 1.5, lwd = 2,
     xlab = "Tuổi", ylab = "Thu nhập",
     main = paste("DBSCAN: eps =", eps_optimal, ", minPts = 5"))
legend("topright", 
       legend = c(paste("Cụm", 1:max(db_customers$cluster)), "Noise"),
       col = 2:(max(db_customers$cluster)+2), 
       pch = c(rep(19, max(db_customers$cluster)), 4))

# So sánh với K-Means
km_cust <- kmeans(customers_db, centers = 3, nstart = 25)
plot(customers_db$Age, customers_db$Income,
     col = c("red", "blue", "green")[km_cust$cluster],
     pch = 19, cex = 1.5,
     xlab = "Tuổi", ylab = "Thu nhập",
     main = "K-Means (K=3)")
points(km_cust$centers, pch = 4, cex = 3, lwd = 3)

# Cluster sizes
barplot(table(db_customers$cluster), 
        names.arg = c("Noise", paste("Cụm", 1:max(db_customers$cluster))),
        col = c("black", rainbow(max(db_customers$cluster))),
        main = "Phân bố điểm",
        ylab = "Số điểm")

par(mfrow = c(1, 1))

# Loại bỏ noise
valid_clusters <- db_customers$cluster[db_customers$cluster != 0]
valid_data <- customers_db[db_customers$cluster != 0, ]

if(max(db_customers$cluster) > 0) {
  data.frame(
    Cum = 1:max(db_customers$cluster),
    So_luong = as.numeric(table(valid_clusters)),
    Tuoi_TB = round(tapply(valid_data$Age, valid_clusters, mean), 1),
    Thu_nhap_TB = round(tapply(valid_data$Income, valid_clusters, mean), 1)
  )
}

# Số noise
sum(db_customers$cluster == 0)

par(mfrow = c(1, 3))

# K-Means
km_final <- kmeans(customers_db, centers = 3, nstart = 25)
plot(customers_db$Age, customers_db$Income,
     col = c("red", "blue", "green")[km_final$cluster],
     pch = 19, cex = 1.2,
     xlab = "Tuổi", ylab = "Thu nhập",
     main = "K-Means")
points(km_final$centers, pch = 4, cex = 2.5, lwd = 2.5)

# Hierarchical
hc_final <- hclust(dist(customers_db), method = "ward.D2")
clusters_hc_final <- cutree(hc_final, k = 3)
plot(customers_db$Age, customers_db$Income,
     col = c("red", "blue", "green")[clusters_hc_final],
     pch = 19, cex = 1.2,
     xlab = "Tuổi", ylab = "Thu nhập",
     main = "Hierarchical")

# DBSCAN
plot(customers_db$Age, customers_db$Income,
     col = db_customers$cluster + 1,
     pch = ifelse(db_customers$cluster == 0, 4, 19),
     cex = 1.2, lwd = 2,
     xlab = "Tuổi", ylab = "Thu nhập",
     main = "DBSCAN")

par(mfrow = c(1, 1))

set.seed(2026)

# Tạo dữ liệu khách hàng thực tế hơn
# Nhóm 1: Sinh viên - Trẻ, thu nhập thấp, chi tiêu thấp
group1 <- data.frame(
  Age = rnorm(80, 22, 3),
  Income = rnorm(80, 25, 5),
  Spending = rnorm(80, 30, 8)
)

# Nhóm 2: Trung niên - Trung tuổi, thu nhập cao, chi tiêu cao
group2 <- data.frame(
  Age = rnorm(100, 45, 6),
  Income = rnorm(100, 80, 12),
  Spending = rnorm(100, 75, 10)
)

# Nhóm 3: Người cao tuổi - Lớn tuổi, thu nhập trung bình, chi tiêu vừa
group3 <- data.frame(
  Age = rnorm(90, 65, 5),
  Income = rnorm(90, 50, 8),
  Spending = rnorm(90, 50, 10)
)

# Nhóm 4: Outliers - Một số khách hàng đặc biệt
outliers <- data.frame(
  Age = c(18, 75, 30, 55),
  Income = c(15, 120, 90, 35),
  Spending = c(95, 20, 10, 90)
)

# Gộp tất cả
customers_project <- rbind(group1, group2, group3, outliers)
customers_project$CustomerID <- 1:nrow(customers_project)

# Sắp xếp lại cột
customers_project <- customers_project[, c("CustomerID", "Age", "Income", "Spending")]

# Xem dữ liệu
head(customers_project, 10)

summary(customers_project[, 2:4])

par(mfrow = c(2, 2))

# Age vs Income
plot(customers_project$Age, customers_project$Income,
     pch = 19, cex = 1.2, col = "steelblue",
     xlab = "Tuổi", ylab = "Thu nhập (triệu/tháng)",
     main = "Age vs Income")

# Age vs Spending
plot(customers_project$Age, customers_project$Spending,
     pch = 19, cex = 1.2, col = "steelblue",
     xlab = "Tuổi", ylab = "Spending Score (0-100)",
     main = "Age vs Spending Score")

# Income vs Spending
plot(customers_project$Income, customers_project$Spending,
     pch = 19, cex = 1.2, col = "steelblue",
     xlab = "Thu nhập", ylab = "Spending Score",
     main = "Income vs Spending Score")

# Histogram Age
hist(customers_project$Age, breaks = 20, col = "lightblue",
     xlab = "Tuổi", main = "Phân bố Tuổi")

par(mfrow = c(1, 1))

# Chuẩn bị dữ liệu (bỏ CustomerID)
data_clustering <- customers_project[, 2:4]

# Tính WSS cho K = 1 đến 10
wss_kmeans <- sapply(1:10, function(k) {
  kmeans(data_clustering, centers = k, nstart = 25)$tot.withinss
})

par(mfrow = c(1, 2))

# Elbow plot
plot(1:10, wss_kmeans, type = "b", pch = 19, col = "blue", lwd = 2, cex = 1.5,
     xlab = "Số cụm K", ylab = "WSS",
     main = "K-Means: Elbow Method")
grid()
points(3, wss_kmeans[3], col = "red", pch = 19, cex = 3)
text(3, wss_kmeans[3] + 5000, "K = 3", col = "red", font = 2)

# % giảm
pct_decrease_km <- c(NA, -diff(wss_kmeans)/wss_kmeans[-10]*100)
plot(2:10, pct_decrease_km[-1], type = "b", pch = 19, col = "darkgreen", lwd = 2,
     xlab = "K", ylab = "% Giảm WSS", main = "Tốc độ giảm WSS")
abline(h = 10, col = "red", lty = 2)

par(mfrow = c(1, 1))

# K-Means với K = 3
km_result <- kmeans(data_clustering, centers = 3, nstart = 50)

# Gán nhãn
customers_project$KMeans_Cluster <- km_result$cluster

# Centroids
km_result$centers

par(mfrow = c(2, 2))

# Age vs Income
plot(customers_project$Age, customers_project$Income,
     col = c("red", "blue", "green")[km_result$cluster],
     pch = 19, cex = 1.3,
     xlab = "Tuổi", ylab = "Thu nhập",
     main = "K-Means: Age vs Income")
points(km_result$centers[, 1:2], pch = 4, cex = 3, lwd = 3)

# Age vs Spending
plot(customers_project$Age, customers_project$Spending,
     col = c("red", "blue", "green")[km_result$cluster],
     pch = 19, cex = 1.3,
     xlab = "Tuổi", ylab = "Spending Score",
     main = "K-Means: Age vs Spending")
points(km_result$centers[, c(1,3)], pch = 4, cex = 3, lwd = 3)

# Income vs Spending
plot(customers_project$Income, customers_project$Spending,
     col = c("red", "blue", "green")[km_result$cluster],
     pch = 19, cex = 1.3,
     xlab = "Thu nhập", ylab = "Spending Score",
     main = "K-Means: Income vs Spending")
points(km_result$centers[, 2:3], pch = 4, cex = 3, lwd = 3)

# Cluster sizes
barplot(table(km_result$cluster), 
        names.arg = paste("Cụm", 1:3),
        col = c("red", "blue", "green"),
        main = "K-Means: Kích thước cụm",
        ylab = "Số khách hàng")

par(mfrow = c(1, 1))

# Tính distance matrix
dist_matrix <- dist(data_clustering)

# Hierarchical clustering (Ward's method)
hc_result <- hclust(dist_matrix, method = "ward.D2")

# Vẽ dendrogram
plot(hc_result, labels = FALSE, hang = -1,
     main = "Hierarchical Clustering: Dendrogram (Ward's Method)",
     xlab = "", sub = "")
rect.hclust(hc_result, k = 3, border = "red")
abline(h = 60, col = "blue", lty = 2, lwd = 2)
text(150, 65, "Cắt ở đây → K = 3", col = "blue", font = 2)

# Cắt dendrogram tại K = 3
hc_clusters <- cutree(hc_result, k = 3)

# Gán nhãn
customers_project$HC_Cluster <- hc_clusters

# Kích thước cụm
table(hc_clusters)

par(mfrow = c(2, 2))

# Age vs Income
plot(customers_project$Age, customers_project$Income,
     col = c("red", "blue", "green")[hc_clusters],
     pch = 19, cex = 1.3,
     xlab = "Tuổi", ylab = "Thu nhập",
     main = "Hierarchical: Age vs Income")

# Age vs Spending
plot(customers_project$Age, customers_project$Spending,
     col = c("red", "blue", "green")[hc_clusters],
     pch = 19, cex = 1.3,
     xlab = "Tuổi", ylab = "Spending Score",
     main = "Hierarchical: Age vs Spending")

# Income vs Spending
plot(customers_project$Income, customers_project$Spending,
     col = c("red", "blue", "green")[hc_clusters],
     pch = 19, cex = 1.3,
     xlab = "Thu nhập", ylab = "Spending Score",
     main = "Hierarchical: Income vs Spending")

# Cluster sizes
barplot(table(hc_clusters), 
        names.arg = paste("Cụm", 1:3),
        col = c("red", "blue", "green"),
        main = "Hierarchical: Kích thước cụm",
        ylab = "Số khách hàng")

par(mfrow = c(1, 1))

library(dbscan)

# k-NN distance (k = 5)
knn_dist <- kNNdist(data_clustering, k = 5)
knn_sorted <- sort(knn_dist)

plot(knn_sorted, type = "l", lwd = 2, col = "blue",
     xlab = "Điểm (sắp xếp)", ylab = "5-NN Distance",
     main = "DBSCAN: k-NN Distance Graph")
grid()

# Đánh dấu elbow
eps_optimal <- 12
abline(h = eps_optimal, col = "red", lty = 2, lwd = 2)
text(150, eps_optimal + 2, paste("eps ≈", eps_optimal), col = "red", font = 2)

# DBSCAN với eps và minPts
db_result <- dbscan(data_clustering, eps = eps_optimal, minPts = 5)

# Gán nhãn
customers_project$DBSCAN_Cluster <- db_result$cluster

# Kết quả
table(db_result$cluster)

# Số cụm và noise
max(db_result$cluster)

sum(db_result$cluster == 0)

par(mfrow = c(2, 2))

# Age vs Income
plot(customers_project$Age, customers_project$Income,
     col = db_result$cluster + 1,
     pch = ifelse(db_result$cluster == 0, 4, 19),
     cex = 1.3, lwd = 2,
     xlab = "Tuổi", ylab = "Thu nhập",
     main = "DBSCAN: Age vs Income")

# Age vs Spending
plot(customers_project$Age, customers_project$Spending,
     col = db_result$cluster + 1,
     pch = ifelse(db_result$cluster == 0, 4, 19),
     cex = 1.3, lwd = 2,
     xlab = "Tuổi", ylab = "Spending Score",
     main = "DBSCAN: Age vs Spending")

# Income vs Spending
plot(customers_project$Income, customers_project$Spending,
     col = db_result$cluster + 1,
     pch = ifelse(db_result$cluster == 0, 4, 19),
     cex = 1.3, lwd = 2,
     xlab = "Thu nhập", ylab = "Spending Score",
     main = "DBSCAN: Income vs Spending")

# Cluster sizes
cluster_counts <- table(db_result$cluster)
barplot(cluster_counts, 
        names.arg = c("Noise", paste("Cụm", 1:(length(cluster_counts)-1))),
        col = c("black", rainbow(length(cluster_counts)-1)),
        main = "DBSCAN: Phân bố",
        ylab = "Số khách hàng")

par(mfrow = c(1, 1))

par(mfrow = c(3, 3))

# Row 1: K-Means
plot(customers_project$Age, customers_project$Income,
     col = c("red", "blue", "green")[km_result$cluster],
     pch = 19, cex = 1.2,
     main = "K-Means: Age vs Income",
     xlab = "Tuổi", ylab = "Thu nhập")

plot(customers_project$Age, customers_project$Spending,
     col = c("red", "blue", "green")[km_result$cluster],
     pch = 19, cex = 1.2,
     main = "K-Means: Age vs Spending",
     xlab = "Tuổi", ylab = "Spending")

plot(customers_project$Income, customers_project$Spending,
     col = c("red", "blue", "green")[km_result$cluster],
     pch = 19, cex = 1.2,
     main = "K-Means: Income vs Spending",
     xlab = "Thu nhập", ylab = "Spending")

# Row 2: Hierarchical
plot(customers_project$Age, customers_project$Income,
     col = c("red", "blue", "green")[hc_clusters],
     pch = 19, cex = 1.2,
     main = "Hierarchical: Age vs Income",
     xlab = "Tuổi", ylab = "Thu nhập")

plot(customers_project$Age, customers_project$Spending,
     col = c("red", "blue", "green")[hc_clusters],
     pch = 19, cex = 1.2,
     main = "Hierarchical: Age vs Spending",
     xlab = "Tuổi", ylab = "Spending")

plot(customers_project$Income, customers_project$Spending,
     col = c("red", "blue", "green")[hc_clusters],
     pch = 19, cex = 1.2,
     main = "Hierarchical: Income vs Spending",
     xlab = "Thu nhập", ylab = "Spending")

# Row 3: DBSCAN
plot(customers_project$Age, customers_project$Income,
     col = db_result$cluster + 1,
     pch = ifelse(db_result$cluster == 0, 4, 19),
     cex = 1.2, lwd = 2,
     main = "DBSCAN: Age vs Income",
     xlab = "Tuổi", ylab = "Thu nhập")

plot(customers_project$Age, customers_project$Spending,
     col = db_result$cluster + 1,
     pch = ifelse(db_result$cluster == 0, 4, 19),
     cex = 1.2, lwd = 2,
     main = "DBSCAN: Age vs Spending",
     xlab = "Tuổi", ylab = "Spending")

plot(customers_project$Income, customers_project$Spending,
     col = db_result$cluster + 1,
     pch = ifelse(db_result$cluster == 0, 4, 19),
     cex = 1.2, lwd = 2,
     main = "DBSCAN: Income vs Spending",
     xlab = "Thu nhập", ylab = "Spending")

par(mfrow = c(1, 1))

library(cluster)

# K-Means
sil_kmeans <- silhouette(km_result$cluster, dist_matrix)
avg_sil_kmeans <- mean(sil_kmeans[, 3])

# Hierarchical
sil_hc <- silhouette(hc_clusters, dist_matrix)
avg_sil_hc <- mean(sil_hc[, 3])

# DBSCAN (chỉ tính cho non-noise)
valid_points <- db_result$cluster != 0
if(sum(valid_points) > 0) {
  sil_dbscan <- silhouette(db_result$cluster[valid_points], 
                           dist(data_clustering[valid_points, ]))
  avg_sil_dbscan <- mean(sil_dbscan[, 3])
} else {
  avg_sil_dbscan <- NA
}

# Bảng kết quả
data.frame(
  Thuat_toan = c("K-Means", "Hierarchical", "DBSCAN"),
  Silhouette_Score = round(c(avg_sil_kmeans, avg_sil_hc, avg_sil_dbscan), 4),
  Danh_gia = c(
    ifelse(avg_sil_kmeans > 0.5, "Tốt", "Trung bình"),
    ifelse(avg_sil_hc > 0.5, "Tốt", "Trung bình"),
    ifelse(avg_sil_dbscan > 0.5, "Tốt", "Trung bình")
  )
)

par(mfrow = c(3, 1))

plot(sil_kmeans, col = c("red", "blue", "green"), border = NA,
     main = paste("K-Means: Avg Silhouette =", round(avg_sil_kmeans, 3)))

plot(sil_hc, col = c("red", "blue", "green"), border = NA,
     main = paste("Hierarchical: Avg Silhouette =", round(avg_sil_hc, 3)))

if(!is.na(avg_sil_dbscan)) {
  plot(sil_dbscan, col = rainbow(max(db_result$cluster)), border = NA,
       main = paste("DBSCAN: Avg Silhouette =", round(avg_sil_dbscan, 3)))
}

par(mfrow = c(1, 1))

# K-Means
wss_kmeans_val <- km_result$tot.withinss

# Hierarchical
wss_hc_val <- sum(sapply(1:3, function(i) {
  cluster_data <- data_clustering[hc_clusters == i, ]
  if(nrow(cluster_data) > 1) {
    centroid <- colMeans(cluster_data)
    sum(rowSums((cluster_data - matrix(rep(centroid, each = nrow(cluster_data)), 
                                       ncol = 3))^2))
  } else {
    0
  }
}))

# DBSCAN
if(max(db_result$cluster) > 0) {
  wss_dbscan_val <- sum(sapply(1:max(db_result$cluster), function(i) {
    cluster_data <- data_clustering[db_result$cluster == i, ]
    if(nrow(cluster_data) > 1) {
      centroid <- colMeans(cluster_data)
      sum(rowSums((cluster_data - matrix(rep(centroid, each = nrow(cluster_data)), 
                                         ncol = 3))^2))
    } else {
      0
    }
  }))
} else {
  wss_dbscan_val <- NA
}

data.frame(
  Thuat_toan = c("K-Means", "Hierarchical", "DBSCAN"),
  WSS = round(c(wss_kmeans_val, wss_hc_val, wss_dbscan_val), 2),
  Xep_hang = rank(c(wss_kmeans_val, wss_hc_val, 
                    ifelse(is.na(wss_dbscan_val), Inf, wss_dbscan_val)))
)

# Tính Dunn Index thủ công
# Dunn = min(khoảng cách giữa cụm) / max(khoảng cách trong cụm)

calculate_dunn <- function(data, clusters) {
  # Loại bỏ noise (nếu có)
  valid <- clusters != 0
  data_clean <- data[valid, ]
  clusters_clean <- clusters[valid]
  
  unique_clusters <- unique(clusters_clean)
  n_clusters <- length(unique_clusters)
  
  if(n_clusters < 2) return(NA)
  
  # Khoảng cách giữa cụm (inter-cluster)
  inter_dist <- Inf
  for(i in 1:(n_clusters-1)) {
    for(j in (i+1):n_clusters) {
      c1_points <- data_clean[clusters_clean == unique_clusters[i], ]
      c2_points <- data_clean[clusters_clean == unique_clusters[j], ]
      min_dist <- min(as.matrix(dist(rbind(c1_points, c2_points)))[
        1:nrow(c1_points), 
        (nrow(c1_points)+1):(nrow(c1_points)+nrow(c2_points))
      ])
      inter_dist <- min(inter_dist, min_dist)
    }
  }
  
  # Khoảng cách trong cụm (intra-cluster)
  intra_dist <- 0
  for(k in unique_clusters) {
    cluster_points <- data_clean[clusters_clean == k, ]
    if(nrow(cluster_points) > 1) {
      max_dist <- max(dist(cluster_points))
      intra_dist <- max(intra_dist, max_dist)
    }
  }
  
  return(inter_dist / intra_dist)
}

# Tính cho 3 thuật toán
dunn_kmeans <- calculate_dunn(data_clustering, km_result$cluster)
dunn_hc <- calculate_dunn(data_clustering, hc_clusters)
dunn_dbscan <- calculate_dunn(data_clustering, db_result$cluster)

data.frame(
  Thuat_toan = c("K-Means", "Hierarchical", "DBSCAN"),
  Dunn_Index = round(c(dunn_kmeans, dunn_hc, dunn_dbscan), 4),
  Xep_hang = rank(-c(dunn_kmeans, dunn_hc, 
                     ifelse(is.na(dunn_dbscan), -Inf, dunn_dbscan)))
)

data.frame(
  Cum = paste("Cụm", 1:3),
  KMeans = as.numeric(table(km_result$cluster)),
  Hierarchical = as.numeric(table(hc_clusters)),
  DBSCAN = c(as.numeric(table(db_result$cluster[db_result$cluster != 0])), 
             rep(NA, 3 - max(db_result$cluster)))
)

data.frame(
  Metric = c("Tổng số điểm", "Số điểm noise", "Tỷ lệ noise (%)"),
  DBSCAN = c(
    nrow(customers_project),
    sum(db_result$cluster == 0),
    round(sum(db_result$cluster == 0) / nrow(customers_project) * 100, 2)
  )
)

comparison_table <- data.frame(
  Tieu_chi = c("Silhouette Score", "WSS", "Dunn Index", 
               "Số cụm", "Outliers", "Tốc độ", "Dễ sử dụng"),
  KMeans = c(
    round(avg_sil_kmeans, 3),
    round(wss_kmeans_val, 0),
    round(dunn_kmeans, 3),
    3,
    "Không phát hiện",
    "Nhanh",
    "Dễ"
  ),
  Hierarchical = c(
    round(avg_sil_hc, 3),
    round(wss_hc_val, 0),
    round(dunn_hc, 3),
    3,
    "Không phát hiện",
    "Chậm",
    "Trung bình"
  ),
  DBSCAN = c(
    ifelse(is.na(avg_sil_dbscan), "N/A", round(avg_sil_dbscan, 3)),
    ifelse(is.na(wss_dbscan_val), "N/A", round(wss_dbscan_val, 0)),
    ifelse(is.na(dunn_dbscan), "N/A", round(dunn_dbscan, 3)),
    max(db_result$cluster),
    paste(sum(db_result$cluster == 0), "điểm"),
    "Trung bình",
    "Khó (chọn eps)"
  )
)

comparison_table

cluster_profiles <- data.frame(
  Cum = 1:3,
  So_luong = as.numeric(table(km_result$cluster)),
  Tuoi_TB = round(tapply(customers_project$Age, km_result$cluster, mean), 1),
  Thu_nhap_TB = round(tapply(customers_project$Income, km_result$cluster, mean), 1),
  Spending_TB = round(tapply(customers_project$Spending, km_result$cluster, mean), 1)
)

cluster_profiles

