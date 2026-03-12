# =========================================================
# 2.1. Giới thiệu Dataset
# =========================================================
# Tên dataset: Marketing Campaign
# Nguồn: Dữ liệu nội bộ / Kaggle
# Mô tả: Dữ liệu chi tiết về thông tin khách hàng, chi tiêu và phản hồi của họ với các chiến dịch marketing.
#
# Các biến quan trọng:
# Year_Birth: Năm sinh của khách hàng
# Education: Trình độ học vấn (Basic, Graduation, Master, PhD, 2n Cycle)
# Marital_Status: Tình trạng hôn nhân
# Income: Thu nhập hàng năm của hộ gia đình
# Kidhome / Teenhome: Số lượng trẻ nhỏ / thanh thiếu niên trong gia đình
# Dt_Customer: Ngày ghi danh trở thành khách hàng
# MntWines, MntFruits...: Số tiền chi cho các nhóm sản phẩm trong 2 năm qua
# Num...Purchases: Số lượt mua hàng qua các kênh (Web, Store, Catalog)
# AcceptedCmp1 - 5, Response: Phản hồi của khách với các chiến dịch marketing (1=Yes, 0=No)

# =========================================================
# 2.2. BƯỚC 1: Load và Khám phá Dữ liệu
# =========================================================

# Load dữ liệu (dữ liệu phân cách bằng tab)
campaign <- read.csv("D:/R/labs/data/dataset - customer personality analysis/marketing_campaign.csv", sep = "\t")

View(campaign)

# Xem 6 dòng đầu
head(campaign)

# Kiểm tra cấu trúc
str(campaign)

# Tóm tắt thống kê
summary(campaign)

# =========================================================
# Loại bỏ các cột không cần thiết
# ID, Z_CostContact, Z_Revenue thường không dùng trong phân tích
# =========================================================

head(campaign[, -c(1,27,28)])

campaign <- campaign[, -c(1,27,28)]

# =========================================================
# 2.3. BƯỚC 2: Xử lý Missing Data
# =========================================================

# Tìm các dòng bị thiếu dữ liệu
campaign[!complete.cases(campaign), ]

# Đếm số dòng bị thiếu
length(campaign[!complete.cases(campaign), ])

# ---------------------------------------------------------
# Xử lý biến Income
# ---------------------------------------------------------

summary(campaign$Income)

# Tính median (bỏ NA)
median_income <- median(campaign$Income, na.rm = TRUE)

# Điền NA bằng median
campaign$Income[is.na(campaign$Income)] <- median_income

# Kiểm tra lại
sum(is.na(campaign$Income))

# Kiểm tra lại toàn bộ dataset
sum(!complete.cases(campaign))

# =========================================================
# 2.4. BƯỚC 3: Feature Engineering (Tạo biến mới)
# =========================================================

# ---------------------------------------------------------
# Tạo biến Age
# ---------------------------------------------------------

current_year <- as.numeric(format(Sys.Date(), "%Y"))

campaign$Age <- current_year - campaign$Year_Birth

summary(campaign$Age)

# ---------------------------------------------------------
# Tổng số trẻ trong gia đình
# ---------------------------------------------------------

campaign$Total_Children <- campaign$Kidhome + campaign$Teenhome

summary(campaign$Total_Children)

# ---------------------------------------------------------
# Tổng chi tiêu
# ---------------------------------------------------------

campaign$Total_Spending <- campaign$MntWines +
  campaign$MntFruits +
  campaign$MntMeatProducts +
  campaign$MntFishProducts +
  campaign$MntSweetProducts +
  campaign$MntGoldProds

summary(campaign$Total_Spending)

# =========================================================
# 2.5. BƯỚC 4: Chuyển đổi Categorical Data thành Factor
# =========================================================

str(campaign)

# ---------------------------------------------------------
# Education - Trình độ học vấn
# ---------------------------------------------------------

summary(factor(campaign$Education))

campaign$Education <- factor(
  campaign$Education,
  levels = c("Basic", "2n Cycle", "Graduation", "Master", "PhD"),
  ordered = TRUE
)

# ---------------------------------------------------------
# Marital Status
# ---------------------------------------------------------

summary(factor(campaign$Marital_Status))

# Gom các giá trị hiếm thành "Single"
campaign$Marital_Status[
  campaign$Marital_Status %in% c("Alone", "Absurd", "YOLO")
] <- "Single"

campaign$Marital_Status <- factor(campaign$Marital_Status)

# ---------------------------------------------------------
# Date Variable
# ---------------------------------------------------------

summary(campaign$Dt_Customer)

campaign$Dt_Customer <- as.Date(
  campaign$Dt_Customer,
  format = "%d-%m-%Y"
)

# =========================================================
# 2.6. BƯỚC 5: Tự động hóa với lapply()
# =========================================================

# Các biến nhị phân
binary_vars <- c(
  "AcceptedCmp1",
  "AcceptedCmp2",
  "AcceptedCmp3",
  "AcceptedCmp4",
  "AcceptedCmp5",
  "Complain",
  "Response"
)

# Kiểm tra trước khi chuyển
lapply(campaign[, binary_vars], function(x) {
  summary(factor(x))
})

# Chuyển sang factor yes/no
campaign[, binary_vars] <- lapply(
  campaign[, binary_vars],
  factor,
  levels = c(0,1),
  labels = c("no","yes")
)

# Kiểm tra lại
str(campaign[, binary_vars])

# =========================================================
# 2.7. BƯỚC 6: Kiểm tra Outliers
# =========================================================

# Boxplot thu nhập
boxplot(campaign$Income,
        main="Income Distribution",
        horizontal=TRUE)

# Boxplot tổng chi tiêu
boxplot(campaign$Total_Spending,
        main="Total Spending Distribution",
        horizontal=TRUE)

# =========================================================
# 2.8. BƯỚC 7: Kiểm tra kết quả cuối
# =========================================================

# Kiểm tra cấu trúc
str(campaign)

# Tóm tắt thống kê
summary(campaign)

# Kiểm tra missing data
sum(!complete.cases(campaign))

# =========================================================
# 2.9. Lưu dữ liệu sạch
# =========================================================

write.csv(
  campaign,
  "../data/campaign_cleaned.csv",
  row.names = FALSE)

