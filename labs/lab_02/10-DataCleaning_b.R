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
# 2.2. LOAD DATA
# =========================================================

# Đọc file CSV từ đường dẫn
# sep = "\t" vì file được phân cách bằng tab
campaign <- read.csv(
  "D:/R/labs/data/dataset - customer personality analysis/marketing_campaign.csv",
  sep = "\t"
)

# =========================================================
# 2.3. XEM DỮ LIỆU BAN ĐẦU
# =========================================================

# Mở bảng dữ liệu trong RStudio
View(campaign)

# Xem 6 dòng đầu tiên để hiểu cấu trúc
head(campaign)

# Xem 6 dòng cuối để kiểm tra dữ liệu
tail(campaign)

# Kiểm tra số dòng và số cột
dim(campaign)

# Kiểm tra kiểu dữ liệu từng cột
str(campaign)

# Xem thống kê mô tả (min, max, mean,...)
summary(campaign)

# =========================================================
# 2.4. XÓA CỘT KHÔNG CẦN THIẾT
# =========================================================

# Loại bỏ:
# - ID: không dùng trong phân tích
# - Z_CostContact và Z_Revenue: không có ý nghĩa
campaign <- campaign[, -c(1,27,28)]

# Kiểm tra lại số cột sau khi xóa
dim(campaign)

# =========================================================
# 2.5. KIỂM TRA MISSING DATA
# =========================================================

# Đếm tổng số giá trị NA
sum(is.na(campaign))

# Hiển thị các dòng có NA
campaign[!complete.cases(campaign), ]

# Đếm số dòng có NA
length(campaign[!complete.cases(campaign), ])

# =========================================================
# 2.6. XỬ LÝ BIẾN INCOME
# =========================================================

# Xem thống kê biến Income
summary(campaign$Income)

# Tính median (bỏ qua NA)
median_income <- median(campaign$Income, na.rm = TRUE)

# Điền giá trị NA bằng median
campaign$Income[is.na(campaign$Income)] <- median_income

# Kiểm tra lại NA
sum(is.na(campaign$Income))

# =========================================================
# 2.7. FEATURE ENGINEERING
# =========================================================

# Lấy năm hiện tại từ hệ thống
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Tạo biến Age = năm hiện tại - năm sinh
campaign$Age <- current_year - campaign$Year_Birth

# Tạo biến tổng số con
campaign$Total_Children <- campaign$Kidhome + campaign$Teenhome

# Tạo biến tổng chi tiêu
campaign$Total_Spending <-
  campaign$MntWines +
  campaign$MntFruits +
  campaign$MntMeatProducts +
  campaign$MntFishProducts +
  campaign$MntSweetProducts +
  campaign$MntGoldProds

# Kiểm tra các biến mới
summary(campaign$Age)
summary(campaign$Total_Children)
summary(campaign$Total_Spending)

# =========================================================
# 2.8. CHUYỂN ĐỔI BIẾN CATEGORICAL
# =========================================================

# Chuyển Education thành factor có thứ tự
campaign$Education <- factor(
  campaign$Education,
  levels = c("Basic","2n Cycle","Graduation","Master","PhD"),
  ordered = TRUE
)

# Gom các giá trị hiếm về "Single"
campaign$Marital_Status[
  campaign$Marital_Status %in% c("Alone","Absurd","YOLO")
] <- "Single"

# Chuyển thành factor
campaign$Marital_Status <- factor(campaign$Marital_Status)

# Chuyển dạng ngày
campaign$Dt_Customer <- as.Date(
  campaign$Dt_Customer,
  format = "%d-%m-%Y"
)

# =========================================================
# 2.9. XỬ LÝ BIẾN NHỊ PHÂN
# =========================================================

# Danh sách các biến nhị phân
binary_vars <- c(
  "AcceptedCmp1","AcceptedCmp2","AcceptedCmp3",
  "AcceptedCmp4","AcceptedCmp5","Complain","Response"
)

# Chuyển 0/1 thành no/yes
campaign[, binary_vars] <- lapply(
  campaign[, binary_vars],
  factor,
  levels = c(0,1),
  labels = c("no","yes")
)

# =========================================================
# 2.10. CHUẨN HÓA TÊN CỘT
# =========================================================

# Đưa về chữ thường
colnames(campaign) <- tolower(colnames(campaign))

# Thay khoảng trắng bằng _
colnames(campaign) <- gsub(" ", "_", colnames(campaign))

# Kiểm tra lại
colnames(campaign)

# =========================================================
# 2.11. XÓA DỮ LIỆU TRÙNG
# =========================================================

# Đếm số dòng trùng
sum(duplicated(campaign))

# Xóa dòng trùng
campaign <- campaign[!duplicated(campaign), ]

# Kiểm tra lại
sum(duplicated(campaign))

# =========================================================
# 2.12. XỬ LÝ OUTLIERS
# =========================================================

# Loại tuổi không hợp lệ (<18 hoặc >100)
campaign <- campaign[campaign$age >= 18 & campaign$age <= 100, ]

# Loại income <= 0
campaign <- campaign[campaign$income > 0, ]

# Tính IQR để phát hiện outlier
q1 <- quantile(campaign$income, 0.25)
q3 <- quantile(campaign$income, 0.75)
iqr <- q3 - q1

# Xác định ngưỡng trên
upper <- q3 + 1.5 * iqr

# Loại outlier
campaign <- campaign[campaign$income <= upper, ]

# =========================================================
# 2.13. TEXT CLEANING
# =========================================================

# Chuẩn hóa chữ (viết hoa chữ cái đầu)
campaign$education <- tools::toTitleCase(tolower(campaign$education))
campaign$marital_status <- tools::toTitleCase(tolower(campaign$marital_status))

# Xóa khoảng trắng dư
campaign$education <- trimws(campaign$education)
campaign$marital_status <- trimws(campaign$marital_status)

# Kiểm tra giá trị unique
unique(campaign$education)
unique(campaign$marital_status)

# =========================================================
# 2.14. XỬ LÝ NGÀY
# =========================================================

# Tính số ngày khách hàng đã tham gia
campaign$customer_days <- as.numeric(Sys.Date() - campaign$dt_customer)

# Kiểm tra
summary(campaign$customer_days)

# Loại giá trị lỗi (âm)
campaign <- campaign[campaign$customer_days >= 0, ]

# =========================================================
# 2.15. CHUẨN HÓA DỮ LIỆU
# =========================================================

# Chuẩn hóa income
campaign$income_scaled <- scale(campaign$income)

# Chuẩn hóa spending
campaign$total_spending_scaled <- scale(campaign$total_spending)

# =========================================================
# 2.16. LOG TRANSFORM
# =========================================================

# Giảm skewness
campaign$log_income <- log1p(campaign$income)
campaign$log_spending <- log1p(campaign$total_spending)

# =========================================================
# 2.17. KIỂM TRA BIẾN SỐ
# =========================================================

summary(campaign$age)
hist(campaign$age)

summary(campaign$total_children)
table(campaign$total_children)

# =========================================================
# 2.18. KIỂM TRA CATEGORICAL
# =========================================================

table(campaign$education)
prop.table(table(campaign$education))

table(campaign$marital_status)
prop.table(table(campaign$marital_status))

# =========================================================
# 2.19. KIỂM TRA LOGIC
# =========================================================

# Chi tiêu = 0
campaign[campaign$total_spending == 0, ]

# Income cao nhưng chi tiêu thấp
campaign[campaign$income > 100000 & campaign$total_spending < 100, ]

# =========================================================
# 2.20. KIỂM TRA CUỐI
# =========================================================

str(campaign)
summary(campaign)
dim(campaign)

# =========================================================
# 2.21. LƯU FILE
# =========================================================

write.csv(
  campaign,
  "../data/campaign_cleaned_final.csv",
  row.names = FALSE
)