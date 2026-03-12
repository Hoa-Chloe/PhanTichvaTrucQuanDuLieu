# 2.1. Giới thiệu Dataset
# Tên dataset: Student Alcohol Consumption
# Nguồn: UCI Machine Learning Repository
# Mô tả: Dữ liệu về tình trạng tiêu thụ rượu của học sinh trung học ở Bồ Đào Nha
# 
# Các biến quan trọng:
#   
# school: Trường học (GP hoặc MS)
# sex: Giới tính
# age: Tuổi
# Medu, Fedu: Trình độ học vấn của cha mẹ (0-4)
# studytime: Thời gian học mỗi tuần
# Dalc: Mức độ uống rượu trong tuần (1-5)
# Walc: Mức độ uống rượu cuối tuần (1-5)
# Ctrl + Shift + C

# 2.2. BƯỚC 1: Load và Khám phá Dữ liệu

# Load dữ liệu
alcohol <- read.csv("D:/R/labs/data/dataset - student alcohol consumption/student-alcohol.csv")
View(alcohol)

# Xem 6 dòng đầu tiên
head(alcohol)

# Kiểm tra cấu trúc
str(alcohol)

# Tóm tắt thống kê
summary(alcohol)

# Loại bỏ cột đầu tiên (có thể là cột ID không cần thiết)
head(alcohol[,-1])  # Xem trước khi xóa
alcohol <- alcohol[,-1]  # Xóa cột 1

# 
# Lưu ý:
#   
#   [,-1] nghĩa là "lấy tất cả các dòng, loại bỏ cột 1"
# Luôn kiểm tra trước khi xóa để tránh mất dữ liệu quan trọng

# 2.3. BƯỚC 2: Xử lý Missing Data

# Phát hiện missing data
# Tìm các dòng có dữ liệu thiếu
alcohol[!complete.cases(alcohol), ]
# Đếm số dòng bị thiếu
length(alcohol[!complete.cases(alcohol), ])


# Xử lý biến số Numeric - Age

# Kiểm tra phân bố tuổi
summary(alcohol$age)

# Tính trung vị (bỏ qua NA)
median(alcohol$age, na.rm = TRUE)

# Điền missing values bằng median
alcohol$age[is.na(alcohol$age)]<- median(alcohol$age, na.rm = TRUE)

# Kiểm tra xem còn NA không
alcohol$age[is.na(alcohol$age)]

# Xử lý biến phân loại - Mjob

# Tìm các dòng có dữ liệu thiếu
alcohol[!complete.cases(alcohol), ]
# Đếm số dòng bị thiếu
length(alcohol[!complete.cases(alcohol), ])

# Điền giá trị "other" cho dòng 63 (Mjob bị thiếu)
alcohol$Mjob[63]<-"other"

alcohol[!complete.cases(alcohol), ]

# 2.4. BƯỚC 3: Chuyển đổi Categorical Data thành Factor
str(alcohol)

# Biến nhị phân đơn giản (Binảy Variables)
# School : GP / MS
summary(factor(alcohol$school))

alcohol$school<-factor(alcohol$school,
                       levels = c("GP", "MS"),
                       labels = c("Gabriel Pereira", "Mousino da Silveira")
)
# Sex - Giới tính
summary(factor(alcohol$sex))

alcohol$sex <- factor(alcohol$sex, 
                      levels = c("F", "M"), 
                      labels = c("female", "male"))

# Address - Nơi ở
summary(factor(alcohol$address))

alcohol$address <- factor(alcohol$address, 
                          levels = c("R", "U"), 
                          labels = c("rural", "urban"))

# Family size - Quy mô gia đình
summary(factor(alcohol$famsize))

alcohol$famsize <- factor(alcohol$famsize, 
                          levels = c("GT3", "LE3"), 
                          labels = c("more than 3", "less or equal to 3"))


# Parent's cohabitation status - Tình trạng chung sống của cha mẹ
summary(factor(alcohol$Pstatus))


alcohol$Pstatus <- factor(alcohol$Pstatus, 
                          levels = c("A", "T"), 
                          labels = c("living apart", "living together"))

str(alcohol)


# Ordinal Factors (Biến có thứ tự)
# Mother's education - Trình độ học vấn của mẹ
summary(factor(alcohol$Medu))
alcohol$Medu<-factor(alcohol$Medu,
                     levels = c(0,1,2,3,4),
                     labels = c("none", "primary", "primary higher", "secondary", "higher"),
                     ordered = TRUE
)

# Father's education - Trình độ học vấn của cha
summary(factor(alcohol$Fedu))

alcohol$Fedu <- factor(alcohol$Fedu, 
                       levels = c(0, 1, 2, 3, 4), 
                       labels = c("none", "primary", "primary higher", 
                                  "secondary", "higher"), 
                       ordered = TRUE)

# Reason to choose this school - Lý do chọn trường
summary(factor(alcohol$reason))
alcohol$reason<-factor(alcohol$reason)

# Kiểm tra cấu trúc sau khi chuyển đổi
str(alcohol)
summary(alcohol)

summary(factor(alcohol$guardian))
alcohol$guardian <- factor(alcohol$guardian)

# Biến thời gian và khoảng cách
# Travel time - Thời gian đi học
summary(alcohol$traveltime)

# Ý nghĩa: 1 - <15 phút, 2 - 15-30 phút, 3 - 30-60 phút, 4 - >1 giờ
alcohol$traveltime <- factor(alcohol$traveltime, 
                             levels = c(1, 2, 3, 4), 
                             labels = c("0-15 min", "15-30 min",
                                        "30-60 min", "above 60 min"),
                             ordered = TRUE)

# Study time - Thời gian học mỗi tuần
summary(alcohol$studytime)
summary(factor(alcohol$studytime))

# Ý nghĩa: 1 - <2 giờ, 2 - 2-5 giờ, 3 - 5-10 giờ, 4 - >10 giờ
alcohol$studytime <- factor(alcohol$studytime, 
                            levels = c(1, 2, 3, 4), 
                            labels = c("0-2 hours", "2-5 hours",
                                       "5-10 hours", "above 10 hours"),
                            ordered = TRUE)

# Các biến nhị phân
str(alcohol)

alcohol$schoolsup<-factor(alcohol$schoolsup, levels=c("no", "yes"))
# alcohol$famsup<-factor(alcohol$famsup, levels=c("no", "yes"))
# alcohol$schoolsup<-factor(alcohol$schoolsup, levels=c("no", "yes"))
# alcohol$schoolsup<-factor(alcohol$schoolsup, levels=c("no", "yes"))
# alcohol$schoolsup<-factor(alcohol$schoolsup, levels=c("no", "yes"))
# alcohol$schoolsup<-factor(alcohol$schoolsup, levels=c("no", "yes"))
# alcohol$schoolsup<-factor(alcohol$schoolsup, levels=c("no", "yes"))
# alcohol$schoolsup<-factor(alcohol$schoolsup, levels=c("no", "yes"))


# Nhận xét: Có 8 biến cùng dạng yes/no → lặp code 8 lần rất tốn công!

# BƯỚC 4: Tự động hóa với lapply()
# Liệt kê tất cả các biến binary (yes/no)
binaryVariables<-c("schoolsup", "famsup", "paid", "activities", 
                   "nursery", "higher", "internet", "romantic")

# Xem dữ liệu
alcohol[,binaryVariables]


# Kiểm tra từng biến
lapply(alcohol[,binaryVariables], summary)

lapply(alcohol[,binaryVariables], factor)

lapply(alcohol[,binaryVariables], function (x) {summary(factor(x))})

# Giải thích lapply:
#   
#   lapply(danh_sách, hàm): áp dụng hàm lên từng phần tử của danh sách
# Trả về một list kết quả

# Vấn đề: Biến internet có nhiều level (0, 1, NO, YES, no, yes) → lỗi nhập liệu!

# Giải pháp: Chuẩn hóa
alcohol$internet[alcohol$internet==0]<-"no"
alcohol$internet[alcohol$internet=="NO"]<-"no"
alcohol$internet[alcohol$internet==1]<-"yes"
alcohol$internet[alcohol$internet=="YES"]<-"yes"

# Kiểm tra lại
summary(factor(alcohol$internet))

# Bài học: Trong thực tế, dữ liệu thường không "sạch" → cần kiểm tra kỹ!

# Kiểm tra lại tất cả biến binary
lapply(alcohol[, binaryVariables], function(x) {summary(factor(x))})

lapply(alcohol[,binaryVariables], factor)

str(alcohol)

alcohol[,binaryVariables] <- lapply(alcohol[,binaryVariables], factor)

# BƯỚC 5: Xử lý biến mức độ (1-5 scale)
# Các biến có mức độ từ 1-5
leveledVariables <- c("famrel", "freetime", "goout", "Dalc", "Walc")

# Xem phân bố
lapply(alcohol[, leveledVariables], summary)


# Ý nghĩa:
#   
#   freetime: Thời gian rảnh sau giờ học
# goout: Tần suất đi chơi với bạn bè
# Dalc: Mức độ uống rượu trong ngày thường (Daily Alcohol Consumption)
# Walc: Mức độ uống rượu cuối tuần (Weekend Alcohol Consumption)
# Scale: 1 = rất thấp, 2 = thấp, 3 = trung bình, 4 = cao, 5 = rất cao
# 
# 

# Chuyển đổi
alcohol[, leveledVariables] <- lapply(alcohol[, leveledVariables], 
                                      factor, 
                                      levels = c(1, 2, 3, 4, 5), 
                                      labels = c("very low", "low", "average",
                                                 "high", "very high"),
                                      ordered = TRUE)


str(alcohol)

# Health status - Tình trạng sức khỏe
summary(alcohol$health)

# Ý nghĩa: 1 = rất tệ, 2 = tệ, 3 = trung bình, 4 = tốt, 5 = rất tốt
alcohol$health <- factor(alcohol$health, 
                         levels = c(1, 2, 3, 4, 5), 
                         labels = c("very bad", "bad", "average",
                                    "good", "very good"),
                         ordered = TRUE)


# BƯỚC 6: Kiểm tra kết quả cuối cùng
# Xem cấu trúc dữ liệu sau khi clean
str(alcohol)

# Tóm tắt thống kê
summary(alcohol)

# Kiểm tra missing data
sum(!complete.cases(alcohol))  # Phải = 0

# 7. Lưu dữ liệu sạch
write.csv(alcohol, "../data/dataset - student alcohol consumption/student-alcohol-cleaned.csv", row.names = FALSE)

# Quy trình làm việc được khuyến nghị