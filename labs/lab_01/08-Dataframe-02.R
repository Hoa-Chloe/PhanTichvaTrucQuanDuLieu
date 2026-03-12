# 1. Các thao tác nâng cao (Data Wrangling)
# Tạo dataframe
set1 <- data.frame(IdClient = c(1:6, 8), Product = c(rep("Thit Kho", 4), rep("Ca Kho", 5)))
set2 <- data.frame(IdClient = c(1,2,5,9), Region = c(rep("TP. HCM", 3), "Dong Thap"))
                                                      
                                                      
set1
set2

# Gộp bảng dữ liệu (merging/ joins)
# a. Inner join (chỉ lấy phần chung có cả ở 2 set)
set3 <- merge(set1, set2, bby = "IdClient")
set3

#b. Outer join (lấy tất cả, cái nào bị thiếu thì điền là NA)
set4 <- merge(set1, set2, by = "IdClient", all = TRUE)
set4

# c. Left join (giữ nguyên bảng bên trái set1, tìm bảng phải ghép vào)
set5 <- merge(set1, set2, b = "IdClient", all.x = TRUE)
set5

# d. Right join (giữ nguyên bảng bên phải set2, tìm bảng trái ghép vào)
set6 <- merge(set1, set2, by = "IdClient", all.y = TRUE)
set6

# Sắp xếp dữ liệu(sort/ order)
sort(set1$IdClient)
sort(set1$IdClient, decreasing = TRUE)
order(set1$IdClient, decreasing = TRUE)

# Sử dụng hàm order để lấy chỉ số index đã sắp xếp sau đó áp dụng cho df
set1[order(set1$IdClient, decreasing = TRUE),]

iris[order(iris$Sepal.Length), ]

# Lọc dữ liệu (filtering)
set4[set4$Region == "TP. HCM" & !is.na(set4$Region), ]
set4[set4$Product == "Ca Kho" & !is.na(set4$Product), ]
set4[(set4$Product == "Ca Kho" & !is.na(set4$Product)) & set4$IdClient >= 6, ]

iris[iris$Species = "setosa",]
iris[iris$Species = "setosa" & iris$Sepal.Length>=1.5,]

# Làm việc với factor
str(set4)
set4$Product <- as.factor(set4&Product)
set4$Region <- as.factor(set4&Region)
summary(set4)

# 2. Làm sạch dữ liệu (Data Cleaning)
set4
# Phát hiện được dữ liệu có bị thiếu (missing value)
is.na(set4)
sum(is.na(set4))
set4[is.na(set4),]

#Phát hiện các dòng có chứa NA
is.na(set4)
sum(is.na(set4))
# complete.cases => dòng đã có đầy đủ dữ liệu
# Lọc ra các dòng không có NA
set4[complete.cases(set4),]
# Lọc các dòng có ít nhất một cột NA
set4[!complete.cases(set4),]
# Xóa đi các dòng bị NA
set4_clean <- na.omit(set4)
set4_clean

# Điền thiếu
set4$Region <= as.character(set4$Region)
set4[is.na(set4$Region), "Region"] <- "VietNam"














