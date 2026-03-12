### Phần 1 ############################
# # Nhiệm vụ
# # 1. Tạo và thêm tên duy nhất vào năm vector có độ dài 8. 
# Làm cho các kiểu dữ liệu của chúng đa dạng. 
# Tạo một dataframe có tên "mySet1" từ các vector đã tạo.
# # a) Hiển thị dòng thứ 5 của dataframe đã tạo.
# # b) Thay đổi tên của cột thứ hai của dataframe 
# mySet1 thành "column02"
# # c) Hiển thị 7 dòng đầu tiên của dataframe mySet1. 
# Sử dụng hai phương pháp khác nhau - với chỉ số và với một hàm.

vector1 <- 1:8 # vector số nguyên
vector2 <- seq(0, 1, length.out=8) # vector số thực
vector3 <- c(T, F,T,T, F,T,F,F)
vector4 <- c("A", "B", "C", "D", "E", "F", "G", "H")
vector5 <- as.factor(c("Thấp", "Cao", "Trung Bình", "Cao", "Cao", "Thấp", "Thấp", "Trung Bình"))

# Tạo dataframe
# a
mySet1 <- data.frame(vector1, vector2, vector3, vector4, vector5)
mySet1[5,]
# b
colnames(mySet1)[2]<-"column02"
# c
mySet1[1:7,]
head(mySet1, 7)

# 2. Sử dụng bộ dữ liệu iris. 
# Sử dụng chỉ số để hiển thị giá trị của mỗi 
# dòng thứ 3 giữa quan sát thứ 40 và 120.
# Cố gắng sử dụng một dòng lệnh duy nhất 
# (rút gọn mã để nó vừa trong một dòng duy nhất, 
# không có bất kỳ bước trung gian nào).
iris
seq(40,120,3)
iris[seq(40,120,3),]

# 3. Sử dụng bộ dữ liệu có sẵn "women".
# a) Thay đổi kiểu của cột đầu tiên thành kiểu ký tự.
# b) Thêm hai dòng mới vào bộ dữ liệu với các số tự tạo. Đảm bảo rằng bạn không làm mất các kiểu của biến trong dataframe chính trong quá trình này.
# c) Thêm biến mới vào bộ dữ liệu và đặt tên là "shoe_size". Sử dụng hàm runif để tạo các giá trị cho biến này. Kích thước giày phải là số nguyên giữa 35 và 42.
women
women$height <- as.character(women$height)
str(women)

new_rows <- data.frame(
  height = as.character(c(70,71)),
  weight = c(165,168)
)

women <- rbind(women, new_rows)

runif(nrow(women), min=35, max=43)
floor(runif(nrow(women), min=35, max=43))
women$shoe_size<-floor(runif(nrow(women), min=35, max=43))


### Phần 2 ############################
### Sử dụng bộ dữ liệu có sẵn CO2 cho các nhiệm vụ sau:
# 1. In giá trị CO2 uptake từ lớn nhất đến nhỏ nhất.
CO2
#order(CO2$uptake, decreasing = TRUE)
#CO2[order(CO2$uptake, decreasing = TRUE),]
sort(CO2$uptake, decreasing = TRUE)

# 2. Hiển thị các dòng của bộ dữ liệu CO2 có Type là Quebec và Treatment là chilled.
CO2[CO2$Type=="Quebec" & CO2$Treatment=="chilled",]

# 3. Hiển thị các dòng của bộ dữ liệu CO2 có uptake lớn hơn 40 và 
# được sắp xếp theo giá trị conc từ nhỏ nhất đến lớn nhất.
# Điểm thưởng nếu giữ toàn bộ mã trong một dòng duy nhất. Nếu cần tạo
# một đối tượng trung gian, hãy đặt tên là 'temp'.
temp<-CO2[order(CO2$conc),]
temp[temp$uptake>40,]

CO2[order(CO2$conc),][CO2[order(CO2$conc),]$uptake>40,]

# 4. Làm thế nào để sắp xếp ngẫu nhiên bộ dữ liệu CO2? GỢI Ý: Bạn có thể cần tạo
# một vector với các chỉ số ngẫu nhiên từ kết quả của order(runif(...)).
# Tham khảo phần "Picking random rows from data".
# Điểm thưởng nếu viết mã trong một dòng duy nhất không có đối tượng trung gian.
CO2[order(runif(nrow(CO2))),]

### Chạy mã này trước khi thực hiện các nhiệm vụ tiếp theo
set.seed(123)
missCO2 <- CO2
missCO2[c(as.integer(runif(10)*nrow(missCO2)),14:16),"uptake"] <- NA
missCO2[c(as.integer(runif(10)*nrow(missCO2)),14:16),"conc"] <- NA
missCO2$weight <- paste0(as.integer(runif(nrow(missCO2))*30),"kg")

# 5. Hiển thị các dòng của bộ dữ liệu missCO2 có ít nhất một giá trị bị thiếu.
missCO2[!complete.cases(missCO2),]
# Giải thích:
# - complete.cases(missCO2) trả về TRUE cho các dòng không có giá trị NA
# - Toán tử ! phủ định kết quả, trả về TRUE cho các dòng có ít nhất một giá trị NA
# - missCO2[chỉ số, ] chọn các dòng thỏa mãn điều kiện

# 6. Điền các giá trị uptake bị thiếu với giá trị 20.


# 7. Điền các giá trị conc bị thiếu với giá trị trung bình của conc.

# 8. Trích xuất giá trị số từ biến weight và lưu trong cột mới
# "weightNumber". Điểm thưởng nếu giữ mã trong một dòng,
# không sử dụng đối tượng trung gian nào.


### Các bài tập ######################################################################
### Sử dụng bộ dữ liệu có sẵn CO2 cho các nhiệm vụ sau:
# 1. In giá trị CO2 uptake từ lớn nhất đến nhỏ nhất.
CO2
sort(CO2$uptake, decreasing = TRUE)
# 2. Hiển thị các dòng của bộ dữ liệu CO2 có Type là Quebec và Treatment là chilled.
CO2[CO2$Type == "Quebec" & CO2$Treatment == "chilled", ]
# 3. Hiển thị các dòng của bộ dữ liệu CO2 có uptake lớn hơn 40 và 
# được sắp xếp theo giá trị conc từ nhỏ nhất đến lớn nhất.
# Điểm thưởng nếu giữ toàn bộ mã trong một dòng duy nhất. Nếu cần tạo
# một đối tượng trung gian, hãy đặt tên là 'temp'.
CO2[order(CO2$conc), ][CO2[order(CO2$conc), ]$uptake > 40, ]
# 4. Làm thế nào để sắp xếp ngẫu nhiên bộ dữ liệu CO2? GỢI Ý: Bạn có thể cần tạo
# một vector với các chỉ số ngẫu nhiên từ kết quả của order(runif(...)).
# Tham khảo phần "Picking random rows from data".
# Điểm thưởng nếu viết mã trong một dòng duy nhất không có đối tượng trung gian.
### Chạy mã này trước khi thực hiện các nhiệm vụ tiếp theo
set.seed(123)
missCO2 <- CO2
missCO2[c(as.integer(runif(10)*nrow(missCO2)),14:16),"uptake"] <- NA
missCO2[c(as.integer(runif(10)*nrow(missCO2)),14:16),"conc"] <- NA
missCO2$weight <- paste0(as.integer(runif(nrow(missCO2))*30),"kg")


# 5. Hiển thị các dòng của bộ dữ liệu missCO2 có ít nhất một giá trị bị thiếu.
missCO2[!complete.cases(missCO2), ]
# 6. Điền các giá trị uptake bị thiếu với giá trị 20.

# 7. Điền các giá trị conc bị thiếu với giá trị trung bình của conc.

# 8. Trích xuất giá trị số từ biến weight và lưu trong cột mới
# "weightNumber". Điểm thưởng nếu giữ mã trong một dòng,
# không sử dụng đối tượng trung gian nào.


#######################
### Các bài tập ######################################################################
### Sử dụng bộ dữ liệu có sẵn CO2 cho các nhiệm vụ sau:
# 1. In giá trị CO2 uptake từ lớn nhất đến nhỏ nhất.


# 2. Hiển thị các dòng của bộ dữ liệu CO2 có Type là Quebec và Treatment là chilled.

# 3. Hiển thị các dòng của bộ dữ liệu CO2 có uptake lớn hơn 40 và 
# được sắp xếp theo giá trị conc từ nhỏ nhất đến lớn nhất.
# Điểm thưởng nếu giữ toàn bộ mã trong một dòng duy nhất. Nếu cần tạo
# một đối tượng trung gian, hãy đặt tên là 'temp'.




# 4. Làm thế nào để sắp xếp ngẫu nhiên bộ dữ liệu CO2? GỢI Ý: Bạn có thể cần tạo
# một vector với các chỉ số ngẫu nhiên từ kết quả của order(runif(...)).
# Tham khảo phần "Picking random rows from data".
# Điểm thưởng nếu viết mã trong một dòng duy nhất không có đối tượng trung gian.


### Chạy mã này trước khi thực hiện các nhiệm vụ tiếp theo
set.seed(123)
missCO2 <- CO2
missCO2[c(as.integer(runif(10)*nrow(missCO2)),14:16),"uptake"] <- NA
missCO2[c(as.integer(runif(10)*nrow(missCO2)),14:16),"conc"] <- NA
missCO2$weight <- paste0(as.integer(runif(nrow(missCO2))*30),"kg")
missCO2

# 5. Hiển thị các dòng của bộ dữ liệu missCO2 có ít nhất một giá trị bị thiếu.


# Giải thích:
# - complete.cases(missCO2) trả về TRUE cho các dòng không có giá trị NA
# - Toán tử ! phủ định kết quả, trả về TRUE cho các dòng có ít nhất một giá trị NA
# - missCO2[chỉ số, ] chọn các dòng thỏa mãn điều kiện

# 6. Điền các giá trị uptake bị thiếu với giá trị 20.

# 7. Điền các giá trị conc bị thiếu với giá trị trung bình của conc.


# 8. Trích xuất giá trị số từ biến weight và lưu trong cột mới
# "weightNumber". Điểm thưởng nếu giữ mã trong một dòng,
# không sử dụng đối tượng trung gian nào.



# Lưu DATA FRAME vào một tệp CSV

# Lưu DATA FRAME vào một tệp RDS (định dạng R)

# Lưu nhiều đối tượng vào một tệp RData
# save(iris, mtcars, file = "multiple_datasets.RData")