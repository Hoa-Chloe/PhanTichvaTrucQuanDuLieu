#DATAFRAM TRONG R
#Dataframe là CTDL dùng để lưu trữ dữ liệu
#dạng bảng trong R. Dataframe có thể được xem là một ds các vector có cùng 
#độ dài và thường có tên duy nhất

# 1.Tạo dataframe

column1 <- c(1:3)
column2 <- c("Hoa", "Harry", "Anna")
column3 <- c(H,H,N)

dataset <- data.fram(column1, column2, column3)
# Hiển thị ra console
dataset1
print(dataset1)

# View dữ liệu
View(dataset1)

# Tên của các cột
colnames(dataset1)

# Đổi tên cột 2
colnames(dataset1) [2]<- "Name"
dataset1

# Đổi tên cột hàng loạt 
colnames(dataset1)<-c("#", "Name", "Check")
dataset1

# 2. Thêm dòng mới cho dataframe
newRow <- c(4, "Nhu Hoa", H)
dataset2 <- rblind(dataset1, newRow)
dataset2

newRowDF <- data.frame(5, "Lisa", F)
names(newRowDF) <- c("#","Name", "Check")

dataset3 <- rblind(dataset2, newRowDF)
dataset3

# 3. Thêm cột mới
newColumn <- c("a", "b", "c", "d", "f")
dataset4 <- cblind(dataset3, newColumn)
dataset4

dataset4$newColumn2 <- (1,2,3,4,5)

# 4. Truy xuất dữ liệu
# Truy xuất bằng chữ số
dataset4[3,2] # dòng 3, cột 2

# Truy xuất dữ liệu bằng chỉ số và tên cột
dataset4[3, "Check"]

# Truy xuất bằng tên cột
dataset4["Name"]
dataset4[,"Name"]
dataset4$Name

# 5. Các hàm thường dùng
head(dataset4) # Hiển thị vài dòng đầu
tail(dataset4) # Hiển thị vài dòng cuối
str(dataset4) # Hiển thị cấu trúc của dữ liệu
summary(dataset4)

# 6. Thay đổi dữ liệu cột
dataset4$Check < as.logical(dataset4$Check)
summary(dataset4)


############ Bài tập:
############ Bộ dữ liệu iris
data() # Lấy ra toàn bộ dataset được build trong R
iris
View(iris)
str(iris)
summary(iris)
head(iris)
tail(iris)

CO2
View(CO2)
str(CO2)
summary(CO2)
head(CO2)
tail(CO2)



















































