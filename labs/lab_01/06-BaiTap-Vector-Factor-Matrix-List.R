# ### BÀI TẬP ###
# 
# 1. Tạo một factor với các giá trị "a", "b", "c" có độ dài 7. 
# Thêm nhãn "Letter A", "Letter B", "Letter C". 
# Tóm tắt giá trị của factor.

v <- c("a", "b", "c", "c", "b", "a", "b")

v_factor <- factor(v,
                   levels = c("a", "b", "c"),
                   labels = c("Letter A", "Letter B", "Letter C")
)
summary(v_factor)
str(v_factor)

# 2. Tạo một vector số có giá trị từ 1-4 và độ dài 10. 
# Bạn có thể sử dụng bất kỳ hàm nào để tạo vector. 
# Giá trị có thể được sắp xếp ngẫu nhiên. 
# Tóm tắt biến và kiểm tra kiểu của nó. 
# Sau đó sử dụng vector này để tạo một factor có thứ tự. 
# Đặt các mức "low" "medium" "high" "very high". 
# Tóm tắt giá trị và so sánh với vector ban đầu.

x <- sample(1:4, 10, replace = TRUE)
print(x)
class(x)
summary(x)

# Chuyển vector thành factor
x_factor <- factor(x,
                   levels = c(1:4),
                   labels = c("low", "medium", "high", "very high"),
                   ordered = TRUE
)
summary(x_factor)

# 3. Tạo một ma trận có 5 hàng và 2 cột, điền số 0. 
# Lưu vào biến "table".
# a) Điền cột 1 với giá trị 3
# b) Đặt phần tử thứ 3 của cột 2 thành 20
# c) In các giá trị của cột 2. Kiểm tra kiểu giá trị trong cột này
# d) Thay đổi phần tử thứ 4 của cột 2 thành "twelve". 
# In lại giá trị của cột 2. Kiểm tra kiểu của chúng. Có gì khác?
# e) Kiểu giá trị của cột 1 là gì? Tại sao?

table <- matrix(0, nrow=5, ncol=2)

table[,1] <- 3

table[3,2] <- 20

print("Giá trị cột 2:")
print(table[,2])
class(table[,2])

table[4,2] <- "twelve" 
class(table[,2])
class(table[,1])

# 4. Tạo bốn biến với các kiểu khác nhau (vectors, matrices, 
#                                           single values).
# Tạo một list từ các đối tượng này đặt tên "myList".
# a) Lấy phần tử thứ hai của list và thêm một giá trị vào đó.
# Lưu thay đổi để nó hiển thị trong list.
# b) Thêm phần tử mới vào cuối list - tạo thành vector 6 phần tử 
# với bất kỳ kiểu nào.
# c) In phần tử thứ 4 của đối tượng cuối cùng trong list.
# d) Thay đổi giá trị của phần tử thứ 5 của đối tượng cuối cùng 
# thành NA.

vec1 <- c("a", "b", "c")
mat1 <-matrix(1:6, nrow=2)
single_val <-100
vec2 <- c(1,2,3)

# Tạo list
myList <- list(vec1, mat1, single_val, vec2)

# a
myList[[1]]<-c(myList[[1]], c("d"))

#b
length(myList)

myList[[length(myList)+1]] <- c(5,6,7,8,9)

#c
myList[[length(myList)]][4]

#d
myList[[length(myList)]][5] <-NA
