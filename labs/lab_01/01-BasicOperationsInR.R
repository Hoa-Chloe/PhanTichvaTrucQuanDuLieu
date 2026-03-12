# Bước 1: Khai báo các thông số
chieu_dai <- 20
chieu_rong <- 10
don_vi <- "met"

# Bước 2: Tính toán chu vi và diện tích
chu_vi <- (chieu_dai+chieu_rong)*2
dien_tich <- chieu_dai*chieu_rong

# Bước 3: Sử dụng toán so sánh + logic
# Kiểm tra nếu diện tích > 150 và chiều dài lớn hơnchiều rộng
check_dk <- (dien_tich>150) & (chieu_dai>chieu_rong)

# Bước 4: In kết quả
print(paste("Chu vi là: ", chu_vi, don_vi))
ketQua <- paste("Diện tích là: ", dien_tich, don_vi)
ketQua

# Bước 5: Kiểm tra kiểu dữ liệu
class(don_vi)
class(check_dk)

# class() : kiểm tra kiểu dữ liệu

# is.numeric() : kiểm tra dữ liệu có phải là numeric hay không
# is.integer() : kiểm tra dữ liệu có phải là integer hay không
# is.logical() : kiểm tra dữ liệu có phải là logical hay không
# is.character(): kiểm tra dữ liệu có phải là character hay không

# Chuyển đổi kiểu
# as.numeric()
# as.integer()
# as.logical()
# as.character()
# as.Date()

a<-1.5
class(a)

b<-10
class(b)

is.integer(a)
is.integer(b)
is.numeric(b)

b<-6.89
class(b)
b<-as.integer(b)
class(b)
print(b)

# Bổ sung
help(print)
help(class)

class(check_dk) <- "character"
check_dk
