# List trong R là một cấu trúc dữ liệu linh hoạt có thể chứa:
# Nhiều kiểu dữ liệu khác nhau
# Các đối tượng có độ dài khác nhau
# Các cấu trúc dữ liệu phức tạp (vector, matrix, dataframe, thậm chí list khác)

# Tạo một list đơn giản
student <- list(
  name = 'Hoa',
  age = 18,
  grades = c(8.5, 9.0, 7.5),
  passed = TRUE
)

# Truy cập dữ liệu
# Cách 1 : Dùng [ ] ==> trả về list con
student[1]
class(student[1])

# Cách 2 : Dung [[]] => trả về giá trị
student[[1]]
class(student[[1]])

student[[3]][c(1,3)]

# Cách 3 : Dùng $ ==> truy cập theo tên
student$grades

# Thêm giá trị vào
student$id <- '012345678'
print(student)

# Cập nhật giá trị
student$grades <- c(10,10,10)
student
