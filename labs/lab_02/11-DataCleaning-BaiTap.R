# 1. Đọc dữ liệu
clients <- read.csv("D:/R/labs/data/clients.csv")

# 2. Xem cấu trúc dữ liệu
View(clients)
str(clients)
summary(clients)

# 3. Sửa các cột bị đọc sai kiểu dữ liệu (character -> numeric)
cols_to_fix <- c("Income", "Teenhome", "Recency")

clients[cols_to_fix] <- lapply(clients[cols_to_fix], function(x) suppressWarnings(as.numeric(as.character(x))))

# 4. Kiểm tra các cột có giá trị thiếu
na_counts <- colSums(is.na(clients))
na_counts[na_counts > 0]

# ----- Điền giá trị thiếu -----

# Year_Birth
clients$Year_Birth[is.na(clients$Year_Birth)] <- median(clients$Year_Birth, na.rm = TRUE)

# Income
clients$Income[is.na(clients$Income)] <- median(clients$Income, na.rm = TRUE)

# Các cột còn lại
cols_fill <- c("MntWines", "Response", "Teenhome", "Recency")

for(col in cols_fill){
  clients[[col]][is.na(clients[[col]])] <- median(clients[[col]], na.rm = TRUE)}

# 5. Kiểm tra lại NA
sum(is.na(clients))

# 6. Chuyển Education thành ordered factor
clients$Education <- factor(clients$Education,levels = c("Basic","2n","Graduation","Master","PhD"),ordered = TRUE)

# 7. Chuyển các biến phân loại thành factor
cols_factor <- c(
  "Marital_Status",
  "Response",
  "Complain",
  paste0("AcceptedCmp",1:5)
)

clients[cols_factor] <- lapply(clients[cols_factor], factor)

# 8. Lưu dữ liệu
save(clients, file = "D:/R/labs/data/clientsInR.RData")

