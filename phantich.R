# Cài đặt và tải các thư viện cần thiết
library(dplyr)
library(tidyr)      # Add this line for pivot_longer()
library(ggplot2)
library(corrplot)
library(VIM)
library(mice)
library(caret)
library(randomForest)
library(xgboost)
library(glmnet)
library(car)
library(performance)

# 1. ĐỌC VÀ KHÁM PHÁ DỮ LIỆU TỔNG QUAN

# Đọc dữ liệu từ file CSV
data <- read.csv("food_consumption_complete.csv")

# Thông tin cơ bản về dữ liệu
cat(" THÔNG TIN CƠ BẢN VỀ DỮ LIỆU \n")
cat("Kích thước dữ liệu:", dim(data), "\n")
cat("Số quan sát:", nrow(data), "\n")
cat("Số biến:", ncol(data), "\n\n")

# Kiểm tra cấu trúc và thông tin cơ bản của dữ liệu
str(data)

# Thống kê mô tả cho tất cả các biến
summary(data)

# 2. PHÂN TÍCH VÀ XỬ LÝ GIÁ TRỊ THIẾU (MISSING VALUES)
cat(" KIỂM TRA VÀ PHÂN TÍCH GIÁ TRỊ THIẾU \n")
missing_counts <- sapply(data, function(x) sum(is.na(x)))
missing_data <- data.frame(
  bien_so = names(missing_counts),
  so_gia_tri_thieu = missing_counts,
  phan_tram_thieu = round(missing_counts / nrow(data) * 100, 2)
) %>%
  arrange(desc(so_gia_tri_thieu))

print(missing_data)
# Trực quan hóa dữ liệu thiếu bằng biểu đồ
VIM::aggr(data, col = c('navyblue', 'red'), numbers = TRUE, sortVars = TRUE,
          main = "Phân tích dữ liệu thiếu")

# Xử lý và thay thế giá trị thiếu một cách hợp lý
data_clean <- data %>%
  # Thay thế NA bằng 0 cho các biến chi phí y tế (hợp lý vì không sử dụng dịch vụ = 0 chi phí)
  mutate(
    across(c(outpatient_visits, outpatient_cost, inpatient_visits, inpatient_cost,
             insurance_premium, medicine_cost, equipment_cost, health_subsidy), 
           ~replace_na(., 0)),
    # Với các biến thu nhập, thay thế bằng giá trị trung vị để tránh ảnh hưởng của outliers
    across(c(total_income, other_income), ~replace_na(., median(., na.rm = TRUE)))
  )

# 3. PHÂN TÍCH KHÁM PHÁ DỮ LIỆU CHI TIẾT (EXPLORATORY DATA ANALYSIS)

cat("=== PHÂN TÍCH KHÁM PHÁ DỮ LIỆU CHI TIẾT ===\n")

# Phân tích phân bố của biến mục tiêu (chi tiêu thực phẩm)
ggplot(data_clean, aes(x = total_food_con)) +
  geom_histogram(bins = 50, fill = "skyblue", alpha = 0.7, color = "black") +
  labs(title = "Phân bố Chi tiêu Thực phẩm của Hộ gia đình", 
       x = "Chi tiêu thực phẩm (VNĐ)", y = "Số lượng hộ gia đình") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Biến đổi logarit để giảm độ lệch (skewness) của dữ liệu
data_clean$log_food_con <- log(data_clean$total_food_con + 1)

ggplot(data_clean, aes(x = log_food_con)) +
  geom_histogram(bins = 50, fill = "lightgreen", alpha = 0.7, color = "black") +
  labs(title = "Phân bố Log(Chi tiêu Thực phẩm)", 
       x = "Log(Chi tiêu thực phẩm)", y = "Số lượng hộ gia đình") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Thống kê mô tả chi tiết theo từng năm
yearly_stats <- data_clean %>%
  group_by(year) %>%
  summarise(
    chi_tieu_tb = mean(total_food_con, na.rm = TRUE),
    chi_tieu_trung_vi = median(total_food_con, na.rm = TRUE),
    do_lech_chuan = sd(total_food_con, na.rm = TRUE),
    thu_nhap_tb = mean(total_income, na.rm = TRUE),
    quy_mo_ho_tb = mean(household_size, na.rm = TRUE),
    .groups = 'drop'
  )
cat("Thống kê theo năm:\n")
print(yearly_stats)

# Phân tích mối quan hệ: Chi tiêu thực phẩm theo quy mô hộ gia đình
ggplot(data_clean, aes(x = household_size, y = total_food_con)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Mối quan hệ: Chi tiêu Thực phẩm và Quy mô Hộ gia đình", 
       x = "Số thành viên trong hộ", y = "Chi tiêu thực phẩm (VNĐ)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Phân tích mối quan hệ: Chi tiêu thực phẩm theo thu nhập
ggplot(data_clean, aes(x = total_income, y = total_food_con)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Mối quan hệ: Chi tiêu Thực phẩm và Thu nhập", 
       x = "Tổng thu nhập hộ gia đình (VNĐ)", y = "Chi tiêu thực phẩm (VNĐ)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# 4. KỸ THUẬT TẠO ĐẶC TRƯNG MỚI (FEATURE ENGINEERING)

cat("TẠO CÁC ĐẶC TRƯNG MỚI CHO MÔ HÌNH \n")

# Tạo các biến đặc trưng mới để cải thiện hiệu suất mô hình
data_features <- data_clean %>%
  mutate(
    # Chi tiêu thực phẩm bình quân đầu người
    chi_tieu_dau_nguoi = total_food_con / household_size,
    
    # Thu nhập bình quân đầu người
    thu_nhap_dau_nguoi = total_income / household_size,
    
    # Tỷ lệ chi tiêu thực phẩm trên tổng thu nhập (Hệ số Engel)
    ty_le_chi_tieu_thu_nhap = ifelse(total_income > 0, total_food_con / total_income, 0),
    
    # Tổng chi phí chăm sóc sức khỏe
    tong_chi_phi_y_te = outpatient_cost + inpatient_cost + medicine_cost + equipment_cost,
    
    # Chi phí y tế bình quân đầu người
    chi_phi_y_te_dau_nguoi = tong_chi_phi_y_te / household_size,
    
    # Biến nhị phân: Có mua bảo hiểm y tế hay không
    co_bao_hiem = ifelse(insurance_premium > 0, 1, 0),
    
    # Biến nhị phân: Có sử dụng dịch vụ y tế hay không
    su_dung_dich_vu_y_te = ifelse(outpatient_visits > 0 | inpatient_visits > 0, 1, 0),
    
    # Nhóm tuổi của chủ hộ
    nhom_tuoi_chu_ho = case_when(
      head_age < 30 ~ "Tre",
      head_age >= 30 & head_age < 60 ~ "Trung_nien",
      head_age >= 60 ~ "Cao_tuoi"
    ),
    
    # Phân loại theo mức thu nhập (quintile)
    nhom_thu_nhap = ntile(total_income, 5)
  )

# 5. PHÂN TÍCH TƯƠNG QUAN GIỮA CÁC BIẾN

# Tạo ma trận tương quan cho các biến số
numeric_vars <- data_features %>%
  select_if(is.numeric) %>%
  select(-h_code, -tinh, -year) %>%
  cor(use = "complete.obs")

# Vẽ biểu đồ ma trận tương quan
corrplot(numeric_vars, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black",
         title = "Ma trận tương quan giữa các biến số")

# Xác định các biến có tương quan cao nhất với chi tiêu thực phẩm
cor_with_target <- numeric_vars[,"total_food_con"] %>%
  abs() %>%
  sort(decreasing = TRUE)
cat("Top 10 biến có tương quan cao nhất với chi tiêu thực phẩm:\n")
print(head(cor_with_target, 10))

# 6. CHUẨN BỊ DỮ LIỆU CHO QUÁ TRÌNH HỌC MÁY
# Lựa chọn các đặc trưng quan trọng cho việc xây dựng mô hình
model_data <- data_features %>%
  select(
    # Biến mục tiêu cần dự đoán
    total_food_con,
    
    # Đặc điểm hộ gia đình
    household_size, head_age, head_gender,
    
    # Các biến kinh tế
    total_income, other_income, thu_nhap_dau_nguoi,
    
    # Các biến y tế và sức khỏe
    outpatient_visits, outpatient_cost, inpatient_visits, inpatient_cost,
    insurance_premium, medicine_cost, equipment_cost, health_subsidy,
    tong_chi_phi_y_te, chi_phi_y_te_dau_nguoi, co_bao_hiem, su_dung_dich_vu_y_te,
    
    # Các đặc trưng được tạo mới
    chi_tieu_dau_nguoi, nhom_thu_nhap,
    
    # Vị trí địa lý và thời gian
    tinh, year, area_type1, area_type2
  ) %>%
  # Chuyển đổi các biến phân loại thành factor
  mutate(
    head_gender = as.factor(head_gender),
    area_type1 = as.factor(area_type1),
    area_type2 = as.factor(area_type2),
    tinh = as.factor(tinh),
    year = as.factor(year),
    co_bao_hiem = as.factor(co_bao_hiem),
    su_dung_dich_vu_y_te = as.factor(su_dung_dich_vu_y_te),
    nhom_thu_nhap = as.factor(nhom_thu_nhap)
  ) %>%
  # Loại bỏ các quan sát có giá trị thiếu
  na.omit()

cat("Dữ liệu sau khi xử lý có", nrow(model_data), "quan sát và", ncol(model_data), "biến\n")

# 7. CHIA DỮ LIỆU THÀNH TẬP HUẤN LUYỆN VÀ TẬP KIỂM TRA
# Thiết lập seed để đảm bảo kết quả có thể tái lập
set.seed(123)

# Chia dữ liệu: 80% cho huấn luyện, 20% cho kiểm tra
train_index <- createDataPartition(model_data$total_food_con, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

cat("Tập huấn luyện:", nrow(train_data), "quan sát\n")
cat("Tập kiểm tra:", nrow(test_data), "quan sát\n")

# 8. XÂY DỰNG VÀ HUẤN LUYỆN CÁC MÔ HÌNH HỌC MÁY

cat(" XÂY DỰNG VÀ HUẤN LUYỆN MÔ HÌNH \n")

# 8.1. Linear Regression Model
cat("Huấn luyện mô hình Linear Regression\n")
lm_model <- lm(total_food_con ~ ., data = train_data)

# 8.2. Random Forest Model
cat("Huấn luyện mô hình Random Forest\n")
rf_model <- train(
  total_food_con ~ .,
  data = train_data,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5),
  ntree = 100,
  importance = TRUE
)

# 8.3. XGBoost Model
cat("Huấn luyện mô hình XGBoost\n")
# Prepare data for XGBoost
train_matrix <- model.matrix(total_food_con ~ . - 1, data = train_data)
train_label <- train_data$total_food_con

xgb_model <- train(
  total_food_con ~ .,
  data = train_data,
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 5),
  verbosity = 0
)

# 8.4. Elastic Net Model
cat("Huấn luyện mô hình Elastic Net\n")
elastic_model <- train(
  total_food_con ~ .,
  data = train_data,
  method = "glmnet",
  trControl = trainControl(method = "cv", number = 5),
  tuneLength = 10
)

cat("Hoàn thành huấn luyện tất cả các mô hình!\n\n")

# 9. DỰ ĐOÁN VÀ ĐÁNH GIÁ MÔ HÌNH
cat(" DỰ ĐOÁN VÀ ĐÁNH GIÁ MÔ HÌNH \n")

# Make predictions
lm_pred <- predict(lm_model, test_data)
rf_pred <- predict(rf_model, test_data)
xgb_pred <- predict(xgb_model, test_data)
elastic_pred <- predict(elastic_model, test_data)

# Calculate evaluation metrics
calculate_metrics <- function(actual, predicted) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  r2 <- cor(actual, predicted)^2
  mape <- mean(abs((actual - predicted) / actual)) * 100
  
  return(c(RMSE = rmse, MAE = mae, R2 = r2, MAPE = mape))
}

# Evaluation results
results <- data.frame(
  Model = c("Linear Regression", "Random Forest", "XGBoost", "Elastic Net"),
  rbind(
    calculate_metrics(test_data$total_food_con, lm_pred),
    calculate_metrics(test_data$total_food_con, rf_pred),
    calculate_metrics(test_data$total_food_con, xgb_pred),
    calculate_metrics(test_data$total_food_con, elastic_pred)
  )
)

cat(" KẾT QUẢ ĐÁNH GIÁ MÔ HÌNH \n")
print(results)
# 10. FEATURE IMPORTANCE

# Random Forest Feature Importance
rf_importance <- varImp(rf_model)
plot(rf_importance, top = 15, main = "Feature Importance - Random Forest")

# XGBoost Feature Importance
xgb_importance <- varImp(xgb_model)
plot(xgb_importance, top = 15, main = "Feature Importance - XGBoost")

# 11. MODEL DIAGNOSTICS

# Best model selection
best_model_idx <- which.min(results$RMSE)
best_model_name <- results$Model[best_model_idx]
cat("Mô hình tốt nhất:", best_model_name, "\n")

# Get best model
best_model <- switch(best_model_name,
                     "Linear Regression" = lm_model,
                     "Random Forest" = rf_model,
                     "XGBoost" = xgb_model,
                     "Elastic Net" = elastic_model)

best_pred <- switch(best_model_name,
                    "Linear Regression" = lm_pred,
                    "Random Forest" = rf_pred,
                    "XGBoost" = xgb_pred,
                    "Elastic Net" = elastic_pred)

# Prediction vs Actual plot
ggplot(data.frame(Actual = test_data$total_food_con, Predicted = best_pred),
       aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = paste("Actual vs Predicted -", best_model_name),
       x = "Actual Food Consumption", y = "Predicted Food Consumption") +
  theme_minimal()

# Residuals plot
residuals <- test_data$total_food_con - best_pred
ggplot(data.frame(Predicted = best_pred, Residuals = residuals),
       aes(x = Predicted, y = Residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = paste("Residuals Plot -", best_model_name),
       x = "Predicted Values", y = "Residuals") +
  theme_minimal()

# 12. SAVE MODELS
# Save the best model
saveRDS(best_model, "best_food_consumption_model.rds")
cat("Đã lưu mô hình tốt nhất vào file: best_food_consumption_model.rds\n")

# Save results
write.csv(results, "model_evaluation_results.csv", row.names = FALSE)
cat("Đã lưu kết quả đánh giá vào file: model_evaluation_results.csv\n")

# 13. PREDICTION FUNCTION
predict_food_consumption <- function(model_file, new_data) {
  model <- readRDS(model_file)
  predictions <- predict(model, new_data)
  return(predictions)
}

cat(" HOÀN THÀNH \n")
cat("Để sử dụng mô hình cho dự đoán mới:\n")
cat("predictions <- predict_food_consumption('best_food_consumption_model.rds', new_data)\n")
