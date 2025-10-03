rm(list = ls())

# Helper function
add_zero <- function(x) {
  tibble(x_text = as.character(x)) %>% 
    mutate(n_digits = str_count(x_text),
           n_max = max(n_digits, na.rm = TRUE), 
           delta = n_max - n_digits, 
           pre = strrep("0", times = delta), 
           full_code = str_c(pre, x_text)) %>% 
    pull(full_code) %>% 
    return()
}

library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(haven)

# Xác định hộ có dữ liệu cả 3 năm
read_dta("C:/Users/My PC/VHLSS 2016/Bang hoi VHLSS 2016/Data VHLSS 2016/Ho1.dta") -> ho1_2016

# Tạo lại các biến h_code và lọc dữ liệu
ho1_2016 <- ho1_2016 %>% 
  mutate(h_code16 = str_c(add_zero(tinh), add_zero(huyen), add_zero(xa), add_zero(diaban), add_zero(hoso)),
         h_code14 = str_c(add_zero(tinh14), add_zero(huyen14), add_zero(xa14), add_zero(diaban14), add_zero(hoso14)))

h_code_16_14 <- ho1_2016 %>% 
  filter(h_code16 == h_code14) %>% 
  pull(h_code16) %>% 
  unique()

read_dta("C:/Users/My PC/VHLSS 2104/VHLSS 2104/VHLSS2014_Households/Ho1.dta") -> ho1_2014

ho1_2014 <- ho1_2014 %>% 
  mutate(h_code14 = str_c(add_zero(tinh), add_zero(huyen), add_zero(xa), add_zero(diaban), add_zero(hoso)),
         h_code12 = str_c(add_zero(tinh12), add_zero(huyen12), add_zero(xa12), add_zero(diaban12), add_zero(hoso12)))

h_code_14_12 <- ho1_2014 %>%
  filter(h_code14 == h_code12) %>% 
  pull(h_code14) %>% 
  unique()

h_code_common <- base::intersect(h_code_16_14, h_code_14_12)

# Xử lý dữ liệu thực phẩm
process_food <- function(file_path, year) {
  read_dta(file_path) %>%
    filter(m5a2ma >= 101 & m5a2ma <= 154 & !m5a2ma %in% c(144, 145, 146, 147, 148, 149)) %>%
    mutate(h_code = str_c(add_zero(tinh), add_zero(huyen), add_zero(xa), add_zero(diaban), add_zero(hoso))) %>% 
    filter(h_code %in% h_code_common) %>% 
    select(tinh, h_code, food_con = m5a2c2b) %>%
    group_by(h_code, tinh) %>% 
    summarise(total_food_con = sum(food_con, na.rm = TRUE), .groups = 'drop') %>%
    mutate(tinh = as.numeric(tinh), year = year)
}

data_food_consumption <- bind_rows(
  process_food("C:\\Users\\My PC\\VHLSS 2016\\Bang hoi VHLSS 2016\\Data VHLSS 2016\\Muc5a2.dta", 2016),
  process_food("C:\\Users\\My PC\\VHLSS 2104\\VHLSS 2104\\VHLSS2014_Households\\Muc5a2.dta", 2014),
  process_food("C:\\Users\\My PC\\VHLSS 2012\\VHLSS 2012\\Muc5a2.dta", 2012)
)

# Chỉ giữ hộ có đủ 3 năm dữ liệu
full_16_14_12 <- data_food_consumption %>% 
  group_by(h_code) %>% 
  count() %>% 
  filter(n == 3) %>% 
  pull(h_code)

food_consumption_balanced <- data_food_consumption %>% filter(h_code %in% full_16_14_12)

# Đọc dữ liệu bổ sung
read_dta("C:\\Users\\My PC\\VHLSS 2012\\VHLSS 2012\\Ho1.dta") -> ho1_2012
read_dta("C:/Users/My PC/VHLSS 2016/Bang hoi VHLSS 2016/Data VHLSS 2016/Muc1A.dta") -> muc1_2016
read_dta("C:/Users/My PC/VHLSS 2104/VHLSS 2104/VHLSS2014_Households/Muc1A.dta") -> muc1_2014
read_dta("C:\\Users\\My PC\\VHLSS 2012\\VHLSS 2012\\Muc1A.dta") -> muc1_2012

# Quy mô hộ gia đình - sửa lỗi ở đây
household_size_all <- bind_rows(
  ho1_2016 %>% 
    select(h_code16, tsnguoi) %>% 
    rename(h_code = h_code16, household_size = tsnguoi) %>% 
    mutate(year = 2016),
  ho1_2014 %>% 
    select(h_code14, tsnguoi) %>% 
    rename(h_code = h_code14, household_size = tsnguoi) %>% 
    mutate(year = 2014),
  ho1_2012 %>% 
    mutate(h_code = str_c(add_zero(tinh), add_zero(huyen), add_zero(xa), add_zero(diaban), add_zero(hoso))) %>% 
    select(h_code, tsnguoi) %>% 
    rename(household_size = tsnguoi) %>% 
    mutate(year = 2012)
) %>% filter(h_code %in% food_consumption_balanced$h_code)

# Đặc điểm chủ hộ
household_heads_all <- bind_rows(
  muc1_2016 %>% filter(m1ac3 == 1) %>% 
    mutate(h_code = str_c(add_zero(tinh), add_zero(huyen), add_zero(xa), add_zero(diaban), add_zero(hoso))) %>% 
    select(h_code, head_gender = m1ac2, head_age = m1ac5, area_type1 = m1ac11, area_type2 = m1ac12) %>% 
    mutate(across(c(head_gender, head_age, area_type1, area_type2), as.numeric), year = 2016),
  muc1_2014 %>% filter(m1ac3 == 1) %>% 
    mutate(h_code = str_c(add_zero(tinh), add_zero(huyen), add_zero(xa), add_zero(diaban), add_zero(hoso))) %>% 
    select(h_code, head_gender = m1ac2, head_age = m1ac5, area_type1 = m1ac11, area_type2 = m1ac12) %>% 
    mutate(across(c(head_gender, head_age, area_type1, area_type2), as.numeric), year = 2014),
  muc1_2012 %>% filter(m1ac3 == 1) %>% 
    mutate(h_code = str_c(add_zero(tinh), add_zero(huyen), add_zero(xa), add_zero(diaban), add_zero(hoso))) %>% 
    select(h_code, head_gender = m1ac2, head_age = m1ac5, area_type1 = m1ac10, area_type2 = m1ac9) %>% 
    mutate(across(c(head_gender, head_age, area_type1, area_type2), as.numeric), year = 2012)
) %>% filter(h_code %in% food_consumption_balanced$h_code)

# Function xử lý dữ liệu chung
process_data <- function(file_path, year, columns) {
  read_dta(file_path) %>% 
    mutate(h_code = str_c(add_zero(tinh), add_zero(huyen), add_zero(xa), add_zero(diaban), add_zero(hoso)), year = year) %>% 
    filter(h_code %in% food_consumption_balanced$h_code) %>%
    select(h_code, year, all_of(columns)) %>%
    mutate(across(where(is.labelled), as.numeric)) %>%
    group_by(h_code, year) %>%
    summarise(across(everything(), ~first(na.omit(.))), .groups = 'drop')
}

# Dữ liệu Muc3
muc3a_all <- bind_rows(
  process_data("C:/Users/My PC/VHLSS 2016/Bang hoi VHLSS 2016/Data VHLSS 2016/Muc3B.dta", 2016, c("m3c5a", "m3c5b", "m3c6a", "m3c6b")),
  process_data("C:/Users/My PC/VHLSS 2104/VHLSS 2104/VHLSS2014_Households/Muc3B.dta", 2014, c("m3c5a", "m3c5b", "m3c6a", "m3c6b")),
  process_data("C:\\Users\\My PC\\VHLSS 2012\\VHLSS 2012\\Muc3A.dta", 2012, c("m3c5a", "m3c5b", "m3c6a", "m3c6b"))
)

muc3b_all <- bind_rows(
  process_data("C:/Users/My PC/VHLSS 2016/Bang hoi VHLSS 2016/Data VHLSS 2016/Muc3A.dta", 2016, c("m3c11", "m3c12a", "m3c12b", "m3c13", "m3c14", "m3c15")),
  process_data("C:/Users/My PC/VHLSS 2104/VHLSS 2104/VHLSS2014_Households/Muc3C.dta", 2014, c("m3c11", "m3c12a", "m3c12b", "m3c13", "m3c14", "m3c15")),
  process_data("C:\\Users\\My PC\\VHLSS 2012\\VHLSS 2012\\Muc3B.dta", 2012, c("m3c11", "m3c12a", "m3c12b", "m3c13", "m3c14", "m3c15"))
)

# Thu nhập
income_all <- bind_rows(
  process_data("C:/Users/My PC/VHLSS 2016/Bang hoi VHLSS 2016/Data VHLSS 2016/Ho3.dta", 2016, c("thubq", "thunhap")),
  process_data("C:/Users/My PC/VHLSS 2104/VHLSS 2104/VHLSS2014_Households/Ho3.dta", 2014, c("thubq", "thunhap")),
  process_data("C:\\Users\\My PC\\VHLSS 2012\\VHLSS 2012\\Ho13.dta", 2012, c("thubq", "thunhap"))
)

# Tổng thu nhập 
sumincome_all <- bind_rows(
  process_data("C:/Users/My PC/VHLSS 2016/Bang hoi VHLSS 2016/Data VHLSS 2016/Ho2.dta", 2016, c("m4atn", "m4dtn")),
  process_data("C:/Users/My PC/VHLSS 2104/VHLSS 2104/VHLSS2014_Households/Ho2.dta", 2014, c("m4atn", "m4dtn")),
  process_data("C:\\Users\\My PC\\VHLSS 2012\\VHLSS 2012\\Ho1.dta", 2012, c("m4atn")),
  process_data("C:\\Users\\My PC\\VHLSS 2012\\VHLSS 2012\\Ho14.dta", 2012, c("m4dtn"))
)
# Merge tất cả
food_consumption_complete <- food_consumption_balanced %>% 
  left_join(household_size_all, by = c("h_code", "year")) %>%
  left_join(household_heads_all, by = c("h_code", "year")) %>%
  left_join(muc3a_all, by = c("h_code", "year")) %>%
  left_join(muc3b_all, by = c("h_code", "year")) %>%
  left_join(income_all, by = c("h_code", "year")) %>%
  left_join(sumincome_all, by = c("h_code", "year")) %>%
  arrange(h_code, year)

food_consumption_complete <- food_consumption_complete %>%
  rename(
    # Sử dụng dịch vụ
    outpatient_visits = m3c5a,          # Số lần khám ngoại trú
    outpatient_cost = m3c5b,            # Chi phí khám ngoại trú
    inpatient_visits = m3c6a,           # Số lần điều trị nội trú
    inpatient_cost = m3c6b,             # Chi phí điều trị nội trú
    
    # Bảo hiểm
    insurance_premium = m3c11,          # Tiền mua BHYT
    insurance_outpatient = m3c12a,      # Dùng BH ngoại trú (0/1)
    insurance_inpatient = m3c12b,       # Dùng BH nội trú (0/1)
    
    # Chi phí khác
    medicine_cost = m3c13,              # Chi phí thuốc
    equipment_cost = m3c14,             # Chi phí dụng cụ y tế
    health_subsidy = m3c15,              # Trợ cấp y tế
    
    # tong thu nhap tu luong , thuong , luong hưu 
    total_income = m4atn,
    #tong thu khac 
     other_income = m4dtn
  )
food_consumption_complete <- food_consumption_complete %>%
  relocate(total_food_con, .after = last_col())
write.csv(food_consumption_complete, "C:/Users/My PC/food_consumption_complete.csv", row.names = FALSE)


# Kiểm tra dữ liệu
summary(food_consumption_complete)
str(food_consumption_complete)

# Kiểm tra giá trị thiếu 
colSums(is.na(food_consumption_complete))






