library(tidyverse)
library(REDCapR)


# Загрузка данных ---------------------------------------------------------
data_redcap    <- read_csv('_DATA_2022-08-08_1325.csv')

data <- readxl::read_xlsx('общая база.xlsx')
data <- data %>% 
  select('ФИО пациента',
         sex = 'Пол',
         birthdate = 'Дата.рожд.',
         ds_dt = 'Дата установления первичного диагноза МБ',
         relapse_dt = 'Дата рецидива (не событие)',
         event = '1-е событие',
         event_date = 'Дата события',
         outcome = 'Исход',
         outcome_date = 'Дата исхода',
         date = 'Дата первичной операции',
         mgroup = 'Молекулярная группа') 


# Переименовываем столбцы ---------------------------------------------------------
data <- data %>%
  mutate(name = str_split(`ФИО пациента`, " ", n = 3, simplify = T)) %>%
  mutate(last_name = name[,1], first_name = name[,2], middle_name = name[,3])




# Ищем пациентов, которые есть в базе ---------------------------------------------------------
# Для таблицы

data_double_first <- data %>% 
  select(first_name,
         last_name,
         birthdate,
         ds_dt,
         date,
         relapse_dt,
         mgroup) %>% 
  mutate(pat_fio = paste(last_name,str_sub(`first_name`,1,1),gsub("-", "",birthdate), sep = "_"))

data_double_rel <- data %>% 
  select(first_name,
         last_name,
         birthdate,
         ds_dt,
         date,
         outcome,
         outcome_date)

data_double <- data_double_first %>% 
  bind_cols(data_double_rel) %>%
  add_column(table_id = c(1:max(row_number(data$sex))))

data_one <- data_redcap %>%
  select(record_id,
         first_name,
         last_name,
         birthdate,
         ds_dt,
         date,
         relapse,
         relapse_dt,
         mgroup,
         outcome,
         outcome_date) %>%
  mutate(pat_fio = paste(last_name,str_sub(`first_name`,1,1),gsub("-", "",birthdate), sep = "_"))
  data_one[data_one$record_id %in% data_one_new$record_id, 'relapse'] <-  '1'

data_double <- data_one %>% 
  full_join(data_double, by = 'pat_fio') %>% 
  filter(!is.na(table_id) | (!is.na(record_id) & relapse == 1))
openxlsx::write.xlsx(data_double, 'data.xlsx')
