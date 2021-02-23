# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Novartis CHC 2020Q4
# Purpose:      Result
# programmer:   Zhe Liu
# Date:         2021-02-23
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- History delivery CHC ----
history.novartis.chc <- history.novartis %>% 
  filter(CHANNEL == 'CHC') %>% 
  mutate(PACK_ID = stri_pad_left(PACK_ID, 7, 0))


##---- 2020Q4 result ----
## Beijing
novartis.2020q4.bj.ly <- history.novartis.chc %>% 
  filter(CITY == '北京市', QUARTER == '2019Q4') %>% 
  mutate(QUARTER = '2020Q4') %>% 
  select(-ends_with('_LY')) %>% 
  rename(SALES_VALUE_LY = SALES_VALUE, 
         TOTAL_UNITS_LY = TOTAL_UNITS, 
         COUNTING_UNITS_LY = COUNTING_UNITS)

novartis.2020q4.bj <- history.novartis.chc %>% 
  filter(CITY == '北京市', QUARTER == '2020Q3') %>% 
  left_join(growth.bj, by = c('PACK_ID' = 'packcode')) %>% 
  mutate(growth_value = if_else(is.na(growth_value), 1, growth_value), 
         growth_volume = if_else(is.na(growth_volume), 1, growth_volume), 
         growth_dosage = if_else(is.na(growth_dosage), 1, growth_dosage)) %>% 
  mutate(QUARTER = '2020Q4', 
         SALES_VALUE = SALES_VALUE * growth_value, 
         TOTAL_UNITS = TOTAL_UNITS * growth_volume, 
         COUNTING_UNITS = COUNTING_UNITS * growth_dosage) %>% 
  select(-ends_with('_LY'), -starts_with('growth')) %>% 
  full_join(novartis.2020q4.bj.ly) %>% 
  mutate_if(is.numeric, function(x) {
    if_else(is.na(x), 0, x)
  })

## Shanghai
novartis.2020q4.sh.ly <- history.novartis.chc %>% 
  filter(CITY == '上海市', QUARTER == '2019Q4') %>% 
  mutate(QUARTER = '2020Q4') %>% 
  select(-ends_with('_LY')) %>% 
  rename(SALES_VALUE_LY = SALES_VALUE, 
         TOTAL_UNITS_LY = TOTAL_UNITS, 
         COUNTING_UNITS_LY = COUNTING_UNITS)

novartis.2020q4.sh <- history.novartis.chc %>% 
  filter(CITY == '上海市', QUARTER == '2020Q3') %>% 
  left_join(growth.sh, by = c('PACK_ID' = 'packid')) %>% 
  mutate(growth_value = if_else(is.na(growth_value), 1, growth_value), 
         growth_dosage = if_else(is.na(growth_dosage), 1, growth_dosage)) %>% 
  mutate(QUARTER = '2020Q4', 
         SALES_VALUE = SALES_VALUE * growth_value, 
         TOTAL_UNITS = TOTAL_UNITS * growth_dosage, 
         COUNTING_UNITS = COUNTING_UNITS * growth_dosage) %>% 
  select(-ends_with('_LY'), -starts_with('growth')) %>% 
  full_join(novartis.2020q4.sh.ly) %>% 
  mutate_if(is.numeric, function(x) {
    if_else(is.na(x), 0, x)
  })

## Guangzhou
novartis.2020q4.gz.ly <- history.novartis.chc %>% 
  filter(CITY == '广州市', QUARTER == '2019Q4') %>% 
  mutate(QUARTER = '2020Q4') %>% 
  select(-ends_with('_LY')) %>% 
  rename(SALES_VALUE_LY = SALES_VALUE, 
         TOTAL_UNITS_LY = TOTAL_UNITS, 
         COUNTING_UNITS_LY = COUNTING_UNITS)

novartis.2020q4.gz <- history.novartis.chc %>% 
  filter(CITY == '广州市', QUARTER == '2020Q3') %>% 
  left_join(growth.gz, by = c('PACK_ID' = 'packcode')) %>% 
  mutate(growth_value = if_else(is.na(growth_value), 1, growth_value), 
         growth_volume = if_else(is.na(growth_volume), 1, growth_volume)) %>% 
  mutate(QUARTER = '2020Q4', 
         SALES_VALUE = SALES_VALUE * growth_value, 
         TOTAL_UNITS = TOTAL_UNITS * growth_volume, 
         COUNTING_UNITS = COUNTING_UNITS * growth_volume) %>% 
  select(-ends_with('_LY'), -starts_with('growth')) %>% 
  full_join(novartis.2020q4.gz.ly) %>% 
  mutate_if(is.numeric, function(x) {
    if_else(is.na(x), 0, x)
  })


##---- Write ----
novartis.2020q4 <- history.novartis.chc %>% 
  bind_rows(novartis.2020q4.bj, novartis.2020q4.sh, novartis.2020q4.gz)

write.csv(novartis.2020q4, '03_Outputs/qv_data_2018Q1_2020Q4_CHC.csv', 
          fileEncoding = 'UTF-8', row.names = FALSE)



