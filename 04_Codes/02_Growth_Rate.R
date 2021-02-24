# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Novartis CHC 2020Q4
# Purpose:      Growth rate
# programmer:   Zhe Liu
# Date:         2021-02-23
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Growth rate ----
## CHPA
growth.chpa <- chpa.2020q4 %>% 
  mutate(growth_rmb = `2020Q4_RENMINBI` / `2020Q3_RENMINBI`, 
         growth_unit = `2020Q4_UNIT` / `2020Q3_UNIT`, 
         growth_su = `2020Q4_SU` / `2020Q3_SU`) %>% 
  select(Pack_ID, growth_rmb, growth_unit, growth_su) %>% 
  filter(!is.na(growth_rmb), !is.infinite(growth_rmb), growth_rmb != 0)

## Beijing
growth.bj <- raw.100 %>% 
  filter(Province == '北京市', Quarter %in% c('2020Q3', '2020Q4'), 
         !is.na(packcode)) %>% 
  mutate(Year = as.character(Year), 
         packcode = stri_pad_left(packcode, 7, 0)) %>% 
  group_by(Quarter, packcode) %>% 
  summarise(Value = sum(Value, na.rm = TRUE), 
            Volume = sum(Volume, na.rm = TRUE), 
            Dosage_Unit = sum(Dosage_Unit, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = packcode, 
              names_from = Quarter, 
              values_from = c(Value, Volume, Dosage_Unit), 
              values_fill = 0) %>% 
  mutate(growth_value = `Value_2020Q4` / `Value_2020Q3`, 
         growth_volume = `Volume_2020Q4` / `Volume_2020Q3`, 
         growth_dosage = `Dosage_Unit_2020Q4` / `Dosage_Unit_2020Q3`, 
         growth_dosage = if_else(is.na(growth_dosage) | is.infinite(growth_dosage) | growth_dosage == 0, 
                                 growth_volume, growth_dosage)) %>% 
  filter(!is.na(growth_value), !is.infinite(growth_value), growth_value > 0) %>% 
  select(packcode, growth_value, growth_volume, growth_dosage) %>% 
  full_join(growth.chpa, by = c('packcode' = 'Pack_ID')) %>% 
  mutate(growth_value = if_else(is.na(growth_value), growth_rmb, growth_value), 
         growth_volume = if_else(is.na(growth_volume), growth_unit, growth_volume), 
         growth_dosage = if_else(is.na(growth_dosage), growth_su, growth_dosage)) %>% 
  mutate(growth_value = case_when(packcode %in% c('4514502', '5953102', '5869402', '6530702') ~ growth_rmb, 
                                  TRUE ~ growth_value), 
         growth_volume = case_when(packcode %in% c('4514502', '5953102', '5869402', '6530702') ~ growth_unit, 
                                   TRUE ~ growth_volume), 
         growth_dosage = case_when(packcode %in% c('4514502', '5953102', '5869402', '6530702') ~ growth_su, 
                                   TRUE ~ growth_dosage)) %>% 
  select(packcode, growth_value, growth_volume, growth_dosage)

## Shanghai
growth.sh <- raw.cpa %>% 
  filter(CITY == '上海市') %>% 
  rename(Brand = PRODUCT_NAME, 
         Form = APP2_COD, 
         Specifications = PACK_DES, 
         Pack_Number = PACK_NUMBER, 
         Manufacturer = CORP_NAME) %>% 
  mutate(Brand = ifelse(is.na(Brand), Molecule, Brand), 
         Form =  ifelse(is.na(Form), NA_character_, Form), 
         Specifications =  ifelse(is.na(Specifications), NA_character_, Specifications), 
         Pack_Number =  ifelse(is.na(Pack_Number), NaN, as.character(Pack_Number)), 
         Manufacturer =  ifelse(is.na(Manufacturer), NA_character_, Manufacturer)) %>% 
  mutate(min1 = paste(Brand, Form, Specifications, Pack_Number, Manufacturer, sep = '|')) %>% 
  left_join(cpa.mapping, by = c('Brand', 'Specifications', 'Pack_Number', 'Form', 'Manufacturer', 'min1')) %>% 
  filter(!is.na(pfc)) %>% 
  mutate(q = case_when(MONTH %in% c(1, 2, 3) ~ 'Q1', 
                       MONTH %in% c(4, 5, 6) ~ 'Q2', 
                       MONTH %in% c(7, 8, 9) ~ 'Q3', 
                       MONTH %in% c(10, 11, 12) ~ 'Q4', 
                       TRUE ~ NA_character_), 
         quarter = stri_paste(YEAR, q), 
         packid = stri_pad_left(pfc, 7, 0)) %>% 
  filter(MONTH %in% c(7, 8, 10, 11)) %>% 
  group_by(quarter, packid) %>% 
  summarise(VALUE = sum(VALUE, na.rm = TRUE), 
            DOSAGE = sum(STANDARD_UNIT, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = packid, 
              names_from = quarter, 
              values_from = c(VALUE, DOSAGE), 
              values_fill = 0) %>% 
  mutate(growth_value = `VALUE_2020Q4` / `VALUE_2020Q3`, 
         growth_dosage = `DOSAGE_2020Q4` / `DOSAGE_2020Q3`) %>% 
  filter(!is.na(growth_value), !is.infinite(growth_value), growth_value > 0) %>% 
  select(packid, growth_value, growth_dosage) %>% 
  full_join(growth.chpa, by = c('packid' = 'Pack_ID')) %>% 
  mutate(growth_value = if_else(is.na(growth_value), growth_rmb, growth_value), 
         growth_dosage = if_else(is.na(growth_dosage), growth_su, growth_dosage)) %>% 
  select(packid, growth_value, growth_dosage)

## Guangzhou
growth.gz <- raw.gz %>% 
  filter(Quarter %in% c('2020Q3', '2020Q4'), !is.na(packcode)) %>% 
  group_by(Quarter, packcode) %>% 
  summarise(Value = sum(Value, na.rm = TRUE), 
            Volume = sum(Volume, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = packcode, 
              names_from = Quarter, 
              values_from = c(Value, Volume), 
              values_fill = 0) %>% 
  mutate(growth_value = `Value_2020Q4` / `Value_2020Q3`, 
         growth_volume = `Volume_2020Q4` / `Volume_2020Q3`) %>% 
  filter(!is.na(growth_value), !is.infinite(growth_value), growth_value > 0) %>% 
  select(packcode, growth_value, growth_volume) %>% 
  full_join(growth.chpa, by = c('packcode' = 'Pack_ID')) %>% 
  mutate(growth_value = if_else(is.na(growth_value), growth_rmb, growth_value), 
         growth_volume = if_else(is.na(growth_volume), growth_unit, growth_volume)) %>% 
  mutate(growth_value = case_when(packcode %in% c('6749004', '3365902', '4743002') ~ growth_rmb, 
                                  TRUE ~ growth_value), 
         growth_volume = case_when(packcode %in% c('6749004', '3365902', '4743002') ~ growth_unit, 
                                   TRUE ~ growth_volume)) %>% 
  select(packcode, growth_value, growth_volume)
