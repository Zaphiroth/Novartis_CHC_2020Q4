# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Novartis CHC 2020Q4
# Purpose:      Readin data
# programmer:   Zhe Liu
# Date:         2021-02-23
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Growth data ----
## Bluebook(Beijing)
raw.100 <- read_csv('02_Inputs/data/shequ_100_bjjszj_20_packid_moleinfo.csv', 
                    locale = locale(encoding = 'GB18030'))

## CPA(Shanghai)
raw.cpa <- read_feather('02_Inputs/data/cpa_辉瑞2020年1-11月.feather')
cpa.mapping <- read.xlsx('02_Inputs/data/产品匹配表_Pfizer_20201029.xlsx')

## Guangzhou
raw.gz <- read_feather('02_Inputs/data/Servier_guangzhou_17181920_packid_moleinfo.feather')

## CHPA
chpa.2020q4 <- read.xlsx('02_Inputs/ims_chpa_to20Q4_fmt.xlsx')


##---- History delivery ----
history.novartis <- fread('06_Deliveries/qv_data_2018Q1_2020Q3_1129.csv', encoding = 'UTF-8')
