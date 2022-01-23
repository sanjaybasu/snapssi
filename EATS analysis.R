rm(list = ls())
library(tidyverse)
library(tableone)
library(lubridate)
library(readr)
library(tidyr)
library(stringr)
library(kableExtra)
library(ggplot2)

setwd("C:/LIFESPAN_D/CDSS Data")
ssi14 <- read_csv("C:/LIFESPAN_D/CDSS Data/sdx_ssi_2014_final_deid.csv",
                  col_select = c("CDSS_UID",
                                 "CF_IND",
                                 "SSI1401",
                                 "SSI1402",
                                 "SSI1403",
                                 "SSI1404",
                                 "SSI1405",
                                 "SSI1406",
                                 "SSI1407",
                                 "SSI1408",
                                 "SSI1409",
                                 "SSI1410",
                                 "SSI1411",
                                 "SSI1412",
                                 "CC1401",
                                 "CC1402",
                                 "CC1403",
                                 "CC1404",
                                 "CC1405",
                                 "CC1406",
                                 "CC1407",
                                 "CC1408",
                                 "CC1409",
                                 "CC1410",
                                 "CC1411",
                                 "CC1412")) %>%
  distinct()
ssi15 <- read_csv("C:/LIFESPAN_D/CDSS Data/sdx_ssi_2015_final_deid.csv",
                  col_select = c("CDSS_UID",
                                 "CF_IND",
                                 "SSI1501",
                                 "SSI1502",
                                 "SSI1503",
                                 "SSI1504",
                                 "SSI1505",
                                 "SSI1506",
                                 "SSI1507",
                                 "SSI1508",
                                 "SSI1509",
                                 "SSI1510",
                                 "SSI1511",
                                 "SSI1512",
                                 "CC1501",
                                 "CC1502",
                                 "CC1503",
                                 "CC1504",
                                 "CC1505",
                                 "CC1506",
                                 "CC1507",
                                 "CC1508",
                                 "CC1509",
                                 "CC1510",
                                 "CC1511",
                                 "CC1512")) %>%
  distinct()
ssi16 <- read_csv("C:/LIFESPAN_D/CDSS Data/sdx_ssi_2016_final_deid.csv",
                  col_select = c("CDSS_UID",
                                 "CF_IND",
                                 "SSI1601",
                                 "SSI1602",
                                 "SSI1603",
                                 "SSI1604",
                                 "SSI1605",
                                 "SSI1606",
                                 "SSI1607",
                                 "SSI1608",
                                 "SSI1609",
                                 "SSI1610",
                                 "SSI1611",
                                 "SSI1612",
                                 "CC1601",
                                 "CC1602",
                                 "CC1603",
                                 "CC1604",
                                 "CC1605",
                                 "CC1606",
                                 "CC1607",
                                 "CC1608",
                                 "CC1609",
                                 "CC1610",
                                 "CC1611",
                                 "CC1612")) %>%
  distinct()
ssi17 <- read_csv("C:/LIFESPAN_D/CDSS Data/sdx_ssi_2017_final_deid.csv",
                  col_select = c("CDSS_UID",
                                 "CF_IND",
                                 "SSI1701",
                                 "SSI1702",
                                 "SSI1703",
                                 "SSI1704",
                                 "SSI1705",
                                 "SSI1706",
                                 "SSI1707",
                                 "SSI1708",
                                 "SSI1709",
                                 "SSI1710",
                                 "SSI1711",
                                 "SSI1712",
                                 "CC1701",
                                 "CC1702",
                                 "CC1703",
                                 "CC1704",
                                 "CC1705",
                                 "CC1706",
                                 "CC1707",
                                 "CC1708",
                                 "CC1709",
                                 "CC1710",
                                 "CC1711",
                                 "CC1712")) %>%
  distinct()
ssi18 <- read_csv("C:/LIFESPAN_D/CDSS Data/sdx_ssi_2018_final_deid.csv",
                  col_select = c("CDSS_UID",
                                 "CF_IND",
                                 "SSI1801",
                                 "SSI1802",
                                 "SSI1803",
                                 "SSI1804",
                                 "SSI1805",
                                 "SSI1806",
                                 "SSI1807",
                                 "SSI1808",
                                 "SSI1809",
                                 "SSI1810",
                                 "SSI1811",
                                 "SSI1812",
                                 "CC1801",
                                 "CC1802",
                                 "CC1803",
                                 "CC1804",
                                 "CC1805",
                                 "CC1806",
                                 "CC1807",
                                 "CC1808",
                                 "CC1809",
                                 "CC1810",
                                 "CC1811",
                                 "CC1812")) %>%
  distinct()
ssi19 <- read_csv("C:/LIFESPAN_D/CDSS Data/sdx_ssi_2019_final_deid.csv",
                  col_select = c("CDSS_UID",
                                 "CF_IND",
                                 "SSI1901",
                                 "SSI1902",
                                 "SSI1903",
                                 "SSI1904",
                                 "SSI1905",
                                 "SSI1906",
                                 "SSI1907",
                                 "SSI1908",
                                 "SSI1909",
                                 "SSI1910",
                                 "SSI1911",
                                 "SSI1912",
                                 "CC1901",
                                 "CC1902",
                                 "CC1903",
                                 "CC1904",
                                 "CC1905",
                                 "CC1906",
                                 "CC1907",
                                 "CC1908",
                                 "CC1909",
                                 "CC1910",
                                 "CC1911",
                                 "CC1912")) %>%
  distinct()
ssi20 <- read_csv("C:/LIFESPAN_D/CDSS Data/sdx_ssi_2020_final_deid.csv",
                  col_select = c("CDSS_UID",
                                 "CF_IND",
                                 "SSI2001",
                                 "SSI2002",
                                 "SSI2003",
                                 "SSI2004",
                                 "SSI2005",
                                 "SSI2006",
                                 "SSI2007",
                                 "SSI2008",
                                 "SSI2009",
                                 "SSI2010",
                                 "SSI2011",
                                 "SSI2012",
                                 "CC2001",
                                 "CC2002",
                                 "CC2003",
                                 "CC2004",
                                 "CC2005",
                                 "CC2006",
                                 "CC2007",
                                 "CC2008",
                                 "CC2009",
                                 "CC2010",
                                 "CC2011",
                                 "CC2012")) %>%
  distinct()


snap14 <- read_csv("C:/LIFESPAN_D/CDSS Data/calfresh_2014_deid.csv",
                   col_select = c("CDSS_UID",
                                  "HOUSEHOLD",
                                  "YEAR",
                                  "DOB",
                                  "LANGUAGE",
                                  "RACE",
                                  "SEX",
                                  "FAF14_01",
                                  "FAF14_02",
                                  "FAF14_03",
                                  "FAF14_04",
                                  "FAF14_05",
                                  "FAF14_06",
                                  "FAF14_07",
                                  "FAF14_08",
                                  "FAF14_09",
                                  "FAF14_10",
                                  "FAF14_11",
                                  "FAF14_12",
                                  "FCC14_01",
                                  "FCC14_02",
                                  "FCC14_03",
                                  "FCC14_04",
                                  "FCC14_05",
                                  "FCC14_06",
                                  "FCC14_07",
                                  "FCC14_08",
                                  "FCC14_09",
                                  "FCC14_10",
                                  "FCC14_11",
                                  "FCC14_12")) %>%
  distinct()


snap15 <- read_csv("C:/LIFESPAN_D/CDSS Data/calfresh_2015_deid.csv",
                   col_select = c("CDSS_UID",
                                  "HOUSEHOLD",
                                  "YEAR",
                                  "DOB",
                                  "LANGUAGE",
                                  "RACE",
                                  "SEX",
                                  "FAF15_01",
                                  "FAF15_02",
                                  "FAF15_03",
                                  "FAF15_04",
                                  "FAF15_05",
                                  "FAF15_06",
                                  "FAF15_07",
                                  "FAF15_08",
                                  "FAF15_09",
                                  "FAF15_10",
                                  "FAF15_11",
                                  "FAF15_12",
                                  "FCC15_01",
                                  "FCC15_02",
                                  "FCC15_03",
                                  "FCC15_04",
                                  "FCC15_05",
                                  "FCC15_06",
                                  "FCC15_07",
                                  "FCC15_08",
                                  "FCC15_09",
                                  "FCC15_10",
                                  "FCC15_11",
                                  "FCC15_12")) %>%
  distinct()



snap16 <- read_csv("C:/LIFESPAN_D/CDSS Data/calfresh_2016_deid.csv",
                   col_select = c("CDSS_UID",
                                  "HOUSEHOLD",
                                  "YEAR",
                                  "DOB",
                                  "LANGUAGE",
                                  "RACE",
                                  "SEX",
                                  "FAF16_01",
                                  "FAF16_02",
                                  "FAF16_03",
                                  "FAF16_04",
                                  "FAF16_05",
                                  "FAF16_06",
                                  "FAF16_07",
                                  "FAF16_08",
                                  "FAF16_09",
                                  "FAF16_10",
                                  "FAF16_11",
                                  "FAF16_12",
                                  "FCC16_01",
                                  "FCC16_02",
                                  "FCC16_03",
                                  "FCC16_04",
                                  "FCC16_05",
                                  "FCC16_06",
                                  "FCC16_07",
                                  "FCC16_08",
                                  "FCC16_09",
                                  "FCC16_10",
                                  "FCC16_11",
                                  "FCC16_12")) %>%
  distinct()




snap17 <- read_csv("C:/LIFESPAN_D/CDSS Data/calfresh_2017_deid.csv",
                   col_select = c("CDSS_UID",
                                  "HOUSEHOLD",
                                  "YEAR",
                                  "DOB",
                                  "LANGUAGE",
                                  "RACE",
                                  "SEX",
                                  "FAF17_01",
                                  "FAF17_02",
                                  "FAF17_03",
                                  "FAF17_04",
                                  "FAF17_05",
                                  "FAF17_06",
                                  "FAF17_07",
                                  "FAF17_08",
                                  "FAF17_09",
                                  "FAF17_10",
                                  "FAF17_11",
                                  "FAF17_12",
                                  "FCC17_01",
                                  "FCC17_02",
                                  "FCC17_03",
                                  "FCC17_04",
                                  "FCC17_05",
                                  "FCC17_06",
                                  "FCC17_07",
                                  "FCC17_08",
                                  "FCC17_09",
                                  "FCC17_10",
                                  "FCC17_11",
                                  "FCC17_12")) %>%
  distinct()



snap18 <- read_csv("C:/LIFESPAN_D/CDSS Data/calfresh_2018_deid.csv",
                   col_select = c("CDSS_UID",
                                  "HOUSEHOLD",
                                  "YEAR",
                                  "DOB",
                                  "LANGUAGE",
                                  "RACE",
                                  "SEX",
                                  "FAF18_01",
                                  "FAF18_02",
                                  "FAF18_03",
                                  "FAF18_04",
                                  "FAF18_05",
                                  "FAF18_06",
                                  "FAF18_07",
                                  "FAF18_08",
                                  "FAF18_09",
                                  "FAF18_10",
                                  "FAF18_11",
                                  "FAF18_12",
                                  "FCC18_01",
                                  "FCC18_02",
                                  "FCC18_03",
                                  "FCC18_04",
                                  "FCC18_05",
                                  "FCC18_06",
                                  "FCC18_07",
                                  "FCC18_08",
                                  "FCC18_09",
                                  "FCC18_10",
                                  "FCC18_11",
                                  "FCC18_12")) %>%
  distinct()



snap19 <- read_csv("C:/LIFESPAN_D/CDSS Data/calfresh_2019_deid.csv",
                   col_select = c("CDSS_UID",
                                  "HOUSEHOLD",
                                  "YEAR",
                                  "DOB",
                                  "LANGUAGE",
                                  "RACE",
                                  "SEX",
                                  "FAF19_01",
                                  "FAF19_02",
                                  "FAF19_03",
                                  "FAF19_04",
                                  "FAF19_05",
                                  "FAF19_06",
                                  "FAF19_07",
                                  "FAF19_08",
                                  "FAF19_09",
                                  "FAF19_10",
                                  "FAF19_11",
                                  "FAF19_12",
                                  "FCC19_01",
                                  "FCC19_02",
                                  "FCC19_03",
                                  "FCC19_04",
                                  "FCC19_05",
                                  "FCC19_06",
                                  "FCC19_07",
                                  "FCC19_08",
                                  "FCC19_09",
                                  "FCC19_10",
                                  "FCC19_11",
                                  "FCC19_12")) %>%
  distinct()



snap20 <- read_csv("C:/LIFESPAN_D/CDSS Data/calfresh_2020_deid.csv",
                   col_select = c("CDSS_UID",
                                  "HOUSEHOLD",
                                  "YEAR",
                                  "DOB",
                                  "LANGUAGE",
                                  "RACE",
                                  "SEX",
                                  "FAF20_01",
                                  "FAF20_02",
                                  "FAF20_03",
                                  "FAF20_04",
                                  "FAF20_05",
                                  "FAF20_06",
                                  "FAF20_07",
                                  "FAF20_08",
                                  "FAF20_09",
                                  "FAF20_10",
                                  "FAF20_11",
                                  "FAF20_12",
                                  "FCC20_01",
                                  "FCC20_02",
                                  "FCC20_03",
                                  "FCC20_04",
                                  "FCC20_05",
                                  "FCC20_06",
                                  "FCC20_07",
                                  "FCC20_08",
                                  "FCC20_09",
                                  "FCC20_10",
                                  "FCC20_11",
                                  "FCC20_12")) %>%
  distinct()

county_codes <- read_csv("C:/LIFESPAN_D/CDSS Data/county_codes.csv")


# 2014 ssi participation panel

df14_ssi = ssi14 %>%
  gather(period,ssi,SSI1401:SSI1412) %>%
  select(CDSS_UID, period, ssi) %>%
  distinct() %>%
  mutate(ssi = replace_na(ssi,0),
         month = as.numeric(str_sub(period,-2,-1))) %>%
  select(CDSS_UID, ssi, month) %>%
  distinct()

# 2014 snap participation panel

df14_snap = snap14 %>%
  gather(period, snap, FAF14_01:FAF14_12) %>%
  select(CDSS_UID, period, snap) %>%
  distinct() %>%
  mutate(snap = replace_na(snap,0),
         month = as.numeric(str_sub(period,-2,-1))) %>%
  select(CDSS_UID, snap, month) %>%
  distinct()

# 2014 county panel

df14_ct = ssi14 %>%
  full_join(snap14) %>%
  gather(period, county, CC1401:CC1412) %>%
  select(CDSS_UID, period, county) %>%
  distinct() %>%
  mutate(county = replace_na(county,0),
         month = as.numeric(str_sub(period,-2,-1))) %>%
  select(CDSS_UID, county, month) %>%
  distinct()

# 2014 demographics panel

df14_demo = ssi14 %>%
  full_join(snap14) %>%
  select(CDSS_UID, HOUSEHOLD, DOB, LANGUAGE, RACE, SEX) %>%
  mutate(age = 2014-as.numeric(str_sub(DOB,1,4))) %>%
  select(CDSS_UID, HOUSEHOLD, age, LANGUAGE, RACE, SEX)

# 2014 panel

df14 = df14_demo %>%
  full_join(df14_ct) %>%
  full_join(df14_ssi) %>%
  full_join(df14_snap) %>%
  mutate(year = 2014)




# 2015 snap participation panel

df15_snap = ssi15 %>%
  full_join(snap15) %>%
  gather(period, snap, FAF15_01:FAF15_12) %>%
  select(CDSS_UID, period, snap) %>%
  distinct() %>%
  mutate(snap = replace_na(snap,0),
         month = as.numeric(str_sub(period,-2,-1))) %>%
  select(CDSS_UID, snap, month) %>%
  distinct()

# 2015 county panel

df15_ct = ssi15 %>%
  full_join(snap15) %>%
  gather(period, county, CC1501:CC1512) %>%
  select(CDSS_UID, period, county) %>%
  distinct() %>%
  mutate(county = replace_na(county,0),
         month = as.numeric(str_sub(period,-2,-1))) %>%
  select(CDSS_UID, county, month) %>%
  distinct()

# 2015 demographics panel

df15_demo = ssi15 %>%
  full_join(snap15) %>%
  select(CDSS_UID, HOUSEHOLD, DOB, LANGUAGE, RACE, SEX) %>%
  mutate(age = 2015-as.numeric(str_sub(DOB,1,4))) %>%
  select(CDSS_UID, HOUSEHOLD, age, LANGUAGE, RACE, SEX)

# 2015 panel

df15 = df15_demo %>%
  full_join(df15_ct) %>%
  full_join(df15_snap) %>%
  mutate(year = 2015)




# 2016 snap participation panel

df16_snap = ssi16 %>%
  full_join(snap16) %>%
  gather(period, snap, FAF16_01:FAF16_12) %>%
  select(CDSS_UID, period, snap) %>%
  distinct() %>%
  mutate(snap = replace_na(snap,0),
         month = as.numeric(str_sub(period,-2,-1))) %>%
  select(CDSS_UID, snap, month) %>%
  distinct()

# 2016 county panel

df16_ct = ssi16 %>%
  full_join(snap16) %>%
  gather(period, county, CC1601:CC1612) %>%
  select(CDSS_UID, period, county) %>%
  distinct() %>%
  mutate(county = replace_na(county,0),
         month = as.numeric(str_sub(period,-2,-1))) %>%
  select(CDSS_UID, county, month) %>%
  distinct()

# 2016 demographics panel

df16_demo = ssi16 %>%
  full_join(snap16) %>%
  select(CDSS_UID, HOUSEHOLD, DOB, LANGUAGE, RACE, SEX) %>%
  mutate(age = 2016-as.numeric(str_sub(DOB,1,4))) %>%
  select(CDSS_UID, HOUSEHOLD, age, LANGUAGE, RACE, SEX)

# 2016 panel

df16 = df16_demo %>%
  full_join(df16_ct) %>%
  full_join(df16_snap) %>%
  mutate(year = 2016)





# 2017 snap participation panel

df17_snap = ssi17 %>%
  full_join(snap17) %>%
  gather(period, snap, FAF17_01:FAF17_12) %>%
  select(CDSS_UID, period, snap) %>%
  distinct() %>%
  mutate(snap = replace_na(snap,0),
         month = as.numeric(str_sub(period,-2,-1))) %>%
  select(CDSS_UID, snap, month) %>%
  distinct()

# 2017 county panel

df17_ct = ssi17 %>%
  full_join(snap17) %>%
  gather(period, county, CC1701:CC1712) %>%
  select(CDSS_UID, period, county) %>%
  distinct() %>%
  mutate(county = replace_na(county,0),
         month = as.numeric(str_sub(period,-2,-1))) %>%
  select(CDSS_UID, county, month) %>%
  distinct()

# 2017 demographics panel

df17_demo = ssi17 %>%
  full_join(snap17) %>%
  select(CDSS_UID, HOUSEHOLD, DOB, LANGUAGE, RACE, SEX) %>%
  mutate(age = 2017-as.numeric(str_sub(DOB,1,4))) %>%
  select(CDSS_UID, HOUSEHOLD, age, LANGUAGE, RACE, SEX)

# 2017 panel

df17 = df17_demo %>%
  full_join(df17_ct) %>%
  full_join(df17_snap) %>%
  mutate(year = 2017)





# 2018 snap participation panel

df18_snap = ssi18 %>%
  full_join(snap18) %>%
  gather(period, snap, FAF18_01:FAF18_12) %>%
  select(CDSS_UID, period, snap) %>%
  distinct() %>%
  mutate(snap = replace_na(snap,0),
         month = as.numeric(str_sub(period,-2,-1))) %>%
  select(CDSS_UID, snap, month) %>%
  distinct()

# 2018 county panel

df18_ct = ssi18 %>%
  full_join(snap18) %>%
  gather(period, county, CC1801:CC1812) %>%
  select(CDSS_UID, period, county) %>%
  distinct() %>%
  mutate(county = replace_na(county,0),
         month = as.numeric(str_sub(period,-2,-1))) %>%
  select(CDSS_UID, county, month) %>%
  distinct()

# 2018 demographics panel

df18_demo = ssi18 %>%
  full_join(snap18) %>%
  select(CDSS_UID, HOUSEHOLD, DOB, LANGUAGE, RACE, SEX) %>%
  mutate(age = 2018-as.numeric(str_sub(DOB,1,4))) %>%
  select(CDSS_UID, HOUSEHOLD, age, LANGUAGE, RACE, SEX)

# 2018 panel

df18 = df18_demo %>%
  full_join(df18_ct) %>%
  full_join(df18_snap) %>%
  mutate(year = 2018)





# 2019 snap participation panel

df19_snap = ssi19 %>%
  full_join(snap19) %>%
  gather(period, snap, FAF19_01:FAF19_12) %>%
  select(CDSS_UID, period, snap) %>%
  distinct() %>%
  mutate(snap = replace_na(snap,0),
         month = as.numeric(str_sub(period,-2,-1))) %>%
  select(CDSS_UID, snap, month) %>%
  distinct()

# 2019 county panel

df19_ct = ssi19 %>%
  full_join(snap19) %>%
  gather(period, county, CC1901:CC1912) %>%
  select(CDSS_UID, period, county) %>%
  distinct() %>%
  mutate(county = replace_na(county,0),
         month = as.numeric(str_sub(period,-2,-1))) %>%
  select(CDSS_UID, county, month) %>%
  distinct()

# 2019 demographics panel

df19_demo = ssi19 %>%
  full_join(snap19) %>%
  select(CDSS_UID, HOUSEHOLD, DOB, LANGUAGE, RACE, SEX) %>%
  mutate(age = 2019-as.numeric(str_sub(DOB,1,4))) %>%
  select(CDSS_UID, HOUSEHOLD, age, LANGUAGE, RACE, SEX)

# 2019 panel

df19 = df19_demo %>%
  full_join(df19_ct) %>%
  full_join(df19_snap) %>%
  mutate(year = 2019)





# 2020 snap participation panel

df20_snap = ssi20 %>%
  full_join(snap20) %>%
  gather(period, snap, FAF20_01:FAF20_12) %>%
  select(CDSS_UID, period, snap) %>%
  distinct() %>%
  mutate(snap = replace_na(snap,0),
         month = as.numeric(str_sub(period,-2,-1))) %>%
  select(CDSS_UID, snap, month) %>%
  distinct()

# 2020 county panel

df20_ct = ssi20 %>%
  full_join(snap20) %>%
  gather(period, county, CC2001:CC2012) %>%
  select(CDSS_UID, period, county) %>%
  distinct() %>%
  mutate(county = replace_na(county,0),
         month = as.numeric(str_sub(period,-2,-1))) %>%
  select(CDSS_UID, county, month) %>%
  distinct()

# 2020 demographics panel

df20_demo = ssi20 %>%
  full_join(snap20) %>%
  select(CDSS_UID, HOUSEHOLD, DOB, LANGUAGE, RACE, SEX) %>%
  mutate(age = 2020-as.numeric(str_sub(DOB,1,4))) %>%
  select(CDSS_UID, HOUSEHOLD, age, LANGUAGE, RACE, SEX)

# 2020 panel

df20 = df20_demo %>%
  full_join(df20_ct) %>%
  full_join(df20_snap) %>%
  mutate(year = 2020)




# multiyear panel

df = rbind(df14, df15, df16, df17, df18, df19, df20)

table(df$month, df$year, df$snap)
table(df$HOUSEHOLD[df$snap==1 & df$year<2019])

df_snap = df %>%
  group_by(month,year,county) %>%
  summarise(count = n(),
            perc_snap = sum(snap==1 & ssi==1)/count) %>%
  mutate(time = make_date(month = month, year = year)) %>%
  select(time, county, perc_snap)
df_snap$county = as.factor(df_snap$county)

ggplot(df_snap, aes(time,perc_snap, colour = county)) +
  geom_point()

rm(list= ls()[!(ls() %in% c('df','county_codes'))])

# linkage file
link <- read_csv("C:/LIFESPAN_D/CDSS files/dhcs_calfresh_eats_linkage.csv",
                 col_select = c("RECORD1_ID1",
                                "RECORD2_ID1"))

# medical claims

#2015

dm15 <- read_csv("C:/LIFESPAN_D/Aaron_Baum/Data_Slices/DMT2_SSI_ICD10_2015_AO_17SEP2021.csv") %>%
  distinct()
inpt15 <- read_csv("C:/LIFESPAN_D/Aaron_Baum/Data_Slices/inpatient_SSI_CLAIMS_2015_AO_17SEP2021.csv")




#2016
clm16 <- read_csv("C:/LIFESPAN_D/DHCS/SSI_CLAIMS_2016_AO_17SEP2021.CSV",
                  col_select = c("ENCRYPTED_AKA_CIN",
                                 "SVC_FROM_DT",
                                 "CLAIM_TYPE_CD",
                                 "SEC_DIAG_CD",
                                 "PROC_CD")) %>%
  distinct()

#2017

clm17 <- read_csv("C:/LIFESPAN_D/DHCS/SSI_CLAIMS_2017_AO_17SEP2021.CSV",
                  col_select = c("ENCRYPTED_AKA_CIN",
                                 "SVC_FROM_DT",
                                 "CLAIM_TYPE_CD",
                                 "SEC_DIAG_CD",
                                 "PROC_CD")) %>%
  distinct()

#2018

clm18 <- read_csv("C:/LIFESPAN_D/DHCS/SSI_CLAIMS_2018_AO_17SEP2021.CSV",
                  col_select = c("ENCRYPTED_AKA_CIN",
                                 "SVC_FROM_DT",
                                 "CLAIM_TYPE_CD",
                                 "SEC_DIAG_CD",
                                 "PROC_CD")) %>%
  distinct()

#2019

clm19 <- read_csv("C:/LIFESPAN_D/DHCS/SSI_CLAIMS_2019_AO_17SEP2021.CSV",
                  col_select = c("ENCRYPTED_AKA_CIN",
                                 "SVC_FROM_DT",
                                 "CLAIM_TYPE_CD",
                                 "SEC_DIAG_CD",
                                 "PROC_CD")) %>%
  distinct()

#2020

clm20 <- read_csv("C:/LIFESPAN_D/DHCS/SSI_CLAIMS_2020_AO_17SEP2021.CSV",
                  col_select = c("ENCRYPTED_AKA_CIN",
                                 "SVC_FROM_DT",
                                 "CLAIM_TYPE_CD",
                                 "SEC_DIAG_CD",
                                 "PROC_CD")) %>%
  distinct()








