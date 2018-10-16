library(dplyr) 
library(plyr)
library(reshape2)
library(tidyr)

options("digits" = 12)
source('function.R')

# Quebec 2016 data, at CLSC level -----------------------------------------

#### read in the datasets obtained from census 2016, at four different region devisions ####
# df_combine in function.R
cd.data = df_combine("Data/census_data_2016/nFzbbLDEVuPz11pLQ_data_CD.csv", 
                     "Data/census_data_2016/nFzbbLDEVuPz11pLQ_data_CD.txt",
                     "Data/census_data_2016/income_cutof_pre/bC08v9cE6LS_data_CD.csv",
                     "Data/census_data_2016/income_cutof_pre/bC08v9cE6LS_header_CD.txt",
                     "Data/census_data_2016/total_population/fi3xBCGF33oxEm_data_CD.csv",
                     "Data/census_data_2016/total_population/fi3xBCGF33oxEm_data_CD.txt",
                     "Data/census_data_2016/immigration, family structure, housing/Q71kmE8NAyfgoH_data_CD.csv",
                     "Data/census_data_2016/immigration, family structure, housing/Q71kmE8NAyfgoH_header_CD.txt")
cd.data = cd.data[, -c(2:5)]

csd.data = df_combine("Data/census_data_2016/E8v0huQ0frN4m_data_CSD.csv",
                      "Data/census_data_2016/E8v0huQ0frN4m_data_CSD.txt",
                      "Data/census_data_2016/income_cutof_pre/80uJhuNcMUij8uaNrC_data_CSD.csv",
                      "Data/census_data_2016/income_cutof_pre/80uJhuNcMUij8uaNrC_header_CSD.txt",
                      "Data/census_data_2016/total_population/k2Tb5LTN4ON_data_CSD.csv",
                      "Data/census_data_2016/total_population/k2Tb5LTN4ON_data_CSD.txt",
                      "Data/census_data_2016/immigration, family structure, housing/wURdDj13eJ_data_CSD.csv",
                      "Data/census_data_2016/immigration, family structure, housing/wURdDj13eJ_header_CSD.txt")
csd.data = csd.data[, -c(2:5)]

ct.data = df_combine("Data/census_data_2016/G8iUTCA6zplcndk_data_CT.csv",
                     "Data/census_data_2016/G8iUTCA6zplcndk_data_CT.txt",
                     "Data/census_data_2016/income_cutof_pre/sNfIvcAf6rLcz_data_CT.csv",
                     "Data/census_data_2016/income_cutof_pre/sNfIvcAf6rLcz_header_CT.txt",
                     "Data/census_data_2016/total_population/swC9HfRqADoe_data_CT.csv",
                     "Data/census_data_2016/total_population/swC9HfRqADoe_data_CT.txt",
                     "Data/census_data_2016/immigration, family structure, housing/b7pvli7FLB43i_data_CT.csv",
                     "Data/census_data_2016/immigration, family structure, housing/b7pvli7FLB43i_header_CT.txt")
ct.data = ct.data[, -c(2:4)]

da.data = df_combine("Data/census_data_2016/zngeryB53_data_DA.csv",
                     "Data/census_data_2016/zngeryB53_data_DA.txt",
                     "Data/census_data_2016/income_cutof_pre/qO6fozE6eB_data_DA.csv",
                     "Data/census_data_2016/income_cutof_pre/qO6fozE6eB_header_DA.txt",
                     "Data/census_data_2016/total_population/ErO0Imol3vrRjoi_data_DA.csv",
                     "Data/census_data_2016/total_population/ErO0Imol3vrRjoi_data_DA.txt",
                     "Data/census_data_2016/immigration, family structure, housing/6IOEpi8povq3vI66_data_DA.csv",
                     "Data/census_data_2016/immigration, family structure, housing/6IOEpi8povq3vI66_header_DA.txt")
da.data <- da.data[da.data$census_id > 2500, ]
da.data = da.data[, -c(2:4)]

census.data = do.call("rbind", list(cd.data, csd.data, ct.data, da.data))
rm(cd.data);rm(csd.data);rm(ct.data);rm(da.data)

#### map to health region: CLSC ####
map.table.clsc = read.csv("Data/census_data_2016/mapping_tables/clsc_census_mapping_gd.csv",header = TRUE) # mapping table btw census and CLSC, table obtained from Guido
map.clsc = merge(map.table.clsc, census.data, by = "census_id")
map.clsc= map.clsc[, !names(map.clsc) %in% c("census_id", "census_type")]
rm(map.table.clsc)

clsc.data = sum_by_key(map.clsc, colnames(map.clsc)[1]) # sum_by_key in function.R
clsc.data$year = 2016
rm(map.clsc)

#### extract nine indicators for pophr loader ####
clsc.values = ext_concept(clsc.data, "CLSC_code")
clsc.values = lapply(clsc.values, ChangeNames) #from 2cols, ChangeNames to "Nom", "Den", "Year"

#### extract age pyramid ####
clsc.pop.values = ext_pop_number(clsc.data, "CLSC_code")
clsc.pop.values = lapply(list(clsc.pop.values), ChangeNames_pop)#from 2cols, ChangeNames to "0-14","15-64","65+","Year"

clsc.age.pry = ext_age_pry(clsc.data, "CLSC_code")
clsc.age.pry$Year = 2016

clsc.values = c(clsc.values, clsc.pop.values = clsc.pop.values, clsc.age.pry = list(clsc.age.pry))
rm(clsc.pop.values) ; rm(clsc.age.pry)
#### output results ####
writecsv(clsc.values, "CLSC_2016") # the file name contains the first part of the first col in list 1 of clsc.values




# Canada 2016 data, at province level, no mapping needed -------------------------------------

#### read in the datasets obtained from census 2016, at provincial level ####
ca.data = df_combine("Data/census_data_2016/Canada_wise_data_by_prov/vtapChnNRqQopJ_data.csv",
                     "Data/census_data_2016/Canada_wise_data_by_prov/vtapChnNRqQopJ_header.txt",
                     "Data/census_data_2016/Canada_wise_data_by_prov/StozuELDohF9jp_data.csv",
                     "Data/census_data_2016/Canada_wise_data_by_prov/StozuELDohF9jp_header.txt")

ca.data$year = 2016

#### extract indicators for pophr loader ####
ca.values = ext_concept(ca.data, names(ca.data)[1])
ca.values = lapply(ca.values, ChangeNames) #from 2cols, ChangeNames to "Nom", "Den", "Year"

#### extract age pyramid ####
ca.pop.values = ext_pop_number(ca.data, names(ca.data)[1])
ca.pop.values = lapply(list(ca.pop.values), ChangeNames_pop)#from 2cols, ChangeNames to "0-14","15-64","65+","Year"

ca.age.pry = ext_age_pry(ca.data, names(ca.data)[1])
ca.age.pry$Year = 2016

ca.values = c(ca.values, ca.pop.values = ca.pop.values, ca.age.pry = list(ca.age.pry))
rm(ca.pop.values) ; rm(ca.age.pry)
#### output results ####
writecsv(ca.values, "CA_Prov_2016") # the file name contains the first part of the first col in list 1 of clsc.values

# Canada 2016 data, at health region level, mapping needed -------------------------------------
ca.hr.csd.data.2016 = df_combine("Data/census_data_2016/Canada_wise_data_by_hr/JGIgKwt06AB5T9h_data_CSD.csv", 
                                 "Data/census_data_2016/Canada_wise_data_by_hr/JGIgKwt06AB5T9h_data_CSD.txt")

ca.hr.da.data.2016 = df_combine("Data/census_data_2016/Canada_wise_data_by_hr/bgQq89z8aLC_data_DA.csv", 
                                "Data/census_data_2016/Canada_wise_data_by_hr/bgQq89z8aLC_data_DA.txt")

ca.hr.da.data.2016 <- ca.hr.da.data.2016[ca.hr.da.data.2016$census_id > 2500, ]

ca.hr.csd.data.2016 = ca.hr.csd.data.2016[, order(names(ca.hr.csd.data.2016))]
ca.hr.da.data.2016 = ca.hr.da.data.2016[, order(names(ca.hr.da.data.2016))]

ca.hr.census.data.2016 = do.call("rbind", list(ca.hr.csd.data.2016, ca.hr.da.data.2016))
rm(ca.hr.da.data.2016);rm(ca.hr.csd.data.2016)

#### map to health region ####
map.table.hr = read.csv("Data/census_data_2016/Canada_wise_data_by_hr/map.table.hr.2016.census.csv",header = TRUE) # mapping table btw census and CLSC, table obtained from Guido
map.hr = merge(map.table.hr, ca.hr.census.data.2016, by = "census_id")
map.hr= map.hr[, !names(map.hr) %in% c("census_id", "census_type")]
rm(map.table.hr)

hr.data.2016 = sum_by_key(map.hr, colnames(map.hr)[1]) # sum_by_key in function.R
hr.data.2016$year = 2016
rm(map.hr)

#### extract nine indicators for pophr loader ####
hr.values.2016 = ext_concept(hr.data.2016, "HR_code")
hr.values.2016 = lapply(hr.values.2016, ChangeNames) #from 2cols, ChangeNames to "Nom", "Den", "Year"

#### extract age pyramid ####
hr.pop.values.2016 = ext_pop_number(hr.data.2016, "HR_code")
hr.pop.values.2016 = lapply(list(hr.pop.values.2016), ChangeNames_pop)#from 2cols, ChangeNames to "0-14","15-64","65+","Year"

hr.age.pry.2016 = ext_age_pry(hr.data.2016, "HR_code")
hr.age.pry.2016$Year = 2016

hr.values.2016 = c(hr.values.2016, hr.pop.values = hr.pop.values.2016, hr.age.pry = list(hr.age.pry.2016))
rm(hr.pop.values.2016) ; rm(hr.age.pry.2016)
#### output results ####
writecsv(hr.values.2016, "HR_2016") # the file name contains the first part of the first col in list 1 of clsc.values
