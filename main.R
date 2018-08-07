library(dplyr) 
library(plyr)
library(reshape2)
library(tidyr)

options("digits" = 12)
source('function.R')

# A: the data structure is different from census 2016
#   1. age pyramid: in census2016, there is summary data for 0-15, 15-65, 65+, 85+, but no such summary in census 2011
#   2. household type:in census2016, there are one-census fml, multi-census fml, non-census fml. In census2011, there are census fml and non-census fml
#   3. low-income: a. in census2016, we choose the low income based on after-tax low-income cut-offs(LICOs)and its prevalence. But these two measures are 
#      not in census2011. which we choose low-income based on after-tax low income measure (LIM-AT)
# B: in census 2011, there are two survey: 2011 census (age, household,Language); 2011NHS (Immi, Edu, Labor, Commu, Housing, Income)

# Quebec 2016 data, at CLSC level -----------------------------------------
#### read in the datasets obtained from census 2016, at four different region devisions ####
# df_combine in function.R
cd.data.2011 = df_combine("Data/census_data_2011/QC/CAbUjP1ekjo_data_CD.csv", 
                     "Data/census_data_2011/QC/CAbUjP1ekjo_header_CD.txt",
                     "Data/census_data_2011/QC/CGI4PTqJiJaO6G_data_CD.csv",
                     "Data/census_data_2011/QC/CGI4PTqJiJaO6G_header_CD.txt",
                     "Data/census_data_2011/QC/J9e74ar8snwCz_data_CD.csv",
                     "Data/census_data_2011/QC/J9e74ar8snwCz_header_CD.txt")

csd.data.2011 = df_combine("Data/census_data_2011/QC/h9UaJOB67yLyx_data_CSD.csv",
                      "Data/census_data_2011/QC/h9UaJOB67yLyx_header_CSD.txt",
                      "Data/census_data_2011/QC/knyLvT8TBNpfhlKdfwaT_data_CSD.csv",
                      "Data/census_data_2011/QC/knyLvT8TBNpfhlKdfwaT_header_CSD.txt",
                      "Data/census_data_2011/QC/BbsilpAzrr0jIj_data_CSD.csv",
                      "Data/census_data_2011/QC/BbsilpAzrr0jIj_header_CSD.txt")

ct.data.2011 = df_combine("Data/census_data_2011/QC/bhJdinbwGRl9_data_CT.csv",
                     "Data/census_data_2011/QC/bhJdinbwGRl9_header_CT.txt",
                     "Data/census_data_2011/QC/Bxz8przOfwtQM_data_CT.csv",
                     "Data/census_data_2011/QC/Bxz8przOfwtQM_header_CT.txt",
                     "Data/census_data_2011/QC/LHlui9jp9ipo_data_NHS_Gatineau_CT.csv",
                     "Data/census_data_2011/QC/LHlui9jp9ipo_header_NHS_Gitineau_CT.txt",
                     "Data/census_data_2011/QC/tcet77319IeUj8x_data_Gatineau_CT.csv",
                     "Data/census_data_2011/QC/tcet77319IeUj8x_header_Gatineau_CT.txt")


da.data.2011 = df_combine("Data/census_data_2011/QC/yP2elduJ_data_DA.csv",
                     "Data/census_data_2011/QC/yP2elduJ_header_DA.txt",
                     "Data/census_data_2011/QC/zLEeT0sUjb_data_DA.csv",
                     "Data/census_data_2011/QC/zLEeT0sUjb_header_DA.txt")

da.data.2011 <- da.data.2011[da.data.2011$census_id > 2500, ]

cd.data.2011 = cd.data.2011[, order(names(cd.data.2011))]
csd.data.2011 = csd.data.2011[, order(names(csd.data.2011))]
ct.data.2011 = ct.data.2011[, order(names(ct.data.2011))]
da.data.2011 = da.data.2011[, order(names(da.data.2011))]
da.names = names(da.data.2011)
da.names = gsub(pattern = "certificate", 
                replacement =  "certificate,",
                x = da.names)
da.names = gsub(pattern = "Car", 
                     replacement =  "Car,",
                     x = da.names)
names(da.data.2011) = da.names

census.data.2011 = do.call("rbind", list(cd.data.2011, csd.data.2011, ct.data.2011, da.data.2011))
rm(cd.data.2011);rm(csd.data.2011);rm(ct.data.2011);rm(da.data.2011)

#### map to health region: CLSC ####
map.table.clsc = read.csv("Data/census_data_2016/mapping_tables/clsc_census_mapping_gd.csv",header = TRUE) # mapping table btw census and CLSC, table obtained from Guido
map.clsc.2011 = merge(map.table.clsc, census.data.2011, by = "census_id")
map.clsc.2011= map.clsc.2011[, !names(map.clsc.2011) %in% c("census_id", "census_type")]
rm(map.table.clsc)

clsc.data.2011 = sum_by_key(map.clsc.2011, colnames(map.clsc.2011)[1]) # sum_by_key in function.R
clsc.data.2011$year = 2011
rm(map.clsc.2011)

#### extract nine indicators for pophr loader ####
clsc.values.2011 = ext_concept(clsc.data.2011, "CLSC_code")
clsc.values.2011 = lapply(clsc.values.2011, ChangeNames) #from 2cols, ChangeNames to "Nom", "Den", "Year"

#### extract age pyramid ####
clsc.pop.values.2011 = ext_pop_number(clsc.data.2011, "CLSC_code")
clsc.pop.values.2011 = lapply(list(clsc.pop.values.2011), ChangeNames_pop)#from 2cols, ChangeNames to "0-14","15-64","65+","Year"

clsc.age.pry.2011 = ext_age_pry(clsc.data.2011, "CLSC_code")
clsc.age.pry.2011$Year = 2011

clsc.values.2011 = c(clsc.values.2011, clsc.pop.values = clsc.pop.values.2011, clsc.age.pry = list(clsc.age.pry.2011))
rm(clsc.pop.values.2011) ; rm(clsc.age.pry.2011)
#### output results ####
writecsv(clsc.values.2011, "CLSC_2011")


# Canada 2011 data, at province level, no mapping needed -------------------------------------

#### read in the datasets obtained from census 2016, at provincial level ####
ca.data.2011 = df_combine("Data/census_data_2011/Canada_wise_data_by_prov/iBNs0aHr8BGB_data.csv",
                     "Data/census_data_2011/Canada_wise_data_by_prov/iBNs0aHr8BGB_header.txt",
                     "Data/census_data_2011/Canada_wise_data_by_prov/u96UkAd2GnV_data.csv",
                     "Data/census_data_2011/Canada_wise_data_by_prov/u96UkAd2GnV_header.txt")

ca.data.2011$year = 2011

#### extract indicators for pophr loader ####
ca.values.2011 = ext_concept(ca.data.2011, names(ca.data.2011)[1])
ca.values.2011 = lapply(ca.values.2011, ChangeNames) #from 2cols, ChangeNames to "Nom", "Den", "Year"

#### extract age pyramid ####
ca.pop.values.2011 = ext_pop_number(ca.data.2011, names(ca.data.2011)[1])
ca.pop.values.2011 = lapply(list(ca.pop.values.2011), ChangeNames_pop)#from 2cols, ChangeNames to "0-14","15-64","65+","Year"

ca.age.pry.2011 = ext_age_pry(ca.data.2011, names(ca.data.2011)[1])
ca.age.pry.2011$Year = 2011

ca.values.2011 = c(ca.values.2011, ca.pop.values = ca.pop.values.2011, ca.age.pry = list(ca.age.pry.2011))
rm(ca.pop.values.2011) ; rm(ca.age.pry.2011)
#### output results ####
writecsv(ca.values.2011, "CA_Prov_2011") # the file name contains the first part of the first col in list 1 of clsc.values

