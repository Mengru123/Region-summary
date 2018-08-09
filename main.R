library(dplyr) 
library(plyr)
library(reshape2)
library(tidyr)

options("digits" = 12)
source('function.R')
# difference
# 1. income: in 2011 not sure about whether it's LIM or LICO
# 2. no low_income_0_17, but low-income_0_6
# 3. no direct count for low-income population number, but have total number and prevalence, so in script calculate the numerator
# 4. education here need to added up in the script 15-24,25-65, 65+

# Quebec 2006 data, at CLSC level -----------------------------------------

#### read in the datasets obtained from census 2016, at four different region devisions ####
# df_combine in function.R
cd.data.2006 = df_combine("Data/census_data_2006/census_2006_CD.csv", 
                          "Data/census_data_2006/census_2006_CD.txt")

csd.data.2006 = df_combine("Data/census_data_2006/census_2006_CSD.csv", 
                         "Data/census_data_2006/census_2006_CSD.txt")

ct.data.2006 = df_combine("Data/census_data_2006/U4pAzOO9PHIoycFC_data_CT.csv",
                     "Data/census_data_2006/U4pAzOO9PHIoycFC_header_CT.txt",
<<<<<<< HEAD
                     "Data/census_data_2006/cpgDbfPyr0C_data_Gatineau_CT.csv",
                     "Data/census_data_2006/cpgDbfPyr0C_header_Gatineau_CT.txt")
=======
                     "Data/census_data_2006/cpgDbfPyr0C_data_Ganibeau_CT.csv",
                     "Data/census_data_2006/cpgDbfPyr0C_header_Ganibeau_CT.txt")
>>>>>>> 702ab8e857dae0bb7a8ccf7e1a76df5a05409ead

names(ct.data.2006)[1] = "census_id"

da.data.2006 = df_combine("Data/census_data_2006/cGcSMP3oFgv_data_DA.csv",
                     "Data/census_data_2006/cGcSMP3oFgv_header_DA.txt")
da.data.2006 = da.data.2006[, -1]
names(da.data.2006)[1] = "census_id"
da.data.2006 <- da.data.2006[da.data.2006$census_id > 2500, ]

cd.data.2006 = cd.data.2006[, 1:94]
csd.data.2006 = csd.data.2006[, 1:94]
ct.data.2006 = ct.data.2006[, 1:94]
da.data.2006 = da.data.2006[, 1:94]

col.names = names(cd.data.2006)
names(csd.data.2006) = col.names
names(ct.data.2006) = col.names
names(da.data.2006) = col.names

census.data.2006 = do.call("rbind", list(cd.data.2006, csd.data.2006, ct.data.2006, da.data.2006))
rm(cd.data.2006);rm(csd.data.2006);rm(ct.data.2006);rm(da.data.2006)

#### map to health region: CLSC ####
map.table.clsc = read.csv("Data/census_data_2006/clsc_census_mapping2006.csv",header = TRUE) # mapping table btw census and CLSC, table obtained from Guido
map.clsc = merge(map.table.clsc, census.data.2006, by = "census_id")
map.clsc= map.clsc[, !names(map.clsc) %in% c("census_id", "census_type")]
rm(map.table.clsc)

clsc.data.2006 = sum_by_key(map.clsc, colnames(map.clsc)[1]) # sum_by_key in function.R
clsc.data.2006$year = 2006
rm(map.clsc)

#### extract nine indicators for pophr loader ####
clsc.values.2006 = ext_concept(clsc.data.2006, "CLSC_code")
clsc.values.2006 = lapply(clsc.values.2006, ChangeNames) #from 2cols, ChangeNames to "Nom", "Den", "Year"

#### extract age pyramid ####
clsc.pop.values.2006 = ext_pop_number(clsc.data.2006, "CLSC_code")
clsc.pop.values.2006 = lapply(list(clsc.pop.values.2006), ChangeNames_pop)#from 2cols, ChangeNames to "0-14","15-64","65+","Year"

clsc.age.pry.2006 = ext_age_pry(clsc.data.2006, "CLSC_code")
clsc.age.pry.2006$Year = 2006

clsc.values.2006 = c(clsc.values.2006, clsc.pop.values = clsc.pop.values.2006, clsc.age.pry = list(clsc.age.pry.2006))
rm(clsc.pop.values.2006) ; rm(clsc.age.pry.2006)
#### output results ####
writecsv(clsc.values.2006, "CLSC_2006") # the file name contains the first part of the first col in list 1 of clsc.values




# Canada 2016 data, at province level, no mapping needed -------------------------------------

#### read in the datasets obtained from census 2016, at provincial level ####
ca.data.2006 = df_combine("Data/census_data_2006/Canada_wise_data_by_prov/retrieveCensusPROV_CA.csv",
                     "Data/census_data_2006/Canada_wise_data_by_prov/retrieveCensusPROV_CA.txt")

ca.data.2006$year = 2006
ca.data.2006 = ca.data.2006[, -c(8,28)]

#### extract indicators for pophr loader ####
ca.values.2006 = ext_concept(ca.data.2006, names(ca.data.2006)[1])
ca.values.2006 = lapply(ca.values.2006, ChangeNames) #from 2cols, ChangeNames to "Nom", "Den", "Year"

#### extract age pyramid ####
ca.pop.values.2006 = ext_pop_number(ca.data.2006, names(ca.data.2006)[1])
ca.pop.values.2006 = lapply(list(ca.pop.values.2006), ChangeNames_pop)#from 2cols, ChangeNames to "0-14","15-64","65+","Year"

ca.age.pry.2006 = ext_age_pry(ca.data.2006, names(ca.data.2006)[1])
ca.age.pry.2006$Year = 2006

ca.values.2006 = c(ca.values.2006, ca.pop.values = ca.pop.values.2006, ca.age.pry = list(ca.age.pry.2006))
rm(ca.pop.values.2006) ; rm(ca.age.pry.2006)
#### output results ####
writecsv(ca.values.2006, "CA_Prov_2006") # the file name contains the first part of the first col in list 1 of clsc.values

