## Generate indicators for the region summary view, given the mapping file provided by Guido ####
library(dplyr) # pipe
library(plyr)
library(reshape2)
library(ggplot2)
library(tidyr)
library(gridExtra) #for ggplot that can display multiple plot together
library(tmap) # use for mapping
library(rgdal) # use for mapping
options("digits" = 12)

# read tables and prepare the one census.data  table containing raw number at 4 different geo divisions (cd,csd,ct,da) ####
setwd("C:/Users/myuan/Google Drive/1. Practicum/3. Notes/10. Region summary data/Inidcators")
#setwd("D:/Google Drive_MR/1. Practicum/3. Notes/10. Region summary data/Inidcators") # home computer
cd.data <-read.csv("census_data_2016/nFzbbLDEVuPz11pLQ_data_CD.csv", header = TRUE) # census data at CD level
cd.in <-read.csv("census_data_2016/income_cutof_pre/BGjxddMe_data_CD.csv",header = TRUE) # low-income cut-off prevalence
cd.totalpop <-read.csv("census_data_2016/total_population/fi3xBCGF33oxEm_data_CD.csv",header = TRUE) # totalpopulation
colnames(cd.totalpop) <-c("COL0", "totalpop", "total_0_14",  "total_15_64", "total_65+")
cd.data <- cd.data[, -c(2:5)]
cd.data$type <- "CD"
cd.data <-join_all(list(cd.data, cd.in, cd.totalpop),by = "COL0",type = 'full')
rm(cd.in);rm(cd.totalpop)

csd.data <- read.csv("census_data_2016/E8v0huQ0frN4m_data_CSD.csv", header = TRUE)
csd.data <- csd.data[, -c(2:5)]
csd.data$type <- "CSD"
csd.in <-read.csv("census_data_2016/income_cutof_pre/MtTcrBvBmKMcMe_data_CSD.csv",header = TRUE) # low-income cut-off prevalence
csd.totalpop <-read.csv("census_data_2016/total_population/k2Tb5LTN4ON_data_CSD.csv",header = TRUE) # totalpopulation
colnames(csd.totalpop) <- c("COL0", "totalpop", "total_0_14",  "total_15_64", "total_65+")
csd.data <-join_all(list(csd.data, csd.in, csd.totalpop),by = "COL0",type = 'full')
rm(csd.in);rm(csd.totalpop)

ct.data <- read.csv("census_data_2016/G8iUTCA6zplcndk_data_CT.csv", header = TRUE)
ct.data <- ct.data[, -c(2:4)]
ct.data$type <- "CT"
ct.in <-read.csv("census_data_2016/income_cutof_pre/H3yaFrFVGjHuc_data_CT.csv",header = TRUE) # low-income cut-off prevalence
ct.totalpop <-read.csv("census_data_2016/total_population/swC9HfRqADoe_data_CT.csv",header = TRUE) # totalpopulation
colnames(ct.totalpop) <-c("COL0", "totalpop", "total_0_14",  "total_15_64", "total_65+")
ct.data <- join_all(list(ct.data, ct.in, ct.totalpop),by = "COL0",type = 'full')
rm(ct.in);rm(ct.totalpop)

da.data <- read.csv("census_data_2016/zngeryB53_data_DA.csv", header = TRUE)
da.data <- da.data[, -c(2:4)]
da.data$type <- "DA"
da.in <-read.csv("census_data_2016/income_cutof_pre/ULkBM9b67fy_data_DA.csv",header = TRUE) # low-income cut-off prevalence
da.totalpop <-read.csv("census_data_2016/total_population/ErO0Imol3vrRjoi_data_DA.csv",header = TRUE) # totalpopulation
colnames(da.totalpop) <-c("COL0", "totalpop", "total_0_14",  "total_15_64", "total_65+")
da.data <-join_all(list(da.data, da.in, da.totalpop),by = "COL0",type = 'full')
rm(da.in);rm(da.totalpop)
da.data <- da.data[da.data$COL0 > 2500, ]

qc.data<-da.in <-read.csv("census_data_2016/Quebec_data/aE9kmuQQhjhs_data_QC.csv",header = TRUE) # data for quebec


col.names <-c("census_id","Male_total","Male_0_14","Male_0_4", "Male_5_9","Male_10_14","Male_15_64","Male_15_19",
              "Male_20_24","Male_25_29", "Male_30_34","Male_35_39","Male_40_44","Male_45_49","Male_50_54","Male_55_59",
              "Male_60_64","Male_65+","Male_65_69","Male_70_74","Male_75_79","Male_80_84","Male_85+","Male_85_89",
              "Male_90_94","Male_95_99","Male_100+","Male_Ave_age","Female_total","Female_0_14","Female_0_4","Female_5_9",
              "Female_10_14","Female_15_64","Female_15_19","Female_20_24","Female_25_29","Female_30_34","Female_35_39",
              "Female_40_44","Female_45_49","Female_50_54","Female_55_59","Female_60_64","Female_65+","Female_65_69",
              "Female_70_74","Female_75_79","Female_80_84","Female_85+","Female_85_89","Female_90_94","Female_95_99",
              "Female_100+","Female_Ave_age","lang_total","lang_en_only","lang_fr_only","lang_en_fr","lang_nen_nfr",
              "lowincome_total","lowincome_0_17","Edu_total","Edu_lowhighs","Edu_highsch","Edu_postse","Labor_total",
              "Labor_inforced","Labor_employed","Labor_unemployed","Labor_notinfor","comnu_total","com_bycardriver",
              "com_bycarpassenger","com_ptransit","com_walk","com_bicycle","com_other","census_type","lowincome_prevalence_total",
              "lowincome_prevalence_0_17","totalpopulation","totalpop_0_14","totalpop_15_64","totalpop_65+")


col.nameqc<-c("qc_id","Male_total","Male_0_14","Male_0_4", "Male_5_9","Male_10_14","Male_15_64","Male_15_19",
              "Male_20_24","Male_25_29", "Male_30_34","Male_35_39","Male_40_44","Male_45_49","Male_50_54","Male_55_59",
              "Male_60_64","Male_65+","Male_65_69","Male_70_74","Male_75_79","Male_80_84","Male_85+","Male_85_89",
              "Male_90_94","Male_95_99","Male_100+","Male_Ave_age","Female_total","Female_0_14","Female_0_4","Female_5_9",
              "Female_10_14","Female_15_64","Female_15_19","Female_20_24","Female_25_29","Female_30_34","Female_35_39",
              "Female_40_44","Female_45_49","Female_50_54","Female_55_59","Female_60_64","Female_65+","Female_65_69",
              "Female_70_74","Female_75_79","Female_80_84","Female_85+","Female_85_89","Female_90_94","Female_95_99",
              "Female_100+","Female_Ave_age","lang_total","lang_en_only","lang_fr_only","lang_en_fr","lang_nen_nfr",
              "lowincome_total","lowincome_0_17","Edu_total","Edu_lowhighs","Edu_highsch","Edu_postse","Labor_total",
              "Labor_inforced","Labor_employed","Labor_unemployed","Labor_notinfor","comnu_total","com_bycardriver",
              "com_bycarpassenger","com_ptransit","com_walk","com_bicycle","com_other","lowincome_prevalence_total",
              "lowincome_prevalence_0_17","totalpopulation","totalpop_0_14","totalpop_15_64","totalpop_65+")

colnames(cd.data) <-col.names ;colnames(csd.data) <-col.names
colnames(ct.data) <- col.names;colnames(da.data) <- col.names
census.data <- do.call("rbind", list(cd.data, csd.data, ct.data, da.data))
rm(cd.data);rm(csd.data);rm(ct.data);rm(da.data)

colnames(qc.data) <-col.nameqc
# don't have the 1-year age distribution from the census,
# so:calculate the denominator reversely: number of low-income pps0-17/prevalence of low-income0-17 (obtained from census)

census.data$lowincome_denominator_0_17 <- census.data$lowincome_0_17 / (census.data$lowincome_prevalence_0_17 / 100)
census.data$lowincome_denominator_total <-census.data$lowincome_total / (census.data$lowincome_prevalence_total /100)
qc.data$lowincome_denominator_0_17 <- qc.data$lowincome_0_17 / (qc.data$lowincome_prevalence_0_17 / 100)
qc.data$lowincome_denominator_total <-qc.data$lowincome_total / (qc.data$lowincome_prevalence_total /100)

# mapping to mapping tables ####
map.table.clsc <-read.csv("census_data_2016/mapping_tables/clsc_census_mapping_gd.csv",header = TRUE) # mapping table btw census and CLSC, table obtained from Guido

map_to_hr<-function(dataset){
    a<-merge(dataset, census.data, by="census_id")
    print(nrow(a[a$census_type.x != a$census_type.y, ]))
    a<-a[, -c(1, 3, 81)]
    a}

map.clsc<-map_to_hr(map.table.clsc); rm(map.table.clsc)

group_to_HR <- function(dataset, hr_code) {  # group to health regions
    t<- dataset %>%
        group_by_(hr_code) %>%
        summarise_all(funs(sum(., na.rm = TRUE)))
    print(all.equal(unique(t[,1]), t[,1]))
    return(t)
}
clsc.data <- group_to_HR(map.clsc, as.character(colnames(map.clsc)[1])) ;rm(map.clsc)
clsc.data$year<-2016
qc.data$year<-2016

Lowincome_total<- clsc.data[,c("CLSC_code" , "lowincome_total","lowincome_denominator_total" , "year" )]
lowincome_0_17<- clsc.data[,c("CLSC_code" , "lowincome_0_17" , "lowincome_denominator_0_17" , "year" )]
emplyment<-clsc.data[,c("CLSC_code" , "Labor_employed" , "Labor_inforced" , "year" )]
edu_lowerthanhigh<-clsc.data[,c("CLSC_code" , "Edu_lowhighs" , "Edu_total" , "year" )]
language_fren<-clsc.data %>%                                                     # speak english or french
    mutate(lang_fren= lang_fr_only + lang_en_fr +lang_en_only)  %>% 
    select(CLSC_code, lang_fren, lang_total, year)
Journey_towork<- clsc.data  %>%
    mutate(commu = com_walk + com_bicycle)%>%
    select(CLSC_code, commu, comnu_total, year)
col.names1<- c("CLSC_code", "Nom", "Den", "Year")
colnames(Lowincome_total)<-col.names1
colnames(lowincome_0_17)<-col.names1
colnames(emplyment)<-col.names1
colnames(edu_lowerthanhigh)<-col.names1
colnames(language_fren)<-col.names1
colnames(Journey_towork)<-col.names1

qc.Lowincome_total<- qc.data[,c("qc_id" , "lowincome_total","totalpopulation" , "year" )]
qc.lowincome_0_17<- qc.data[,c("qc_id" , "lowincome_0_17" , "lowincome_denominator_0_17" , "year" )]
qc.emplyment<-qc.data[,c("qc_id" , "Labor_employed" , "Labor_inforced" , "year" )]
qc.edu_lowerthanhigh<-qc.data[,c("qc_id" , "Edu_lowhighs" , "Edu_total" , "year" )]
qc.language_fren<-qc.data %>%
    mutate(lang_fren= lang_fr_only + lang_en_fr+lang_en_only)  %>% 
    select(qc_id, lang_fren, lang_total, year)
qc.Journey_towork<- qc.data  %>%
    mutate(commu = com_walk + com_bicycle)%>%
    select(qc_id, commu, comnu_total, year)

col.names1qc<- c("qc_id", "Nom", "Den", "Year")
colnames(qc.Lowincome_total)<-col.names1qc
colnames(qc.lowincome_0_17)<-col.names1qc
colnames(qc.emplyment)<-col.names1qc
colnames(qc.edu_lowerthanhigh)<-col.names1qc
colnames(qc.language_fren)<-col.names1qc
colnames(qc.Journey_towork)<-col.names1qc


Population.number<-clsc.data[,c("totalpopulation","totalpop_0_14", "totalpop_15_64" ,  "totalpop_65+" , "year" )]
qc.population.number<-qc.data[,c("totalpopulation","totalpop_0_14", "totalpop_15_64" ,  "totalpop_65+" , "year" )]

age.pyramid<-clsc.data[, c("CLSC_code","Male_total","Male_0_4", "Male_5_9","Male_10_14","Male_15_19",
                           "Male_20_24","Male_25_29", "Male_30_34","Male_35_39","Male_40_44","Male_45_49","Male_50_54","Male_55_59",
                           "Male_60_64","Male_65_69","Male_70_74","Male_75_79","Male_80_84","Male_85_89",
                           "Male_90_94","Male_95_99","Male_100+","Female_total","Female_0_4","Female_5_9",
                           "Female_10_14","Female_15_19","Female_20_24","Female_25_29","Female_30_34","Female_35_39",
                           "Female_40_44","Female_45_49","Female_50_54","Female_55_59","Female_60_64","Female_65_69",
                           "Female_70_74","Female_75_79","Female_80_84","Female_85_89","Female_90_94","Female_95_99",
                           "Female_100+")]


qc.age.pyramid<-qc.data[, c("qc_id","Male_total","Male_0_4", "Male_5_9","Male_10_14","Male_15_19",
                           "Male_20_24","Male_25_29", "Male_30_34","Male_35_39","Male_40_44","Male_45_49","Male_50_54","Male_55_59",
                           "Male_60_64","Male_65_69","Male_70_74","Male_75_79","Male_80_84","Male_85_89",
                           "Male_90_94","Male_95_99","Male_100+","Female_total","Female_0_4","Female_5_9",
                           "Female_10_14","Female_15_19","Female_20_24","Female_25_29","Female_30_34","Female_35_39",
                           "Female_40_44","Female_45_49","Female_50_54","Female_55_59","Female_60_64","Female_65_69",
                           "Female_70_74","Female_75_79","Female_80_84","Female_85_89","Female_90_94","Female_95_99",
                           "Female_100+")]
# check the data through visualization #####
check_age_popu <- function(dataset, hr_code) {
    dataset.F <- dataset %>%
        select_at(.vars = vars(as.character(hr_code), starts_with("Female")))
    dataset.F.age <- melt(dataset.F, id = c(as.character(hr_code)))
    dataset.F.tep <- dataset.F.age  %>%
        separate(variable, c("sex", "age", "_"))
    dataset.F.tep$age<- paste(dataset.F.tep$age,"_", dataset.F.tep$`_`)

    dataset.M <- dataset %>%
        select_at(.vars = vars(as.character(hr_code), starts_with("Male")))
    dataset.M.age <-
        melt(dataset.M, id = c(as.character(hr_code)))
    dataset.M.tep <- dataset.M.age  %>%
        separate(variable, c("sex", "age", "_"))
    dataset.M.tep$age<- paste(dataset.M.tep$age,"_", dataset.M.tep$`_`)
    col.names <- c(hr_code, "sex", "age" , "su", "population")
    colnames(dataset.F.tep) <- col.names
    colnames(dataset.M.tep) <- col.names
    dataset.age.sex <- rbind(dataset.M.tep, dataset.F.tep)
    dataset.age.sex
}

clsc.age.sex.popu<-check_age_popu(dataset = age.pyramid, hr_code = "CLSC_code")
clsc.age.sex.popu<-clsc.age.sex.popu[, -4]
qc.age.sex.popu<-check_age_popu(dataset = qc.age.pyramid, hr_code = "qc_id")
qc.age.sex.popu<-qc.age.sex.popu[, -4]



writecsv<-function(dataset, name){
    write.csv(dataset, file = paste0("region summary data Results/",name, ".csv"))
}

writecsv(Population.number, "Population.number")
writecsv(Lowincome_total, "Lowincome_total")
writecsv(lowincome_0_17,"lowincome_0_17" )
writecsv(emplyment,"emplyment")
writecsv(edu_lowerthanhigh,"edu_lowerthanhigh" )
writecsv(language_fren,"language_fren")
writecsv(Journey_towork,"Journey_towork" )
writecsv(clsc.age.sex.popu,"clsc.age.sex.popu" )

writecsv(qc.population.number, "qc.population.number")
writecsv(qc.Lowincome_total, "qc.Lowincome_total")
writecsv(qc.lowincome_0_17,"qc.lowincome_0_17" )
writecsv(qc.emplyment,"qc.emplyment")
writecsv(qc.edu_lowerthanhigh,"qc.edu_lowerthanhigh" )
writecsv(qc.language_fren,"qc.language_fren")
writecsv(qc.Journey_towork,"qc.Journey_towork" )
writecsv(qc.age.sex.popu,"qc.age.sex.popu" )


### indicator for average income ####
avg.income.CT<- read.csv("average income/dDibqSCaijH_data_CT.csv", header = TRUE)
colnames(avg.income.CT)<-c("census_id","CMA_code", "CT_name", "avg_income_bf_tax","avg_income_af_tax" )
avg.income.CT<-avg.income.CT[, c("census_id", "avg_income_bf_tax","avg_income_af_tax" )]
avg.income.CT$hr_type<-"CT"

avg.income.CSD<- read.csv("average income/PkF441uo4H9g_data_CSD.csv", header = TRUE)
colnames(avg.income.CSD)<-c("census_id","CD_code", "CSD_code", "avg_income_bf_tax","avg_income_af_tax" )
avg.income.CSD<-avg.income.CSD[, c("census_id", "avg_income_bf_tax","avg_income_af_tax" )]
avg.income.CSD$hr_type<-"CSD"

avg.income.CD<- read.csv("average income/UP5JmA7jmfNEOLg_data_CD.csv", header = TRUE)
colnames(avg.income.CD)<-c("census_id","CD_code", "avg_income_bf_tax","avg_income_af_tax" )
avg.income.CD<-avg.income.CD[, c("census_id", "avg_income_bf_tax","avg_income_af_tax" )]
avg.income.CD$hr_type<-"CD"

avg.income.DA<- read.csv("average income/xAsxBjdDE_data_DA.csv", header = TRUE)
colnames(avg.income.DA)<-c("census_id","CD_code", "CD_name","DA_code", "avg_income_bf_tax","avg_income_af_tax" )
avg.income.DA<-avg.income.DA[, c("census_id", "avg_income_bf_tax","avg_income_af_tax" )]
avg.income.DA$hr_type<-"DA"
avg.income.DA <- avg.income.DA[avg.income.DA$census_id > 2500, ]


avg.income <- do.call("rbind", list(avg.income.CD, avg.income.CSD, avg.income.CT, avg.income.DA))
rm(avg.income.CD);rm(avg.income.CSD); rm(avg.income.CT); rm(avg.income.DA)

## mapping to the clsc
# !! stopped here, because when sum to the all income for each CT, the denominator should be known where should be the number of household. 
# in 25% sample data, we don't know the number of household, but know the number of recipients and their average income, so if we want to 
# calculate the average income for recipients not for household, it should be ok, 
# otherwise
# we have number of househodl for the 100% sample size, and 25% average income, so maybe a little dismathch but, still doable
# so, right now, waiting for the decision

avg.income.map.clsc<-merge(map.table.clsc, avg.income, by="census_id")
identical(avg.income.map.clsc$census_type, avg.income.map.clsc$hr_type)
avg.income.map.clsc$ck<- ifelse(avg.income.map.clsc$census_id == avg.income.map.clsc$hr_type, 1, 0)
#avg.income.map.clsc<-avg.income.map.clsc[, c("census_id", "")]

avg.income. clsc<- avg.income.map.clsc %>%
    group_by(hr_code) %>%
    summarise_all(funs())
rm(map.table.clsc)

group_to_HR <- function(dataset, hr_code) {  # group to health regions
    t<- dataset %>%
        group_by_(hr_code) %>%
        summarise_all(funs(sum(., na.rm = TRUE)))
    print(all.equal(unique(t[,1]), t[,1]))
    return(t)
}
clsc.data <- group_to_HR(map.clsc, as.character(colnames(map.clsc)[1])) ;rm(map.clsc)
clsc.data$year<-2016
qc.data$year<-2016 