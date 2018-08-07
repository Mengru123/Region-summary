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
colnames.totpop <- c("COL0", "totalpopulation","totalpop_0_14","totalpop_15_64","totalpop_65+")
colnames(cd.totalpop) <-colnames.totpop
cd.other3<- read.csv("census_data_2016/immigration, family structure, housing/Q71kmE8NAyfgoH_data_CD.csv", header = TRUE) # immigrants,family structure, housing indicators
cd.other3.names <- read.delim("census_data_2016/immigration, family structure, housing/Q71kmE8NAyfgoH_header_CD.txt", stringsAsFactors = FALSE)
cd.other3.names <- cd.other3.names[-1, ]
colnames.other3<-c("COL0", 
                   "COL1 - Households by type / Total - Private households by household type - 100% data", 
                   "COL2 - Households by type / Total - Private households by household type - 100% data / One-census-family households", 
                   "COL3 - Households by type / Total - Private households by household type - 100% data / One-census-family households / Without children in a census family", 
                   "COL4 - Households by type / Total - Private households by household type - 100% data / One-census-family households / With children in a census family", 
                   "COL5 - Households by type / Total - Private households by household type - 100% data / Multiple-census-family households", 
                   "COL6 - Households by type / Total - Private households by household type - 100% data / Non-census-family households", 
                   "COL7 - Households by type / Total - Private households by household type - 100% data / Non-census-family households / One-person households", 
                   "COL8 - Households by type / Total - Private households by household type - 100% data / Non-census-family households / Two-or-more person non-census-family households", 
                   "COL9 - Immigration - Total Sex / Total - Citizenship for the population in private households - 25% sample data", 
                   "COL10 - Immigration - Total Sex / Total - Citizenship for the population in private households - 25% sample data / Canadian citizens", 
                   "COL11 - Immigration - Total Sex / Total - Citizenship for the population in private households - 25% sample data / Not Canadian citizens", 
                   "COL12 - Immigration - Total Sex / Total - Immigrant status and period of immigration for the population in private households - 25% sample data", 
                   "COL13 - Immigration - Total Sex / Total - Immigrant status and period of immigration for the population in private households - 25% sample data / Non-immigrants", 
                   "COL14 - Immigration - Total Sex / Total - Immigrant status and period of immigration for the population in private households - 25% sample data / Immigrants", 
                   "COL15 - Immigration - Total Sex / Total - Immigrant status and period of immigration for the population in private households - 25% sample data / Non-permanent residents", 
                   "COL16 - Immigration - Total Sex / Total - Citizenship for the population in private households - 25% sample data / Canadian citizens / Canadian citizens aged under 18", 
                   "COL17 - Immigration - Total Sex / Total - Citizenship for the population in private households - 25% sample data / Canadian citizens / Canadian citizens aged 18 and over", 
                   "COL18 - Housing - Total Sex / Total - Private households by tenure - 25% sample data", 
                   "COL19 - Housing - Total Sex / Total - Private households by tenure - 25% sample data / Owner", 
                   "COL20 - Housing - Total Sex / Total - Private households by tenure - 25% sample data / Renter", 
                   "COL21 - Housing - Total Sex / Total - Private households by tenure - 25% sample data / Band housing", 
                   "COL22 - Housing - Total Sex / Total -  Owner and tenant households with household total income greater than zero, in non-farm, non-reserve private dwellings by shelter-cost-to-income ratio - 25% sample data", 
                   "COL23 - Housing - Total Sex / Total -  Owner and tenant households with household total income greater than zero, in non-farm, non-reserve private dwellings by shelter-cost-to-income ratio - 25% sample data / Spending less than 30% of income on shelter costs", 
                   "COL24 - Housing - Total Sex / Total -  Owner and tenant households with household total income greater than zero, in non-farm, non-reserve private dwellings by shelter-cost-to-income ratio - 25% sample data / Spending 30% or more of income on shelter costs"
)
colnames(cd.other3) <- colnames.other3
cd.data <- cd.data[, -c(2:5)]
cd.data$type <- "CD"
cd.data <-join_all(list(cd.data, cd.in, cd.totalpop, cd.other3),by = "COL0",type = 'full')
rm(cd.in);rm(cd.totalpop);rm(cd.other3); rm(cd.other3.names)

csd.data <- read.csv("census_data_2016/E8v0huQ0frN4m_data_CSD.csv", header = TRUE)
csd.data <- csd.data[, -c(2:5)]
csd.data$type <- "CSD"
csd.in <-read.csv("census_data_2016/income_cutof_pre/MtTcrBvBmKMcMe_data_CSD.csv",header = TRUE) # low-income cut-off prevalence
csd.totalpop <-read.csv("census_data_2016/total_population/k2Tb5LTN4ON_data_CSD.csv",header = TRUE) # totalpopulation
colnames(csd.totalpop) <- colnames.totpop

csd.other3<- read.csv("census_data_2016/immigration, family structure, housing/wURdDj13eJ_data_CSD.csv", header = TRUE)
csd.other3.names <- read.delim("census_data_2016/immigration, family structure, housing/wURdDj13eJ_header_CSD.txt", stringsAsFactors = FALSE)
csd.other3.names <- csd.other3.names[-1, ]
colnames(csd.other3) <- colnames.other3

csd.data <-join_all(list(csd.data, csd.in, csd.totalpop, csd.other3),by = "COL0",type = 'full')
rm(csd.in);rm(csd.totalpop);rm(csd.other3); rm(csd.other3.names)

ct.data <- read.csv("census_data_2016/G8iUTCA6zplcndk_data_CT.csv", header = TRUE)
ct.data <- ct.data[, -c(2:4)]
ct.data$type <- "CT"
ct.in <-read.csv("census_data_2016/income_cutof_pre/H3yaFrFVGjHuc_data_CT.csv",header = TRUE) # low-income cut-off prevalence
ct.totalpop <-read.csv("census_data_2016/total_population/swC9HfRqADoe_data_CT.csv",header = TRUE) # totalpopulation
colnames(ct.totalpop) <-colnames.totpop
ct.other3<- read.csv("census_data_2016/immigration, family structure, housing/b7pvli7FLB43i_data_CT.csv", header = TRUE)
ct.other3.names <- read.delim("census_data_2016/immigration, family structure, housing/b7pvli7FLB43i_header_CT.txt", stringsAsFactors = FALSE)
ct.other3.names <- ct.other3.names[-1, ]
colnames(ct.other3) <- colnames.other3
ct.data <- join_all(list(ct.data, ct.in, ct.totalpop,ct.other3),by = "COL0",type = 'full')
rm(ct.in);rm(ct.totalpop);rm(ct.other3); rm(ct.other3.names)

da.data <- read.csv("census_data_2016/zngeryB53_data_DA.csv", header = TRUE)
da.data <- da.data[, -c(2:4)]
da.data$type <- "DA"
da.in <-read.csv("census_data_2016/income_cutof_pre/ULkBM9b67fy_data_DA.csv",header = TRUE) # low-income cut-off prevalence
da.totalpop <-read.csv("census_data_2016/total_population/ErO0Imol3vrRjoi_data_DA.csv",header = TRUE) # totalpopulation
colnames(da.totalpop) <-colnames.totpop
da.other3<- read.csv("census_data_2016/immigration, family structure, housing/6IOEpi8povq3vI66_data_DA.csv", header = TRUE)
da.other3.names <- read.delim("census_data_2016/immigration, family structure, housing/6IOEpi8povq3vI66_header_DA.txt", stringsAsFactors = FALSE)
da.other3.names <- da.other3.names[-1, ]
colnames(da.other3) <- colnames.other3

da.data <-join_all(list(da.data, da.in, da.totalpop,da.other3),by = "COL0",type = 'full')
rm(da.in);rm(da.totalpop);rm(da.other3); rm(da.other3.names);rm(colnames.other3);rm(colnames.totpop)
da.data <- da.data[da.data$COL0 > 2500, ]

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
        "lowincome_prevalence_0_17")

colnames(cd.data)[1:81] <-col.names ;colnames(csd.data)[1:81] <-col.names
colnames(ct.data)[1:81] <- col.names;colnames(da.data)[1:81] <- col.names
census.data <- do.call("rbind", list(cd.data, csd.data, ct.data, da.data))
rm(cd.data);rm(csd.data);rm(ct.data);rm(da.data); rm(col.names)
# don't have the 1-year age distribution from the census,
# so:calculate the denominator reversely: number of low-income pps0-17/prevalence of low-income0-17 (obtained from census)

census.data$lowincome_denominator_0_17 <- census.data$lowincome_0_17 / (census.data$lowincome_prevalence_0_17 / 100)
census.data$lowincome_denominator_total <-census.data$lowincome_total / (census.data$lowincome_prevalence_total /100)

# mapping to mapping tables ####
map.table.clsc <-read.csv("census_data_2016/mapping_tables/clsc_census_mapping_gd.csv",header = TRUE) # mapping table btw census and CLSC, table obtained from Guido
# map.table.rss<- read.csv("census_data_2016/mapping_tables/rss_to_census_gd.csv",header = TRUE) # mapping table btw census and rss, table obtained from Guido
# map.table.rss<-map.table.rss[, -1]
# clsc to rss mapping
# clsc.shp <-readOGR("5. CLSC_boundary_2016", "Territoires_CLSC_2017", encoding = "UTF-8")
# map.table.clsc.to.rss<-clsc.shp@data [,c(1,5)]
# map.table.clsc.to.rss$CLSC_code<-as.numeric(as.character(map.table.clsc.to.rss$CLSC_code))
# map.table.clsc.to.rss$RSS_code<-as.numeric(as.character(map.table.clsc.to.rss$RSS_code))

map_to_hr<-function(dataset){
    a<-merge(dataset, census.data, by="census_id")
    print(nrow(a[a$census_type.x != a$census_type.y, ]))
    a<-a[, -c(1, 3, 81)]
    a}

map.clsc<-map_to_hr(map.table.clsc); rm(map.table.clsc)
#map.rss<- map_to_hr(map.table.rss) ; rm(map.table.rss)

group_to_HR <- function(dataset, hr_code) {  # group to health regions
    t<- dataset %>%
        group_by_(hr_code) %>%
        summarise_all(funs(sum(., na.rm = TRUE)))
    print(all.equal(unique(t[,1]), t[,1]))
    return(t)
}
clsc.data <- group_to_HR(map.clsc, as.character(colnames(map.clsc)[1])) ;rm(map.clsc)
#rss.data.direct<-group_to_HR(map.rss, as.character(colnames(map.rss)[1])); rm(map.rss)
#clsc.data.tempto.rss<- merge(map.table.clsc.to.rss, clsc.data, by = "CLSC_code"); rm(map.table.clsc.to.rss)
# rss.data.fm.clsc<- clsc.data.tempto.rss %>%
#     select(-c(CLSC_code)) %>%
#     group_by(RSS_code) %>%
#     summarise_all(funs(sum(., na.rm = TRUE)))
# 
# diff <- rss.data.direct[,FALSE] # because for loop will add column, but the row number should be the same with the forloop inputs
# for (i in 2:ncol(rss.data.direct)) {
#     diff[ ,i-1] <-  (rss.data.direct[, i] - rss.data.fm.clsc[, i])
# }    


createIndi <- function(dataset) {  # generate indicators from the hr (health region) tables
        dataset %>%
            mutate_at(.vars = vars(contains('Male')), .funs = funs(pop = (. / Male_total) * 100)) %>%
            mutate_at(.vars = vars(contains('Female')),.funs = funs(pop = (. / Female_total) * 100)) %>%
            mutate(langpop = (((lang_fr_only + lang_en_fr) / lang_total) * 100)) %>%   # Only French is officical language in QC
            mutate(lowincomepop = (lowincome_total / totalpopulation) * 100) %>%
            mutate(lowincomepop1 = (lowincome_total / lowincome_denominator_total) * 100) %>%
            # used to check the reliability of using the denominator calculated reversely from the prevalence of low-income
            mutate(lowincomepop0_17 = (lowincome_0_17 / lowincome_denominator_0_17) * 100) %>% ## Denominator should be total pop 0-17
            mutate(edupop = (Edu_lowhighs / Edu_total) * 100) %>%
            mutate(employpop = (Labor_employed / Labor_total) * 100) %>%
            mutate(commupop = ((com_ptransit + com_walk + com_bicycle) / comnu_total) *100)  # use public transit, walk and bicycle are the nominator
    }
clsc.indi <- createIndi(clsc.data)
rss.indi<-createIndi(rss.data.direct)
rss.indi.fm.clsc<-createIndi(rss.data.fm.clsc)

diff1 <- rss.indi.fm.clsc[,FALSE] # because for loop will add column, but the row number should be the same with the forloop inputs
for (i in 2:ncol(rss.indi.fm.clsc)) {
    diff1[, 1]
    diff1[ ,i-1] <-  (rss.indi[, i] - rss.indi.fm.clsc[, i])
} 

# finalize the indicator dataset for different health regions ####
creat_indi_fn<- function(dataset){
    t<-dataset[, c(as.character(colnames(dataset)[1]),"totalpopulation","totalpop_0_14","totalpop_15_64","totalpop_65+", "Male_0_4_pop",
                   "Male_5_9_pop","Male_10_14_pop","Male_15_19_pop","Male_20_24_pop","Male_25_29_pop","Male_30_34_pop","Male_35_39_pop",
                   "Male_40_44_pop","Male_45_49_pop","Male_50_54_pop","Male_55_59_pop","Male_60_64_pop","Male_65_69_pop","Male_70_74_pop",
                   "Male_75_79_pop","Male_80_84_pop","Male_85_89_pop","Male_90_94_pop","Male_95_99_pop","Male_100+_pop","Female_0_4_pop",
                   "Female_5_9_pop","Female_10_14_pop","Female_15_19_pop","Female_20_24_pop","Female_25_29_pop","Female_30_34_pop","Female_35_39_pop",
                   "Female_40_44_pop","Female_45_49_pop","Female_50_54_pop","Female_55_59_pop","Female_60_64_pop","Female_65_69_pop",
                   "Female_70_74_pop","Female_75_79_pop","Female_80_84_pop","Female_85_89_pop","Female_90_94_pop","Female_95_99_pop",
                   "Female_100+_pop","langpop","lowincomepop","lowincomepop0_17","edupop","employpop","commupop")]
    return(t)
}
clsc.indi.fn<-creat_indi_fn(clsc.indi)
rss.indi.fn<-creat_indi_fn(rss.indi)
diff2<-creat_indi_fn(diff1)
diff3<-diff2[, c("langpop","lowincomepop","lowincomepop0_17","edupop","employpop","commupop")]
colMax <- function(data) sapply(data, max, na.rm = TRUE)
a<-data.frame(colMax(diff2))


# check the data through visualization #####
check_age <- function(dataset, hr_code) {
    dataset.age <- dataset[, c(1, 6:47)]
    dataset.F <- dataset.age %>%
        select_at(.vars = vars(as.character(hr_code), starts_with("Female")))
    dataset.F.age <- melt(dataset.F, id = c(as.character(hr_code)))
    dataset.F.tep <- dataset.F.age  %>%
        separate(variable, c("Female", "pop"), "_")
    dataset.M <- dataset.age %>%
        select_at(.vars = vars(as.character(hr_code), starts_with("Male")))
    dataset.M.age <-
        melt(dataset.M, id = c(as.character(hr_code)))
    dataset.M.tep <- dataset.M.age  %>%
        separate(variable, c("Male", "pop"), "_")
    col.names <- c(hr_code, "sex", "age" , "prop")
    colnames(dataset.F.tep) <- col.names
    colnames(dataset.M.tep) <- col.names
    dataset.M.tep$prop <- -1 * dataset.M.tep$prop
    dataset.age.sex <- rbind(dataset.M.tep, dataset.F.tep)
    dataset.age.sex$age[dataset.age.sex$age == "100+"] <- 100
    dataset.age.sex
}
clsc.age.sex<-check_age(dataset = clsc.indi.fn, hr_code = "CLSC_code")

#plot the age pyramid #####
createAge.plot<- function(dataset,hr_code, n) {
    a<-ceiling(seq(1, n, (n-1)/5)) 
    code<- dataset[, 1]
    t<-unique(code) 
    for (i in 1: (length(a)-1)) { 
        x<-a[i]
        y<-a[i+1]-1
      temp<- t[x:y]  
      p1<- ggplot(subset (dataset[dataset[,1] %in% temp,]), aes(x = as.numeric(age), y = prop, fill = sex)) +
          geom_bar(data = subset(dataset[dataset[,1] %in% temp,], sex == "Female"), stat = "identity") +
          geom_bar(data=subset(dataset[dataset[,1] %in% temp,], sex == "Male"), stat = "identity") +
          scale_y_continuous(breaks = seq(-20, 20, 5),labels = seq(-20,20,5)) +
          facet_wrap(c(as.character(hr_code)))+
          coord_flip() +
          scale_fill_brewer(palette = "Set1") +
          theme_bw()
     print(p1)
    }
}
createAge.plot(clsc.age.sex,"CLSC_code", 166)

# showing on the map############
# clsc that mannually identified as need to be checked
need.ck <- c(17011,17012,18011,9171,11131,14111,15111,11141,6533,7152,9111,9122,6533,6311,6225,6321,
            6323,6322,6432,3126,3123,3124,3125)
ttm()
setwd("C:/Users/myuan/Google Drive/1. Practicum/3. Notes/10. Region summary data/Mapping")
clsc.shp <-readOGR("5. CLSC_boundary_2016", "Territoires_CLSC_2017", encoding = "UTF-8")
clsc.map <-tm_shape(clsc.shp) + tm_polygons(col = "#67a9cf",border.col = "blue",lwd = 3) +
    #tm_facets("clsccode")+ tm_shape(clsc.shp)+tm_polygons(border.col = "#ef8a62",alpha=.2) +
    #tm_shape(clsc.shp[clsc.shp$CLSC_code %in% need.ck, ]) + tm_polygons(col ="red",lwd = 4,border.col = "red")
    tm_shape(clsc.shp[clsc.shp$CLSC_code =="06311", ]) + tm_polygons(col ="red",lwd = 4,border.col = "red")

clsc.map
