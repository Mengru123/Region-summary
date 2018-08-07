## Generate indicators for the region summary view, given the mapping file provided by Guido ####

library(dplyr) # pipe
library(plyr)
library(reshape2)
library(ggplot2)
library(tidyr)
library(gridExtra) #for ggplot that can display multiple plot together 
library(tmap) # use for mapping
library(rgdal) # use for mapping 

options("digits"=12)

# read tables and prepare the data ####
#setwd("C:/Users/myuan/Google Drive/1. Practicum/3. Notes/10. Region summary data/Inidcators")
setwd("D:/Google Drive_MR/1. Practicum/3. Notes/10. Region summary data/Inidcators")
map.table<- read.csv("clsc_census_mapping_gd.csv", header = TRUE) # mapping table obtained from Guido
cd.data<-read.csv("census_data_2016/nFzbbLDEVuPz11pLQ_data_CD.csv", header = TRUE) # census data at CD level
cd.in<-read.csv("census_data_2016/income_cutof_pre/BGjxddMe_data_CD.csv", header = TRUE) # low-income cut-off prevalence
cd.totalpop<- read.csv("census_data_2016/total_population/L7tLrqREL7dnf_data_CD.csv", header = TRUE) # totalpopulation
colnames(cd.totalpop)<-c("COL0", "totalpop")
cd.data<- cd.data[, -c(2:5)]
cd.data$type<- "CD"  
cd.data<-join_all(list(cd.data, cd.in, cd.totalpop), by = "COL0", type = 'full')

csd.data<- read.csv("census_data_2016/E8v0huQ0frN4m_data_CSD.csv", header = TRUE)
csd.data<- csd.data[, -c(2:5)]
csd.data$type<- "CSD"
csd.in<-read.csv("census_data_2016/income_cutof_pre/MtTcrBvBmKMcMe_data_CSD.csv", header = TRUE) # low-income cut-off prevalence
csd.totalpop<- read.csv("census_data_2016/total_population/3xKRJbM0e5rHVz9_data_CSD.csv", header = TRUE) # totalpopulation
colnames(csd.totalpop)<-c("COL0", "totalpop")
csd.data<-join_all(list(csd.data, csd.in, csd.totalpop), by = "COL0", type = 'full')

ct.data<-read.csv("census_data_2016/G8iUTCA6zplcndk_data_CT.csv", header = TRUE)
ct.data<- ct.data[, -c(2:4)]
ct.data$type<- "CT" 
ct.in<-read.csv("census_data_2016/income_cutof_pre/H3yaFrFVGjHuc_data_CT.csv", header = TRUE) # low-income cut-off prevalence
ct.totalpop<- read.csv("census_data_2016/total_population/z4PJHBGDz5o8K_data_CT.csv", header = TRUE) # totalpopulation
colnames(ct.totalpop)<-c("COL0", "totalpop")
ct.data<-join_all(list(ct.data, ct.in, ct.totalpop), by = "COL0", type = 'full')

da.data<- read.csv("census_data_2016/zngeryB53_data_DA.csv", header = TRUE)
da.data<- da.data[, -c(2:4)]
da.data$type<- "DA" 
da.in<-read.csv("census_data_2016/income_cutof_pre/ULkBM9b67fy_data_DA.csv", header = TRUE) # low-income cut-off prevalence
da.totalpop<- read.csv("census_data_2016/total_population/bLz4sQza2lfj_data_DA.csv", header = TRUE) # totalpopulation
colnames(da.totalpop)<-c("COL0", "totalpop")
da.data<-join_all(list(da.data, da.in, da.totalpop), by = "COL0", type = 'full')
da.data<-da.data[da.data$COL0>2500, ]

ncol(cd.data);ncol(csd.data); ncol(ct.data); ncol(da.data)

col.names<-c("census_id", "Male_total", "Male_0_14", "Male_0_4","Male_5_9", "Male_10_14", "Male_15_64",
             "Male_15_19", "Male_20_24", "Male_25_29", "Male_30_34", "Male_35_39", "Male_40_44", "Male_45_49", 
             "Male_50_54", "Male_55_59", "Male_60_64", "Male_65+",
             "Male_65_69", "Male_70_74", "Male_75_79", "Male_80_84","Male_85+",
             "Male_85_89", "Male_90_94", "Male_95_99", "Male_100+", "Male_Ave_age", 
             "Female_total", "Female_0_14","Female_0_4", "Female_5_9", "Female_10_14", "Female_15_64", 
             "Female_15_19", "Female_20_24", "Female_25_29", "Female_30_34", "Female_35_39", "Female_40_44","Female_45_49",
             "Female_50_54", "Female_55_59", "Female_60_64","Female_65+",
             "Female_65_69", "Female_70_74", "Female_75_79", "Female_80_84","Female_85+",
             "Female_85_89", "Female_90_94", "Female_95_99",  "Female_100+", "Female_Ave_age", 
             "lang_total", "lang_en_only", "lang_fr_only", "lang_en_fr", "lang_nen_nfr",
             "lowincome_total", "lowincome_0_17", 
             "Edu_total", "Edu_lowhighs","Edu_highsch","Edu_postse",  
             "Labor_total", "Labor_inforced", "Labor_employed", "Labor_unemployed", "Labor_notinfor",
             "comnu_total", "com_bycardriver", "com_bycarpassenger", "com_ptransit", "com_walk", "com_bicycle", "com_other", 
             "census_type", "lowincome_prevalence_total", "lowincome_prevalence_0_17", "totalpopulation")

colnames(cd.data)<-col.names; colnames(csd.data)<-col.names; colnames(ct.data)<-col.names;colnames(da.data)<-col.names

census.data<-do.call("rbind", list(cd.data, csd.data, ct.data, da.data))

# don't have the 1-year age distribution from the census, 
# so:calculate the denominator reversely: number of low-income pps0-17/prevalence of low-income0-17 (obtained from census)
census.data$lowincome_denominator_0_17<- census.data$lowincome_0_17/ (census.data$lowincome_prevalence_0_17/100)
census.data$lowincome_denominator_total<- census.data$lowincome_total/ (census.data$lowincome_prevalence_total/100)


map.cs<- merge(map.table, census.data, by= "census_id")
map.cs[map.cs$census_type.x != map.cs$census_type.y, ] # check if there is a conflict
map.cs<- map.cs[, -c(1,3,81)]

clsc.data<- map.cs %>%
  group_by (CLSC_code) %>%
  summarise_all(funs(sum(., na.rm = TRUE)))
unique(clsc.data$CLSC_code)

clsc.indi<- clsc.data %>%
  mutate_at(.vars = vars(contains('Male')), .funs =funs(pop=(./Male_total)*100)) %>%
  mutate_at(.vars = vars(contains('Female')), .funs =funs(pop=(./Female_total)*100)) %>%
    mutate(langpop=(((lang_fr_only+lang_en_fr)/lang_total)*100)) %>%
    mutate(lowincomepop=(lowincome_total/totalpopulation)*100) %>% 
    mutate(lowincomepop1=(lowincome_total/lowincome_denominator_total)*100) %>%   
    # used to check the reliability of using the denominator calculated reversely from the prevalence of low-income 
    mutate(lowincomepop0_17=(lowincome_0_17/lowincome_denominator_0_17)*100) %>% ## Denominator should be total pop 0-17
    mutate(edupop = (Edu_lowhighs/Edu_total)*100) %>%
    mutate(employpop=(Labor_employed/Labor_total)*100) %>%
    mutate(commupop= ((com_ptransit+com_walk+com_bicycle)/comnu_total)*100)

# new.data<-clsc.data[,order(names(clsc.data))]
# sub.male<-new.data[,which(substr(names(new.data),1,4)=="Male")]
# head(sub.male)
# apply(sub.male,2,function(x){x/clsc.data$Total_Male})

### check datasets ####
temp<-clsc.indi[, c("lowincomepop", "lowincomepop1","lowincomepop0_17", "lowincome_denominator_total", "lowincome_denominator_0_17")]
temp$totalpopu<-clsc.data$Male_total + clsc.data$Female_total
temp$total1<-clsc.data$totalpopulation
temp$d<-temp$total1-temp$totalpopu
temp$popu_0_15<-clsc.data$Male_0_14 + clsc.data$Female_0_14
temp$popu_0_19<-clsc.data$Male_0_14 + clsc.data$Female_0_14+clsc.data$Male_15_19 + clsc.data$Female_15_19
temp$diff_totoal<-temp$totalpopu - temp$lowincome_denominator_total
temp$diff_0_15<-temp$popu_0_15 - temp$lowincome_denominator_0_17
temp$diff_0_19<-temp$popu_0_19 - temp$lowincome_denominator_0_17
View(temp)
rm(temp)
# the denominator(0-17) calculated reversely from the prevalence of low-income was between the populaiton 0-15 to 0-17, 
# so, this low-income_0-17 prevalence = number of low-income 0-17/denominator0-17 totoal population calculated reversely from prevelnce of low-income

#check the DAs that have NA for total population 
a<- csd.data[csd.data$census_id %in% c(2402902, 2403902, 2403904, 2404904, 2422902, 2466092, 2467802, 
                                       2485803, 2485907, 2493902, 2493904, 2493906, 2494926, 2494928, 
                                       2494930, 2495902, 2497810, 2497902, 2497906, 2497908, 2497912, 
                                       2497914, 2498904, 2498912, 2499010, 2499030, 2499035, 2499040, 
                                       2499045, 2499050, 2499055, 2499065, 2499070, 2499877, 2499878, 
                                       2499879, 2499883, 2499887, 2499888, 2499889, 2499890, 2499891, 
                                       2499892, 2499893, 2499894, 2499895, 2499902, 2499904),]

# final dataset for CLSC ####
clsc.indi.fn<-clsc.indi[, c("CLSC_code", "Male_total", "Male_0_14", "Male_15_64", "Male_65+",  
                            "Female_total", "Female_0_14", "Female_15_64", "Female_65+",  
                            "Male_0_4_pop", "Male_5_9_pop", "Male_10_14_pop", "Male_15_19_pop", "Male_20_24_pop", 
                            "Male_25_29_pop", "Male_30_34_pop", "Male_35_39_pop", "Male_40_44_pop", "Male_45_49_pop",
                            "Male_50_54_pop", "Male_55_59_pop", "Male_60_64_pop", "Male_65_69_pop", "Male_70_74_pop",
                            "Male_75_79_pop", "Male_80_84_pop", "Male_85_89_pop", "Male_90_94_pop", "Male_95_99_pop", "Male_100+_pop", 
                            "Female_0_4_pop", "Female_5_9_pop", "Female_10_14_pop", "Female_15_19_pop", "Female_20_24_pop", 
                            "Female_25_29_pop", "Female_30_34_pop", "Female_35_39_pop", "Female_40_44_pop", "Female_45_49_pop",
                            "Female_50_54_pop", "Female_55_59_pop", "Female_60_64_pop","Female_65_69_pop", "Female_70_74_pop", 
                            "Female_75_79_pop", "Female_80_84_pop",  "Female_85_89_pop","Female_90_94_pop", "Female_95_99_pop", "Female_100+_pop", 
                            "langpop", "lowincomepop", "lowincomepop0_17", "edupop", "employpop", "commupop")]

             
# check the data through visualization #####

clsc.age<- clsc.indi.fn[, c(1, 10:51)]
clsc.F<- clsc.age %>%
  select_at(.vars = vars( "CLSC_code", starts_with("Female")))
clsc.M<- clsc.age %>%
  select_at(.vars = vars( "CLSC_code", starts_with("Male")))

clsc.F.age<- melt(clsc.F, id=c("CLSC_code"))
clsc.F.tep<-clsc.F.age  %>%
  separate(variable, c("Female", "pop"), "_")
                  
clsc.M.age<- melt(clsc.M, id=c("CLSC_code"))                  
clsc.M.tep<-clsc.M.age  %>%
  separate(variable, c("Male", "pop"), "_") 

col.names<- c("CLSC_code", "sex", "age" ,"prop")
colnames(clsc.F.tep) <-col.names; colnames(clsc.M.tep)<- col.names
clsc.M.tep$prop <- -1*clsc.M.tep$prop 
clsc.age.sex<- rbind(clsc.M.tep, clsc.F.tep)
clsc.age.sex$age[clsc.age.sex$age=="100+"]<-100

t<-unique(clsc.age.sex$CLSC_code)

# create plot of all clsc's age pyramid ##########
## it works, but takes time and have legend and grid separately, is no better than facet_wrap(~CLSC_code)
plot.age.pyramid<- function(clsc_id) {
  chart_title<- paste0("clsc no.",clsc_id )
  n1<-ggplot(subset (clsc.age.sex[clsc.age.sex$CLSC_code == clsc_id,]), aes(x = as.numeric(age), y = prop, fill = sex)) + 
    geom_bar(data = subset(clsc.age.sex[clsc.age.sex$CLSC_code == clsc_id,], sex == "Female"), stat = "identity") + 
    geom_bar(data=subset(clsc.age.sex[clsc.age.sex$CLSC_code == clsc_id,], sex == "Male"), stat = "identity") + 
    scale_y_continuous(breaks = seq(-20, 20, 5),
                       labels = seq(-20,20,5)) +
    coord_flip() + 
    ggtitle(chart_title)
    scale_fill_brewer(palette = "Set1") + 
    theme_set(theme_bw() + theme(legend.position = "none"))
    n1
}   
# p<-list()
# for (i in 1:25) {
#   p[[i]]<- plot.age.pyramid(t[i])
# }
# do.call(grid.arrange, p)

# other way to show multiple plot at the same time using facet: #####
# folder where you want the graphs to be saved:
#results <- "C:/Users/myuan/Google Drive/1. Practicum/3. Notes/10. Region summary data/Results"  
temp<-t[1:25]
temp<-t[26:50]
temp<-t[51:75]
temp<-t[76:100]
temp<-t[101:150]
temp<-t[151:166]
 ggplot(subset (clsc.age.sex[clsc.age.sex$CLSC_code %in% temp,]), aes(x = as.numeric(age), y = prop, fill = sex)) + 
      geom_bar(data = subset(clsc.age.sex[clsc.age.sex$CLSC_code %in% temp,], sex == "Female"), stat = "identity") + 
      geom_bar(data=subset(clsc.age.sex[clsc.age.sex$CLSC_code %in% temp,], sex == "Male"), stat = "identity") + 
      scale_y_continuous(breaks = seq(-20, 20, 5),
                         labels = seq(-20,20,5)) +
      facet_wrap(~CLSC_code)+
      coord_flip() + 
      ggtitle("clsc_age_pyramid")
      scale_fill_brewer(palette = "Set1") + 
      theme_bw() 

  

# clsc that mannually identified as need to be checked
      
need.ck<-c(17011, 17012,18011,9171,11131,14111,15111, 11141,6533,7152,
           9111,9122,6533,6311,6225,6321,6323,6322,6432,3126,3123,3124,3125)
      
# showing the map############
ttm()
setwd("C:/Users/myuan/Google Drive/1. Practicum/3. Notes/10. Region summary data/Mapping")
clsc.shp<-readOGR("5. CLSC_boundary_2016","Territoires_CLSC_2017", encoding = "UTF-8") 

clsc.map<-tm_shape(clsc.shp)+tm_polygons(col="#67a9cf",border.col = "blue",lwd=3)+
  #tm_facets("clsccode")+
  #tm_shape(clsc.shp)+tm_polygons(border.col = "#ef8a62",alpha=.2) +
  tm_shape(clsc.shp[clsc.shp$CLSC_code %in% need.ck, ]) + tm_polygons(col="red", lwd=4, border.col = "red")
  #tm_facets("CLSC_code")
clsc.map


# other way to show multiple plot at the same time using facet: need to repeat the  "plot the age pyramid" for every "temp" #####
# createAge.plot<- function(dataset,hr_code,n) {
#     a<-ceiling(seq(1, n, (n-1)/5))
#     code<- dataset[, 1]
#     t<-unique(code)
#     for (i in 1: (length(a)-1)) {
#       temp<- t[a[i]: a[(i+1)] ]
#       p1<- ggplot(subset (dataset[dataset[,1] %in% temp,]), aes(x = as.numeric(age), y = prop, fill = sex)) + 
#           geom_bar(data = subset(dataset[dataset[,1] %in% temp,], sex == "Female"), stat = "identity") + 
#           geom_bar(data=subset(dataset[dataset[,1] %in% temp,], sex == "Male"), stat = "identity") + 
#           scale_y_continuous(breaks = seq(-20, 20, 5),
#                              labels = seq(-20,20,5)) +
#           facet_wrap(~eval(substitute(hr_code), dataset))+
#           coord_flip() +
#           scale_fill_brewer(palette = "Set1") + 
#           theme_bw()  
#           print(p1)
#     }
# }
# createAge.plot(clsc.age.sex,CLSC_code, 166)

