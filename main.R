library(dplyr) 
library(plyr)
library(reshape2)
library(ggplot2)
library(tidyr)
library(gridExtra) #for ggplot that can display multiple plot together
library(tmap) 
library(rgdal) 

options("digits" = 12)

# read data obtained from Canada census, 2016
df_read = function(df.csv, df.txt) {
    df = read.csv(df.csv, header = TRUE, stringsAsFactors = FALSE)
    df.txt = read.delim(df.txt, stringsAsFactors = FALSE)[-1,] # remove the title line in the name file
    for (i in 1:length(df.txt)) {
        df.txt = gsub(paste('COL', i, ' - ', sep = ''), '', df.txt)
    }
    colnames(df) = append(df.txt, "census_id", after = 0 )
    return(df)
}

df_combine = function(...) {
    arguments = list(...)
    n = length(arguments)
    if (n %% 2 == 0) {
        for (i in seq(1, n, by = 2)) {
            ds = df_read(as.character(arguments[i]),as.character(arguments[i + 1]))
            assign(paste('ds', (i %/% 2 + 1), sep = "_"), ds)
        }
        for (j in 1:(n / 2)) {
            if (j == 1) {
                df = get(paste('ds', j, sep = "_"))
            }
            else {
                df = join_all(list(df, get(paste('ds', j, sep = "_"))),by = "census_id",type = 'full')
            }
        }
        return(df)
    }
    else {
        print ("Please provide both data file and header file")
    }
}
                    
#### read in the datasets obtained from census, at four different region devisions ####
cd.data = df_combine("Data/census_data_2016/nFzbbLDEVuPz11pLQ_data_CD.csv", 
                     "Data/census_data_2016/nFzbbLDEVuPz11pLQ_data_CD.txt",
                     "Data/census_data_2016/income_cutof_pre/BGjxddMe_data_CD.csv",
                     "Data/census_data_2016/income_cutof_pre/BGjxddMe_data_CD.txt",
                     "Data/census_data_2016/total_population/fi3xBCGF33oxEm_data_CD.csv",
                     "Data/census_data_2016/total_population/fi3xBCGF33oxEm_data_CD.txt",
                     "Data/census_data_2016/immigration, family structure, housing/Q71kmE8NAyfgoH_data_CD.csv",
                     "Data/census_data_2016/immigration, family structure, housing/Q71kmE8NAyfgoH_header_CD.txt")
cd.data = cd.data[, -c(2:5)]

csd.data = df_combine("Data/census_data_2016/E8v0huQ0frN4m_data_CSD.csv",
                      "Data/census_data_2016/E8v0huQ0frN4m_data_CSD.txt",
                      "Data/census_data_2016/income_cutof_pre/MtTcrBvBmKMcMe_data_CSD.csv",
                      "Data/census_data_2016/income_cutof_pre/MtTcrBvBmKMcMe_data_CSD.txt",
                      "Data/census_data_2016/total_population/k2Tb5LTN4ON_data_CSD.csv",
                      "Data/census_data_2016/total_population/k2Tb5LTN4ON_data_CSD.txt",
                      "Data/census_data_2016/immigration, family structure, housing/wURdDj13eJ_data_CSD.csv",
                      "Data/census_data_2016/immigration, family structure, housing/wURdDj13eJ_header_CSD.txt")
csd.data = csd.data[, -c(2:5)]

ct.data = df_combine("Data/census_data_2016/G8iUTCA6zplcndk_data_CT.csv",
                     "Data/census_data_2016/G8iUTCA6zplcndk_data_CT.txt",
                     "Data/census_data_2016/income_cutof_pre/H3yaFrFVGjHuc_data_CT.csv",
                     "Data/census_data_2016/income_cutof_pre/H3yaFrFVGjHuc_data_CT.txt",
                     "Data/census_data_2016/total_population/swC9HfRqADoe_data_CT.csv",
                     "Data/census_data_2016/total_population/swC9HfRqADoe_data_CT.txt",
                     "Data/census_data_2016/immigration, family structure, housing/b7pvli7FLB43i_data_CT.csv",
                     "Data/census_data_2016/immigration, family structure, housing/b7pvli7FLB43i_header_CT.txt")
ct.data = ct.data[, -c(2:4)]

da.data = df_combine("Data/census_data_2016/zngeryB53_data_DA.csv",
                     "Data/census_data_2016/zngeryB53_data_DA.txt",
                     "Data/census_data_2016/income_cutof_pre/ULkBM9b67fy_data_DA.csv",
                     "Data/census_data_2016/income_cutof_pre/ULkBM9b67fy_data_DA.txt",
                     "Data/census_data_2016/total_population/ErO0Imol3vrRjoi_data_DA.csv",
                     "Data/census_data_2016/total_population/ErO0Imol3vrRjoi_data_DA.txt",
                     "Data/census_data_2016/immigration, family structure, housing/6IOEpi8povq3vI66_data_DA.csv",
                     "Data/census_data_2016/immigration, family structure, housing/6IOEpi8povq3vI66_header_DA.txt")
da.data <- da.data[da.data$census_id > 2500, ]
da.data = da.data[, -c(2:4)]

census.data = do.call("rbind", list(cd.data, csd.data, ct.data, da.data))
census.data$year = 2016
rm(cd.data);rm(csd.data);rm(ct.data);rm(da.data)

#### add aggregatable value for lowincome indicators ####
add_lowincome_denom = function(df) {
    df$lowincome_denominator_0_17 = df$`Income - Total Sex / In low income based on the Low-income cut-offs, after tax (LICO-AT) / 0 to 17 years` / (census.data$`Income - Total Sex / Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%) / 0 to 17 years (%)` / 100)
    df$lowincome_denominator_total = df$`Income - Total Sex / In low income based on the Low-income cut-offs, after tax (LICO-AT)` / (census.data$`Income - Total Sex / Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)` /100)
    df
}
census.data = add_lowincome_denom(census.data)

#### map to health region: CLSC ####
map.table.clsc = read.csv("Data/census_data_2016/mapping_tables/clsc_census_mapping_gd.csv",header = TRUE) # mapping table btw census and CLSC, table obtained from Guido
map.clsc = merge(map.table.clsc, census.data, by = "census_id")
map.clsc= map.clsc[, !names(map.clsc) %in% c("census_id", "census_type")]
rm(map.table.clsc)

group_to_HR = function(df, hr_code) {  # group to health regions
    t<- df %>%
        group_by_(as.character(hr_code)) %>%
        summarise_all(funs(sum(., na.rm = TRUE)))
    return(t)
}

clsc.data = group_to_HR(map.clsc, colnames(map.clsc)[1])
rm(map.clsc)

#### extract data for pophr loader ####
ext_concept = function(df, key) {
Lowincome_total = df %>% 
    select(as.character(key),
           `Income - Total Sex / In low income based on the Low-income cut-offs, after tax (LICO-AT)`,
           `lowincome_denominator_total`,
            year )

lowincome_0_17 = df %>%
    select(as.character(key),
           `Income - Total Sex / In low income based on the Low-income cut-offs, after tax (LICO-AT) / 0 to 17 years`,
           `lowincome_denominator_0_17`, 
            year)

emplyment = df %>%
    select(as.character(key),
           `Labour - Total Sex / Total - Population aged 15 years and over by Labour force status - 25% sample data / In the labour force / Employed`,
           `Labour - Total Sex / Total - Population aged 15 years and over by Labour force status - 25% sample data / In the labour force`,
            year)

edu_lowerthanhigh = df %>%
    select(as.character(key),
           `Education - Total Sex / Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data / No certificate, diploma or degree`,
           `Education - Total Sex / Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data`,
            year)

language_fren = df %>%                                                     # speak english or french
    mutate(lang_fren= `Knowledge of official language - Both sexes / Total - Knowledge of official languages for the total population excluding institutional residents - 100% data ; Both sexes / French only ; Both sexes` 
           + `Knowledge of official language - Both sexes / Total - Knowledge of official languages for the total population excluding institutional residents - 100% data ; Both sexes / English and French ; Both sexes` 
           +`Knowledge of official language - Both sexes / Total - Knowledge of official languages for the total population excluding institutional residents - 100% data ; Both sexes / English only ; Both sexes`)  %>% 
    select(as.character(key), lang_fren, 
           `Knowledge of official language - Both sexes / Total - Knowledge of official languages for the total population excluding institutional residents - 100% data ; Both sexes`, 
            year)

Journey_towork = df  %>%
    mutate(commu = `Journey to Work - Total Sex / Total - Main mode of commuting for the employed labour force aged 15 years and over in private households with a usual place of work or no fixed workplace address - 25% sample data / Walked` 
           + `Journey to Work - Total Sex / Total - Main mode of commuting for the employed labour force aged 15 years and over in private households with a usual place of work or no fixed workplace address - 25% sample data / Bicycle`)%>%
    select(as.character(key), 
           commu, 
           `Journey to Work - Total Sex / Total - Main mode of commuting for the employed labour force aged 15 years and over in private households with a usual place of work or no fixed workplace address - 25% sample data`,
           year)

household_type = df %>%
    mutate(census_family_all = `Households by type / Total - Private households by household type - 100% data / One-census-family households`
           + `Households by type / Total - Private households by household type - 100% data / Multiple-census-family households`) %>%
    select(as.character(key),
           census_family_all,
           `Households by type / Total - Private households by household type - 100% data`,
           year)

immigrant_type = df %>%
    mutate(PR_above = `Immigration - Total Sex / Total - Immigrant status and period of immigration for the population in private households - 25% sample data / Non-immigrants`
           + `Immigration - Total Sex / Total - Immigrant status and period of immigration for the population in private households - 25% sample data / Immigrants`) %>%
    select(as.character(key),
           PR_above,
           `Immigration - Total Sex / Total - Immigrant status and period of immigration for the population in private households - 25% sample data`,
           year)

housing_type = df %>%
    select(as.character(key),
           `Housing - Total Sex / Total - Private households by tenure - 25% sample data / Owner`,
           `Housing - Total Sex / Total - Private households by tenure - 25% sample data`,
           year)

dfs = list(Lowincome_total = Lowincome_total, 
           lowincome_0_17 = lowincome_0_17, 
           emplyment = emplyment,
           edu_lowerthanhigh = edu_lowerthanhigh,
           language_fren = language_fren,
           Journey_towork = Journey_towork,
           Journey_towork = Journey_towork,
           household_type = household_type, 
           immigrant_type = immigrant_type,
           housing_type = housing_type)
return(dfs)
}
clsc.values = ext_concept(clsc.data, "CLSC_code")

ChangeNames = function(x) {
    names(x) = c("CLSC_code", "Nom", "Den", "Year")
    return(x)
}
clsc.values = lapply(clsc.values, ChangeNames)

#### extract age pyramid ####
ext_pop_number = function(df, key) {
    Pop_number <- df %>%
        select(as.character(key),
               `Age & Sex - Both sexes / Total - Age groups and average age of the population - 100% data ; Both sexes`,
               `Age & Sex - Both sexes / Total - Age groups and average age of the population - 100% data ; Both sexes / 0 to 14 years ; Both sexes`,
               `Age & Sex - Both sexes / Total - Age groups and average age of the population - 100% data ; Both sexes / 15 to 64 years ; Both sexes`,
               `Age & Sex - Both sexes / Total - Age groups and average age of the population - 100% data ; Both sexes / 65 years and over ; Both sexes`,
               year)
    return(Pop_number)
}
clsc.pop.values = ext_pop_number(clsc.data, "CLSC_code")

ChangeNames_pop = function(x) {
    names(x) = c("CLSC_code", "Total_population", "0-14","15-64","65+","Year")
    return(x)
}
clsc.pop.values = lapply(list(clsc.pop.values), ChangeNames_pop)

ext_age_pry = function(df,key) {
    df.F = df %>%
        select_at(.vars = vars(as.character(key), starts_with("Age & Sex - Females / Total"))) %>%
        select(-ends_with("/ 85 years and over ; Females" )) %>% 
        select(-ends_with("/ 65 years and over ; Females" )) %>% 
        select(-ends_with("/ 15 to 64 years ; Females" )) %>% 
        select(-ends_with("/ 0 to 14 years ; Females" )) 
    df.F = melt(df.F, id = c(as.character(key)), value.name = "Count", variable.name = "Age")
    df.F[] = lapply(df.F, gsub, 
                        pattern = 'Age & Sex - Females / Total - Age groups and average age of the population - 100% data ; Females',
                        replacement ='') 
    df.F = df.F  %>%
        mutate(Age = if_else(is.na(Age)|Age =="", "Total", Age))
    df.F[] = lapply(df.F, gsub, 
                        pattern = '/ 0 to 14 years ; Females / |/ 15 to 64 years ; Females / |/ 65 years and over ; Females / |85 years and over ; Females / | years ; Females| ; Females| ',
                        replacement ='')  
    df.F[] = lapply(df.F, gsub, 
                        pattern = 'to|yearsandover',
                        replacement ='-') 
    df.F$Sex = "Female"

    df.M <- df %>%
        select_at(.vars = vars(as.character(key), starts_with("Age & Sex - Males / Total"))) %>%
        select(-ends_with("/ 85 years and over ; Males" )) %>% 
        select(-ends_with("/ 65 years and over ; Males" )) %>% 
        select(-ends_with("/ 15 to 64 years ; Males" )) %>% 
        select(-ends_with("/ 0 to 14 years ; Males" )) 
    df.M = melt(df.M, id = c(as.character(key)), value.name = "Count", variable.name = "Age")
    df.M[] = lapply(df.M, gsub, 
                        pattern = 'Age & Sex - Males / Total - Age groups and average age of the population - 100% data ; Males',
                        replacement ='') 
    df.M = df.M  %>%
        mutate(Age = if_else(is.na(Age)|Age =="", "Total", Age))
    df.M[] = lapply(df.M, gsub, 
                        pattern = '/ 0 to 14 years ; Males / |/ 15 to 64 years ; Males / |/ 65 years and over ; Males / |85 years and over ; Males / | years ; Males| ; Males| ',
                        replacement ='')  
    df.M[] = lapply(df.M, gsub, 
                        pattern = 'to|yearsandover',
                        replacement ='-') 
    df.M$Sex = "Male"
    
    df.age.sex <- rbind(df.M, df.F)
    df.age.sex
}
clsc.age.pry = ext_age_pry(clsc.data, "CLSC_code")

clsc.values = c(clsc.values, clsc.pop.values = clsc.pop.values, clsc.age.pry = list(clsc.age.pry))
#### output results ####
writecsv<-function(df.list){
    for (i in 1:length(df.list))
    write.csv(df.list[i], file = paste0("Output/",names(df.list[i]), "_2018.07.05.csv"))
}
writecsv(clsc.values)
