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

sum_by_key = function(df, key) {  # group to health regions
    t<- df %>%
        group_by_(as.character(key)) %>%
        summarise_all(funs(sum(., na.rm = TRUE)))
    return(t)
}

ext_concept = function(df, key) {
    Lowincome_LICO_total = df %>% 
        select(as.character(key),
               `Income - Total Sex / In low income based on the Low-income cut-offs, after tax (LICO-AT)`,
               `Income - Total Sex / Total - Low-income status in 2015 for the population in private households to whom low-income concepts are applicable - 100% data`,
               year )
    
    Lowincome_LICO_0_17 = df %>%
        select(as.character(key),
               `Income - Total Sex / In low income based on the Low-income cut-offs, after tax (LICO-AT) / 0 to 17 years`,
               `Income - Total Sex / Total - Low-income status in 2015 for the population in private households to whom low-income concepts are applicable - 100% data / 0 to 17 years`, 
               year)
   
     Lowincome_LIM_total = df %>% 
        select(as.character(key),
               `Income - Total Sex / In low income based on the Low-income measure, after tax (LIM-AT)`,
               `Income - Total Sex / Total - Low-income status in 2015 for the population in private households to whom low-income concepts are applicable - 100% data`,
               year )
    
    Lowincome_LIM_0_17 = df %>%
        select(as.character(key),
               `Income - Total Sex / In low income based on the Low-income measure, after tax (LIM-AT) / 0 to 17 years`,
               `Income - Total Sex / Total - Low-income status in 2015 for the population in private households to whom low-income concepts are applicable - 100% data / 0 to 17 years`, 
               year)
    
    Emplyment = df %>%
        select(as.character(key),
               `Labour - Total Sex / Total - Population aged 15 years and over by Labour force status - 25% sample data / In the labour force / Employed`,
               `Labour - Total Sex / Total - Population aged 15 years and over by Labour force status - 25% sample data / In the labour force`,
               year)
    
    Edu_lowerthanhigh = df %>%
        select(as.character(key),
               `Education - Total Sex / Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data / No certificate, diploma or degree`,
               `Education - Total Sex / Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data`,
               year)
    
    Language_fren = df %>%                                                     # speak english or french
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
    
    Household_type = df %>%
        mutate(census_family_all = `Households by type / Total - Private households by household type - 100% data / One-census-family households`
               + `Households by type / Total - Private households by household type - 100% data / Multiple-census-family households`) %>%
        select(as.character(key),
               census_family_all,
               `Households by type / Total - Private households by household type - 100% data`,
               year)
    
    Immigrant_type = df %>%
        mutate(PR_above = `Immigration - Total Sex / Total - Immigrant status and period of immigration for the population in private households - 25% sample data / Non-immigrants`
               + `Immigration - Total Sex / Total - Immigrant status and period of immigration for the population in private households - 25% sample data / Immigrants`) %>%
        select(as.character(key),
               PR_above,
               `Immigration - Total Sex / Total - Immigrant status and period of immigration for the population in private households - 25% sample data`,
               year)
    
    Housing_type = df %>%
        select(as.character(key),
               `Housing - Total Sex / Total - Private households by tenure - 25% sample data / Owner`,
               `Housing - Total Sex / Total - Private households by tenure - 25% sample data`,
               year)
    
    dfs = list(Lowincome_LICO_total = Lowincome_LICO_total, 
               Lowincome_LICO_0_17 = Lowincome_LICO_0_17, 
               Lowincome_LIM_total = Lowincome_LIM_total,
               Lowincome_LIM_0_17 = Lowincome_LIM_0_17,
               Emplyment = Emplyment,
               Edu_lowerthanhigh = Edu_lowerthanhigh,
               Language_fren = Language_fren,
               Journey_towork = Journey_towork,
               Household_type = Household_type, 
               Immigrant_type = Immigrant_type,
               Housing_type = Housing_type)
    return(dfs)
}

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

ChangeNames = function(x) {
    names(x)[2:length(x)] = c("Nom", "Den", "Year")
    return(x)
}

ChangeNames_pop = function(x) {
    names(x)[2:length(x)] = c("Total_population", "0-14","15-64","65+","Year")
    return(x)
}

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

writecsv<-function(df.list, key){
    for (i in 1:length(df.list))
        write.csv(df.list[i], file = paste0("Output/",as.character(key),"_",names(df.list[i]), "_2018.07.05.csv"))
}