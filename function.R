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
     Lowincome_LIM_total = df %>% 
        select(as.character(key),
               `Income of individuals in 2010 (part 4) - Both sexes / In low income in 2010 based on after-tax low-income measure (LIM-AT); Both sexes`,
               `Income of individuals in 2010 (part 4) - Both sexes / Population in private households for income status; Both sexes`,
               year )
    
    Lowincome_LIM_0_17 = df %>%
        select(as.character(key),
               `Income of individuals in 2010 (part 4) - Both sexes / In low income in 2010 based on after-tax low-income measure (LIM-AT); Both sexes / Less than 18 years; Both sexes`,
               `Income of individuals in 2010 (part 4) - Both sexes / Population in private households for income status; Both sexes / Less than 18 years; Both sexes`, 
               year)
    
    Employment = df %>%
        select(as.character(key),
               `Labour force status - Both sexes / Total population aged 15 years and over by labour force status; Both sexes / In the labour force; Both sexes / Employed; Both sexes`,
               `Labour force status - Both sexes / Total population aged 15 years and over by labour force status; Both sexes / In the labour force; Both sexes`,
               year)
    
    Edu_lowerthanhigh = df %>%
        select(as.character(key),
               `Education - Both sexes / Total population aged 15 years and over by highest certificate, diploma or degree; Both sexes / No certificate, diploma or degree; Both sexes`,
               `Education - Both sexes / Total population aged 15 years and over by highest certificate, diploma or degree; Both sexes`,
               year)
    
    Language_FrEn = df %>%                                                     # speak english or french
        mutate(lang_fren= `Knowledge of official languages - Both sexes / Total population excluding institutional residents; Both sexes / French only; Both sexes` 
               + `Knowledge of official languages - Both sexes / Total population excluding institutional residents; Both sexes / English and French; Both sexes` 
               +`Knowledge of official languages - Both sexes / Total population excluding institutional residents; Both sexes / English only; Both sexes`)  %>% 
        select(as.character(key), lang_fren, 
               `Knowledge of official languages - Both sexes / Total population excluding institutional residents; Both sexes`, 
               year)
    
    Journey_towork = df  %>%
        mutate(commu = `Mode of transportation - Both sexes / Total employed population aged 15 years and over with a usual place of work or no fixed workplace address by mode of transportation; Both sexes / Walked; Both sexes` 
               + `Mode of transportation - Both sexes / Total employed population aged 15 years and over with a usual place of work or no fixed workplace address by mode of transportation; Both sexes / Bicycle; Both sexes`)%>%
        select(as.character(key), 
               commu, 
               `Mode of transportation - Both sexes / Total employed population aged 15 years and over with a usual place of work or no fixed workplace address by mode of transportation; Both sexes`,
               year)
    
    Household_type = df %>%
        select(as.character(key),
               `Household and dwelling characteristics / Total number of private households by household type / Census family households`,
               `Household and dwelling characteristics / Total number of private households by household type`,
               year)
    
    Immigrant_type = df %>%
        mutate(PR_above = `Immigrant status and period of immigration - Both sexes / Total population in private households by immigrant status and period of immigration; Both sexes / Non-immigrants; Both sexes`
               + `Immigrant status and period of immigration - Both sexes / Total population in private households by immigrant status and period of immigration; Both sexes / Immigrants; Both sexes`) %>%
        select(as.character(key),
               PR_above,
               `Immigrant status and period of immigration - Both sexes / Total population in private households by immigrant status and period of immigration; Both sexes`,
               year)
    
    Housing_type = df %>%
        select(as.character(key),
               `Household characteristics / Total number of private households by tenure / Renter`,
               `Household characteristics / Total number of private households by tenure`,
               year)
    
    dfs = list(Lowincome_LIM_total = Lowincome_LIM_total,
               Lowincome_LIM_0_17 = Lowincome_LIM_0_17,
               Employment = Employment,
               Edu_lowerthanhigh = Edu_lowerthanhigh,
               Language_FrEn = Language_FrEn,
               Journey_towork = Journey_towork,
               Household_type = Household_type, 
               Immigrant_type = Immigrant_type,
               Housing_type = Housing_type)
    return(dfs)
}

ext_pop_number = function(df, key) {
    Pop_number <- df %>%
        select(as.character(key),
               `Age & Sex - Both sexes / Total population by age groups; Both sexes`,
               year)
    return(Pop_number)
}

ChangeNames = function(x) {
    names(x)[2:length(x)] = c("Nom", "Den", "Year")
    return(x)
}

ChangeNames_pop = function(x) {
    names(x)[2:length(x)] = c("Total_population", "Year") #"0-14","15-64","65+",
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
                    pattern = 'Age & Sex - Females / Total population by age groups; Females',
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
                    pattern = 'Age & Sex - Males / Total population by age groups; Males',
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
        write.csv(df.list[i], file = paste0("Output/",as.character(key),"_",names(df.list[i]), ".csv"))
}