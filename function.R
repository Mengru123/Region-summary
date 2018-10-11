df_read = function(df.csv, df.txt) {
    df = read.csv(df.csv, header = TRUE, stringsAsFactors = FALSE)
    df.txt = read.delim(df.txt, stringsAsFactors = FALSE)[-1,] # remove the title line in the name file
    for (i in 1:length(df.txt)) {
        t1 = paste('COL', i, ' - ', '|', 'COL', i, '  - ',sep = '')
        df.txt = gsub(pattern = t1, '', df.txt)
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

    Lowincome_total = df %>% 
        mutate(low.income_nom = `Total persons in private households - 20% sample data `  * `Total persons in private households - 20% sample data / Prevalence of low income after tax in 2005 % `/100) %>%
        select(as.character(key),
               low.income_nom,
               `Total persons in private households - 20% sample data `,
               year )
    
    Lowincome_0_6 = df %>%
        mutate(low.income_nom = `Total persons less than 6 years of age `  * `Total persons less than 6 years of age / Prevalence of low income after tax in 2005 % `/100) %>%
        select(as.character(key),
               low.income_nom,
               `Total persons less than 6 years of age `,
               year)
   
    Employment = df %>%
        select(as.character(key),
               `In the labour force / Employed `,
               `Total population 15 years and over by labour force activity - 20% sample data / In the labour force `,
               year)

    Edu_lowerthanhigh = df %>%
        mutate(edu_15above_low = `Total population 15 to 24 years by highest certificate, diploma or degree - 20% sample data / No certificate, diploma or degree `
                   + `Total population 25 to 64 years by highest certificate, diploma or degree - 20% sample data / No certificate, diploma or degree `
               + `Total population 65 years and over by highest certificate, diploma or degree - 20% sample data / No certificate, diploma or degree `)%>%
        mutate(edu_15above_all= `Total population 15 to 24 years by highest certificate, diploma or degree - 20% sample data `
               + `Total population 25 to 64 years by highest certificate, diploma or degree - 20% sample data `
               + `Total population 65 years and over by highest certificate, diploma or degree - 20% sample data ` )%>%
        select(as.character(key),
               edu_15above_low,
               edu_15above_all,
               year)
    
    Language_FrEn = df %>%                                                     # speak english or french
        mutate(lang_fren= `Total population by knowledge of official languages - 20% sample data / English and French ` 
               + `Total population by knowledge of official languages - 20% sample data / French only ` 
               +`Total population by knowledge of official languages - 20% sample data / English only `)  %>% 
        select(as.character(key), lang_fren, 
               `Total population by knowledge of official languages - 20% sample data `, 
               year)
    
    Journey_towork = df  %>%
        mutate(commu = `Total employed labour force 15 years and over with usual place of work or no fixed workplace address by mode of transportation - 20% sample data / Walked ` 
               + `Total employed labour force 15 years and over with usual place of work or no fixed workplace address by mode of transportation - 20% sample data / Bicycle `)%>%
        select(as.character(key), 
               commu, 
               `Total employed labour force 15 years and over with usual place of work or no fixed workplace address by mode of transportation - 20% sample data `,
               year)
    
    Household_type = df %>%
        mutate(census_family_all = `Total number of private households by household type - 20% sample data / One-family households `
               + `Total number of private households by household type - 20% sample data / Multiple-family households `) %>%
        select(as.character(key),
               census_family_all,
               `Total number of private households by household type - 20% sample data `,
               year)
    
    # the no.of census_family_all is different from no.of census families in private household(this tends to be bigger), 
    # as if one multi-family household will count 1 in census_family_all, but could count as 3 if it has three census-families
 
    #Immigrant_type = df %>%
     #   mutate(PR_above = `Total population by immigrant status and place of birth - 20% sample data / Non-immigrants `
      #         + `Total population by immigrant status and place of birth - 20% sample data / Immigrants `) %>%
       # select(as.character(key),
        #       PR_above,
         #      `Total population by immigrant status and place of birth - 20% sample data `,
          #     year)
    
    
    Immigrant_type = df %>%
        select(as.character(key),
               `Total population by immigrant status and place of birth - 20% sample data / Immigrants `,
               `Total population by immigrant status and place of birth - 20% sample data `,
               year)
    
    Housing_type = df %>%
        select(as.character(key),
               `Total number of occupied private dwellings by housing tenure - 20% sample data / Rented `,
               `Total number of occupied private dwellings by housing tenure - 20% sample data `,
               year)
    
    dfs = list(Lowincome_total = Lowincome_total,
               Lowincome_0_6 = Lowincome_0_6,
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
               `Total population by sex and age groups - 100% data `,
               #`Age & Sex - Both sexes / Total - Age groups and average age of the population - 100% data ; Both sexes / 0 to 14 years ; Both sexes`,
              # `Age & Sex - Both sexes / Total - Age groups and average age of the population - 100% data ; Both sexes / 15 to 64 years ; Both sexes`,
              # `Age & Sex - Both sexes / Total - Age groups and average age of the population - 100% data ; Both sexes / 65 years and over ; Both sexes`,
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
        select_at(.vars = vars(as.character(key), `Total population by sex and age groups - 100% data / Female, total `, starts_with("Female, total / "))) 
    df.F = melt(df.F, id = c(as.character(key)), value.name = "Count", variable.name = "Age")
    df.F[] = lapply(df.F, gsub, 
                    pattern = 'Female, total / |Total population by sex and age groups - 100% data / Female, total ',
                    replacement ='') 
    df.F = df.F  %>%
        mutate(Age = if_else(is.na(Age)|Age =="", "Total", Age))
    df.F[] = lapply(df.F, gsub, 
                    pattern = '/ 0 to 14 years ; Females / |/ 15 to 64 years ; Females / |/ 65 years and over ; Females / |85 years and over ; Females / | years ; Females| ; Females| years ',
                    replacement ='')  
    df.F[] = lapply(df.F, gsub, 
                    pattern = 'to|and over ',
                    replacement ='-') 
    df.F$Sex = "Female"
    
    df.M <- df %>%
        select_at(.vars = vars(as.character(key), `Total population by sex and age groups - 100% data / Male, total `,starts_with("Male, total / "))) 
    df.M = melt(df.M, id = c(as.character(key)), value.name = "Count", variable.name = "Age")
    df.M[] = lapply(df.M, gsub, 
                    pattern = 'Total population by sex and age groups - 100% data / Male, total |Male, total / ',
                    replacement ='') 
    df.M = df.M  %>%
        mutate(Age = if_else(is.na(Age)|Age =="", "Total", Age))
    df.M[] = lapply(df.M, gsub, 
                    pattern = '/ 0 to 14 years ; Males / |/ 15 to 64 years ; Males / |/ 65 years and over ; Males / |85 years and over ; Males / | years ; Males| ; Males| years ',
                    replacement ='')  
    df.M[] = lapply(df.M, gsub, 
                    pattern = 'to|and over ',
                    replacement ='-') 
    df.M$Sex = "Male"
    
    df.age.sex <- rbind(df.M, df.F)
    df.age.sex
}

writecsv<-function(df.list, key){
    for (i in 1:length(df.list))
        write.csv(df.list[i], file = paste0("Output/",as.character(key),"_",names(df.list[i]), ".csv"))
}
