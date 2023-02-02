
require(tidyverse)
require(GlobalFundr)
require(readxl)

#  a. AABOUT ####
# script to get and tidy inputs to the CT input data tool mainly partner data from PIP plus some data on key pops directly from John Stover Avenir health

# results are fed into ct_input_data_pivot_reader.xlsx (refresh the load tab refresh)
# query, advanced editor, 
# set to auto refresh

# use indicator_year_list.csv to determine the indicators and years to show in the dashboard
# use partner_data_lookup.csv to define indicator display names and the order of the indicators on the use dashboard 

# location of final dashboard
# https://analytics.theglobalfund.org/#/views/CTInsightsSurveyonStrategicTargetModellingfor2023-25/Startpage?:display_count=n&:iid=1&:origin=viz_share_link&:showAppBanner=false&:showVizHome=n


# TO DO ####
# validate malaria330 and p_iptp3_all 

#  b. SWITCHBOARD SET UP WORKING DIRECTORIES, SPECIFY ANY ADDITIONAL PACKAGES, DEFINE PARAMETERS ####


out <- paste0(getwd(),"/out/")  
in_ <- paste0(getwd(),"/in_/")

# define not in function, used to exclude indicators 
`%notin%` <- Negate(`%in%`)

#  c. DEFINE SWITCHBOARD PARAMETERS TO UPDATE EACH USE OF SCRIPT 
#  STEP 1 . READ IN SOURCE FILES, LOOK UP TABLES ETC ####


# define 6 year average periods by indicator 


# group 1 standard indicators
av_end_year <- 2021
av_start_year <- av_end_year-5


# group2 - inds with a one year lag
lag1_av_end_year <- av_end_year-1
lag1_av_start_year <- av_end_year-6

# group3 - inds with a one year lag
lag2_av_end_year <- av_end_year-2
lag2_av_start_year <- av_end_year-7

av_start_year
av_end_year

lag1_av_start_year 
lag1_av_end_year

lag2_av_start_year
lag2_av_end_year

#   1.a get country and iso3 names ####
geog <- GlobalFundr::extractGeographyReference() %>% select(ISO3,GeographyName)
# get geog from global environment or if not there, from online

#   1.b also get GF GlobalFundRegions_list 
GlobalFundRegions_list <- GlobalFundr::extractGeographyReference() %>% 
  select(ISO3,GlobalFundRegion,GlobalFundDepartment)
# 5 gf departments and 10 gf regions


#   1.b. select the indicators to display in the final dashboard data ####

indicator_year_list_db_2 <-
  read_csv("in_/indicator_year_list_db_2.csv") %>% select(id,component,source)



#   1.c read in eligibility look up table  ####
elig_lookup <- extractEligibilityList(year = 2022) %>%
                rename_with(tolower) %>%
                select(iso3,component,eligibility) %>%
                spread(key = component,value = eligibility) %>%
                rename_with(tolower) %>%
                mutate(tuberculosis=ifelse(tuberculosis=="Transition Funding", "Yes", tuberculosis)) %>%
                mutate(malaria=ifelse(malaria=="Transition Funding", "Yes", malaria)) %>%
                mutate(eligible=ifelse(`hiv/aids`=="Yes"|tuberculosis=="Yes"|malaria=="Yes","Yes",NA))

#   1.d get PIP data ####
pip_data <-
  # can use offline copy of pip for working offline
  # read.csv("~/__SI/_2023-2028 Strategic target setting/partner input data for CT review tool/data/out/unprocessed_pip_2022-11-22.csv")

  extractPIP(indicators = c(
    "HIV163","HIV46","HIV186","HIV183","HIV417","HIV437","HIV211","HIV440","HIV893",
                            "TB645","TB1","TB11","TB64",
                            "TB81","TB190","TB202","TB289","TB285","TB85", "TB84","TB239","TB269","TB285",
                            "TB273","TB289","TB641","TB639","TB643","TB683","TB646","TB642","TB648","TB656","TB273","TB269","TB495","TB640",
                            "Malaria1", "Malaria32","Malaria7", # Malaria32 source annex 5G WMR col D ' At risk low and high'
                            "Malaria24", "Malaria48",
                            "Malaria39","Malaria41", "Malaria45","Malaria31","Malaria189","Malaria330","Malaria327","Malaria325","Malaria326",
                            "Malaria186","Malaria343", "Malaria454","Malaria334"
                            )) %>%
  select(ISOCountryCode,Year,
         # ActivityAreaName,
         ActivityAreaIndicatorCode,Percentage,Numerator,Denominator) %>%
  mutate(value=ifelse(is.na(Numerator),Percentage,Numerator)) %>%
  select(-c(Numerator,Denominator,Percentage)) %>%
  rename(iso3=ISOCountryCode) %>%
  rename(year=Year) %>%
  rename(name=ActivityAreaIndicatorCode) %>%
  rename_with(tolower) %>%
  filter(year<=av_end_year&year>=lag2_av_start_year) 

# for reference / audit write out a copy of the source data file
save_pip <- 0
if(save_pip==1) {
  write.csv(raw_PIP_data,file = paste0(out,"raw_PIP_data",Sys.Date(),".csv"),row.names = FALSE)
  print(paste0("A copy of PIP was written to",out,"raw_PIP_data",Sys.Date(),".csv"))
}

#  STEP 2 obtain iso3 level n/d for all needed indicators  ####

# back calculate the country level ITN use and access


pip_data <- 
  pip_data %>% 
  pivot_wider(names_from = name,values_from = value) %>% 

# back caclulate the numeratoros for bednet access and use 

  mutate(Malaria186_n=Malaria186/100*Malaria32) %>% 
  mutate(Malaria189_n=Malaria189/100*Malaria32) %>% 
  # for p_mdr_tx use varying n/d per year 
  mutate(TB_n_mdr_tx=ifelse(year<=2019,TB85,TB641+TB639)) %>% 
  mutate(TB_d_mdr_tx=ifelse(year<=2019,TB84,TB640)) %>% 
  mutate(year=as.numeric(year))

                                                                              
#  STEP 3 add eligibility criteria ####

pip_data <-
  pip_data %>% 
  left_join(elig_lookup %>% 
              mutate(across(everything(), ~replace(.,. =="Yes", "1")))) %>%
  select(-eligible) %>% 
  rename(elig_hiv=`hiv/aids`,elig_tb=tuberculosis,elig_mal=malaria)



#  STEP 4 summarise n and ds to 10 regions  ####

regions_pip_data <-
  full_join(
  full_join(
  # hiv eligible
  pip_data %>%
  filter(elig_hiv==1) %>%
    group_by(iso3) %>%
    select(iso3,year,contains("HI")) %>%
    right_join(GlobalFundRegions_list, by=c("iso3"="ISO3")) %>%
  filter(!is.na(GlobalFundRegion)) %>%
  # select(-(GlobalFundRegion)) %>%
    group_by(year,GlobalFundRegion) %>%
  summarise_if(is.numeric,sum,na.rm = TRUE) %>%
  filter(!is.na(year)) %>%
  rename(iso3=GlobalFundRegion) %>%
  mutate(year=as.numeric(year)) %>%
  ungroup(),

# TBeligible
pip_data %>%
  filter(elig_tb==1) %>%
  group_by(iso3) %>%
  select(iso3,year,contains("TB")) %>%
  right_join(GlobalFundRegions_list, by=c("iso3"="ISO3")) %>%
  filter(!is.na(GlobalFundRegion)) %>%
  # select(-(GlobalFundRegion)) %>%
  group_by(year,GlobalFundRegion) %>%
  summarise_if(is.numeric,sum,na.rm = TRUE) %>%
  filter(!is.na(year)) %>%
  rename(iso3=GlobalFundRegion) %>%
  mutate(year=as.numeric(year)) %>%
  ungroup()
  ),

# malaria eligible
pip_data %>%
  filter(elig_mal==1) %>%
  group_by(iso3) %>%
  select(iso3,year,contains("MA")) %>%
  right_join(GlobalFundRegions_list, by=c("iso3"="ISO3")) %>%
  filter(!is.na(GlobalFundRegion)) %>%
  # select(-(GlobalFundRegion)) %>%
  group_by(year,GlobalFundRegion) %>%
  summarise_if(is.numeric,sum,na.rm = TRUE) %>%
  filter(!is.na(year)) %>%
  rename(iso3=GlobalFundRegion) %>%
  mutate(year=as.numeric(year)) %>%
  ungroup())


# check for blanks if needed
  # table(regions_pip_data$iso3)



#  STEP 5 summarise to 5 regions  ####

depts_pip_data <-
  full_join(
    full_join(
      # hiv eligible
      pip_data %>%
        filter(elig_hiv==1) %>%
        group_by(iso3) %>%
        select(iso3,year,contains("HI")) %>%
        right_join(GlobalFundRegions_list, by=c("iso3"="ISO3")) %>%
        filter(!is.na(GlobalFundDepartment)) %>%
        # select(-(GlobalFundDepartment)) %>%
        group_by(year,GlobalFundDepartment) %>%
        summarise_if(is.numeric,sum,na.rm = TRUE) %>%
        filter(!is.na(year)) %>%
        rename(iso3=GlobalFundDepartment) %>%
        mutate(year=as.numeric(year)) %>%
        ungroup(),

      # TBeligible
      pip_data %>%
        filter(elig_tb==1) %>%
        group_by(iso3) %>%
        select(iso3,year,contains("TB")) %>%
        right_join(GlobalFundRegions_list, by=c("iso3"="ISO3")) %>%
        filter(!is.na(GlobalFundDepartment)) %>%
        # select(-(GlobalFundDepartment)) %>%
        group_by(year,GlobalFundDepartment) %>%
        summarise_if(is.numeric,sum,na.rm = TRUE) %>%
        filter(!is.na(year)) %>%
        rename(iso3=GlobalFundDepartment) %>%
        mutate(year=as.numeric(year)) %>%
        ungroup()
    ),

    # malaria eligible
    pip_data %>%
      filter(elig_mal==1) %>%
      group_by(iso3) %>%
      select(iso3,year,contains("MA")) %>%
      right_join(GlobalFundRegions_list, by=c("iso3"="ISO3")) %>%
      filter(!is.na(GlobalFundDepartment)) %>%
      # select(-(GlobalFundDepartment)) %>%
      group_by(year,GlobalFundDepartment) %>%
      summarise_if(is.numeric,sum,na.rm = TRUE) %>%
      filter(!is.na(year)) %>%
      rename(iso3=GlobalFundDepartment) %>%
      mutate(year=as.numeric(year)) %>%
      ungroup())

# join dept and regions together 
regions_pip_data <-
  full_join(depts_pip_data,regions_pip_data)

# regional sums matching to this point pip_data2023-01-31_for regional checks.xlsx


#  STEP 6 join country and region level n / ds ] ####

pip_data <- 
  full_join(pip_data, regions_pip_data)
# regional sums matching to this point pip_data2023-01-31_for regional checks.xlsx

#  STEP 7 add 6 year averages for n/d s at both country and region level  ####

# separate dfs needed for each disease
pip_data_6ya_m <-# malaria eligible-  all indicators use same 6 year period
  pip_data %>%
  filter(elig_mal==1| nchar(iso3>3)) %>% 
  group_by(iso3) %>% 
  select(iso3,year,contains("MA")) %>% 
  filter(year>=av_start_year & year <=av_end_year) %>% 
  summarise(across(where(is.numeric),sum,na.rm=TRUE)) %>%
  mutate(across(where(is.numeric), ~.x/6,na.rm=TRUE)) %>%
  ungroup() %>% 
  mutate(year=paste0(av_start_year,"-",av_end_year))
  
  
pip_data_6ya_h <-
    full_join(
    
    # hiv eligible - all indicators use same 6 year period except HIV211
      pip_data %>%
      filter(elig_hiv==1 | nchar(iso3>3)) %>% #use nchar to also keep in regional sums for single years calculated in previous step
      group_by(iso3) %>% 
      select(iso3,year,contains("HI")) %>% 
      select(-c("HIV211")) %>% 
      filter(year>=av_start_year & year <=av_end_year) %>% 
      summarise(across(where(is.numeric),sum,na.rm=TRUE)) %>%
      mutate(across(where(is.numeric), ~.x/6,na.rm=TRUE)) %>%
      ungroup() %>% 
      mutate(year=paste0(av_start_year,"-",av_end_year))
      ,
      # HIV211
      pip_data %>%
        filter(elig_hiv==1 | nchar(iso3>3)) %>% #use nchar to also keep in regional sums for single years calculated in previous step
        group_by(iso3) %>% 
        select(iso3,year,HIV211) %>% 
        filter(year>=lag1_av_start_year & year <=lag1_av_end_year) %>% 
        summarise(across(where(is.numeric),sum,na.rm=TRUE)) %>%
        mutate(across(where(is.numeric), ~.x/6,na.rm=TRUE)) %>%
        ungroup() %>% 
        mutate(year=paste0(lag1_av_start_year,"-",lag1_av_end_year))
               )
      
    # tb eligible all indicators except those needing lagging 
pip_data_6ya_t <- 
      full_join(
      full_join(     
      pip_data %>%
      filter(elig_tb==1| nchar(iso3>3)) %>% 
      group_by(iso3) %>% 
      select(iso3,year,contains("TB"),-c("TB289","TB285","TB273","TB269")) %>% 
      filter(year>=av_start_year & year <=av_end_year) %>% 
      summarise(across(where(is.numeric),sum,na.rm=TRUE)) %>%
      mutate(across(where(is.numeric), ~.x/6,na.rm=TRUE)) %>%
      ungroup() %>% 
      mutate(year=paste0(av_start_year,"-",av_end_year)),
      
      # tb eligible all indicators lagging by t-1 for p_tb_tx_succ
      pip_data %>%
        filter(elig_tb==1| nchar(iso3>3)) %>% 
        group_by(iso3) %>% 
        select(iso3,year,TB289,TB285,elig_tb) %>% 
        filter(year>=lag1_av_start_year & year <=lag1_av_end_year) %>% 
        summarise(across(where(is.numeric),sum,na.rm=TRUE)) %>%
        mutate(across(where(is.numeric), ~.x/6,na.rm=TRUE)) %>%
        ungroup() %>% 
        mutate(year=paste0(lag1_av_start_year,"-",lag1_av_end_year))),
      
      # tb eligible all indicators lagging by t-1 for p_mdr_tb_tx_succ
      pip_data %>%
        filter(elig_tb==1| nchar(iso3>3)) %>% 
        group_by(iso3) %>% 
        select(iso3,year,TB273,TB269,elig_tb) %>% 
        filter(year>=lag2_av_start_year & year <=lag2_av_end_year) %>% 
        summarise(across(where(is.numeric),sum,na.rm=TRUE)) %>%
        mutate(across(where(is.numeric), ~.x/6,na.rm=TRUE)) %>%
        ungroup() %>% 
        mutate(year=paste0(lag2_av_start_year,"-",lag2_av_end_year))
        )
   

# join together the temporary tb + hiv/mal dataframes

pip_data_6ya <- 
full_join(  
full_join(pip_data_6ya_t,pip_data_6ya_m),pip_data_6ya_h)

pip_data <- full_join(pip_data %>% mutate(year=as.character(year)),pip_data_6ya)

# checks 
pip_data %>% filter(iso3=="TZA"
            |iso3=="Central Africa")

pip_data %>% filter(iso3=="TZA" & (year =="2016-2021"| year =="2015-2020" | year =="2014-2019"))

#  STEP 8 calculate coverage from n / ds at 1. country, 2. country 6ya, 3. department , 4. department6ya , 5. region, 6. region 6ya  ####
  

  #first make df long

pip_data <-
pip_data %>%
  select(-(contains("elig"))) %>% 
  pivot_longer(names_to = "name",values_to = "value",3:last_col())
   
# first  process mal and hiv  calculated fields in a new dataframe
mh <-
  full_join(
   
    pip_data %>% 
     mutate(id=substr(name,1,2)) %>% 
      filter(id=="Ma" & year==2021 |
             id=="Po" & year==2021 |
             id=="Ma"& year=="2016-2021") %>% 
     select(-id) %>% 
      pivot_wider(names_from = name,values_from = value) %>%
      mutate(p_mal_parasitic=((Malaria39+Malaria41)/Malaria45)*100) %>% 
      mutate(p_iptp_3=(Malaria330*Malaria327/100)/(Malaria454*Malaria327/100)) %>%
      mutate(p_itn_access=(Malaria186_n/Malaria32)*100) %>% 
      mutate(p_itn_use=(Malaria189_n/Malaria32)*100)
      ,
      pip_data %>% 
      mutate(id=substr(name,1,2)) %>% 
      filter(id=="HI" & year==2021 |
             id=="Po" & year==2021 |
              id=="HI" & year=="2016-2021") %>% 
      select(-id) %>% 
      pivot_wider(names_from = name,values_from = value) %>% 
      mutate(p_art_cov=(HIV163/HIV46)*100) %>% 
      mutate(p_vls_sup=(HIV437/HIV46)*100) %>% 
      mutate(p_kos=(HIV440/HIV46)*100) %>% 
      mutate(p_pmtct_cov=(HIV186/HIV183)*100)   
      )

# then process tb calculated fields in order of year used for calculation

t <- 
  full_join(
    full_join(  
      # Tb calculated fields that use latest year (2021)
      pip_data %>% 
        mutate(id=substr(name,1,2)) %>% 
        filter(id=="TB" & year==2021 |
                 id=="Po" & year==2021 |
                 id=="TB" & year=="2016-2021") %>%
        select(-(id)) %>% #drop the id col
        pivot_wider(names_from = name,values_from = value) %>%
        # mutate(p_tb_tx_succ=TB273/TB269*100) %>%
        mutate(p_tb_hiv_art=(TB190/TB202)*100)  %>% 
        mutate(p_hiv_tb_tpt=ifelse(is.na(TB643/TB642),(TB648/TB656)*100,(TB643/TB642)*100)) %>% 
        mutate(p_tb_tx_cov=(TB81/TB11)*100) %>% 
        mutate(p_mdr_tx=ifelse(TB_n_mdr_tx/TB_d_mdr_tx>1,1,TB_n_mdr_tx/TB_d_mdr_tx)) 
      
      ,
      
      # Tb calculated fields that use t-1
      # 2020  
      pip_data %>% 
        mutate(id=substr(name,1,2)) %>% 
        filter(id=="TB" & year==2020|
               id=="TB" & year=="2015-2020") %>% 
        select(-(id)) %>% #drop the id col
        mutate(value=replace_na(value,0)) %>% 
        pivot_wider(names_from = name,values_from = value) %>% 
        select(iso3,year,TB289,TB285) %>% 
        mutate_all(.,~  replace_na(.,0)) %>% 
        # mutate(p_art_tb_ipt=((TB643+TB683)/TB646)*100) %>% 
        mutate(p_tb_tx_succ=(TB289/TB285)*100)  
        # mutate(p_art_tb_ipt=ifelse(is.nan(p_art_tb_ipt),NA,p_art_tb_ipt)) %>% 
        # mutate(p_art_tb_ipt=ifelse(is.infinite(p_art_tb_ipt),NA,p_art_tb_ipt)) %>% 
        # select(iso3,year, TB289,TB285,TB643,TB683,TB646,
               # p_art_tb_ipt,
               # p_tb_tx_succ)
    ), #no duplication of year to here
    
    # tb calculated fields that use t-2 
    pip_data %>% 
      # right_join(elig_lookup %>% filter(tuberculosis=="Yes") %>% select(iso3)) %>% 
      mutate(id=substr(name,1,2)) %>% 
      filter(id=="TB" & year=="2014-2019"|
               id=="TB" & year ==2019) %>% 
      select(-(id)) %>% #drop the id col
      pivot_wider(names_from = name,values_from = value) %>% 
      select(iso3,year,TB273,TB269) %>% 
      mutate(p_mdr_tb_tx_succ=(TB273/TB269)*100) 
      
      # select(iso3,year,p_mdr_tb_tx_succ,TB273,TB269)  
  ) 
  # mutate(year=as.numeric(year))

# do not join by Population 1 which would create duplicate year records
pip_data <- 
        full_join(mh,t, by=c("iso3","year")) %>%
        select(-(contains("Population"))) %>% 
        pivot_longer(names_to = "name",values_to = "value",3:last_col())



# 
# #  STEP 9 produce outputs ####
#   
# # reduce to indicators to display indicators and  years

# rename n_mdr_tx at this point before filtering to list

pip_data <-
pip_data %>% 
  mutate(name=ifelse(name=="TB_n_mdr_tx","n_mdr_tx",name))  %>% 
  # also remove OST regional summs as these are not accurate as n/ds are not published HIV893
  mutate(Value=ifelse(name=="HIV893" & nchar(iso3)>3,NA,value)) 


# apply the list of specified years and indicators to display
pip_data <-
pip_data %>% 
  mutate(id=paste0(name,year)) %>% 
  right_join(indicator_year_list_db_2) 


# add back in eligibility criteria
pip_data <- 
pip_data %>% 
  left_join(elig_lookup %>% 
              mutate(across(everything(), ~replace(.,. =="Yes", "1")))) %>%
  select(-eligible) %>% 
  rename(elig_hiv=`hiv/aids`,elig_tb=tuberculosis,elig_mal=malaria) 

# filter to eligible countries and all regional records
pip_data <-
full_join(
full_join(
full_join(
  # regional records
  pip_data %>% 
  filter(nchar(iso3)>3),
 
  # hiv eligible countries 
  pip_data %>% filter(component=="HIV" & elig_hiv==1)
  ),
 # tb eligible countries 
  pip_data %>% filter(component=="TB" & elig_tb==1)
),
# malaria eligible countries 
  pip_data %>% filter(component=="Malaria" & elig_mal==1)
) %>% 
  select(-c(contains("elig")))


# rename columns to match Mikaela's output
pip_data <-
pip_data %>%
#   right_join(indicator_year_list_db_2 %>% select(component,source))%>% 
  rename_with(str_to_title) %>%
  rename(ISO3=Iso3) %>% 
  mutate(Category="Actual") %>% 
  mutate(DataType="Point") %>% 
  select(ISO3,Year,Category,Name,Value,Source,Component,DataType)
  
  # mikaela names 
# ISO3	Year	Category	Name	Value	Source	Component	TargetSwap	DataType	Indicator	Position




# write out the final csv

 write.csv(pip_data,
           file = paste0(out,"historical_data_db_2_",Sys.Date(),".csv"), row.names = FALSE, na="")
      
      

      
      
      
      
     
       
          



    
    