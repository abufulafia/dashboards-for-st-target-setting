
source("~/__SI/_R_scripts_git/richards_functions_June_2019.R")
loadmypackages()


#  a. ABOUT ####
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

#  b. SET UP WORKING DIRECTORIES, SPECIFY ANY ADDITIONAL PACKAGES ####


out <- paste0(getwd(),"/out/")  
in_ <- paste0(getwd(),"/in_/")

col_names <- c("component", #general #hiv #TB #malaria
               "category", #population #disease burden #service coverage #outcome #Service coverage/outcome
               "indicator",
               "source", #source including year 
               "country",
               "year",
               "name", # indicator code
               "value")

#  c. DEFINE SWITCHBOARD PARAMETERS TO UPDATE EACH USE OF SCRIPT 
#  STEP 1 . READ IN SOURCE FILES, LOOK UP TABLES ETC ####

#   1.a get country and iso3 names ####
# geog <- GlobalFundr::extractGeographyReference() %>% select(ISO3,GeographyName)
# get geog from global environment or if not there, from online


geog <- read.csv("~/__SI/_2023-2028 Strategic target setting/partner input data for CT review tool/data/out/geog2022-11-22.csv")

#   1.b eligibility lookup table  ####
# elig_lookup <- extractEligibilityList(year = 2022) %>%
#                 rename_with(tolower) %>% 
#                 select(iso3,component,eligibility) %>% 
#                 spread(key = component,value = eligibility) %>% 
#                 rename_with(tolower) %>% 
#                 mutate(tuberculosis=ifelse(tuberculosis=="Transition funding", "Yes", tuberculosis)) %>% 
#                 mutate(eligible=ifelse(`hiv/aids`=="Yes"|tuberculosis=="Yes"|malaria=="Yes","Yes",NA))
elig_lookup <- read_csv("out/elig_lookup2022-11-22.csv")

#   1.c get key populations data  ####
key_pops <-
  read_excel(paste0(in_,"Key pops data 2021 for GF.xlsx"), skip = 1, range="a2:j108") %>% 
  rename_with(tolower) %>% 
  rename(iso3=iso_a3) %>% 
  select(2,3:5,8:10)  #take the GOALS size estimates

#   1.d get key populations look up table  ####
# create a names look up table for the key populations definitions 
 key_pops_lookup <- 
  data.frame(
    name=c("sw_d",
           "msm_d",
           "pwid_d",
           "sw_n",
           "msm_n",
           "pwid_n",
           "p_sw_cov",
           "p_msm_cov",
           "p_pwid_cov"
    ),
    indicator=c("# Sex Workers population estimate",
          "# men who have sex with men population estimate",
          "# people who inject drugs population estimate",
          "# of sex workers reached with prevention programs",
          "# of men who have sex with men reached with prevention programs",
          "# of people who inject drugs reached with prevention programs",
          "% of sex workers reached with prevention programs",
          "% of men who have sex with men reached with prevention programs",
          "% of people who inject drugs reached with prevention programs"
    )
  )

#   1.e get VMMC intervention numbers  ####
vmmc <- read_excel(paste0(in_,"VMMC.xlsx"), skip = 1) %>% 
  select(Country,`2021`) %>% 
  rename_with(tolower) %>% 
  head(15) %>% 
  mutate(country=ifelse(country=="Tanzania", "Tanzania (United Republic)",country))

#   1.f get VMMC population number table ####
vmmc_d <- 
  read_excel(paste0(in_,"Testing and male pop 10_29.xlsx"), sheet = 2, range = "A2:L17") %>% 
  select(Country,`2021`) %>% 
  rename_with(tolower) %>% 
  mutate(country=ifelse(country=="United Republic of Tanzania","Tanzania (United Republic)",country)) %>% 
  mutate(country=ifelse(country=="Swaziland","Eswatini",country))

#   1.g get PIP data ####
pip <-
  # can use offline copy of pip for speed
  # read.csv("~/__SI/_2023-2028 Strategic target setting/partner input data for CT review tool/data/out/unprocessed_pip_2022-11-22.csv")

  extractPIP(indicators = c("Population1",
                            "HIV1","HIV121","HIV163","HIV46","HIV186","HIV183",
                            "HIV211","HIV437","HIV440","HIV215",
                            "TB645","TB11","TB58","TB64","TB1",
                            "TB81","TB190","TB202","TB289","TB285","TB85","TB239","TB269","TB285",
                            "TB273","TB289","TB641","TB639","TB643","TB683","TB646","TB642","TB648","TB656",
                            "Malaria1", "Malaria32","Malaria7", # Malaria32 source annex 5G WMR col D ' At risk low and high'
                            "Malaria24", "Malaria48",
                            "Malaria39","Malaria41", "Malaria45","Malaria31","Malaria189","Malaria330","Malaria327",
                            "Malaria186","Malaria445","Malaria325","Malaria343", "Malaria454", "Malaria325"
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
  filter(year<=2021&year>=2019)

# for reference / audit write out a copy of the source data file
write.csv(pip,file = paste0(out,"unprocessed_pip_",Sys.Date(),".csv")
          , row.names = FALSE, na="NA")

# separately read in VMMC coverage
vmmc_pip <- extractPIP(indicators = c("HIV215")) %>%
  select(ISOCountryCode,Year,
         # ActivityAreaName,
         ActivityAreaIndicatorCode,Percentage,Numerator,Denominator) %>%
  mutate(value=ifelse(is.na(Numerator),Percentage,Numerator)) %>%
  select(-c(Numerator,Denominator,Percentage)) %>%
  rename(iso3=ISOCountryCode) %>%
  rename(year=Year) %>%
  rename(name=ActivityAreaIndicatorCode) %>%
  rename_with(tolower) %>%
  filter(year<=2021&year>=2015)


# separately read in survey estimates of LLIN acces and LLIN use (variable years so add dataframe last)
llins_pip <- extractPIP(indicators = c("Malaria19","Malaria21")) %>%
  select(ISOCountryCode,Year,
         # ActivityAreaName,
         ActivityAreaIndicatorCode,Percentage,Numerator,Denominator) %>%
  mutate(value=ifelse(is.na(Numerator),Percentage,Numerator)) %>%
  select(-c(Numerator,Denominator,Percentage)) %>%
  rename(iso3=ISOCountryCode) %>%
  rename(year=Year) %>%
  rename(name=ActivityAreaIndicatorCode) %>%
  rename_with(tolower) %>%
  filter(year<=2021&year>=2015) 


#   1.h get data dictionary  ####
# read in the single data dictionary
partner_data_lookup <- read_csv(paste0(in_,"partner_data_lookup.csv"))

#   1.i get performance framework extract prepared by Mikaela  ####
pf_df <- 
  read.csv(paste0(in_,"CT_input_PF_HF.csv")) %>% 
  left_join(geog, by=c("iso3"="ISO3")) %>% 
  rename(country=GeographyName) %>% 
  select(iso3,year,country,component,category,source,name,value,indicator,position)
  colnames(pf_df)
  
#   1.j get WHO Malaria modeled bednets coverage ####
# survey based indicators dropped or final run
# mal_pip <-
#   read_excel("in_/WMR2021_Annex5D.xlsx", na = c("-"), skip = 2, ) %>% 
#   select(1,2,4) %>% 
#   filter(row_number()!=c(1,143,207,232,260)) %>%  #remove regional headings
#   rename(country=1) %>% 
#   rename(year=2) %>% 
#   rename(p_llin_use=3) %>% #modeled version from MAP published in WMR
#   mutate(country=ifelse(country=="United Republic of Tanzania2",
#                         "Tanzania (United Republic)",country)) %>% 
#   mutate(country=ifelse(country=="Democratic Republic of the Congo",
#                         "Congo (Democratic Republic)",country)) %>% 
#   # mutate(country=ifelse(country=="Côte d'Ivoire",
#   #                       "Côte d'Ivoire",country)) %>%
#   mutate(country=ifelse(country=="South Sudan1",
#                         "South Sudan",country)) %>%
#     fill(1,.direction="down") %>% 
#   filter(!is.na(p_llin_use)) %>% 
#   mutate(year=as.numeric(year)) %>% 
#   group_by(country) %>% 
#   # take the latest available year for each country
#   top_n(1,year) %>%
#   left_join(geog, by=c("country"="GeographyName"))  %>% 
#   ungroup() %>% 
#   rename_with(tolower) %>% 
#   select(iso3,year,p_llin_use) %>% 
#   mutate(iso3=ifelse(is.na(iso3),"CIV",iso3))
#   
  

#  End of Switchboard: Items below the switchboard line should be able to run without modification ####
#   1.k get list of indicators/years for end user display ####


# read in list of indicators and year to display in user version (indicators not listed here will be droped )
indicator_year_list <- as.data.frame(read_csv("in_/indicator_year_list.csv",col_names = TRUE))

#  STEP 2 process Keypops data ####
key_pops <-
  key_pops %>%
  left_join(geog, by= c("iso3"="ISO3")) %>%
  select(-(GeographyName)) %>% 
  rename(sw_d=2) %>%
  rename(msm_d=3) %>% 
  rename(pwid_d=4) %>% 
  rename(sw_n=5) %>% 
  rename(msm_n=6) %>% 
  rename(pwid_n=7) %>%  
  # calculate coverage by key population 
  mutate(p_sw_cov=(sw_n/sw_d)*100) %>% 
  mutate(p_msm_cov=(msm_n/msm_d)*100) %>%
  mutate(p_pwid_cov=(pwid_n/pwid_d)*100) %>% 
  # cap at 100%
  mutate(p_sw_cov=ifelse(p_sw_cov>100,100,p_sw_cov)) %>%
  mutate(p_msm_cov=ifelse(p_msm_cov>100,100,p_msm_cov)) %>%
  mutate(p_pwid_cov=ifelse(p_pwid_cov>100,100,p_pwid_cov)) %>%
  select(iso3,everything()) %>% 
  # gather(key=name,value=value,3:last_col()) %>% 
  mutate(year=2021) %>% 
  mutate(year=as.numeric(year)) 
                                                                              
#  STEP 3 process VMMC data  ####
vmmc <-
full_join(
  vmmc %>%
  left_join(geog, by= c("country"="GeographyName")) %>%
  rename(vmmc_n=`2021`),
  vmmc_d %>% 
  rename(vmmc_d=`2021`)
  ) %>% 
  # mutate(p_vmmc_cov=vmmc_n/vmmc_d*100) %>%
  # mutate(p_vmmc_cov=ifelse(p_vmmc_cov>100,100,p_vmmc_cov)) %>% 
  select(ISO3,everything()) %>% 
  gather(key=name,value = value,3:last_col()) %>% 
  mutate(year=2021) %>% 
  mutate(year=as.numeric(year)) %>% 
  rename_with(tolower) %>% 
  select(-(country)) %>% 
  spread(key=name,value=value)
  


#  STEP 4 join VMMC and keypops data [ct_partner_data] ####
# join in wide format to create NAs
ct_partner_data <-
    full_join(vmmc, key_pops) 
  


#  STEP 5 process PIP data [3 temp data frames]####

  # first  process mal and hiv  calculated fields in a new dataframe
  mh <- 
    full_join(
      pip %>% 
        right_join(elig_lookup %>% filter(malaria=="Yes") %>% select(iso3)) %>% 
        mutate(id=substr(name,1,2)) %>% 
        filter(id=="Ma" & year==2020 |id=="Po" & year==2020) %>% 
        filter(year==2020) %>% 
        select(-id) %>% 
        pivot_wider(names_from = name,values_from = value) %>% 
        mutate(p_mal_parasitic=((Malaria39+Malaria41)/Malaria45)*100) %>% 
        mutate(p_mal_par= (Malaria32/Population1)*100) %>% 
        mutate(p_iptp3_all=
                 ((Malaria330/100*Malaria327)/(Malaria454/100*Malaria327))*100) %>% #iptp3 in all settings not just ANC facilities
        mutate(year=as.numeric(year))
      ,
      
      pip %>% 
        right_join(elig_lookup %>% filter(`hiv/aids`=="Yes") %>% select(iso3)) %>% 
        mutate(id=substr(name,1,2)) %>% 
        filter(id=="HI" & year==2021 |id=="Po" & year==2021) %>% 
        filter(year==2021) %>% 
        select(-id) %>% 
        pivot_wider(names_from = name,values_from = value) %>% 
        mutate(p_art_cov=(HIV163/HIV46)*100) %>% 
        mutate(p_vls_sup=(HIV437/HIV46)*100) %>% 
        mutate(p_kos=(HIV440/HIV46)*100) %>% 
        mutate(p_pmtct_cov=(HIV186/HIV183)*100)  %>% 
        mutate(year=as.numeric(year))
    )
  
  # then process tb calculated fields in order of year used for calculation
  
  t <- 
    full_join(
    full_join(  
    # Tb calculated fields that use latest year (2021)
    pip %>% 
      right_join(elig_lookup %>% filter(tuberculosis=="Yes") %>% select(iso3)) %>% 
      mutate(id=substr(name,1,2)) %>% 
      filter(id=="TB" & year==2021 |id=="Po" & year==2021) %>%
      select(-(id)) %>% #drop the id col
      pivot_wider(names_from = name,values_from = value) %>% 
      mutate(p_tb_hiv_art=(TB190/TB202)*100)  %>% 
      mutate(mdr_tx=(TB641+TB639)) %>% 
      mutate(p_hiv_tb_tpt=ifelse(is.na(TB643/TB642),(TB648/TB656)*100,(TB643/TB642)*100))
      
    ,
    
    # Tb calculated fields that use t-1
    # 2020  
    pip %>% 
      right_join(elig_lookup %>% filter(tuberculosis=="Yes") %>% select(iso3)) %>% 
      mutate(id=substr(name,1,2)) %>% 
      filter(id=="TB" & year==2020) %>% 
      select(-(id)) %>% #drop the id col
      mutate(value=replace_na(value,0)) %>% 
      pivot_wider(names_from = name,values_from = value) %>% 
      mutate_all(.,~  replace_na(.,0)) %>% 
      # mutate(p_art_tb_ipt=((TB643+TB683)/TB646)*100) %>% 
      mutate(p_tb_tx_succ=(TB289/TB285)*100) %>% 
      # mutate(p_art_tb_ipt=ifelse(is.nan(p_art_tb_ipt),NA,p_art_tb_ipt)) %>% 
      # mutate(p_art_tb_ipt=ifelse(is.infinite(p_art_tb_ipt),NA,p_art_tb_ipt)) %>% 
      select(iso3,year, TB289,TB285,TB643,TB683,TB646,
             # p_art_tb_ipt,
             p_tb_tx_succ)
    ), #no duplication of year to here
  
    # tb calculated fields that use t-2 
    pip %>% 
    right_join(elig_lookup %>% filter(tuberculosis=="Yes") %>% select(iso3)) %>% 
    mutate(id=substr(name,1,2)) %>% 
    filter(id=="TB" & year==2019) %>% 
    select(-(id)) %>% #drop the id col
    pivot_wider(names_from = name,values_from = value) %>% 
    mutate(p_mdr_tb_tx_succ=(TB273/TB269)*100) %>% 
    select(iso3,year,p_mdr_tb_tx_succ,TB273,TB269)  
    ) %>% 
    mutate(year=as.numeric(year))
  
  # do not join by Population 1 which would create duplicate year records
  htm <- full_join(mh,t, by=c("iso3","year")) %>% select(-(contains("Population")))
  
  
  # new step add back in population 1 from PIP ####
  
  htm <-
  full_join(htm,
  pip %>%  
  mutate(year=as.numeric(year)) %>% 
  right_join(elig_lookup %>% filter(eligible=="Yes") %>% select(iso3)) %>% 
  filter(name=="Population1" & year ==2021) %>% 
  pivot_wider(names_from = name,values_from = value))
  

  
  
#  STEP 6 join the pip data to the already joined data both in wide format [ct_partner_data]####

  # 
  # indicator_year_list= c("HIV12021","HIV1212021")
  

ct_partner_data <-
    full_join(ct_partner_data,htm)

#  STEP 7 add in the display names and remove indicators and years that are not needed by end user  ####
  ct_partner_data <-
  ct_partner_data %>% 
  gather(key=name,value=value,3:last_col()) %>% 
  left_join(partner_data_lookup) %>%
  
   # specific the years to retain per indicator 
  mutate(id=paste0(name,year)) %>% 
  right_join(indicator_year_list) %>% 
    
    
  select(iso3,year,component,category,source,name,value,indicator) %>% 
  left_join(geog, by= c("iso3"="ISO3")) %>% 
  rename(country=GeographyName) %>% 
  select(iso3,year,country,component,category,source,name,value,indicator)

#  STEP 8 also add VMMC survey based coverage from pip which has variable year ####
  ct_partner_data <-
  full_join(ct_partner_data,  
  left_join(
  vmmc_pip %>% 
    mutate(component="HIV") %>% 
    mutate(category="Service coverage / Outcome") %>% 
    mutate(source= "UNAIDS Global AIDS Monitoring, Statcompiler") %>% 
    mutate(indicator="% voluntary male circumcision coverage"), 
    geog, by = c("iso3"="ISO3")) %>% 
    rename(country=GeographyName) %>% 
    mutate(year=as.numeric(year)) %>% 
    group_by(iso3) %>% 
    # filter(year==pmax(year)) %>% 
    top_n(1,year) %>% 
    ungroup() %>% 
    select(iso3,year,country,component,category,source,name,value,indicator))
    

  #  new step also add LLIN survey estimates from WHO which have variable year ####

  # not used in final version
  
  # ct_partner_data <-
    # full_join(ct_partner_data,  
    #       
    #           left_join(
    #           llins_pip %>% 
    #               mutate(component="Malaria") %>% 
    #               mutate(category="Service coverage") %>% 
    #               mutate(source= "WHO World Malaria Report 2021 data (Annex 3Ea)") %>% 
    #               mutate(indicator= ifelse(name=="Malaria21", 
    #                                        "% of population with access to an ITN",
    #                                        "% of population who slept under an ITN last night")), 
    #             geog, by = c("iso3"="ISO3")) %>% 
    #             rename(country=GeographyName) %>% 
    #             mutate(year=as.numeric(year)) %>% 
    #             select(iso3,year,country,component,category,source,name,value,indicator)
    #           )
    # 
#  STEP 9 Join to performance framework pf_df data not needed as Mikaela now adding later in the process ####
  ct_partner_data <-rbind(ct_partner_data,pf_df)




# add component eligibilty for Nobu 
  ct_partner_data <-
  ct_partner_data %>% 
  left_join(elig_lookup %>% 
  select(-(eligible))) %>% 
  rename(hiv_elig=`hiv/aids`) %>% 
  rename(tb_elig=`tuberculosis`) %>% 
  rename(malaria_elig=`malaria`) %>% 
  mutate_all(funs(str_replace(.,"Yes","1"))) %>% 
  mutate_all(funs(str_replace(.,"No","NA")))  %>% 
  filter(
         (component=="HIV" & hiv_elig ==1)|
          (component=="TB" & tb_elig==1)| 
           (component =="Malaria" & malaria_elig==1)) %>% 
  select(-contains('elig'))
  
  
# STEP 11  order according to the order in the partner data lookup file #### 
  ct_partner_data <-
  ct_partner_data %>% 
  left_join(partner_data_lookup %>% select(name,number_position,anumber_position)) %>% 
  # arrange(number_position,component) 
  arrange(anumber_position,component) %>% 
  select(-number_position) %>% 
  rename(position=anumber_position)  
  

# add a fixed to avoid triplicates of population 1
  
  ct_partner_data <- 
  ct_partner_data %>%  
    filter((component!="HIV" | position != "b1")) %>% 
    filter((component!="HIV" | position != "c1")) %>%
    filter((component!="TB" | position != "a1")) %>%
    filter((component!="TB" | position != "c1")) %>%
    filter((component!="Malaria" | position != "a1")) %>%
    filter((component!="Malaria" | position != "b1")) 
  
  
  #  STEP 10 remove non eligible components for Nobu ####
  # ct_partner_data <-rbind(ct_partner_data,pf_df)
  
#  STEP 11 write out final csv file [ct_partner_data] in long format ####

# write out final data 
write.csv(ct_partner_data,file = paste0(out,"ct_partner_data_",
                                        # Sys.Date(),
                                        ".csv")
          , row.names = FALSE, na="NA")



#  REFERENCE long form definitions TPT among PLHIV  ####



# [TPT among PLHIV - CORRECTION Erica Kufa, 18/11/2022 ]
# TB/HIV-7 Percentage of PLHIV on ART who initiated TB preventive therapy among those eligible during the reporting period
# 
# 
# 
# NUMERATOR:
#   
#   TB643: Total number of people who are on antiretroviral therapy and who started TB preventive treatment  (Assuming all started on TPT are eligible)
# 
# TB683: Total number of people who are on antiretroviral therapy and eligible for TB preventive treatment (TPT) who started TPT
# 
# 
# 
# DENOMINATOR:
#   
#   TB646: Total number of people who are on antiretroviral therapy and who are eligible for TB preventive treatment (TPT)


# [ conf_rrmdr_tx ]
# Timimi Hazim, Friday, November 18, 2022 12:15 PM
# the equivalent of conf_rrmdr_tx is the sum of conf_rr_nfqr_tx + conf_rr_fqr_tx  .
# conf_rrmdr_tx 
# mutate(rrmdr_all_tx = ifelse(rrmdr_014_tx > 0,
#                              
#                              NA,
#                              
#                              unconf_rrmdr_tx + conf_rrmdr_tx + unconf_rr_nfqr_tx + conf_rr_nfqr_tx + conf_rr_fqr_tx)) %>%



# conf_rr_fqr_tx	= TB639
# = Number of patients with laboratory-confirmed resistance to rifampicin and resistance to any fluoroquinolone (i.e. with pre-XDR-TB or XDR-TB) who started treatment for pre-XDR-TB or XDR-TB.

# conf_rr_nfqr_tx	TB641
# Number of patients with laboratory-confirmed resistance to rifampicin and with no known resistance to any fluoroquinolones who started treatment for MDR-TB. (This does not include patients treated for pre-XDR-TB or XDR-TB).



    