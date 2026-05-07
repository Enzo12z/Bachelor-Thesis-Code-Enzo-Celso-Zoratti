install.packages("mediation")
install.packages("corrplot")
install.packages("tidyverse") 
install.packages("ggplot2")
install.packages("lmtest")
install.packages("psych")
library(mediation)
library(sandwich)
library(lmtest)
library(data.table)
library(corrplot)
library(tidyverse)
library(haven)
library(psych) #for cronbachs alpha


#------oversight Data and methods used in my code w/  sources
## Data preparation:  Wickham et al. (2019)
# Mediation analysis: Tingley et al. (2014)
# ParlGov election data: Döring & Manow (2023)

#internet sources directly linked via url for code snippets 


# -- Causal mediation analysis ---
# Tingley et al. (2014) - mediation package 
#library(mediation)






####################################################


#start. :')


#-----TREAMTMENT Variable

#>> Right wing vote share : populist: treshhold = 7;  right-wingc = 6 :: columN nr.11 : left_right 
election  <- read.csv("view_election.csv") #!!! Every COLUM = 1 ELECTION ENTRY!! (different starting points but ~early 1900s)

european_countries <- c(
  "Austria", "Croatia", "Denmark", "Finland", "France",
  "Germany", "United Kingdom", "Hungary", "Iceland", "Italy",
  "Lithuania", "Norway", "Slovakia", "Slovenia",
  "Spain", "Sweden", "Switzerland")

election_eu <- election %>%
  filter(country_name %in% european_countries)

#------------
nrow(election)          #all parties in all elections (incl non-european)
nrow(election_eu)       #European parties only 
table(election_eu$country_name)        # parties per country
summary(election_eu$left_right)       #distribution of scores left_right (colum 11 dataset ->view_election(ParlGov))
#NA = every election run where no score was given to parties (e.g. one country with NA = 8 means it ran 8 times:don't confound NA = 432 with 432 single 
#"handling missing values in r" >> v helpful: https://www.r-bloggers.com/2021/04/handling-missing-values-in-r/

hist(election_eu$left_right)

#------#filter election -> right wing parties (right wing & right-wing populist parties rwpp) https://stackoverflow.com/questions/45884261 / https://www.statology.org/dplyr-filter-multiple-conditions/
rw_8 <- election_eu %>%filter(election_type == "parliament",
                              !is.na(left_right),
                              left_right >= 8)           #fiters out for parliament elections with available left_right data (NO N.A) and parties with >- 8 entry (here rw)




#rw_8 vote share per country 2010& 2020

rwpp8_voteshare2010 <- election_eu %>%
  filter(election_type == "parliament",       #Filters for: parliament election; available left_right and vote_share entries & right-wing (treshold of 6)
         !is.na(left_right),!is.na(vote_share),
         left_right >= 8,as.Date(election_date) <= as.Date("2010-12-31")) %>%       #and filter for date (till end of 2020; see slice_max beneath that works now)
  group_by(country_name, election_date) %>%                      #one country per one election grouped
  summarise(rw_vote = sum(vote_share)) %>%                   #combining right wing vote share
  group_by(country_name) %>%
  slice_max(election_date, n = 1) %>%                #see slice vs filter. (slice = position in dataset) & (filter = condition for dataset)
  ungroup()                                 #to be able to summarize etc later without r using groups 

rwpp8_voteshare2020 <- election_eu %>%
  filter(election_type == "parliament",    
         !is.na(left_right),!is.na(vote_share),
         left_right >= 8,as.Date(election_date) <= as.Date("2020-12-31")) %>%  
  group_by(country_name, election_date) %>% 
  summarise(rw_vote = sum(vote_share)) %>% 
  group_by(country_name) %>%
  slice_max(election_date, n = 1) %>% 
  ungroup()  




#----- OUTCOME Variable & Mediator

#2010: Column names 
#v29 >> Pay mich higher prices (Q12a)
#v30 >> Pay much hgiher taxes (Q12b)
#v31 >> cut standard of living (Q12c)

#2020: Column names
#v26 >> pay much higher prices (Q11a)
#v27 >> pay much higher taxes (Q11b)
#v28 >> cut standard of livimg (Q11c)

#Mediator
#v14 >> Trust in institutions: Country nationality parliament (Q5d)



issp2010 <- read_sav("ZA5500(2010Env3)_v3-0-0.sav")
issp2020 <- read_sav("ZA7650(2020Env4)_v2-0-0.sav")

issp2010 <- issp2010 %>%  
  mutate(across(c(v29, v30, v31), ~ ifelse(. %in% 1:5, ., NA))) #keeps values 1 till 5: all other values will be NA https://stackoverflow.com/questions/74122648/recode-missing-values-in-multiple-columns-mutate-with-across-and-ifelse (see questionnaire; Scaling: Very willing = 1 -> Very unwilling = 5: No answer = 8)

issp2020 <- issp2020 %>%
  mutate(across(c(v26, v27, v28), ~ ifelse(. %in% 1:5, ., NA))) #""

issp2020 <- issp2020 %>%
  mutate(trust_parl = ifelse(v14 %in% 0:10, v14, NA)) #Scale 1-10 in ISSP Env iV



issp2010 <- issp2010 %>%
  mutate(across(c(v29, v30, v31), ~ 6 - .)) #reverse the (reverse) issp scale : see https://stackoverflow.com/questions/26877917/reverse-scoring-items

issp2020 <- issp2020 %>%
  mutate(across(c(v26, v27, v28), ~ 6 - .)) #""




#---- WTP measuring index
issp2010 <- issp2010 %>%mutate(wtp_index = rowMeans(cbind(v29, v30, v31), na.rm = TRUE)) #https://019b2dbc-0ac6-3185-aa9f-0bea50e3da6d.share.connect.posit.cloud/merge-join.html & see "handling missing values in R > calculating average"
issp2020 <- issp2020%>% mutate(wtp_index = rowMeans(cbind(v26, v27, v28), na.rm = TRUE))

alpha2010 <- alpha(issp2010[, c("v29", "v30", "v31")])
alpha2020 <- alpha(issp2020[, c("v26", "v27", "v28")])

print(alpha2010)
print(alpha2020)

#---applying right wing treatment

issp2020 <- issp2020 %>%
  mutate(wtp_index = rowMeans(cbind(v26, v27, v28), na.rm = TRUE),
         country_name = case_when(country == 756 ~ "Switzerland",
                                  country == 352 ~ "Iceland",
                                  country == 724 ~ "Spain",
                                  country == 40  ~ "Austria",
                                  country == 250 ~ "France",
                                  country == 440 ~ "Lithuania",
                                  country == 191 ~ "Croatia",
                                  country == 705 ~ "Slovenia",
                                  country == 578 ~ "Norway",
                                  country == 703 ~ "Slovakia",
                                  country == 276 ~ "Germany",
                                  country == 380 ~ "Italy",
                                  country == 246 ~ "Finland",
                                  country == 752 ~ "Sweden",
                                  country == 348 ~ "Hungary",
                                  country == 208 ~ "Denmark",
                                  TRUE ~ NA_character_))

issp2020_analysed <- issp2020 %>% left_join(rwpp8_voteshare2020, by ="country_name") #for robustness check change to rw_voteshare2020 and same beneath

#repeat same with 2010

issp2010 <- issp2010 %>%
  mutate(wtp_index = rowMeans(cbind(v29, v30,v31), na.rm = TRUE),
         country_name = case_when(country == 756 ~ "Switzerland",
                                  country == 352 ~ "Iceland",
                                  country == 826 ~ "United Kingdom",
                                  country == 724 ~ "Spain",
                                  country == 40  ~ "Austria",
                                  country == 250 ~ "France",
                                  country == 440 ~ "Lithuania",
                                  country == 191 ~ "Croatia",
                                  country == 705 ~ "Slovenia",
                                  country == 578 ~ "Norway",
                                  country == 703 ~ "Slovakia",
                                  country == 276 ~ "Germany",
                                  country == 380 ~ "Italy",
                                  country == 246 ~ "Finland",
                                  country == 752 ~ "Sweden",
                                  country == 348 ~ "Hungary",
                                  country == 208 ~ "Denmark",
                                  TRUE ~ NA_character_))

issp2010_analysed <- issp2010 %>%left_join(rwpp8_voteshare2010, by = "country_name")


issp2010_analysed <- issp2010_analysed %>%filter(!is.na(country_name))
issp2020_analysed <- issp2020_analysed %>%filter(!is.na(country_name))

head(names(issp2010_analysed), 10)
tail(names(issp2020_analysed), 10)



#-------------
#controls
#2010
issp2010_analysed <- issp2010_analysed %>%
  mutate(
    age    = ifelse(AGE %in% 15:99, AGE, NA),
    female = ifelse(SEX %in% 1:2, SEX - 1, NA),  # dummy fro 0 = male& 1 = female
    educ   = ifelse(EDUCYRS %in% 0:25, EDUCYRS, NA),
    urban  = ifelse(URBRURAL %in% 1:5, URBRURAL, NA))

#2020
issp2020_analysed <- issp2020_analysed %>%
  mutate(
    age    = ifelse(AGE %in% 15:99, AGE, NA),
    female = ifelse(SEX %in% 1:2, SEX - 1, NA),     
    educ   = ifelse(EDUCYRS %in% 0:25, EDUCYRS, NA),
    urban  = ifelse(URBRURAL %in% 1:5, URBRURAL, NA))

#check the added controls again
tail(names(issp2010_analysed), 10)
tail(names(issp2020_analysed), 10)


#main analysis -----
med_data <- na.omit(issp2020_analysed[, c("wtp_index", "trust_parl", "rw_vote",        #>>resolves "number of observations do not match" bc it removes missing values to create identical rows // see Imai et al  (2010, p 139)  &&& *pasted up here to also make model1 comparable (by using both same data set for year 2020)
                                          "age", "female", "educ", "urban", "country_name")])


#2010
modelCheck <- lm(wtp_index ~ rw_vote + age + female + educ + urban, data = issp2010_analysed)
coeftest(modelCheck, vcov = vcovCL(modelCheck, cluster = ~country_name))    #apply clustered robust standard errors  within countries observations are different for each country >> heteroskedasticity is probable


#2020
model1 <- lm(wtp_index ~ rw_vote + age + female + educ + urban,data = med_data)
coeftest(model1, vcov = vcovCL(model1, cluster = ~country_name))


#---------
#MEDIATION
library(mediation)

set.seed(2026)

med_data <- na.omit(issp2020_analysed[, c("wtp_index", "trust_parl", "rw_vote",        #>>resolves "number of observations do not match" bc it removes missing values to create identical rows // see Imai et al  (2010, p 139)
                                          "age", "female", "educ", "urban", "country_name")])

count(med_data) #check how many obs left after list deletion 
nrow(med_data)


med.fit <- lm(trust_parl ~ rw_vote + age + female + educ + urban, data = med_data)     #1) Mediator model: regessing parl_trust on rw_vote here (and on controls) -> model Y (trust_parl) as function of X (rw_vote) ,, with controls

out.fit <- lm(wtp_index ~ trust_parl + rw_vote + age + female + educ + urban,  data = med_data)   #2)Output model: regressing wtp on mediator (trust) and rw_vote -> 




med.out <- mediation::mediate(med.fit, out.fit,
                              treat    = "rw_vote",
                              mediator = "trust_parl",
                              cluster  = med_data$country_name,
                              sims     = 1000)
summary(med.out)



#vfinal
