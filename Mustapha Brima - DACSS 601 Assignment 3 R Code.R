#Mustapha Brima    DACSS 601 Assignment 3

#The data found in the covid-responses.tab.xlsx file was a result of the 
#findings of how Italian peoples responded to the first few days living 
#in the COVID-19 Pandemic. The data displays the different opinions and 
#preferences of Italian citizens as they began to go about their regular 
#day-to day practices while being in quarantine and not being able to 
#venture outside of their homes. The Italian government had conducted 
#this study in an effort to make quarantine more suitable while having 
#its citizens follow public health guidelines as they pertained to the 
#pandemic as they would "...need interventions that make staying following 
#public health protocols more desirable" 
#(https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/1SBQCX). 
#The only values that seem to be missing in the data are names and emails 
#of each participant which were most likely kept confidential. As for 
#other aspects, the 'finish' variable only shows either ones or nothing at all, 
#while the 'UserLanguage' variable doesn't properly display the language that 
#each participant speaks. 
#I have attempted to rewrite the data for the finish and UserLanguage variables 
#so that one variable indicates whether they completed the survey or not 
#('finish') by returning TRUE instead of '1' and FALSE instead of 'NA'. While 
#the other veriable (UserLanguage), I have made to more adequately display the 
#language spoken by each participant of the survey as 'IT' would be for Italian and 
# 'EN' would be for English.


> library(readxl)
> covid_responses_tab <- read_excel("Assignment 3/covid-responses.tab.xlsx")
> View(covid_responses_tab)
> library(tidyverse)
-- Attaching packages -------------------------------- tidyverse 1.3.0 --
  v ggplot2 3.3.3     v purrr   0.3.4
v tibble  3.0.6     v dplyr   1.0.4
v tidyr   1.1.2     v stringr 1.4.0
v readr   1.4.0     v forcats 0.5.1
-- Conflicts ----------------------------------- tidyverse_conflicts() --
  x dplyr::filter() masks stats::filter()
x dplyr::lag()    masks stats::lag()
> data <- read.csv("covid-responses.tab.xlsx - covid-responses.tab.csv")
Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
  In file(file, "rt") :
  cannot open file 'covid-responses.tab.xlsx - covid-responses.tab.csv': No such file or directory
> data <- read.csv("covid-responses.tab.xlsx")
Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
  In file(file, "rt") :
  cannot open file 'covid-responses.tab.xlsx': No such file or directory
> myData <- read_excel("covid-responses.tab.xlsx")
Error: `path` does not exist: ‘covid-responses.tab.xlsx’
> head(covid_responses_tab, 5)
# A tibble: 5 x 80
StartDate           EndDate             Status Progress Duration..in.se~ Finished
<dttm>              <dttm>              <chr>     <dbl>            <dbl> <lgl>   
  1 2020-03-17 16:56:00 2020-03-17 17:01:00 Surve~      100              281 TRUE    
2 2020-03-17 20:30:00 2020-03-17 20:38:00 Surve~      100              442 TRUE    
3 2020-03-18 07:33:00 2020-03-18 07:33:00 IP Ad~      100                4 TRUE    
4 2020-03-18 07:41:00 2020-03-18 07:58:00 IP Ad~      100             1013 TRUE    
5 2020-03-18 07:58:00 2020-03-18 08:00:00 IP Ad~      100               79 TRUE    
# ... with 74 more variables: RecordedDate <dttm>, ResponseId <chr>,
#   RecipientLastName <lgl>, RecipientEmail <lgl>, ExternalReference <lgl>,
#   LocationLatitude <dbl>, LocationLongitude <dbl>, DistributionChannel <chr>,
#   UserLanguage <chr>, Q1 <chr>, SelfReported_Behavio_1 <dbl>,
#   SelfReported_Behavio_2 <dbl>, SelfReported_Behavio_3 <dbl>,
#   SelfReported_Behavio_4 <dbl>, SelfReported_Behavio_5 <dbl>, Reflection <chr>,
#   T2 <chr>, social <chr>, handshake <chr>, stores <chr>, curfew <chr>,
#   SOB_1 <dbl>, SOB_2 <dbl>, SOB_3 <dbl>, SOB_4 <dbl>, financialpunishment <chr>,
#   Geldstrafe_1_1 <dbl>, Geldstrafe_2_1 <dbl>, perceivedreaction <chr>, Q36 <chr>,
#   Q37 <chr>, Q23 <chr>, perceivedeffectivnes <chr>, anxiety_1 <chr>,
#   anxiety_2 <chr>, anxiety_3 <chr>, anxiety_4 <chr>, anxiety_5 <chr>, Q24 <chr>,
#   Q25 <chr>, Q25_13_TEXT <chr>, Q26 <chr>, Q26_11_TEXT <chr>, age <dbl>,
#   gender <chr>, gender_3_TEXT <chr>, zipcode <chr>, health <chr>,
#   health_conditions <chr>, personality_1 <chr>, personality_2 <chr>,
#   personality_3 <chr>, personality_4 <chr>, personality_5 <chr>,
#   personality_6 <chr>, personality_7 <chr>, personality_8 <chr>,
#   personality_9 <chr>, personality_10 <chr>, Q27_5 <chr>, Q35 <chr>, Q62_5 <chr>,
#   Q62_6 <chr>, Q62_7 <chr>, Q62_8 <chr>, Q62_9 <chr>, Q62_11 <chr>, Q62_12 <chr>,
#   Q62_13 <chr>, Q62_14 <chr>, Q62_15 <chr>, rid <chr>, treatment <chr>,
#   finish <dbl>
> covid_responses_tab <- covid_responses_tab %>%
  +     mutate(finishNEW = case_when(
    +         finish >= 1  - "TRUE",
    +         finish == NA - "FALSE")
    +     )
Error: Problem with `mutate()` input `finishNEW`.
x non-numeric argument to binary operator
i Input `finishNEW` is `case_when(finish >= 1 - "TRUE", finish == NA - "FALSE")`.
Run `rlang::last_error()` to see where the error occurred.
>  covid_responses_tab <- covid_responses_tab %>%
  +      mutate(finishNEW = case_when(
    +          finish >= 1  ~ "TRUE",
    +          finish == NA ~ "FALSE")
    +      )
> covid_responses_tab <- covid_responses_tab %>%
  +     mutate(ageNew = case_when(
    +         age == 2020 - age)
    +     )
Error: Problem with `mutate()` input `ageNew`.
x Case 1 (`age == 2020 - age`) must be a two-sided formula, not a logical vector.
i Input `ageNew` is `case_when(age == 2020 - age)`.
Run `rlang::last_error()` to see where the error occurred.
> covid_responses_tab %>%
  +          mutate(age == 2020 - age)
# A tibble: 3,460 x 82
StartDate           EndDate             Status Progress Duration..in.se~ Finished
<dttm>              <dttm>              <chr>     <dbl>            <dbl> <lgl>   
  1 2020-03-17 16:56:00 2020-03-17 17:01:00 Surve~      100              281 TRUE    
2 2020-03-17 20:30:00 2020-03-17 20:38:00 Surve~      100              442 TRUE    
3 2020-03-18 07:33:00 2020-03-18 07:33:00 IP Ad~      100                4 TRUE    
4 2020-03-18 07:41:00 2020-03-18 07:58:00 IP Ad~      100             1013 TRUE    
5 2020-03-18 07:58:00 2020-03-18 08:00:00 IP Ad~      100               79 TRUE    
6 2020-03-18 07:04:00 2020-03-18 08:00:00 IP Ad~      100             3327 TRUE    
7 2020-03-18 08:02:00 2020-03-18 08:05:00 IP Ad~      100              231 TRUE    
8 2020-03-18 07:58:00 2020-03-18 08:09:00 IP Ad~      100              628 TRUE    
9 2020-03-18 08:00:00 2020-03-18 08:11:00 IP Ad~      100              636 TRUE    
10 2020-03-18 08:09:00 2020-03-18 08:11:00 IP Ad~      100              124 TRUE    
# ... with 3,450 more rows, and 76 more variables: RecordedDate <dttm>,
#   ResponseId <chr>, RecipientLastName <lgl>, RecipientEmail <lgl>,
#   ExternalReference <lgl>, LocationLatitude <dbl>, LocationLongitude <dbl>,
#   DistributionChannel <chr>, UserLanguage <chr>, Q1 <chr>,
#   SelfReported_Behavio_1 <dbl>, SelfReported_Behavio_2 <dbl>,
#   SelfReported_Behavio_3 <dbl>, SelfReported_Behavio_4 <dbl>,
#   SelfReported_Behavio_5 <dbl>, Reflection <chr>, T2 <chr>, social <chr>,
#   handshake <chr>, stores <chr>, curfew <chr>, SOB_1 <dbl>, SOB_2 <dbl>,
#   SOB_3 <dbl>, SOB_4 <dbl>, financialpunishment <chr>, Geldstrafe_1_1 <dbl>,
#   Geldstrafe_2_1 <dbl>, perceivedreaction <chr>, Q36 <chr>, Q37 <chr>, Q23 <chr>,
#   perceivedeffectivnes <chr>, anxiety_1 <chr>, anxiety_2 <chr>, anxiety_3 <chr>,
#   anxiety_4 <chr>, anxiety_5 <chr>, Q24 <chr>, Q25 <chr>, Q25_13_TEXT <chr>,
#   Q26 <chr>, Q26_11_TEXT <chr>, age <dbl>, gender <chr>, gender_3_TEXT <chr>,
#   zipcode <chr>, health <chr>, health_conditions <chr>, personality_1 <chr>,
#   personality_2 <chr>, personality_3 <chr>, personality_4 <chr>,
#   personality_5 <chr>, personality_6 <chr>, personality_7 <chr>,
#   personality_8 <chr>, personality_9 <chr>, personality_10 <chr>, Q27_5 <chr>,
#   Q35 <chr>, Q62_5 <chr>, Q62_6 <chr>, Q62_7 <chr>, Q62_8 <chr>, Q62_9 <chr>,
#   Q62_11 <chr>, Q62_12 <chr>, Q62_13 <chr>, Q62_14 <chr>, Q62_15 <chr>, rid <chr>,
#   treatment <chr>, finish <dbl>, finishNEW <chr>, `age == 2020 - age` <lgl>
>              )
Error: unexpected ')' in "             )"
> covid_responses_tab %>%
  +               mutate(age = 2020 - age)
# A tibble: 3,460 x 81
StartDate           EndDate             Status Progress Duration..in.se~ Finished
<dttm>              <dttm>              <chr>     <dbl>            <dbl> <lgl>   
  1 2020-03-17 16:56:00 2020-03-17 17:01:00 Surve~      100              281 TRUE    
2 2020-03-17 20:30:00 2020-03-17 20:38:00 Surve~      100              442 TRUE    
3 2020-03-18 07:33:00 2020-03-18 07:33:00 IP Ad~      100                4 TRUE    
4 2020-03-18 07:41:00 2020-03-18 07:58:00 IP Ad~      100             1013 TRUE    
5 2020-03-18 07:58:00 2020-03-18 08:00:00 IP Ad~      100               79 TRUE    
6 2020-03-18 07:04:00 2020-03-18 08:00:00 IP Ad~      100             3327 TRUE    
7 2020-03-18 08:02:00 2020-03-18 08:05:00 IP Ad~      100              231 TRUE    
8 2020-03-18 07:58:00 2020-03-18 08:09:00 IP Ad~      100              628 TRUE    
9 2020-03-18 08:00:00 2020-03-18 08:11:00 IP Ad~      100              636 TRUE    
10 2020-03-18 08:09:00 2020-03-18 08:11:00 IP Ad~      100              124 TRUE    
# ... with 3,450 more rows, and 75 more variables: RecordedDate <dttm>,
#   ResponseId <chr>, RecipientLastName <lgl>, RecipientEmail <lgl>,
#   ExternalReference <lgl>, LocationLatitude <dbl>, LocationLongitude <dbl>,
#   DistributionChannel <chr>, UserLanguage <chr>, Q1 <chr>,
#   SelfReported_Behavio_1 <dbl>, SelfReported_Behavio_2 <dbl>,
#   SelfReported_Behavio_3 <dbl>, SelfReported_Behavio_4 <dbl>,
#   SelfReported_Behavio_5 <dbl>, Reflection <chr>, T2 <chr>, social <chr>,
#   handshake <chr>, stores <chr>, curfew <chr>, SOB_1 <dbl>, SOB_2 <dbl>,
#   SOB_3 <dbl>, SOB_4 <dbl>, financialpunishment <chr>, Geldstrafe_1_1 <dbl>,
#   Geldstrafe_2_1 <dbl>, perceivedreaction <chr>, Q36 <chr>, Q37 <chr>, Q23 <chr>,
#   perceivedeffectivnes <chr>, anxiety_1 <chr>, anxiety_2 <chr>, anxiety_3 <chr>,
#   anxiety_4 <chr>, anxiety_5 <chr>, Q24 <chr>, Q25 <chr>, Q25_13_TEXT <chr>,
#   Q26 <chr>, Q26_11_TEXT <chr>, age <dbl>, gender <chr>, gender_3_TEXT <chr>,
#   zipcode <chr>, health <chr>, health_conditions <chr>, personality_1 <chr>,
#   personality_2 <chr>, personality_3 <chr>, personality_4 <chr>,
#   personality_5 <chr>, personality_6 <chr>, personality_7 <chr>,
#   personality_8 <chr>, personality_9 <chr>, personality_10 <chr>, Q27_5 <chr>,
#   Q35 <chr>, Q62_5 <chr>, Q62_6 <chr>, Q62_7 <chr>, Q62_8 <chr>, Q62_9 <chr>,
#   Q62_11 <chr>, Q62_12 <chr>, Q62_13 <chr>, Q62_14 <chr>, Q62_15 <chr>, rid <chr>,
#   treatment <chr>, finish <dbl>, finishNEW <chr>
> covid_responses_tab %>%
  +                    mutate(age = 2020 - age) %>%
  + mutate(age = 2020 - age)
# A tibble: 3,460 x 81
StartDate           EndDate             Status Progress Duration..in.se~ Finished
<dttm>              <dttm>              <chr>     <dbl>            <dbl> <lgl>   
  1 2020-03-17 16:56:00 2020-03-17 17:01:00 Surve~      100              281 TRUE    
2 2020-03-17 20:30:00 2020-03-17 20:38:00 Surve~      100              442 TRUE    
3 2020-03-18 07:33:00 2020-03-18 07:33:00 IP Ad~      100                4 TRUE    
4 2020-03-18 07:41:00 2020-03-18 07:58:00 IP Ad~      100             1013 TRUE    
5 2020-03-18 07:58:00 2020-03-18 08:00:00 IP Ad~      100               79 TRUE    
6 2020-03-18 07:04:00 2020-03-18 08:00:00 IP Ad~      100             3327 TRUE    
7 2020-03-18 08:02:00 2020-03-18 08:05:00 IP Ad~      100              231 TRUE    
8 2020-03-18 07:58:00 2020-03-18 08:09:00 IP Ad~      100              628 TRUE    
9 2020-03-18 08:00:00 2020-03-18 08:11:00 IP Ad~      100              636 TRUE    
10 2020-03-18 08:09:00 2020-03-18 08:11:00 IP Ad~      100              124 TRUE    
# ... with 3,450 more rows, and 75 more variables: RecordedDate <dttm>,
#   ResponseId <chr>, RecipientLastName <lgl>, RecipientEmail <lgl>,
#   ExternalReference <lgl>, LocationLatitude <dbl>, LocationLongitude <dbl>,
#   DistributionChannel <chr>, UserLanguage <chr>, Q1 <chr>,
#   SelfReported_Behavio_1 <dbl>, SelfReported_Behavio_2 <dbl>,
#   SelfReported_Behavio_3 <dbl>, SelfReported_Behavio_4 <dbl>,
#   SelfReported_Behavio_5 <dbl>, Reflection <chr>, T2 <chr>, social <chr>,
#   handshake <chr>, stores <chr>, curfew <chr>, SOB_1 <dbl>, SOB_2 <dbl>,
#   SOB_3 <dbl>, SOB_4 <dbl>, financialpunishment <chr>, Geldstrafe_1_1 <dbl>,
#   Geldstrafe_2_1 <dbl>, perceivedreaction <chr>, Q36 <chr>, Q37 <chr>, Q23 <chr>,
#   perceivedeffectivnes <chr>, anxiety_1 <chr>, anxiety_2 <chr>, anxiety_3 <chr>,
#   anxiety_4 <chr>, anxiety_5 <chr>, Q24 <chr>, Q25 <chr>, Q25_13_TEXT <chr>,
#   Q26 <chr>, Q26_11_TEXT <chr>, age <dbl>, gender <chr>, gender_3_TEXT <chr>,
#   zipcode <chr>, health <chr>, health_conditions <chr>, personality_1 <chr>,
#   personality_2 <chr>, personality_3 <chr>, personality_4 <chr>,
#   personality_5 <chr>, personality_6 <chr>, personality_7 <chr>,
#   personality_8 <chr>, personality_9 <chr>, personality_10 <chr>, Q27_5 <chr>,
#   Q35 <chr>, Q62_5 <chr>, Q62_6 <chr>, Q62_7 <chr>, Q62_8 <chr>, Q62_9 <chr>,
#   Q62_11 <chr>, Q62_12 <chr>, Q62_13 <chr>, Q62_14 <chr>, Q62_15 <chr>, rid <chr>,
#   treatment <chr>, finish <dbl>, finishNEW <chr>
> replace_na(age, "noagegiven")
Error in replace_na(age, "noagegiven") : object 'age' not found
> covid_responses_tab %>%                                                        replace_na(age, "noagegiven")
Error in is_list(replace) : object 'age' not found
Error: 1 components of `...` were not used.

We detected these problematic arguments:
  * `..1`

Did you misspecify an argument?
  Run `rlang::last_error()` to see where the error occurred.
> covid_responses_tab <- covid_responses_tab %>%
  +           mutate(userLanguageNEW = case_when(
    +                   UserLanguage == EN  ~ "ENGLISH",
    +                   UserLanguage == IT ~ "ITALIAN")
    +               )
Error: Problem with `mutate()` input `userLanguageNEW`.
x object 'EN' not found
i Input `userLanguageNEW` is `case_when(...)`.
Run `rlang::last_error()` to see where the error occurred.
> covid_responses_tab<-mutate(covid_responses_tab, UserLanguageNEW = recode(userLanguage, `IT` = "ITALIAN", `EN` = "ENGLISH"))
Error: Problem with `mutate()` input `UserLanguageNEW`.
x object 'userLanguage' not found
i Input `UserLanguageNEW` is `recode(userLanguage, IT = "ITALIAN", EN = "ENGLISH")`.
Run `rlang::last_error()` to see where the error occurred.
> 
  > covid_responses_tab<-mutate(covid_responses_tab, UserLanguageNEW = recode(UserLanguage, `IT` = "ITALIAN", `EN` = "ENGLISH"))
> rlang::last_error()
<error/dplyr:::mutate_error>
  Problem with `mutate()` input `UserLanguageNEW`.
x object 'userLanguage' not found
i Input `UserLanguageNEW` is `recode(userLanguage, IT = "ITALIAN", EN = "ENGLISH")`.
Backtrace:
  1. dplyr::mutate(...)
7. base::.handleSimpleError(...)
8. dplyr:::h(simpleError(msg, call))
Run `rlang::last_trace()` to see the full context.
> covid_responses_tab<-mutate(covid_responses_tab, UserLanguage = recode(UserLanguage, `IT` = "ITALIAN", `EN` = "ENGLISH"))