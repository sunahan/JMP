######################### Load Packages #######################
library(readxl)
library(MASS)
library(viridis)
library(miceadds)
library(gdata)
library(latex2exp)
library(ggplot2)
library(ggpubr)
library("stats")
library(estimatr)
library(zoo)
library(lmtest)
library(sandwich)
library(grid)
library(gridExtra)
library("matrixStats")
library(plm)
library(stargazer)
library(magrittr)
library(texreg)
library(xtable)
library(Matrix)
library(lme4)
library(nlme)
library(fixest)
library(modelsummary)
library(corrplot)
library(tidyselect)
library(tidyverse)
library(writexl)
#################################################################
##---------------------------------------------------------------
##             Load Restructure data - T1 and T5               --
##---------------------------------------------------------------
range <- c(1:3,13:15)

for(i in range){
  if(i < 13){x <- read_excel(paste("C://Users//kccsu//Desktop//SchoolChoice//Data//DA_", i, ".xlsx", sep = "")) %>%
    dplyr::select(
      ttc,
      participant.id_in_session, participant.code, Comprehensive.1.player.quiz_payoff:Comprehensive.1.player.quiz13,
      Blue_Open_First_PR.1.player.payoff:Blue_Open_First_PR.1.player.assigned_code,
      Blue_Open_First_PR.2.player.payoff:Blue_Open_First_PR.2.player.assigned_code,
      Blue_Reserved_First_PR.1.player.payoff:Blue_Reserved_First_PR.1.player.assigned_code,
      Blue_Reserved_First_PR.2.player.payoff:Blue_Reserved_First_PR.2.player.assigned_code,
      
      Blue_Open_First.1.player.payoff:Blue_Open_First.1.player.assigned_code,
      Blue_Open_First.2.player.payoff:Blue_Open_First.2.player.assigned_code,
      Blue_Open_First.3.player.payoff:Blue_Open_First.3.player.assigned_code,
      Blue_Open_First.4.player.payoff:Blue_Open_First.4.player.assigned_code,
      Blue_Open_First.5.player.payoff:Blue_Open_First.5.player.assigned_code,
      
      Blue_Reserved_First.1.player.payoff:Blue_Reserved_First.1.player.assigned_code,
      Blue_Reserved_First.2.player.payoff:Blue_Reserved_First.2.player.assigned_code,
      Blue_Reserved_First.3.player.payoff:Blue_Reserved_First.3.player.assigned_code,
      Blue_Reserved_First.4.player.payoff:Blue_Reserved_First.4.player.assigned_code,
      Blue_Reserved_First.5.player.payoff:Blue_Reserved_First.5.player.assigned_code,
      
      Green_Reserved_First.1.player.payoff:Green_Reserved_First.1.player.assigned_code,
      Green_Reserved_First.2.player.payoff:Green_Reserved_First.2.player.assigned_code,
      Green_Reserved_First.3.player.payoff:Green_Reserved_First.3.player.assigned_code,
      Green_Reserved_First.4.player.payoff:Green_Reserved_First.4.player.assigned_code,
      Green_Reserved_First.5.player.payoff:Green_Reserved_First.5.player.assigned_code,
      
      Green_Open_First.1.player.payoff:Green_Open_First.1.player.assigned_code,
      Green_Open_First.2.player.payoff:Green_Open_First.2.player.assigned_code,
      Green_Open_First.3.player.payoff:Green_Open_First.3.player.assigned_code,
      Green_Open_First.4.player.payoff:Green_Open_First.4.player.assigned_code,
      Green_Open_First.5.player.payoff:Green_Open_First.5.player.assigned_code,
      Survey.1.player.survey1:Survey.1.player.survey15,
      PayRandomApp.1.player.app_to_pay:PayRandomApp.1.player.final_payment) %>% mutate(session = i)
  nam <- paste("dt", i, sep = "_")
  assign(nam, x)
  }else{x <- read_excel(paste("C://Users//kccsu//Desktop//SchoolChoice//Data//TTC_", i, ".xlsx", sep = "")) %>%
    dplyr::select(
      ttc,
      participant.id_in_session, participant.code, Comprehensive.1.player.quiz_payoff:Comprehensive.1.player.quiz13,
      Blue_Open_First_PR.1.player.payoff:Blue_Open_First_PR.1.player.assigned_code,
      Blue_Open_First_PR.2.player.payoff:Blue_Open_First_PR.2.player.assigned_code,
      Blue_Reserved_First_PR.1.player.payoff:Blue_Reserved_First_PR.1.player.assigned_code,
      Blue_Reserved_First_PR.2.player.payoff:Blue_Reserved_First_PR.2.player.assigned_code,
      
      Blue_Open_First.1.player.payoff:Blue_Open_First.1.player.assigned_code,
      Blue_Open_First.2.player.payoff:Blue_Open_First.2.player.assigned_code,
      Blue_Open_First.3.player.payoff:Blue_Open_First.3.player.assigned_code,
      Blue_Open_First.4.player.payoff:Blue_Open_First.4.player.assigned_code,
      Blue_Open_First.5.player.payoff:Blue_Open_First.5.player.assigned_code,
      
      Blue_Reserved_First.1.player.payoff:Blue_Reserved_First.1.player.assigned_code,
      Blue_Reserved_First.2.player.payoff:Blue_Reserved_First.2.player.assigned_code,
      Blue_Reserved_First.3.player.payoff:Blue_Reserved_First.3.player.assigned_code,
      Blue_Reserved_First.4.player.payoff:Blue_Reserved_First.4.player.assigned_code,
      Blue_Reserved_First.5.player.payoff:Blue_Reserved_First.5.player.assigned_code,
      
      Green_Reserved_First.1.player.payoff:Green_Reserved_First.1.player.assigned_code,
      Green_Reserved_First.2.player.payoff:Green_Reserved_First.2.player.assigned_code,
      Green_Reserved_First.3.player.payoff:Green_Reserved_First.3.player.assigned_code,
      Green_Reserved_First.4.player.payoff:Green_Reserved_First.4.player.assigned_code,
      Green_Reserved_First.5.player.payoff:Green_Reserved_First.5.player.assigned_code,
      
      Green_Open_First.1.player.payoff:Green_Open_First.1.player.assigned_code,
      Green_Open_First.2.player.payoff:Green_Open_First.2.player.assigned_code,
      Green_Open_First.3.player.payoff:Green_Open_First.3.player.assigned_code,
      Green_Open_First.4.player.payoff:Green_Open_First.4.player.assigned_code,
      Green_Open_First.5.player.payoff:Green_Open_First.5.player.assigned_code,
      Survey.1.player.survey1:Survey.1.player.survey15,
      PayRandomApp.1.player.app_to_pay:PayRandomApp.1.player.final_payment) %>% mutate(session = i)
  nam <- paste("dt", i, sep = "_")
  assign(nam, x)
  }
}
##---------------------------------------------------------------
##                  Combine all data                           --
##---------------------------------------------------------------
df_1 <- rbind(dt_1, dt_2, dt_3, dt_13, dt_14, dt_15)
df_1 <- df_1 %>% rename(subject.id = participant.id_in_session,
                    q_payoff = Comprehensive.1.player.quiz_payoff, q1 = Comprehensive.1.player.quiz1, q2 = Comprehensive.1.player.quiz2,
                    q3 = Comprehensive.1.player.quiz3, q4 = Comprehensive.1.player.quiz4, q5 = Comprehensive.1.player.quiz5,
                    q6 = Comprehensive.1.player.quiz6, q7 = Comprehensive.1.player.quiz7, q8 = Comprehensive.1.player.quiz8,
                    q9 = Comprehensive.1.player.quiz9, q10 = Comprehensive.1.player.quiz10, q11 = Comprehensive.1.player.quiz11,
                    q12 = Comprehensive.1.player.quiz12, q13 = Comprehensive.1.player.quiz13, 
                    
                    bof_pp1_po = Blue_Open_First_PR.1.player.payoff, bof_pp1_s1= Blue_Open_First_PR.1.player.school1, 
                    bof_pp1_s2= Blue_Open_First_PR.1.player.school2,bof_pp1_s3= Blue_Open_First_PR.1.player.school3, 
                    bof_pp1_s= Blue_Open_First_PR.1.player.matched_school, bof_pp1_code = Blue_Open_First_PR.1.player.assigned_code,
                    
                    bof_pp2_po = Blue_Open_First_PR.2.player.payoff, bof_pp2_s1= Blue_Open_First_PR.2.player.school1, 
                    bof_pp2_s2= Blue_Open_First_PR.2.player.school2,bof_pp2_s3= Blue_Open_First_PR.2.player.school3, 
                    bof_pp2_s= Blue_Open_First_PR.2.player.matched_school, bof_pp2_code = Blue_Open_First_PR.2.player.assigned_code,
                    
                    brf_pp1_po = Blue_Reserved_First_PR.1.player.payoff, brf_pp1_s1= Blue_Reserved_First_PR.1.player.school1, 
                    brf_pp1_s2= Blue_Reserved_First_PR.1.player.school2, brf_pp1_s3= Blue_Reserved_First_PR.1.player.school3, 
                    brf_pp1_s= Blue_Reserved_First_PR.1.player.matched_school, brf_pp1_code = Blue_Reserved_First_PR.1.player.assigned_code, 
                    
                    brf_pp2_po = Blue_Reserved_First_PR.2.player.payoff, brf_pp2_s1= Blue_Reserved_First_PR.2.player.school1, 
                    brf_pp2_s2= Blue_Reserved_First_PR.2.player.school2, brf_pp2_s3= Blue_Reserved_First_PR.2.player.school3,
                    brf_pp2_= Blue_Reserved_First_PR.2.player.matched_school, brf_pp2_code = Blue_Reserved_First_PR.2.player.assigned_code,
                    
                    bof_1_po = Blue_Open_First.1.player.payoff, bof_1_s1 = Blue_Open_First.1.player.school1,
                    bof_1_s2 = Blue_Open_First.1.player.school2, bof_1_s3 = Blue_Open_First.1.player.school3,
                    bof_1_s = Blue_Open_First.1.player.matched_school, bof_1_code  = Blue_Open_First.1.player.assigned_code,
                    
                    bof_2_po = Blue_Open_First.2.player.payoff, bof_2_s1 = Blue_Open_First.2.player.school1,
                    bof_2_s2 = Blue_Open_First.2.player.school2, bof_2_s3 = Blue_Open_First.2.player.school3,
                    bof_2_s = Blue_Open_First.2.player.matched_school, bof_2_code  = Blue_Open_First.2.player.assigned_code,
                    
                    bof_3_po = Blue_Open_First.3.player.payoff, bof_3_s1 = Blue_Open_First.3.player.school1,
                    bof_3_s2 = Blue_Open_First.3.player.school2, bof_3_s3 = Blue_Open_First.3.player.school3,
                    bof_3_s = Blue_Open_First.3.player.matched_school, bof_3_code  = Blue_Open_First.3.player.assigned_code,
                    
                    bof_4_po = Blue_Open_First.4.player.payoff, bof_4_s1 = Blue_Open_First.4.player.school1,
                    bof_4_s2 = Blue_Open_First.4.player.school2, bof_4_s3 = Blue_Open_First.4.player.school3,
                    bof_4_s = Blue_Open_First.4.player.matched_school, bof_4_code  = Blue_Open_First.4.player.assigned_code,
                    
                    bof_5_po = Blue_Open_First.5.player.payoff, bof_5_s1 = Blue_Open_First.5.player.school1,
                    bof_5_s2 = Blue_Open_First.5.player.school2, bof_5_s3 = Blue_Open_First.5.player.school3,
                    bof_5_s = Blue_Open_First.5.player.matched_school, bof_5_code  = Blue_Open_First.5.player.assigned_code,
                    
                    brf_1_po = Blue_Reserved_First.1.player.payoff, brf_1_s1 = Blue_Reserved_First.1.player.school1,
                    brf_1_s2 = Blue_Reserved_First.1.player.school2, brf_1_s3 = Blue_Reserved_First.1.player.school3,
                    brf_1_s = Blue_Reserved_First.1.player.matched_school, brf_1_code  = Blue_Reserved_First.1.player.assigned_code,
                    
                    brf_2_po = Blue_Reserved_First.2.player.payoff, brf_2_s1 = Blue_Reserved_First.2.player.school1,
                    brf_2_s2 = Blue_Reserved_First.2.player.school2, brf_2_s3 = Blue_Reserved_First.2.player.school3,
                    brf_2_s = Blue_Reserved_First.2.player.matched_school, brf_2_code  = Blue_Reserved_First.2.player.assigned_code,
                    
                    brf_3_po = Blue_Reserved_First.3.player.payoff, brf_3_s1 = Blue_Reserved_First.3.player.school1,
                    brf_3_s2 = Blue_Reserved_First.3.player.school2, brf_3_s3 = Blue_Reserved_First.3.player.school3,
                    brf_3_s = Blue_Reserved_First.3.player.matched_school, brf_3_code  = Blue_Reserved_First.3.player.assigned_code,
                    
                    brf_4_po = Blue_Reserved_First.4.player.payoff, brf_4_s1 = Blue_Reserved_First.4.player.school1,
                    brf_4_s2 = Blue_Reserved_First.4.player.school2, brf_4_s3 = Blue_Reserved_First.4.player.school3,
                    brf_4_s = Blue_Reserved_First.4.player.matched_school, brf_4_code  = Blue_Reserved_First.4.player.assigned_code,
                    
                    brf_5_po = Blue_Reserved_First.5.player.payoff, brf_5_s1 = Blue_Reserved_First.5.player.school1,
                    brf_5_s2 = Blue_Reserved_First.5.player.school2, brf_5_s3 = Blue_Reserved_First.5.player.school3,
                    brf_5_s = Blue_Reserved_First.5.player.matched_school, brf_5_code  = Blue_Reserved_First.5.player.assigned_code,
                    
                    grf_1_po = Green_Reserved_First.1.player.payoff, grf_1_s1 = Green_Reserved_First.1.player.school1,
                    grf_1_s2 = Green_Reserved_First.1.player.school2, grf_1_s3 = Green_Reserved_First.1.player.school3,
                    grf_1_s = Green_Reserved_First.1.player.matched_school, grf_1_code  = Green_Reserved_First.1.player.assigned_code,
                    
                    grf_2_po = Green_Reserved_First.2.player.payoff, grf_2_s1 = Green_Reserved_First.2.player.school1,
                    grf_2_s2 = Green_Reserved_First.2.player.school2, grf_2_s3 = Green_Reserved_First.2.player.school3,
                    grf_2_s = Green_Reserved_First.2.player.matched_school, grf_2_code  = Green_Reserved_First.2.player.assigned_code,
                    
                    grf_3_po = Green_Reserved_First.3.player.payoff, grf_3_s1 = Green_Reserved_First.3.player.school1,
                    grf_3_s2 = Green_Reserved_First.3.player.school2, grf_3_s3 = Green_Reserved_First.3.player.school3,
                    grf_3_s = Green_Reserved_First.3.player.matched_school, grf_3_code  = Green_Reserved_First.3.player.assigned_code,
                    
                    grf_4_po = Green_Reserved_First.4.player.payoff, grf_4_s1 = Green_Reserved_First.4.player.school1,
                    grf_4_s2 = Green_Reserved_First.4.player.school2, grf_4_s3 = Green_Reserved_First.4.player.school3,
                    grf_4_s = Green_Reserved_First.4.player.matched_school, grf_4_code  = Green_Reserved_First.4.player.assigned_code,
                    
                    grf_5_po = Green_Reserved_First.5.player.payoff, grf_5_s1 = Green_Reserved_First.5.player.school1,
                    grf_5_s2 = Green_Reserved_First.5.player.school2, grf_5_s3 = Green_Reserved_First.5.player.school3,
                    grf_5_s = Green_Reserved_First.5.player.matched_school, grf_5_code  = Green_Reserved_First.5.player.assigned_code,
                    
                    gof_1_po = Green_Open_First.1.player.payoff, gof_1_s1 = Green_Open_First.1.player.school1,
                    gof_1_s2 = Green_Open_First.1.player.school2, gof_1_s3 = Green_Open_First.1.player.school3,
                    gof_1_s = Green_Open_First.1.player.matched_school, gof_1_code  = Green_Open_First.1.player.assigned_code,
                    
                    gof_2_po = Green_Open_First.2.player.payoff, gof_2_s1 = Green_Open_First.2.player.school1,
                    gof_2_s2 = Green_Open_First.2.player.school2, gof_2_s3 = Green_Open_First.2.player.school3,
                    gof_2_s = Green_Open_First.2.player.matched_school, gof_2_code  = Green_Open_First.2.player.assigned_code,
                    
                    gof_3_po = Green_Open_First.3.player.payoff, gof_3_s1 = Green_Open_First.3.player.school1,
                    gof_3_s2 = Green_Open_First.3.player.school2, gof_3_s3 = Green_Open_First.3.player.school3,
                    gof_3_s = Green_Open_First.3.player.matched_school, gof_3_code  = Green_Open_First.3.player.assigned_code,
                    
                    gof_4_po = Green_Open_First.4.player.payoff, gof_4_s1 = Green_Open_First.4.player.school1,
                    gof_4_s2 = Green_Open_First.4.player.school2, gof_4_s3 = Green_Open_First.4.player.school3,
                    gof_4_s = Green_Open_First.4.player.matched_school, gof_4_code  = Green_Open_First.4.player.assigned_code,
                    
                    gof_5_po = Green_Open_First.5.player.payoff, gof_5_s1 = Green_Open_First.5.player.school1,
                    gof_5_s2 = Green_Open_First.5.player.school2, gof_5_s3 = Green_Open_First.5.player.school3,
                    gof_5_s = Green_Open_First.5.player.matched_school, gof_5_code  = Green_Open_First.5.player.assigned_code,
                    
                    survey1 = Survey.1.player.survey1, survey2 = Survey.1.player.survey2, survey3 = Survey.1.player.survey3, 
                    survey4 = Survey.1.player.survey4, survey5 = Survey.1.player.survey5, survey6 = Survey.1.player.survey6, 
                    survey7 = Survey.1.player.survey7, survey8 = Survey.1.player.survey8, survey9 = Survey.1.player.survey9, 
                    survey10 = Survey.1.player.survey10, survey11 = Survey.1.player.survey11, survey12 = Survey.1.player.survey12, 
                    survey13 = Survey.1.player.survey13, survey14 = Survey.1.player.survey14, survey15 = Survey.1.player.survey15,
                    period_earning = PayRandomApp.1.player.final_period_pay, total_earning = PayRandomApp.1.player.final_payment
)


##---------------------------------------------------------------
##                  Restructure data                           --
##---------------------------------------------------------------

df_1 <- df_1 %>% dplyr::select(session, subject.id, ttc, q_payoff, period_earning, total_earning, bof_1_po:survey15)

df_1_code <- df_1 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_code"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "c",
    values_to = "role"
  ) %>% 
  separate(c, c("treatment", "period", "x"), sep="_") %>% 
  select(-c("period", "x"))

df_1_s1 <- df_1 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_s1"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "s1",
    values_to = "s1_reported"
  ) %>% 
  separate(s1, c("treatment", "period", "x"), sep="_") %>% 
  select("s1_reported")

df_1_s2 <- df_1 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_s2"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "s2",
    values_to = "s2_reported"
  ) %>% 
  separate(s2, c("treatment", "period", "x"), sep="_") %>% 
  select("s2_reported")

df_1_s3 <- df_1 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_s3"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "s3",
    values_to = "s3_reported"
  ) %>% 
  separate(s3, c("treatment", "period", "x"), sep="_") %>% 
  select("s3_reported")

df_1_s <- df_1 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_s"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "s",
    values_to = "s_assigned"
  ) %>% 
  separate(s, c("treatment", "period", "x"), sep="_") %>% 
  select("s_assigned")

df_1_po <- df_1 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_po"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "c",
    values_to = "payoff"
  ) %>% 
  separate(c, c("treatment", "period", "x"), sep="_") %>% 
  select("payoff")

z <- c(1:20)
period <- rep(z, 36)

long_df_1 <- cbind(df_1_code, df_1_s1, df_1_s2, df_1_s3, df_1_s, df_1_po, period)


##---------------------------------------------------------------
##             Load Restructure data - T2 and T6               --
##---------------------------------------------------------------

range <- c(4:6,16:18)

for(i in range){
  if(i < 13){x <- read_excel(paste("C://Users//kccsu//Desktop//SchoolChoice//Data//DA_", i, ".xlsx", sep = "")) %>%
    dplyr::select(
      ttc,
      participant.id_in_session, participant.code, Comprehensive.1.player.quiz_payoff:Comprehensive.1.player.quiz13,
      Blue_Open_First_PR.1.player.payoff:Blue_Open_First_PR.1.player.assigned_code,
      Blue_Open_First_PR.2.player.payoff:Blue_Open_First_PR.2.player.assigned_code,
      Blue_Reserved_First_PR.1.player.payoff:Blue_Reserved_First_PR.1.player.assigned_code,
      Blue_Reserved_First_PR.2.player.payoff:Blue_Reserved_First_PR.2.player.assigned_code,
      
      Green_Open_First.1.player.payoff:Green_Open_First.1.player.assigned_code,
      Green_Open_First.2.player.payoff:Green_Open_First.2.player.assigned_code,
      Green_Open_First.3.player.payoff:Green_Open_First.3.player.assigned_code,
      Green_Open_First.4.player.payoff:Green_Open_First.4.player.assigned_code,
      Green_Open_First.5.player.payoff:Green_Open_First.5.player.assigned_code,
      
      Green_Reserved_First.1.player.payoff:Green_Reserved_First.1.player.assigned_code,
      Green_Reserved_First.2.player.payoff:Green_Reserved_First.2.player.assigned_code,
      Green_Reserved_First.3.player.payoff:Green_Reserved_First.3.player.assigned_code,
      Green_Reserved_First.4.player.payoff:Green_Reserved_First.4.player.assigned_code,
      Green_Reserved_First.5.player.payoff:Green_Reserved_First.5.player.assigned_code,
      
      Blue_Reserved_First.1.player.payoff:Blue_Reserved_First.1.player.assigned_code,
      Blue_Reserved_First.2.player.payoff:Blue_Reserved_First.2.player.assigned_code,
      Blue_Reserved_First.3.player.payoff:Blue_Reserved_First.3.player.assigned_code,
      Blue_Reserved_First.4.player.payoff:Blue_Reserved_First.4.player.assigned_code,
      Blue_Reserved_First.5.player.payoff:Blue_Reserved_First.5.player.assigned_code,
      
      Blue_Open_First.1.player.payoff:Blue_Open_First.1.player.assigned_code,
      Blue_Open_First.2.player.payoff:Blue_Open_First.2.player.assigned_code,
      Blue_Open_First.3.player.payoff:Blue_Open_First.3.player.assigned_code,
      Blue_Open_First.4.player.payoff:Blue_Open_First.4.player.assigned_code,
      Blue_Open_First.5.player.payoff:Blue_Open_First.5.player.assigned_code,
      Survey.1.player.survey1:Survey.1.player.survey15,
      PayRandomApp.1.player.app_to_pay:PayRandomApp.1.player.final_payment) %>% mutate(session = i)
  nam <- paste("dt", i, sep = "_")
  assign(nam, x)
  }else{x <- read_excel(paste("C://Users//kccsu//Desktop//SchoolChoice//Data//TTC_", i, ".xlsx", sep = "")) %>%
    dplyr::select(
      ttc,
      participant.id_in_session, participant.code, Comprehensive.1.player.quiz_payoff:Comprehensive.1.player.quiz13,
      Blue_Open_First_PR.1.player.payoff:Blue_Open_First_PR.1.player.assigned_code,
      Blue_Open_First_PR.2.player.payoff:Blue_Open_First_PR.2.player.assigned_code,
      Blue_Reserved_First_PR.1.player.payoff:Blue_Reserved_First_PR.1.player.assigned_code,
      Blue_Reserved_First_PR.2.player.payoff:Blue_Reserved_First_PR.2.player.assigned_code,
      
      Green_Open_First.1.player.payoff:Green_Open_First.1.player.assigned_code,
      Green_Open_First.2.player.payoff:Green_Open_First.2.player.assigned_code,
      Green_Open_First.3.player.payoff:Green_Open_First.3.player.assigned_code,
      Green_Open_First.4.player.payoff:Green_Open_First.4.player.assigned_code,
      Green_Open_First.5.player.payoff:Green_Open_First.5.player.assigned_code,
      
      Green_Reserved_First.1.player.payoff:Green_Reserved_First.1.player.assigned_code,
      Green_Reserved_First.2.player.payoff:Green_Reserved_First.2.player.assigned_code,
      Green_Reserved_First.3.player.payoff:Green_Reserved_First.3.player.assigned_code,
      Green_Reserved_First.4.player.payoff:Green_Reserved_First.4.player.assigned_code,
      Green_Reserved_First.5.player.payoff:Green_Reserved_First.5.player.assigned_code,
      
      Blue_Reserved_First.1.player.payoff:Blue_Reserved_First.1.player.assigned_code,
      Blue_Reserved_First.2.player.payoff:Blue_Reserved_First.2.player.assigned_code,
      Blue_Reserved_First.3.player.payoff:Blue_Reserved_First.3.player.assigned_code,
      Blue_Reserved_First.4.player.payoff:Blue_Reserved_First.4.player.assigned_code,
      Blue_Reserved_First.5.player.payoff:Blue_Reserved_First.5.player.assigned_code,
      
      Blue_Open_First.1.player.payoff:Blue_Open_First.1.player.assigned_code,
      Blue_Open_First.2.player.payoff:Blue_Open_First.2.player.assigned_code,
      Blue_Open_First.3.player.payoff:Blue_Open_First.3.player.assigned_code,
      Blue_Open_First.4.player.payoff:Blue_Open_First.4.player.assigned_code,
      Blue_Open_First.5.player.payoff:Blue_Open_First.5.player.assigned_code,
      Survey.1.player.survey1:Survey.1.player.survey15,
      PayRandomApp.1.player.app_to_pay:PayRandomApp.1.player.final_payment) %>% mutate(session = i)#, period = rep(c(1:20), 36))
  nam <- paste("dt", i, sep = "_")
  assign(nam, x)
  }
}

df_2 <- rbind(dt_4, dt_5, dt_6, dt_16, dt_17, dt_18)
df_2 <- df_2 %>% rename(subject.id = participant.id_in_session,
                    q_payoff = Comprehensive.1.player.quiz_payoff, q1 = Comprehensive.1.player.quiz1, q2 = Comprehensive.1.player.quiz2,
                    q3 = Comprehensive.1.player.quiz3, q4 = Comprehensive.1.player.quiz4, q5 = Comprehensive.1.player.quiz5,
                    q6 = Comprehensive.1.player.quiz6, q7 = Comprehensive.1.player.quiz7, q8 = Comprehensive.1.player.quiz8,
                    q9 = Comprehensive.1.player.quiz9, q10 = Comprehensive.1.player.quiz10, q11 = Comprehensive.1.player.quiz11,
                    q12 = Comprehensive.1.player.quiz12, q13 = Comprehensive.1.player.quiz13, 
                    
                    bof_pp1_po = Blue_Open_First_PR.1.player.payoff, bof_pp1_s1= Blue_Open_First_PR.1.player.school1, 
                    bof_pp1_s2= Blue_Open_First_PR.1.player.school2,bof_pp1_s3= Blue_Open_First_PR.1.player.school3, 
                    bof_pp1_s= Blue_Open_First_PR.1.player.matched_school, bof_pp1_code = Blue_Open_First_PR.1.player.assigned_code,
                    
                    bof_pp2_po = Blue_Open_First_PR.2.player.payoff, bof_pp2_s1= Blue_Open_First_PR.2.player.school1, 
                    bof_pp2_s2= Blue_Open_First_PR.2.player.school2,bof_pp2_s3= Blue_Open_First_PR.2.player.school3, 
                    bof_pp2_s= Blue_Open_First_PR.2.player.matched_school, bof_pp2_code = Blue_Open_First_PR.2.player.assigned_code,
                    
                    brf_pp1_po = Blue_Reserved_First_PR.1.player.payoff, brf_pp1_s1= Blue_Reserved_First_PR.1.player.school1, 
                    brf_pp1_s2= Blue_Reserved_First_PR.1.player.school2, brf_pp1_s3= Blue_Reserved_First_PR.1.player.school3, 
                    brf_pp1_s= Blue_Reserved_First_PR.1.player.matched_school, brf_pp1_code = Blue_Reserved_First_PR.1.player.assigned_code, 
                    
                    brf_pp2_po = Blue_Reserved_First_PR.2.player.payoff, brf_pp2_s1= Blue_Reserved_First_PR.2.player.school1, 
                    brf_pp2_s2= Blue_Reserved_First_PR.2.player.school2, brf_pp2_s3= Blue_Reserved_First_PR.2.player.school3,
                    brf_pp2_= Blue_Reserved_First_PR.2.player.matched_school, brf_pp2_code = Blue_Reserved_First_PR.2.player.assigned_code,
                    
                    gof_1_po = Green_Open_First.1.player.payoff, gof_1_s1 = Green_Open_First.1.player.school1,
                    gof_1_s2 = Green_Open_First.1.player.school2, gof_1_s3 = Green_Open_First.1.player.school3,
                    gof_1_s = Green_Open_First.1.player.matched_school, gof_1_code  = Green_Open_First.1.player.assigned_code,
                    
                    gof_2_po = Green_Open_First.2.player.payoff, gof_2_s1 = Green_Open_First.2.player.school1,
                    gof_2_s2 = Green_Open_First.2.player.school2, gof_2_s3 = Green_Open_First.2.player.school3,
                    gof_2_s = Green_Open_First.2.player.matched_school, gof_2_code  = Green_Open_First.2.player.assigned_code,
                    
                    gof_3_po = Green_Open_First.3.player.payoff, gof_3_s1 = Green_Open_First.3.player.school1,
                    gof_3_s2 = Green_Open_First.3.player.school2, gof_3_s3 = Green_Open_First.3.player.school3,
                    gof_3_s = Green_Open_First.3.player.matched_school, gof_3_code  = Green_Open_First.3.player.assigned_code,
                    
                    gof_4_po = Green_Open_First.4.player.payoff, gof_4_s1 = Green_Open_First.4.player.school1,
                    gof_4_s2 = Green_Open_First.4.player.school2, gof_4_s3 = Green_Open_First.4.player.school3,
                    gof_4_s = Green_Open_First.4.player.matched_school, gof_4_code  = Green_Open_First.4.player.assigned_code,
                    
                    gof_5_po = Green_Open_First.5.player.payoff, gof_5_s1 = Green_Open_First.5.player.school1,
                    gof_5_s2 = Green_Open_First.5.player.school2, gof_5_s3 = Green_Open_First.5.player.school3,
                    gof_5_s = Green_Open_First.5.player.matched_school, gof_5_code  = Green_Open_First.5.player.assigned_code,
                    
                    grf_1_po = Green_Reserved_First.1.player.payoff, grf_1_s1 = Green_Reserved_First.1.player.school1,
                    grf_1_s2 = Green_Reserved_First.1.player.school2, grf_1_s3 = Green_Reserved_First.1.player.school3,
                    grf_1_s = Green_Reserved_First.1.player.matched_school, grf_1_code  = Green_Reserved_First.1.player.assigned_code,
                    
                    grf_2_po = Green_Reserved_First.2.player.payoff, grf_2_s1 = Green_Reserved_First.2.player.school1,
                    grf_2_s2 = Green_Reserved_First.2.player.school2, grf_2_s3 = Green_Reserved_First.2.player.school3,
                    grf_2_s = Green_Reserved_First.2.player.matched_school, grf_2_code  = Green_Reserved_First.2.player.assigned_code,
                    
                    grf_3_po = Green_Reserved_First.3.player.payoff, grf_3_s1 = Green_Reserved_First.3.player.school1,
                    grf_3_s2 = Green_Reserved_First.3.player.school2, grf_3_s3 = Green_Reserved_First.3.player.school3,
                    grf_3_s = Green_Reserved_First.3.player.matched_school, grf_3_code  = Green_Reserved_First.3.player.assigned_code,
                    
                    grf_4_po = Green_Reserved_First.4.player.payoff, grf_4_s1 = Green_Reserved_First.4.player.school1,
                    grf_4_s2 = Green_Reserved_First.4.player.school2, grf_4_s3 = Green_Reserved_First.4.player.school3,
                    grf_4_s = Green_Reserved_First.4.player.matched_school, grf_4_code  = Green_Reserved_First.4.player.assigned_code,
                    
                    grf_5_po = Green_Reserved_First.5.player.payoff, grf_5_s1 = Green_Reserved_First.5.player.school1,
                    grf_5_s2 = Green_Reserved_First.5.player.school2, grf_5_s3 = Green_Reserved_First.5.player.school3,
                    grf_5_s = Green_Reserved_First.5.player.matched_school, grf_5_code  = Green_Reserved_First.5.player.assigned_code,
                    
                    brf_1_po = Blue_Reserved_First.1.player.payoff, brf_1_s1 = Blue_Reserved_First.1.player.school1,
                    brf_1_s2 = Blue_Reserved_First.1.player.school2, brf_1_s3 = Blue_Reserved_First.1.player.school3,
                    brf_1_s = Blue_Reserved_First.1.player.matched_school, brf_1_code  = Blue_Reserved_First.1.player.assigned_code,
                    
                    brf_2_po = Blue_Reserved_First.2.player.payoff, brf_2_s1 = Blue_Reserved_First.2.player.school1,
                    brf_2_s2 = Blue_Reserved_First.2.player.school2, brf_2_s3 = Blue_Reserved_First.2.player.school3,
                    brf_2_s = Blue_Reserved_First.2.player.matched_school, brf_2_code  = Blue_Reserved_First.2.player.assigned_code,
                    
                    brf_3_po = Blue_Reserved_First.3.player.payoff, brf_3_s1 = Blue_Reserved_First.3.player.school1,
                    brf_3_s2 = Blue_Reserved_First.3.player.school2, brf_3_s3 = Blue_Reserved_First.3.player.school3,
                    brf_3_s = Blue_Reserved_First.3.player.matched_school, brf_3_code  = Blue_Reserved_First.3.player.assigned_code,
                    
                    brf_4_po = Blue_Reserved_First.4.player.payoff, brf_4_s1 = Blue_Reserved_First.4.player.school1,
                    brf_4_s2 = Blue_Reserved_First.4.player.school2, brf_4_s3 = Blue_Reserved_First.4.player.school3,
                    brf_4_s = Blue_Reserved_First.4.player.matched_school, brf_4_code  = Blue_Reserved_First.4.player.assigned_code,
                    
                    brf_5_po = Blue_Reserved_First.5.player.payoff, brf_5_s1 = Blue_Reserved_First.5.player.school1,
                    brf_5_s2 = Blue_Reserved_First.5.player.school2, brf_5_s3 = Blue_Reserved_First.5.player.school3,
                    brf_5_s = Blue_Reserved_First.5.player.matched_school, brf_5_code  = Blue_Reserved_First.5.player.assigned_code,
                    
                    bof_1_po = Blue_Open_First.1.player.payoff, bof_1_s1 = Blue_Open_First.1.player.school1,
                    bof_1_s2 = Blue_Open_First.1.player.school2, bof_1_s3 = Blue_Open_First.1.player.school3,
                    bof_1_s = Blue_Open_First.1.player.matched_school, bof_1_code  = Blue_Open_First.1.player.assigned_code,
                    
                    bof_2_po = Blue_Open_First.2.player.payoff, bof_2_s1 = Blue_Open_First.2.player.school1,
                    bof_2_s2 = Blue_Open_First.2.player.school2, bof_2_s3 = Blue_Open_First.2.player.school3,
                    bof_2_s = Blue_Open_First.2.player.matched_school, bof_2_code  = Blue_Open_First.2.player.assigned_code,
                    
                    bof_3_po = Blue_Open_First.3.player.payoff, bof_3_s1 = Blue_Open_First.3.player.school1,
                    bof_3_s2 = Blue_Open_First.3.player.school2, bof_3_s3 = Blue_Open_First.3.player.school3,
                    bof_3_s = Blue_Open_First.3.player.matched_school, bof_3_code  = Blue_Open_First.3.player.assigned_code,
                    
                    bof_4_po = Blue_Open_First.4.player.payoff, bof_4_s1 = Blue_Open_First.4.player.school1,
                    bof_4_s2 = Blue_Open_First.4.player.school2, bof_4_s3 = Blue_Open_First.4.player.school3,
                    bof_4_s = Blue_Open_First.4.player.matched_school, bof_4_code  = Blue_Open_First.4.player.assigned_code,
                    
                    bof_5_po = Blue_Open_First.5.player.payoff, bof_5_s1 = Blue_Open_First.5.player.school1,
                    bof_5_s2 = Blue_Open_First.5.player.school2, bof_5_s3 = Blue_Open_First.5.player.school3,
                    bof_5_s = Blue_Open_First.5.player.matched_school, bof_5_code  = Blue_Open_First.5.player.assigned_code,

                    survey1 = Survey.1.player.survey1, survey2 = Survey.1.player.survey2, survey3 = Survey.1.player.survey3, 
                    survey4 = Survey.1.player.survey4, survey5 = Survey.1.player.survey5, survey6 = Survey.1.player.survey6, 
                    survey7 = Survey.1.player.survey7, survey8 = Survey.1.player.survey8, survey9 = Survey.1.player.survey9, 
                    survey10 = Survey.1.player.survey10, survey11 = Survey.1.player.survey11, survey12 = Survey.1.player.survey12, 
                    survey13 = Survey.1.player.survey13, survey14 = Survey.1.player.survey14, survey15 = Survey.1.player.survey15,
                    period_earning = PayRandomApp.1.player.final_period_pay, total_earning = PayRandomApp.1.player.final_payment
)

df_2 <- df_2 %>% dplyr::select(session, subject.id, ttc, q_payoff, period_earning, total_earning, gof_1_po:survey15)


df_2_code <- df_2 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_code"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "c",
    values_to = "role"
  ) %>% 
  separate(c, c("treatment", "period", "x"), sep="_") %>% 
  select(-c("period", "x"))

df_2_s1 <- df_2 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_s1"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "s1",
    values_to = "s1_reported"
  ) %>% 
  separate(s1, c("treatment", "period", "x"), sep="_") %>% 
  select("s1_reported")

df_2_s2 <- df_2 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_s2"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "s2",
    values_to = "s2_reported"
  ) %>% 
  separate(s2, c("treatment", "period", "x"), sep="_") %>% 
  select("s2_reported")

df_2_s3 <- df_2 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_s3"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "s3",
    values_to = "s3_reported"
  ) %>% 
  separate(s3, c("treatment", "period", "x"), sep="_") %>% 
  select("s3_reported")

df_2_s <- df_2 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_s"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "s",
    values_to = "s_assigned"
  ) %>% 
  separate(s, c("treatment", "period", "x"), sep="_") %>% 
  select("s_assigned")

df_2_po <- df_2 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_po"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "c",
    values_to = "payoff"
  ) %>% 
  separate(c, c("treatment", "period", "x"), sep="_") %>% 
  select("payoff")

long_df_2 <- cbind(df_2_code, df_2_s1, df_2_s2, df_2_s3, df_2_s, df_2_po, period)

##---------------------------------------------------------------
##             Load Restructure data - T3 and T7               --
##---------------------------------------------------------------

range <- c(7:9,19:21)

for(i in range){
  if(i < 13){x <- read_excel(paste("C://Users//kccsu//Desktop//SchoolChoice//Data//DA_", i, ".xlsx", sep = "")) %>%
    dplyr::select(
      ttc,
      participant.id_in_session, participant.code, Comprehensive.1.player.quiz_payoff:Comprehensive.1.player.quiz13,
      Blue_Open_First_PR.1.player.payoff:Blue_Open_First_PR.1.player.assigned_code,
      Blue_Open_First_PR.2.player.payoff:Blue_Open_First_PR.2.player.assigned_code,
      Blue_Reserved_First_PR.1.player.payoff:Blue_Reserved_First_PR.1.player.assigned_code,
      Blue_Reserved_First_PR.2.player.payoff:Blue_Reserved_First_PR.2.player.assigned_code,
      
      Blue_Reserved_First.1.player.payoff:Blue_Reserved_First.1.player.assigned_code,
      Blue_Reserved_First.2.player.payoff:Blue_Reserved_First.2.player.assigned_code,
      Blue_Reserved_First.3.player.payoff:Blue_Reserved_First.3.player.assigned_code,
      Blue_Reserved_First.4.player.payoff:Blue_Reserved_First.4.player.assigned_code,
      Blue_Reserved_First.5.player.payoff:Blue_Reserved_First.5.player.assigned_code,
      
      Blue_Open_First.1.player.payoff:Blue_Open_First.1.player.assigned_code,
      Blue_Open_First.2.player.payoff:Blue_Open_First.2.player.assigned_code,
      Blue_Open_First.3.player.payoff:Blue_Open_First.3.player.assigned_code,
      Blue_Open_First.4.player.payoff:Blue_Open_First.4.player.assigned_code,
      Blue_Open_First.5.player.payoff:Blue_Open_First.5.player.assigned_code,
      
      Green_Open_First.1.player.payoff:Green_Open_First.1.player.assigned_code,
      Green_Open_First.2.player.payoff:Green_Open_First.2.player.assigned_code,
      Green_Open_First.3.player.payoff:Green_Open_First.3.player.assigned_code,
      Green_Open_First.4.player.payoff:Green_Open_First.4.player.assigned_code,
      Green_Open_First.5.player.payoff:Green_Open_First.5.player.assigned_code,
      
      Green_Reserved_First.1.player.payoff:Green_Reserved_First.1.player.assigned_code,
      Green_Reserved_First.2.player.payoff:Green_Reserved_First.2.player.assigned_code,
      Green_Reserved_First.3.player.payoff:Green_Reserved_First.3.player.assigned_code,
      Green_Reserved_First.4.player.payoff:Green_Reserved_First.4.player.assigned_code,
      Green_Reserved_First.5.player.payoff:Green_Reserved_First.5.player.assigned_code,
      Survey.1.player.survey1:Survey.1.player.survey15,
      PayRandomApp.1.player.app_to_pay:PayRandomApp.1.player.final_payment) %>% mutate(session = i)
  nam <- paste("dt", i, sep = "_")
  assign(nam, x)
  }else{x <- read_excel(paste("C://Users//kccsu//Desktop//SchoolChoice//Data//TTC_", i, ".xlsx", sep = "")) %>%
    dplyr::select(
      ttc,
      participant.id_in_session, participant.code, Comprehensive.1.player.quiz_payoff:Comprehensive.1.player.quiz13,
      Blue_Open_First_PR.1.player.payoff:Blue_Open_First_PR.1.player.assigned_code,
      Blue_Open_First_PR.2.player.payoff:Blue_Open_First_PR.2.player.assigned_code,
      Blue_Reserved_First_PR.1.player.payoff:Blue_Reserved_First_PR.1.player.assigned_code,
      Blue_Reserved_First_PR.2.player.payoff:Blue_Reserved_First_PR.2.player.assigned_code,
      
      Blue_Reserved_First.1.player.payoff:Blue_Reserved_First.1.player.assigned_code,
      Blue_Reserved_First.2.player.payoff:Blue_Reserved_First.2.player.assigned_code,
      Blue_Reserved_First.3.player.payoff:Blue_Reserved_First.3.player.assigned_code,
      Blue_Reserved_First.4.player.payoff:Blue_Reserved_First.4.player.assigned_code,
      Blue_Reserved_First.5.player.payoff:Blue_Reserved_First.5.player.assigned_code,
      
      Blue_Open_First.1.player.payoff:Blue_Open_First.1.player.assigned_code,
      Blue_Open_First.2.player.payoff:Blue_Open_First.2.player.assigned_code,
      Blue_Open_First.3.player.payoff:Blue_Open_First.3.player.assigned_code,
      Blue_Open_First.4.player.payoff:Blue_Open_First.4.player.assigned_code,
      Blue_Open_First.5.player.payoff:Blue_Open_First.5.player.assigned_code,
      
      Green_Open_First.1.player.payoff:Green_Open_First.1.player.assigned_code,
      Green_Open_First.2.player.payoff:Green_Open_First.2.player.assigned_code,
      Green_Open_First.3.player.payoff:Green_Open_First.3.player.assigned_code,
      Green_Open_First.4.player.payoff:Green_Open_First.4.player.assigned_code,
      Green_Open_First.5.player.payoff:Green_Open_First.5.player.assigned_code,
      
      Green_Reserved_First.1.player.payoff:Green_Reserved_First.1.player.assigned_code,
      Green_Reserved_First.2.player.payoff:Green_Reserved_First.2.player.assigned_code,
      Green_Reserved_First.3.player.payoff:Green_Reserved_First.3.player.assigned_code,
      Green_Reserved_First.4.player.payoff:Green_Reserved_First.4.player.assigned_code,
      Green_Reserved_First.5.player.payoff:Green_Reserved_First.5.player.assigned_code,
      Survey.1.player.survey1:Survey.1.player.survey15,
      PayRandomApp.1.player.app_to_pay:PayRandomApp.1.player.final_payment) %>% mutate(session = i)
  nam <- paste("dt", i, sep = "_")
  assign(nam, x)
  }
}


df_3 <- rbind(dt_7, dt_8, dt_9, dt_19, dt_20, dt_21)
df_3 <- df_3 %>% rename(subject.id = participant.id_in_session,
                        q_payoff = Comprehensive.1.player.quiz_payoff, q1 = Comprehensive.1.player.quiz1, q2 = Comprehensive.1.player.quiz2,
                        q3 = Comprehensive.1.player.quiz3, q4 = Comprehensive.1.player.quiz4, q5 = Comprehensive.1.player.quiz5,
                        q6 = Comprehensive.1.player.quiz6, q7 = Comprehensive.1.player.quiz7, q8 = Comprehensive.1.player.quiz8,
                        q9 = Comprehensive.1.player.quiz9, q10 = Comprehensive.1.player.quiz10, q11 = Comprehensive.1.player.quiz11,
                        q12 = Comprehensive.1.player.quiz12, q13 = Comprehensive.1.player.quiz13, 
                        
                        bof_pp1_po = Blue_Open_First_PR.1.player.payoff, bof_pp1_s1= Blue_Open_First_PR.1.player.school1, 
                        bof_pp1_s2= Blue_Open_First_PR.1.player.school2,bof_pp1_s3= Blue_Open_First_PR.1.player.school3, 
                        bof_pp1_s= Blue_Open_First_PR.1.player.matched_school, bof_pp1_code = Blue_Open_First_PR.1.player.assigned_code,
                        
                        bof_pp2_po = Blue_Open_First_PR.2.player.payoff, bof_pp2_s1= Blue_Open_First_PR.2.player.school1, 
                        bof_pp2_s2= Blue_Open_First_PR.2.player.school2,bof_pp2_s3= Blue_Open_First_PR.2.player.school3, 
                        bof_pp2_s= Blue_Open_First_PR.2.player.matched_school, bof_pp2_code = Blue_Open_First_PR.2.player.assigned_code,
                        
                        brf_pp1_po = Blue_Reserved_First_PR.1.player.payoff, brf_pp1_s1= Blue_Reserved_First_PR.1.player.school1, 
                        brf_pp1_s2= Blue_Reserved_First_PR.1.player.school2, brf_pp1_s3= Blue_Reserved_First_PR.1.player.school3, 
                        brf_pp1_s= Blue_Reserved_First_PR.1.player.matched_school, brf_pp1_code = Blue_Reserved_First_PR.1.player.assigned_code, 
                        
                        brf_pp2_po = Blue_Reserved_First_PR.2.player.payoff, brf_pp2_s1= Blue_Reserved_First_PR.2.player.school1, 
                        brf_pp2_s2= Blue_Reserved_First_PR.2.player.school2, brf_pp2_s3= Blue_Reserved_First_PR.2.player.school3,
                        brf_pp2_= Blue_Reserved_First_PR.2.player.matched_school, brf_pp2_code = Blue_Reserved_First_PR.2.player.assigned_code,
                        
                        brf_1_po = Blue_Reserved_First.1.player.payoff, brf_1_s1 = Blue_Reserved_First.1.player.school1,
                        brf_1_s2 = Blue_Reserved_First.1.player.school2, brf_1_s3 = Blue_Reserved_First.1.player.school3,
                        brf_1_s = Blue_Reserved_First.1.player.matched_school, brf_1_code  = Blue_Reserved_First.1.player.assigned_code,
                        
                        brf_2_po = Blue_Reserved_First.2.player.payoff, brf_2_s1 = Blue_Reserved_First.2.player.school1,
                        brf_2_s2 = Blue_Reserved_First.2.player.school2, brf_2_s3 = Blue_Reserved_First.2.player.school3,
                        brf_2_s = Blue_Reserved_First.2.player.matched_school, brf_2_code  = Blue_Reserved_First.2.player.assigned_code,
                        
                        brf_3_po = Blue_Reserved_First.3.player.payoff, brf_3_s1 = Blue_Reserved_First.3.player.school1,
                        brf_3_s2 = Blue_Reserved_First.3.player.school2, brf_3_s3 = Blue_Reserved_First.3.player.school3,
                        brf_3_s = Blue_Reserved_First.3.player.matched_school, brf_3_code  = Blue_Reserved_First.3.player.assigned_code,
                        
                        brf_4_po = Blue_Reserved_First.4.player.payoff, brf_4_s1 = Blue_Reserved_First.4.player.school1,
                        brf_4_s2 = Blue_Reserved_First.4.player.school2, brf_4_s3 = Blue_Reserved_First.4.player.school3,
                        brf_4_s = Blue_Reserved_First.4.player.matched_school, brf_4_code  = Blue_Reserved_First.4.player.assigned_code,
                        
                        brf_5_po = Blue_Reserved_First.5.player.payoff, brf_5_s1 = Blue_Reserved_First.5.player.school1,
                        brf_5_s2 = Blue_Reserved_First.5.player.school2, brf_5_s3 = Blue_Reserved_First.5.player.school3,
                        brf_5_s = Blue_Reserved_First.5.player.matched_school, brf_5_code  = Blue_Reserved_First.5.player.assigned_code,
                        
                        bof_1_po = Blue_Open_First.1.player.payoff, bof_1_s1 = Blue_Open_First.1.player.school1,
                        bof_1_s2 = Blue_Open_First.1.player.school2, bof_1_s3 = Blue_Open_First.1.player.school3,
                        bof_1_s = Blue_Open_First.1.player.matched_school, bof_1_code  = Blue_Open_First.1.player.assigned_code,
                        
                        bof_2_po = Blue_Open_First.2.player.payoff, bof_2_s1 = Blue_Open_First.2.player.school1,
                        bof_2_s2 = Blue_Open_First.2.player.school2, bof_2_s3 = Blue_Open_First.2.player.school3,
                        bof_2_s = Blue_Open_First.2.player.matched_school, bof_2_code  = Blue_Open_First.2.player.assigned_code,
                        
                        bof_3_po = Blue_Open_First.3.player.payoff, bof_3_s1 = Blue_Open_First.3.player.school1,
                        bof_3_s2 = Blue_Open_First.3.player.school2, bof_3_s3 = Blue_Open_First.3.player.school3,
                        bof_3_s = Blue_Open_First.3.player.matched_school, bof_3_code  = Blue_Open_First.3.player.assigned_code,
                        
                        bof_4_po = Blue_Open_First.4.player.payoff, bof_4_s1 = Blue_Open_First.4.player.school1,
                        bof_4_s2 = Blue_Open_First.4.player.school2, bof_4_s3 = Blue_Open_First.4.player.school3,
                        bof_4_s = Blue_Open_First.4.player.matched_school, bof_4_code  = Blue_Open_First.4.player.assigned_code,
                        
                        bof_5_po = Blue_Open_First.5.player.payoff, bof_5_s1 = Blue_Open_First.5.player.school1,
                        bof_5_s2 = Blue_Open_First.5.player.school2, bof_5_s3 = Blue_Open_First.5.player.school3,
                        bof_5_s = Blue_Open_First.5.player.matched_school, bof_5_code  = Blue_Open_First.5.player.assigned_code,
                        
                        gof_1_po = Green_Open_First.1.player.payoff, gof_1_s1 = Green_Open_First.1.player.school1,
                        gof_1_s2 = Green_Open_First.1.player.school2, gof_1_s3 = Green_Open_First.1.player.school3,
                        gof_1_s = Green_Open_First.1.player.matched_school, gof_1_code  = Green_Open_First.1.player.assigned_code,
                        
                        gof_2_po = Green_Open_First.2.player.payoff, gof_2_s1 = Green_Open_First.2.player.school1,
                        gof_2_s2 = Green_Open_First.2.player.school2, gof_2_s3 = Green_Open_First.2.player.school3,
                        gof_2_s = Green_Open_First.2.player.matched_school, gof_2_code  = Green_Open_First.2.player.assigned_code,
                        
                        gof_3_po = Green_Open_First.3.player.payoff, gof_3_s1 = Green_Open_First.3.player.school1,
                        gof_3_s2 = Green_Open_First.3.player.school2, gof_3_s3 = Green_Open_First.3.player.school3,
                        gof_3_s = Green_Open_First.3.player.matched_school, gof_3_code  = Green_Open_First.3.player.assigned_code,
                        
                        gof_4_po = Green_Open_First.4.player.payoff, gof_4_s1 = Green_Open_First.4.player.school1,
                        gof_4_s2 = Green_Open_First.4.player.school2, gof_4_s3 = Green_Open_First.4.player.school3,
                        gof_4_s = Green_Open_First.4.player.matched_school, gof_4_code  = Green_Open_First.4.player.assigned_code,
                        
                        gof_5_po = Green_Open_First.5.player.payoff, gof_5_s1 = Green_Open_First.5.player.school1,
                        gof_5_s2 = Green_Open_First.5.player.school2, gof_5_s3 = Green_Open_First.5.player.school3,
                        gof_5_s = Green_Open_First.5.player.matched_school, gof_5_code  = Green_Open_First.5.player.assigned_code,
                        
                        grf_1_po = Green_Reserved_First.1.player.payoff, grf_1_s1 = Green_Reserved_First.1.player.school1,
                        grf_1_s2 = Green_Reserved_First.1.player.school2, grf_1_s3 = Green_Reserved_First.1.player.school3,
                        grf_1_s = Green_Reserved_First.1.player.matched_school, grf_1_code  = Green_Reserved_First.1.player.assigned_code,
                        
                        grf_2_po = Green_Reserved_First.2.player.payoff, grf_2_s1 = Green_Reserved_First.2.player.school1,
                        grf_2_s2 = Green_Reserved_First.2.player.school2, grf_2_s3 = Green_Reserved_First.2.player.school3,
                        grf_2_s = Green_Reserved_First.2.player.matched_school, grf_2_code  = Green_Reserved_First.2.player.assigned_code,
                        
                        grf_3_po = Green_Reserved_First.3.player.payoff, grf_3_s1 = Green_Reserved_First.3.player.school1,
                        grf_3_s2 = Green_Reserved_First.3.player.school2, grf_3_s3 = Green_Reserved_First.3.player.school3,
                        grf_3_s = Green_Reserved_First.3.player.matched_school, grf_3_code  = Green_Reserved_First.3.player.assigned_code,
                        
                        grf_4_po = Green_Reserved_First.4.player.payoff, grf_4_s1 = Green_Reserved_First.4.player.school1,
                        grf_4_s2 = Green_Reserved_First.4.player.school2, grf_4_s3 = Green_Reserved_First.4.player.school3,
                        grf_4_s = Green_Reserved_First.4.player.matched_school, grf_4_code  = Green_Reserved_First.4.player.assigned_code,
                        
                        grf_5_po = Green_Reserved_First.5.player.payoff, grf_5_s1 = Green_Reserved_First.5.player.school1,
                        grf_5_s2 = Green_Reserved_First.5.player.school2, grf_5_s3 = Green_Reserved_First.5.player.school3,
                        grf_5_s = Green_Reserved_First.5.player.matched_school, grf_5_code  = Green_Reserved_First.5.player.assigned_code,
                         
                        survey1 = Survey.1.player.survey1, survey2 = Survey.1.player.survey2, survey3 = Survey.1.player.survey3, 
                        survey4 = Survey.1.player.survey4, survey5 = Survey.1.player.survey5, survey6 = Survey.1.player.survey6, 
                        survey7 = Survey.1.player.survey7, survey8 = Survey.1.player.survey8, survey9 = Survey.1.player.survey9, 
                        survey10 = Survey.1.player.survey10, survey11 = Survey.1.player.survey11, survey12 = Survey.1.player.survey12, 
                        survey13 = Survey.1.player.survey13, survey14 = Survey.1.player.survey14, survey15 = Survey.1.player.survey15,
                        period_earning = PayRandomApp.1.player.final_period_pay, total_earning = PayRandomApp.1.player.final_payment
)

df_3 <- df_3 %>% dplyr::select(session, subject.id, ttc, q_payoff, period_earning, total_earning, brf_1_po:survey15)


df_3_code <- df_3 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_code"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "c",
    values_to = "role"
  ) %>% 
  separate(c, c("treatment", "period", "x"), sep="_") %>% 
  select(-c("period", "x"))

df_3_s1 <- df_3 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_s1"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "s1",
    values_to = "s1_reported"
  ) %>% 
  separate(s1, c("treatment", "period", "x"), sep="_") %>% 
  select("s1_reported")

df_3_s2 <- df_3 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_s2"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "s2",
    values_to = "s2_reported"
  ) %>% 
  separate(s2, c("treatment", "period", "x"), sep="_") %>% 
  select("s2_reported")

df_3_s3 <- df_3 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_s3"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "s3",
    values_to = "s3_reported"
  ) %>% 
  separate(s3, c("treatment", "period", "x"), sep="_") %>% 
  select("s3_reported")

df_3_s <- df_3 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_s"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "s",
    values_to = "s_assigned"
  ) %>% 
  separate(s, c("treatment", "period", "x"), sep="_") %>% 
  select("s_assigned")

df_3_po <- df_3 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_po"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "c",
    values_to = "payoff"
  ) %>% 
  separate(c, c("treatment", "period", "x"), sep="_") %>% 
  select("payoff")

long_df_3 <- cbind(df_3_code, df_3_s1, df_3_s2, df_3_s3, df_3_s, df_3_po, period)

##---------------------------------------------------------------
##             Load Restructure data - T3 and T8               --
##---------------------------------------------------------------

range <- c(10:12,22:24)

for(i in range){
  if(i < 13){x <- read_excel(paste("C://Users//kccsu//Desktop//SchoolChoice//Data//DA_", i, ".xlsx", sep = "")) %>%
    dplyr::select(
      ttc,
      participant.id_in_session, participant.code, Comprehensive.1.player.quiz_payoff:Comprehensive.1.player.quiz13,
      Blue_Open_First_PR.1.player.payoff:Blue_Open_First_PR.1.player.assigned_code,
      Blue_Open_First_PR.2.player.payoff:Blue_Open_First_PR.2.player.assigned_code,
      Blue_Reserved_First_PR.1.player.payoff:Blue_Reserved_First_PR.1.player.assigned_code,
      Blue_Reserved_First_PR.2.player.payoff:Blue_Reserved_First_PR.2.player.assigned_code,
      
      Green_Reserved_First.1.player.payoff:Green_Reserved_First.1.player.assigned_code,
      Green_Reserved_First.2.player.payoff:Green_Reserved_First.2.player.assigned_code,
      Green_Reserved_First.3.player.payoff:Green_Reserved_First.3.player.assigned_code,
      Green_Reserved_First.4.player.payoff:Green_Reserved_First.4.player.assigned_code,
      Green_Reserved_First.5.player.payoff:Green_Reserved_First.5.player.assigned_code,
      
      Green_Open_First.1.player.payoff:Green_Open_First.1.player.assigned_code,
      Green_Open_First.2.player.payoff:Green_Open_First.2.player.assigned_code,
      Green_Open_First.3.player.payoff:Green_Open_First.3.player.assigned_code,
      Green_Open_First.4.player.payoff:Green_Open_First.4.player.assigned_code,
      Green_Open_First.5.player.payoff:Green_Open_First.5.player.assigned_code,
     
      Blue_Open_First.1.player.payoff:Blue_Open_First.1.player.assigned_code,
      Blue_Open_First.2.player.payoff:Blue_Open_First.2.player.assigned_code,
      Blue_Open_First.3.player.payoff:Blue_Open_First.3.player.assigned_code,
      Blue_Open_First.4.player.payoff:Blue_Open_First.4.player.assigned_code,
      Blue_Open_First.5.player.payoff:Blue_Open_First.5.player.assigned_code,
      
      Blue_Reserved_First.1.player.payoff:Blue_Reserved_First.1.player.assigned_code,
      Blue_Reserved_First.2.player.payoff:Blue_Reserved_First.2.player.assigned_code,
      Blue_Reserved_First.3.player.payoff:Blue_Reserved_First.3.player.assigned_code,
      Blue_Reserved_First.4.player.payoff:Blue_Reserved_First.4.player.assigned_code,
      Blue_Reserved_First.5.player.payoff:Blue_Reserved_First.5.player.assigned_code,
     
      Survey.1.player.survey1:Survey.1.player.survey15,
      PayRandomApp.1.player.app_to_pay:PayRandomApp.1.player.final_payment) %>% mutate(session = i)
  nam <- paste("dt", i, sep = "_")
  assign(nam, x)
  }else{x <- read_excel(paste("C://Users//kccsu//Desktop//SchoolChoice//Data//TTC_", i, ".xlsx", sep = "")) %>%
    dplyr::select(
      ttc,
      participant.id_in_session, participant.code, Comprehensive.1.player.quiz_payoff:Comprehensive.1.player.quiz13,
      Blue_Open_First_PR.1.player.payoff:Blue_Open_First_PR.1.player.assigned_code,
      Blue_Open_First_PR.2.player.payoff:Blue_Open_First_PR.2.player.assigned_code,
      Blue_Reserved_First_PR.1.player.payoff:Blue_Reserved_First_PR.1.player.assigned_code,
      Blue_Reserved_First_PR.2.player.payoff:Blue_Reserved_First_PR.2.player.assigned_code,
      
      Green_Reserved_First.1.player.payoff:Green_Reserved_First.1.player.assigned_code,
      Green_Reserved_First.2.player.payoff:Green_Reserved_First.2.player.assigned_code,
      Green_Reserved_First.3.player.payoff:Green_Reserved_First.3.player.assigned_code,
      Green_Reserved_First.4.player.payoff:Green_Reserved_First.4.player.assigned_code,
      Green_Reserved_First.5.player.payoff:Green_Reserved_First.5.player.assigned_code,
      
      Green_Open_First.1.player.payoff:Green_Open_First.1.player.assigned_code,
      Green_Open_First.2.player.payoff:Green_Open_First.2.player.assigned_code,
      Green_Open_First.3.player.payoff:Green_Open_First.3.player.assigned_code,
      Green_Open_First.4.player.payoff:Green_Open_First.4.player.assigned_code,
      Green_Open_First.5.player.payoff:Green_Open_First.5.player.assigned_code,
      
      Blue_Open_First.1.player.payoff:Blue_Open_First.1.player.assigned_code,
      Blue_Open_First.2.player.payoff:Blue_Open_First.2.player.assigned_code,
      Blue_Open_First.3.player.payoff:Blue_Open_First.3.player.assigned_code,
      Blue_Open_First.4.player.payoff:Blue_Open_First.4.player.assigned_code,
      Blue_Open_First.5.player.payoff:Blue_Open_First.5.player.assigned_code,
      
      Blue_Reserved_First.1.player.payoff:Blue_Reserved_First.1.player.assigned_code,
      Blue_Reserved_First.2.player.payoff:Blue_Reserved_First.2.player.assigned_code,
      Blue_Reserved_First.3.player.payoff:Blue_Reserved_First.3.player.assigned_code,
      Blue_Reserved_First.4.player.payoff:Blue_Reserved_First.4.player.assigned_code,
      Blue_Reserved_First.5.player.payoff:Blue_Reserved_First.5.player.assigned_code,
      Survey.1.player.survey1:Survey.1.player.survey15,
      PayRandomApp.1.player.app_to_pay:PayRandomApp.1.player.final_payment) %>% mutate(session = i)
  nam <- paste("dt", i, sep = "_")
  assign(nam, x)
  }
}


df_4 <- rbind(dt_10, dt_11, dt_12, dt_22, dt_23, dt_24)
df_4 <- df_4 %>% rename(subject.id = participant.id_in_session,
                        q_payoff = Comprehensive.1.player.quiz_payoff, q1 = Comprehensive.1.player.quiz1, q2 = Comprehensive.1.player.quiz2,
                        q3 = Comprehensive.1.player.quiz3, q4 = Comprehensive.1.player.quiz4, q5 = Comprehensive.1.player.quiz5,
                        q6 = Comprehensive.1.player.quiz6, q7 = Comprehensive.1.player.quiz7, q8 = Comprehensive.1.player.quiz8,
                        q9 = Comprehensive.1.player.quiz9, q10 = Comprehensive.1.player.quiz10, q11 = Comprehensive.1.player.quiz11,
                        q12 = Comprehensive.1.player.quiz12, q13 = Comprehensive.1.player.quiz13, 
                        
                        bof_pp1_po = Blue_Open_First_PR.1.player.payoff, bof_pp1_s1= Blue_Open_First_PR.1.player.school1, 
                        bof_pp1_s2= Blue_Open_First_PR.1.player.school2,bof_pp1_s3= Blue_Open_First_PR.1.player.school3, 
                        bof_pp1_s= Blue_Open_First_PR.1.player.matched_school, bof_pp1_code = Blue_Open_First_PR.1.player.assigned_code,
                        
                        bof_pp2_po = Blue_Open_First_PR.2.player.payoff, bof_pp2_s1= Blue_Open_First_PR.2.player.school1, 
                        bof_pp2_s2= Blue_Open_First_PR.2.player.school2,bof_pp2_s3= Blue_Open_First_PR.2.player.school3, 
                        bof_pp2_s= Blue_Open_First_PR.2.player.matched_school, bof_pp2_code = Blue_Open_First_PR.2.player.assigned_code,
                        
                        brf_pp1_po = Blue_Reserved_First_PR.1.player.payoff, brf_pp1_s1= Blue_Reserved_First_PR.1.player.school1, 
                        brf_pp1_s2= Blue_Reserved_First_PR.1.player.school2, brf_pp1_s3= Blue_Reserved_First_PR.1.player.school3, 
                        brf_pp1_s= Blue_Reserved_First_PR.1.player.matched_school, brf_pp1_code = Blue_Reserved_First_PR.1.player.assigned_code, 
                        
                        brf_pp2_po = Blue_Reserved_First_PR.2.player.payoff, brf_pp2_s1= Blue_Reserved_First_PR.2.player.school1, 
                        brf_pp2_s2= Blue_Reserved_First_PR.2.player.school2, brf_pp2_s3= Blue_Reserved_First_PR.2.player.school3,
                        brf_pp2_= Blue_Reserved_First_PR.2.player.matched_school, brf_pp2_code = Blue_Reserved_First_PR.2.player.assigned_code,
                        
                        grf_1_po = Green_Reserved_First.1.player.payoff, grf_1_s1 = Green_Reserved_First.1.player.school1,
                        grf_1_s2 = Green_Reserved_First.1.player.school2, grf_1_s3 = Green_Reserved_First.1.player.school3,
                        grf_1_s = Green_Reserved_First.1.player.matched_school, grf_1_code  = Green_Reserved_First.1.player.assigned_code,
                        
                        grf_2_po = Green_Reserved_First.2.player.payoff, grf_2_s1 = Green_Reserved_First.2.player.school1,
                        grf_2_s2 = Green_Reserved_First.2.player.school2, grf_2_s3 = Green_Reserved_First.2.player.school3,
                        grf_2_s = Green_Reserved_First.2.player.matched_school, grf_2_code  = Green_Reserved_First.2.player.assigned_code,
                        
                        grf_3_po = Green_Reserved_First.3.player.payoff, grf_3_s1 = Green_Reserved_First.3.player.school1,
                        grf_3_s2 = Green_Reserved_First.3.player.school2, grf_3_s3 = Green_Reserved_First.3.player.school3,
                        grf_3_s = Green_Reserved_First.3.player.matched_school, grf_3_code  = Green_Reserved_First.3.player.assigned_code,
                        
                        grf_4_po = Green_Reserved_First.4.player.payoff, grf_4_s1 = Green_Reserved_First.4.player.school1,
                        grf_4_s2 = Green_Reserved_First.4.player.school2, grf_4_s3 = Green_Reserved_First.4.player.school3,
                        grf_4_s = Green_Reserved_First.4.player.matched_school, grf_4_code  = Green_Reserved_First.4.player.assigned_code,
                        
                        grf_5_po = Green_Reserved_First.5.player.payoff, grf_5_s1 = Green_Reserved_First.5.player.school1,
                        grf_5_s2 = Green_Reserved_First.5.player.school2, grf_5_s3 = Green_Reserved_First.5.player.school3,
                        grf_5_s = Green_Reserved_First.5.player.matched_school, grf_5_code  = Green_Reserved_First.5.player.assigned_code,
                        
                        gof_1_po = Green_Open_First.1.player.payoff, gof_1_s1 = Green_Open_First.1.player.school1,
                        gof_1_s2 = Green_Open_First.1.player.school2, gof_1_s3 = Green_Open_First.1.player.school3,
                        gof_1_s = Green_Open_First.1.player.matched_school, gof_1_code  = Green_Open_First.1.player.assigned_code,
                        
                        gof_2_po = Green_Open_First.2.player.payoff, gof_2_s1 = Green_Open_First.2.player.school1,
                        gof_2_s2 = Green_Open_First.2.player.school2, gof_2_s3 = Green_Open_First.2.player.school3,
                        gof_2_s = Green_Open_First.2.player.matched_school, gof_2_code  = Green_Open_First.2.player.assigned_code,
                        
                        gof_3_po = Green_Open_First.3.player.payoff, gof_3_s1 = Green_Open_First.3.player.school1,
                        gof_3_s2 = Green_Open_First.3.player.school2, gof_3_s3 = Green_Open_First.3.player.school3,
                        gof_3_s = Green_Open_First.3.player.matched_school, gof_3_code  = Green_Open_First.3.player.assigned_code,
                        
                        gof_4_po = Green_Open_First.4.player.payoff, gof_4_s1 = Green_Open_First.4.player.school1,
                        gof_4_s2 = Green_Open_First.4.player.school2, gof_4_s3 = Green_Open_First.4.player.school3,
                        gof_4_s = Green_Open_First.4.player.matched_school, gof_4_code  = Green_Open_First.4.player.assigned_code,
                        
                        gof_5_po = Green_Open_First.5.player.payoff, gof_5_s1 = Green_Open_First.5.player.school1,
                        gof_5_s2 = Green_Open_First.5.player.school2, gof_5_s3 = Green_Open_First.5.player.school3,
                        gof_5_s = Green_Open_First.5.player.matched_school, gof_5_code  = Green_Open_First.5.player.assigned_code,
                        
                        bof_1_po = Blue_Open_First.1.player.payoff, bof_1_s1 = Blue_Open_First.1.player.school1,
                        bof_1_s2 = Blue_Open_First.1.player.school2, bof_1_s3 = Blue_Open_First.1.player.school3,
                        bof_1_s = Blue_Open_First.1.player.matched_school, bof_1_code  = Blue_Open_First.1.player.assigned_code,
                        
                        bof_2_po = Blue_Open_First.2.player.payoff, bof_2_s1 = Blue_Open_First.2.player.school1,
                        bof_2_s2 = Blue_Open_First.2.player.school2, bof_2_s3 = Blue_Open_First.2.player.school3,
                        bof_2_s = Blue_Open_First.2.player.matched_school, bof_2_code  = Blue_Open_First.2.player.assigned_code,
                        
                        bof_3_po = Blue_Open_First.3.player.payoff, bof_3_s1 = Blue_Open_First.3.player.school1,
                        bof_3_s2 = Blue_Open_First.3.player.school2, bof_3_s3 = Blue_Open_First.3.player.school3,
                        bof_3_s = Blue_Open_First.3.player.matched_school, bof_3_code  = Blue_Open_First.3.player.assigned_code,
                        
                        bof_4_po = Blue_Open_First.4.player.payoff, bof_4_s1 = Blue_Open_First.4.player.school1,
                        bof_4_s2 = Blue_Open_First.4.player.school2, bof_4_s3 = Blue_Open_First.4.player.school3,
                        bof_4_s = Blue_Open_First.4.player.matched_school, bof_4_code  = Blue_Open_First.4.player.assigned_code,
                        
                        bof_5_po = Blue_Open_First.5.player.payoff, bof_5_s1 = Blue_Open_First.5.player.school1,
                        bof_5_s2 = Blue_Open_First.5.player.school2, bof_5_s3 = Blue_Open_First.5.player.school3,
                        bof_5_s = Blue_Open_First.5.player.matched_school, bof_5_code  = Blue_Open_First.5.player.assigned_code,
                        
                        brf_1_po = Blue_Reserved_First.1.player.payoff, brf_1_s1 = Blue_Reserved_First.1.player.school1,
                        brf_1_s2 = Blue_Reserved_First.1.player.school2, brf_1_s3 = Blue_Reserved_First.1.player.school3,
                        brf_1_s = Blue_Reserved_First.1.player.matched_school, brf_1_code  = Blue_Reserved_First.1.player.assigned_code,
                        
                        brf_2_po = Blue_Reserved_First.2.player.payoff, brf_2_s1 = Blue_Reserved_First.2.player.school1,
                        brf_2_s2 = Blue_Reserved_First.2.player.school2, brf_2_s3 = Blue_Reserved_First.2.player.school3,
                        brf_2_s = Blue_Reserved_First.2.player.matched_school, brf_2_code  = Blue_Reserved_First.2.player.assigned_code,
                        
                        brf_3_po = Blue_Reserved_First.3.player.payoff, brf_3_s1 = Blue_Reserved_First.3.player.school1,
                        brf_3_s2 = Blue_Reserved_First.3.player.school2, brf_3_s3 = Blue_Reserved_First.3.player.school3,
                        brf_3_s = Blue_Reserved_First.3.player.matched_school, brf_3_code  = Blue_Reserved_First.3.player.assigned_code,
                        
                        brf_4_po = Blue_Reserved_First.4.player.payoff, brf_4_s1 = Blue_Reserved_First.4.player.school1,
                        brf_4_s2 = Blue_Reserved_First.4.player.school2, brf_4_s3 = Blue_Reserved_First.4.player.school3,
                        brf_4_s = Blue_Reserved_First.4.player.matched_school, brf_4_code  = Blue_Reserved_First.4.player.assigned_code,
                        
                        brf_5_po = Blue_Reserved_First.5.player.payoff, brf_5_s1 = Blue_Reserved_First.5.player.school1,
                        brf_5_s2 = Blue_Reserved_First.5.player.school2, brf_5_s3 = Blue_Reserved_First.5.player.school3,
                        brf_5_s = Blue_Reserved_First.5.player.matched_school, brf_5_code  = Blue_Reserved_First.5.player.assigned_code,
     
                        survey1 = Survey.1.player.survey1, survey2 = Survey.1.player.survey2, survey3 = Survey.1.player.survey3, 
                        survey4 = Survey.1.player.survey4, survey5 = Survey.1.player.survey5, survey6 = Survey.1.player.survey6, 
                        survey7 = Survey.1.player.survey7, survey8 = Survey.1.player.survey8, survey9 = Survey.1.player.survey9, 
                        survey10 = Survey.1.player.survey10, survey11 = Survey.1.player.survey11, survey12 = Survey.1.player.survey12, 
                        survey13 = Survey.1.player.survey13, survey14 = Survey.1.player.survey14, survey15 = Survey.1.player.survey15,
                        period_earning = PayRandomApp.1.player.final_period_pay, total_earning = PayRandomApp.1.player.final_payment
)

df_4 <- df_4 %>% dplyr::select(session, subject.id, ttc, q_payoff, period_earning, total_earning, grf_1_po:survey15)


df_4_code <- df_4 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_code"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "c",
    values_to = "role"
  ) %>% 
  separate(c, c("treatment", "period", "x"), sep="_") %>% 
  select(-c("period", "x"))

df_4_s1 <- df_4 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_s1"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "s1",
    values_to = "s1_reported"
  ) %>% 
  separate(s1, c("treatment", "period", "x"), sep="_") %>% 
  select("s1_reported")

df_4_s2 <- df_4 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_s2"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "s2",
    values_to = "s2_reported"
  ) %>% 
  separate(s2, c("treatment", "period", "x"), sep="_") %>% 
  select("s2_reported")

df_4_s3 <- df_4 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_s3"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "s3",
    values_to = "s3_reported"
  ) %>% 
  separate(s3, c("treatment", "period", "x"), sep="_") %>% 
  select("s3_reported")

df_4_s <- df_4 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_s"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "s",
    values_to = "s_assigned"
  ) %>% 
  separate(s, c("treatment", "period", "x"), sep="_") %>% 
  select("s_assigned")

df_4_po <- df_4 %>% dplyr::select(session, subject.id, ttc, q_payoff, ends_with("_po"), survey1:survey15) %>%
  pivot_longer(
    cols = -c("session", "subject.id", "ttc", "q_payoff", "survey1":"survey15"),
    names_to = "c",
    values_to = "payoff"
  ) %>% 
  separate(c, c("treatment", "period", "x"), sep="_") %>% 
  select("payoff")

long_df_4 <- cbind(df_4_code, df_4_s1, df_4_s2, df_4_s3, df_4_s, df_4_po, period)

##---------------------------------------------------------------
##               Combine long_df_1:long_df_4                   --
##---------------------------------------------------------------

long_df <- rbind(long_df_1, long_df_2, long_df_3, long_df_4)

##---------------------------------------------------------------
##                      mutating variables                     --
##---------------------------------------------------------------

long_df %<>%
  mutate(session = as.numeric(session),
         subject.id = session*6-(6-subject.id),
         q_payoff = as.numeric(q_payoff),
         period = as.numeric(period),
         role = as.numeric(role),
         payoff = as.numeric(payoff),
         minority = as.numeric(ifelse((role == "1" | role == "2" | role == "3" | role == "4"), "0",
                                      ifelse((role == "5" | role == "6"), 
                                             "1", "none"))),
         open = as.numeric(ifelse((treatment == "bof" | treatment == "gof"), "1",
                                  ifelse((treatment == "brf" | treatment == "grf"), 
                                         "0", "none"))),
         blue = as.numeric(ifelse((treatment == "bof" | treatment == "brf"), "1",
                                  ifelse((treatment == "gof" | treatment == "grf"), 
                                         "0", "none"))),
         s1_true = ifelse((role == "1" | role == "2" | role == "4"), "B",
                          ifelse((role == "3" | role == "5" | role == "6"), 
                                 "C", "none")),
         s2_true = ifelse((role == "1" | role == "2" | role == "4"), "C",
                          ifelse((role == "3" | role == "5" | role == "6"), 
                                 "B", "none")),
         s3_true = ifelse((role == "1" | role == "2" | role == "4"), "A",
                          ifelse((role == "3" | role == "5" | role == "6"), 
                                 "A", "none")),
         true_1 = as.numeric(ifelse((s1_reported == s1_true), "1", "0")),
         true_2 = as.numeric(ifelse((s2_reported == s2_true), "1", "0")),
         true_3 = as.numeric(ifelse((s3_reported == s3_true), "1", "0")),
         true_all = as.numeric(ifelse((true_1 == "1" & true_2 == "1" & true_3 == "1"), "1", "0")),
         assigned_rank = as.numeric(ifelse((role == "1" & s_assigned == "B" | role == "2" & s_assigned == "B" | role == "4" & s_assigned == "B" |
                                              role == "3" & s_assigned == "C" | role == "5" & s_assigned == "C" | role == "6" & s_assigned == "C"), "1",
                                           ifelse((role == "1" & s_assigned == "C" | role == "2" & s_assigned == "C" | role == "4" & s_assigned == "C" |
                                                     role == "3" & s_assigned == "B" | role == "5" & s_assigned == "B" | role == "6" & s_assigned == "B"), "2", "3"))),
         white = as.numeric(ifelse(survey2 == "White", "1", "0")),
         black = as.numeric(ifelse(survey2 == "Black/ African-American", "1", "0")),
         asian = as.numeric(ifelse(survey2 == "Asian", "1", "0")),
         female = as.numeric(ifelse(survey1 == "Female", "1", "0")),
         male = as.numeric(ifelse(survey1 == "Other/ Wish not to answer", "1", "0")),
         hispanic = as.numeric(ifelse(survey3 == "Yes", "1", "0")),
         q_pass = as.numeric(ifelse(q_payoff > 10, "1", "0")))

long_df <- long_df %>%
  rename(age = survey4,
         years = survey5,
         major = survey6,
         econ = survey7,
         gpa = survey8)

long_df$major <- toupper(long_df$major)

long_df <- long_df[, c("session", "subject.id", "period", "ttc", "q_payoff", "q_pass", "role", "minority", "open", "blue", "s_assigned", "payoff", 
                       "s1_reported", "s2_reported", "s3_reported", "s1_true", "s2_true", "s3_true", "true_1", "true_2", "true_3", "true_all", "assigned_rank", 
                       "white", "black", "asian", "female", "male", "hispanic", "survey9", "survey10", "survey11", "survey12",
                       "survey13", "survey14", "survey15")]

write_xlsx(long_df, 
           "C://Users//kccsu//Desktop//SchoolChoice//Data//ind_dattc.xlsx")


##---------------------------------------------------------------
##                        group level data                     --
##---------------------------------------------------------------
##---------------------------------------------------------------
##                         for stability                       --
##---------------------------------------------------------------

g_long_df <- rbind(long_df_1, long_df_2, long_df_3, long_df_4)
write_xlsx(g_long_df, 
           "C://Users//kccsu//Desktop//SchoolChoice//Data//g_ind_dattc.xlsx")

##---------------------------------------------------------------
##                      mutating variables                     --
##---------------------------------------------------------------
g_long_df <- read_xlsx("C://Users//kccsu//Desktop//SchoolChoice//Data//g_ind_dattc.xlsx")
g_long_df %<>%
  mutate(session = as.numeric(session),
         #subject.id = session*6-(6-subject.id),
         q_payoff = as.numeric(q_payoff),
         period = as.numeric(period),
         role = as.numeric(role),
         payoff = as.numeric(payoff),
         minority = as.numeric(ifelse((role == "1" | role == "2" | role == "3" | role == "4"), "0",
                                      ifelse((role == "5" | role == "6"), 
                                             "1", "none"))),
         open = as.numeric(ifelse((treatment == "bof" | treatment == "gof"), "1",
                                  ifelse((treatment == "brf" | treatment == "grf"), 
                                         "0", "none"))),
         blue = as.numeric(ifelse((treatment == "bof" | treatment == "brf"), "1",
                                  ifelse((treatment == "gof" | treatment == "grf"), 
                                         "0", "none"))),
         s1_true = ifelse((role == "1" | role == "2" | role == "4"), "B",
                          ifelse((role == "3" | role == "5" | role == "6"), 
                                 "C", "none")),
         s2_true = ifelse((role == "1" | role == "2" | role == "4"), "C",
                          ifelse((role == "3" | role == "5" | role == "6"), 
                                 "B", "none")),
         s3_true = ifelse((role == "1" | role == "2" | role == "4"), "A",
                          ifelse((role == "3" | role == "5" | role == "6"), 
                                 "A", "none")),
         true_1 = as.numeric(ifelse((s1_reported == s1_true), "1", "0")),
         true_2 = as.numeric(ifelse((s2_reported == s2_true), "1", "0")),
         true_3 = as.numeric(ifelse((s3_reported == s3_true), "1", "0")),
         true_all = as.numeric(ifelse((true_1 == "1" & true_2 == "1" & true_3 == "1"), "1", "0")),
         assigned_rank = as.numeric(ifelse((role == "1" & s_assigned == "B" | role == "2" & s_assigned == "B" | role == "4" & s_assigned == "B" |
                                              role == "3" & s_assigned == "C" | role == "5" & s_assigned == "C" | role == "6" & s_assigned == "C"), "1",
                                           ifelse((role == "1" & s_assigned == "C" | role == "2" & s_assigned == "C" | role == "4" & s_assigned == "C" |
                                                     role == "3" & s_assigned == "B" | role == "5" & s_assigned == "B" | role == "6" & s_assigned == "B"), "2", "3"))))

g_long_df <- g_long_df[, c("session", "subject.id", "period", "ttc", "q_payoff", "role", "minority", "open", "blue", "s_assigned", "payoff", 
                       "s1_reported", "s2_reported", "s3_reported")] #, "s1_true", "s2_true", "s3_true", "true_1", "true_2", "true_3", "true_all", "assigned_rank", 
                       #"survey1", "survey2", "survey3", "survey4", "survey5", "survey6", "survey7", "survey8", "survey9", "survey10", "survey11", "survey12",
                       #"survey13", "survey14", "survey15")]

  
g_wide_df <- g_long_df %>%
  pivot_wider(
    id_cols = c("session", "period", "ttc", "open", "blue"),
    names_from = "role", #which column (or columns) to get the name of the output column
    values_from = c("s_assigned", "s1_reported", "s2_reported", "s3_reported", "payoff", "role") #which column (or columns) to get the cell values from
  )


g_wide_df <- select(g_wide_df,"session", "period", "ttc", "open", "blue", 
                    #"role_1", "role_2", "role_3", "role_4", "role_5", "role_6",     
                    "payoff_1", "payoff_2", "payoff_3", "payoff_4", "payoff_5", "payoff_6", 
                    "s_assigned_1", "s_assigned_2","s_assigned_3", "s_assigned_4", "s_assigned_5", "s_assigned_6",
                    "s1_reported_1", "s1_reported_2", "s1_reported_3", "s1_reported_4", "s1_reported_5", "s1_reported_6",
                    "s2_reported_1", "s2_reported_2", "s2_reported_3", "s2_reported_4", "s2_reported_5", "s2_reported_6",
                    "s3_reported_1", "s3_reported_2", "s3_reported_3", "s3_reported_4", "s3_reported_5", "s3_reported_6",)


##---------------------------------------------------------------
##                  export data to run python                  --
##---------------------------------------------------------------
pydata <- g_wide_df %>% dplyr::select(session, period, ttc, open, blue, s1_reported_1:s3_reported_6) %>%
  rowwise() %>%
  mutate('1' = paste(c_across(ends_with("1")), collapse = ""),
         '2' = paste(c_across(ends_with("2")), collapse = ""),
         '3' = paste(c_across(ends_with("3")), collapse = ""),
         '4' = paste(c_across(ends_with("4")), collapse = ""),
         '5' = paste(c_across(ends_with("5")), collapse = ""),
         '6' = paste(c_across(ends_with("6")), collapse = "")) 

pydata <- pydata %>% dplyr::select(session, period, ttc, open, blue, '1':'6') %>%
  arrange(ttc, blue, open)
write_xlsx(pydata, 
           "C://Users//kccsu//Desktop//SchoolChoice//Data//cleaned_data//pydata.xlsx")

ttc_open_blue <- pydata %>% filter(ttc == 1 & open == 1 & blue == 1) 
write_xlsx(ttc_open_blue,"C://Users//kccsu//Desktop//SchoolChoice//Data//Python//ttc_open_blue.xlsx")

ttc_reserved_blue <- pydata %>% filter(ttc == 1 & open == 0 & blue == 1) 
write_xlsx(ttc_reserved_blue,"C://Users//kccsu//Desktop//SchoolChoice//Data//Python//ttc_reserved_blue.xlsx")

ttc_reserved_green <- pydata %>% filter(ttc == 1 & open == 0 & blue == 0) 
write_xlsx(ttc_reserved_green,"C://Users//kccsu//Desktop//SchoolChoice//Data//Python//ttc_reserved_green.xlsx")

ttc_open_green <- pydata %>% filter(ttc == 1 & open == 1 & blue == 0) 
write_xlsx(ttc_open_green,"C://Users//kccsu//Desktop//SchoolChoice//Data//Python//ttc_open_green.xlsx")

da_open_blue <- pydata %>% filter(ttc == 0 & open == 1 & blue == 1) 
write_xlsx(da_open_blue,"C://Users//kccsu//Desktop//SchoolChoice//Data//Python//da_open_blue.xlsx")

da_open_green <- pydata %>% filter(ttc == 0 & open == 1 & blue == 0) 
write_xlsx(da_open_green,"C://Users//kccsu//Desktop//SchoolChoice//Data//Python//da_open_green.xlsx")

da_reserved_blue <- pydata %>% filter(ttc == 0 & open == 0 & blue == 1) 
write_xlsx(da_reserved_blue,"C://Users//kccsu//Desktop//SchoolChoice//Data//Python//da_reserved_blue.xlsx")

da_reserved_green <- pydata %>% filter(ttc == 0 & open == 0 & blue == 0) 
write_xlsx(da_reserved_green,"C://Users//kccsu//Desktop//SchoolChoice//Data//Python//da_reserved_green.xlsx")

##-------------------------------------------------------------
##                    import seat type data                  --
##-------------------------------------------------------------
ttc_open_blue <- read_xlsx("C://Users//kccsu//Desktop//SchoolChoice//Data//Python//ttc_open_blue_output.xlsx")
ttc_reserved_blue <- read_xlsx("C://Users//kccsu//Desktop//SchoolChoice//Data//Python//ttc_reserved_blue_output.xlsx")
ttc_open_green <- read_xlsx("C://Users//kccsu//Desktop//SchoolChoice//Data//Python//ttc_open_green_output.xlsx")
ttc_reserved_green <- read_xlsx("C://Users//kccsu//Desktop//SchoolChoice//Data//Python//ttc_reserved_green_output.xlsx")
da_open_blue <- read_xlsx("C://Users//kccsu//Desktop//SchoolChoice//Data//Python//da_open_blue_output.xlsx")
da_reserved_blue <- read_xlsx("C://Users//kccsu//Desktop//SchoolChoice//Data//Python//da_reserved_blue_output.xlsx")
da_open_green <- read_xlsx("C://Users//kccsu//Desktop//SchoolChoice//Data//Python//da_open_green_output.xlsx")
da_reserved_green <- read_xlsx("C://Users//kccsu//Desktop//SchoolChoice//Data//Python//da_reserved_green_output.xlsx")

python_output <- rbind(ttc_open_blue, ttc_reserved_blue, ttc_open_green, ttc_reserved_green,
                       da_open_blue, da_reserved_blue, da_open_green, da_reserved_green)

g_wide_merged <- merge(g_wide_df, python_output, 
                       by = c("session", "period", "ttc", "open", "blue",
                              "s_assigned_1", "s_assigned_2", "s_assigned_3", "s_assigned_4", "s_assigned_5", "s_assigned_6"))

g_wide_sorted <- g_wide_merged[order(g_wide_merged$session, g_wide_merged$period, decreasing = c(FALSE, FALSE)), ]
write_xlsx(g_wide_sorted, 
           "C://Users//kccsu//Desktop//SchoolChoice//Data//g_wide_python.xlsx")

##-------------------------------------------------------------
##              create stability and JE columns              --
##-------------------------------------------------------------
# First calculate instances of justified envy for each school, then depending on the payoffs
# replace the values with those calculated values or zeros. 

g_wide_python <- read_xlsx("C://Users//kccsu//Desktop//SchoolChoice//Data//g_wide_python.xlsx")

g_wide_df_stability <- g_wide_python %>% 
  mutate(
    num_envy_1_B = 0, # anyone who got school B and has lower priority than 1
    num_envy_1_B = ifelse(s_assigned_2 == "B", num_envy_1_B + 1, num_envy_1_B),
    num_envy_1_B = ifelse(s_assigned_4 == "B", num_envy_1_B + 1, num_envy_1_B),
    num_envy_1_B = ifelse(s_assigned_5 == "B" & blue == 0 & seat_5 == "O", num_envy_1_B + 1, num_envy_1_B),
    num_envy_1_B = ifelse(s_assigned_6 == "B" & blue == 0 & seat_6 == "O", num_envy_1_B + 1, num_envy_1_B),
    num_envy_1_C = 0, 
    num_envy_1_C = ifelse(s_assigned_3 == "C" & blue == 1, num_envy_1_C + 1, num_envy_1_C),
    num_envy_1_C = ifelse(s_assigned_6 == "C" & blue == 1 & seat_6 == "O", num_envy_1_C + 1, num_envy_1_C),    
    num_envy_1_C = ifelse(s_assigned_2 == "C", num_envy_1_C + 1, num_envy_1_C),
    num_envy_1_C = ifelse(s_assigned_4 == "C", num_envy_1_C + 1, num_envy_1_C),
    num_envy_1_C = ifelse(payoff_1 == 10, 0, num_envy_1_C),
    envied_1 = ifelse(payoff_1 == 10, num_envy_1_B,
                      ifelse(payoff_1 == 0, num_envy_1_C + num_envy_1_B,
                             ifelse(payoff_1 == 20, 0, 0))),
    num_envy_2_B = 0,
    num_envy_2_B = ifelse(s_assigned_4 == "B", num_envy_2_B + 1, num_envy_2_B),
    num_envy_2_B = ifelse(s_assigned_5 == "B" & blue == "0" & seat_5 == "O", num_envy_2_B + 1, num_envy_2_B),
    num_envy_2_B = ifelse(s_assigned_6 == "B" & blue == "0" & seat_6 == "O", num_envy_2_B + 1, num_envy_2_B),
    num_envy_2_C = 0,
    num_envy_2_C = ifelse(s_assigned_4 == "C", num_envy_2_C + 1, num_envy_2_C),
    envied_2 = ifelse(payoff_2 == 10, num_envy_2_B,
                      ifelse(payoff_2 == 0, num_envy_2_C + num_envy_2_B,
                             ifelse(payoff_2 == 20, 0, 0))),
    
    num_envy_3_C = 0, # anyone who got school C and has lower priority than 3
    num_envy_3_C = ifelse(s_assigned_2 == "C", num_envy_3_C + 1, num_envy_3_C),
    num_envy_3_C = ifelse(s_assigned_4 == "C", num_envy_3_C + 1, num_envy_3_C),
    num_envy_3_C = ifelse(s_assigned_1 == "C" & blue == 0, num_envy_3_C + 1, num_envy_3_C),
    num_envy_3_C = ifelse(s_assigned_5 == "C" & blue == 0 & seat_5 == "O", num_envy_3_C + 1, num_envy_3_C),
    num_envy_3_B = 0, # anyone who got school B and has lower priority than 3
    num_envy_3_B = ifelse(s_assigned_1 == "B", num_envy_3_B + 1, num_envy_3_B),
    num_envy_3_B = ifelse(s_assigned_2 == "B", num_envy_3_B + 1, num_envy_3_B),
    num_envy_3_B = ifelse(s_assigned_4 == "B", num_envy_3_B + 1, num_envy_3_B),
    num_envy_3_B = ifelse(s_assigned_5 == "B" & seat_5 == "O", num_envy_3_B + 1, num_envy_3_B),
    num_envy_3_B = ifelse(s_assigned_6 == "B" & seat_6 == "O", num_envy_3_B + 1, num_envy_3_B),
    envied_3 = ifelse(payoff_3 == 10, num_envy_3_C,
                      ifelse(payoff_3 == 0, num_envy_3_C + num_envy_3_B,
                             ifelse(payoff_3 == 20, 0, 0))),
    envied_4 = 0,
    
    num_envy_5_C = 0, # anyone who got school C and has lower priority than 5
    num_envy_5_C = ifelse(s_assigned_1 == "C", num_envy_5_C + 1, num_envy_5_C),
    num_envy_5_C = ifelse(s_assigned_2 == "C", num_envy_5_C + 1, num_envy_5_C),
    num_envy_5_C = ifelse(s_assigned_4 == "C", num_envy_5_C + 1, num_envy_5_C),
    num_envy_5_C = ifelse(s_assigned_3 == "C" & blue == 1, num_envy_5_C + 1, num_envy_5_C),
    num_envy_5_C = ifelse(s_assigned_3 == "C" & blue == 0 & seat_3 == "R", num_envy_5_C + 1, num_envy_5_C),
    num_envy_5_C = ifelse(s_assigned_6 == "C" & blue == 1, num_envy_5_C + 1, num_envy_5_C),
    num_envy_5_B = 0,
    num_envy_5_B = ifelse(s_assigned_6 == "B", num_envy_5_B + 1, num_envy_5_B),
    num_envy_5_B = ifelse(s_assigned_1 == "B" & blue == 1, num_envy_5_B + 1, num_envy_5_B),
    num_envy_5_B = ifelse(s_assigned_1 == "B" & blue == 0 & seat_1 == "R", num_envy_5_B + 1, num_envy_5_B),
    num_envy_5_B = ifelse(s_assigned_2 == "B" & blue == 1, num_envy_5_B + 1, num_envy_5_B),
    num_envy_5_B = ifelse(s_assigned_4 == "B" & blue == 1, num_envy_5_B + 1, num_envy_5_B),
    num_envy_5_B = ifelse(s_assigned_2 == "B" & blue == 0 & seat_2 == "R", num_envy_5_B + 1, num_envy_5_B),
    num_envy_5_B = ifelse(s_assigned_4 == "B" & blue == 0 & seat_4 == "R", num_envy_5_B + 1, num_envy_5_B), 
    num_envy_5_B = ifelse(s_assigned_3 == "B" & seat_3 == "R", num_envy_5_B + 1, num_envy_5_B), 
    envied_5 = ifelse(payoff_5 == 10, num_envy_5_C,
                      ifelse(payoff_5 == 0, num_envy_5_C + num_envy_5_B,
                             ifelse(payoff_5 == 20, 0, 0))),

    num_envy_6_C = 0, # anyone who got school C and has lower priority than 6
    num_envy_6_C = ifelse(s_assigned_5 == "C" & blue == 0, num_envy_6_C + 1, num_envy_6_C),
    num_envy_6_C = ifelse(s_assigned_1 == "C" & blue == 0, num_envy_6_C + 1, num_envy_6_C),
    num_envy_6_C = ifelse(s_assigned_1 == "C" & blue == 1 & seat_6 == "R", num_envy_6_C + 1, num_envy_6_C),
    num_envy_6_C = ifelse(s_assigned_3 == "C", num_envy_6_C + 1, num_envy_6_C),
    num_envy_6_C = ifelse(s_assigned_2 == "C", num_envy_6_C + 1, num_envy_6_C),
    num_envy_6_C = ifelse(s_assigned_4 == "C", num_envy_6_C + 1, num_envy_6_C),
    num_envy_6_B = 0,
    num_envy_6_B = ifelse(s_assigned_1 == "B" & blue == 1, num_envy_6_B + 1, num_envy_6_B),
    num_envy_6_B = ifelse(s_assigned_1 == "B" & blue == 0 & seat_1 == "R", num_envy_6_B + 1, num_envy_6_B),
    num_envy_6_B = ifelse(s_assigned_2 == "B" & blue == 1, num_envy_6_B + 1, num_envy_6_B),
    num_envy_6_B = ifelse(s_assigned_2 == "B" & blue == 0 & seat_2 == "R", num_envy_6_B + 1, num_envy_6_B),
    num_envy_6_B = ifelse(s_assigned_4 == "B" & blue == 1, num_envy_6_B + 1, num_envy_6_B),
    num_envy_6_B = ifelse(s_assigned_4 == "B" & blue == 0 & seat_4 == "R", num_envy_6_B + 1, num_envy_6_B),
    num_envy_6_B = ifelse(s_assigned_3 == "B" & seat_3 == "R", num_envy_6_B + 1, num_envy_6_B),
    envied_6 = ifelse(payoff_6 == 10, num_envy_6_C,
                      ifelse(payoff_6 == 0, num_envy_6_C + num_envy_6_B,
                             ifelse(payoff_6 == 20, 0, 0))))

g_wide_df_stability <- g_wide_df_stability %>% 
  dplyr::select(envied_1, envied_2, envied_3, envied_4, envied_5, envied_6) %>%
  mutate(envied = rowSums(across(c(envied_1:envied_6))),
         stable = ifelse(envied > 0, 0, 1))

g_wide_df <- cbind(g_wide_python, g_wide_df_stability)

# Check if way of defining stability is correct:
check <- g_wide_df %>% 
  mutate(aspredicted = ifelse(ttc == 0 & s1_reported_1 == "B" &  s1_reported_2 == "B" & s1_reported_3 == "C" &  s1_reported_4 == "B" &  
                                s1_reported_5 == "C" &  s1_reported_6 == "C" & s2_reported_1 == "C" &  s2_reported_2 == "C" &
                                s2_reported_3 == "B" &  s2_reported_4 == "C" & s2_reported_5 == "B" &  s2_reported_6 == "B", 0, 1),
         correct = ifelse(aspredicted == 0 & envied == 0 & stable == 1, 0, 1))

comparison <- check$aspredicted == check$correct
same_value <- all(comparison) # if same_value is TRUE then we are good. 

write_xlsx(g_wide_df, 
           "C://Users//kccsu//Desktop//SchoolChoice//Data//group_level_dt.xlsx")

##---------------------------------------------------------------
##               converting group level stability              --
##---------------------------------------------------------------
##---------------------------------------------------------------
##                data to individual level data                --
##---------------------------------------------------------------

ind_s <- g_wide_df %>% dplyr::select(session, period, ttc, open, blue, starts_with("s_")) %>%
  pivot_longer(
    cols = -c("session", "period", "ttc", "open", "blue"),
    names_to = "c", # can name it as any
    values_to = "s_assigned" # name of the column you want the values to be transformed
  ) %>% 
  #s_assigned_1: s is school, assigned is valuename; what the column values are, 1 is role
  separate(c, c("school", "valuename", "role"), sep="_") %>% 
  select(-c("school", "valuename"))

ind_s1 <- g_wide_df %>% dplyr::select(session, period, ttc, open, blue, starts_with("s1_")) %>%
  pivot_longer(
    cols = -c("session", "period", "ttc", "open", "blue"),
    names_to = "s1", # can name it as any
    values_to = "s1_reported" # name of the column you want the values to be transformed
  ) %>% 
  separate(s1, c("school", "valuename", "role"), sep="_") %>% 
  select("s1_reported")

ind_s2 <- g_wide_df %>% dplyr::select(session, period, ttc, open, blue, starts_with("s2_")) %>%
  pivot_longer(
    cols = -c("session", "period", "ttc", "open", "blue"),
    names_to = "s2", # can name it as any
    values_to = "s2_reported" # name of the column you want the values to be transformed
  ) %>% 
  separate(s2, c("school", "valuename", "role"), sep="_") %>% 
  select("s2_reported")

ind_s3 <- g_wide_df %>% dplyr::select(session, period, ttc, open, blue, starts_with("s3_")) %>%
  pivot_longer(
    cols = -c("session", "period", "ttc", "open", "blue"),
    names_to = "s3", # can name it as any
    values_to = "s3_reported" # name of the column you want the values to be transformed
  ) %>% 
  separate(s3, c("school", "valuename", "role"), sep="_") %>% 
  select("s3_reported")

ind_envied <- g_wide_df %>% dplyr::select(session, period, ttc, open, blue, starts_with("envied_")) %>%
  pivot_longer(
    cols = -c("session", "period", "ttc", "open", "blue"),
    names_to = "e", # can name it as any
    values_to = "envied" # name of the column you want the values to be transformed
  ) %>% 
  separate(e, c("school", "valuename"), sep="_") %>% 
  select("envied")

ind_long_df <- cbind(ind_s, ind_s1, ind_s2, ind_s3, ind_envied)

ind_long_merged <- merge(long_df, ind_long_df, 
                       by = c("session", "period", "ttc", "open", "blue", "role",
                              "s_assigned", "s1_reported", "s2_reported", "s3_reported"))

ind_long_merged <- ind_long_merged %>% 
  mutate(envy = ifelse(envied > 0, 1, 0))

write_xlsx(ind_long_merged, 
           "C://Users//kccsu//Desktop//SchoolChoice//Data//ind_long_df.xlsx")

##################################################################################################
##---------------------------------------------------------------
##                  export data to run python                  --
##---------------------------------------------------------------
pydata <- g_wide_df %>% dplyr::select(session, period, ttc, open, blue, s1_reported_1:s3_reported_6) %>%
  rowwise() %>%
  mutate('1' = paste(c_across(ends_with("1")), collapse = ""),
         '2' = paste(c_across(ends_with("2")), collapse = ""),
         '3' = paste(c_across(ends_with("3")), collapse = ""),
         '4' = paste(c_across(ends_with("4")), collapse = ""),
         '5' = paste(c_across(ends_with("5")), collapse = ""),
         '6' = paste(c_across(ends_with("6")), collapse = ""),
         '7' = "BCA", # for 1,2, and 4
         '8' = "CBA") # for 3,5, and 6

pydata <- pydata %>% dplyr::select(session, period, ttc, open, blue, '1':'8') %>%
  arrange(ttc, blue, open)

ttc_open_blue <- pydata %>% filter(ttc == 1 & open == 1 & blue == 1) 
write_xlsx(ttc_open_blue,"C://Users//kccsu//Desktop//SchoolChoice//Data//Strategy//ttc_open_blue.xlsx")

ttc_reserved_blue <- pydata %>% filter(ttc == 1 & open == 0 & blue == 1) 
write_xlsx(ttc_reserved_blue,"C://Users//kccsu//Desktop//SchoolChoice//Data//Strategy//ttc_reserved_blue.xlsx")

ttc_reserved_green <- pydata %>% filter(ttc == 1 & open == 0 & blue == 0) 
write_xlsx(ttc_reserved_green,"C://Users//kccsu//Desktop//SchoolChoice//Data//Strategy//ttc_reserved_green.xlsx")

ttc_open_green <- pydata %>% filter(ttc == 1 & open == 1 & blue == 0) 
write_xlsx(ttc_open_green,"C://Users//kccsu//Desktop//SchoolChoice//Data//Strategy//ttc_open_green.xlsx")

da_open_blue <- pydata %>% filter(ttc == 0 & open == 1 & blue == 1) 
write_xlsx(da_open_blue,"C://Users//kccsu//Desktop//SchoolChoice//Data//Strategy//da_open_blue.xlsx")

da_open_green <- pydata %>% filter(ttc == 0 & open == 1 & blue == 0) 
write_xlsx(da_open_green,"C://Users//kccsu//Desktop//SchoolChoice//Data//Strategy//da_open_green.xlsx")

da_reserved_blue <- pydata %>% filter(ttc == 0 & open == 0 & blue == 1) 
write_xlsx(da_reserved_blue,"C://Users//kccsu//Desktop//SchoolChoice//Data//Strategy//da_reserved_blue.xlsx")

da_reserved_green <- pydata %>% filter(ttc == 0 & open == 0 & blue == 0) 
write_xlsx(da_reserved_green,"C://Users//kccsu//Desktop//SchoolChoice//Data//Strategy//da_reserved_green.xlsx")

##---------------------------------------------------------------
##                  adding consequential columns               --
##---------------------------------------------------------------

long_df <- long_df %>%
  mutate(consequential = ifelse(true_all == 0 & (s_assigned != s_c_assigned), 1, 0),
         c_assigned_rank = as.numeric(ifelse((role == "1" & s_c_assigned == "B" | role == "2" & s_c_assigned == "B" | role == "4" & s_c_assigned == "B" |
                                                role == "3" & s_c_assigned == "C" | role == "5" & s_c_assigned == "C" | role == "6" & s_c_assigned == "C"), "1",
                                             ifelse((role == "1" & s_c_assigned == "C" | role == "2" & s_c_assigned == "C" | role == "4" & s_c_assigned == "C" |
                                                       role == "3" & s_c_assigned == "B" | role == "5" & s_c_assigned == "B" | role == "6" & s_c_assigned == "B"), "2", "3"))),
         benefit = ifelse(consequential == 1 & (assigned_rank < c_assigned_rank), 1, 0),
         true_alt = ifelse(consequential == 0 | true_all == 1, 1, 0))

