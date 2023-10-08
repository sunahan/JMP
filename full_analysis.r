######################### Load Packages #######################
library(readxl)
library(MASS)
library(viridis)
library(miceadds)
library(gdata)
library(latex2exp)
library(ggplot2)
library(ggpubr)
library(stats)
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
library(ggeffects)
library(margins)
library(scales)
library(car)
library(ordinal)
#################################################################
##---------------------------------------------------------------
##                         Load data                           --
##---------------------------------------------------------------
long_df <- read_xlsx("C://Users//san4//Dropbox//An_SunAh//Exp_Reserve_Data//ind_long_df.xlsx")
long_df <- read_xlsx("C://Users//kccsu//Desktop//SchoolChoice//Data//cleaned//ind_long_df.xlsx")

long_df <- long_df %>%
  mutate(ttc = as.factor(ttc),
         minority = as.factor(minority),
         open = as.factor(open),
         blue = as.factor(blue),
         role = as.factor(role),
         assigned_rank = as.factor(assigned_rank))

long_df_blue <- long_df %>% 
  filter(blue == 1)


long_df_green <- long_df %>% 
  filter(blue == 0)

##-------------------------------------------------------------
##                      strategic behavior                   --
##-------------------------------------------------------------
##-------------------------------------------------------------
##         Table 3: Truth-telling rates by mechanism         --
##-------------------------------------------------------------
tt_none <- long_df %>% 
  group_by(ttc) %>%
  summarize(round(mean(none_true),4))

tt_top <- long_df %>% 
  group_by(ttc) %>%
  summarize(round(mean(true_1),4))

tt_all <- long_df %>% 
  group_by(ttc) %>%
  summarize(round(mean(true_all),4))

##-------------------------------------------------------------
##    Table 4: Truth-telling rates by mechanism, po, types   --
##-------------------------------------------------------------
tt_all_ttc_open_min <- long_df %>% 
  group_by(open, ttc, minority) %>%
  summarize(round(mean(true_all),4))

##-------------------------------------------------------------
##          Table 5: Truth-telling probit regression         --
##-------------------------------------------------------------
tt_reg_simple_blue <- glm(true_all ~ ttc + open + minority, 
                    family = binomial(link = "probit"), 
                    data = long_df_blue)

tt_reg_simple_green <- glm(true_all ~ ttc + open + minority, 
                          family = binomial(link = "probit"), 
                          data = long_df_green)

tt_reg_blue <- glm(true_all ~ period + I(period^2) + open * minority * ttc,
              family = binomial(link = "probit"),
              data = long_df_blue)

tt_reg_green <- glm(true_all ~ period + I(period^2) + open * minority * ttc,
                   family = binomial(link = "probit"),
                   data = long_df_green)

# Add clustered standard errors
tt_reg_simple_blue_clustered <- glm.cluster(true_all ~ ttc + open + minority,  
                                      family = binomial(link = "probit"), 
                                      cluster="session",
                                      data = long_df_blue)

tt_reg_simple_green_clustered <- glm.cluster(true_all ~ ttc + open + minority,  
                                            family = binomial(link = "probit"), 
                                            cluster="session",
                                            data = long_df_green)

tt_reg_blue_clustered <- glm.cluster(true_all ~ period + I(period^2) + open * minority * ttc,
                                          family = binomial(link = "probit"), 
                                          cluster="session",
                                          data = long_df_blue)

tt_reg_green_clustered <- glm.cluster(true_all ~ period + I(period^2) + open * minority * ttc,
                                     family = binomial(link = "probit"), 
                                     cluster="session",
                                     data = long_df_green)

# Put them into tables
stargazer(tt_reg_simple, tt_reg,
          title = "Truthful reporting",
          column.labels = c("first choice", "second choice"),
          label = "Tab:Truthful_reporting",
          omit = "subject.id", single.row = T)

##-------------------------------------------------------------
##                Wilcoxon Rank-Sum on TT                    --
##-------------------------------------------------------------
# Wilcoxon Rank-sum unmatched test to see if ttc and da are different non-parametrically
# Subset data for ttc = 1 and 0
group1 <- subset(long_df_blue, ttc == 1)$true_all
group2 <- subset(long_df_blue, ttc == 0)$true_all
result <- wilcox.test(group1, group2, correct = FALSE)
result <- t.test(group1, group2) # student's t-test
print(result)

# OF and RF
group1 <- subset(long_df_blue, open == 1)$true_all
group2 <- subset(long_df_blue, open == 0)$true_all
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

# Minority and majority
group1 <- subset(long_df_blue, minority == 1)$true_all
group2 <- subset(long_df_blue, minority == 0)$true_all
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

# Minority and majority
group1 <- subset(long_df_green, open == 0 & minority == 1)$true_all
group2 <- subset(long_df_green, open == 1 & minority == 1)$true_all
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

##-------------------------------------------------------------
##                tt: Joint Hypothesis testing               --
##-------------------------------------------------------------
# Likelihood raio test 
# Fit the reduced model (without interaction terms)
tt_reg_reduced_open <- glm(true_all ~ period + I(period^2) + open + minority * ttc,
                           family = binomial(link = "probit"),
                           data = long_df_blue)
lrt_open <- lrtest(tt_reg_blue, tt_reg_reduced_open)

tt_reg_reduced_min <- glm(true_all ~ period + I(period^2) + minority + open * ttc,
                          family = binomial(link = "probit"),
                          data = long_df_blue)
lrt_min <- lrtest(tt_reg_blue, tt_reg_reduced_min)

tt_reg_reduced_ttc <- glm(true_all ~ period + I(period^2) + ttc + minority * open,
                          family = binomial(link = "probit"),
                          data = long_df_blue)
lrt_ttc <- lrtest(tt_reg_blue, tt_reg_reduced_ttc)

# Print the test result
print(lrt_open)
print(lrt_min)
print(lrt_ttc)

##-------------------------------------------------------------
##               tt by open, minority under blue             --
##-------------------------------------------------------------
tt_reg_blue <- glm(true_all ~ period + I(period^2) + open * minority * ttc,
                   family = binomial(link = "probit"),
                   data = long_df_blue)

tt_blue <-  ggeffect(tt_reg_blue, terms = c("open", "minority")) # x = open, group = minority

tt_blue_plot <- ggplot(tt_blue, aes(x = x, y = predicted)) +
  geom_point(aes(shape = group), size = 5, position = position_dodge(0.2)) +
  scale_shape_manual(values = c(17, 0), labels = c("Majority", "Minority")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group), 
                position = position_dodge(0.2), width = 0.2) +
  scale_color_grey() + 
  theme_classic() +
  labs(y = "Rates of Truth-telling", x = "Precedence Order") +
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 17),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(breaks = c(0,1), label = c("Reserved \n First", "Open \n First")) +
  guides(color = "none") +
  theme(panel.border = element_rect(color = "black", size = 0.7, fill = NA)) +
  theme(panel.spacing = unit(0.5, "lines")) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.5, linetype = "dotted")) + # Vertical grid lines
  theme(panel.grid.major.x = element_blank()) +  # Remove horizontal grid lines
  theme(panel.grid.minor = element_blank())  # Remove minor grid lines

H0 <- "open = open:minority"

# Perform the Wald test
wald_test <- waldtest(tt_reg_blue, hypothesis = H0)


# Print the test results
summary(wald_test)

##-------------------------------------------------------------
##               tt by open, minority under green            --
##-------------------------------------------------------------
tt_green <-  ggeffect(tt_reg_green, terms = c("open", "minority")) # x = open, group = minority

tt_green_plot <- ggplot(tt_green, aes(x = x, y = predicted)) +
  geom_point(aes(shape = group), size = 5, position = position_dodge(0.2)) +
  scale_shape_manual(values = c(17, 0), labels = c("Majority", "Minority")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group), 
                position = position_dodge(0.2), width = 0.2) +
  scale_color_grey() + 
  theme_classic() +
  labs(y = "Rates of Truth-telling", x = "Precedence Order") +
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 17),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(breaks = c(0,1), label = c("Reserved \n First", "Open \n First")) +
  guides(color = "none") + 
  theme(panel.border = element_rect(color = "black", size = 0.7, fill = NA)) +
  theme(panel.spacing = unit(0.5, "lines")) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.5, linetype = "dotted")) + # Vertical grid lines
  theme(panel.grid.major.x = element_blank()) +  # Remove horizontal grid lines
  theme(panel.grid.minor = element_blank())  # Remove minor grid lines

tt_green_plot

##-------------------------------------------------------------
##                     strategy distribution                 --
##-------------------------------------------------------------  
strategy_data <- long_df %>%
  mutate(role_124 = as.numeric(ifelse((role == "1" | role == "2" | role =="4"), "1", "0")),
         strategy = case_when((s1_reported == "A" & s2_reported == "B" & s3_reported == "C") ~ "abc", 
                              (s1_reported == "A" & s2_reported == "C" & s3_reported == "B") ~ "acb",          
                              (s1_reported == "B" & s2_reported == "A" & s3_reported == "C") ~ "bac", 
                              (s1_reported == "B" & s2_reported == "C" & s3_reported == "A") ~ "bca",
                              (s1_reported == "C" & s2_reported == "A" & s3_reported == "B") ~ "cab",
                              (s1_reported == "C" & s2_reported == "B" & s3_reported == "A") ~ "cba",TRUE ~ "0"))

strategy_data$role_124 <- factor(strategy_data$role_124, levels = c(0,1))
#strategy_data$ttc <- factor(strategy_data$ttc, levels = c(0,1))
ttc_labs <- c("0" = "Deferred Acceptance", "1" = "Top Trading Cycles")
blue_labs <- c("0" = "Green", "1" = "Blue")

strategy_hist <- ggplot(strategy_data, aes(x = strategy, fill = role_124, group = role_124)) + 
  geom_bar(aes(y = ..count../sum(..count..)), width = 0.5, alpha = 1, position = position_dodge()) +
  #facet_wrap(~blue, labeller = labeller(blue = blue_labs)) +
  labs(x = "Possible strategies", y = "") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = c("0"="gray78","1"="gray39"), labels = c("Student 3,5,6", "Student 1,2,4")) +
  theme_classic() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.position = "top") +
  theme(panel.border = element_rect(color = "black", size = 0.7, fill = NA)) +
  theme(panel.spacing = unit(0.5, "lines")) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.5, linetype = "dotted")) + # Vertical grid lines
  theme(panel.grid.major.x = element_blank()) +  # Remove horizontal grid lines
  theme(panel.grid.minor = element_blank())  # Remove minor grid lines

# Do students flipping their first and second choice school because they have higher priority at the second school?
strategy_data <- strategy_data %>%
  mutate(flip = as.numeric(case_when((role == "1" & strategy == "cba" & blue == "1") ~ "1", 
                                     (role == "3" & strategy == "bca" & blue == "1") ~ "1",
                                     (role == "3" & strategy == "bca" & blue == "0" & open == "1") ~ "1",
                                     (role == "5" & strategy == "bca" & blue == "0" & open == "0") ~ "1",TRUE ~ "0")))
# number of students who flip
n_flip <- nrow(subset(strategy_data, (role_124 == "1" & strategy == "cba") | (role_124 == "0" & strategy == "bca")))/2880
# number of students who flip bc they have higher priority at the respective sch
mean(strategy_data$flip)


                          
##-------------------------------------------------------------
##                          Efficiency                       --
##-------------------------------------------------------------
##-------------------------------------------------------------
##               Table 6: Efficiency regression              --
##-------------------------------------------------------------
# Run regressions
eff_reg_simple_blue <- lm(payoff ~ ttc + open + minority, 
                             data = long_df_blue, 
                          cluster="session")
eff_reg_simple_green <- lm(payoff ~ ttc + open + minority, 
                             data = long_df_green,
                           cluster="session")
eff_reg_blue <- lm(payoff ~ period + I(period^2) + open * minority * ttc,  
                     data = long_df_blue,
                   cluster="session")
eff_reg_green <- lm(payoff ~ period + I(period^2) + open * minority * ttc,  
                      data = long_df_green,
                    cluster="session")

# Make Table 6
stargazer(payoff_reg_simple,
          payoff_reg,
          title = "payoff_impact",
          column.labels = c("standard", "w/out learning"),
          label = "payoff_impact",
          omit = "subject.id", single.row = T)

# Fixed Effect model and with Learning Effect (ctrl + shift + c)
# payoff_plm_simple <- plm(payoff ~ ttc + minority + open, 
#                          data = long_df, index = c("subject.id"), model = "within")
# payoff_plm_reg <- plm(payoff ~ ttc * open * blue * minority + period,  
#                       data = long_df, index = c("subject.id"), model = "within")
# payoff_int_learning_ols <- lm(payoff ~ ttc * open * blue * minority + period, 
#                               data = long_df %>% filter(period > 10))
# payoff_int_learning_plm <- plm(payoff ~ ttc * open * blue * minority + period, 
#                                data = long_df %>% filter(period > 10), index = c("subject.id"), model = "within")

##-------------------------------------------------------------
##               Wilcoxon Rank-Sum on payoff                 --
##-------------------------------------------------------------
# Subset data for ttc = 1 and 0
group1 <- subset(long_df_green, ttc == 1)$payoff
group2 <- subset(long_df_green, ttc == 0)$payoff
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

# OF and RF
group1 <- subset(long_df_blue, open == 1)$payoff
group2 <- subset(long_df_blue, open == 0)$payoff
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

# Minority and majority
group1 <- subset(long_df_blue, minority == 1)$payoff
group2 <- subset(long_df_blue, minority == 0)$payoff
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

##-------------------------------------------------------------
##              payoff: Joint Hypothesis testing             --
##-------------------------------------------------------------
# ANOVA test
# Fit the reduced model (without interaction terms)
payoff_reg_reduced_open <- lm(payoff ~ period + I(period^2) + open + minority * ttc, 
                              data = long_df_blue)
ft_open <- anova(eff_reg_blue, payoff_reg_reduced_open)

payoff_reg_reduced_min <- lm(payoff ~ period + I(period^2) + minority + open * ttc, 
                             data = long_df_green)
ft_min <- anova(eff_reg_green, payoff_reg_reduced_min)

payoff_reg_reduced_ttc <- lm(payoff ~ period + I(period^2) + ttc + minority * open, 
                             data = long_df_green)
ft_ttc <- anova(eff_reg_green, payoff_reg_reduced_ttc)

# Print the test result
print(ft_open)
print(ft_min)
print(ft_ttc)
print(ft_blue)

##-------------------------------------------------------------
##             payoff by open, minority under blue           --
##-------------------------------------------------------------
eff_theory_blue <- data.frame(
  x = c(0,1,0,1),
  predicted = c(7.5, 7.5, 20, 20),
  group = c(0, 0, 1, 1)
  
)

eff_blue <- ggeffect(eff_reg_blue, terms = c("open", "minority")) # x = open, group = minority

eff_blue_plot <- ggplot(eff_blue, aes(x = x, y = predicted)) +
  geom_point(aes(shape = group), size = 5, position = position_dodge(0.2)) +
  scale_shape_manual(values = c(17, 0), labels = c("Majority", "Minority")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group), 
                position = position_dodge(0.2), width = 0.2) +
  geom_point(data = eff_theory_blue, aes(x = as.factor(x), y = predicted, shape = as.factor(group)),
             position = position_dodge(0.2), color = "red", size = 5, show.legend = FALSE) +
  scale_color_grey() + 
  theme_classic() +
  labs(y = "Payoff", x = "Precedence Order") +
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 17),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(breaks = c(0,1), label = c("Reserved \n First", "Open \n First")) +
  guides(color = "none") + 
  theme(panel.border = element_rect(color = "black", size = 0.7, fill = NA)) +
  theme(panel.spacing = unit(0.5, "lines")) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.5, linetype = "dotted")) + # Vertical grid lines
  theme(panel.grid.major.x = element_blank()) +  # Remove horizontal grid lines
  theme(panel.grid.minor = element_blank()) # Remove minor grid lines

eff_blue_plot

##-------------------------------------------------------------
##             payoff by open, minority under green          --
##-------------------------------------------------------------
eff_theory_green <- data.frame(
  x = c(0,1,0,1),
  predicted = c(10, 7.5, 15, 20),
  group = c(0, 0, 1, 1)
  
)
eff_green <- ggeffect(eff_reg_green, terms = c("open", "minority")) # x = open, group = minority

eff_green_plot <- ggplot(eff_green, aes(x = x, y = predicted)) +
  geom_point(aes(shape = group), size = 5, position = position_dodge(0.2)) +
  scale_shape_manual(values = c(17, 0), labels = c("Majority", "Minority")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group), 
                position = position_dodge(0.2), width = 0.2) +
  geom_point(data = eff_theory_green, aes(x = as.factor(x), y = predicted, shape = as.factor(group)),
             position = position_dodge(0.2), color = "red", size = 5, show.legend = FALSE) +
  scale_color_grey() + 
  theme_classic() +
  labs(y = "Payoff", x = "Precedence Order") +
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 17),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(breaks = c(0,1), label = c("Reserved \n First", "Open \n First")) +
  guides(color = "none") + 
  theme(panel.border = element_rect(color = "black", size = 0.7, fill = NA)) +
  theme(panel.spacing = unit(0.5, "lines")) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.5, linetype = "dotted")) + # Vertical grid lines
  theme(panel.grid.major.x = element_blank()) +  # Remove horizontal grid lines
  theme(panel.grid.minor = element_blank())  # Remove minor grid lines

###############################################################
linearHypothesis(eff_reg_green,"minority1 = open1:minority1")

##-------------------------------------------------------------
##                          Stability                        --
##-------------------------------------------------------------
##-------------------------------------------------------------
##                          load data                        --
##-------------------------------------------------------------
g_wide_df <- read_xlsx("C://Users//kccsu//Desktop//SchoolChoice//Data//group_level_dt.xlsx")
g_wide_df <- read_xlsx("C://Users//san4//Dropbox//SchoolChoice//Data//cleaned//group_level_dt.xlsx")

stability_data <- g_wide_df %>% 
  mutate(ttc = as.factor(ttc),
         open = as.factor(open),
         blue = as.factor(blue))

stability_blue <- stability_data %>%
  filter(blue == 1)
stability_green <- stability_data %>%
  filter(blue == 0)

##-------------------------------------------------------------
##           Figure 4: Percentage of stable outcomes         --
##-------------------------------------------------------------
percentage_data <- g_wide_df %>%
  group_by(ttc) %>%
  summarize(percentage = mean(stable == 1, na.rm = TRUE) * 100)

sqrt(0.7*(1-0.7)/240)
sqrt(0.6583*(1-0.6583)/240)

stable_bar <- ggplot(percentage_data, aes(x = factor(ttc), y = percentage, fill = factor(ttc))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.3, alpha = 0.5) +
  scale_fill_manual(values = c("#999999", "#000000")) +
  labs(x = "", y = "Proportion") +
  scale_x_discrete(labels = c("0" = "DA", "1" = "TTC")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_classic() +
  theme(axis.text = element_text(size = 15), legend.position = "none", plot.title = element_text(hjust = 0.5, size = 20)) +
  ggtitle("Percentage of Stable Outcomes")

stable_bar

# test to see if they are statistically different
group1 <- subset(stab_reg_green, ttc == 1)$stable # RF Blue
group2 <- subset(stab_reg_green, ttc == 0)$stable # OF Bluer
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

##-------------------------------------------------------------
##              Table 7: Group level stability reg           --
##-------------------------------------------------------------
stab_reg_simple_blue <- glm(stable ~ ttc + open, 
                        family = binomial(link = "probit"), 
                        data = stability_blue) 
stab_reg_simple_green <- glm(stable ~ ttc + open, 
                            family = binomial(link = "probit"), 
                            data = stability_green) 
stab_reg_blue <- glm(stable ~ period + I(period^2) + ttc * open, 
                            family = binomial(link = "probit"), 
                            data = stability_blue) 
stab_reg_green <- glm(stable ~ period + I(period^2) + ttc * open, 
                     family = binomial(link = "probit"), 
                     data = stability_green) 

# Clustered standard errors at session level
stab_reg_simple_blue_clustered <- glm.cluster(stable ~ ttc + open, 
                                          data = stability_blue,
                                          family = binomial(link = "probit"), 
                                          cluster="session")
stab_reg_simple_green_clustered <- glm.cluster(stable ~ ttc + open, 
                                              data = stability_green,
                                              family = binomial(link = "probit"), 
                                              cluster="session")
stab_reg_blue_clustered <- glm.cluster(stable ~ period + I(period^2) + ttc * open, 
                                              data = stability_blue,
                                              cluster="session") 
stab_reg_green_clustered <- glm.cluster(stable ~ period + I(period^2) + ttc * open, 
                                       data = stability_green,
                                       cluster="session") 
# Make tables
stargazer(stability_ols, stability_int_ols,
          title = "Truthful reporting",
          label = "Tab:Truthful_reporting",
          omit = "subject.id", single.row = T)

##-------------------------------------------------------------
##               Wilcoxon Rank-Sum on payoff                 --
##-------------------------------------------------------------
# Subset data for ttc = 1 and 0
group1 <- subset(stability_green, ttc == 1)$stable
group2 <- subset(stability_green, ttc == 0)$stable
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

# OF and RF
group1 <- subset(stability_green, open == 1)$stable
group2 <- subset(stability_green, open == 0)$stable
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)


##-------------------------------------------------------------
##              stab: Joint Hypothesis testing               --
##-------------------------------------------------------------
# Likelihood ratio test 
# Fit the reduced model (without interaction terms)
stab_reg_reduced_open <- glm(stable ~ period + I(period^2) + open + ttc,
                           family = binomial(link = "probit"),
                           data = stability_green)
s_lrt_open <- lrtest(stab_reg_green, stab_reg_reduced_open)

stab_reg_reduced_ttc <- glm(stable ~ period + I(period^2) + ttc + open,
                          family = binomial(link = "probit"),
                          data = stability_green)
s_lrt_ttc <- lrtest(stab_reg_green, stab_reg_reduced_ttc)

# Print the test result
print(s_lrt_open)
print(s_lrt_ttc)
print(s_lrt_blue)

##-------------------------------------------------------------
##              stab: Joint Hypothesis testing               --
##-------------------------------------------------------------
stab_green <-  ggeffect(stab_reg_green, term = "open") # x = open, group = minority
stab_blue <-  ggeffect(stab_reg_blue, term = "open") 

##-------------------------------------------------------------
##           Table 8: Individual level stability reg         --
##-------------------------------------------------------------
# Individual level Regression
stab_reg_simple <- glm(envy ~ open + blue + minority + none_true, 
                              family = binomial(link = "probit"), 
                              data = long_df) 

stab_reg <- glm(envy ~ period + I(period^2) + open * minority * blue * none_true,
                       family = binomial(link = "probit"), 
                       data = long_df) 

stab_clustered <- glm.cluster(envy ~ open + blue + minority + none_true, 
                              data = long_df,
                              family = binomial(link = "probit"), 
                              cluster="session")

stab_int_clustered <- glm.cluster(envy ~ open * blue * minority * none_true, 
                                  data = long_df,
                                  family = binomial(link = "probit"), 
                                  cluster="session")
stargazer(stab_probit, stab_int_probit,
          title = "Truthful reporting",
          label = "Tab:Truthful_reporting",
          omit = "subject.id", single.row = T)


# Wilcoxon Rank-sum unmatched test to see if ttc and da are different non-parametrically
# Subset data for minority = 1 and 0
group1 <- subset(long_df, minority == 1)$envy
group2 <- subset(long_df, minority == 0)$envy
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

# Wilcoxon Rank-sum unmatched test to see if ttc and da are different non-parametrically
# Subset data for minority = 1 and 0
group1 <- subset(long_df, none_true == 1)$envy
group2 <- subset(long_df, none_true == 0)$envy
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

##-------------------------------------------------------------
##                         role by role                      --
##-------------------------------------------------------------
##-------------------------------------------------------------
##                       strategic behavior                  --
##-------------------------------------------------------------
t_role_green <- glm(true_all ~ period + I(period^2) + ttc * open * role, 
                  family = binomial(link = "probit"), 
                  data = long_df_green)

pred <- ggeffect(t_role_green, terms = c("role", "open"))

########### Figure: TT rates by student roles, open for Green ##################
tt_roles_green <- ggplot(pred, aes(x = x, y = predicted)) +
  geom_point(aes(shape = group), size = 3, position = position_dodge(0.3)) +
  scale_shape_manual(values = c(16, 8), labels = c("Reserved First", "Open First")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group),
                position = position_dodge(0.3), width = 0.2) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(y = "Rates of Truth-telling", x = "Student Roles") +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(labels = c("Role1", "Role2", "Role3", "Role4", "Role5", "Role6")) +
  guides(color = "none") +
  theme(panel.border = element_rect(color = "black", size = 0.7, fill = NA)) +
  theme(panel.spacing = unit(0.5, "lines")) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.5, linetype = "dotted")) + # Vertical grid lines
  theme(panel.grid.major.x = element_blank()) +  # Remove horizontal grid lines
  theme(panel.grid.minor = element_blank())  # Remove minor grid lines

################################################################################
#Blue
t_role_blue <- glm(true_all ~  period + I(period^2) + ttc * open * role,
                    family = binomial(link = "probit"), 
                    data = long_df_blue)
pred <- ggeffect(t_role_blue, terms = c("role", "open"))

########### Figure: TT rates by student roles, open for Blue ##################
tt_roles_blue <- ggplot(pred, aes(x = x, y = predicted)) +
  geom_point(aes(shape = group), size = 3, position = position_dodge(0.3)) +
  scale_shape_manual(values = c(16, 8), labels = c("Reserved First", "Open First")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group),
                position = position_dodge(0.3), width = 0.2) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(y = "Rates of Truth-telling", x = "Student Roles") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(labels = c("Role1", "Role2", "Role3", "Role4", "Role5", "Role6")) +
  guides(color = "none") +
  theme(panel.border = element_rect(color = "black", size = 0.7, fill = NA)) +
  theme(panel.spacing = unit(0.5, "lines")) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.5, linetype = "dotted")) + # Vertical grid lines
  theme(panel.grid.major.x = element_blank()) +  # Remove horizontal grid lines
  theme(panel.grid.minor = element_blank())  # Remove minor grid lines

################################################################################
###### test to see if the diff between OF and RF are statistically different 
group1 <- subset(long_df, role == 5 & open == 0 & blue == 0 )$true_all # RF Blue
group2 <- subset(long_df, role == 5 & open == 1 & blue == 0 )$true_all # OF Blue
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

################### Count of justified envies for each roles ###################

JE <- subset(stability_blue, stable == "0")
JE_sums <- colSums(JE[, c("envied", "envied_1", "envied_2", "envied_3", "envied_4", 
                          "envied_5", "envied_6")])
plot_data <- data.frame(variable = c("envied", "envied_1", "envied_2", "envied_3", 
                                     "envied_4", "envied_5", "envied_6"), sum = JE_sums)

a <- ggplot(plot_data, aes(x = variable, y = sum)) +
  geom_bar(stat = "identity", fill = "blue") +
  scale_x_discrete(labels=c('envied'= parse(text = TeX('$Total_JE$')),
                            'envied_1'= parse(text = TeX('$JE_{1}$')),
                            'envied_2'= parse(text = TeX('$JE_{2}$')),
                            'envied_3'= parse(text = TeX('$JE_{3}$')),
                            'envied_4'= parse(text = TeX('$JE_{4}$')),
                            'envied_5'= parse(text = TeX('$JE_{5}$')),
                            'envied_6'= parse(text = TeX('$JE_{6}$')))) +
  labs(x = "", y = "Number of Justified Envies")

##-------------------------------------------------------------
##                      individual efficiency               --
##-------------------------------------------------------------
# regression equation
eff_role_green <- lm(payoff ~ ttc * open * role, 
                  data = long_df_green)

# marginal effects of open, blue by roles
pred <- ggeffect(eff_role_green, terms = c("role", "open"))

########### Figure: Efficiency by student roles, open for Green #################
eff_roles_green <- ggplot(pred, aes(x = x, y = predicted)) +
  geom_point(aes(shape = group), size = 3, position = position_dodge(0.3)) +
  scale_shape_manual(values = c(16, 8), labels = c("Reserved First", "Open First")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group),
                position = position_dodge(0.3), width = 0.2) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(y = "Payoff", x = "Student Roles") +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(labels = c("Role1", "Role2", "Role3", "Role4", "Role5", "Role6")) +
  guides(color = "none") + 
  theme(panel.border = element_rect(color = "black", size = 0.7, fill = NA)) +
  theme(panel.spacing = unit(0.5, "lines")) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.5, linetype = "dotted")) + # Vertical grid lines
  theme(panel.grid.major.x = element_blank()) +  # Remove horizontal grid lines
  theme(panel.grid.minor = element_blank())  # Remove minor grid lines

################################################################################
eff_role_blue <- lm(payoff ~ ttc * open * role, 
                     data = long_df_blue)
pred <- ggeffect(eff_role_blue, terms = c("role", "open")) 

########### Figure: Efficiency by student roles, open for Blue #################
eff_roles_blue <- ggplot(pred, aes(x = x, y = predicted)) +
  geom_point(aes(shape = group), size = 3, position = position_dodge(0.3)) +
  scale_shape_manual(values = c(16, 8), labels = c("Reserved First", "Open First")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group),
                position = position_dodge(0.3), width = 0.2) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(y = "Payoff", x = "Student Roles") +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(labels = c("Role1", "Role2", "Role3", "Role4", "Role5", "Role6")) +
  guides(color = "none") + 
  theme(panel.border = element_rect(color = "black", size = 0.7, fill = NA)) +
  theme(panel.spacing = unit(0.5, "lines")) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.5, linetype = "dotted")) + # Vertical grid lines
  theme(panel.grid.major.x = element_blank()) +  # Remove horizontal grid lines
  theme(panel.grid.minor = element_blank())  # Remove minor grid lines


###### test to see if the diff between OF and RF are statistically different 
group1 <- subset(long_df, role == 6 & open == 0 & blue == 0)$payoff # RF Blue
group2 <- subset(long_df, role == 6 & open == 1 & blue == 0)$payoff # OF Blue
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

##-------------------------------------------------------------
##     Appendix: consequential/inconsequential deviations    --
##-------------------------------------------------------------
# The true preferences for student 1 is BCA. She reports CBA.  She is assigned to C.
# Inconsequential deviation if the counterfactual report of BCA would have resulted in her being assigned to C
# Consequential deviation if the counterfactual report of BCA would have resulted in her being assigned to B

ta_truealt <- long_df %>% 
  group_by(ttc, open, minority) %>%
  summarize(round(mean(true_alt),4))

# redo the strategic analysis with alternate tt definition, true_alt
ta_reg_simple_blue <- glm(true_alt ~ ttc + open + minority, 
                          family = binomial(link = "probit"), 
                          data = long_df_blue)

ta_reg_simple_green <- glm(true_alt ~ ttc + open + minority, 
                           family = binomial(link = "probit"), 
                           data = long_df_green)

ta_reg_blue <- glm(true_alt ~ period + I(period^2) + open * minority * ttc,
                   family = binomial(link = "probit"),
                   data = long_df_blue)

ta_reg_green <- glm(true_alt ~ period + I(period^2) + open * minority * ttc,
                    family = binomial(link = "probit"),
                    data = long_df_green)

# Add clustered standard errors
ta_reg_simple_blue_clustered <- glm.cluster(true_alt ~ ttc + open + minority,  
                                            family = binomial(link = "probit"), 
                                            cluster="session",
                                            data = long_df_blue)

ta_reg_simple_green_clustered <- glm.cluster(true_alt ~ ttc + open + minority,  
                                             family = binomial(link = "probit"), 
                                             cluster="session",
                                             data = long_df_green)

ta_reg_blue_clustered <- glm.cluster(true_alt ~ period + I(period^2) + open * minority * ttc,
                                     family = binomial(link = "probit"), 
                                     cluster="session",
                                     data = long_df_blue)

ta_reg_green_clustered <- glm.cluster(true_alt ~ period + I(period^2) + open * minority * ttc,
                                      family = binomial(link = "probit"), 
                                      cluster="session",
                                      data = long_df_green)
##-------------------------------------------------------------
##               ta by open, minority under blue             --
##-------------------------------------------------------------
ta_reg_blue <- glm(true_alt ~ period + I(period^2) + open * minority * ttc,
                   family = binomial(link = "probit"),
                   data = long_df_blue)

ta_blue <-  ggeffect(ta_reg_blue, terms = c("open", "minority")) # x = open, group = minority

ta_blue_plot <- ggplot(ta_blue, aes(x = x, y = predicted)) +
  geom_point(aes(shape = group), size = 3, position = position_dodge(0.2)) +
  scale_shape_manual(values = c(17, 0), labels = c("Majority", "Minority")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group), 
                position = position_dodge(0.2), width = 0.2) +
  scale_color_grey() + 
  theme_classic() +
  labs(y = "Rates of Truth-telling", x = "Precedence Order") +
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 17),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(breaks = c(0,1), label = c("Reserved \n First", "Open \n First")) +
  guides(color = "none") +
  theme(panel.border = element_rect(color = "black", size = 0.7, fill = NA)) +
  theme(panel.spacing = unit(0.5, "lines")) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.5, linetype = "dotted")) + # Vertical grid lines
  theme(panel.grid.major.x = element_blank()) +  # Remove horizontal grid lines
  theme(panel.grid.minor = element_blank())  # Remove minor grid lines

ta_blue_plot
##-------------------------------------------------------------
##               ta by open, minority under green            --
##-------------------------------------------------------------
ta_reg_green <- glm(true_alt ~ period + I(period^2) + open * minority * ttc,
                    family = binomial(link = "probit"),
                    data = long_df_green)

ta_green <-  ggeffect(ta_reg_green, terms = c("open", "minority")) # x = open, group = minority

ta_green_plot <- ggplot(ta_green, aes(x = x, y = predicted)) +
  geom_point(aes(shape = group), size = 4, position = position_dodge(0.2)) +
  scale_shape_manual(values = c(17, 0), labels = c("Majority", "Minority")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group), 
                position = position_dodge(0.2), width = 0.2) +
  scale_color_grey() + 
  theme_classic() +
  labs(y = "Rates of Truth-telling", x = "Precedence Order") +
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 17),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(breaks = c(0,1), label = c("Reserved \n First", "Open \n First")) +
  guides(color = "none") + 
  theme(panel.border = element_rect(color = "black", size = 0.7, fill = NA)) +
  theme(panel.spacing = unit(0.5, "lines")) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.5, linetype = "dotted")) + # Vertical grid lines
  theme(panel.grid.major.x = element_blank()) +  # Remove horizontal grid lines
  theme(panel.grid.minor = element_blank())  # Remove minor grid lines

ta_green_plot

##-------------------------------------------------------------
##                         role by role                      --
##-------------------------------------------------------------
##-------------------------------------------------------------
##                       strategic behavior                  --
##-------------------------------------------------------------
# Green
ta_role_green <- glm(true_alt ~ period + I(period^2) + ttc * open * role, 
                    family = binomial(link = "probit"), 
                    data = long_df_green)

pred <- ggeffect(ta_role_green, terms = c("role", "open"))

# replace low CI with 1 for estimates very close to 1
pred$conf.low <- replace(pred$conf.low, pred$conf.low == pred$conf.low[2], 1)
pred$conf.low <- replace(pred$conf.low, pred$conf.low == pred$conf.low[4], 1)
pred$conf.low <- replace(pred$conf.low, pred$conf.low == pred$conf.low[6], 1)
pred$conf.low <- replace(pred$conf.low, pred$conf.low == pred$conf.low[8], 1)
pred$conf.low <- replace(pred$conf.low, pred$conf.low == pred$conf.low[10], 1)
pred$conf.low <- replace(pred$conf.low, pred$conf.low == pred$conf.low[12], 1)

########### Figure: TT rates by student roles, open for Green ##################
ta_roles_green <- ggplot(pred, aes(x = x, y = predicted)) +
  geom_point(aes(shape = group), size = 3, position = position_dodge(0.3)) +
  scale_shape_manual(values = c(16, 8), labels = c("Reserved First", "Open First")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group),
        position = position_dodge(0.3), width = 0.2) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(y = "Rates of Truth-telling", x = "Student Roles") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(labels = c("Role1", "Role2", "Role3", "Role4", "Role5", "Role6")) +
  guides(color = "none") +
  theme(panel.border = element_rect(color = "black", size = 0.7, fill = NA)) +
  theme(panel.spacing = unit(0.5, "lines")) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.5, linetype = "dotted")) + # Vertical grid lines
  theme(panel.grid.major.x = element_blank()) +  # Remove horizontal grid lines
  theme(panel.grid.minor = element_blank())  # Remove minor grid lines

################################################################################
#Blue
ta_role_blue <- glm(true_alt ~  period + I(period^2) + ttc * open * role, 
                   family = binomial(link = "probit"), 
                   data = long_df_blue)
pred <- ggeffect(ta_role_blue, terms = c("role", "open"))

pred$conf.low <- replace(pred$conf.low, pred$conf.low == pred$conf.low[2], 1)
pred$conf.low <- replace(pred$conf.low, pred$conf.low == pred$conf.low[4], 1)
pred$conf.low <- replace(pred$conf.low, pred$conf.low == pred$conf.low[10], 1)
pred$conf.low <- replace(pred$conf.low, pred$conf.low == pred$conf.low[11], 1)

########### Figure: TT rates by student roles, open for Blue ##################
ta_roles_blue <- ggplot(pred, aes(x = x, y = predicted)) +
  geom_point(aes(shape = group), size = 3, position = position_dodge(0.3)) +
  scale_shape_manual(values = c(16, 8), labels = c("Reserved First", "Open First")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group),
                position = position_dodge(0.3), width = 0.2) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(y = "Rates of Truth-telling", x = "Student Roles") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(labels = c("Role1", "Role2", "Role3", "Role4", "Role5", "Role6")) +
  guides(color = "none") +
  theme(panel.border = element_rect(color = "black", size = 0.7, fill = NA)) +
  theme(panel.spacing = unit(0.5, "lines")) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.5, linetype = "dotted")) + # Vertical grid lines
  theme(panel.grid.major.x = element_blank()) +  # Remove horizontal grid lines
  theme(panel.grid.minor = element_blank())  # Remove minor grid lines

################################################################################
# Robustness checks:
# Does student who pass the quiz do better?
pass <- long_df %>%
  filter(q_pass == 1)
nopass <- long_df %>%
  filter(q_pass == 0)

t_test_quiz <- t.test(pass$true_all, nopass$true_all, alternative = "greater")
print(t_test_quiz)

# White vs nonwhite students?
white <- long_df %>%
  filter(white == 1)
nonwhite <- long_df %>%
  filter(white == 0)

t_test_quiz <- t.test(white$true_all, nonwhite$true_all, alternative = "greater")
print(t_test_quiz)

# Female vs male?
female <- long_df %>%
  filter(female == 1)
male <- long_df %>%
  filter(female == 1)

t_test_quiz <- t.test(female$true_all, male$true_all, alternative = "greater")
print(t_test_quiz)

# comparing first period vs last period?
first <- long_df %>%
  filter(period == 1)
last <- long_df %>%
  filter(period == 20)

t_test_quiz <- t.test(first$true_all, last$true_all, alternative = "greater")
print(t_test_quiz)

# True_all by period
tt_le <- long_df %>%
  group_by(period) %>%
  summarize(round(mean(true_all),4))

######################## TT rate by open, min, ttc ############################
tt_reg <- glm(true_all ~ period + I(period^2) + open * minority * ttc * blue,
                   family = binomial(link = "probit"),
                   data = long_df)

blue_labs <- c("0" = "DA", "1" = "TTC")
pred <- ggeffect(tt_reg, terms = c("open", "minority", "ttc")) # x = open, group = minority, facet = blue

tt_plot <- ggplot(pred, aes(x = x, y = predicted)) +
  geom_point(aes(shape = group), size = 3, position = position_dodge(0.2)) +
  scale_shape_manual(values = c(17, 0), labels = c("Majority", "Minority")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group), 
                position = position_dodge(0.2), width = 0.2) +
  scale_color_grey() + 
  theme_classic() +
  facet_wrap(~ facet, labeller = labeller(facet = blue_labs)) +
  labs(y = "Rates of Truth-telling", x = "Precedence Order") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(breaks = c(0,1), label = c("Reserved \n First", "Open \n First")) +
  guides(color = "none") +
  theme(panel.border = element_rect(color = "black", size = 0.7, fill = NA)) +
  theme(panel.spacing = unit(0.5, "lines")) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.5, linetype = "dotted")) + # Vertical grid lines
  theme(panel.grid.major.x = element_blank()) +  # Remove horizontal grid lines
  theme(panel.grid.minor = element_blank())  # Remove minor grid lines

tt_plot

######################## TT rate by open, min, ttc ############################
ttc_labs <- c("0" = "Deferred Acceptance", "1" = "Top Trading Cycles")

tt_reg <- glm(true_all ~ period + I(period^2) + open * minority * ttc * blue,
                   family = binomial(link = "probit"),
                   data = long_df)

pred <- ggeffect(tt_reg, terms = c("open", "minority", "ttc")) # x = open, group = minority, facet = blue

tt_plot <- ggplot(pred, aes(x = x, y = predicted)) +
  geom_point(aes(shape = group), size = 3, position = position_dodge(0.2)) +
  scale_shape_manual(values = c(17, 0), labels = c("Majority", "Minority")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group), 
                position = position_dodge(0.2), width = 0.2) +
  scale_color_grey() + 
  theme_classic() +
  facet_wrap(~ facet, labeller = labeller(facet = ttc_labs)) +
  labs(y = "Proportion of Truth-telling Rates", x = "Precedence Order") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(breaks = c(0,1), label = c("Reserved \n First", "Open \n First")) +
  guides(color = "none") +
  theme(panel.border = element_rect(color = "black", size = 0.7, fill = NA)) +
  theme(panel.spacing = unit(0.5, "lines")) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.5, linetype = "dotted")) + # Vertical grid lines
  theme(panel.grid.major.x = element_blank()) +  # Remove horizontal grid lines
  theme(panel.grid.minor = element_blank())  # Remove minor grid lines

tt_plot

########################### Eff by open, min, ttc #############################
eff_reg <- lm(payoff ~  period + I(period^2) + open * minority * ttc * blue, data = long_df)
pred <- ggeffect(eff_reg, terms = c("open", "minority", "ttc")) # group = minority, facet = ttc

payoff_plot <- ggplot(pred, aes(x = x, y = predicted)) +
  geom_point(aes(shape = group), size = 3) +
  scale_shape_manual(values = c(17, 0), labels = c("Majority", "Minority")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme_classic() +
  facet_wrap(~ facet, labeller = labeller(facet = ttc_labs)) +
  labs(y = "Payoff", x = "Precedence Order") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(breaks = c(0,1), label = c("Reserved \n First", "Open \n First")) +
  guides(color = "none") +
  theme(panel.border = element_rect(color = "black", size = 0.7, fill = NA)) +
  theme(panel.spacing = unit(0.5, "lines")) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.5, linetype = "dotted")) + # Vertical grid lines
  theme(panel.grid.major.x = element_blank()) +  # Remove horizontal grid lines
  theme(panel.grid.minor = element_blank())  # Remove minor grid lines

payoff_plot


##-------------------------------------------------------------
##                      Appendix                       --
##-------------------------------------------------------------
##-------------------------------------------------------------
##            Robustnesscheck:Efficiency regression          --
##-------------------------------------------------------------
# Run regressions
oeff_reg_green <- clm(assigned_rank ~ period + I(period^2) + open * minority * ttc, 
                          data = long_df_green,
                          cluster = "session")

oeff_reg_blue <- clm(assigned_rank ~ period + I(period^2) + open * minority * ttc, 
                      data = long_df_blue,
                      cluster = "session")

oeff_reg_simple_blue <- clm(assigned_rank ~ ttc + open + minority, 
                          data = long_df_blue,
                          cluster = "session")

oeff_reg_simple_green <- clm(assigned_rank ~ ttc + open + minority, 
                           data = long_df_green,
                           cluster = "session")
##-------------------------------------------------------------
##             payoff by open, minority under blue           --
##-------------------------------------------------------------
oeff_blue <- ggeffect(oeff_reg_blue, terms = c("open", "minority")) # x = open, group = minority

##-------------------------------------------------------------
##             payoff by open, minority under green          --
##-------------------------------------------------------------
oeff_green <- ggeffect(oeff_reg_green, terms = c("open", "minority")) # x = open, group = minority

###############################################################