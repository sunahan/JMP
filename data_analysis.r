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
#################################################################
##---------------------------------------------------------------
##                      Load data                              --
##---------------------------------------------------------------
ind_long_df <- read_xlsx("C://Users//kccsu//Desktop//SchoolChoice//Data//ind_long_df.xlsx")
long_df <- ind_long_df %>%
  mutate(ttc = as.factor(ttc),
         minority = as.factor(minority),
         open = as.factor(open),
         blue = as.factor(blue),
         true_1_2 = as.numeric(ifelse((true_1 == "1" & true_2 == "1"), "1", "0")),
         none_true = as.numeric(ifelse((true_1== "0" & true_2 == "0" & true_3 == "0"), "1", "0")))

##-------------------------------------------------------------
##                     truth-telling rates                   --
##-------------------------------------------------------------
##-------------------------------------------------------------
##                         proportions                       --
##-------------------------------------------------------------
# Truth-telling rates by session
truth_telling_1 <- long_df %>% 
  group_by(period) %>%
  summarize(tt_rate_1 = round(mean(true_1),2))

truth_telling_2 <- long_df %>% 
  group_by(session) %>%
  summarize(tt_rate_2 = round(mean(true_2),2))

truth_telling_3 <- long_df %>% 
  group_by(session) %>%
  summarize(tt_rate_3 = round(mean(true_3),2))

truth_telling_all <- long_df %>% 
  group_by(period) %>%
  summarize(tt_rate_all = round(mean(true_all),4))

# Table 3: Truth-telling rates by mechanism
tt_1 <- long_df %>% 
  group_by(ttc) %>%
  summarize(tt_rate_1 = round(mean(true_1),4))

tt_1_2 <- long_df %>% 
  group_by(ttc) %>%
  summarize(tt_rate_1_2 = round(mean(true_1_2),4))

tt_all <- long_df %>% 
  #group_by(ttc) %>%
  summarize(tt_rate_3 = round(mean(true_all),4))

tt_none <- long_df %>% 
  #group_by(ttc) %>%
  summarize(tt_rate_none = round(mean(none_true),4))


# Table4: Truth-telling rates by precedence order and types of students
truth_telling_1 <- long_df %>% 
  group_by(open, minority) %>%
  summarize(tt_rate_1 = round(mean(true_1),2))

truth_telling_2 <- long_df %>% 
  group_by(open, minority) %>%
  summarize(tt_rate_2 = round(mean(true_2),2))

truth_telling_3 <- long_df %>% 
  group_by(open, minority) %>%
  summarize(tt_rate_3 = round(mean(true_3),2))

# Truth-telling rates by precedence order and types of students by mechanism
truth_telling_1 <- long_df %>% 
  group_by(open, minority, ttc) %>%
  summarize(tt_rate_1 = round(mean(true_1),2))

truth_telling_2 <- long_df %>% 
  group_by(open, minority, ttc) %>%
  summarize(tt_rate_2 = round(mean(true_2),2))

truth_telling_3 <- long_df %>% 
  group_by(open, minority, ttc) %>%
  summarize(tt_rate_3 = round(mean(true_3),2))

truth_telling_all <- long_df %>% 
  group_by(open, ttc, minority) %>%
  summarize(tt_rate_all = round(mean(true_all),4))


##-------------------------------------------------------------
##                     truth-telling rates                   --
##-------------------------------------------------------------
##-------------------------------------------------------------
##                     probit regression                     --
##-------------------------------------------------------------
t_rate_1_probit <- glm(true_1 ~ minority + ttc + period + open + blue, 
                       family = binomial(link = "probit"), 
                       data = long_df)
t_rate_2_probit <- glm(true_2 ~ minority + ttc + period + open + blue, 
                       family = binomial(link = "probit"), 
                       data = long_df)
t_rate_3_probit <- glm(true_3 ~ minority + ttc + period + open + blue, 
                       family = binomial(link = "probit"), 
                       data = long_df)

stargazer(t_rate_1_probit, t_rate_2_probit, t_rate_3_probit,
          title = "Truthful reporting",
          column.labels = c("first choice", "second choice", "third choice"),
          label = "Tab:Truthful_reporting",
          omit = "subject.id", single.row = T)

# Cluster standard errors at the session level
t_rate_1_probit_clustered <- glm.cluster(true_1 ~ minority + ttc + period + open + blue, 
                                         family = binomial(link = "probit"), 
                                         cluster="session",
                                         data = long_df)
# Marginal effects
mean <- model.frame(t_rate_all_probit) %>%
  map_df(mean)

# Marginal effect at the mean
# Calculated at the mean values
t_rate_all_probit_at_mean <- margins(t_rate_all_probit, at = mean[-1])
summary(t_rate_all_probit_at_mean)

############################################ Table 5 ############################################ 
# Average marginal effect
# ME is calculated for each observations and averaged across all observations
# Run regressions
t_all_probit <- glm(true_all ~ ttc + open + blue + minority, 
                    family = binomial(link = "probit"), 
                    data = long_df)

t_all_int_probit <- glm(true_all ~ ttc * open * blue * minority + period, 
                        family = binomial(link = "probit"), 
                        data = long_df)

# Calculate AME
t_all_probit_ame <- margins(t_all_probit)
t_all_int_probit_ame <- margins(t_all_int_probit)
summary(t_all_probit_ame)
summary(t_all_int_probit_ame)


# Add clustered standard errors
t_all_probit_clustered <- glm.cluster(true_all ~ ttc + open + blue + minority, 
                                      family = binomial(link = "probit"), 
                                      cluster="session",
                                      data = long_df)

t_all_int_probit_clustered <- glm.cluster(true_all ~ ttc * open * blue * minority + period,
                                          family = binomial(link = "probit"), 
                                          cluster="session",
                                          data = long_df)

# Put them into tables
stargazer(t_all_probit_ame, t_all_int_probit_ame,
          title = "Truthful reporting",
          column.labels = c("first choice", "second choice"),
          label = "Tab:Truthful_reporting",
          omit = "subject.id", single.row = T)

############################################ Figure 1 ############################################ 
blue_labs <- c("0" = "Green priorities", "1" = "Blue priorities")
tt_impact_int <- glm(true_all ~ open * minority * blue, data = long_df)
pred <- ggpredict(tt_impact_int, terms = c("open", "minority", "blue")) # x = open, group = minority, facet = blue

tt_plot <- ggplot(pred, aes(x = x, y = predicted)) +
  geom_point(aes(shape = group), size = 3, position = position_dodge(0.2)) +
  scale_shape_manual(values = c(17, 0), labels = c("Majority", "Minority")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group), 
                position = position_dodge(0.2), width = 0.2) +
  scale_color_grey() + 
  theme_classic() +
  facet_wrap(~ facet, labeller = labeller(facet = blue_labs)) +
  labs(y = "Proportion of Truth-telling Rates", x = "Precedence Order") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(breaks = c(0,1), label = c("Reserved \n First", "Open \n First")) +
  guides(color = "none")


######################################## Aggregate priorities ###################################
tt_impact_int <- glm(true_all ~ period + I(period^2) + open * minority * blue * ttc , data = long_df)
pred <- ggpredict(tt_impact_int, terms = c("open", "minority")) # group = minority

tt_plot <- ggplot(pred, aes(x = x, y = predicted)) +
  geom_point(aes(shape = group), size = 3, position = position_dodge(0.2)) +
  scale_shape_manual(values = c(17, 0), labels = c("Majority", "Minority")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group), 
                position = position_dodge(0.2), width = 0.2) +
  scale_color_grey() + 
  theme_classic() +
  #facet_wrap(~ facet, labeller = labeller(facet = blue_labs)) +
  labs(y = "Proportion of Truth-telling Rates", x = "Precedence Order") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(breaks = c(0,1), label = c("Reserved \n First", "Open \n First")) +
  guides(color = "none")




# Wilcoxon Rank-sum unmatched test to see if ttc and da are different non-parametrically
# Subset data for ttc = 1 and 0
group1 <- subset(long_df, ttc == 1)$true_all
group2 <- subset(long_dft, ttc == 0)$true_all
result <- wilcox.test(group1, group2, correct = FALSE)
result <- t.test(group1, group2) # student's t-test
print(result)

# OF and RF
group1 <- subset(long_df, open == 1)$true_all
group2 <- subset(long_df, open == 0)$true_all
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

# Blue and Green
group1 <- subset(long_df, blue == 1)$true_all
group2 <- subset(long_df, blue == 0)$true_all
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

# Minority and majority
group1 <- subset(long_df, minority == 1)$true_all
group2 <- subset(long_df, minority == 0)$true_all
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

# Minority and majority
group1 <- subset(long_df, blue == 1 & open == 0 & minority == 1)$true_all
group2 <- subset(long_df, blue == 1 & open == 0 & minority == 0)$true_all
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

##-------------------------------------------------------------
##                     truth-telling rates                   --
##-------------------------------------------------------------
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
strategy_data$ttc <- factor(strategy_data$ttc, levels = c(0,1))
ttc_labs <- c("0" = "Deferred Acceptance", "1" = "Top Trading Cycles")

strategy_hist <- ggplot(strategy_data, aes(x = strategy, fill = role_124, group = role_124)) + 
  geom_bar(aes(y = ..count../sum(..count..)), width = 0.5, alpha = 1, position = position_dodge()) +
  facet_wrap(~ttc, labeller = labeller(ttc = ttc_labs)) +
  labs(x = "Possible strategies", y = "") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = c("0"="gray78","1"="gray39"), labels = c("Student 3,5,6", "Student 1,2,4")) +
  theme_classic() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.position = "top")

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
##                     Comprehension check                   --
##-------------------------------------------------------------
##-------------------------------------------------------------
##                      Quiz distribution                    --
##-------------------------------------------------------------
# q_payoff is the number of correct quizzes on the first try
# Figure: Quiz payoff (quiz_distribution 1100*700)
quiz_bar <- ggplot(long_df, aes(x = q_payoff)) +
  theme_classic() +
  geom_bar(aes(y = ..count../sum(..count..)), width = 0.5, alpha = 0.5, position="identity", fill = "#666666") +
  scale_y_continuous(name = "Percentage", labels = scales::percent_format()) +
  xlab("Number of Correct Questions on the First Attempt") +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20))

quiz_bar

# Average earnings
avg_quiz <- round(mean(long_df$q_payoff * 0.25), 2) #quiz
avg_earnings <- round(mean(long_df$q_payoff*0.25 + long_df$payoff), 2) #quiz + experiment
earnings_range <- long_df$q_payoff*0.25 + long_df$payoff + 7.5
summary(earnings_range)

avg_payoff <- round(mean(long_df$payoff), 2) #experiment
summary(avg_payoff)

# Summary stats for number of correct quizzes
quiz <- long_df %>% select(q_payoff) %>% summary()

##-------------------------------------------------------------
##                          Efficiency                       --
##-------------------------------------------------------------
##-------------------------------------------------------------
##                     payoff distribution                   --
##-------------------------------------------------------------

# Figure: Average Payoff by Student Roles (Average_payoff_by_roles 1100*700)
payoff_mode <- long_df %>% dplyr::select(session, subject.id, role, payoff) %>%
  group_by(role) %>%
  mutate(avg_payoff = mean(payoff))


payoff_bar <- ggplot(payoff_mode, aes(x = factor(role), y= avg_payoff)) +
  theme_classic() +
  geom_bar(stat="identity", width = 0.5, alpha = 0.5, position="identity", fill = "#666666") +
  scale_y_continuous(name="Average Payoff") +
  scale_x_discrete(name = "Student Roles", 
                   labels=c("1" = "Student 1", "2" = "Student 2", "3" = "Student 3", 
                            "4" = "Student 4", "5" = "Student 5", "6" = "Student 6")) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20))

payoff_bar
##-------------------------------------------------------------
##                          Efficiency                       --
##-------------------------------------------------------------
##-------------------------------------------------------------
##                      payoff regressions                   --
##-------------------------------------------------------------
# Run regressions
payoff_ols <- lm(payoff ~ ttc + open + blue + minority, data = long_df) 
payoff_plm <- plm(payoff ~ ttc + minority + open, 
                  data = long_df, index = c("subject.id"), model = "within")
payoff_int_ols <- lm(payoff ~ ttc * open * blue * minority + period, 
                              data = long_df)
payoff_int_plm <- plm(payoff ~ ttc * open * blue * minority + period,  
                               data = long_df, index = c("subject.id"), model = "within")

# With learning effect
payoff_int_learning_ols <- lm(payoff ~ ttc * open * blue * minority + period, 
                              data = long_df %>% filter(period > 10))
payoff_int_learning_plm <- plm(payoff ~ ttc * open * blue * minority + period, 
                               data = long_df %>% filter(period > 10), index = c("subject.id"), model = "within")

# Make Table 6
stargazer(payoff_ols,
          payoff_int_ols,
          title = "payoff_impact",
          column.labels = c("standard", "w/out learning"),
          label = "payoff_impact",
          omit = "subject.id", single.row = T)

# Clustered standard errors at session level
payoff_ols_clustered <- lm.cluster(payoff ~ ttc + open + blue + minority, data = long_df,
                                   cluster="session")

payoff_int_ols_clustered <- lm.cluster(payoff ~ ttc * open * blue * minority + period, 
                                       data = long_df,
                                       cluster="session")

payoff_int_learning_ols_clustered <- lm.cluster(payoff ~ open * minority * ttc + period, 
                                                data = long_df %>% filter(period > 10),
                                                cluster="session")

# Wilcoxon Rank-sum unmatched test to see if ttc and da are different non-parametrically
# Subset data for ttc = 1 and 0
group1 <- subset(long_df, ttc == 1)$payoff
group2 <- subset(long_df, ttc == 0)$payoff
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

# OF and RF
group1 <- subset(long_df, open == 1)$payoff
group2 <- subset(long_df, open == 0)$payoff
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

# Blue and Green
group1 <- subset(long_df, blue == 1)$payoff
group2 <- subset(long_df, blue == 0)$payoff
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

# Minority and majority
group1 <- subset(long_df, minority == 1)$payoff
group2 <- subset(long_df, minority == 0)$payoff
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

##-------------------------------------------------------------
##                          Efficiency                       --
##-------------------------------------------------------------
##-------------------------------------------------------------
##                payoff regressions figures                 --
##-------------------------------------------------------------

##### Figure - payoff by types, mechanism, precedence order (unused bc of smooth line)
payoff_plot <- ggplot(long_df, aes(x = open, y = payoff, color = minority)) +
  scale_color_manual(values = c("steelblue", "green"), labels = c("majority", "minority")) +
  geom_smooth(method = "lm", formula = y ~ x, linewidth = 2) +
  facet_wrap(~ttc, labeller = labeller(ttc = ttc_labs)) +
  labs(y = "Payoff by types of students", x = "") +
  theme_classic() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_continuous(breaks = c(0,1), label = c("Reserved \n First", "Open \n First"))

##### Figure - payoff by types, mechanism, precedence order
ttc_labs <- c("0" = "Deferred Acceptance", "1" = "Top Trading Cycles")
payoff_impact_int <- lm(payoff ~ open * ttc * minority, data = long_df)
pred <- ggpredict(payoff_impact_int, terms = c("open", "minority", "ttc")) # group = minority, facet = ttc


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
  scale_x_discrete(breaks = c(0,1), label = c("Reserved \n First", "Open \n First")) 

############################ Figure 3 - payoff by types, precedence order, priority order
blue_labs <- c("0" = "Green priorities", "1" = "Blue priorities")
payoff_impact_int <- lm(payoff ~ open * minority * blue , data = long_df)
pred <- ggpredict(payoff_impact_int, terms = c("open", "minority", "blue")) # group = minority, facet = ttc

payoff_plot <- ggplot(pred, aes(x = x, y = predicted)) +
  geom_point(aes(shape = group), size = 3, position = position_dodge(0.2)) +
  scale_shape_manual(values = c(17, 0), labels = c("Majority", "Minority")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = group), 
                 position = position_dodge(0.2), width = 0.2) +
  scale_color_grey() +
  theme_classic() +
  facet_wrap(~ facet, labeller = labeller(facet = blue_labs)) +
  labs(y = "Payoff", x = "Precedence Order") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(breaks = c(0,1), label = c("Reserved \n First", "Open \n First")) +
  guides(color = "none")


payoff_plot

##-------------------------------------------------------------
##                          Stability                        --
##-------------------------------------------------------------
##-------------------------------------------------------------
##                  group stability regression               --
##-------------------------------------------------------------
g_wide_df <- read_xlsx("C://Users//kccsu//Desktop//SchoolChoice//Data//cleaned//group_level_dt.xlsx")

stability_data <- g_wide_df %>% 
  mutate(ttc = as.factor(ttc),
         open = as.factor(open),
         blue = as.factor(blue))

############################ Figure 4 - Bar graph of number of justified envies per mechanism
percentage_data <- g_wide_df %>%
  group_by(ttc) %>%
  summarize(percentage = mean(stable == 1, na.rm = TRUE) * 100)

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

###### test to see if they are statistically different
group1 <- subset(stability_data, ttc == 1)$stable # RF Blue
group2 <- subset(stability_data, ttc == 0)$stable # OF Bluer
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)


totals_envy <- aggregate(envied ~ ttc, data = stability_data, FUN = sum)

envy_plot <- ggplot(totals_envy, aes(x = factor(ttc), y = envied, fill = factor(ttc))) +
  theme_classic() +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))), width = 0.3, alpha = 0.3) +
  scale_fill_manual(values = c("#999999","#000000")) +
  labs(x = "", y = "") + 
  scale_x_discrete(labels = c("0" = "DA", "1" = "TTC")) +
  scale_y_continuous(limits = c(0, 1)) + #expand = c(0, 0)) +
  theme(axis.text=element_text(size=15), legend.position = "none") 

combined <- ggarrange(stability_plot, envy_plot,  
          labels = c("Number of Stable Assignment Outcomes", "Instances of Justified Envy"),
          ncol = 2, nrow = 1)



# Group level Regression
stability_probit <- glm(stable ~ ttc + open + blue, 
                     family = binomial(link = "probit"), 
                     data = stability_data) 

stability_int_probit <- glm(stable ~ ttc * open * blue, 
                         family = binomial(link = "probit"), 
                         data = stability_data) 

stability_probit_clustered <- glm.cluster(stable ~ ttc + open + blue, 
                                      data = stability_data,
                                      family = binomial(link = "probit"), 
                                      cluster="session")
stability_int_probit_clustered <- glm.cluster(stable ~ ttc * open * blue, 
                                          data = stability_data,
                                          cluster="session") 

stargazer(stability_ols, stability_int_ols,
          title = "Truthful reporting",
          label = "Tab:Truthful_reporting",
          omit = "subject.id", single.row = T)

# Number of envies by mechanism
sum(g_wide_df[which(g_wide_df$ttc == "1"), 48])
sum(g_wide_df[which(g_wide_df$ttc == "0"), 48])

# Number of stable outcomes by mechanism
sum(g_wide_df[which(g_wide_df$ttc == "1"), 49])
sum(g_wide_df[which(g_wide_df$ttc == "0"), 49])

# Number of instances of justified envy in Green RF, but in eqm it should be 5*3*8 = 120
sum(g_wide_df[which(g_wide_df$blue == "0" & g_wide_df$open == "0" ), 42])

# Wilcoxon Rank-sum unmatched test to see if ttc and da are different non-parametrically
# Subset data for ttc = 1 and 0
group1 <- subset(stability_data, ttc == 1)$stable
group2 <- subset(stability_data, ttc == 0)$stable
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

# Subset data for open = 1 and 0
group1 <- subset(stability_data, open == 1)$stable
group2 <- subset(stability_data, open == 0)$stable
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

# Subset data for blue = 1 and 0
group1 <- subset(stability_data, blue == 1)$stable
group2 <- subset(stability_data, blue == 0)$stable
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

############################ Table 9 - Rates of stability across precedence order and priority order
## rates are same under following scenarios

# terms = c("open", "blue") OF Blue & RF Green
# terms = c("ttc", "blue") DA Blue & TTC Blue
# terms = c("ttc", "blue", "open")
stability_int_probit <- glm(stable ~ ttc * open * blue + period + I(period^2), 
                            family = binomial(link = "probit"), 
                            data = stability_data)


pred <- ggpredict(stability_int_probit, terms = c("blue", "ttc")) # group = blue

pred$x <- as.numeric(pred$x)
pred$group <- as.numeric(pred$group)
pred <- round(pred, 4)

JE <- subset(stability_data, stable == "0")
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
##                          Stability                        --
##-------------------------------------------------------------
##-------------------------------------------------------------
##              individual stability regression              --
##-------------------------------------------------------------
# Individual level Regression
stab_probit <- glm(envy ~ open + blue + minority + none_true, 
                        family = binomial(link = "probit"), 
                        data = long_df) 

stab_int_probit <- glm(envy ~ open * blue * minority * none_true, 
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
##                       role by role                        --
##-------------------------------------------------------------
##-------------------------------------------------------------
##              individual strategic behavior                --
##-------------------------------------------------------------
long_df <- long_df %>%
  mutate(role2 = as.factor(as.numeric(ifelse(role == 2, "1", "0"))),
         role3 = as.factor(as.numeric(ifelse(role == 3, "1", "0"))),
         role4 = as.factor(as.numeric(ifelse(role == 4, "1", "0"))),
         role5 = as.factor(as.numeric(ifelse(role == 5, "1", "0"))),
         role6 = as.factor(as.numeric(ifelse(role == 6, "1", "0"))))

# regression equation

t_all_role <- glm(true_all ~ ttc * open * blue * role2 + ttc * open * blue * role3 + 
                    ttc * open * blue * role4 + ttc * open * blue * role5 + 
                    ttc * open * blue * role6 + period + I(period^2), 
                  family = binomial(link = "probit"), 
                  data = long_df)

# marginal effects of open, blue by roles
pred1 <- ggpredict(t_all_role, terms = c("role2", "open", "blue" )) %>% filter(x == 0)
pred2 <- ggpredict(t_all_role, terms = c("role2", "open", "blue" )) %>% filter(x == 1) 
pred3 <- ggpredict(t_all_role, terms = c("role3", "open", "blue" )) %>% filter(x == 1) 
pred4 <- ggpredict(t_all_role, terms = c("role4", "open", "blue" )) %>% filter(x == 1)
pred5 <- ggpredict(t_all_role, terms = c("role5", "open", "blue" )) %>% filter(x == 1) 
pred6 <- ggpredict(t_all_role, terms = c("role6", "open", "blue" )) %>% filter(x == 1) 
values <- rep(paste0("role", seq(length.out = 6, by = 1)), each = 4)

pred <- rbind(pred1, pred2, pred3, pred4, pred5, pred6)

############################ Figure: Proportion of truth-telling rates by student roles, open for Blue
pred_blue <- pred %>% 
  mutate(role = values, 
         open = group,
         blue = facet) %>% 
  select(c(8:10,2:5)) %>%
  filter(blue == 1)

tt_roles_blue <- ggplot(pred_blue, aes(x = role, y = predicted)) +
  geom_point(aes(shape = open), size = 3, position = position_dodge(0.3)) +
  scale_shape_manual(values = c(16, 8), labels = c("Reserved First", "Open First")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = open),
                position = position_dodge(0.3), width = 0.2) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(y = "Proportion of Truth-telling Rates", x = "Student Roles", title = "Blue Priority") +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(labels = c("Role1", "Role2", "Role3", "Role4", "Role5", "Role6")) +
  guides(color = "none")

###### test to see if the diff between OF and RF are statistically different 
group1 <- subset(long_df, role == 1 & open == 0 & blue == 0 )$true_all # RF Blue
group2 <- subset(long_df, role == 1 & open == 1 & blue == 0 )$true_all # OF Blue
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

############################ Figure: Proportion of truth-telling rates by student roles, open for Green
pred_green <- pred %>% 
  mutate(role = values, 
         open = group,
         blue = facet) %>% 
  select(c(8:10,2:5)) %>%
  filter(blue == 0)

tt_roles_green <- ggplot(pred_green, aes(x = role, y = predicted)) +
  geom_point(aes(shape = open), size = 3, position = position_dodge(0.3)) +
  scale_shape_manual(values = c(16, 8), labels = c("Reserved First", "Open First")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = open),
                position = position_dodge(0.3), width = 0.2) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(y = "Proportion of Truth-telling Rates", x = "Student Roles", title = "Green Priority") +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(labels = c("Role1", "Role2", "Role3", "Role4", "Role5", "Role6")) +
  guides(color = "none")



############################ Figure: Proportion of truth-telling rates by student roles, open; facet by blue
pred <- rbind(pred1, pred2, pred3, pred4, pred5, pred6)
pred <- pred %>% 
  mutate(role = values, 
        open = group,
        blue = facet) %>% 
  select(c(8:10,2:5))

blue_labs <- c("0" = "Green priorities", "1" = "Blue priorities")

tt_plot <- ggplot(pred, aes(x = role, y = predicted)) +
  geom_point(aes(shape = open), size = 3, position = position_dodge(0.3)) +
  scale_shape_manual(values = c(16, 8), labels = c("Reserved First", "Open First")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = open),
                position = position_dodge(0.3), width = 0.2) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  facet_wrap(~ blue, labeller = labeller(blue = blue_labs)) +
  labs(y = "Proportion of Truth-telling Rates", x = "Student Roles", title = "") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(labels = c("Role1", "Role2", "Role3", "Role4", "Role5", "Role6")) +
  guides(color = "none")

##-------------------------------------------------------------
##                      individual efficiency               --
##-------------------------------------------------------------
# regression equation
payoff_role <- lm(payoff ~ ttc * open * blue * role2 + ttc * open * blue * role3 + 
                    ttc * open * blue * role4 + ttc * open * blue * role5 + 
                    ttc * open * blue * role6 + period + I(period^2), 
                  data = long_df)

# marginal effects of open, blue by roles
pred1 <- ggpredict(payoff_role, terms = c("role2", "open", "blue" )) %>% filter(x == 0)
pred2 <- ggpredict(payoff_role, terms = c("role2", "open", "blue" )) %>% filter(x == 1) 
pred3 <- ggpredict(payoff_role, terms = c("role3", "open", "blue" )) %>% filter(x == 1) 
pred4 <- ggpredict(payoff_role, terms = c("role4", "open", "blue" )) %>% filter(x == 1)
pred5 <- ggpredict(payoff_role, terms = c("role5", "open", "blue" )) %>% filter(x == 1) 
pred6 <- ggpredict(payoff_role, terms = c("role6", "open", "blue" )) %>% filter(x == 1) 
values <- rep(paste0("role", seq(length.out = 6, by = 1)), each = 4)

pred <- rbind(pred1, pred2, pred3, pred4, pred5, pred6)

############################ Figure: Payoff by student roles, open for Blue
pred_blue <- pred %>% 
  mutate(role = values, 
         open = group,
         blue = facet) %>% 
  select(c(8:10,2:5)) %>%
  filter(blue == 1)

eff_roles_blue <- ggplot(pred_blue, aes(x = role, y = predicted)) +
  geom_point(aes(shape = open), size = 3, position = position_dodge(0.3)) +
  scale_shape_manual(values = c(16, 8), labels = c("Reserved First", "Open First")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = open),
                position = position_dodge(0.3), width = 0.2) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(y = "Payoff", x = "Student Roles", title = "Blue Priority") +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(labels = c("Role1", "Role2", "Role3", "Role4", "Role5", "Role6")) +
  guides(color = "none")

############################ Figure: Payoff by student roles, open for Green
pred_green <- pred %>% 
  mutate(role = values, 
         open = group,
         blue = facet) %>% 
  select(c(8:10,2:5)) %>%
  filter(blue == 0)

eff_roles_green <- ggplot(pred_green, aes(x = role, y = predicted)) +
  geom_point(aes(shape = open), size = 3, position = position_dodge(0.3)) +
  scale_shape_manual(values = c(16, 8), labels = c("Reserved First", "Open First")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = open),
                position = position_dodge(0.3), width = 0.2) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(y = "Payoff", x = "Student Roles", title = "Green Priority") +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(labels = c("Role1", "Role2", "Role3", "Role4", "Role5", "Role6")) +
  guides(color = "none")

############################ Figure 7 Payoff by roles, open; facet by Blue
pred_blue <- pred %>% 
  mutate(role = values, 
         open = group,
         blue = facet) %>% 
  select(c(8:10,2:5))

blue_labs <- c("0" = "Green priorities", "1" = "Blue priorities")

po_plot <- ggplot(pred, aes(x = role, y = predicted)) +
  geom_point(aes(shape = open), size = 3, position = position_dodge(0.3)) +
  scale_shape_manual(values = c(16, 8), labels = c("Reserved First", "Open First")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = open),
                position = position_dodge(0.3), width = 0.2) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  facet_wrap(~ blue, labeller = labeller(blue = blue_labs)) +
  labs(y = "Payoff", x = "") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  scale_x_discrete(labels = c("Role1", "Role2", "Role3", "Role4", "Role5", "Role6")) +
  guides(color = "none")

###### test to see if the diff between OF and RF are statistically different 
group1 <- subset(long_df, role == 6 & open == 0 & blue == 0)$payoff # RF Blue
group2 <- subset(long_df, role == 6 & open == 1 & blue == 0)$payoff # OF Blue
result <- wilcox.test(group1, group2, correct = FALSE)
print(result)

##-------------------------------------------------------------
##                           Appendix                        --
##-------------------------------------------------------------
##-------------------------------------------------------------
##                        Learning effects                   --
##-------------------------------------------------------------
#A cubed term allows for more flexibility in modeling the learning effect, 
#potentially capturing patterns such as initial improvements followed by 
#diminishing returns or even reversal of the learning effect.


############# Truth-telling rates learning effect #############
# Fit the polynomial regression model for each session and subject separately
tt_models <- long_df %>%
  group_by(session, subject.id) %>%
  glm(true_all ~ ttc * open * blue * minority + poly(period, 3), 
                 family = binomial(link = "probit"), 
                 data = .)

# Overall Learning Effect

le <- ggpredict(tt_models, terms = "period")

le_plot <- ggplot(le, aes(x = x, y = predicted)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_classic() +
  labs(x = "Period", y = "Proportion of Truth-telling", shape = NULL) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) 

# Learning Effect by mechanism 
le_tt_ttc <- ggpredict(tt_models, terms = c("period", "ttc"))

le_tt_ttc_plot <- ggplot(le_tt_ttc, aes(x = x, y = predicted, group = group, color = group, shape = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) + 
  scale_shape_manual(values = c(16, 8), labels = c("DA", "TTC")) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(x = "Period", y = "Proportion of Truth-telling", shape = NULL) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  guides(color = "none")


# Learning Effect by precedence order 
le_tt_open <- ggpredict(tt_models, terms = c("period", "open"))

le_tt_open_plot <- ggplot(le_tt_open, aes(x = x, y = predicted, group = group, color = group, shape = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) + 
  scale_shape_manual(values = c(16, 8), labels = c("Reserved First", "Open First")) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(x = "Period", y = "Proportion of Truth-Telling", shape = NULL) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  guides(color = "none")

# Learning Effect by priority order 
le_tt_blue <- ggpredict(tt_models, terms = c("period", "blue"))

le_tt_blue_plot <- ggplot(le_tt_blue, aes(x = x, y = predicted, group = group, color = group, shape = group)) +
  geom_line(size = 1.2) +
  geom_point(size=2) + 
  scale_shape_manual(values = c(16, 8), labels = c("Green", "Blue")) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(x = "Period", y = "Proportion of Truth-Telling", shape = NULL) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  guides(color = "none")

################# Efficiency learning effect ##################
# Fit the polynomial regression model for each session and subject separately
eff_models <- long_df %>%
  group_by(session, subject.id) %>%
  lm(payoff ~ ttc * open * blue * minority + poly(period, 3), 
      data = .)

# Overall Learning Effect
le <- ggpredict(eff_models, terms = "period")

le_plot <- ggplot(le, aes(x = x, y = predicted)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_classic() +
  labs(x = "Period", y = "Payoff", shape = NULL) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) 

# Learning Effect by precedence order 
le_eff_ttc <- ggpredict(eff_models, terms = c("period", "ttc"))

le_eff_ttc_plot <- ggplot(le_eff_ttc, aes(x = x, y = predicted, group = group, color = group, shape = group)) +
  geom_line(size = 1.2) +
  geom_point(size=2) + 
  scale_shape_manual(values = c(16, 8), labels = c("DA", "TTC")) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(x = "Period", y = "Payoff", shape = NULL) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  guides(color = "none")


# Learning Effect by precedence order 
le_eff_open <- ggpredict(eff_models, terms = c("period", "open"))

le_eff_open_plot <- ggplot(le_eff_open, aes(x = x, y = predicted, group = group, color = group, shape = group)) +
  geom_line(size = 1.2) +
  geom_point(size=2) + 
  scale_shape_manual(values = c(16, 8), labels = c("Reserved First", "Open First")) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(x = "Period", y = "Payoff", shape = NULL) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  guides(color = "none")

# Learning Effect by priority order 
le_eff_blue <- ggpredict(eff_models, terms = c("period", "blue"))

le_eff_blue_plot <- ggplot(le_eff_blue, aes(x = x, y = predicted, group = group, color = group, shape = group)) +
  geom_line(size = 1.2) +
  geom_point(size=2) + 
  scale_shape_manual(values = c(16, 8), labels = c("Green", "Blue")) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(x = "Period", y = "Payoff", shape = NULL) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  guides(color = "none")

################# Stability learning effect ##################
# Fit the polynomial regression model for each session and subject separately
stab_models <- long_df %>%
  group_by(session, subject.id) %>%
  glm(envy ~ ttc * open * blue * minority + poly(period, 3), 
      family = binomial(link = "probit"), 
      data = .)

# Overall Learning Effect
le <- ggpredict(stab_models, terms = "period")

le_plot <- ggplot(le, aes(x = x, y = predicted)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_classic() +
  labs(x = "Period", y = "Envied", shape = NULL) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) 

# Learning Effect by precedence order 
le_stab_ttc <- ggpredict(stab_models, terms = c("period", "ttc"))

le_stab_ttc_plot <- ggplot(le_stab_ttc, aes(x = x, y = predicted, group = group, color = group, shape = group)) +
  geom_line(size = 1.2) +
  geom_point(size=2) + 
  scale_shape_manual(values = c(16, 8), labels = c("DA", "TTC")) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(x = "Period", y = "Envied", shape = NULL) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  guides(color = "none")


# Learning Effect by precedence order 
le_eff_open <- ggpredict(eff_models, terms = c("period", "open"))

le_eff_open_plot <- ggplot(le_eff_open, aes(x = x, y = predicted, group = group, color = group, shape = group)) +
  geom_line(size = 1.2) +
  geom_point(size=2) + 
  scale_shape_manual(values = c(16, 8), labels = c("Reserved First", "Open First")) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(x = "Period", y = "Payoff", shape = NULL) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  guides(color = "none")

# Learning Effect by priority order 
le_eff_blue <- ggpredict(eff_models, terms = c("period", "blue"))

le_eff_blue_plot <- ggplot(le_eff_blue, aes(x = x, y = predicted, group = group, color = group, shape = group)) +
  geom_line(size = 1.2) +
  geom_point(size=2) + 
  scale_shape_manual(values = c(16, 8), labels = c("Green", "Blue")) +
  scale_color_grey() +  # Set colors to grayscale
  theme_classic() +
  labs(x = "Period", y = "Payoff", shape = NULL) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "transparent", color = "transparent")) +
  guides(color = "none")

##############################################################################
################# TT rates by mechanism and precedence order##################
ttc_labs <- c("0" = "DA", "1" = "TTC")
tt_impact_int <- lm(true_all ~ open * minority * ttc, data = long_df)
pred <- ggpredict(tt_impact_int, terms = c("open", "minority", "ttc")) # group = minority, facet = ttc

tt_ttc_po_plot <- ggplot(pred, aes(x = x, y = predicted)) +
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
  guides(color = "none")

