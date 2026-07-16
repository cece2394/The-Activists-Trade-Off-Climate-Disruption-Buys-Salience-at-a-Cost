library(ggplot2)
library(marginaleffects)
library(stargazer)
library(dplyr)

d <- read.csv("disruption_data_clean.csv")

#n and retention rate
sum(!is.na(d$Treated))/nrow(d)
table(d$Treated) #per condition

#leave out wave 2 dropouts
d <- d[!is.na(d$Treated),]

#descriptives
prop.table(table(d$FT_next)) #pct. Moderaterne
prop.table(table(d$FT_next_wing)) #pct. from each wing
prop.table(table(d$FT_next[d$FT_next_wing=="other"])) #composition of "other" political orientation
prop.table(table(d$region)) #n per region

#main outcomes
outcome_cols <- c("SalienceClim_any", "Highway","Govt","Concern","Behavior")
outcome_names <- c("Salience", "Highway opposition", "Govt. dissatisf.", "Concern", "Behavior intent")

#normalize differences by width of scale (e.g. divide by 4 for 1-5 scale)
#this is equivalent to rescaling original variables to 0-1
for(outcome in outcome_cols[-1]){
  scalemax <- max(d[outcome], na.rm=T)
  d[paste0(outcome,"_diff_norm")]  <- d[paste0(outcome,"_diff")]/(scalemax-1)
}

#salience does not need rescaling; already 0-1
d["SalienceClim_any_diff_norm"] <- d["SalienceClim_any_diff"]

#weighting
library(survey)
d_weighted <- svydesign(ids = ~1, data = d, weights = d$weight_w2)


##main effect of treatment on first differences

#main: salience, highway support, satisfaction with govt, concern, behavior
summary(svyglm(SalienceClim_any_diff ~ Treated, design=d_weighted))
summary(svyglm(Highway_diff ~ Treated, design=d_weighted))
summary(svyglm(Govt_diff ~ Treated, design=d_weighted))
summary(svyglm(Concern_diff ~ Treated, design=d_weighted))
summary(svyglm(Behavior_diff ~ Treated, design=d_weighted))

#percent mentioning Climate in wave 1 (weighted and unweighted)
d %>% summarise(weighted_mean = sum(SalienceClim_any * weight_w2) / sum(weight_w2))
prop.table(table(d$SalienceClim_any))

#significant effects in SDs
fit_salience <- svyglm(SalienceClim_any_diff ~ Treated, design=d_weighted)
fit_salience$coefficients["Treated"] / sd(d$SalienceClim_any, na.rm=T) #effect in SDs
fit_highway <- svyglm(Highway_diff ~ Treated, design=d_weighted)
fit_highway$coefficients["Treated"] / sd(d$Highway, na.rm=T) #effect in SDs

#secondary: other policies
summary(svyglm(Policy_diff ~ Treated, design=d_weighted))


##main effect graph

#effect sizes (0-1) and CIs
est_CI <- as.data.frame(t(sapply(outcome_cols, function(outcome) {
  formula <- as.formula(paste0(outcome, "_diff_norm ~ Treated"))
  fit <- svyglm(formula, design=d_weighted)
  c(fit$coefficients["Treated"], confint(fit)["Treated",])
})))

#clean
colnames(est_CI) <- c("est", "CI_lo", "CI_hi")
est_CI$outcome <- outcome_names
est_CI$outcome <- factor(est_CI$outcome, levels=outcome_names)
est_CI$study <- "Study 1, DK"

#export
write.csv(est_CI, "main_estimates_CIs.csv", row.names=F)


##manipulation checks

#time spent on article (treated only)
tapply(d$Treated_timer, d$Treated, mean, ra.rm=T)
t.test(Treated_timer ~ Manipulation_correct, d=d[d$Treated==1,]) #by correct answer

#rate of correct answer by treatment
tapply(d$Manipulation_correct, d$Treated, mean, ra.rm=T)
tapply(d$Manipulation_correct | d$Manipulation_protest, d$Treated, mean, ra.rm=T)
#also counting respondents giving a "protest" answer

