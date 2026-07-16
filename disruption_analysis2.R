library(ggplot2)
library(marginaleffects)
library(stargazer)

d <- read.csv("disruption_data_clean2.csv")

#n and retention rate
sum(!is.na(d$Treated))/nrow(d)
table(d$Treated) #per condition

#leave out wave 2 dropouts
d <- d[!is.na(d$Treated),]

#descriptives
round(prop.table(table(d$Party)), 2) #pct. from each party
prop.table(table(d$Ideology3)) #pct. from each Ideology wing
prop.table(table(d$Next_elect_wing)) #pct. from each party wing
prop.table(table(d$SalienceClim_any))

#main outcomes
outcome_cols <- c("SalienceClim_any", "Nonewoil","Govt","Concern","Behavior")
outcome_names <- c("Salience", "Oil opposition", "Govt. dissatisf.", "Concern", "Behavior intent")

#normalize differences by width of scale (e.g. divide by 4 for 1-5 scale)
#this is equivalent to rescaling original variables to 0-1
for(outcome in outcome_cols[-1]){
  scalemax <- max(d[outcome], na.rm=T)
  d[paste0(outcome,"_diff_norm")]  <- d[paste0(outcome,"_diff")]/(scalemax-1)
}

#salience does not need rescaling; already 0-1
d["SalienceClim_any_diff_norm"] <- d["SalienceClim_any_diff"]


##main effect of treatment on first differences

#main: salience, highway support, satisfaction with govt, concern, behavior
summary(lm(SalienceClim_any_diff ~ Treated, data=d))
summary(lm(Nonewoil_diff ~ Treated, data=d))
summary(lm(Govt_diff ~ Treated, data=d))
summary(lm(Concern_diff ~ Treated, data=d))
summary(lm(Behavior_diff ~ Treated, data=d))

#significant effects in SDs
fit_salience <- lm(SalienceClim_any_diff ~ Treated, data=d)
fit_salience$coefficients["Treated"] / sd(d$SalienceClim_any, na.rm=T) #effect in SDs
fit_nonewoil <- lm(Nonewoil_diff ~ Treated, data=d)
fit_nonewoil$coefficients["Treated"] / sd(d$Nonewoil, na.rm=T) #effect in SDs

#secondary: other policies, fossil investments
summary(lm(Policy_diff ~ Treated, data=d))
summary(lm(Fossilinv_diff ~ Treated, data=d))


##main effect graph

#effect sizes (0-1) and CIs
est_CI <- as.data.frame(t(sapply(outcome_cols, function(outcome) {
  formula <- as.formula(paste0(outcome, "_diff_norm ~ Treated"))
  fit <- lm(formula, data=d)
  c(fit$coefficients["Treated"], confint(fit)["Treated",])
})))

#clean
colnames(est_CI) <- c("est", "CI_lo", "CI_hi")
est_CI$outcome <- outcome_names
est_CI$outcome <- factor(est_CI$outcome, levels=outcome_names)
est_CI$study <- "Study 2, UK"

#export
write.csv(est_CI, "main_estimates_CIs2.csv", row.names=F)


##polarization and heterogeneous effects

#set left as baseline category
d$Ideology3 <- factor(d$Ideology3, levels=c("left", "center", "right"))

#by pol. orientation: conditional effect estimates and CIs for figure
cond_est_CI <- lapply(outcome_cols, function(outcome){
  
  #fit interactive model (with normalized outcome)
  formula <- as.formula(paste0(outcome, "_diff_norm ~ Treated*Ideology3"))
  fit <- lm(formula, data=d)
  print(outcome)
  print(summary(fit))
  
  #get marginal effect of Treated for different values of the moderator
  CATEs <- slopes(fit, newdata = datagrid(Treated = 0, Ideology3 = c("left", "center", "right")))
  #note: only need one Treated value, as model is linear: other values would give same result
  CATEs <- tail(CATEs, 3)[c("Ideology3","estimate","conf.low","conf.high")] 
  CATEs

})
cond_est_CI <- as.data.frame(do.call(rbind, cond_est_CI))
colnames(cond_est_CI) <- c("Ideology", "est", "CI_lo", "CI_hi")
cond_est_CI$outcome <- rep(outcome_names, each=3)
cond_est_CI$outcome <- factor(cond_est_CI$outcome, levels=outcome_names)

#figure
pdf(paste0("figures/disruption_byideology2.pdf"), 11.5, 6)
print(ggplot(cond_est_CI, aes(x=outcome, y=est, color=Ideology)) + 
        geom_hline(yintercept=0, color="dark grey") +
        geom_errorbar(aes(ymin=CI_lo, ymax=CI_hi), width=.1, position=position_dodge(.2)) +
        geom_point(position=position_dodge(.2)) +
        ylab(paste0("Effect of disruption treatment")) +
        theme_bw() +
        theme(axis.title.x=element_blank(),
              axis.title.y=element_text(size=17),
              axis.text=element_text(size=17),
              legend.position="bottom",
              legend.title=element_text(size=17),
              legend.text=element_text(size=17)))
dev.off()

#see Appendix script for results summarized in-text


##manipulation checks

#time spent on article (treated only)
tapply(d$Treated_timer, d$Treated, mean, ra.rm=T)
t.test(Treated_timer ~ Manipulation_correct, d=d[d$Treated==1,]) #by correct answer

#rate of correct answer by treatment
tapply(d$Manipulation_correct, d$Treated, mean, ra.rm=T)
tapply(d$Manipulation_correct | d$Manipulation_protest, d$Treated, mean, ra.rm=T)
#also counting respondents giving a "protest" answer


