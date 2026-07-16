library(ggplot2)
library(xtable)
library(stargazer)
library(stringr)
library(BayesFactor)
library(marginaleffects)

d <- read.csv("disruption_data_clean2.csv")

#leave out wave 2 dropouts
d <- d[!is.na(d$Treated),]

#main outcomes
outcome_cols <- c("SalienceClim_any","Nonewoil","Govt","Concern","Behavior")
outcome_cols_all <- c(outcome_cols, "Policy", "Fossilinv")
outcome_names <- c("Salience", "Oil opposition", "Govt. dissatisf.", "Concern", "Behavior intent")
outcome_names_all <- c(outcome_names, "(Climate policy)", "(Fossil regulat.)")
outcome_shorts <- c("Salience", "Oil opp.", "Govt. dissat.", "Concern", "Behavior intent")


##Appendix C: demographics and representativeness

#create binned age variable
d$Age.binned <- cut(d$Age, 
                  breaks = c(18, 24, 34, 44, 54, Inf), 
                  labels = c("18-24", "25-34", "35-44", "45-54", "55+"),
                  right = TRUE)

#get proportions of each demographic group
dem_vars <- c("Sex", "Age.binned", "Ethnicity.simplified")
props <- apply(d[dem_vars], 2, function(x) data.frame(prop.table(table(x))))
props <- do.call(rbind, props)
colnames(props) <- c("demographic", "prop_sample")

#merge with population statistics
population_props <- read.csv("population data/population_statistics_Prolific.csv")
props <- cbind(props, population_props["Proportion"])

#convert to % and round
props$prop_sample <- round(props$prop_sample*100, 1)
props$Proportion <- round(props$Proportion*100, 1)

#prepare table for Latex
library(xtable)
props_xt <- xtable(props, digits=1,
                   caption="Percentages of sample in each demographic
                   group, compared to UK population percentages for the study period
                   (2021 and 2022 census data compiled by Prolific).", label="tab:demographics2")
names(props_xt) <- c('Demographic','Sample %, unweighted','Population %')
print(props_xt, file = "tables/demographics2.tex", include.rownames=F,
      hline.after = c(-1, 0, 2, 7, 12))

#pct. intending to vote for each party, leaving out undecideds and
#Northern Ireland party voters
NI_parties <- c("Sinn Féin", "Alliance Party of Northern Ireland")
eligible <- d$Next_elect_wing != "other" & !(d$Party %in% NI_parties)
party <- d$Party[eligible]
props_party <- data.frame(prop.table(table(party)))

#convert to % and round
props_party$Freq <- round(props_party$Freq*100, 1)
props_party <- rbind(props_party, data.frame(party=c("UK Independence Party", "Change UK"),
                                             Freq=c(0, 0)))

#merge with Politico estimates
election_polls <- read.csv("population data/voting_intentions_Politico_UK.csv")
props_party <- cbind(props_party, election_polls)[-3]
props_party <- props_party[order(props_party$poll_proportion, decreasing=TRUE),]

#prepare table for Latex
props_xt <- xtable(props_party, digits=1,
                   caption="Unweighted and weighted percentages of sample intending to vote for each
                   party, compared to Politico Poll of Polls (Sinn Fein and APNI not included, as most
                   UK opinion polls do not cover the 3\\% of voters who live in Northern Ireland).",
                   label="tab:votingintention2")
names(props_xt) <- c('Party','Sample %, unweighted', 'Politico %')
print(props_xt, file = "tables/votingintention2.tex", include.rownames=F)

#comparison of left/right/center %
prop.table(table(d$Next_elect_wing[eligible]))
library(dplyr)
election_polls$Next_elect_wing <- recode(election_polls$party, `Labour Party` = "left", `Plaid Cymru` = "left",
                                         `Green Party` = "left", `Workers Party of Britain` = "left", `Scottish National Party (SNP)` = "left",
                                         `Social Democratic and Labour Party (SDLP)` = "left", `Liberal Democrats` = "center", 
                                         `Conservative Party` = "right", `Reform UK` = "right", `UK Independence Party`="right",
                                         `Change UK` = "right")
prop.table(table(election_polls$Next_elect_wing))

#pct. mentioning climate or environment in wave 1
mean(d$SalienceClim_any)


##Appendix F: histograms of wave 1 outcomes

#prepare long data (including political wing for when we split by that)
d_outcomes <- d[c("hashID", outcome_cols_all, "Ideology3", "Next_elect_wing")]
d_outcomes <- reshape(d_outcomes, idvar=c("hashID",  "Ideology3", "Next_elect_wing"),
                      varying=list(2:8), times=outcome_names_all,
                      v.names="response", timevar="outcome", direction="long")
d_outcomes$outcome <- factor(d_outcomes$outcome, levels=outcome_names_all)
d_outcomes$Next_elect_wing <- factor(str_to_title(d_outcomes$Next_elect_wing),
                                 levels=c("Left","Center","Right"))

#draw plot
pdf(paste0("figures/descriptive_histograms2.pdf"), 5, 7.5)
ggplot(d_outcomes, aes(response)) + 
  geom_bar(fill="salmon") + 
  facet_wrap(~outcome, scales="free", ncol=2) + 
  scale_x_continuous(breaks=seq(0,7)) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())
dev.off()


##Appendix G: descriptive tables: means by pre/post and condition

diffs <- t(sapply(outcome_cols_all, function(outcome) {
  
  #kick out outcome-specific dropouts
  d_complete <- d[ !is.na(d[outcome]) & !is.na(d[paste0(outcome,"_w2")]) ,]
  
  #calculate means
  w1_means <- tapply(d_complete[[outcome]], d_complete$Treated, mean, na.rm=T)
  w2_means <- tapply(d_complete[[paste0(outcome,"_w2")]], d_complete$Treated, mean, na.rm=T)
  
  #calculate differences
  control_diff <- w2_means[1] - w1_means[1]
  treat_diff <- w2_means[2] - w1_means[2]
  did <- treat_diff - control_diff
  
  round(c(w1_means[1], w2_means[1], control_diff,
          w1_means[2], w2_means[2], treat_diff,
          did), 3)
  
}))
colnames(diffs) <- c("Control, pre","Control, post", "Control, diff.",
                     "Treated, pre","Treated, post", "Treated, diff.",
                     "Diff in diff")
rownames(diffs) <- outcome_names_all

#bring in SEs and p-values
SE_pval <- as.data.frame(t(sapply(outcome_cols_all, function(outcome) {
  formula <- as.formula(paste0(outcome, "_diff ~ Treated"))
  fit <- lm(formula, data=d)
  round(summary(fit)$coef["Treated",c(2,4)], 3)
})))
colnames(SE_pval) <- c("SE","p-val.")
diffs <- cbind(diffs, SE_pval)

#Bayes factors
Bayes <- sapply(outcome_cols_all, function(outcome) {
  outcomediff <- paste0(outcome, "_diff")
  formula <- as.formula(paste0(outcomediff, " ~ Treated"))
  fit <- lmBF(formula, data=d[!is.na(d[outcomediff]),])
  extractBF(fit)[1,1]
})
diffs$Bayes <- Bayes

#n
n <- sapply(outcome_cols_all, function(outcome) {
  sum(!is.na(d[paste0(outcome, "_diff")]))
})
diffs$n <- n

#write to Latex
print(xtable(diffs, digits=c(0,2,2,2,2,2,2,2,2,3,2,0)), file="tables/conditional_means2.tex", include.colnames=F, only.contents = T,
      hline.after = NULL)


##Appendix I: regression tables for heterogeneous effects by voting intention and ideology

#set orders of levels
d$Next_elect_wing <- factor(d$Next_elect_wing, levels=c("left","center","other","right"))
d$Ideology3 <- factor(d$Ideology3, levels=c("left","center","right"))

#by political orientation (next election voting intention)
fit_bypol_sal <- lm(SalienceClim_any_diff ~ Treated*Next_elect_wing, data=d)
fit_bypol_oil <- lm(Nonewoil_diff ~ Treated*Next_elect_wing, data=d)
fit_bypol_govt <- lm(Govt_diff ~ Treated*Next_elect_wing, data=d)
fit_bypol_conc <- lm(Concern_diff ~ Treated*Next_elect_wing, data=d)
fit_bypol_behav <- lm(Behavior_diff ~ Treated*Next_elect_wing, data=d)

#for main text, since left-right interaction is significant for government dissatisfaction
#get marginal effect of Treated for different values of the moderator
CATEs_govt <- slopes(fit_bypol_govt, newdata = datagrid(Treated = 0, Next_elect_wing = c("left", "right")))
tail(CATEs_govt, 2)[c("Next_elect_wing","estimate","p.value","conf.low","conf.high")] 

#stargazer tables for appendix
stargazer(list(fit_bypol_sal, fit_bypol_oil, fit_bypol_govt, fit_bypol_conc, fit_bypol_behav),
          covariate.labels=c("Treated", "Center", "Other", "Right", "Treated:Other", "Treated:Center", "Treated:Right"),
          dep.var.labels=outcome_shorts, omit = c("Constant"), omit.stat=c("f","ser"), digits=2,
          out="tables/disruption_bypolitics2.tex", label="tab:disruption_bypolitics2",
          title="Models for Study 2 with interaction between disruption treatment
          and voting intention: left (baseline category), center, undecided/other or right. Dependent variables
          are first-differenced outcomes between wave 2 and wave 1 on their original scales.",
          star.cutoffs=c(0.05,0.01,0.001))

#by ideology
fit_byideo_sal <- lm(SalienceClim_any_diff ~ Treated*Ideology3, data=d)
fit_byideo_oil <- lm(Nonewoil_diff ~ Treated*Ideology3, data=d)
fit_byideo_govt <- lm(Govt_diff ~ Treated*Ideology3, data=d)
fit_byideo_conc <- lm(Concern_diff ~ Treated*Ideology3, data=d)
fit_byideo_behav <- lm(Behavior_diff ~ Treated*Ideology3, data=d)

#for main text, since some effects are significant for right-wingers (only)
#get marginal effect of Treated for different values of the moderator
CATEs_sal <- slopes(fit_byideo_sal, newdata = datagrid(Treated = 0, Ideology3 = c("right")))
tail(CATEs_sal, 1)[c("estimate","p.value","conf.low","conf.high")] 
CATEs_oil <- slopes(fit_byideo_oil, newdata = datagrid(Treated = 0, Ideology3 = c("right")))
tail(CATEs_oil, 1)[c("estimate","p.value","conf.low","conf.high")] 

#stargazer tables for appendix
stargazer(list(fit_byideo_sal, fit_byideo_oil, fit_byideo_govt, fit_byideo_conc, fit_byideo_behav),
          covariate.labels=c("Treated", "Center", "Right", "Treated:Center", "Treated:Right"),
          dep.var.labels=outcome_shorts, omit = c("Constant"), omit.stat=c("f","ser"), digits=2,
          out="tables/disruption_byideology2.tex", label="tab:disruption_byideology2",
          title="Models for Study 2 with interaction between disruption treatment
          and ideology: left (baseline category), center or right. Dependent variables
          are first-differenced outcomes between wave 2 and wave 1 on their original scales.",
          star.cutoffs=c(0.05,0.01,0.001))



##Appendix I: figure for heterogeneous effects by voting intention (ideology is in main analyses)

#normalize differences by width of scale (e.g. divide by 4 for 1-5 scale)
#this is equivalent to rescaling original variables to 0-1
for(outcome in outcome_cols[-1]){
  scalemax <- max(d[outcome], na.rm=T)
  d[paste0(outcome,"_diff_norm")]  <- d[paste0(outcome,"_diff")]/(scalemax-1)
}

#salience does not need rescaling; already 0-1
d["SalienceClim_any_diff_norm"] <- d["SalienceClim_any_diff"]

#conditional effect estimates and CIs for figure
cond_est_CI <- lapply(outcome_cols, function(outcome){
  
  #fit interactive model (with normalized outcome)
  formula <- as.formula(paste0(outcome, "_diff_norm ~ Treated*Next_elect_wing"))
  fit <- lm(formula, data=d)
  #note: you can also just do lm(get(outcome) ~ ...)
  
  #get marginal effect of Treated for different values of the moderator
  CATEs <- slopes(fit, newdata = datagrid(Treated = 0, Next_elect_wing = c("left", "center", "other", "right")))
  #note: only need one Treated value, as model is linear: other values would give same result
  CATEs <- tail(CATEs, 4)[c("Next_elect_wing","estimate","conf.low","conf.high")] 
  CATEs
  
})
cond_est_CI <- as.data.frame(do.call(rbind, cond_est_CI))
colnames(cond_est_CI) <- c("Orientation", "est", "CI_lo", "CI_hi")
cond_est_CI$outcome <- rep(outcome_names, each=4)
cond_est_CI$outcome <- factor(cond_est_CI$outcome, levels=outcome_names)

#get colors: default from three-color palette plus yellow
library(scales)
show_col(hue_pal()(3))

#figure
pdf(paste0("figures/disruption_bypolitics2.pdf"), 11.5, 6)
print(ggplot(cond_est_CI, aes(x=outcome, y=est, color=Orientation)) + 
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
              legend.text=element_text(size=17)) +
        labs(color="Voting intention") + 
        scale_color_discrete(labels=c("left", "center", "other", "right")) +
        scale_color_manual(values=c("#f8766d", "#CBA804", "#00ba38", "#619cff")))
dev.off()


##Appendix I: Ceiling effects analysis

#pct. top responses on concern among left wing
mean(d$Concern[d$Next_elect_wing=="left"] == 7, na.rm=T) #on both items
mean(d$Concern[d$Next_elect_wing=="left"] >= 6.5, na.rm=T) #on at least one item

#pct. top responses on concern among left wing
mean(d$Nonewoil[d$Next_elect_wing=="left"] == 5, na.rm=T)

#pct. bottom responses on behavior among right wing
mean(d$Behavior[d$Next_elect_wing=="right"] == 1, na.rm=T) #on both items

#interactions by voting intention, leaving out respondents with top answer in Wave 1
ints_pol <- lapply(outcome_cols, function(outcome){
  formula <- as.formula(paste0(outcome, "_diff ~ Treated*Next_elect_wing"))
  notmax <- ( d[outcome] != max(d[outcome], na.rm=T) )
  fit <- lm(formula, data=d[notmax,])
  int <- tail(summary(fit)$coefficients[,c(1,2,4)], 1)
  n <- nobs(fit)
  int <- cbind(int, n)
})
ints_pol <- do.call(rbind, ints_pol)
rownames(ints_pol) <- outcome_names
print(xtable(ints_pol, digits=c(0,2,2,3,0),
             caption="Interaction coefficients on each outcome measure, between the media treatment and left-
                      wing (baseline category) versus right-wing voting intention (Study 2),
                      leaving out respondents that gave the top answer(s) on that outcome variable in Wave 1.",
             label="tab:disruption_noceiling_pol"),
      file="tables/disruption_noceiling_pol2.tex", caption.placement = "top")

#interactions by ideology, leaving out respondents with top answer in Wave 1
ints_ideo <- lapply(outcome_cols, function(outcome){
  formula <- as.formula(paste0(outcome, "_diff ~ Treated*Ideology3"))
  notmax <- ( d[outcome] != max(d[outcome], na.rm=T) )
  fit <- lm(formula, data=d[notmax,])
  int <- tail(summary(fit)$coefficients[,c(1,2,4)], 1)
  n <- nobs(fit)
  int <- cbind(int, n)
})
ints_ideo <- do.call(rbind, ints_ideo)
rownames(ints_ideo) <- outcome_names
print(xtable(ints_ideo, digits=c(0,2,2,3,0),
             caption="Interaction coefficients on each outcome measure, between the media treatment and left-
                      wing (baseline category) versus right-wing ideology (Study 2),
                      leaving out respondents that gave the top answer(s) on that outcome variable in Wave 1.",
             label="tab:disruption_noceiling_ideo2"),
      file="tables/disruption_noceiling_ideo2.tex", caption.placement = "top")


##Appendix I: regression tables for heterogeneous effects by wave 1 outcomes

#interactions by wave 1 answer
ints <- lapply(outcome_cols, function(outcome){
  formula <- as.formula(paste0(outcome, "_diff ~  Treated*", outcome))
  fit <- lm(formula, data=d)
  int <- tail(summary(fit)$coefficients[,c(1,2,4)], 1)
  n <- nobs(fit)
  int <- cbind(int, n)
})
ints <- do.call(rbind, ints)
rownames(ints) <- outcome_names
print(xtable(ints, digits=c(0,2,2,3,0),
             caption="Interaction coefficients on each outcome measure, between the media
             treatment and respondents' wave 1 (pre-treatment) attitude on the same outcome (Study 2).",
             label="tab:disruption_bywave1_2"),
      file="tables/disruption_bywave1_2.tex", caption.placement = "top")

#interaction effects on salience by relevant wave 1 outcomes
moderators <- c(2,3,4,6,7)
sal_ints <- lapply(outcome_cols_all[moderators], function(mdr){
  formula <- as.formula(paste0("SalienceClim_any_diff ~  Treated*", mdr))
  fit <- lm(formula, data=d)
  int <- tail(summary(fit)$coefficients[,c(1,2,4)], 1)
  n <- nobs(fit)
  int <- cbind(int, n)
})
sal_ints <- do.call(rbind, sal_ints)
rownames(sal_ints) <- outcome_names_all[moderators]
print(xtable(sal_ints, digits=c(0,2,2,3,0),
             caption="Interaction effects on salience as the DV, between the media
             treatment and respondents' wave 1 (pre-treatment) attitude on the each outcome (Study 2).",
             label="tab:disruption_salience_bywave1_2"),
      file="tables/disruption_salience_bywave1_2.tex", caption.placement = "top")


##Appendix I: regression tables for heterogeneous effects by gender and age

#interactions by gender
ints <- lapply(outcome_cols, function(outcome){
  formula <- as.formula(paste0(outcome, "_diff ~  Treated*Sex"))
  fit <- lm(formula, data=d)
  int <- tail(summary(fit)$coefficients[,c(1,2,4)], 1)
  n <- nobs(fit)
  int <- cbind(int, n)
})
ints <- do.call(rbind, ints)
rownames(ints) <- outcome_names
print(xtable(ints, digits=c(0,2,2,3,0),
             caption="Interaction coefficients between gender and the media
             treatment on each outcome measure (Study 2). Reference category is
             female.",
             label="tab:disruption_bygender2"),
      file="tables/disruption_bygender2.tex", caption.placement = "top")

#interactions by age
d$age10 <- d$Age/10
ints <- lapply(outcome_cols, function(outcome){
  formula <- as.formula(paste0(outcome, "_diff ~  Treated*age10"))
  fit <- lm(formula, data=d)
  int <- tail(summary(fit)$coefficients[,c(1,2,4)], 1)
  n <- nobs(fit)
  int <- cbind(int, n)
})
ints <- do.call(rbind, ints)
rownames(ints) <- outcome_names
print(xtable(ints, digits=c(0,2,2,3,0),
             caption="Interaction coefficients between age and the media
             treatment on each outcome measure (Study 2). Age is in decades
             (i.e. divided by 10).",
             label="tab:disruption_byage2"),
      file="tables/disruption_byage2.tex", caption.placement = "top")

