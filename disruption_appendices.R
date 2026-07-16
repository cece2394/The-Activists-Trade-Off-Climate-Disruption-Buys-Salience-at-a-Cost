
library(ggplot2)
library(xtable)
library(stargazer)
library(stringr)
library(BayesFactor)
library(marginaleffects)
library(dplyr)
library(survey)
library(xtable)
library(readxl)

d <- read.csv("disruption_data_clean.csv")

#leave out wave 2 dropouts
d <- d[!is.na(d$Treated),]

#main outcomes
outcome_cols <- c("SalienceClim_any", "Highway","Govt","Concern","Behavior")
outcome_cols_all <- c(outcome_cols, "Policy")
outcome_names <- c("Salience", "Highway opposition", "Govt. dissatisf.", "Concern", "Behavior intent")
outcome_names_all <- c(outcome_names, "(Climate policy)")
outcome_shorts <- c("Salience", "Highway opp.", "Govt. dissat.", "Concern", "Behavior intent")


##Appendix C: demographics and representativeness

#order regions logically (east to west)
d$region <- factor(d$region, levels = c("Capital region", "Zealand", "Southern Denmark",
                                        "Middle Jutland", "North Jutland"))

#function to get both weighted and unweighted frequency tables
get_props <- function(var, d){
  
  # Compute the total weight for each level within the factor variable
  proportions <- aggregate(weight_w2 ~ get(var), data = d, FUN = sum)
  # Rename columns for clarity
  colnames(proportions) <- c("demographic", "weighted_sum")
  # Calculate the weighted proportion by dividing by the total sum of weights
  proportions$weighted_prop <- proportions$weighted_sum / sum(d$weight_w2)
  # Add the unweighted proportions
  proportions$unweighted_prop <- prop.table(table(d[var]))
  # Keep only relevant columns
  proportions <- proportions[, c("demographic", "unweighted_prop", "weighted_prop")]
  
  return(proportions)
  
}

#get weighted proportions of each demographic group
dem_vars <- c("gender", "profile_age_rec", "region")
weighted_props <- list()
for (var in dem_vars) {
  weighted_props[[var]] <- get_props(var, d)
}
weighted_props <- do.call(rbind, weighted_props)

#convert to % and round
weighted_props$unweighted_prop <- round(weighted_props$unweighted_prop*100, 1)
weighted_props$weighted_prop <- round(weighted_props$weighted_prop*100, 1)

#merge with population statistics
population_props <- read.csv("population data/population_statistics_YouGov.csv")
props <- cbind(weighted_props, population_props["proportion"])

#prepare table for Latex
props_xt <- xtable(props, digits=1,
                   caption="Unweighted and weighted percentages of sample in each demographic
                   group, compared to Danish population percentages for the study period (as obtained
                   from YouGov).", label="tab:demographics")
names(props_xt) <- c('Demographic','Sample %, unweighted','Sample %, weighted','Population %')
print(props_xt, file = "tables/demographics.tex", include.rownames=F,
      hline.after = c(-1, 0, 2, 5, 10))

#pct. intending to vote for each party, leaving out undecideds
props_party <- get_props("FT_next", d[d$FT_next_wing != "other",])
colnames(props_party)[1] <- "party"

#convert to % and round
props_party$unweighted_prop <- round(props_party$unweighted_prop*100, 1)
props_party$weighted_prop <- round(props_party$weighted_prop*100, 1)

#merge with population statistics
election_polls <- read_excel("population data/voting_intentions_Nov2023.xlsx", na = "NA")
election_polls <- election_polls[,c("party_letter","poll_proportion")]
props_party$party_letter <- sapply(strsplit(props_party$party, "\\."), `[`, 1)
props_party$party <- sapply(strsplit(props_party$party, "\\."), `[`, 2)
props_party <- merge(props_party, election_polls)
props_party <- props_party[order(props_party$poll_proportion, decreasing=TRUE),]

#shorten long party names
props_party$party <- gsub(" - Inger Støjberg", "", props_party$party)
props_party$party <- gsub(", Danmarks Liberale Parti", "", props_party$party)
props_party$party <- gsub(" - De Rød-Grønne", "", props_party$party)

#prepare table for Latex
props_xt <- xtable(props_party[2:5], digits=1,
                   caption="Unweighted and weighted percentages of sample intending to vote for each
                   party, compared to combined Voxmeter and Epinion polls (n=3298).", label="tab:votingintention")
names(props_xt) <- c('Party','Sample %, unweighted','Sample %, weighted','Large poll %')
print(props_xt, file = "tables/votingintention.tex", include.rownames=F)

#pct. mentioning climate (only, no environment) in wave 1
mean(d$SalienceClim_noenv)
weighted.mean(d$SalienceClim_noenv, w=d$weight_w2)


##Appendix F: histograms of wave 1 outcomes

#prepare long data (including political wing for when we split by that)
d_outcomes <- d[c("caseid", outcome_cols_all, "FT_next_wing")]
colnames(d_outcomes) <- c("caseid", outcome_names_all, "Orientation")
d_outcomes <- reshape(d_outcomes, idvar=c("caseid", "Orientation"), varying=list(2:7), times=outcome_names_all,
                      v.names="response", timevar="outcome", direction="long")
d_outcomes$outcome <- factor(d_outcomes$outcome, levels=outcome_names_all)
d_outcomes$Orientation <- factor(str_to_title(d_outcomes$Orientation))

#draw plot
pdf(paste0("figures/descriptive_histograms.pdf"), 5, 7.5)
ggplot(d_outcomes, aes(response)) + 
  geom_bar(fill="salmon") + 
  facet_wrap(~outcome, scales="free", ncol=2) + 
  scale_x_continuous(breaks=seq(0,7)) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())
dev.off()


##Appendix G: descriptive tables: means by pre/post and condition

#weighting
d_weighted <- svydesign(ids = ~1, data = d, weights = d$weight_w2)

#cycle over all outcome variables
diffs <- t(sapply(outcome_cols_all, function(outcome) {
  
  #kick out outcome-specific dropouts
  d_complete <- d[ !is.na(d[outcome]) & !is.na(d[paste0(outcome,"_w2")]) ,]
  
  #make the "outcome" columns contain the current outcome variable 
  d_complete$outcome <- d_complete[[outcome]]
  d_complete$outcome_w2 <- d_complete[[paste0(outcome,"_w2")]]
  
  #calculate wave 1 means (weighted by wave 2 weights)
  w1_means <- d_complete %>%
    group_by(Treated) %>%
    summarise(weighted_mean = sum(outcome * weight_w2) / sum(weight_w2))
  #extract control group and treated group means
  w1_means <- as.vector(w1_means[,2]$weighted_mean)
  
  #calculate wave 2 means (weighted by wave 2 weights)
  w2_means <- d_complete %>%
    group_by(Treated) %>%
    summarise(weighted_mean = sum(outcome_w2 * weight_w2) / sum(weight_w2))
  #extract control group and treated group means
  w2_means <- as.vector(w2_means[,2]$weighted_mean)
  
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
  fit <- svyglm(formula, design=d_weighted)
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
print(xtable(diffs, digits=c(0,2,2,2,2,2,2,2,2,3,2,0)), file="tables/conditional_means.tex", include.colnames=F, only.contents = T,
      hline.after = NULL)


##Appendix H: without population weights

#main: salience, highway support, satisfaction with govt, concern, behavior
fit_noweight_sal <- lm(SalienceClim_any_diff ~ Treated, data=d)
fit_noweight_high <-lm(Highway_diff ~ Treated, data=d)
fit_noweight_govt <-lm(Govt_diff ~ Treated, data=d)
fit_noweight_conc <-lm(Concern_diff ~ Treated, data=d)
fit_noweight_behav <-lm(Behavior_diff ~ Treated, data=d)

fit_noweight <- list(fit_noweight_sal, fit_noweight_high, fit_noweight_govt, fit_noweight_conc, fit_noweight_behav)
stargazer(fit_noweight,
          dep.var.labels=outcome_shorts, omit = c("Constant"), omit.stat=c("f","ser"), digits=2,
          out="tables/disruption_unweighted.tex", title="Effect of the climate disruption media treatment
          on climate attitudes, without using survey weights (Study 1 only). Dependent variables are first-differenced
          outcomes between wave 2 and wave 1 on their original scales.", star.cutoffs=c(0.05,0.01,0.001))


##Appendix I: regression tables for heterogeneous effects by voting intention

#by political orientation (FT voting intention)
fit_bypol_sal <- lm(SalienceClim_any_diff ~ Treated*FT_next_wing, data=d)
fit_bypol_high <- lm(Highway_diff ~ Treated*FT_next_wing, data=d)
fit_bypol_govt <- lm(Govt_diff ~ Treated*FT_next_wing, data=d)
fit_bypol_conc <- lm(Concern_diff ~ Treated*FT_next_wing, data=d)
fit_bypol_behav <- lm(Behavior_diff ~ Treated*FT_next_wing, data=d)

#stargazer tables for appendix
fit_bypol <- list(fit_bypol_sal, fit_bypol_high, fit_bypol_govt, fit_bypol_conc, fit_bypol_behav)
stargazer(fit_bypol,
          covariate.labels=c("Treated", "Other", "Right", "Treated:Other", "Treated:Right"),
          dep.var.labels=outcome_shorts, omit = c("Constant"), omit.stat=c("f","ser"), digits=2,
          out="tables/disruption_bypolitics.tex", label="tab:disruption_bypolitics",
          title="Models for Study 1 with interaction between disruption treatment
          and voting intention: left (baseline category), undecided/other or right. Dependent variables
          are first-differenced outcomes between wave 2 and wave 1 on their original scales.",
          star.cutoffs=c(0.05,0.01,0.001))


##Appendix I: figure for heterogeneous effects by pol. orientation

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
  formula <- as.formula(paste0(outcome, "_diff_norm ~ Treated*FT_next_wing"))
  fit <- lm(formula, data=d)
  
  #get marginal effect of Treated for different values of the moderator
  CATEs <- slopes(fit, newdata = datagrid(Treated = 0, FT_next_wing = c("left", "other", "right")))
  #note: only need one Treated value, as model is linear: other values would give same result
  CATEs <- tail(CATEs, 3)[c("FT_next_wing","estimate","conf.low","conf.high")] 
  CATEs
  
})
cond_est_CI <- as.data.frame(do.call(rbind, cond_est_CI))
colnames(cond_est_CI) <- c("Orientation", "est", "CI_lo", "CI_hi")
cond_est_CI$outcome <- rep(outcome_names, each=3)
cond_est_CI$outcome <- factor(cond_est_CI$outcome, levels=outcome_names)

#figure
pdf(paste0("figures/disruption_bypolitics.pdf"), 11.5, 6)
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
        scale_color_discrete(labels=c("left", "undecided/other", "right")))
dev.off()


##Appendix I: Ceiling effects analysis

mean(d$SalienceClim_any[d$FT_next_wing=="left"] == 1, na.rm=T)
mean(d$SalienceClim_any[d$FT_next_wing=="right"] == 1, na.rm=T)

#pct. top responses on concern among left wing
mean(d$Concern[d$FT_next_wing=="left"] == 7, na.rm=T) #on both items
mean(d$Concern[d$FT_next_wing=="left"] >= 6.5, na.rm=T) #on at least one item

#pct. top responses on concern among left wing
mean(d$Nonewoil[d$FT_next_wing=="left"] == 5, na.rm=T)

#pct. bottom responses on behavior among right wing
mean(d$Behavior[d$FT_next_wing=="right"] == 1, na.rm=T) #on both items

#interactions by voting intention, leaving out respondents with top answer in Wave 1
ints_pol <- lapply(outcome_cols, function(outcome){
  formula <- as.formula(paste0(outcome, "_diff ~ Treated*FT_next_wing"))
  notmax <- ( d[outcome] != max(d[outcome], na.rm=T) )
  fit <- lm(formula, data=d[notmax,])
  print(summary(fit))
  int <- tail(summary(fit)$coefficients[,c(1,2,4)], 1)
  n <- nobs(fit)
  int <- cbind(int, n)
})
ints_pol <- do.call(rbind, ints_pol)
rownames(ints_pol) <- outcome_names
print(xtable(ints_pol, digits=c(0,2,2,3,0),
             caption="Interaction coefficients on each outcome measure, between the media treatment and left-
                      wing (baseline category) versus right-wing voting intention (Study 1),
                      leaving out respondents that gave the top answer(s) on that outcome variable in Wave 1.",
             label="tab:disruption_noceiling_pol"),
      file="tables/disruption_noceiling_pol.tex", caption.placement = "top")

#for salience, among left-wingers, the treatment is very good at causing them to mention climate if they hadn't so far
tapply(d$SalienceClim_any_w2[d$FT_next_wing=="left"],
       list(d$Treated[d$FT_next_wing=="left"], d$SalienceClim_any[d$FT_next_wing=="left"]), mean, na.rm=T)

#but why do we see an effect on right-wing respondents when we don't leave out wave 1 already-mentioners?
tapply(d$SalienceClim_any_w2[d$FT_next_wing=="right"],
       list(d$Treated[d$FT_next_wing=="right"], d$SalienceClim_any[d$FT_next_wing=="right"]), mean, na.rm=T)
#it's because the treatment causes right-wing respondents who already mentioned climate change to keep mentioning it

#percent salience (wave 2) among left and right wing respondents, treated and untreated
round(tapply(d$SalienceClim_any_w2, list(d$Treated, d$FT_next_wing), mean, na.rm=T), 2)


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
             treatment and respondents' wave 1 (pre-treatment) attitude on the same outcome (Study 1).",
             label="tab:disruption_bywave1"),
      file="tables/disruption_bywave1.tex", caption.placement = "top")

#interaction effects on salience by relevant wave 1 outcomes
moderators <- c(2,3,4,6)
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
             treatment and respondents' wave 1 (pre-treatment) attitude on the each outcome (Study 1).",
             label="tab:disruption_salience_bywave1"),
      file="tables/disruption_salience_bywave1.tex", caption.placement = "top")


##Appendix I: regression tables for heterogeneous effects by gender and age

#interactions by gender
ints <- lapply(outcome_cols, function(outcome){
  formula <- as.formula(paste0(outcome, "_diff ~  Treated*gender"))
  fit <- lm(formula, data=d)
  int <- tail(summary(fit)$coefficients[,c(1,2,4)], 1)
  n <- nobs(fit)
  int <- cbind(int, n)
})
ints <- do.call(rbind, ints)
rownames(ints) <- outcome_names
print(xtable(ints, digits=c(0,2,2,3,0),
             caption="Interaction coefficients between gender and the media
             treatment on each outcome measure (Study 1). Reference category is
             female.",
             label="tab:disruption_bygender"),
      file="tables/disruption_bygender.tex", caption.placement = "top")

#interactions by age (in decades)
d$age10 <- d$age/10
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
             treatment on each outcome measure (Study 1). Age is in decades
             (i.e. divided by 10).",
             label="tab:disruption_byage"),
      file="tables/disruption_byage.tex", caption.placement = "top")

#investigation of salience effect gender differential
tapply(d$SalienceClim_any_diff, list(d$gender, d$Treated), mean) #treatment effects
tapply(d$SalienceClim_any, list(d$gender, d$Treated), mean) #starting points

#investigation of behavior effect gender differential
tapply(d$Behavior_diff, list(d$gender, d$Treated), mean, na.rm=T) #treatment effects
tapply(d$Behavior, list(d$gender, d$Treated), mean, na.rm=T) #starting points
