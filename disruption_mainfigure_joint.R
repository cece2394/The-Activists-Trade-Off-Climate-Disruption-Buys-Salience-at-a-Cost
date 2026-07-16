library(ggplot2)

#get estimates from both studies
est_CI1 <- read.csv("main_estimates_CIs.csv")
est_CI2 <- read.csv("main_estimates_CIs2.csv")
est_CI <- rbind(est_CI1, est_CI2)

#rename outcomes to match
est_CI[c(2,7) ,"outcome"] <- "Policy message"

#ordering of outcomes (already in correct order)
est_CI$outcome <- factor(est_CI$outcome, levels=unique(est_CI$outcome))

#plot
pdf(paste0("figures/disruption_main_effects_joint.pdf"), 10, 5)
print(ggplot(est_CI, aes(x=outcome, y=est, color=study)) + 
        geom_hline(yintercept=0, color="dark grey") +
        geom_errorbar(aes(ymin=CI_lo, ymax=CI_hi), width=.1, position=position_dodge(.2)) +
        geom_point(position=position_dodge(.2)) +
        ylab(paste0("Effect of disruption treatment")) +
        theme_bw() +
        theme(axis.title.x=element_blank(),
              axis.title.y=element_text(size=15),
              axis.text=element_text(size=15),
              legend.position="bottom",
              legend.title=element_blank(),
              legend.text=element_text(size=13)))
dev.off()