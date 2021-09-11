# =========================================================================== #
# Analysis of Rotterdam data (MCMC)
# =========================================================================== #
source("rotterdam_path_packages.R", echo=F)
source("rotterdam_functions.R", echo=F)

version <- "with_VIM_SDD"
folder <- c("Outbreak","Nonoutbreak")
wards <- c("outbreak","nonoutbreak")
suffix <- "outbreakVSnonoutbreak"

labels_res <- c(alpha="alpha", 
                beta="beta", 
                alphaContr="Background transmission", 
                betaContr="Cross-transmission", 
                f="Importation probability", 
                meanPrev="Mean prevalence", 
                phi="Test sensitivity")
labels_routes <- c(background="Background transmission", 
                   crossT="Cross-transmission",
                   nImp = "Importations")
df_res <- NULL
for(i in 1:length(wards)){
  load(file=paste0(resPath, version,"/", folder[i], "/res_",version, "_", wards[i],".RData"))
  res <- cbind(res, scenario=folder[i])
  df_res <- rbind(df_res, res)
} 

stat.summary(res[res$scenario=="Nonoutbreak",],precision=4)
stat.summary(res[res$scenario=="Outbreak",],precision=4)


df_res <- df_res[, c("i","scenario","alpha","beta","alphaContr","betaContr","f","meanPrev","phi","nImp","acq")]
df_res_long <- melt(df_res, id=c("i", "scenario"))
df_res_long$scenario <- factor(df_res_long$scenario, levels=c("Outbreak", "Nonoutbreak"))


# Compare transmission rates
rate_plot <- ggplot(data=df_res_long[df_res_long$variable%in%c("alpha"),],aes(x=variable, y=value, fill=variable)) + 
  facet_wrap(~scenario) +
  geom_bar(stat="summary", fun='mean', width=0.6, fill=colors[1]) +
  geom_errorbar(stat="summary", 
                fun.min=percentile(2.5),
                fun.max=percentile(97.5), width=0.1) + 
  labs(y="") + 
  theme_publication() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave(rate_plot, file=paste0(figPath, "background_rate_outbreakVSnonoutbreak.pdf"), width=12, height=9)
# Bar plots with mean and percentiles
contr_plots <- ggplot(data=df_res_long[df_res_long$variable%in%c("alphaContr","betaContr"),], 
                      aes(x=variable, y=100*value, fill=variable)) + 
  facet_wrap(~scenario, labeller=labeller(variable=labels_routes[c(1,2)])) + 
  geom_bar(stat="summary", fun='mean', width=0.6, fill=c(colors, colors)) +
  geom_errorbar(stat="summary", 
                fun.min=percentile(2.5),
                fun.max=percentile(97.5), width=0.1) + 
  labs(y="Percentage (%)") + 
  scale_x_discrete(limits = (levels(plot_res_long$variable)[c(3,4)]),
                   labels=labels_res[c(3,4)]) +
  scale_y_continuous(limit=c(0,100)) +
  # coord_flip() +
  theme_publication() + 
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")
contr_plots
ggsave(contr_plots, file = paste0(figPath, "contr_barplots_", version, "_",suffix,".pdf"), width=16, height=9)

# Plot of importation probability for both wards
imp_plots <- ggplot(df_res_long[df_res_long$variable=="f",], aes(x=variable, y=100*value)) + 
  facet_wrap(~scenario) +
  geom_bar(stat="summary", fun='mean', width=0.6, fill="grey") +
  geom_errorbar(stat="summary", 
                fun.min=percentile(2.5),
                fun.max=percentile(97.5), width=0.1) + 
  labs(y="Percentage (%)") + 
  theme_publication() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
imp_plots
ggsave(imp_plots, file = paste0(figPath, "imp_barplots_", version,"_",suffix, ".pdf"), width=12, height=9)

# Plot of test sensitivity for both wards
sens_plots <- ggplot(df_res_long[df_res_long$variable=="phi",], aes(x=variable, y=100*value)) + 
  facet_wrap(~scenario) +
  geom_bar(stat="summary", fun='mean', width=0.6, fill="grey") +
  geom_errorbar(stat="summary", 
                fun.min=percentile(2.5),
                fun.max=percentile(97.5), width=0.1) + 
  labs(y="Percentage (%)") + 
  theme_publication() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
sens_plots
ggsave(sens_plots, file = paste0(figPath, "sens_barplots_", version, "_",suffix, ".pdf"), width=12, height=9)


# Plot of mean prevalence (#cases/#patients) for both wards
prev_plots <- ggplot(df_res_long[df_res_long$variable=="meanPrev",], aes(x=variable, y=100*value)) + 
  facet_wrap(~scenario) +
  geom_bar(stat="summary", fun='mean', width=0.6, fill= "grey") +
  geom_errorbar(stat="summary", 
                fun.min=percentile(2.5),
                fun.max=percentile(97.5), width=0.1) + 
  labs(y="Percentage (%)") + 
  theme_publication() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
prev_plots
ggsave(prev_plots, file = paste0(figPath, "prev_barplots_", version, "_", suffix, ".pdf"), width=12, height=9)


# Plot absolute number of importations and acquisitions for both wards
df_imp_acq <- df_res[,c("i","scenario","nImp", "acq")]
df_imp_acq_long <- melt(df_imp_acq, id=c("i","scenario"))
labels_imp_acq <- c(nImp="Importations", 
                    acq="Acquisitions")
df_imp_acq_long$variable <- factor(df_imp_acq_long$variable, levels=c("acq", "nImp"))
df_imp_acq_long$scenario <- factor(df_imp_acq_long$scenario, levels=c("Outbreak","Nonoutbreak"))

plot_imp_acq <- ggplot(data=df_imp_acq_long, aes(x=variable, y=value)) + 
  facet_wrap(~scenario) +
  geom_bar(stat="summary", fun='mean', width=0.6, fill="grey") +
  geom_errorbar(stat="summary", 
                fun.min=percentile(2.5),
                fun.max=percentile(97.5), width=0.1) + 
  labs(y="Total number") + 
  scale_y_continuous(breaks=seq(0,max(df_imp_acq_long$value), by = 10)) +
  scale_x_discrete(labels=labels_imp_acq) +
  # coord_flip() + 
  theme_publication() + 
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size=20))
plot_imp_acq 
ggsave(plot_imp_acq, file = paste0(figPath, "import_acq_barplots_", version, "_", suffix, ".pdf"), width=16, height=9)

# Percentage
total <- df_imp_acq$nImp + df_imp_acq$acq
df_imp_acq$nImp <- 100*df_imp_acq$nImp/total
df_imp_acq$acq <-  100*df_imp_acq$acq/total
df_imp_acq_long <- melt(df_imp_acq, id=c("i","scenario"))
df_imp_acq_long$variable <- factor(df_imp_acq_long$variable, levels=c("acq", "nImp"))
df_imp_acq_long$scenario <- factor(df_imp_acq_long$scenario, levels=c("Outbreak","Nonoutbreak"))

plot_imp_acq_perc <- ggplot(data=df_imp_acq_long, aes(x=variable, y=value)) + 
  facet_wrap(~scenario) +
  geom_bar(stat="summary", fun='mean', width=0.6, fill="grey") +
  geom_errorbar(stat="summary", 
                fun.min=percentile(2.5),
                fun.max=percentile(97.5), width=0.1) + 
  labs(y="Percentage (%)") + 
  scale_y_continuous(breaks=seq(0,max(df_imp_acq_long$value), by = 20)) +
  scale_x_discrete(labels=labels_imp_acq) +
  # coord_flip() + 
  theme_publication() + 
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size=20))
plot_imp_acq_perc 
ggsave(plot_imp_acq_perc, file = paste0(figPath, "import_acq_perc_barplots_", version, "_", suffix, ".pdf"), width=16, height=9)


# Plot number of positive patients divided into importations and acquisitions
df_routes <- df_res[,c("i","scenario","alphaContr", "betaContr", "nImp", "acq")]
df_routes$background <- df_routes$alphaContr*df_routes$acq
df_routes$crossT <- df_routes$betaContr*df_routes$acq
df_routes <- df_routes[,c("i","scenario","background", "crossT", "nImp")]
df_routes_long <- melt(df_routes, id=c("i", "scenario"))
labels_routes <- c(background="Background transmission", 
                   crossT="Cross-transmission",
                   nImp = "Importations")
df_routes_long$variable <- factor(df_routes_long$variable, levels=c("background", "crossT", "nImp"))
df_routes_long$scenario <- factor(df_routes_long$scenario, levels=c("Outbreak","Nonoutbreak"))

plot_routes <- ggplot(data=df_routes_long, aes(x=variable, y=value, fill=variable)) + 
  facet_wrap(~scenario) +
  geom_bar(stat="summary", fun='mean', width=0.6, fill=c(colors, "grey", colors, "grey")) +
  geom_errorbar(stat="summary", 
                fun.min=percentile(2.5),
                fun.max=percentile(97.5), width=0.1) + 
  labs(y="Total number") + 
  scale_x_discrete(limits = (levels(df_routes_long$variable)),
                   labels=labels_routes) +
  # coord_flip() +
  theme_publication() + 
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")
plot_routes
ggsave(plot_routes, file = paste0(figPath, "routes_barplots_", version, "_", suffix,".pdf"), width=16, height=9)


