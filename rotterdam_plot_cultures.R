# ============================================================================ #
# Rotterdam data
# ----------------------------------------------------------------------------
# Plot culture results over time
# ============================================================================ #
rm(list=ls())

source("rotterdam_functions.R")      # Includes functions
source("rotterdam_path_packages.R")  # Loading R packages, includes file paths
source("rotterdam_vars.R")           # Includes all important variables (study period, eligible sites, ...)
source("rotterdam_admission.R")      # Admission data

load(file="../data/culturesDivided_with_VIM_SDD.RData")

cultureFile_1 <- cbind(culturesDivided$cult_1, ward=1)
cultureFile_2 <- cbind(culturesDivided$cult_2, ward=2)
cultureFile <- rbind(cultureFile_1, cultureFile_2)

pos_patID <- as.data.frame(unique(cultureFile[which(cultureFile$result==1),"pID"]))

# write.table(pos_patID, file="../data/pos_patID.csv", row.names = F, col.names = F)

# Data frame with only positive cultures
df <- cultureFile[cultureFile$result==1,] %>% group_by(samplingDate, ward) %>% dplyr::summarise(count=n())

df_first_pos <- cultureFile[cultureFile$result==1,] %>% group_by(pID) %>% dplyr::summarise(firstPosDate=min(samplingDate))

# Plot cultures per month
axis.title.y <- "Number of positive patients"
title <- "Number of positive cultures over time"
legend.title <- "VIM test"
legend.labels <- c("no", "yes")
axis.text.size=16
axis.text.size.x=16
axis.title.size=20

# ============================================================================ #
# Plot only VIM cultures
# ============================================================================ #
axis.title.y <- "Number of positive cultures"
title <- "Number of positive VIM PA cultures over time"

df_first_pos <- as.data.frame(cbind(df_first_pos, count=1))
cult <- df_first_pos %>% group_by(firstPosDate) %>% dplyr::summarise(sum=sum(count))

cult_plot <- ggplot(data=cult, aes(x=firstPosDate, y=sum)) + 
  geom_bar(stat="identity", position="stack") + 
  scale_x_date(labels = scales::date_format("%b %Y"),date_breaks="6 months") + 
  labs(y="Number of positive patients") +
  scale_fill_discrete(name = legend.title, labels = legend.labels) + 
  theme_bw() + 
  theme(axis.text.y=element_text(size=axis.text.size),
        axis.text.x = element_text(size=axis.text.size.x, angle=45, hjust=1),
        axis.title.y = element_text(size=axis.title.size),
        axis.title.x = element_blank(),
        plot.title= element_blank())
cult_plot
ggsave(cult_plot, file=paste0(figPath, "Fig2_pos_pat_over_time_combined.pdf"), width=16, height=9)


# Per month
cult_month <- df_first_pos %>% mutate(month=month(firstPosDate), year=year(firstPosDate)) %>% group_by(month,year) %>% dplyr::summarise(sum=n())
# cult_month <- df %>% mutate(month=month(samplingDate), year=year(samplingDate)) %>% group_by(month,year,ward) %>% summarise(sum=sum(count))
cult_month$date <- as.Date(paste(2021,cult_month$month,'01',sep="-"))
date_breaks <- seq(as.Date("2010-01-01", format="%Y-%m-%d"), as.Date("2018-12-31", format="%Y-%m-%d"), "month")
vim_month <- ggplot(data=cult_month, aes(x=date, y=sum)) + 
              facet_grid(~year) + 
              geom_bar(stat="identity", position="stack") + 
              scale_x_date(labels = scales::date_format("%b"), date_breaks="1 month") + 
              labs(y=axis.title.y, title = paste0(title, " (per month)")) +
              scale_fill_discrete(name = legend.title, labels = legend.labels) + 
              theme_bw() + 
              theme(axis.text.y=element_text(size=axis.text.size),
                    axis.text.x = element_text(size=13, angle=90, hjust=1),
                    axis.title.y = element_text(size=axis.title.size),
                    axis.title.x = element_blank(),
                    plot.title= element_blank(), 
                    strip.text = element_text(size=18))
vim_month
ggsave(vim_month, file=paste0(figPath,"vim_pos_pat_per_month_ward.pdf"), width=16, height=6)

# Per year
cult_year <- df %>% mutate(date=year(samplingDate)) %>% group_by(date, ward) %>% summarise(sum=sum(count))
cult_year$date <- as.Date(as.character(cult_year$date), format="%Y")
vim_year <- ggplot(data=cult_year, aes(x=date, y=sum)) + 
              facet_grid(ward~., scales="free") + 
              geom_bar(stat="identity", position="stack") + 
              scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
              labs(y=axis.title.y, title = paste0(title, " (per year)")) +
              scale_fill_discrete(name = legend.title, labels = legend.labels) + 
              theme_bw() + 
              theme(axis.text.y=element_text(size=axis.text.size),
                    axis.text.x = element_text(size=axis.text.size.x, angle=45, hjust=1),
                    axis.title.y = element_text(size=axis.title.size),
                    axis.title.x = element_blank(),
                    plot.title= element_text(hjust=0.5, size=22), 
                    strip.text = element_text(size=16))
vim_year
ggsave(vim_year, file=paste0(figPath,"vim_pos_cult_per_year_ward.pdf"), width=12, height=9)


# Plot both VIM and non-VIM cultures
p_month <- ggplot(data=cult_month, aes(x=date, y=sum, fill=as.factor(vim))) + 
            facet_grid(ward~year, scales="free") + 
            geom_bar(stat="identity", position="stack") + 
            scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
            labs(y=axis.title.y, title = paste0(title, " (per month)")) +
            scale_fill_discrete(name = legend.title, labels = legend.labels) + 
            theme_publication() + 
            theme(axis.text.y=element_text(size=axis.text.size),
                  axis.text.x = element_text(size=8, angle=90, hjust=1),
                  axis.title.y = element_text(size=axis.title.size),
                  axis.title.x = element_blank(),
                  plot.title= element_text(hjust=0.5, size=22), 
                  strip.text = element_text(size=16))
p_month
ggsave(p_month, file=paste0(figPath,"pos_cult_per_month_ward.pdf"), width=12, height=9)


p_year <- ggplot(data=cult_year, aes(x=date, y=sum, fill=as.factor(vim))) + 
  facet_grid(ward~., scales="free") + 
  geom_bar(stat="identity", position="stack") + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
  labs(y=axis.title.y, title = paste0(title, " (per year)")) +
  scale_fill_discrete(name = legend.title, labels = legend.labels) + 
  theme_publication() + 
  theme(axis.text.y=element_text(size=axis.text.size),
        axis.text.x = element_text(size=axis.text.size.x, angle=45, hjust=1),
        axis.title.y = element_text(size=axis.title.size),
        axis.title.x = element_blank(),
        plot.title= element_text(hjust=0.5, size=22), 
        strip.text = element_text(size=16))
p_year
ggsave(p_year, file=paste0(figPath,"pos_cult_per_year_ward.pdf"), width=12, height=9)
