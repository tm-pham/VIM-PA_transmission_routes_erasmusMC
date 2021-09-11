suffix <- "_nonoutbreak"
folder <- paste0("/Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/Combacte/RotterdamProject/data/MCMC_data_with_VIM_SDD", suffix,"/")
admF1 <- paste0("admDates_with_VIM_SDD",suffix ,"_1.txt")
admF2 <- paste0("admDates_with_VIM_SDD",suffix ,"_2.txt")
cultF1 <- paste0("cultureresults_with_VIM_SDD",suffix,"_1.txt")
cultF2 <- paste0("cultureresults_with_VIM_SDD", suffix, "_2.txt")

adm_df1 <- read.table(paste0(folder, admF1), header=F, sep=",")
adm_df2 <- read.table(paste0(folder, admF2), header=F, sep=",")
cult_df1 <- read.table(paste0(folder, cultF1), header=F, sep=",")
cult_df2 <- read.table(paste0(folder, cultF2), header=F, sep=",")


(max_date_1 <- max(adm_df1[,2]))
(max_id_1 <- max(unique(adm_df1[,1])))
adm_df2[,2] <- adm_df2[,2] + max_date_1 + 1
cult_df2[,2] <- cult_df2[,2] + max_date_1 + 1
adm_df2[,1] <- adm_df2[,1] + max_id_1 + 1
cult_df2[,1] <- cult_df2[,1] + max_id_1 + 1

adm_df <- rbind(adm_df1, adm_df2)
cult_df <- rbind(cult_df1, cult_df2)

write.table(adm_df, file=paste0(folder, "admDates_with_VIM_SDD", suffix, ".txt"), 
            row.names = F, col.names = F, sep = ",")
write.table(cult_df, file=paste0(folder, "cultureresults_with_VIM_SDD", suffix, ".txt"), 
            row.names = F, col.names = F, sep = ",")

