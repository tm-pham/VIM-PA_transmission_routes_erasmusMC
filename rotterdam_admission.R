# ============================================================================ #
# Rotterdam data
# ----------------------------------------------------------------------------
# Admission data
# ============================================================================ #
source("rotterdam_path_packages.R")
source("rotterdam_functions.R")
source("rotterdam_vars.R")

# ============================================================================ #
# Read in admission data (tibble)
admissionData <- data_list[[1]]
# admissionData <- read_excel(paste0(dataPath,"admissionData_ICU_2008-2018_ErasmusMC_raw.xlsx"))
admFile <- admissionData[,c("Patientnumber","VIM_positive","Studynumber","gender","ward","StartDate", "enddate","roomcode")]

# Change column names
colnames(admFile) <- c("pID", "vim","admID","gender", "ward", "admDate", "disDate","room")
admFile <- admFile[order(admFile$admDate),]

# Add LOS in hours
admFile$LOS_hr <- as.numeric(difftime(admFile$disDate, admFile$admDate, units="hours"))
admFile$LOS <- as.numeric(ceiling(admFile$LOS_hr/24))

# Divide into wards
admFile$ward[admFile$ward%in%ward_1] <- 1
admFile$ward[admFile$ward%in%ward_2] <- 2
admFile$ward <- as.factor(admFile$ward)

admFile_1 <- admFile[which(admFile$ward==1), ] 
admFile_2 <- admFile[which(admFile$ward==2), ] 

vim_positive_pat <- unique(admFile[admFile$vim==1, "pID"])
