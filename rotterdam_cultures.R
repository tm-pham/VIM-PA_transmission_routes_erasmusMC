# ============================================================================ #
# Rotterdam data
# ----------------------------------------------------------------------------
# Culture results
# ============================================================================ #
rm(list=ls())

source("rotterdam_functions.R")      # Includes functions
source("rotterdam_path_packages.R")  # Loading R packages, includes file paths
source("rotterdam_vars.R")           # Includes all important variables (study period, eligible sites, ...)
source("rotterdam_admission.R")      # Admission data

include_sdd <- 1; include_VIM <- 1
if(include_VIM){
  suffix <- "_with_VIM_SDD"
  if(!include_sdd) suffix <- "_only_VIM"
}else{
  if(!include_sdd) print("Error. No cultures would be included.")
  suffix <- "_only_SDD"
}
if(outbreak_period) suffix <- paste0(suffix, "_outbreak")
if(non_outbreak_period&&!outbreak_period) suffix <- paste0(suffix, "_nonoutbreak")
print(suffix)

# Read in culture results (tibble)
cultureResults <- data_list[[2]]
# cultureResults <- read_excel(paste0(dataPath, "cultureResults_ICU_2008-2018_ErasmusMC_raw.xlsx"))
cultFile <- cultureResults[,c("Patientnumber", "ExaminationId", "AfnDat", "Mat","ExaminationType", "Organisme")]
colnames(cultFile) <- c("pID", "examinationId","samplingDate", "site","examinationType", "organism")

# =========================== #
# Process dates
# =========================== #
# Convert dates 
admFile$admDate <- as.Date(admFile$admDate, format="%Y-%m-%d")
admFile$disDate <- as.Date(admFile$disDate, format="%Y-%m-%d")
cultFile$samplingDate <- as.Date(cultFile$samplingDate, format="%Y-%m-%d")

# =========================== #
# Check whether admission dates
# of patients are in study period
# If not, change admission date 
# to start of study period
# =========================== #
indPatPeriod <- intersect(which(admFile$admDate<startDate),which(admFile$disDate>=startDate))
length(indPatPeriod) # 23 patients have admission dates before start of study period
admFile[indPatPeriod,"admDate"] <- startDate

# =========================== #
# Check whether discharge dates
# of patients are in study period
# If not, change discharge date 
# to end of study period
# =========================== #
indPatDisPeriod <- intersect(which(admFile$disDate>endDate),which(admFile$admDate>=startDate))
length(indPatDisPeriod) # 4734 patients have admission dates before start of study period
admFile[indPatDisPeriod,"disDate"] <- endDate

# Use data only from study period
admFile <- admFile[intersect(which(admFile$admDate>=startDate), which(admFile$disDate<=endDate)), ]
admFile <- admFile[admFile$admDate<=endDate,]
cultFile <- cultFile[intersect(which(cultFile$samplingDate>=startDate), which(cultFile$samplingDate<=endDate)), ]

# Remove patients where admission date = discharge date
inclPatDate <- which(admFile$admDate!=admFile$disDate)
if(length(inclPatDate)>0){
  admFile <- admFile[inclPatDate, ]
  inclPatCult <- which(cultFile$pID%in%admFile$pID)
  if(length(inclPatCult)>0){
    cultFile <- cultFile[inclPatCult,]
  }
}

# Number of admitted patients
print(paste0("Number of admitted patients in both wards: ", length(unique(admFile$pID))))

# Number of patients with culture results
print(paste0("Number of patients with culture results (both wards): ", length(unique(cultFile$pID))))

# Number of admitted patients without culture results
print(paste0("Number of admitted patients without culture results: ", sum(!unique(admFile$pID)%in%unique(cultFile$pID))))

# =========================== #
# Choose culture sites
# Only Throat and Rectum 
# samples are considered
# =========================== #
# Use culture results for eligible sites
print(paste0("Valid sites: ")); validSites
cultFile <- cultFile[cultFile$site%in%validSites,]

# =========================== #
# Choose examination types
# =========================== #
# Examination Type
print(paste0("Valid examination types: ")); validExamTypes
examType <- unique(cultFile$examinationType)
examType <- examType[order(!examType%in%validExamTypes)]

# All examination types ordered (valid examination types at the top)
examType_filename <- paste0(dataPath, "ErasmusMC_ICU_2010_2018_examination_type.csv")
if(!file.exists(examType_filename)){
  print("Writing table with examination types in file.")
  write.table(as.data.frame(cbind(examinationType=examType)), 
              file=examType_filename, 
              row.names=F, col.names = F)
}

# Further filter for valid examination types
# Depending on whether only VIM, or including SDD swabs
if(include_VIM){
  cultFile <- cultFile[cultFile$examinationType%in%validExamTypes,]
  if(!include_sdd) cultFile <- cultFile[cultFile$examinationType%in%c("VIM Resistentiekweek","VIM screening PCR"),]
}else{
  validExamTypes <- c("SDD statuskweek", "SDD inventarisatiekweek")
  cultFile <- cultFile[cultFile$examinationType%in%validExamTypes,]
}
print("Final examination types: "); unique(cultFile$examinationType)

# How many culture results from VIM screening
print("Percentage of results per each examination type:"); 100*table(cultFile$examinationType)/sum(table(cultFile$examinationType))

# =========================== #
# Convert positive results
# =========================== #
# Checks
unique(cultFile[str_detect(cultFile$organism, regex("pos", ignore_case = T)),"organism"])
unique(cultFile[str_detect(cultFile$organism, regex("pseudomonas aeruginosa", ignore_case = T)),"organism"])
unique(cultFile[str_detect(cultFile$organism, regex("p. aeruginosa", ignore_case = T)),"organism"])
unique(cultFile[str_detect(cultFile$organism, regex("p. a.", ignore_case = T)),"organism"])

# Positive results
cultFile$result <- ifelse(cultFile$organism%in%posRes, 1, 0)

# Save table with positive results that do not include the word Pseudomonas aeruginosa
posTab <- cultFile[cultFile$organism%in%c("positief","Positief", "POS","PCR POS"),]
# write.table(posTab, file=paste0(dataPath, "cultFile_pos_unsure.csv"), sep=",", row.names=F)
# Feedback: all of unsure positive results are positive

# Negative results
# Exclude any cultures that contain "gist"
# cultFile <- cultFile[!str_detect(cultFile$organism, regex("gist", ignore_case=T)),]
# unique(cultFile[str_detect(cultFile$organism, regex("geen", ignore_case=T)),"organism"])
# unique(cultFile[str_detect(cultFile$organism, regex("neg", ignore_case=T)),"organism"])
# cultFile <- cultFile[cultFile$organism%in%c(posRes,negRes),]

pos_cultures <- cultFile[cultFile$result==1, ]
# write.table(cultFile[cultFile$result==1, ], file=paste0("../data/pos_cultures_to_be_checked.csv"), sep=",", row.names = F)
pos_checked <- read_excel(paste0(dataPath, "pos_cultures_checked_at_EMC_20210225.xlsx"))

cultures2010 <- read_excel(paste0(dataPath, "20210325_positive_cultures_2010.xlsx"))
cultures2010$result <- ifelse(cultures2010$posneg=="pos.", 1, 0)
examID2010 <- cultures2010$ExaminationID

cultFile2010 <- cultFile[F, ]
for(i in 1:nrow(cultures2010)){
  temp <- cultFile[cultFile$examinationId==unlist(cultures2010[i, "ExaminationID"]) & cultFile$site==unlist(cultures2010[i, "samplesite"]),]
  temp$result <- unlist(cultures2010[i, "result"])
  cultFile2010 <- rbind(cultFile2010, temp)
}
cultFile2010 <- cultFile2010 %>% group_by(pID, site, samplingDate) %>% slice(which.max(result))
cultFile2010$VIM <- ifelse(cultFile2010$result==1, "MBLPOS", NA)

if(nrow(cultFile2010)!=length(unique(examID2010))) print("Discrepancy!")

# Add cultures from 2010 to pos_checked
pos_checked <- rbind(pos_checked, cultFile2010)
pos_checked <- pos_checked[order(pos_checked$samplingDate),]

# =========================== #
# Remove patients according
# to checked positive results
# =========================== #
# Number of excluded cultures because of examination types
rem_pos_checked <- pos_checked[!pos_checked$examinationType%in%validExamTypes, ]
nrow(rem_pos_checked)
length(unique(rem_pos_checked$pID))
# Number of excluded cultures because of culture sites 
rem_sites <- pos_checked[!pos_checked$site%in%validSites, ]
nrow(rem_sites)
length(unique(rem_sites$pID))
# Remove cultures according to examination type and culture site
pos_checked <- pos_checked[pos_checked$examinationType%in%validExamTypes, ]
pos_checked <- pos_checked[pos_checked$site%in%validSites, ]

# Further exclude the following patients because the sample was not from the ICU
exclPatId <- c(5787, 7187, 7280, 1038, 4179, 4263, 6169, 7626)
pos_checked <- pos_checked[!pos_checked$pID%in%exclPatId,]

# Add patient that was included in the new VIM file but not in the original data set
new_check <- read_excel(paste0(dataPath, "Ps_aeruginosa_IC_clean_Mui.xlsx"))
addCult <- new_check[new_check$Patientnumber==8321,c("Patientnumber","ExaminationId","AfnDat","Mat","ExaminationType","Organisme", "VIM")]
colnames(addCult) <- colnames(cultFile)
addCult$result <- ifelse(!is.na(addCult$result), 1, 0)
addCult$samplingDate <- as.Date(addCult$samplingDate, format="%Y-%m-%d")
# Add only if the culture results is within the study period
if(addCult$samplingDate>=startDate&&addCult$samplingDate<=endDate){
  cultFile <- rbind(cultFile, addCult)
}


# Include only those patients that were positive in the new VIM file 
examId_include <- unlist(pos_checked[!is.na(pos_checked$VIM),"examinationId"])
cultFile <- cultFile[cultFile$examinationId%in%examId_include||cultFile$result==0,]
cultFile[!cultFile$examinationId%in%examId_include, "result"] <- 0


# Number of patients with positive VIM result
print(paste0("Number of patients with positive VIM result: ", length(unique(cultFile[cultFile$result==1, "pID"]))))
print(paste0("Ward 1: ", sum(unlist(unique(cultFile[cultFile$result==1, c("pID")]))%in%admFile_1$pID)))
print(paste0("Ward 2: ", sum(unlist(unique(cultFile[cultFile$result==1, c("pID")]))%in%admFile_2$pID)))



# Save patient numbers with positive VIM result
# write.table(unique(cultFile[cultFile$result==1, "pID"]), 
#             file=paste0(dataPath, "VIM_positive_patientIDs.csv"),
#             row.names = F)

# Remove redundant test results from other sites (take the maximum value for each day)
cultFile <- cultFile %>% group_by(pID, samplingDate) %>% slice(which.max(result))

# Number of positive results per examination type
# Most are VIM, only 5 are SDD
table(cultFile[cultFile$result==1, "examinationType"])

# =========================== #
# Check whether patients
# with cultures have admission dates
# =========================== #
# Remove culture entries of patients that do not have an admission entry
removePat <- unique(cultFile$pID)[which(!unique(cultFile$pID)%in%unique(admFile$pID))]
length(removePat)
admissionData[admissionData$Patientnumber%in%removePat,]
indRemovePat <- which(cultFile$pID%in%removePat)
if(length(indRemovePat)>0) cultFile <- cultFile[-indRemovePat,]


cultFile <- cultFile[cultFile$site%in%validSites,]

# =========================== #
# Check false-negatives
# =========================== #
# False-negative results
if(length(validSites)==1){
  (FN_filename <- paste0("../data/falsenegatives", suffix,"_",validSites, ".RData"))
}else (FN_filename <- paste0("../data/falsenegatives", suffix, ".RData"))

if(!file.exists(FN_filename)){
  FN <- falsenegative(cultFile, admFile)
  # View(cultFile[cultFile$pID%in%FN$patFN,])
  save(FN, file=FN_filename)
}else load(file=FN_filename)
print(paste0("False negative results before disregarding negative results after a positive result: ", FN$FN))
# 137 false-negative results
# 29 patients with false-negative results

# Disregard false-negative results after a positive result
# Reason: Positive patients get antibiotics and their bacterial load is suppressed
for(p in FN$patFN){
  indPat <- which(cultFile$pID==p)
  indPos <- which(cultFile[cultFile$pID==p,"result"]==1)
  deleteRows <- indPat[-indPos]
  cultFile <- cultFile[-deleteRows,]
}
FN <- falsenegative(cultFile, admFile)
save(FN, file=FN_filename)
print(paste0("False negative results after disregarding negative results after a positive result: ", FN$FN))


# How many VIM screening cultures are positive
table(cultFile[str_detect(cultFile$examinationType, regex("vim", ignore_case = T)),"result"])
# Proportions VIM negative among general negatives, VIM positive among general positives
table(cultFile[str_detect(cultFile$examinationType, regex("vim", ignore_case = T)),"result"])/table(cultFile$result)

# How many positive culture results in general 
table(cultFile$result)

# Make variable that represents whether VIM/no VIM
cultFile$vim <- as.numeric(str_detect(cultFile$examinationType, regex("vim", ignore_case = T)))

# Divide culture results into two wards
cultures <- unique(cultFile[, c("pID", "samplingDate","vim","result")])

# Number of patients without cultures
sum(!unique(admissionData$pID)%in%cultures$pID)


if(length(validSites)==1){
  (cultDivided_filename <- paste0("../data/culturesDivided", suffix,"_",validSites, ".RData"))
}else (cultDivided_filename <- paste0("../data/culturesDivided", suffix, ".RData"))

if(!file.exists(cultDivided_filename)){
    culturesDivided <- divide.cultures(admFile, cultures, 1, 2)
    save(culturesDivided, file=cultDivided_filename)
}else load(file=cultDivided_filename)

cultureFile_1 <- cbind(culturesDivided$cult_1, ward=1)
cultureFile_2 <- cbind(culturesDivided$cult_2, ward=2)

(FN_1 <- falsenegative(cultureFile_1, admFile_1))
(FN_2 <- falsenegative(cultureFile_2, admFile_2))

sum(cultureFile_1$result)
sum(cultureFile_2$result)

# Patient ids with at least one positive culture result
length(unique(cultureFile_1[cultureFile_1$result==1, "pID"]))
length(unique(cultureFile_2[cultureFile_2$result==1, "pID"]))

# Convert dates into numerics 
admissionData <- admFile
admissionData$admDate <- as.numeric(as.Date(admFile$admDate, format="%Y-%m-%d"))
admissionData$disDate <- as.numeric(as.Date(admFile$disDate, format="%Y-%m-%d"))
admissionData$ward <- as.factor(ifelse(admFile$ward==1, 1, 2)) 

admissionData_1 <- admissionData[admissionData$ward==1, ]
admissionData_2 <- admissionData[which(admissionData$ward==2), ] 
head(admissionData_1); head(admissionData_2)

length(unique(admissionData_1$pID)) 
length(unique(admissionData_2$pID))
length(unique(admissionData_1$pID)) + length(unique(admissionData_2$pID))

# ============================================================================ #
# Readmissions
# Both wards combined
readmData <- readmissions(admissionData, cultures)
admData <- as.data.frame(readmData$admFile)
cultFile <- as.data.frame(unique(readmData$cultFile))
mapId <- as.data.frame(readmData$mapId)
length(readmData$combineId)
length(readmData$readmId)
length(unique(mapId$original))
length(unique(mapId$changed)) + length(readmData$readmId)
length(unique(admData$pID))

# Each ward
# Every new admission is a new patient
readmData <- readmissions(admissionData_1, cultureFile_1)
admData_1 <- as.data.frame(readmData$admFile)
cultFile_1 <- as.data.frame(unique(readmData$cultFile))
mapId_1 <- as.data.frame(readmData$mapId)
length(readmData$combineId)
length(readmData$readmId)
# Number of patients
length(unique(mapId_1$original))
length(unique(mapId_1$changed)) + length(readmData$readmId)
length(unique(admData_1$pID))

readmData <- readmissions(admissionData_2, cultureFile_2)
admData_2 <- as.data.frame(readmData$admFile)
cultFile_2 <- as.data.frame(unique(readmData$cultFile))
mapId_2 <- readmData$mapId
# Number of patients
length(unique(mapId_2[,"original"]))
length(unique(mapId_2[,"changed"]))
length(unique(admData_2$pID))

length(unique(admData_1$pID)) + length(unique(admData_2$pID))
length(unique(mapId_1$changed)) + length(unique(mapId_2[,"changed"]))

# Check (should be equal)
if(length(admData_1$pID) != length(unique(admData_1$pID))) print("There are still readmissions in the data")
if(length(admData_2$pID) != length(unique(admData_2$pID))) print("There are still readmissions in the data")

# ============================================================================ #
# Create data for MCMC
# ============================================================================ #
admDates_1 <- as.data.frame(create.admission.file(admData_1))
admDates_2 <- as.data.frame(create.admission.file(admData_2))

cultRes_1 <- cultFile_1[,c("pID","samplingDate", "result")]; cultRes_1$samplingDate <- as.numeric(cultRes_1$samplingDate)
cultRes_2 <- cultFile_2[,c("pID","samplingDate", "result")]; cultRes_2$samplingDate <- as.numeric(cultRes_2$samplingDate)
head(admDates_1); head(admDates_2)
head(cultRes_1); head(cultRes_2)

# Check whether all patients with culture results have admission results
which(!unique(cultRes_1$pID)%in%unique(admDates_1$pID))
which(!unique(cultRes_2$pID)%in%unique(admDates_2$pID))

# Check whether the dates of the culture results are within admission dates
check.cult.adm(admData_1, cultFile_1)
check.cult.adm(admData_2, cultFile_2)

# Relabel patient IDs
relabeled_1 <- relabelpatientID(admDates_1, cultRes_1, mapId_1)
admDates_1 <- relabeled_1$admDates
cultRes_1 <- relabeled_1$cultRes
mapTable_1 <- relabeled_1$mapTable

relabeled_2 <- relabelpatientID(admDates_2, cultRes_2, mapId_2)
admDates_2 <- relabeled_2$admDates
cultRes_2 <- relabeled_2$cultRes
mapTable_2 <- relabeled_2$mapTable

# Relabel dates
relabelDates_1 <- relabeldates(admDates_1, cultRes_1)
admDates_1 <- relabelDates_1$admDates; cultRes_1 <- unique(relabelDates_1$cultRes)
admDates_1 <- admDates_1[order(admDates_1$pID),]

relabelDates_2 <- relabeldates(admDates_2, cultRes_2)
admDates_2 <- relabelDates_2$admDates; cultRes_2 <- unique(relabelDates_2$cultRes)
admDates_2 <- admDates_2[order(admDates_2$pID),]

# Take only row with maximum result on same day
cultRes_1 <- cultRes_1 %>% group_by(pID, samplingDate) %>% slice(which.max(result))
cultRes_2 <- cultRes_2 %>% group_by(pID, samplingDate) %>% slice(which.max(result))

cultFile_1 <- cultFile_1 %>% group_by(pID, samplingDate) %>% slice(which.max(result))
cultFile_2 <- cultFile_2 %>% group_by(pID, samplingDate) %>% slice(which.max(result))

# False-negative results
(FN_1 <- falsenegative(cultRes_1, admDates_1))
(FN_2 <- falsenegative(cultRes_2, admDates_2))

# Number of positive results
sum(cultRes_1$result)
sum(cultRes_2$result)

# Order admission dates
admDates_1 <- admDates_1%>% group_by(pID) %>% arrange(day)
admDates_2 <- admDates_2%>% group_by(pID) %>% arrange(day)

admDates_1 <- lapply(unique(admDates_1$pID), function(p) admDates_1[admDates_1$pID==p,])
admDates_1 <- do.call("rbind", admDates_1)
admDates_2 <- lapply(unique(admDates_2$pID), function(p) admDates_2[admDates_2$pID==p,])
admDates_2 <- do.call("rbind", admDates_2)

c1 <- lapply(unique(cultRes_1$pID), function(p) cultRes_1[cultRes_1$pID==p,])
cultRes_1 <- do.call("rbind", c1)
c2 <- lapply(unique(cultRes_2$pID), function(p) cultRes_2[cultRes_2$pID==p,])
cultRes_2 <- do.call("rbind", c2)



# ============================================================================ #
# Save data to file 
# ============================================================================ #
# scenario <- "_with_VIM_"
# if(include_VIM){
#   if(!include_sdd) output_folder <- "MCMC_data_only_VIM/"
#   else output_folder <- "MCMC_data_with_VIM_SDD/"
# }else{
#   output_folder <- "MCMC_data_only_SDD/"
# }
output_folder <- "MCMC_data"
if(length(validSites)==1) suffix <- paste0(suffix, "_", validSites)
print(paste0(dataPath, output_folder, suffix))

if(!file.exists(paste0(dataPath, output_folder, suffix))){
  dir.create(file.path(paste0(dataPath, output_folder, suffix)))
}

for(ward in unique(admissionData$ward)){
  admDates <- eval(parse(text=paste0("admDates_", ward)))
  cultRes <- eval(parse(text=paste0("cultRes_", ward)))
  write.table(admDates, paste0(dataPath, output_folder, suffix, "/admDates", suffix,"_", ward, ".txt"), sep =",", 
              row.names = FALSE, col.names = FALSE)
  # Save culture results
  write.table(cultRes, file = paste0(dataPath, output_folder,suffix, "/cultureresults", suffix, "_",ward, ".txt"), sep =",", 
              row.names = FALSE, col.names = FALSE)
}
