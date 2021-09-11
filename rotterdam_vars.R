# ============================================================================ #
# Rotterdam project
# ----------------------------------------------------------------------------
# Global variables
# ============================================================================ #
# Define study period
outbreak_period <- 0
non_outbreak_period <- 0
if(outbreak_period){
  startDate <- as.Date("2011-05-01", format="%Y-%m-%d")
  endDate <- as.Date("2015-04-30", format="%Y-%m-%d")
}else{
  startDate <- as.Date("2010-01-01", format="%Y-%m-%d")
  endDate <- as.Date("2018-05-18", format="%Y-%m-%d")
  if(non_outbreak_period){
    startDate <- as.Date("2015-05-01", format="%Y-%m-%d")
    endDate <- as.Date("2018-05-18", format="%Y-%m-%d")
  }
}

# Eligible sites 
validSites <- c("Keel","Rectum")
# validSites <- c("Keel")

# Valid examination types 
validExamTypes <- read_excel(paste0(dataPath, "Data_ICU_2008-2018_ErasmusMC_Explanation.xlsx"), sheet=2)
validExamTypes <- validExamTypes[validExamTypes$Inclusion=="yes","ExaminationType"]
validExamTypes <- unlist(validExamTypes, use.names = F)

# Positive results
posRes <- c("positief","Positief", "POS", "PCR POS", "Pseudomonas aeruginosa")

negRes <- c("Geen resistente micro-organismen gekweekt",
            "Geen aerobe gram negatieve staven of S.aureus gekweekt",
            "Geen aerobe gram negatieve staven gekweekt",
            "Geen resistente Pseudomonas aeruginosa gekweekt",
            "Geen VIM-positieve gramnegatieve staaf gekweekt.",
            "geen groei",
            "NEG","negatief","Negatief")

# Wards
ward_1 <- c("H3Z","3ZBE")
ward_2 <- c("AZIC","H10Z")

# Colors for background transmission and cross-transmission
colors <- c("#6aa84f", "#e69138")
