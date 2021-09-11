# Functions for Rotterdam data analysis
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # If you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  return(x)
}

percentile <- function(perc) {
  # perc is the percentile which should be computed for the numeric vector x
  function(x) quantile(x, perc*1e-2, na.rm=TRUE)
}

check.cult.adm <- function(admData, cultData){
  cultAdmPat <- NULL
  for(i in 1:nrow(cultData)){
    pat <- cultData$pID[i]
    date <- cultData$samplingDate[i]
    ind <- which(admData$pID==pat)
    if(length(ind)>0){
      if(!(date>=admData[ind,"admDate"] & date <=admData[ind, "disDate"])){
        cultAdmPat <- rbind(cultAdmPat, cbind(pat=pat, row=i))
      }
    }
  }
  cultAdmPat <- as.data.frame(cultAdmPat)
  return(cultAdmPat=cultAdmPat)
}



falsenegative <- function(cultRes,admDates){
  FN <- 0
  patFN <- NULL
  colstatus <- cbind(pID=unique(admDates[,1]), colstatus=rep(NA,length(unique(admDates[,1]))))
  for(pat in unique(unlist(cultRes[,1]))){
    days <- cultRes[which(cultRes[,1]==pat),]
    status <- colstatus[which(colstatus[,1]==pat),2] <- unlist(cultRes[which(cultRes[,1]==pat),"result"])[1]
    for(d in 1:nrow(days)){
      if(days[d,"result"]==0 && status==1){ 
        FN<- FN + 1
        patFN <- c(patFN, pat)
      }
      if(days[d,"result"]==1 && status==0){
        status <- colstatus[which(colstatus[,1]==pat),2]<- 1
      }
    }
  }
  return(list(FN=FN,patFN=unique(patFN)))
} 

# falsenegative <- function(cultRes,admDates){
#   FN <- 0
#   patFN <- NULL
#   colstatus <- cbind(pID=unique(admDates[,1]), colstatus=rep(NA,length(unique(admDates[,1]))))
#   for(pat in unique(unlist(cultRes[,1]))){
#     days <- cultRes[which(cultRes[,1]==pat),]
#     status <- colstatus[which(colstatus[,1]==pat),2] <- unlist(cultRes[which(cultRes[,1]==pat),3])[1]
#     for(d in 1:nrow(days)){
#       if(days[d,3]==0 && status==1){ 
#         FN<- FN + 1
#         patFN <- c(patFN, pat)
#       }
#       if(days[d,3]==1 && status==0){
#         status <- colstatus[which(colstatus[,1]==pat),2]<- 1
#       }
#     }
#   }
#   return(list(FN=FN,patFN=unique(patFN)))
# } 

find.cultures <- function(admData, cultData){
  result <- NULL
  patientIDs <- unique(cultData$pID[cultData$pID%in%admData$pID])
  for(p in patientIDs){
    indCult <- which(cultData$pID==p)
    indAdm <- which(admFile$pID==p)
    included <- apply(cultData[indCult,"samplingDate"], 1, function(d) unlist(apply(admFile[indAdm,c("admDate","disDate")], 1, function(x) d>=x[1]&&d<=x[2])))
    result <- rbind(result, cultData[indCult[included],])
  }
  
  
}



divide.cultures <- function(admFile, cultData, ward1, ward2){
  patientIDs <- unique(cultData$pID)
  cultureFile_1 <- cultureFile_2 <- NULL
  for(p in patientIDs){
    indCult <- which(cultData$pID==p)
    indAdm <- which(admFile$pID==p)
    for(i in indCult){
      d <- cultData[i, "samplingDate"]
      included <- unlist(apply(admFile[indAdm,c("admDate","disDate")], 1, function(x) d>=x[1]&&d<=x[2]))
      indIncl <- which(included)[1]
      if(length(indIncl)>0){
        ward <- unique(admFile[indAdm[indIncl],"ward"])
        if(ward%in%ward1) cultureFile_1 <- rbind(cultureFile_1, cultData[i,])
        if(ward%in%ward2) cultureFile_2 <- rbind(cultureFile_2, cultData[i,])
      }
    }
  }
  cultureFile_1 <- as.data.frame(cultureFile_1)
  cultureFile_2 <- as.data.frame(cultureFile_2)
  return(list(cult_1 = cultureFile_1, cult_2=cultureFile_2))
}



easyplot <- function(admDates, cultRes=NULL){
  studyperiod <- seq(min(admDates[,2]), max(admDates[, 2]))
  studylength <-  max(studyperiod)-min(studyperiod)
  Npat <- rep(0,studylength)
  Ncol <- rep(0,studylength)
  for(i in 1:studylength){
    Npat[i] <- length(which(admDates[, 2]==studyperiod[i]))
    if(!is.null(cultRes)) Ncol[i] <- length(which(cultRes[which(cultRes[,2]==studyperiod[i]),3]==1))
  }
  plot.Npat <- plot(Npat, type="l")
  if(!is.null(cultRes)) plot.Ncol <- plot(Ncol, type="l")
  return(list(plot.Npat=plot.Npat, plot.Ncol=plot.Ncol, Npat=Npat, Ncol=Ncol))
}



# Create data set that contains the first culture of each patient
first.culture <- function(cultFile){
  patientIds <- unique(cultFile$pID)
  firstCult <- as.data.frame(cultFile[FALSE,])
  i <- 1
  for(p in patientIds){
    ind <- which(cultFile$pID==p)
    firstCult[i,] <- cultFile[ind[1],]
    i <- i+1
  }
  return(as.data.frame(firstCult))
}

# Number of observed acquisitions
n.acquisitions <- function(admFile, cultFile){
  colnames(cultFile)[which(colnames(cultFile)=="pID")] <- "ID"
  nAdm <- length(unlist(unique(admFile[, "pID"])))
  test <- sqldf('SELECT C."ID", admFile.admDate, C."samplingDate", C.result
              FROM cultFile C
              LEFT JOIN admFile 
              ON admFile.pID=C."ID" AND (admFile.admDate=C."samplingDate" OR admFile.admDate+1=C."samplingDate" OR admFile.admDate+2=C."samplingDate")')
  test <- unique(test)
  # Acquired colonizations
  acqCult <- test[which(is.na(test[, "admDate"])), ]
  acqCol <- acqCult[, c("ID", "result")]; acqCol <- acqCol[!duplicated(acqCol),]
  # Observed acquisitions
  obsAcq.no <- sum(acqCol[, "result"])
  obsAcq.percent <- sum(acqCol[, "result"])/nAdm
  
  return(list(obsAcq.no=obsAcq.no, obsAcq.percent=obsAcq.percent))
}




# Number of positive admissions
n.importations <- function(admFile, cultFile){
  colnames(cultFile)[which(colnames(cultFile)=="pID")] <- "ID"
  nAdm <- length(unlist(unique(admFile[, "pID"])))
  admCult <- sqldf('SELECT pID, admDate, C.result 
                 FROM admFile
                 LEFT JOIN cultFile C on admFile.pID=C."ID" AND (admFile.admDate=C."samplingDate" OR admFile.admDate+1=C."samplingDate" OR admFile.admDate+2=C."samplingDate")')
  admCult <- admCult[!duplicated(admCult),]
  (posAdm.no <- sum(admCult[!is.na(admCult[, "result"]), "result"]))
  (posAdm.percent <- sum(admCult[!is.na(admCult[, "result"]), "result"])/nAdm)
  return(list(posAdm.no=posAdm.no, posAdm.percent=posAdm.percent))
}


# Create admission data for MCMC
create.admission.file <- function(hospStay){
  N <- nrow(hospStay)
  patientIds <- unique(hospStay$pID)
  admDates <- NULL
  for(i in 1:N){
    temp <- seq(hospStay[[i, "admDate"]], hospStay[[i, "disDate"]], 1)
    admDates <- rbind(admDates, cbind(rep(patientIds[i], length(temp)), temp))
  }
  admDates <- cbind(admDates, rep(1, nrow(admDates)))
  colnames(admDates) <- c("pID", "day", "unit")
  return(admDates)
}

# Create culture results for MCMC
create.culture.file <- function(hospStay, admDates, cultData){
  cultRes <- NULL
  cultData[,2] <- as.numeric(as.Date(cultData[,2], format="%Y-%m-%d"))
  cultData <- as.data.frame(cultData[order(cultData$samplingDate),])
  patientIds <- unique(hospStay$pID)
  for(p in patientIds){
    if(p%in%unlist(cultData[, 1])){
      indcult <- which(cultData[, 1]==p)
      ind <- which(hospStay[, 1]==p)
      days <- cultData[indcult, 2]
      if(all(days>=hospStay[ind, "admDate"])&&all(days<=hospStay[ind, "disDate"])){
        cultRes <- rbind(cultRes, cultFile[indcult,c(1,2,ncol(cultFile))]) 
      }else{
        ind <- which(admDates[, 1]==p)
        admDates <- admDates[-ind,]
      }
    }
  }
  return(list(admDates=admDates, cultRes=cultRes))
}

# Relabel patient IDs
relabelpatientID <- function(admDates, cultRes, mapId){
  resultA <- admDates
  resultC <- cultRes
  table <- sort(unique(resultA[, "pID"]))
  mapTable <- cbind(table, rep(NA,length(table)))
  colnames(mapTable)<-c("pID","relabeled")
  v <- seq(1:length(table))
  for(i in 1:length(table)){
    indA <- which(admDates[, "pID"]==table[i])
    indC <- which(cultRes[, "pID"]==table[i])
    resultA[indA, "pID"] <- v[i]
    resultC[indC, "pID"] <- v[i]
    mapTable[i, "relabeled"] <- v[i]
  }
  return(list(admDates=as.data.frame(resultA), 
              cultRes=as.data.frame(resultC), 
              table=table, 
              mapTable=mapTable))
}

# Relabel dates
relabeldates <- function(admDates, cultRes){
  resultA <- admDates
  resultC <- cultRes
  min.date <- min(admDates[, "day"], cultRes[, "samplingDate"])
  resultA[, "day"] <- resultA[, "day"] - min.date
  resultC[, "samplingDate"] <- resultC[, "samplingDate"] - min.date
  return(list(admDates=resultA,cultRes=resultC, min.date=min.date))
}


# Readmissions
readmissions <- function(admFile, cultFile){
  # admissionFile <- as.data.frame(admFile)
  # cultureFile <- as.data.frame(cultFile)
  admFile <- as.data.frame(admFile)
  cultFile <- as.data.frame(cultFile)
  mapId <- NULL
  combineId <- readmId <- NULL
  # Check if there are readmissions
  if(length(admFile$pID) == length(unique(admFile$pID))){
    print("No re-admissions in the data. Nothing to do.")
    return(list(admFile=admFile, cultFile=cultFile))
  }
  # Continue if there are readmissions
  patientIds <- unique(admFile$pID)
  for(p in patientIds){
    ind <- which(admFile$pID==p)
    if(length(ind)>1){
      i <- ind[1]; count <- 1
      while(!is.na(i)){
        ddate <- admFile[i, "disDate"]
        common <- ind[which(unlist(admFile[ind, "admDate"])==as.numeric(ddate))]
        # common <- ind[which(unlist(admFile[ind, "admDate"])==as.numeric(ddate))][2]
        if(length(common)>0){
          while(length(common)>0 & !is.na(common) & admFile[common,"admDate"]==ddate){
            print(paste0("Reduce consecutive readmissions for patient ", p))
            combineId<- c(combineId, p)
            admFile[i, "disDate"] <- admFile[common, "disDate"]
            admFile[i, "LOS"] <- admFile[i, "disDate"] - admFile[i, "admDate"]
            admFile[i, "LOS_hr"] <- admFile[i, "LOS_hr"] + admFile[common, "LOS_hr"]
            admFile <- admFile[-common,] # this needs to be moved
            
            ind <- which(admFile[, "pID"]==p)
            i <- ind[count]
            ddate <- admFile[i, "disDate"]
            common <- ind[which(unlist(admFile[ind, "admDate"])==as.numeric(ddate))][1]
            if(!is.na(common) && common == i){
              common <- ind[which(unlist(admFile[ind, "admDate"])==as.numeric(ddate))][2]
            }
          }
        }
        count <- count + 1
        ind <- which(admFile$pID==p)
        i <- ind[count]
      }
      # Every distinct stay/admission is a new patient
      seq <- 1
      if(length(ind)>1){
        readmId <- c(readmId, p)
        for(i in ind[-1]){
          adate <- admFile[i,"admDate"]; ddate <- admFile[i,"disDate"]
          indcult <- which(cultFile[, "pID"]==p)
          # Check whether culture results are within the dates of the current admission
          indreplace <- intersect(which(cultFile[indcult, "samplingDate"]>=adate), which(cultFile[indcult, "samplingDate"]<=ddate))
          newid <- as.numeric(paste(1234, seq, p, sep=""))
          mapId <- rbind(mapId, cbind(unique(admFile[i,"pID"]), newid))
          cultFile[indcult[indreplace], "pID"] <- newid
          admFile[i,"pID"] <- newid
          seq <- seq +1
        }
      }else{
        mapId <- rbind(mapId, cbind(p,p))
      }

    }else{
      mapId <- rbind(mapId, cbind(p,p))
    }
  }
  colnames(mapId) <- c("original", "changed")
  mapId <- mapId[!duplicated(mapId),]
  return(list(admFile=admFile, cultFile=cultFile, mapId=mapId, 
              combineId=combineId, readmId=readmId))
}



plot.N <- function(hospStay, 
                   title="Number of patients per day",
                   axis.title.x = "Date", axis.title.y = "Occupied beds",
                   axis.text.size=16, axis.title.size=20){
  min.date.adm <- min(hospStay$admDate); max.date.dis <- max(hospStay$disDate)
  seqDate <- as.character(seq(as.Date(min.date.adm), as.Date(max.date.dis), "days"))
  studyperiod <- seq(as.numeric(min(hospStay$admDate)), as.numeric(max(hospStay$disDate)))
  studylength <- length(studyperiod)
  Npat <- rep(0, studylength)
  for(p in unique(hospStay$pID)){
    row <- which(hospStay$pID==p)
    if(length(row)>1){
      i <- 1
      while(i<=length(row)){
        stay <- seq(unlist(hospStay[row[i], "admDate"]), unlist(hospStay[row[i], "disDate"]))
        indstay <- match(stay, studyperiod)
        Npat[indstay] <- Npat[indstay]+1
        i <- i + 1
      }
    }else{
      stay <- seq(unlist(hospStay[row, "admDate"]), unlist(hospStay[row, "disDate"]))
      indstay <- match(stay, studyperiod)
      Npat[indstay] <- Npat[indstay]+1
    }
  }
  
  dm <- as.data.frame(cbind(Npat, seqDate))
  dm$seqDate <- format(as.Date(seqDate), "%d-%m-%Y")
  p <- ggplot(data=dm, aes(x=as.Date(seqDate, format = "%d-%m-%Y"), y= as.numeric(Npat), group=1)) + 
        geom_line() + 
        ggtitle(title) + 
        labs(x=axis.title.x, y=axis.title.y) +
        scale_x_date(date_labels = "%Y") + 
        theme_bw() + 
        theme(axis.text=element_text(size=axis.text.size),
              axis.title = element_text(size=axis.title.size),
              plot.title= element_text(hjust=0.5, size=22))
  return(list(p=p, data=dm))
}


plot.pos.cultures <- function(data, vim = F, 
                              title="Number of positive cultures per day",
                              axis.title.x = "Date", axis.title.y = "Number of positive cultures",
                              axis.text.size=16, axis.title.size=20){
  min.date.cult <- min(data$samplingDate); max.date.cult <- max(data$samplingDate)
  seqDate <- as.character(seq(as.Date(min.date.cult), as.Date(max.date.cult), "days"))
  studyperiod <- seq(as.numeric(min(data$samplingDate)), as.numeric(max(data$samplingDate)))
  studylength <- length(studyperiod)
  if(vim){
    dm <- data[,c("samplingDate","result","vim","ward")]
    p <- ggplot(data=dm, aes(x=samplingDate, y=result, fill=as.factor(vim))) + 
      facet_grid(ward~.) + 
      geom_bar(position="stack", stat="identity") + 
      ggtitle(title) + 
      labs(x=axis.title.x, y=axis.title.y) +
      scale_x_date(date_labels = "%b %Y",date_breaks ="3 months") +
      scale_fill_discrete(name = "VIM test", labels = c("no","yes")) + 
      theme_bw() + 
      theme(axis.text.y=element_text(size=axis.text.size),
            axis.text.x = element_text(size=axis.text.size, angle=45, hjust=1),
            axis.title.y = element_text(size=axis.title.size),
            axis.title.x = element_blank(),
            plot.title= element_text(hjust=0.5, size=22))
  }else{
    dm <- data[data$result==1,c("samplingDate","result","ward")]
    p <- ggplot(data=dm, aes(x=samplingDate, y= result)) + 
      facet_grid(ward~.)+ 
      geom_bar(stat="identity", position="stack") + 
      ggtitle(title) + 
      labs(x=axis.title.x, y=axis.title.y) +
      scale_x_date(date_labels = "%Y") + 
      theme_bw() + 
      theme(axis.text.y=element_text(size=axis.text.size),
            axis.text.x = element_text(size=axis.text.size, angle=45, hjust=1),
            axis.title.y = element_text(size=axis.title.size),
            axis.title.x = element_blank(),
            plot.title= element_text(hjust=0.5, size=22))
  }

  return(list(p=p,data=dm))
  
}


rel.contr <- function(results, ind, contrCol = NULL, precision = 4, median = TRUE, bedwise=0){
  alphaContr<-NULL;betaContr<-NULL;epsContr<-NULL;crossContr<-NULL;envContr<-NULL;env <- 1
  pContr <- NULL
  # indPos <- which(results[ind, "contrEnv"]>=0)
  indPos <- ind
  
  if(is.null(contrCol)){
    for(i in indPos){
      acq <- sum(results[i, c("acontr", "bcontr")], env*results[i, "contrE"])
      alphaContr <- c(alphaContr, results[i, "acontr"]/acq)
      betaContr <- c(betaContr, results[i, "bcontr"]/acq)
      epsContr <- c(epsContr, env*results[i, "contrE"]/acq)
      crossContr <- c(crossContr, env*results[i, "contrCross"]/acq)
      envContr <- c(envContr, env*results[i, "contrEnv"]/acq)
    }
  }else{
    for(i in indPos){
      if(bedwise){
        acq <- sum(results[i, c(contrCol[1], contrCol[2])], bedwise*results[i, contrCol[3]], 
                   env*results[i, contrCol[4]])
      }else{acq <- sum(results[i, c(contrCol[1], contrCol[2])], env*results[i, contrCol[3]])}
      if(acq==0){
        alphaContr <- c(alphaContr, 0)
        betaContr <- c(betaContr, 0)
      }else{
        alphaContr <- c(alphaContr, results[i, contrCol[1]]/acq)
        betaContr <- c(betaContr, results[i, contrCol[2]]/acq) 
      }
      if(bedwise){
        if(acq==0){
          pContr <- c(pContr, 0)
          epsContr <- c(epsContr, 0)
          crossContr <- c(crossContr, 0)
          envContr <- c(envContr, 0)
        }else{
          pContr <- c(pContr, results[i, contrCol[3]]/acq)
          epsContr <- c(epsContr, env*results[i, contrCol[4]]/acq)
          crossContr <- c(crossContr, env*results[i, contrCol[5]]/acq)
          envContr <- c(envContr, env*results[i, contrCol[6]]/acq)
        }
      }else{
        if(acq==0){
          epsContr <- c(epsContr, 0)
          crossContr <- c(crossContr, 0)
          envContr <- c(envContr, 0)
        }else{
          epsContr <- c(epsContr, env*results[i, contrCol[3]]/acq)
          crossContr <- c(crossContr, env*results[i, contrCol[4]]/acq)
          envContr <- c(envContr, env*results[i, contrCol[5]]/acq)
        }
      }
    }
  }
  
  if(median){
    if(bedwise){
      out1 <- round(apply(cbind(alphaContr, betaContr, pContr, epsContr), 2, function(x) median(x, na.rm=TRUE)), 4)
      out2 <- round(apply(cbind(alphaContr, crossContr, envContr), 2, function(x) median(x, na.rm=TRUE)), 4)
    }else{
      out1 <- round(apply(cbind(alphaContr, betaContr, epsContr), 2, function(x) median(x, na.rm=TRUE)), 4)
      out2 <- round(apply(cbind(alphaContr, crossContr, envContr), 2, function(x) median(x, na.rm=TRUE)), 4)
    }
    
  }else{
    if(bedwise){
      out1 <- round(apply(cbind(alphaContr, betaContr, pContr, epsContr), 2, function(x) mean(x, na.rm=TRUE)), 4)
      out2 <- round(apply(cbind(alphaContr, crossContr, envContr), 2, function(x) mean(x, na.rm = TRUE)), 4)
    }else{
      out1 <- round(apply(cbind(alphaContr, betaContr, epsContr), 2, function(x) mean(x, na.rm=TRUE)), 4)
      out2 <- round(apply(cbind(alphaContr, crossContr, envContr), 2, function(x) mean(x, na.rm = TRUE)), 4)
    }
  }
  
  return(list(alphaContr=alphaContr,betaContr=betaContr,pContr=pContr,epsContr=epsContr,crossContr=crossContr,envContr=envContr, 
              out1 = out1, out2 = out2))
}

stat.summary <- function(res, precision = 4, level = 0.05, hpd=TRUE){
  meanresults <- round(apply(res, 2, mean), precision)
  sdresults <- round(apply(res, 2, sd), precision)
  medianresults <- round(apply(res, 2, median), precision)
  
  # COMPUTING CREDIBLE INTERVALS
  CIresults <- apply(res,2, function(x) quantile(x,c(level/2, 1-level/2), na.rm=T))
  CIresults1 <- apply(res,2, function(x) quantile(x,c(0, 1-level), na.rm=T))
  CIresults2 <- apply(res,2, function(x) quantile(x,c(level, 1), na.rm=T))
  
  statsummary <- cbind(meanresults, medianresults, sdresults, 
                       paste("(", round(CIresults[1,],precision), ",", round(CIresults[2,],precision), ")", sep = ""), 
                       paste("(", round(CIresults1[1,],precision), ",", round(CIresults1[2,],precision), ")", sep = ""), 
                       paste("(", round(CIresults2[1,],precision), ",", round(CIresults2[2,],precision), ")", sep = ""))
  colnames(statsummary) <- c("mean", "median", "sd", "95% CrI", "95% CrI (lower)", "95% CrI (upper)")
  
  # if(hpd){
  #   CIresults <- apply(res, 2, function(x) hdi(x, credMass=1-level))
  #   statsummary <- cbind(meanresults, medianresults, sdresults, 
  #                        paste("(", round(CIresults[1,],precision), ",", round(CIresults[2,],precision), ")", sep = ""))
  #   colnames(statsummary) <- c("mean", "median", "sd", "95% HPD")
  # }else{
  #   CIresults <- apply(res,2, function(x) quantile(x,c(level/2, 1-level/2)))
  #   CIresults1 <- apply(res,2, function(x) quantile(x,c(0, 1-level)))
  #   CIresults2 <- apply(res,2, function(x) quantile(x,c(level, 1)))
  #   
  #   statsummary <- cbind(meanresults, medianresults, sdresults, 
  #                        paste("(", round(CIresults[1,],precision), ",", round(CIresults[2,],precision), ")", sep = ""), 
  #                        paste("(", round(CIresults1[1,],precision), ",", round(CIresults1[2,],precision), ")", sep = ""), 
  #                        paste("(", round(CIresults2[1,],precision), ",", round(CIresults2[2,],precision), ")", sep = ""))
  #   colnames(statsummary) <- c("mean", "median", "sd", "95% CrI", "95% CrI (lower)", "95% CrI (upper)")
  # }
  
  rownames(statsummary) <- colnames(res)
  statsummary <- as.data.frame(statsummary)
  return(statsummary)
}

contr.fun <- function(results, nUnits, burnIn=0,precision=4, bedwise=0){
  
  for(i in 1:nUnits){
    assign(paste0("results",i), results[seq(i, nrow(results), by=nUnits), ])
    temp <- eval(parse(text=paste0("results", i)))
    assign(paste0("start",i), which(temp$i==100)-10)
    assign(paste0("ind",i), (eval(parse(text=paste0("start", i)))+burnIn):nrow(temp))
  }
  # ================================================== #
  # Contribution of different transmission routes
  # ================================================== #
  (cnames <- colnames(results1))
  if(bedwise){contrCol <- colnames(results1)[(length(cnames)-5):length(cnames)]
  print("Bedwise environmental contamination.")
  }else{
    if("mu"%in%cnames){
      contrCol <- colnames(results1)[(length(cnames)-4):length(cnames)]
    }else{
      contrCol <- colnames(results1)[(length(cnames)-1):length(cnames)]}
  }
  
  print(paste0("Column names for relative contribution: "))
  print(contrCol)
  resList <- list()
  for(i in 1:nUnits){
    tempRes <- eval(parse(text=paste0("results", i)))
    tempInd <- eval(parse(text=paste0("ind", i)))
    relContr <- rel.contr(tempRes, tempInd, contrCol,bedwise=bedwise)
    alphaContr <- relContr$alphaContr; betaContr <- relContr$betaContr; 
    if("mu"%in%cnames){
      epsContr <- relContr$epsContr; 
      crossContr <- relContr$crossContr; envContr <- relContr$envContr; 
    }
    
    if(bedwise) pContr <- relContr$pContr;
    
    remove <- which(colnames(results1)%in%c("daysatr"))
    if(bedwise){
      assign(paste0("res", i), cbind(tempRes[tempInd,-remove], alphaContr, betaContr, pContr, epsContr, crossContr, envContr))
    }else {
      if("mu"%in%cnames){
        assign(paste0("res", i), cbind(tempRes[tempInd,-remove], alphaContr, betaContr, epsContr, crossContr, envContr))
      }else assign(paste0("res", i), cbind(tempRes[tempInd,-remove], alphaContr, betaContr))
    }
    res <- eval(parse(text=paste0("res", i)))
    print(stat.summary(res,precision=precision))
    resList[[i]] <- res
    # Save into file
    # capture.output(stat.summary(res,precision=precision), file = paste(resPath,folder,"results",date, "_unit", i, ".txt", sep=""))
  }
  if(nUnits==1) resList <- resList[[1]]
  return(resList=resList)
}
