# Here's a bunch of scripts to gather the SELECTED data in useful formats

writeParticipantsFile <- function() {
  
  allParticipants <- read.csv('../../Science/Baseline/data/participants_files.csv', stringsAsFactors=F)
  
  groups <- c('30implicit',
              '30explicit',
              '60implicit_b',
              '60explicit_b',
              'cursorjump',
              'aging_implicit',
              'aging_explicit')
  
  columns <- c('ID','instruction','rotation','age','version','folder')
  localizationParticipants <- allParticipants[which(allParticipants$folder %in% groups),columns]
  
  write.csv(localizationParticipants,file='data/participants.csv',quote=F,row.names=F)
  
}

writeGroupLocalizationData <- function() {
  
  participants <- read.csv('data/participants.csv',stringsAsFactors=F)
  
  groups <- unique(participants$folder)
  
  for (group in groups) {
    
    groupParticipants <- participants$ID[which(participants$folder == group)]
    
    alignedGroupData <- NA
    rotatedGroupData <- NA
    
    for (participant in groupParticipants) {
      
      info <- participants[which(participants$folder == group & participants$ID == participant),]
      
      rotatedParticipantLocalizations <- loadParticipantLocalizations(info, task='aligned')
      
      alignedParticipantLocalizations <- loadParticipantLocalizations(info, task='rotated')
      
      if (is.data.frame(alignedGroupData)) {
        
        alignedGroupData <- rbind(alignedGroupData, alignedParticipantLocalizations)
        
      } else {
        
        alignedGroupData <- alignedParticipantLocalizations
        
      }
      
      if (is.data.frame(rotatedGroupData)) {
        
        rotatedGroupData <- rbind(rotatedGroupData, rotatedParticipantLocalizations)
        
      } else {
        
        rotatedGroupData <- rotatedParticipantLocalizations
        
      }
      
    }
    
    write.csv(alignedGroupData,sprintf('data/%s_localizations.csv',group),row.names=F,quote=F)
    
    write.csv(rotatedGroupData,sprintf('data/%s_rotated_localizations.csv',group),row.names=F,quote=F)
  
  }
  
}

loadParticipantLocalizations <- function(info, task='aligned') {
  
  # get base folder:
  folder <- sprintf('../../Science/Baseline/data/%s/%s/',info$folder,info$ID)
  # find files:
  files <- list()
  
  if (task == 'aligned') {
    
    files[['active']]  <- Sys.glob(sprintf('%s*align*active_tap_selected.txt',folder))
    files[['passive']] <- Sys.glob(sprintf('%s*align*passive_tap_selected.txt',folder))
    
  }
  
  if (task == 'rotated') {
    
    files[['active']]  <- c(Sys.glob(sprintf('%s*EI*active_tap_selected.txt',folder)), Sys.glob(sprintf('%s*IE*active_tap_selected.txt',folder)) )
    files[['passive']] <- c(Sys.glob(sprintf('%s*EI*passive_tap_selected.txt',folder)), Sys.glob(sprintf('%s*IE*passive_tap_selected.txt',folder)) )
   
  }
  
  # read all data:
  active  <- read.table(files[['active']],  header=TRUE, sep='\t')
  passive <- read.table(files[['passive']], header=TRUE, sep='\t')
  
  # add column for active/passive
  active$active <- 1
  passive$active <- 0
  
  # combine data frames:
  combined <- rbind(active,passive)
  
  # add participant and group names:
  comnames <- names(combined)
  combined$ID <- as.character(info$ID)
  combined$group <- as.character(info$folder)
  
  # sort columns:
  combined <- combined[c('group','ID',comnames)]
  
  # participant data done:
  return(combined)
  
}

cleanData <- function(verbose=FALSE,proportion=0.70) {
  
  groups <- c('30implicit',
              '30explicit',
              '60implicit_b',
              '60explicit_b',
              'cursorjump',
              'aging_implicit',
              'aging_explicit')
  
  # collect info on cleaning process in these variables:
  allCntrlParticipants <- 0
  keptCntrlParticipants <- 0
  allAgingParticipants <- 0
  keptAgingParticipants <- 0
  
  # collect all data frames here:
  ALcleanGroups <- list()
  ROcleanGroups <- list()
  
  for (group in groups) {
    
    df  <- read.csv(sprintf('data/%s_localizations.csv',group),         stringsAsFactors=F)
    rdf <- read.csv(sprintf('data/%s_rotated_localizations.csv',group), stringsAsFactors=F)
    
    rawrows <- nrow(df)
    rawIDs <- unique(df$ID)
    rawIDs <- rawIDs[which(!is.na(rawIDs))]
    
    # we remove all rows that were deselected:
    df  <-  df[ df$selected == 1,]
    rdf <- rdf[rdf$selected == 1,]
    # cat(sprintf('A: selected: %1.1f%%\n',100*(nrow(df)/rawrows)))
    
    # we remove all localizations where the hand was 'below' the home position:
    df  <-  df[ df$handy_cm > 0,]
    rdf <- rdf[rdf$handy_cm > 0,]
    # cat(sprintf('B: below home: %1.1f%%\n',100*(nrow(df)/rawrows)))
    
    # we remove all localizations where the hand was not within 1 cm of the arc:
    df  <-  df[abs(sqrt( df$handx_cm^2 +  df$handy_cm^2)-12) < 1,]
    rdf <- rdf[abs(sqrt(rdf$handx_cm^2 + rdf$handy_cm^2)-12) < 1,]
    # cat(sprintf('C: hand at arc: %1.1f%%\n',100*(nrow(df)/rawrows)))
    
    # we remove all localizations where the touch was not within 2 cm of the arc:
    df  <-  df[abs(sqrt( df$handx_cm^2 +  df$handy_cm^2)-12) < 2,]
    rdf <- rdf[abs(sqrt(rdf$handx_cm^2 + rdf$handy_cm^2)-12) < 2,]
    # cat(sprintf('D: tap at arc: %1.1f%%\n',100*(nrow(df)/rawrows)))
    
    # we remove all localizations that took longer than 10 seconds:
    df  <-  df[ df$time_ms <= 10000,]
    rdf <- rdf[rdf$time_ms <= 10000,]
    # if (verbose) {
    #   cat(sprintf('E: tap 10 seconds: %1.1f%%\n',100*(nrow(df)/rawrows)))
    # }
    
    if (!grepl('60',group)) {
      
      # we remove all localizations where the touch was not within 30 degrees of the centre of the arc:
      df  <-  df[abs( df$targetangle_deg -  df$arcangle_deg) <= 30,]
      rdf <- rdf[abs(rdf$targetangle_deg - rdf$arcangle_deg) <= 30,]
      # if (verbose) {
      #   cat(sprintf('F: hand in arc: %1.1f%%\n',100*(nrow(df)/rawrows)))
      # }
      
      NSDS <- 3
      
    } else {
      
      # we remove all localizations where the touch was not within 30 degrees of the centre of the arc:
      df  <-  df[abs( df$targetangle_deg -  df$arcangle_deg) <= 50,]
      rdf <- rdf[abs(rdf$targetangle_deg - rdf$arcangle_deg) <= 50,]
      # if (verbose) {
      #   cat(sprintf('F: hand in arc: %1.1f%%\n',100*(nrow(df)/rawrows)))
      # }
      
      NSDS <- 3
      
    }
      
    # the following two steps are better done for individual participants, I guess:
    theseIDs <- unique(df$ID)
    theseIDs <- theseIDs[which(!is.na(theseIDs))]
    keeptheserows <- c()
    keeptheseROws <- c()

    for (ID in theseIDs) {

      ALpprows <- c()
      ROpprows <- c()

      # we remove all localizations that took longer than median + 3 standard deviations:
      loctime <-  df[ df$ID == ID,]$time_ms # <= median(df[df$ID == ID,]$time_ms) + (2 * sd(df[df$ID == ID,]$time_ms))
      ALpprows <- c(ALpprows, which( df$ID == ID &  df$time_ms <= median(loctime) + (NSDS * sd(loctime)) ) )

      loctime <- rdf[rdf$ID == ID,]$time_ms # <= median(df[df$ID == ID,]$time_ms) + (2 * sd(df[df$ID == ID,]$time_ms))
      ROpprows <- c(ROpprows, which(rdf$ID == ID & rdf$time_ms <= median(loctime) + (NSDS * sd(loctime)) ) )

      # we remove all localizations where the angular difference is outside the normal range for that group:
      angdevs <- atan2( df$handy_cm,  df$handx_cm) - atan2( df$tapy_cm,  df$tapx_cm)
      ALpprows <- intersect(ALpprows, which( df$ID == ID & abs(angdevs-mean(angdevs)) < (NSDS * sd(angdevs))) )

      angdevs <- atan2(rdf$handy_cm, rdf$handx_cm) - atan2(rdf$tapy_cm, rdf$tapx_cm)
      ROpprows <- intersect(ROpprows, which(rdf$ID == ID & abs(angdevs-mean(angdevs)) < (NSDS * sd(angdevs))) )

      keeptheserows <- c(keeptheserows, unique(ALpprows))
      keeptheseROws <- c(keeptheseROws, unique(ROpprows))

    }

    df  <-  df[keeptheserows,]
    rdf <- rdf[keeptheseROws,]
    
    
    
    # we remove all participants that have very few localizations left
    ppsummary <- aggregate(selected ~ ID, data=aggregate(selected ~ ID * active, data=df, FUN=sum), FUN=min)
    keepIDs <- ppsummary$ID[ppsummary$selected >= (72*proportion)]
    df  <-  df[ df$ID %in% keepIDs,]
    rdf <- rdf[rdf$ID %in% keepIDs,]
    
    ALcleanGroups[[group]] <-  df
    ROcleanGroups[[group]] <- rdf
    
    # provide feedback on the cleaning in this group, if required:
    if (verbose) {
      cat(sprintf('%s: kept %d / %d participants (%0.1f%%)\n', group, length(keepIDs), length(rawIDs), (100 * (length(keepIDs)/length(rawIDs))) ))
    }
    
    # collect info on cleaning process:
    if (substring(group, 1, 5) == 'aging') {
      allAgingParticipants <- allAgingParticipants + length(rawIDs)
      keptAgingParticipants <- keptAgingParticipants + length(keepIDs)
      
    } else {
      allCntrlParticipants <- allCntrlParticipants + length(rawIDs)
      keptCntrlParticipants <- keptCntrlParticipants + length(keepIDs)
    }
    
  }
  
  # provide feedback on cleaning process if required:
  if (verbose) {
    cat(sprintf('\nCONTROL: kept %d / %d participants (%0.1f%%)\n', keptCntrlParticipants, allCntrlParticipants, (100 * (keptCntrlParticipants/allCntrlParticipants)) ))
    cat(sprintf('AGING:   kept %d / %d participants (%0.1f%%)\n', keptAgingParticipants, allAgingParticipants, (100 * (keptAgingParticipants/allAgingParticipants)) ))
  }
  
  return(list('aligned'=ALcleanGroups, 'rotated'=ROcleanGroups))
  
}

detrendData <- function(groupData) {
  
  # The output from the cleanData() function should go in here.
  # We'll detrend the data using splines, but have to be carefull not to 
  # extrapolate. Worst case: we loose 2 datapoints.
  
  groups <- names(groupData[['aligned']])
  
  # we'll end up with only two (four) dataframes now:
  cntrl  <- NA
  aging  <- NA
  Rcntrl <- NA
  Raging <- NA
  
  ppno <- 0
  
  for (group in groups) {
    
    df  <- groupData[['aligned']][[group]]
    Rdf <- groupData[['rotated']][[group]]
    # print(group)
    IDs <- unique(df$ID)
    
    for (ID in IDs) {
      
      IDidx <- which(df$ID == ID)
      
      # we'll switch from IDs to numbers:
      ppno <- ppno + 1
      
      participant <- rep(ppno,length(IDidx))
      block <- df[IDidx,'block']
      trial <- df[IDidx,'trial']
      active_bool <- df[IDidx,'active'] 
      # handangle_deg <- df[IDidx,'targetangle_deg']
      arcangle_deg <- df[IDidx,'arcangle_deg']
      tapx <- df[IDidx,'tapx_cm']
      tapy <- df[IDidx,'tapy_cm']
      # I use atan2(X,Y), not atan2(Y,X) to stay well within atan2 range of output:
      localizationangle_deg <- ((atan2(-1*tapx, tapy) / pi) * 180) + 90
      
      handx <- df[IDidx,'handx_cm']
      handy <- df[IDidx,'handy_cm']
      # same use of atan2 here:
      handangle_deg <- ((atan2(-1*handx, handy) / pi) * 180) + 90
      
      # we detrend by getting the predicted localization angle from all other data
      # and subtracting it from the actual data
      # prediction can be an interpolation using splines or kernel smoothing (or LOESS / LOWESS):
      # (my current opinion is that splining is better than the other two)
      
      predicted_angles <- c()
      
      for (trial.idx in c(1:length(IDidx))) {
        
        # w <- 1 - (0.75 * ( df[IDidx,'time_ms'] / max(df[IDidx,'time_ms']) ))
        spl <- smooth.spline(x=handangle_deg[-trial.idx], y=localizationangle_deg[-trial.idx], spar=0.90, keep.data=F )
        
        thissampleprediction <- predict(spl, x=handangle_deg[trial.idx])$y
        #print(thissampleprediction)
        predicted_angles <- c(predicted_angles, thissampleprediction)
        
      }
      
      localizationerror_deg <- (localizationangle_deg - predicted_angles)
      localization_deg <- (localizationangle_deg - localizationerror_deg)
      
      if (substr(group,1,6) == 'aging_') {
        agegroup <- rep('aging',length(IDidx))
      } else {
        agegroup <- rep('cntrl',length(IDidx))
      }
      
      IDdf <- data.frame(agegroup,group,participant,block,trial,active_bool,handangle_deg,localization_deg,RT_ms=df[IDidx,'time_ms'])
      
      # remove the minimum and maximum handangle? (NOT DONE)
      # (they may suffer from extrapolation-like inaccuracies)
      
      if (substr(group,1,6) == 'aging_') {
        # print(ppno)
        if (is.data.frame(aging)) {
          aging <- rbind(aging, IDdf)
        } else {
          aging <- IDdf
        }
      } else {
        # print(ppno)
        if (is.data.frame(cntrl)) {
          cntrl <- rbind(cntrl, IDdf)
        } else {
          cntrl <- IDdf
        }
        
      }
      
      # also deal with the rotated sessions:
      # we only want a generic "training-induced shift" across the trained area
      POI <- seq(45,135,22.5)
      
      # DO THIS SEPARATELY FOR ACTIVE AND PASSIVE!
      
      spl <- smooth.spline(x=handangle_deg, y=localizationangle_deg, spar=0.90, keep.data=F )
      alignedPOIloc <- predict(spl, x=POI)$y
      
      for (active in c(0,1)) {
        
        # now estimate the same points in the rotated session
        rIDidx <- which(Rdf$ID == ID & Rdf$active == active)
        
        Rtapx <- Rdf[rIDidx,'tapx_cm']
        Rtapy <- Rdf[rIDidx,'tapy_cm']
        # I use atan2(X,Y), not atan2(Y,X) to stay well within atan2 range of output:
        Rlocalizationangle_deg <- ((atan2(-1*Rtapx, Rtapy) / pi) * 180) + 90
        
        Rhandx <- Rdf[rIDidx,'handx_cm']
        Rhandy <- Rdf[rIDidx,'handy_cm']
        # same use of atan2 here:
        Rhandangle_deg <- ((atan2(-1*Rhandx, Rhandy) / pi) * 180) + 90
        
        Rspl <- smooth.spline(x=Rhandangle_deg, y=Rlocalizationangle_deg, spar=0.90, keep.data=F )
        rotatedPOIloc <- predict(Rspl, x=POI)$y
        
        # now we have the shift here:
        localizationshift_deg <- mean(rotatedPOIloc - alignedPOIloc)
        
        # let's build a dataframe with one row:
        agegroup <- c(agegroup[1])
        participant <- c(ppno)
        active_bool <- c(active)
        
        
        rIDdf <- data.frame(agegroup,group,participant,active_bool,localizationshift_deg)
        
        if (substr(group,1,6) == 'aging_') {
          # print(ppno)
          if (is.data.frame(Raging)) {
            Raging <- rbind(Raging, rIDdf)
          } else {
            Raging <- rIDdf
          }
        } else {
          # print(ppno)
          if (is.data.frame(Rcntrl)) {
            Rcntrl <- rbind(Rcntrl, rIDdf)
          } else {
            Rcntrl <- rIDdf
          }
        }
        
      }
      
      ##### UP TO HERE THE ADDED ROTATION STUFF
      
    }
    
  }
  
  detrended <- list()
  detrended[['aligned']] <- list()
  detrended[['aligned']][['young']] <- cntrl
  detrended[['aligned']][['aging']] <- aging
  detrended[['rotated']] <- list()
  detrended[['rotated']][['young']] <- Rcntrl
  detrended[['rotated']][['aging']] <- Raging
  
  return(detrended)
  
}

saveDetrendedData <- function() {
  
  dfs <- detrendData(cleanData())
  
  write.csv(dfs[['aligned']][['young']], file='data/young_localization.csv', quote=F, row.names=F)
  write.csv(dfs[['aligned']][['aging']], file='data/aging_localization.csv', quote=F, row.names=F)

  write.csv(dfs[['rotated']][['young']], file='data/young_rotated_localization.csv', quote=F, row.names=F)
  write.csv(dfs[['rotated']][['aging']], file='data/aging_rotated_localization.csv', quote=F, row.names=F)
  
}

loadDetrendedData <- function() {
  
  dfs <- list()
  
  dfs[['young']] <- read.csv(file='data/young_localization.csv', stringsAsFactors=F)
  dfs[['aging']] <- read.csv(file='data/aging_localization.csv', stringsAsFactors=F)
  
  return(dfs)
  
}