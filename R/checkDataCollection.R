
checkListFolders <- function() {
  
  ppa <- read.csv('../../Science/Baseline/data/participant_awareness.csv', stringsAsFactors=F)
  
  # first of all, all participants should be unique:
  cat(sprintf('\nproportion unique participants: %0.03f\n',length(unique(ppa$participant)) / length(ppa$participant)))
  
  # second, all participants should have a folder:
  for (row.idx in c(1:nrow(ppa))) {
    pp <- ppa$participant[row.idx]
    folder <- ppa$group[row.idx]
    if (dir.exists(sprintf('../../Science/Baseline/data/%s/%s', folder, pp)) ) {
      # great
    } else {
      cat(sprintf('participant in list, but no folder: %s (%s)\n', pp, folder))
    }
  }
  
  # third, all folders with data should have their participant in the list:
  groups <- c('30explicit', '30implicit', 'cursorjump', '60explicit_b', '60implicit_b', 'aging_explicit', 'aging_implicit', '60explicit', '60implicit', 'org30implicit', 'org30explicit')
  for (group in groups) {
    
    subfolders <- Sys.glob(sprintf('../../Science/Baseline/data/%s/*', group))
    participants <- basename(subfolders)
    
    for (participant in participants) {
      
      if (participant %in% participants) {
        # great
      } else {
        cat(sprintf('%s\n',participant))
      }
      
    }
    
  }
  
}
