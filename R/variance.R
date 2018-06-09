
source('R/dataCollection.R')

getLocalizationVariance <- function(df) {
  
  participant <- unique(df$participant)
  group <- c()
  
  probs=c(.025, .500, .975)
  
  allCIs <- NA
  
  for (pp.id in participant) {
    
    ppCI <- c()
    
    group <- c(group, df$group[which(df$participant == pp.id)[1]])
    
    for (active in c(1,0)) {
      
      rows <- which(df$participant == pp.id & df$active_bool == active)
      
      scores <- df$handangle_deg[rows] - df$localization_deg[rows]
      
      score_vars <- bootstrap.var.CI(scores, probs=probs, samples=10000)
      
      names(score_vars) <- sprintf('%s%03d',c('p','a')[active+1],probs*1000)
      
      ppCI <- c(ppCI, score_vars)
      
    }
    
    if (is.data.frame(allCIs)) {
      allCIs <- rbind(allCIs, data.frame(t(data.frame(ppCI))))
    } else {
      allCIs <- data.frame(t(data.frame(ppCI)))
    }
    
  }
  
  CI <- data.frame(group,participant)
  CI <- cbind(CI, allCIs)
  row.names(CI) <- c(1:nrow(CI))
  
  return(CI)
  
}

bootstrap.var.CI <- function(scores, probs, samples) {
  
  resamples <- matrix(sample(scores, size=samples*length(scores), replace=TRUE), ncol=samples, nrow=length(scores))
  return(quantile(t(apply(resamples, 1, var)), probs=probs))
  
}

saveVarCIs <- function() {
  
  dfs <- loadDetrendedData()
  
  for (group in names(dfs)) {
    
    df <- dfs[[group]]
    
    vardf <- getLocalizationVariance(df)
    
    write.csv(vardf, file=sprintf('data/%s_varianceCIs.csv',group), quote=F, row.names=F)

  }
  
}

loadVarCIs <- function() {
  
  dfs <- c()
  
  dfs[['young']] <- read.csv('data/young_varianceCIs.csv', stringsAsFactors=F)
  dfs[['aging']] <- read.csv('data/aging_varianceCIs.csv', stringsAsFactors=F)
  
  return(dfs)
  
}