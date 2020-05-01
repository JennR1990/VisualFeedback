getreachesformodel<- function(data) {
  meanreaches<-rowMeans(data[,2:ncol(data)], na.rm=TRUE)
  distortion<- data$distortion
  return(data.frame(meanreaches,distortion))
}

Loaddata<- function (group='passive', task='reaches') {
  # filename <- sprintf('data/%s_%s.csv',group,task)
  # df <- read.csv(filename, stringsAsFactors=F)
  return(read.csv(sprintf('data/%s_%s.csv',group,task), stringsAsFactors=F))
}
 
loadalldata<- function () {
  pause_reaches<<- removeReachOutliers(Loaddata(group='pause'))
  active_reaches<<- removeReachOutliers(Loaddata(group='active'))
  passive_reaches<<- removeReachOutliers(Loaddata())
  nocursor_reaches<<- removeReachOutliers(Loaddata(group='nocursor'))
  nocursorI_reaches<<- removeReachOutliers(Loaddata(group='nocursor', task = 'NI_reaches'))
  passive_localization<<- removeReachOutliers(Loaddata(task = 'localization'))
  active_localization<<- removeReachOutliers(Loaddata(group='active', task = 'localization'))
  nocursor_nocursors<<- removeReachOutliers(Loaddata(group='nocursor', task = 'nocursors'))
  nocursorI_nocursors<<- removeReachOutliers(Loaddata(group='nocursor', task = 'NI_nocursors'))
  exposure_localization<<- removeReachOutliers(Loaddata(group='exposure', task = 'localizations'))
  terminal_localization<<- removeReachOutliers(Loaddata(group='terminal', task = 'localizations'))
  terminal_reaches<<- removeReachOutliers(Loaddata(group='terminal'))
  exposure_reaches<<- removeReachOutliers(Loaddata(group='exposure'))
  newnocursor_reaches<<- cbind(nocursor_reaches, nocursorI_reaches[2:ncol(nocursorI_reaches)])
  newnocursor_nocursors<<- cbind(nocursor_nocursors, nocursorI_nocursors[2:ncol(nocursorI_nocursors)])
  variation_reaches<<- removeReachOutliers(Loaddata(group='variation'))
  variation_localization<<- removeReachOutliers(Loaddata(group='variation', task = 'localizations'))
  }

downloadOSFdata <- function(update=FALSE) {
  
  # this pulls data from the OSF repo:
  files <- c('active_localization.csv'  = 'https://osf.io/mc523/?action=download',
             'active_reaches.csv'       = 'https://osf.io/ejxy9/download',
             'nocursor_nocursors.csv'   = 'https://osf.io/5b8s9/download',
             'nocursor_reaches.csv'     = 'https://osf.io/vmnx7/download',
             'nocursor_NI_nocursors.csv'   = 'https://osf.io/y4k2x/download',
             'nocursor_NI_reaches.csv'     = 'https://osf.io/grnxh/download',
             'passive_localization.csv' = 'https://osf.io/27v54/download',
             'passive_reaches.csv'      = 'https://osf.io/mq5av/download',
             'pause_reaches.csv'        = 'https://osf.io/q59b3/download',
             'terminal_reaches.csv'     = 'https://osf.io/qdk9y/download',
             'terminal_localizations.csv'= 'https://osf.io/6r4bx/download',
             'exposure_reaches.csv'     = 'https://osf.io/6cmns/download',
             'exposure_localizations.csv'= 'https://osf.io/er6u2/download',
             'variation_reaches.csv'     = 'https://osf.io/pk5fy/download',
             'variation_localizations.csv'= 'https://osf.io/txgwj/download'
)

  # check if data directory exists and create if necessary:
  # (data should come from OSF, so is not on github)
  if (!dir.exists('data')) {
    dir.create('data')
  }
  
  # check if each file exists and copy it if necessary: 
  for (filename in names(files)) {
    
    filepath <- sprintf('data/%s',filename)
    
    if (!file.exists(filepath) | update) {
      
      df <- read.csv(url(files[filename]), stringsAsFactors=F)
      write.csv(df, filepath, quote=FALSE, row.names=FALSE)
      
    }
    
  }
  
}


percentNAs <- function (df) {
  return((sum(is.na(df))/prod(dim(df)))*100)
}

# OUTLIER REMOVAL ---------------------------------------------------------

removeSDoutliers <- function(values, sds=3) {
  
  avg <- mean(values, na.rm=TRUE)
  std <- sd(values, na.rm=TRUE) * sds
  
  values[values > avg + std] <- NA
  values[values < avg - std] <- NA
  
  return(values)
  
}

removeIQRoutliers <- function(values, range=3) {
  
  bp <- boxplot(values, range=3, plot=FALSE)
  
  values[values %in% bp$out] <- NA
  
  return(values)
  
}


removeReachOutliers <- function(data) {
  
  ntrials <- nrow(data)
  
  for (trialn in c(1:ntrials)) {
    
    data[trialn,2:ncol(data)] <- removeSDoutliers(as.numeric(data[trialn,2:ncol(data)]))
    
  }
  
  return(data)
  
}