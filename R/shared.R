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
  passive_reaches<<- removeReachOutliers(Loaddata())
  passive_localization<<- removeReachOutliers(Loaddata(task = 'localization'))
  exposure_localization<<- removeReachOutliers(Loaddata(group='exposure', task = 'localization'))
  terminal_localization<<- removeReachOutliers(Loaddata(group='terminal', task = 'localization'))
  terminal_reaches<<- removeReachOutliers(Loaddata(group='terminal'))
  exposure_reaches<<- removeReachOutliers(Loaddata(group='exposure'))
  
  variation_reaches<<- removeReachOutliers(Loaddata(group='variation'))
  variation_localization<<- removeReachOutliers(Loaddata(group='variation', task = 'localizations'))
  }

downloadOSFdata <- function(update=FALSE) {
  
  # this pulls data from the OSF repo:
  files <- c('passive_localization.csv' = 'https://osf.io/27v54/download',
             'passive_reaches.csv'      = 'https://osf.io/mq5av/download',
             'terminal_reaches.csv'     = 'https://osf.io/qdk9y/download',
             'terminal_localization.csv'= 'https://osf.io/6r4bx/download',
             'exposure_reaches.csv'     = 'https://osf.io/6cmns/download',
             'exposure_localization.csv'= 'https://osf.io/er6u2/download',
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

loadcolors <- function() {
  ##Active
  colorA       <<- rgb(1.0, 0.4, 0.0)         # orange
  colorA_trans <<- rgb(1.0, 0.4, 0.0, 0.2)     # transparent orange
  
  
  ## Passive
  colorPA       <<- rgb(0.7, 0.0, 0.7)          # purple
  colorPA_trans <<- rgb(0.7, 0.0, 0.7, 0.2)     # transparent purple
  
  
  ## Pause
  colorNL       <<- rgb(0.63, 0.71, 0.81)      # blue-gray
  colorNL_trans <<- rgb(0.63, 0.71, 0.81, 0.2)  # transparent blue-gray
  
  
  ##No-Cursor
  colorNC       <<- rgb(0.0, 0.7, 0.0)         # green
  colorNC_trans <<- rgb(0.0, 0.7, 0.0, 0.2)     # transparent green
  
  ##New No-Cursor
  colorNNC       <<- rgb(0.1, 0.3, 0.5)         # purple
  colorNNC_trans <<- rgb(0.1, 0.3, 0.5, 0.2)     # transparent purple
  
  #Terminal
  colorT       <<- rgb(1, 0.0, 0.0)         # Red
  colorT_trans <<- rgb(1, 0.0, 0., 0.2)     # transparent Red
  
  ##Exposure
  colorE       <<- rgb(0.85, 0.65, 0.12)         # Yellow
  colorE_trans <<- rgb(0.85, 0.65, 0.12, 0.2)     # transparent Yellow
}