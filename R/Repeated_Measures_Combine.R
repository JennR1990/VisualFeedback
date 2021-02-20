##repeated Measures Combine function for T-Tests

TCombine<- function(data) {
  ParticipantRM<- data.frame()
  participants <- c(2:ncol(data))
  for (participant in participants){
    Aligned<- mean(unlist(data[61:64,participant]), na.rm = TRUE)
    r1<- unlist(data[65,participant])
    r2<- unlist(data[66,participant])
    r3<- unlist(data[67,participant])
    r4<- unlist(data[68,participant])
    R1_Early<- mean(unlist(data[65:68,participant]), na.rm = TRUE)
    R1_second<-mean(unlist(data[69:72,participant]), na.rm = TRUE) 
    R1_Late<- mean(unlist(data[221:224,participant]), na.rm = TRUE)
    R2<- mean(unlist(data[237:240,participant]), na.rm = TRUE)
    EC<- mean(unlist(data[241:244,participant]), na.rm = TRUE)
    EC1<- mean(unlist(data[241,participant]), na.rm = TRUE)
    EC2<- mean(unlist(data[242,participant]), na.rm = TRUE)
    EC3<- mean(unlist(data[243,participant]), na.rm = TRUE)
    EC4<- mean(unlist(data[244,participant]), na.rm = TRUE)
    EC_Late<- mean(unlist(data[273:288,participant]), na.rm = TRUE)
    RM<- data.frame(Aligned, r1, r2,r3,r4, R1_Early, R1_second, R1_Late, R2, EC, EC_Late, EC1, EC2, EC3, EC4)
    if (prod(dim(ParticipantRM)) == 0) {
      ParticipantRM <- RM
    } else {
      ParticipantRM <- rbind(ParticipantRM, RM)
    }
  }
  return(ParticipantRM)
}

##repeated measures combine for ANOVAs

ANOVAcombine<- function(data) {
  ParticipantARM<- data.frame()
  participants <- names(data)[2:dim(data)[2]]
  epochs <- list('R1_early'=c(65,4), 'R1_late'=c(221,4), 'R2D2'=c(237,4), 'EC'=c(273,16))
  Deviations<- c()
  Time<- c()
  ID<- c()
  
  for (participant in participants){
    
    participant_reaches <- unlist(data[,participant])
    
    for (epoch in names(epochs)) {
      
      start <- epochs[[epoch]][1]
      finish <- start -1 + epochs[[epoch]][2]
      Deviations <- c(Deviations, mean(participant_reaches[start:finish], na.rm=TRUE))
      Time <- c(Time, epoch)
      ID <- c(ID, participant)
      ANOVARM<- data.frame(Deviations, Time, ID)
    }
  }
  #b<- !is.nan(ANOVARM$Reaches)
 # ANOVARM<- ANOVARM[c(b),]
  return(ANOVARM)
}


