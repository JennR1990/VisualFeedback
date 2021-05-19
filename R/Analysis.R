## Prep Reach Data for analysis----

PrepdataforT<- function(pasdata, termdata){
  #
  
  Pas_RM<-TCombine(pasdata)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  
  Term_RM<-TCombine(termdata)
  Term_RM$Experiment <- rep('Terminal', nrow(Term_RM))
  
  AllDataRM<- rbind(Pas_RM, Term_RM)
  # 
  return(AllDataRM)
}


PrepdataforANOVA <- function(pasdata, termdata) {
  
  
  Pas_RM<-ANOVAcombine(pasdata)
  Pas_RM$ID <- sprintf('PasLoc.%s',Pas_RM$ID)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  
  Term_RM<-ANOVAcombine(termdata)
  Term_RM$ID <- sprintf('Terminal.%s',Term_RM$ID)
  Term_RM$Experiment <- rep('Terminal', nrow(Term_RM))
  
  
  AllDataRM<- rbind(Pas_RM, Term_RM)
  #
  return(AllDataRM)
  
}


PrepANOVARebounds<- function (pas, term, expo){
  
  
  rebounds<- data.frame()
  Experiment<- c(rep("Passive", times = 32), rep("Terminal", times = 32), rep("Exposure", times = 32))
  ID<- c(rep(1:32,times = 3))
  EC_Late<- colMeans(pas[273:288,2:33], na.rm = TRUE)
  EC_Late<- c(EC_Late,colMeans(term[273:288,2:33], na.rm = TRUE))
  EC_Late<- c(EC_Late,(colMeans(expo[33:48,2:33], na.rm = TRUE)*-1))
  begins<- c(rep(1:32,times = 3))
  ends<-c(rep("p", times = 32), rep("t", times = 32), rep("e", times = 32))
  ID<-paste(begins, ends, sep = ".")
  
  rebounds<-data.frame(cbind(EC_Late, Experiment, ID))
  rebounds$EC_Late<- as.numeric(rebounds$EC_Late)
  
  return(rebounds)
}

PreptoplotRebounds<- function (pas, term, expo){
  
  
  rebounds<- data.frame()
  Experiment<- c(rep("Passive", times = 32), rep("Terminal", times = 32), rep("Exposure", times = 32))
  ID<- c(rep(1:32,times = 3))
  EC_Late<- colMeans(pas[273:288,2:33], na.rm = TRUE)
  EC_Late<- c(EC_Late,colMeans(term[273:288,2:33], na.rm = TRUE))
  EC_Late<- c(EC_Late,(colMeans(expo[33:48,2:33], na.rm = TRUE)*-1))
  EC_Early<- colMeans(pas[241:255,2:33], na.rm = TRUE)
  EC_Early<- c(EC_Early,colMeans(term[241:255,2:33], na.rm = TRUE))
  EC_Early<- c(EC_Early,(colMeans(expo[1:15,2:33], na.rm = TRUE)*-1))
  begins<- c(rep(1:32,times = 3))
  ends<-c(rep("p", times = 32), rep("t", times = 32), rep("e", times = 32))
  ID<-paste(begins, ends, sep = ".")
  
  rebounds<-data.frame(cbind(EC_Late,EC_Early, Experiment, ID))
  rebounds$EC_Late<- as.numeric(rebounds$EC_Late)
  
  return(rebounds)
}


PrepRebounds<- function (pas, term, expo){
  
  
  rebounds<- data.frame()
  #Experiment<- c(rep("Passive", times = 32), rep("Terminal", times = 32), rep("Exposure", times = 32))
  #ID<- c(rep(1:32,times = 3))
  PassEC<- colMeans(pas[273:288,2:33], na.rm = TRUE)
  TermEC<- colMeans(term[273:288,2:33], na.rm = TRUE)
  ExpEC<- colMeans(expo[33:48,2:33], na.rm = TRUE)*-1
  #begins<- c(rep(1:32,times = 3))
  #ends<-c(rep("p", times = 32), rep("t", times = 32), rep("e", times = 32))
  #ID<-paste(begins, ends, sep = ".")
  
  
  
  rebounds<-data.frame(cbind(PassEC, TermEC, ExpEC))
  rebounds$ExpEC<- as.numeric(rebounds$ExpEC)
  rebounds$PassEC<- as.numeric(rebounds$PassEC)
  rebounds$TermEC<- as.numeric(rebounds$TermEC)
  
  return(rebounds)
}

## Prep Prop Data for analysis----


PrepdataforPropT<- function(pasdata, termdata, expdata){
  
  Pas_RM<-TCombine(pasdata)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  Term_RM<-TCombine(termdata)
  Term_RM$Experiment <- rep('Terminal', nrow(Term_RM))
  exp_RM<-TCombine(expdata)
  exp_RM$Experiment <- rep('Exposure', nrow(exp_RM))
  AllDataRM<- rbind( Pas_RM, Term_RM, exp_RM)
  return(AllDataRM)
}

PrepdataforPropANOVA <- function(pasdata, termdata, expdata) {
  
  Pas_RM<-ANOVAcombine(pasdata)
  Pas_RM$ID <- sprintf('PasLoc.%s',Pas_RM$ID)
  Pas_RM$Experiment <- rep('Passive', nrow(Pas_RM))
  
  Term_RM<-ANOVAcombine(termdata)
  Term_RM$ID <- sprintf('TermLoc.%s',Term_RM$ID)
  Term_RM$Experiment <- rep('Terminal', nrow(Term_RM))
  
  Exp_RM<-ANOVAcombine(expdata)
  Exp_RM$ID <- sprintf('ExpLoc.%s',Exp_RM$ID)
  Exp_RM$Experiment <- rep('Exposure', nrow(Exp_RM))
  
  
  
  AllDataRM<- rbind( Pas_RM, Term_RM, Exp_RM)
  
  return(AllDataRM)
  
}


## Anova and T-test Prep codes----

Reboundanalysis<- function(AllDataANOVA){
  
  AllDataANOVA$ID<- as.factor(AllDataANOVA$ID)
  AllDataANOVA$Experiment<- as.factor(AllDataANOVA$Experiment)
  fullmodel <- ezANOVA(data=AllDataANOVA,
                       dv=EC_Late,
                       wid=ID,
                       between = Experiment,
                       type=3,
                       return_aov=TRUE)
  return(fullmodel)
}



ANOVAanalysis<- function(AllDataANOVA){
  AllDataANOVA$ID<- as.factor(AllDataANOVA$ID)
  AllDataANOVA$Experiment<- as.factor(AllDataANOVA$Experiment)
  AllDataANOVA$Time<- as.factor(AllDataANOVA$Time)
  fullmodel <- ezANOVA(data=AllDataANOVA,
                       dv=Deviations,
                       wid=ID,
                       within=Time,
                       between = Experiment,
                       type=3,
                       return_aov=TRUE)
  return(fullmodel)
}





tanalyzedata<- function(AllDataRM){
  IndependentT(AllDataRM, 'Terminal', 'Passive', 'Reaches')
  PairedT(AllDataRM, 'Passive', 'Reaches')
  PairedT(AllDataRM, 'Terminal', 'Reaches')
}



IndependentT<- function(data, exp1, exp2, task) {
  print(sprintf('this is the between subjects comparison of %s to %s %s Data', exp1, exp2, task))
  print('Aligned')
  print(t.test(data$Aligned[data$Experiment == exp1],data$Aligned[data$Experiment == exp2])) #not sig A vs. NC
  print(cohen.d(data$Aligned[data$Experiment == exp1],data$Aligned[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$Aligned[data$Experiment == exp1],data$Aligned[data$Experiment == exp2], na.rm = TRUE))
  print('Beginning of 1st rotation')
  print(t.test(data$R1_Early[data$Experiment == exp1],data$R1_Early[data$Experiment == exp2])) # p-value = 0.04535 A vs. NC
  print(cohen.d(data$R1_Early[data$Experiment == exp1],data$R1_Early[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Early[data$Experiment == exp1],data$R1_Early[data$Experiment == exp2], na.rm = TRUE))
  print('End of 1st rotation')
  print(t.test(data$R1_Late[data$Experiment == exp1],data$R1_Late[data$Experiment == exp2])) # not sig A vs. NC
  print(cohen.d(data$R1_Late[data$Experiment == exp1],data$R1_Late[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Late[data$Experiment == exp1],data$R1_Late[data$Experiment == exp2], na.rm = TRUE))
  print('Beginning of 2nd rotation')
  print(t.test(data$R2[data$Experiment == exp1],data$R2[data$Experiment == exp2])) # not sig  A vs. NC
  print(cohen.d(data$R2[data$Experiment == exp1],data$R2[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$R2[data$Experiment == exp1],data$R2[data$Experiment == exp2], na.rm = TRUE))
  print('Beginning of Error Clamp')
  print(t.test(data$EC[data$Experiment == exp1],data$EC[data$Experiment == exp2])) # p-value = 0.005945  A vs. NC
  print(cohen.d(data$EC[data$Experiment == exp1],data$EC[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC[data$Experiment == exp1],data$EC[data$Experiment == exp2], na.rm = TRUE))
  print('End of Error Clamp (32 trials)')
  print(t.test(data$EC_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp2]))  #p-value = 1.36e-07  A vs. NC
  print(cohen.d(data$EC_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC_Late[data$Experiment == exp1],data$EC_Late[data$Experiment == exp2], na.rm = TRUE))
  
  
}


PairedT<- function(data, exp1, task) {
  print(sprintf('this is the within subjects analysis of %s %s Data', exp1, task))
  print('Is there early learning?')
  print(t.test(data$Aligned[data$Experiment == exp1],data$R1_Early[data$Experiment == exp1], paired = TRUE)) #not sig A vs. NC
  print(cohen.d(data$Aligned[data$Experiment == exp1],data$R1_Early[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))
  print(etaSquaredTtest(data$Aligned[data$Experiment == exp1],data$R1_Early[data$Experiment == exp1], na.rm = TRUE))
   print('Was there learning from beginning to end of 1st rotation?')
  print(t.test(data$R1_Early[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], paired = TRUE)) # p-value = 0.04535 A vs. NC
  print(cohen.d(data$R1_Early[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Early[data$Experiment == exp1],data$R1_Late[data$Experiment == exp1], na.rm = TRUE))
  print('How much could they learn of the 60 degree change?')
  print(t.test(data$R1_Late[data$Experiment == exp1],data$R2[data$Experiment == exp1], paired = TRUE)) # not sig A vs. NC
  print(cohen.d(data$R1_Late[data$Experiment == exp1],data$R2[data$Experiment == exp1], paired = TRUE, na.rm = TRUE)) # not sig A vs. NC
  print(etaSquaredTtest(data$R1_Late[data$Experiment == exp1],data$R2[data$Experiment == exp1], na.rm = TRUE)) # not sig A vs. NC
  print('Is there a change in error clamps across time? (Decay)')
  print(t.test(data$EC[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE)) # p-value = 0.005945  A vs. NC
  print(cohen.d(data$EC[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE, na.rm = TRUE)) # p-value = 0.005945  A vs. NC
  print(etaSquaredTtest(data$EC[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], na.rm = TRUE)) # p-value = 0.005945  A vs. NC
  print('Are the 1st few error clamp trials the same as their aligned behaviour?')
  print(t.test(data$Aligned[data$Experiment == exp1],data$EC[data$Experiment == exp1], paired = TRUE))  #p-value = 1.36e-07  A vs. NC
  print(cohen.d(data$Aligned[data$Experiment == exp1],data$EC[data$Experiment == exp1], paired = TRUE, na.rm = TRUE))  #p-value = 1.36e-07  A vs. NC
  print(etaSquaredTtest(data$Aligned[data$Experiment == exp1],data$EC[data$Experiment == exp1], na.rm = TRUE))  #p-value = 1.36e-07  A vs. NC
  print('Did they eventually decay back to baseline?')
  print(t.test(data$Aligned[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE)) 
  print(cohen.d(data$Aligned[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], paired = TRUE, na.rm = TRUE)) 
  print(etaSquaredTtest(data$Aligned[data$Experiment == exp1],data$EC_Late[data$Experiment == exp1], na.rm = TRUE))

}

## Model Comparison ----
GroupModelAICs <- function(data, group, grid = 'restricted') {
  
  df<- getreachesformodel(data)
  #group='active'# add this to the function call when i use the commented line below
  #df <- read.csv(sprintf('data/%s_reaches.csv', group), stringsAsFactors = FALSE)
  
  schedule <- df$distortion
  
  #Reaches <- as.matrix(df[,2:dim(df)[2]])
  Reaches<- df$meanreaches
  
  twoRateFit <- fitTwoRateReachModel(reaches=Reaches, schedule=schedule, oneTwoRates=2, grid=grid, checkStability=TRUE)
  oneRateFit <- fitTwoRateReachModel(reaches=Reaches, schedule=schedule, oneTwoRates=1, grid=grid, checkStability=TRUE)
  #twoRateFit<- fittworatemodel(Reaches, schedule)
  #oneRateFit<- fitoneratemodel(Reaches, schedule)
  
  
  print(oneRateFit)
  print(twoRateFit)
  
  twoRateMSE <- twoRateReachModelErrors(par=twoRateFit, reaches=Reaches, schedule=schedule)
  oneRateMSE <- twoRateReachModelErrors(par=oneRateFit, reaches=Reaches, schedule=schedule)
  #twoRateMSE <- twoRateReachModelError(par=twoRateFit, reaches=Reaches, distortions =schedule)
  #oneRateMSE <- oneRateReachModelError(par=oneRateFit, reaches=Reaches, distortions =  schedule)
  
  #preparing the AIC stuff
  N <- dim(df)[2] - 1
  # the median length of a phase is 40 trials,
  # and there are 7.2 of those in 288 trials
  InOb <- seriesEffectiveSampleSize(Reaches, method='ac_lag95%CI')
  print(InOb)
  # this is then used for C:
  #C <- InOb*(log(2*pi)+1)
  #twoRateAIC <- (2*4) + InOb*log(twoRateMSE) + C
  #oneRateAIC <- (2*2) + InOb*log(oneRateMSE) + C
  
  twoRateAIC<-(InOb * log(twoRateMSE)) + (2 * 4)
  oneRateAIC<-(InOb * log(oneRateMSE)) + (2 * 2)
  
  cat(sprintf('1-rate AIC: %0.2f  %s  2-rate AIC: %0.2f\n',oneRateAIC,c('>=', ' <')[as.numeric(oneRateAIC<twoRateAIC)+1],twoRateAIC))
  likelihood<-exp((min(c(twoRateAIC, oneRateAIC))-c(twoRateAIC, oneRateAIC))/2)
  print(likelihood)
  #pars<- data.frame(twoRateFit)
  metrics<- data.frame(twoRateAIC, oneRateAIC, likelihood[1], likelihood[2])
  names(metrics)<- c('twoRateAIC', 'oneRateAIC', 'twoRatelikelihood', 'oneratelikelihood')
  #write.csv(AICs, sprintf("ana/AICs/Group AICs for %s Reaches.csv", group), row.names = TRUE, quote = FALSE)
  
  return(metrics)
}




ParticipantReachmodels1<- function (passive,Terminal) {
  #Continuous = 1
  #Terminal = 0
  p_par<- getParticipantFits1(passive)
  p_par$Experiment<-'Continuous'
  p_par$Test_Trial<-1
  t_par<- getParticipantFits1(Terminal)
  t_par$Experiment<-'Terminal'
  t_par$Test_Trial<-0
  
  allpars<- rbind(p_par, t_par)
  return(allpars)
}


getParticipantFits2 <- function(data, grid='restricted') {
  
  participants <- colnames(data)[2:dim(data)[2]]
  distortions <- data$distortion
  
  participantfits <- data.frame(matrix(NA, ncol=6, nrow=length(participants)))
  colnames(participantfits) <- c('participant', 'Rs', 'Ls', 'Rf', 'Lf', 'MSE')
  
  for (ppno in c(1:length(participants))) {
    
    participant <- participants[ppno]
    participant
    reaches <- data[,participant]
    
    #pars<- fitTwoRateReachModel(reaches = reaches, schedule = distortions)
    pars<- fitTwoRateReachModel(reaches=reaches, schedule=distortions, oneTwoRates=2, grid=grid, checkStability=TRUE)
    #pars <- fittworatemodel(reaches, distortions)
    
    participantfits$participant[ppno] <- participant
    participantfits$Rs[ppno] <- pars['Rs']
    participantfits$Ls[ppno] <- pars['Ls']
    participantfits$Rf[ppno] <- pars['Rf']
    participantfits$Lf[ppno] <- pars['Lf']
    participantfits$MSE[ppno] <- twoRateReachModelErrors(pars, reaches, distortions)
    
  }
  
  return(participantfits)
}

getParticipantFits1 <- function(data, grid='restricted') {
  
  participants <- colnames(data)[2:dim(data)[2]]
  distortions <- data$distortion
  
  participantfits <- data.frame(matrix(NA, ncol=4, nrow=length(participants)))
  colnames(participantfits) <- c('participant', 'rs', 'ls', 'MSE')
  
  for (ppno in c(1:length(participants))) {
    
    participant <- participants[ppno]
    participant
    reaches <- data[,participant]
    
    #pars <- fitOneRateReachModel(reaches, distortions)
    pars <- fitTwoRateReachModel(reaches=reaches, schedule=distortions, oneTwoRates=1, grid=grid, checkStability=TRUE)
    
    participantfits$participant[ppno] <- participant
    participantfits$rs[ppno] <- pars['rs']
    participantfits$ls[ppno] <- pars['ls']
    participantfits$MSE[ppno] <- twoRateReachModelErrors(pars, reaches, distortions)
    
  }
  
  return(participantfits)
}


ParticipantReachmodels2<- function (passive,Terminal) {
  #Continuous = 1
  #Terminal = 0
  p_par<- getParticipantFits2(passive)
  p_par$Experiment<-'Continuous'
  p_par$Test_Trial<-1
  t_par<- getParticipantFits2(Terminal)
  t_par$Experiment<-'Terminal'
  t_par$Test_Trial<-0
  
  allpars<- rbind(p_par, t_par)
  return(allpars)
}

pLogRegression <- function(data, variable = Test_Trial) {
  
 
  
  data$variable<- as.factor(data$Test_Trial)
  
  
  
  print(summary(glm(formula = variable ~ Rs + Ls + Rf + Lf, family = binomial(link = "logit"), 
                    data = data)))
  
  model<- glm(formula = variable ~ Rs + Ls + Rf + Lf, family = binomial(link = "logit"), 
              data = data)
  
  #print(predict(model))
  predictions<- predict(model)
  
  predictions[predictions<.5]<-0
  predictions[predictions>.5]<-1
  predictions
  length(which(data$Test_Trial == predictions))
  print(correct<- length(which(data$Test_Trial == predictions)) / length(predictions) )
  
  return(correct)
  
  
  
}

Runlogregression<- function (){
  
  data<-ParticipantReachmodels2(passive_reaches, terminal_reaches)
  
  return(pLogRegression(data))
  
}