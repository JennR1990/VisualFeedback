# Codes to create final plots for manuscript -----




VisualFeedbackReaches <- function () {
  PlotoutLine(passive_reaches, 10:14, c(1,5,6,3,2), "Reach Trials")
  PlotData(passive_reaches, 5, 5 )
  PlotData(terminal_reaches, 6, 6)

}

neuromatchReaches <- function () {
  PlotoutLine(passive_reaches, c(6,12,15, 18:20), c(5,6,7, 8,8,8), "Reach Trials", type = c(1,1,1,4,2,3))
  PlotData(passive_reaches, 5, 5 )
  PlotData(terminal_reaches, 6, 6)
  
  dataCIs <- trialCI(data = exposure_reaches)
  dataCIs <- dataCIs * 1
  exposure_reaches["distortion"][is.na(exposure_reaches["distortion"])] <- 0
  exposure_reaches$Mean <-
    rowMeans(exposure_reaches[, 2:ncol(exposure_reaches)], na.rm = TRUE)
  x <- c(c(241:288),rev(241:288))
  y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
  polygon(x, y, col = colorE_trans, border = NA)
  lines(x=241:288,y=exposure_reaches$Mean * 1, col = colorE, lwd = 1.5, lty = 1 )
  
  reaches <- getreachesformodel(passive_reaches)
  reach_par <-
    fitTwoRateReachModel(
      reaches = reaches$meanreaches,
      schedule = reaches$distortion,
      oneTwoRates = 2,
      grid = 'restricted',
      checkStability = TRUE
    )
  reach_model <- twoRateReachModel(par = reach_par, schedule = reaches$distortion)
   lines(reach_model$total * -1, col = colorPA,lty = 4)
  lines(reach_model$slow * -1, col = colorPA,lty = 2)
  lines(reach_model$fast * -1, col = colorPA,lty = 3)
  
  
  reaches <- getreachesformodel(terminal_reaches)
  reach_par <-
    fitTwoRateReachModel(
      reaches = reaches$meanreaches,
      schedule = reaches$distortion,
      oneTwoRates = 2,
      checkStability = TRUE, grid = 'restricted'
    )
  reach_model <- twoRateReachModel(par = reach_par, schedule = reaches$distortion)
    lines(reach_model$total * -1, col = colorT,lty = 4)
  lines(reach_model$slow * -1, col = colorT,lty = 2)
  lines(reach_model$fast * -1, col = colorT,lty = 3)
  
  
}

ReachAfterEffectReaches <- function (acd, ncd, ncdI) {
  PlotoutLine(acd, 4:5, 3:4, "Reach Trials")
  PlotData(ncd, 3, 3)
  PlotData(ncdI, 4, 4)
}

Localizations1 <- function (pl, tl , expl) {
  PlotoutLine(pl, c(6,12,15), 5:7, "Hand Localizations")
  PlotData(pl, 5, 5,1)
  PlotData(tl, 6, 6,1)
  PlotData(expl, 7, 7,1)
  
  reaches <- getreachesformodel(passive_reaches)
  reach_par <-
    fitTwoRateReachModel(
      reaches = reaches$meanreaches,
      schedule = reaches$distortion,
      oneTwoRates = 2,
      grid = 'restricted',
      checkStability = TRUE
    )
  reach_model <- twoRateReachModel(par = reach_par, schedule = reaches$distortion)
  lines(reach_model$total * -1, col = colorPA,lty = 4)
  lines(reach_model$slow * -1, col = colorPA,lty = 2)
  lines(reach_model$fast * -1, col = colorPA,lty = 3)
  
}


ExposureDatas <- function (expl, exprr) {
  PlotoutLine(expl, 16:17,c(7,7), "Exposure Testing Trial")

  
  dataCIs <- trialCI(data = exprr)
  dataCIs <- dataCIs * 1
  exprr["distortion"][is.na(exprr["distortion"])] <- 0
  exprr$Mean <-
    rowMeans(exprr[, 2:ncol(exprr)], na.rm = TRUE)
  x <- c(c(241:288),rev(241:288))
  y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
  polygon(x, y, col = rgb(0.44, 0.51, 0.57,0.2), border = NA)
  lines(x=241:288,y=exprr$Mean * 1, col = rgb(0.44, 0.51, 0.57), lwd = 1.5, lty = 1 )
  PlotData(expl, 7, 7, 1)

}


Localizations <- function (pl, tl , expl, al) {
  PlotoutLine(pl, 10:13, c(1,5:7), "Hand Localizations")
  PlotData(pl,5, 5,1)
  PlotData(tl, 6, 6,1)
  PlotData(expl, 7, 7,1)
  PlotData(al, 1, 1,1)
}


RAENewPlots <- function (acd, ncnc) {
  PlotRAEoutLine(acd, 4, 3, "Instructed & Uninstructed No-Cursors")
  PlotData(ncnc, 3, 3, x =  c(c(33:288), rev(c(33:288))))
}





ReachAfterEffects <- function (acd,ncd_NC, ncdI) {
  PlotoutLine(acd, 4:5, 3:4, "Reach Aftereffects")
  PlotData(ncd_NC, 3, 3, x =  c(c(33:288), rev(c(33:288))))
  PlotData(ncdI, 4, 4, x =  c(c(33:288), rev(c(33:288))))
}

RAEPlots <- function (acd, ncd, ncdI, ncnc, ncinc) {
  PlotRAEoutLine(acd, 4:6, 3:5, "Instructed & Uninstructed No-Cursors")
  PlotData(ncd, 3, 3)
  PlotData(ncdI, 4, 4)
  PlotData(ncnc, 3, 3, x =  c(c(33:288), rev(c(33:288))), line = 5)
  PlotData(ncinc, 4, 4, x =  c(c(33:288), rev(c(33:288))), line = 5)
}


plotpropmodels<- function (){
  output1<-fitPropModel(passive_reaches, passive_localization)
  output2<-fitPropModel(terminal_reaches, terminal_localization)
  output3<-fitPropModel(exposure_reaches, exposure_localization, exp = 2)
  #output4<-fitPropModel(active_reaches, active_localization)
  
  plot(output1,
    ylim = c(-15, 15),
    xlab = "Trial",
    lwd = 1,
    ylab = "Change in Hand Estimates [°]",
    col = colorPA,
    axes = FALSE,
    main = 'Proportional Models',
    type = 'l', 
    cex.lab = 1.5,
    cex.main = 1.5
  )
  axis(
    1,
    at = c(1, 64, 224, 240, 288),
    cex.axis = 1.5,
    las = 2
  )
  axis(2, at = c(-15, -10,-5,0, 5,10,15), cex.axis = 1.5, las = 2)
  legend(0, 3, legend = c('Continous Group (21%)', 'Terminal Group (19%)', 'Exposure Group (16%)'), col = c(colorPA, colorT, colorE), 
         lty = c(1,1,1), lwd = 2, bty = 'n', cex = 1.5)
  lines(output2, col = colorT, lwd = 1)
  lines(output3, col = colorE, lwd = 1)
  #lines(output4, col = colorA, lwd = 1)
}



ReachmodelCTs <- function() {
  grid <- 'restricted'
  reaches <- getreachesformodel(passive_reaches)
  reach_par <-
    fitTwoRateReachModel(
      reaches = reaches$meanreaches,
      schedule = reaches$distortion,
      oneTwoRates = 2,
      grid = grid,
      checkStability = TRUE
    )
  reach_model <- twoRateReachModel(par = reach_par, schedule = reaches$distortion)
  PlotCToutLine(active_reaches, 7:11, 6:10, 'Reaches')
    lines(reach_model$total * -1, col = colorPA,lty = 4)
    lines(reach_model$slow * -1, col = colorPA,lty = 2)
    lines(reach_model$fast * -1, col = colorPA,lty = 3)
    
    reaches1 <- getreachesformodel(terminal_reaches)
    reach_par1 <-
      fitTwoRateReachModel(
        reaches = reaches1$meanreaches,
        schedule = reaches1$distortion,
        oneTwoRates = 2,
        grid = grid,
        checkStability = TRUE
      )
    reach_model1 <- twoRateReachModel(par = reach_par1, schedule = reaches1$distortion)
    lines(reach_model1$total * -1, col = colorT,lty = 4)
    lines(reach_model1$slow * -1, col = colorT,lty = 2)
    lines(reach_model1$fast * -1, col = colorT,lty = 3)
    


}

PlotoutLine <- function(dataset, exp, color,title, type = c(1)) {
  labels <-
    list (
      'Active Localization Group (N=32)', #orange
      'Passive Localization Group (N=32)', #purple
      'Pause Group (N=32)', #steel blue
      'No-Cursor(N=32)', #blue
      'No-Cursor Instructed(N=15)', #Green
      'Continous (N=32)',
      'Terminal(N=32)', #Red
      'Exposure', #Yellow
      'Passive Localizations (N=32)',
      'Active (N=32)',
      'Passive (N=32)',
      'Terminal (N=32)',
      'No-Cursor (N=47)',
      'Pause (N=32)',
      'Exposure (N=32)',
      'reaches',
      'localizations',
      'model',
      'slow',
      'fast'
      
      
    )
  colorlist <- list(colorA, colorNL, colorNC, colorNNC, colorPA, colorT, colorE, 'black')
  label <- labels[exp]
  colors <- colorlist[color]
  dataCIs <- trialCI(data = dataset)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <-
    rowMeans(dataset[, 2:length(dataset)], na.rm = TRUE)
  plot(
    NULL,
    ylim = c(-35, 35),
    xlab = "Trial",
    ylab = "Hand Direction [°]",
    axes = F,
    main = title,
    type = 'l',
    col = 'white', 
    cex.lab = 1.5,
    cex.main = 1.5, xlim = c(0,288)
  )
  lines(c(1, 64, 64, 224, 224, 240, 240),
        c(0, 0, 30, 30, -30, -30, 0),
        col = rgb(0., 0., 0.))
  lines(c(240, 288),
        c(0, 0),
        lty = 2,
        col = rgb(0., 0., 0.))
  legend(
    0,
    -5,
    legend = c(label),
    col = c(unlist(colors)),
    lty = c(type),
    lwd = c(2),
    bty = 'n', 
    cex = 1
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5,
       las = 2)
  axis(1, at = c(1, 64, 224, 240, 288), cex.axis = 1.5, las = 2)
}



PlotRAEoutLine <- function(dataset, exp, color,title) {
  labels <-
    list (
      'Active Localization Group (N=32)', #orange
      'Passive Localization Group (N=32)', #purple
      'Pause Group (N=32)', #steel blue
      'No-Cursor (N=32)', #blue
      'No-Cursor Instructed Group (N=16)', #Green
      'No-Cursor Data',
      'Continous Group (N=32)',
      'Terminal Group (N=32)', #Red
      'Exposure Group (N=32)', #Yellow
      'Active Localizations (N=32)',
      'Passive Localizations (N=32)',
      'Continous Localizations (N=32)',
      'Terminal Localizations (N=32)',
      'Exposure Localizations (N=32)'
      
    )
  colorlist <- list(colorA, colorNL, colorNC, colorNNC, 'Black', colorPA, colorT, colorE)
  label <- labels[exp]
  colors <- colorlist[color]
  dataCIs <- trialCI(data = dataset)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <-
    rowMeans(dataset[, 2:length(dataset)], na.rm = TRUE)
  plot(
    dataset$Mean,
    ylim = c(-35, 35),
    xlab = "Trial",
    ylab = "Hand Direction [°]",
    axes = F,
    main = title,
    type = 'l',
    col = 'white', 
    cex.lab = 1.5,
    cex.main = 1.5
  )
  lines(c(1, 64, 64, 224, 224, 240, 240),
        c(0, 0, 30, 30, -30, -30, 0),
        col = rgb(0., 0., 0.))
  lines(c(240, 288),
        c(0, 0),
        lty = 2,
        col = rgb(0., 0., 0.))
  legend(
    -10,
    -5,
    legend = c(label),
    col = c(unlist(colors)),
    lty = c(1,1,5),
    lwd = c(2),
    bty = 'n', 
    cex = 1.5
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5,
       las = 2)
  axis(1, at = c(1, 64, 224, 240, 288), cex.axis = 1.5, las = 2)
}



PlotallreachesCI <-
  function (acd = dataset1,
            pad = dataset2,
            nld = dataset3,
            ncd = dataset4,
            ncdI = dataset5) {
    PlotoutLine(acd, 1:5, 1:5, "Training Trials")
    PlotData(acd, 1, 1)
    PlotData(pad, 2, 2)
    PlotData(nld, 3, 3)
    PlotData(ncd, 4, 4)
    PlotData(ncdI, 5, 5)
  }


RegressionPLot <- function(exp) {
  if (exp == 1) {
    PRrm <- TCombine(passive_reaches)
    PRRm <- PRrm$EC_Late * -1
    PPec <- TCombine(passive_localization)
    PPec <- PPec$EC_Late
    plot(
      PPec ~ PRRm,
      col = colorPA,
      xlab = 'Reaches',
      ylab = 'Localization',
      main = 'Localization ~ Reaches During Error Clamp',
      xlim = c(-12, 25),
      ylim = c(-12, 25),
      axes = FALSE
    )
    axis(2,
         at = c(-30, -20, -10, 0, 10, 20, 30),
         cex.axis = 0.75)
    axis(1,
         at = c(-30, -20 - 10, 0, 10, 20, 30),
         cex.axis = 0.75)
    plotRegressionWithCI(PRRm, PPec, colors = c(colorPA_trans, colorPA))
    
    
    Arm <- TCombine(active_reaches)
    ARm <- Arm$EC_Late * -1
    APec <- TCombine(active_localization)
    APec <- APec$EC_Late
    points(APec ~ ARm, col = colorA)
    plotRegressionWithCI(ARm, APec, colors = c(colorA_trans, colorA))
    
    PARrm <- TCombine(pause_reaches[33:320,])
    PARrm <- PARrm[-13,]
    PARRm <- PARrm$EC_Late * -1
    PAPec <- colMeans(Pause[1:32, 2:32], na.rm = TRUE)
    points(PAPec ~ PARRm, col = colorNL)
    plotRegressionWithCI(PARRm, PAPec, colors = c(colorNL_trans, colorNL))
    legend(
      -14,
      23,
      legend = c(
        'Passive Localization',
        'Active Localization',
        'Pause'
      ),
      col = c(colorPA, colorA, colorNL),
      lty = c(1, 1, 1),
      lwd = c(2, 2, 2),
      bty = 'n'
    )
    
    
    
  } else if (exp == 2) {
    PRrm <- TCombine(pause_reaches[33:320,])
    PRrm <- PRrm[-13,]
    PRRm <- PRrm$EC_Late * -1
    PPec <- colMeans(Pause[1:32, 2:32], na.rm = TRUE)
    plot(
      PPec ~ PRRm,
      col = colorNL,
      xlab = 'Reaches',
      ylab = 'Localization',
      main = 'Localization ~ Reaches During Error Clamp',
      xlim = c(-12, 25),
      ylim = c(-12, 25),
      axes = FALSE
    )
    axis(2, at = c(-20, -10, 0, 10, 20), cex.axis = 0.75)
    axis(1, at = c(-10, 0, 10, 20, 30), cex.axis = 0.75)
    plotRegressionWithCI(PRRm, PPec, colors = c(colorNL_trans, colorNL))
    NCrm <- TCombine(nocursor_reaches[33:320,])
    NCRm <- NCrm$EC_Late * -1
    NCPec <- colMeans(NoCursor[1:32, 2:33], na.rm = TRUE)
    points(NCPec ~ NCRm, col = colorNC)
    plotRegressionWithCI(NCRm, NCPec, colors = c(colorNC_trans, colorNC))
    NCIrm <- TCombine(nocursorI_reaches[33:320,])
    NCIRm <- NCIrm$EC_Late * -1
    NCIPec <- colMeans(NewNoC[1:32, 2:17], na.rm = TRUE)
    points(NCIPec ~ NCIRm, col = colorNNC)
    plotRegressionWithCI(NCIRm, NCIPec, colors = c(colorNNC_trans, colorNNC))
    legend(
      -14,
      23,
      legend = c('New No-Cursor', 'No-Cursor', 'Pause'),
      col = c(colorNNC, colorNC, colorNL),
      lty = c(1, 1, 1),
      lwd = c(2, 2, 2),
      bty = 'n'
    )
    
  }
}

RegressionPLotec <- function() {

    PRrm <- TCombine(passive_reaches)
    PRRm <- PRrm$EC_Late * -1
    PPec <- TCombine(passive_localization)
    PPec <- PPec$EC_Late
    plot(
      PPec ~ PRRm,
      col = colorPA_trans,
      xlab = 'Reaches',
      ylab = 'Localization',
      main = 'Localization ~ Reaches During Error Clamp',
      xlim = c(-30, 30),
      ylim = c(-30, 30),
      axes = FALSE,
      pch = 19, cex.lab = 1.5, cex.main = 1.5, asp = 1
    )
    axis(2,
         at = c( -30,-20,-10, 0, 10, 20, 30),
         cex.axis = 1.5, las =2)
    axis(1,
         at = c(-30,-20,- 10, 0, 10, 20, 30),
         cex.axis = 1.5)
    plotRegressionWithCI(PRRm, PPec, colors = c(colorPA_trans, colorPA))
    
    
    Arm <- TCombine(terminal_reaches)
    ARm <- Arm$EC_Late * -1
    APec <- TCombine(terminal_localization)
    APec <- APec$EC_Late
    points(APec ~ ARm, col = colorT_trans, pch = 19)
    plotRegressionWithCI(ARm, APec, colors = c(colorT_trans, colorT))
    
    
    ARm <- as.numeric(colMeans(exposure_reaches[33:48,2:33], na.rm = TRUE))*-1
    APec <- TCombine(exposure_localization)
    APec <- APec$EC_Late
    points(APec ~ ARm, col = colorE_trans, pch = 19)
    plotRegressionWithCI(ARm, APec, colors = c(colorE_trans, colorE))

    legend(
      -30,
      35,
      legend = c(
        'Continuous Training',
        'Terminal Training',
        "Exposure Training"
      ),
      col = c(colorPA, colorT, colorE),
      lty = c(1, 1),
      lwd = c(2, 2),
      bty = 'n', cex = 1.5
    )

}

RegressionPLotR1 <- function() {
  PRrm <- TCombine(passive_reaches)
  PRRm <- PRrm$R1_Late * -1
  PPec <- TCombine(passive_localization)
  PPec <- PPec$R1_Late
  plot(
    PPec ~ PRRm,
    col = colorPA,
    xlab = 'Reaches',
    ylab = 'Localization',
    xlim = c(10, 40),
    ylim = c(-5, 25),
    axes = FALSE, asp = 1
  )
  axis(2,
       at = c( -5, 0, 5, 15, 25),
       cex.axis = 0.75)
  axis(1,
       at = c( 15, 20,30,40),
       cex.axis = 0.75)
  lm<-plotRegressionWithCI(PRRm, PPec, colors = c(colorPA_trans, colorPA))
  slopes<-lm$coefficients[2]
  
  Arm <- TCombine(active_reaches)
  Arm <- Arm[-21,]
  ARm <- Arm$R1_Late * -1
  APec <- TCombine(active_localization)
  APec<- APec[-21,]
  APec <- APec$R1_Late
  points(APec ~ ARm, col = colorA)
  gm<-plotRegressionWithCI(ARm, APec, colors = c(colorA_trans, colorA))
  slopes<-c(slopes,gm$coefficients[2])
  # legend(
  #   16,
  #   32,
  #   legend = c(
  #     'Passive Localization',
  #     'Active Localization'
  #   ),
  #   col = c(colorPA, colorA),
  #   lty = c(1, 1),
  #   lwd = c(2, 2),
  #   bty = 'n'
  # )
  names(slopes)<- c('Passive', 'Active')
  return(slopes)
}
RegressionPLotR1E <- function() {
  PRrm <- TCombine(passive_reaches)
  PRRm <- PRrm$R1_Early * -1
  PPec <- TCombine(passive_localization)
  PPec <- PPec$R1_Early
  plot(
    PPec ~ PRRm,
    col = colorPA,
    xlab = 'Reaches',
    ylab = 'Localization',
    xlim = c(3, 35),
    ylim = c(-5, 20),
    axes = FALSE, asp = 1
  )
  axis(2,
       at = c(  0, 10, 20),
       cex.axis = 0.75)
  axis(1,
       at = c( 5,10, 20, 30),
       cex.axis = 0.75)
  lm<-plotRegressionWithCI(PRRm, PPec, colors = c(colorPA_trans, colorPA))
  slopes<-lm$coefficients[2]
  
  Arm <- TCombine(active_reaches)
  #Arm <- Arm[-21,]
  ARm <- Arm$R1_Early * -1
  APec <- TCombine(active_localization)
  #APec<- APec[-21,]
  APec <- APec$R1_Early
  points(APec ~ ARm, col = colorA)
  gm<-plotRegressionWithCI(ARm, APec, colors = c(colorA_trans, colorA))
  slopes<-c(slopes,gm$coefficients[2])
  # legend(
  #   0,
  #   32,
  #   legend = c(
  #     'Passive Localization',
  #     'Active Localization'
  #   ),
  #   col = c(colorPA, colorA),
  #   lty = c(1, 1),
  #   lwd = c(2, 2),
  #   bty = 'n'
  # )
  names(slopes)<- c('Passive', 'Active')
  return(slopes)
}

RegressionPLot3P <- function() {

  PPec <- TCombine(passive_localization)
  loc <- c(PPec$R2,PPec$Aligned,PPec$R1_Late)
  pert<- c(rep(-30, 32),rep(0, 32),rep(30, 32))
  pert1<- c(rep(-29, 32),rep(1, 32),rep(31, 32))
   plot(
    loc ~ pert1,
    col = colorPA_trans,
    xlab = 'Size of Perturbation [°]',
    ylab = 'Change in Hand Localization [°]',
    xlim = c(-32, 32),
    ylim = c(-32, 32),
    main = 'Size of Perturbation Vs. Localizations',
    axes = FALSE, pch = 19, cex.lab = 1.5, cex.main = 1.5
  )
  axis(2,
       at = c(-30,-20, -10,0,10, 20, 30),
       cex.axis = 1.50, las = 2)
  axis(1,
       at = c( -30, 0,  30),
       cex.axis = 1.5)
  lm<-plotRegressionWithCI(pert, loc, colors = c(colorPA_trans, colorPA))
  slopes<-lm$coefficients[2]
  

  APec <- TCombine(active_localization)
  loca <- c(APec$R2,APec$Aligned,APec$R1_Late)
  perta<- c(rep(-30, 32),rep(0, 32),rep(30, 32))
  pertb<- c(rep(-31, 32),rep(-1, 32),rep(29, 32))

  points(loca ~ pertb, col = colorA_trans, pch = 19)
  gm<-plotRegressionWithCI(perta, loca, colors = c(colorA_trans, colorA))
  slopes<-c(slopes,gm$coefficients[2])
  legend(
    -30,
    40,
    legend = c(
      'Passive Localization',
      'Active Localization'
    ),
    col = c(colorPA, colorA),
    lty = c(1, 1),
    lwd = c(2, 2),
    bty = 'n', cex = 1.5
  )
  names(slopes)<- c('Passive', 'Active')
  return(slopes)
}

RegressionPLotchange <- function() {
  
  PPec <- TCombine(passive_localization)
  a<-PPec$Aligned
  b<-PPec$R1_Late
  c<-PPec$R1_Late - PPec$R2
  loc <- c(a,b,c)
  pert<- c(rep(0, 32),rep(30, 32),rep(60, 32))
  pert1<- c(rep(1, 32),rep(31, 32),rep(61, 32))
  plot(
    loc ~ pert1,
    col = colorPA_trans,
    xlab = 'Change in Size of Pertubation [°]',
    ylab = 'Change in Hand Localization [°]',
    xlim = c(0, 60),
    ylim = c(-10, 30),
    main = 'Change in Perturbation Vs. Localizations',
    axes = FALSE,
    pch = 19
  )
  axis(2,
       at = c( -10, 0, 10 ,20, 30),
       cex.axis = 0.75)
  axis(1,
       at = c( 0, 30,  60),
       cex.axis = 0.75)
  lm<-plotRegressionWithCI(pert, loc, colors = c(colorPA_trans, colorPA))
  slopes<-lm$coefficients[2]
  
  
  
  APec <- TCombine(active_localization)
  loca <- c(APec$Aligned,APec$R1_Late,APec$R1_Late - APec$R2)
  perta<- c(rep(0, 32),rep(30, 32),rep(60, 32))  
  pertb<- c(rep(-1, 32),rep(29, 32),rep(59, 32))
  
  points(loca ~ pertb, col = colorA_trans, pch = 19)
  gm<-plotRegressionWithCI(perta, loca, colors = c(colorA_trans, colorA))
  slopes<-c(slopes,gm$coefficients[2])

  legend(
    0,
    32,
    legend = c(
      'Passive Localization',
      'Active Localization'
    ),
    col = c(colorPA, colorA),
    lty = c(1, 1),
    lwd = c(2, 2),
    bty = 'n'
  )
  names(slopes)<- c('Passive', 'Active')
  return(slopes)
}


# Below are the codes to make the above functions run: these make the subplots inside the main plots -----

##These codes make the plots that have confidence intervals and show each experiments learning curves
##this plots reach, localization and no-cursor data.


PlotData <- function(dataset, color, trans, rotate = -1, x =  c(c(1:288), rev(c(1:288))), line = 1) {
  colorlist <- c(colorA, colorNL, colorNC, colorNNC,colorPA,colorT,colorE)
  translist <-
    c(colorA_trans,
      colorNL_trans,
      colorNC_trans,
      colorNNC_trans,
      colorPA_trans,
      colorT_trans,
      colorE_trans)
  dataCIs <- trialCI(data = dataset)
  dataCIs <- dataCIs * rotate
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <-
    rowMeans(dataset[, 2:ncol(dataset)], na.rm = TRUE)
  x <- x
  y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
  polygon(x, y, col = translist[trans], border = NA)
  lines(x[1:length(dataset$Mean)],dataset$Mean * rotate, col = colorlist[color], lwd = 1.5, lty = line )
}

Plotschedule <- function(dataset) {
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  plot(
    dataset$p1,
    ylim = c(-35, 35),
    xlab = "Trial",
    ylab = "Cursor Rotation [°]",
    axes = F,
    main = "Rotation Schedule",
    type = 'l',
    col = 'white', cex.lab = 1.25
  )
  #rect(65,0,68,30, col = 'grey',border = NA)
  #text(67,33,'early', adj = .5)
  rect(221,0,224,30, col = 'grey',border = NA)
  text(223,33,'late', adj = .5)
  #rect(237,-30,240,0, col = 'grey',border = NA)
  #text(239,-33,'reversed', adj = .5)
  rect(273,-15,288,15, col = 'grey',border = NA)
  text(280,18,'clamped', adj = .5)
  lines(c(1, 64, 64, 224, 224, 240, 240),
        c(0, 0, 30, 30, -30, -30, 0),
        col = rgb(0., 0., 0.))
  lines(c(240, 288),
        c(0, 0),
        lty = 2,
        col = rgb(0., 0., 0.))

  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5, las = 2)
  axis(1, at = c(1, 64, 224, 240, 288), cex.axis = 1.5, las = 2)
}
PlotCToutLine <- function(dataset, exp, color,title) {
  labels <-
    list (
      'Active Localization Group (N=32)', #orange
      'Passive Localization Group (N=32)', #purple
      'Pause Group (N=32)', #steel blue
      'No-Cursor Group (N=32)', #blue
      'No-Cursor Instructed Group (N=16)', #Green
      'Exposure Group (N=32)', #Yellow
      'Continous Group (N=32)',
      'Terminal Group (N=32)', #Red
      'fast',
      'slow',
      'output',
      'Active Localizations (N=32)',
      'Passive Localizations (N=32)')
  colorlist <- list(colorA, colorNL, colorNC, colorNNC, colorE,colorPA, colorT, 'black','black','black')
  label <- labels[exp]
  colors <- colorlist[color]
  dataCIs <- trialCI(data = dataset)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <-
    rowMeans(dataset[, 2:length(dataset)], na.rm = TRUE)
  plot(
    dataset$Mean,
    ylim = c(-35, 35),
    xlab = "Trial",
    ylab = "Hand Direction [°]",
    axes = F,
    main = title,
    type = 'l',
    col = 'white', 
    cex.lab = 1.5,
    cex.main = 1.5
  )
  lines(c(1, 64, 64, 224, 224, 240, 240),
        c(0, 0, 30, 30, -30, -30, 0),
        col = rgb(0., 0., 0.))
  lines(c(240, 288),
        c(0, 0),
        lty = 2,
        col = rgb(0., 0., 0.))
  legend(
    -10,
    -5,
    legend = c(label),
    col = c(unlist(colors)),
    lty = c(1,1,3,2,4),
    lwd = c(2),
    bty = 'n', 
    cex = 1.5
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5,
       las = 2)
  axis(1, at = c(1, 64, 224, 240, 288), cex.axis = 1.5, las = 2)
}




trialCI <- function(data) {
  AllCIs <- data.frame()
  for (trial in 1:nrow(data)) {
    y <- unlist(data[trial, 2:length(data)])
    CItrial <- t.interval(unlist(y))
    if (prod(dim(AllCIs)) == 0) {
      AllCIs <- CItrial
    } else {
      AllCIs <- rbind(AllCIs, CItrial)
    }
  }
  return(AllCIs)
}


t.interval = function(data,
                      variance = var(data, na.rm = TRUE),
                      conf.level = 0.95) {
  z = qt((1 - conf.level) / 2,
         df = length(data) - 1,
         lower.tail = FALSE)
  
  xbar = mean(data, na.rm = TRUE)
  sdx = sqrt(variance / length(data))
  
  return(c(xbar - z * sdx, xbar + z * sdx))
  
}


## This plots the data without a confidence interval but will run the model and add the output to the figure. ----
## It reruns the model everytime you plot so it does take a second or two.

Reachmodel <- function(data, name, grid = 'restricted', condition = 'Reach', ncdata = NA, loc_data = NA, color) {
  grid <- grid
  reaches <- getreachesformodel(data)
  reach_par <-
    fitTwoRateReachModel(
      reaches = reaches$meanreaches,
      schedule = reaches$distortion,
      oneTwoRates = 2,
      grid = grid,
      checkStability = TRUE
    )
  reach_model <-
    twoRateReachModel(par = reach_par, schedule = reaches$distortion)
  
  if (condition == 'nc'){
    reach_model <- reach_model[33:320, ]
    Plotncmodel(data[33:320, ], name, color)
    lines(reach_model$total * -1, col = color,lty = 4)
    lines(reach_model$slow * -1, col = color,lty = 2)
    lines(reach_model$fast * -1, col = color,lty = 3)
    ncreaches <- getreachesformodel(ncdata)
    lines(x = 33:288, y = ncreaches$meanreaches * -1, col = color)
    
  } else if (condition == 'loc') {
    Plotlocmodel(data, name, color)
    lines(reach_model$total * -1, col = color,lty = 4)
    lines(reach_model$slow * -1, col = color,lty = 2)
    lines(reach_model$fast * -1, col = color,lty = 3)
    lines(rowMeans(loc_data[, 2:ncol(loc_data)], na.rm = TRUE), col = color)
  } else{
    Plotmodel(data, name, color)
    lines(reach_model$total * -1, col = color,lty = 4)
    lines(reach_model$slow * -1, col = color,lty = 2)
    lines(reach_model$fast * -1, col = color,lty = 3)
  }
  

  MSE<- twoRateReachModelErrors(reach_par, reaches = reaches$meanreaches,schedule = reaches$distortion )
  RMSE<- sqrt(MSE)
  pars<-c(reach_par,MSE, RMSE)
  names(pars)[5]<-"MSE"
  names(pars)[6]<-"RMSE"
  return(pars)
}

Reachmodelnc <- function(data, ncdata, name, color) {
  reaches <- getreachesformodel(data)
  reach_par <-
    fitTwoRateReachModel(
      reaches = reaches$meanreaches,
      schedule = reaches$distortion,
      oneTwoRates = 2,
      grid = 'skewed',
      checkStability = TRUE
    )
  reach_model1 <-
    twoRateReachModel(par = reach_par, schedule = reaches$distortion)
  reach_model <- reach_model1[33:320, ]
  Plotncmodel(data[33:320, ], name, color)
  lines(reach_model$total * -1, col = color,lty = 4)
  lines(reach_model$slow * -1, col = color,lty = 2)
  lines(reach_model$fast * -1, col = color,lty = 3)
  ncreaches <- getreachesformodel(ncdata)
  lines(x = 33:288, y = ncreaches$meanreaches * -1, col = color)
  return(reach_par)
}



Plotmodel <- function(dataset, name, color) {
  title <- sprintf('%s Testing Trial', name)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[, 2:ncol(dataset)], na.rm = TRUE)
  plot(
    dataset$Mean * -1,
    ylim = c(-35, 35),
    xlab = "Trial",
    lwd = 2,
    ylab = "Hand Direction [°]",
    col = c(rgb(0.8, 0.8, 0.8)),
    axes = FALSE,
    main = title,
    type = 'l', 
    cex.lab = 1.5,
    cex.main = 1.5
  )
  lines(c(1, 64, 64, 224, 224, 240, 240),
        c(0, 0, 30, 30, -30, -30, 0),
        col = rgb(0., 0., 0.))
  lines(c(240, 288),
        c(0, 0),
        lty = 2,
        col = rgb(0., 0., 0.))
  legend(
    -10,
    0,
    legend = c('Reach data', 'model', 'fast', 'slow'),
    col = c(
      rgb(0.44, 0.51, 0.57),
      color,
      color,
      color
    ),
    lty = c(1, 4, 3, 2),
    lwd = c(2, 2, 2, 2),
    bty = 'n', 
    cex = 1.5
  )
  axis(
    1,
    at = c(1, 64, 224, 240, 288),
    cex.axis = 1.5,
    las = 2
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5, las = 2)
  lines(dataset$Mean * -1, col = c(rgb(0.44, 0.51, 0.57)))
}



Plotncmodel <- function(dataset, name, color) {
  title <- sprintf('%s Testing Trial', name)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[, 2:ncol(dataset)], na.rm = TRUE)
  plot(
    dataset$Mean * -1,
    ylim = c(-35, 35),
    xlab = "Trial",
    lwd = 2,
    ylab = "Hand Direction [°]",
    col = c(rgb(0.8, 0.8, 0.8)),
    axes = FALSE,
    main = title,
    type = 'l', 
    cex.lab = 1.5,
    cex.main = 1.5
  )
  lines(c(1, 64, 64, 224, 224, 240, 240),
        c(0, 0, 30, 30, -30, -30, 0),
        col = rgb(0., 0., 0.))
  lines(c(240, 288),
        c(0, 0),
        lty = 2,
        col = rgb(0., 0., 0.))
  legend(
  -10,0,
    legend = c('Reach data', 'No-cursor data', 'model', 'fast', 'slow'),
    col = c(
      rgb(0.44, 0.51, 0.57),
      color,
      color,
      color,
      color
    ),
    lty = c(1, 1, 4, 3, 2),
    lwd = c(2, 2, 2, 2, 2),
    bty = 'n',
    ncol = 2, 
    cex = 1.5
  )
  axis(
    1,
    at = c(1, 64, 224, 240, 288),
    cex.axis = 1.5,
    las = 2
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5, las = 2)
  lines(dataset$Mean * -1, col = c(rgb(0.44, 0.51, 0.57)))
}

Plotlocmodel <- function(dataset, name, color) {
  title <- sprintf('%s Testing Trial', name)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[, 2:ncol(dataset)], na.rm = TRUE)
  plot(
    dataset$Mean * -1,
    ylim = c(-35, 35),
    xlab = "Trial",
    lwd = 2,
    ylab = "Hand Direction [°]",
    col = c(rgb(0.8, 0.8, 0.8)),
    axes = FALSE,
    main = title,
    type = 'l', 
    cex.lab = 1.5,
    cex.main = 1.5
  )
  lines(c(1, 64, 64, 224, 224, 240, 240),
        c(0, 0, 30, 30, -30, -30, 0),
        col = rgb(0., 0., 0.))
  lines(c(240, 288),
        c(0, 0),
        lty = 2,
        col = rgb(0., 0., 0.))
  legend(
    -10,
    0,
    legend = c('Reach data', 'Localization data', 'model', 'fast', 'slow'),
    col = c(
      rgb(0.44, 0.51, 0.57),
      color,
      color,
      color,
      color
    ),
    lty = c(1, 1, 4, 3, 2),
    lwd = c(2, 2, 2, 2, 2),
    bty = 'n',
    ncol = 2, 
    cex = 1.5
  )
  axis(
    1,
    at = c(1, 64, 224, 240, 288),
    cex.axis = 1.5,
    las = 2
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5, las = 2)
  lines(dataset$Mean * -1, col = c(rgb(0.44, 0.51, 0.57)))
}


plotpropmodel<- function (reachdata, locadata){
  
  localizations<-rowMeans(locadata[,2:ncol(locadata)], na.rm=TRUE)
  schedule<- reachdata$distortion
  
  
  par<- fitPropModel(reachdata, locadata)
  plot(localizations, type = 'l', ylim = c(-15,15))
  output<- PropModel(par, schedule)
  lines(output, col = "blue")
  
  
}




## this code runs the linear regression between the two pieces of data you give it and it also plots it for you----
## This is called by the above function RegressionPLot(exp) exp 1 is active passive pause and exp 2 is pause and the two no-cursors

plotRegressionWithCI <-
  function(X, Y, colors = c('#99999999', 'black')) {
    # fit regression model
    this.lm <- lm(Y ~ X)
    
    # where is the interesting data
    pointlocs <- seq(min(X, na.rm = TRUE), max(X, na.rm = TRUE), .1)
    
    # get the confidence interval
    y1 = predict(this.lm,
                 newdata = data.frame(X = pointlocs),
                 interval =
                   "confidence")[, "upr"]
    y2 = predict(this.lm,
                 newdata = data.frame(X = pointlocs),
                 interval =
                   "confidence")[, "lwr"]
    
    # show the confidence interval
    polygon(c(pointlocs, rev(pointlocs)),
            c(y1, rev(y2)),
            col = colors[1],
            border = NA)
    
    # and show a regression line:
    lines(
      range(X, na.rm = TRUE),
      predict(this.lm, newdata = data.frame(X = range(X, na.rm = TRUE))),
      col = colors[2],
      lwd = 2
    )
    return(this.lm)
  }



plotproportionalmodel<- function() {
  
  localizations<-rowMeans(locadata[,2:ncol(locadata)], na.rm=TRUE)
  distortion<- c(rep(0, 64), rep(30, 160), rep (-30,16))
  clampreaches<-rowMeans(reachdata[241:288,2:ncol(reachdata)], na.rm=TRUE)
  clampreaches<- clampreaches*-1
  schedule<- c(distortion,clampreaches)
  
  
  
  
  #this function will take the dataframe made in the last function (dogridsearch) and use the list of parameters to make a new model then compare to output and get a new mse. 
  pargrid <- gridsearch(localizations, schedule, nsteps = 7, topn = 4)
  cat('optimize best fits...\n')
  for (gridpoint in c(1:nrow(pargrid))) { #for each row 
    par<-unlist(pargrid[gridpoint,1]) 
    
    control <- list('maxit'=10000, 'ndeps'=1e-9 )
    fit <- optim(par=par, PropModelMSE, gr=NULL, schedule, localizations, control=control, method = "Brent", lower = 0, upper = 1)
    optpar<- fit$par
    
    
    # stick optpar back in pargrid
    pargrid[gridpoint,1] <- optpar
    
    pargrid[gridpoint,2]<- fit$value
    
  } 
  # get lowest MSE, and pars that go with that
  bestpar <- order(pargrid[,2])[1]
  plot(localizations, type = 'l',  ylim = c(-15,15), axes = FALSE, main = title, ylab = 'Change in Hand Localizations [°]', xlab = "Trial", col = color, cex.lab = 1.5, cex.main = 1.5)
  axis(
    1,
    at = c(1, 64, 224, 240, 288),
    cex.axis = 1.5,
    las = 2
  )
  axis(2, at = c(-15, -10,-5,0, 5,10,15), cex.axis = 1.5, las = 2)
  output<- PropModel(unlist(pargrid[bestpar]), schedule)
  lines(output, col = 'black')
  #lines(localizations, col = color)
  proportion<- sprintf('Proportion = %f', unlist(pargrid[bestpar]))
  print(proportion)
  legend(0,-5, legend = c('Localization data', 'Proportional output'), col = c(color, 'black'), lty = 1,lwd = 2, bty = 'n')
  #legend(-10, -2, legend = c('Localization data', 'proportional', 'fast', 'slow'), col = c(color, "black", color, color), lty = c(1,1,3,2), lwd = 2, bty = 'n', cex = 1.5, ncol =  2)
  #text(144, 0, labels = proportion)
  
  # reaches <- getreachesformodel(reachdata)
  # reach_par <-
  #   fitTwoRateReachModel(
  #     reaches = reaches$meanreaches,
  #     schedule = schedule,
  #     oneTwoRates = 2,
  #     checkStability = TRUE
  #   )
  # reach_model <-
  #   twoRateReachModel(par = reach_par, schedule = schedule)
  # Average<- mean(localizations[182:224], na.rm = TRUE)
  # Scale<- Average/30
  # reach_model$slow<- reach_model$slow*Scale
  # reach_model$fast<- reach_model$fast*Scale
  #lines(reach_model$slow * -1, col = color,lty = 2)
  #lines(reach_model$fast * -1, col = color,lty = 3)
  
  # return(those pars)
  return(unlist(pargrid[bestpar]))
  
}


plotfitPropModel<- function(reachdata, locadata, color, title, exp = 'exp') {
  
  localizations<-rowMeans(locadata[,2:ncol(locadata)], na.rm=TRUE)
  distortion<- c(rep(0, 64), rep(30, 160), rep (-30,16))
  clampreaches<-rowMeans(reachdata[241:288,2:ncol(reachdata)], na.rm=TRUE)
  clampreaches<- clampreaches*-1
  schedule<- c(distortion,clampreaches)


  
  
  #this function will take the dataframe made in the last function (dogridsearch) and use the list of parameters to make a new model then compare to output and get a new mse. 
  pargrid <- gridsearch(localizations, schedule, nsteps = 7, topn = 4)
  cat('optimize best fits...\n')
  for (gridpoint in c(1:nrow(pargrid))) { #for each row 
    par<-unlist(pargrid[gridpoint,1]) 
    
    control <- list('maxit'=10000, 'ndeps'=1e-9 )
    fit <- optim(par=par, PropModelMSE, gr=NULL, schedule, localizations, control=control, method = "Brent", lower = 0, upper = 1)
    optpar<- fit$par
    
    
    # stick optpar back in pargrid
    pargrid[gridpoint,1] <- optpar
    
    pargrid[gridpoint,2]<- fit$value
    
  } 
  # get lowest MSE, and pars that go with that
  bestpar <- order(pargrid[,2])[1]
  plot(localizations, type = 'l',  ylim = c(-15,15), axes = FALSE, main = title, ylab = 'Change in Hand Localizations [°]', xlab = "Trial", col = color, cex.lab = 1.5, cex.main = 1.5)
  axis(
    1,
    at = c(1, 64, 224, 240, 288),
    cex.axis = 1.5,
    las = 2
  )
  axis(2, at = c(-15, -10,-5,0, 5,10,15), cex.axis = 1.5, las = 2)
  output<- PropModel(unlist(pargrid[bestpar]), schedule)
  lines(output, col = 'black')
  #lines(localizations, col = color)
  proportion<- sprintf('Proportion = %f', unlist(pargrid[bestpar]))
  print(proportion)
  legend(0,-5, legend = c('Localization data', 'Proportional output'), col = c(color, 'black'), lty = 1,lwd = 2, bty = 'n')
  #legend(-10, -2, legend = c('Localization data', 'proportional', 'fast', 'slow'), col = c(color, "black", color, color), lty = c(1,1,3,2), lwd = 2, bty = 'n', cex = 1.5, ncol =  2)
  #text(144, 0, labels = proportion)
  
  # reaches <- getreachesformodel(reachdata)
  # reach_par <-
  #   fitTwoRateReachModel(
  #     reaches = reaches$meanreaches,
  #     schedule = schedule,
  #     oneTwoRates = 2,
  #     checkStability = TRUE
  #   )
  # reach_model <-
  #   twoRateReachModel(par = reach_par, schedule = schedule)
  # Average<- mean(localizations[182:224], na.rm = TRUE)
  # Scale<- Average/30
  # reach_model$slow<- reach_model$slow*Scale
  # reach_model$fast<- reach_model$fast*Scale
  #lines(reach_model$slow * -1, col = color,lty = 2)
  #lines(reach_model$fast * -1, col = color,lty = 3)
  
  # return(those pars)
  return(unlist(pargrid[bestpar]))
  
}
PropModel <- function(par, schedule) {
  locest<-c()
  #loop through the perturbations in the schedule:
  for (t in c(1:length(schedule))) {
    # first we calculate what the model does, since the model is proportional, we just multiply the one parameters by the schedule to get what the person should do
    
    locest[t] <- par * schedule[t]
  }
  
  # after we loop through all trials, we return the model output:
  return(locest)
  
}

PropModelMSE <- function(par, schedule, localizations) {
  
  locesti<- PropModel(par, schedule)
  errors <- locesti - localizations
  MSE <- mean(errors^2, na.rm=TRUE)
  
  
  
  return( MSE )
  
}


gridsearch<- function(localizations, schedule, nsteps=7, topn=4) {
  
  
  cat('doing grid search...\n')
  
  steps <- nsteps #say how many points inbetween 0-1 we want
  pargrid <- seq(0.5*(1/steps),1-(0.5*(1/steps)),by=1/steps) #not sure what exactly this does
  MSE<- rep(NA, length(pargrid))
  pargrid<- cbind(pargrid, MSE)
  
  for (gridpoint in c(1:nrow(pargrid))) { #for each row 
    par<-unlist(pargrid[gridpoint,1])    #take that row and take it out of df and make it par 
    pargrid[gridpoint,2] <- PropModelMSE(par, schedule,localizations)
  }
  
  bestN <- order(pargrid[,2])[1:topn]
  
  return(pargrid[bestN,])
}

## This one plots the invidual traces of each participant over the group average. ----

PlotIndividualdata <- function (data, exp, title) {
  labels <-
    list (
      'Active Localization Group (N=32)',
      'Passive Localization Group (N=32)',
      'No Localization Group (N=32)',
      'No-Cursor Group (N=32)',
      'Instructed No-Cursor Group (N=32)'
    )
  PlotoutLine(data, exp, exp, title)
  PlotData(data, exp, exp)
  subdata <- data * -1
  participants <- 2:ncol(data)
  for (pn in participants) {
    lines(subdata[, pn], col = rgb(0.0, 0.7, 0.0, 0.06))
  }
  PlotData(data, exp, exp)
}


## This plots the pre and post localization data from the pause, and two no-cursor paradigms -----
###Compare pre- post localization
#layout(c(1,2,3))
averagedprepost<- function (dataset = c('Pause', 'NoCursor', 'NewNoC')) {
  svglite(file='doc/Pre_Post_Data.svg', width=6, height=9, system_fonts=list(sans = "Arial"))
  layout(c(1,2,3), heights = c(2,2,2))
  
  exp = list('No-Localization Task','No-Cursor Task', 'New No-Cursor Task')
  counter<- 1
  
  for (data in dataset) {
    filename<- sprintf('data/%s_pre_post_Prop.csv', data)
    df<- read.csv(filename, sep = ',', header = TRUE)
    pre<- mean(unlist(df[56:64, 2:ncol(df)]), na.rm = TRUE)
    post<- mean(unlist(df[65:73, 2:ncol(df)]), na.rm = TRUE)
    stuff<- c(pre, post)
    print(stuff)
    barplot(stuff, ylim = c(-1.5,1.5), names = c('Pre', 'Post'), col= c('mediumorchid3', 'red'))
    text(.75,pre+.1, labels = pre)
    text(2,post+.1, labels = post)
    title( main = exp[counter] )
    counter<- counter + 1
  }
  dev.off()
}









