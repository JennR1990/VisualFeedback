neuromatchReaches <- function () {
  PlotoutLine(passive_reaches, c(1:3,6:8), c(1,2,3, 4,4,4), "Reach Trials", type = c(1,1,1,4,2,3))
  PlotData(passive_reaches, 1, 1 )
  PlotData(terminal_reaches, 2, 2)
  
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
  abline(v = 78,col = colorPA)
  abline(v = 85, col = colorT)
  
  
}
PlotoutLine <- function(dataset, exp, color,title, type = c(1), ylabel = "Reach Direction [°]") {
  labels <-
    list (

      'Continous (N=32)',
      'Terminal (N=32)', #Red
      'Exposure (N=32)', #Yellow
      'reaches',
      'localizations',
      'model',
      'slow',
      'fast'
      
      
    )
  colorlist <- list(colorPA, colorT, colorE, 'black')
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
    ylab = ylabel,
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
    -2,
    legend = c(label),
    col = c(unlist(colors)),
    lty = c(type),
    lwd = c(2),
    bty = 'n', 
    cex = 1.2, ncol = 2,
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5,
       las = 2)
  axis(1, at = c(1, 64, 224, 240, 288), cex.axis = 1.5, las = 2)
}

PlotData <- function(dataset, color, trans, rotate = -1, x =  c(c(1:288), rev(c(1:288))), line = 1) {
  colorlist <- c(colorPA,colorT,colorE)
  translist <-
    c(colorPA_trans,
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
  lines(x[1:length(dataset$Mean)],dataset$Mean * rotate, col = colorlist[color], lwd = 1, lty = line )
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

fitPropModel<- function(reachdata, locadata, exp = 1) {
  
  localizations<-rowMeans(locadata[,2:ncol(locadata)], na.rm=TRUE)
  if (exp == 2){
    meanreaches<-rowMeans(reachdata[,2:ncol(reachdata)], na.rm=TRUE)
    distortion<- c(rep(0, 64), rep(30, 160), rep (-30,16))
    schedule<- c(distortion,meanreaches)
  } else {
    meanreaches<-rowMeans(reachdata[241:288,2:ncol(reachdata)], na.rm=TRUE)
    meanreaches<- meanreaches*-1
    reachdata$distortion[241:288]<- as.numeric(meanreaches)
    schedule<- reachdata$distortion
  }
  
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
  print(unlist(pargrid[bestpar]))
  
  output<- PropModel(unlist(pargrid[bestpar]), schedule)
  
  
  # return(those pars)
  #return(unlist(pargrid[bestpar]))
  
  #return the output to use for plots
  return(output)
  
}

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

RegressionPLot <- function() {

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
      xlim = c(-30, 30),
      ylim = c(-20, 20),
      axes = FALSE, asp = 1, cex.lab = 1.25
    )
    axis(2,
         at = c( -20,-10, 0, 10, 20),
         cex.axis = 1.2)
    axis(1,
         at = c(-30,-20,- 10, 0, 10, 20, 30),
         cex.axis = 1.2)
    lines(x = c(-30:30), y = rep(0, times = length(-30:30)), lty = 3)
    abline(v = c(0), lty = 3)
    lm<-plotRegressionWithCI(PRRm, PPec, colors = c(colorPA_trans, colorPA))
    pr<-summary(lm)$r.squared
    ps<-summary(lm)$coefficients[2,1]
    print(summary(lm)$coefficients[2,4])
    
    Arm <- TCombine(terminal_reaches)
    ARm <- Arm$EC_Late * -1
    APec <- TCombine(terminal_localization)
    APec <- APec$EC_Late
    points(APec ~ ARm, col = colorT)
    tm<-plotRegressionWithCI(ARm, APec, colors = c(colorT_trans, colorT))
    tr<-summary(tm)$r.squared
    ts<-summary(tm)$coefficients[2,1]
    print(summary(tm)$coefficients[2,4])
    
    PARRm <- as.numeric(unlist(colMeans(exposure_reaches[33:48,2:33], na.rm = TRUE)))
    PAPec <- TCombine(exposure_localization)
    PAPec<- PAPec$EC_Late*-1
    points(PAPec ~ PARRm, col = colorE)
    em<-plotRegressionWithCI(PARRm, PAPec, colors = c(colorE_trans, colorE))
    er<-summary(em)$r.squared
    es<-summary(em)$coefficients[2,1]
    print(summary(em)$coefficients[2,4])
    
    label1<- sprintf("Continuous, r2=%.2f, β=%.2f", pr,ps)
    label2<- sprintf("Terminal, r2=%.2f, β=%.2f", tr,ts)
    label3<- sprintf("Exposure, r2=%.2f, β=%.2f", er,es)
    legend(
      -35,
      20,
      legend = c(
        label1,
        label2,
        label3
      ),
      col = c(colorPA, colorT, colorE),
      lty = c(1, 1, 1),
      lwd = c(2, 2, 2),
      bty = 'n', cex = 1.25
    )
    
  
}


plotpassiveproppoints<- function(){
  TtestPdata<- PrepdataforPropT(passive_localization, terminal_localization, exposure_localization)
  PPmean<- c()
  PPmean[1]<- mean(TtestPdata$Aligned[TtestPdata$Experiment == 'Passive'], na.rm = TRUE)
  PPmean[2]<- mean(TtestPdata$R1_Early[TtestPdata$Experiment == 'Passive'], na.rm = TRUE)
  PPmean[3]<- mean(TtestPdata$R1_Late[TtestPdata$Experiment == 'Passive'], na.rm = TRUE)
  PPmean[4]<- mean(TtestPdata$R2[TtestPdata$Experiment == 'Passive'], na.rm = TRUE)
  PPmean[5]<- mean(TtestPdata$EC_Late[TtestPdata$Experiment == 'Passive'], na.rm = TRUE)
  
  PPSE<- c()
  PPSE[1]<- (sd(TtestPdata$Aligned[TtestPdata$Experiment == 'Passive'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Passive']))
  PPSE[2]<- (sd(TtestPdata$R1_Early[TtestPdata$Experiment == 'Passive'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Passive']))
  PPSE[3]<- (sd(TtestPdata$R1_Late[TtestPdata$Experiment == 'Passive'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Passive']))
  PPSE[4]<- (sd(TtestPdata$R2[TtestPdata$Experiment == 'Passive'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Passive']))
  PPSE[5]<- (sd(TtestPdata$EC_Late[TtestPdata$Experiment == 'Passive'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Passive']))
  
  PTmean<- c()
  PTmean[1]<- mean(TtestPdata$Aligned[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE)
  PTmean[2]<- mean(TtestPdata$R1_Early[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE)
  PTmean[3]<- mean(TtestPdata$R1_Late[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE)
  PTmean[4]<- mean(TtestPdata$R2[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE)
  PTmean[5]<- mean(TtestPdata$EC_Late[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE)
  
  PTSE<- c()
  PTSE[1]<- (sd(TtestPdata$Aligned[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Terminal']))
  PTSE[2]<- (sd(TtestPdata$R1_Early[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Terminal']))
  PTSE[3]<- (sd(TtestPdata$R1_Late[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Terminal']))
  PTSE[4]<- (sd(TtestPdata$R2[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Terminal']))
  PTSE[5]<- (sd(TtestPdata$EC_Late[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Terminal']))
  
  PEmean<- c()
  PEmean[1]<- mean(TtestPdata$Aligned[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE)
  PEmean[2]<- mean(TtestPdata$R1_Early[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE)
  PEmean[3]<- mean(TtestPdata$R1_Late[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE)
  PEmean[4]<- mean(TtestPdata$R2[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE)
  PEmean[5]<- mean(TtestPdata$EC_Late[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE)
  
  PESE<- c()
  PESE[1]<- (sd(TtestPdata$Aligned[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Exposure']))
  PESE[2]<- (sd(TtestPdata$R1_Early[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Exposure']))
  PESE[3]<- (sd(TtestPdata$R1_Late[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Exposure']))
  PESE[4]<- (sd(TtestPdata$R2[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Exposure']))
  PESE[5]<- (sd(TtestPdata$EC_Late[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Exposure']))
  
  
  plot(y=PPmean[1:3]*1, x = c(.3,.95,1.6), pch = 15, axes = FALSE,xlab = "", ylab = "Hand Direction [°]", col = colorPA, ylim = c(-10,35), cex.lab = 1.5, xlim = c(0,3.5))
  #points(y=PPmean[1:3]*1,x = c(.85,1.85,2.85), pch = 15,  col = colorPA)
  arrows(x0 = c(.3,.95,1.6), y0 = (PPmean[1:3]) - PPSE[1:3]*3, x1 = c(.3,.95,1.6), y1 = (PPmean[1:3]) + PPSE[1:3]*3, code = 3, angle = 90, length = .02, col = colorPA)
  points(y=TtestPdata$Aligned[TtestPdata$Experiment == 'Passive'], pch= 16,x = c(rep(.35, times = length(TtestPdata$Aligned[TtestPdata$Experiment == 'Passive']))),col = colorPA_trans)
  points(y=TtestPdata$R1_Early[TtestPdata$Experiment == 'Passive'], pch= 16,x = c(rep(1, times = length(TtestPdata$R1_Early[TtestPdata$Experiment == 'Passive']))),col = colorPA_trans)
  points(y=TtestPdata$R1_Late[TtestPdata$Experiment == 'Passive'], pch= 16,x = c(rep(1.65, times = length(TtestPdata$R1_Late[TtestPdata$Experiment == 'Passive']))),col = colorPA_trans)
  #segments(x0 = .9, y0 = TtestPdata$Aligned[TtestPdata$Experiment == 'Passive'], x1 = 1.9, y1 = TtestPdata$R1_Early[TtestPdata$Experiment == 'Passive'], col = colorPA_trans)
  #segments(x0 = 1.9, y0 = TtestPdata$Aligned[TtestPdata$Experiment == 'Passive'], x1 = 2.9, y1 = TtestPdata$R1_Early[TtestPdata$Experiment == 'Passive'], col = rgb(0.7, 0.0, 0.7, 0.1))
  
  points(y=PTmean[1:3]*1,x = c(.4,1.05,1.7), pch = 15,  col = colorT)
  arrows(x0 = c(.4,1.05,1.7), y0 = (PTmean[1:3]) - PTSE[1:3]*3, x1 = c(.4,1.05,1.7), y1 = (PTmean[1:3]) + PTSE[1:3]*3, code = 3, angle = 90, length = .02, col = colorT)
  points(y=TtestPdata$Aligned[TtestPdata$Experiment == 'Terminal'], pch= 16,x = c(rep(.45, times = length(TtestPdata$Aligned[TtestPdata$Experiment == 'Terminal']))),col = colorT_trans)
  points(y=TtestPdata$R1_Early[TtestPdata$Experiment == 'Terminal'], pch= 16,x = c(rep(1.1, times = length(TtestPdata$R1_Early[TtestPdata$Experiment == 'Terminal']))),col = colorT_trans)
  #segments(x0 = 1, y0 = TtestPdata$Aligned[TtestPdata$Experiment == 'Terminal'], x1 = 2, y1 = TtestPdata$R1_Early[TtestPdata$Experiment == 'Terminal'], col = colorT_trans)
  points(y=TtestPdata$R1_Late[TtestPdata$Experiment == 'Terminal'], pch= 16,x = c(rep(1.75, times = length(TtestPdata$R1_Late[TtestPdata$Experiment == 'Terminal']))),col = colorT_trans)
  #segments(x0 = 2, y0 = TtestPdata$R1_Early[TtestPdata$Experiment == 'Terminal'], x1 = 3, y1 = TtestPdata$R1_Late[TtestPdata$Experiment == 'Terminal'], col = rgb(1, 0.0, 0., 0.1))
  
  
  points(PEmean[1:3]*1, x = c(.5,1.15,1.8),pch = 15,  col = colorE)
  arrows(x0 = c(.5,1.15,1.8), y0 = (PEmean[1:3]) - PESE[1:3]*3, x1 = c(.5,1.15,1.8), y1 = (PEmean[1:3]) + PESE[1:3]*3, code = 3, angle = 90, length = .02, col = colorE)
  points(y=TtestPdata$Aligned[TtestPdata$Experiment == 'Exposure'], pch= 16,x = c(rep(.55, times = length(TtestPdata$Aligned[TtestPdata$Experiment == 'Exposure']))),col = colorE_trans)
  points(y=TtestPdata$R1_Early[TtestPdata$Experiment == 'Exposure'],pch= 16, x = c(rep(1.2, times = length(TtestPdata$R1_Early[TtestPdata$Experiment == 'Exposure']))),col = colorE_trans)
  points(y=TtestPdata$R1_Late[TtestPdata$Experiment == 'Exposure'], pch= 16,x = c(rep(1.85, times = length(TtestPdata$R1_Late[TtestPdata$Experiment == 'Exposure']))),col = colorE_trans)
  #segments(x0 = 1.1, y0 = TtestPdata$Aligned[TtestPdata$Experiment == 'Exposure'], x1 = 2.1, y1 = TtestPdata$R1_Early[TtestPdata$Experiment == 'Exposure'], col = colorE_trans)
  #segments(x0 = 2.1, y0 = TtestPdata$Aligned[TtestPdata$Experiment == 'Exposure'], x1 = 3.1, y1 = TtestPdata$R1_Early[TtestPdata$Experiment == 'Exposure'], col = rgb(0.85, 0.65, 0.12, 0.1))
  
  
  lines(c(0, .75,.75, 1.85, 1.85),
        c(0, 0, 30, 30, 30),
        col = rgb(0., 0., 0.))
  #lines(c(4.5, 5.5),
  #     c(0, 0),
  #     lty = 2,
  #    col = rgb(0., 0., 0.))
  axis(2, at = c(0,10, 20, 30), cex.axis = 1.5, las = 2)
  axis(1, at = c(.4,1.1,1.7),labels = c("Aligned \n 61-64", "Early \n 65-68", "Late \n 221-224"), cex.axis = 1.25, las = 2)
  #legend(-.3,33, legend = c("Continous", "Terminal", "Exposure"), col = c(colorPA, colorT, colorE), lty = c(1), lwd = c(2), cex = 1.2, bty = "n")
}

plotpassiveprop4points<- function(){
  TtestPdata<- PrepdataforPropT(passive_localization, terminal_localization, exposure_localization)
  PPmean<- c()
  PPmean[1]<- mean(TtestPdata$Aligned[TtestPdata$Experiment == 'Passive'], na.rm = TRUE)
  PPmean[2]<- mean(TtestPdata$R1_Early[TtestPdata$Experiment == 'Passive'], na.rm = TRUE)
  PPmean[3]<- mean(TtestPdata$R1_Late[TtestPdata$Experiment == 'Passive'], na.rm = TRUE)
  PPmean[4]<- mean(TtestPdata$R2[TtestPdata$Experiment == 'Passive'], na.rm = TRUE)
  PPmean[5]<- mean(TtestPdata$EC_Late[TtestPdata$Experiment == 'Passive'], na.rm = TRUE)
  
  PPSE<- c()
  PPSE[1]<- (sd(TtestPdata$Aligned[TtestPdata$Experiment == 'Passive'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Passive']))
  PPSE[2]<- (sd(TtestPdata$R1_Early[TtestPdata$Experiment == 'Passive'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Passive']))
  PPSE[3]<- (sd(TtestPdata$R1_Late[TtestPdata$Experiment == 'Passive'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Passive']))
  PPSE[4]<- (sd(TtestPdata$R2[TtestPdata$Experiment == 'Passive'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Passive']))
  PPSE[5]<- (sd(TtestPdata$EC_Late[TtestPdata$Experiment == 'Passive'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Passive']))
  
  PTmean<- c()
  PTmean[1]<- mean(TtestPdata$Aligned[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE)
  PTmean[2]<- mean(TtestPdata$R1_Early[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE)
  PTmean[3]<- mean(TtestPdata$R1_Late[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE)
  PTmean[4]<- mean(TtestPdata$R2[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE)
  PTmean[5]<- mean(TtestPdata$EC_Late[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE)
  
  PTSE<- c()
  PTSE[1]<- (sd(TtestPdata$Aligned[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Terminal']))
  PTSE[2]<- (sd(TtestPdata$R1_Early[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Terminal']))
  PTSE[3]<- (sd(TtestPdata$R1_Late[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Terminal']))
  PTSE[4]<- (sd(TtestPdata$R2[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Terminal']))
  PTSE[5]<- (sd(TtestPdata$EC_Late[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Terminal']))
  
  PEmean<- c()
  PEmean[1]<- mean(TtestPdata$Aligned[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE)
  PEmean[2]<- mean(TtestPdata$R1_Early[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE)
  PEmean[3]<- mean(TtestPdata$R1_Late[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE)
  PEmean[4]<- mean(TtestPdata$R2[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE)
  PEmean[5]<- mean(TtestPdata$EC_Late[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE)
  
  PESE<- c()
  PESE[1]<- (sd(TtestPdata$Aligned[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Exposure']))
  PESE[2]<- (sd(TtestPdata$R1_Early[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Exposure']))
  PESE[3]<- (sd(TtestPdata$R1_Late[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Exposure']))
  PESE[4]<- (sd(TtestPdata$R2[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Exposure']))
  PESE[5]<- (sd(TtestPdata$EC_Late[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Exposure']))
  
  
  plot(y=PPmean[1:5]*1, x = c(.3,.95,1.6,2.25,2.9), pch = 15, axes = FALSE,xlab = "", ylab = "Hand Localization Change [°]", col = colorPA, ylim = c(-30,30), cex.lab = 1.5, xlim = c(0,5))
  arrows(x0 = c(.3,.95,1.6,2.25,2.9), y0 = (PPmean[1:5]) - PPSE[1:5]*3, x1 = c(.3,.95,1.6,2.25,2.9), y1 = (PPmean[1:5]) + PPSE[1:5]*3, code = 3, angle = 90, length = .02, col = colorPA)
  points(y=TtestPdata$Aligned[TtestPdata$Experiment == 'Passive'], pch= 16,x = c(rep(.35, times = length(TtestPdata$Aligned[TtestPdata$Experiment == 'Passive']))),col = colorPA_trans)
  points(y=TtestPdata$R1_Early[TtestPdata$Experiment == 'Passive'], pch= 16,x = c(rep(1, times = length(TtestPdata$R1_Early[TtestPdata$Experiment == 'Passive']))),col = colorPA_trans)
  points(y=TtestPdata$R1_Late[TtestPdata$Experiment == 'Passive'], pch= 16,x = c(rep(1.65, times = length(TtestPdata$R1_Late[TtestPdata$Experiment == 'Passive']))),col = colorPA_trans)
  points(y=TtestPdata$R2[TtestPdata$Experiment == 'Passive'], pch= 16,x = c(rep(2.3, times = length(TtestPdata$R2[TtestPdata$Experiment == 'Passive']))),col = colorPA_trans)
  points(y=TtestPdata$EC_Late[TtestPdata$Experiment == 'Passive'], pch= 16,x = c(rep(2.95, times = length(TtestPdata$EC_Late[TtestPdata$Experiment == 'Passive']))),col = colorPA_trans)
  
  
  points(y=PTmean[1:5]*1,x = c(.4,1.05,1.7,2.35,3), pch = 15,  col = colorT)
  arrows(x0 = c(.4,1.05,1.7,2.35,3), y0 = (PTmean[1:5]) - PTSE[1:5]*3, x1 = c(.4,1.05,1.7,2.35,3), y1 = (PTmean[1:5]) + PTSE[1:5]*3, code = 3, angle = 90, length = .02, col = colorT)
  points(y=TtestPdata$Aligned[TtestPdata$Experiment == 'Terminal'], pch= 16,x = c(rep(.45, times = length(TtestPdata$Aligned[TtestPdata$Experiment == 'Terminal']))),col = colorT_trans)
  points(y=TtestPdata$R1_Early[TtestPdata$Experiment == 'Terminal'], pch= 16,x = c(rep(1.1, times = length(TtestPdata$R1_Early[TtestPdata$Experiment == 'Terminal']))),col = colorT_trans)
  points(y=TtestPdata$R1_Late[TtestPdata$Experiment == 'Terminal'], pch= 16,x = c(rep(1.75, times = length(TtestPdata$R1_Late[TtestPdata$Experiment == 'Terminal']))),col = colorT_trans)
  points(y=TtestPdata$R2[TtestPdata$Experiment == 'Terminal'], pch= 16,x = c(rep(2.4, times = length(TtestPdata$R2[TtestPdata$Experiment == 'Terminal']))),col = colorT_trans)
  points(y=TtestPdata$EC_Late[TtestPdata$Experiment == 'Terminal'], pch= 16,x = c(rep(3.05, times = length(TtestPdata$EC_Late[TtestPdata$Experiment == 'Terminal']))),col = colorT_trans)
  
  
  
  points(PEmean[1:5]*1, x = c(.5,1.15,1.8,2.45,3.1),pch = 15,  col = colorE)
  arrows(x0 = c(.5,1.15,1.8,2.45,3.1), y0 = (PEmean[1:5]) - PESE[1:5]*3, x1 = c(.5,1.15,1.8,2.45,3.1), y1 = (PEmean[1:5]) + PESE[1:5]*3, code = 3, angle = 90, length = .02, col = colorE)
  points(y=TtestPdata$Aligned[TtestPdata$Experiment == 'Exposure'], pch= 16,x = c(rep(.55, times = length(TtestPdata$Aligned[TtestPdata$Experiment == 'Exposure']))),col = colorE_trans)
  points(y=TtestPdata$R1_Early[TtestPdata$Experiment == 'Exposure'],pch= 16, x = c(rep(1.2, times = length(TtestPdata$R1_Early[TtestPdata$Experiment == 'Exposure']))),col = colorE_trans)
  points(y=TtestPdata$R1_Late[TtestPdata$Experiment == 'Exposure'], pch= 16,x = c(rep(1.85, times = length(TtestPdata$R1_Late[TtestPdata$Experiment == 'Exposure']))),col = colorE_trans)
  points(y=TtestPdata$R2[TtestPdata$Experiment == 'Exposure'], pch= 16,x = c(rep(2.5, times = length(TtestPdata$R2[TtestPdata$Experiment == 'Exposure']))),col = colorE_trans)
  points(y=TtestPdata$EC_Late[TtestPdata$Experiment == 'Exposure'], pch= 16,x = c(rep(3.15, times = length(TtestPdata$EC_Late[TtestPdata$Experiment == 'Exposure']))),col = colorE_trans)
  
  
  lines(c(0, .75,.75, 1.85,2.05, 2.05, 2.95,2.95),
        c(0, 0, 30, 30,30, -30,-30,0),
        col = rgb(0., 0., 0.))
  lines(c(2.95, 3.35),
       c(0, 0),
       lty = 2,
      col = rgb(0., 0., 0.))
  axis(2, at = c(-30,-20,-10,0,10, 20, 30), cex.axis = 1.5, las = 2)
  axis(1, at = c(.4,1.1,1.7,2.4,3),labels = c("Aligned \n 61-64", "Early \n 65-68", "Late \n 221-224", "Reversed \n 237-240", "Clamped \n 273-288"), cex.axis = 1.25, las = 2)
  #legend(-.3,33, legend = c("Continous", "Terminal", "Exposure"), col = c(colorPA, colorT, colorE), lty = c(1), lwd = c(2), cex = 1.2, bty = "n")
}

plotrebound<- function(){
  TtestPdata<-PreptoplotRebounds(passive_reaches, terminal_reaches, exposure_reaches)
  TtestPdata[,1]<-TtestPdata[,1]*-1
  TtestPdata[,2]<-as.numeric(TtestPdata[,2])*-1

  
  PPmean<- c()
  PPmean[1]<- mean(as.numeric(TtestPdata$EC_Early[TtestPdata$Experiment == 'Passive']), na.rm = TRUE)
  PPmean[2]<- mean(TtestPdata$EC_Late[TtestPdata$Experiment == 'Passive'], na.rm = TRUE)

  
  PPSE<- c()
  PPSE[1]<- (sd(TtestPdata$EC_Early[TtestPdata$Experiment == 'Passive'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Passive']))
  PPSE[2]<- (sd(TtestPdata$EC_Late[TtestPdata$Experiment == 'Passive'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Passive']))
  
  PTmean<- c()
  PTmean[1]<- mean(as.numeric(TtestPdata$EC_Early[TtestPdata$Experiment == 'Terminal']), na.rm = TRUE)
  PTmean[2]<- mean(TtestPdata$EC_Late[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE)
  
  PTSE<- c()
  PTSE[1]<- (sd(TtestPdata$EC_Early[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Terminal']))
  PTSE[2]<- (sd(TtestPdata$EC_Late[TtestPdata$Experiment == 'Terminal'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Terminal']))
  
  PEmean<- c()
  PEmean[1]<- mean(as.numeric(TtestPdata$EC_Early[TtestPdata$Experiment == 'Exposure']), na.rm = TRUE)
  PEmean[2]<- mean(TtestPdata$EC_Late[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE)
  
  PESE<- c()
  PESE[1]<- (sd(TtestPdata$EC_Early[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Exposure']))
  PESE[2]<- (sd(TtestPdata$EC_Late[TtestPdata$Experiment == 'Exposure'], na.rm = TRUE))/sqrt(length(TtestPdata$Experiment[TtestPdata$Experiment == 'Exposure']))
 
  # PPmean<- PPmean*-1
  # PEmean<- PEmean*-1
  # PTmean<- PTmean*-1
  
  plot(y = PPmean, x =c( .9,1.9), pch = 15, axes = FALSE, xlab = "Block", ylab = "Hand Direction [°]", col = colorPA, cex.lab = 1.5, ylim = c(-35,35), xlim = c(0.8,2.2) , main = "Rebounds", cex.main = 1.5)
  #points(y=PPmean[1:3]*1,x = c(.85,1.85,2.85), pch = 15,  col = colorPA)
  arrows(x0 = c(.9,1.9), y0 = (PPmean) - PPSE*2, x1 = c(.9,1.9), y1 = (PPmean) + PPSE*2, code = 3, angle = 90, length = .02, col = colorPA)
  points(y=TtestPdata$EC_Early[TtestPdata$Experiment == 'Passive'], pch= 16,x = c(rep(.9, times = length(TtestPdata$EC_Early[TtestPdata$Experiment == 'Passive']))),col = colorPA_trans)
  points(y=TtestPdata$EC_Late[TtestPdata$Experiment == 'Passive'], pch= 16,x = c(rep(1.9, times = length(TtestPdata$EC_Late[TtestPdata$Experiment == 'Passive']))),col = colorPA_trans)
  segments(x0 = .9, y0 = mean(as.numeric(TtestPdata$EC_Early[TtestPdata$Experiment == 'Passive']), na.rm= TRUE), x1 = 1.9, y1 = mean(TtestPdata$EC_Late[TtestPdata$Experiment == 'Passive'],na.rm= TRUE), col = colorPA)
  
  points(y=PTmean,x = c(1.0,2.0), pch = 15,  col = colorT)
  arrows(x0 = c(1.0,2.0), y0 = (PTmean) - PTSE*2, x1 = c(1.0,2.0), y1 = (PTmean) + PTSE*2, code = 3, angle = 90, length = .02, col = colorT)
  points(y=TtestPdata$EC_Early[TtestPdata$Experiment == 'Terminal'], pch= 16,x = c(rep(1.0, times = length(TtestPdata$EC_Early[TtestPdata$Experiment == 'Terminal']))),col = colorT_trans)
  points(y=TtestPdata$EC_Late[TtestPdata$Experiment == 'Terminal'], pch= 16,x = c(rep(2.0, times = length(TtestPdata$EC_Late[TtestPdata$Experiment == 'Terminal']))),col = colorT_trans)
  segments(x0 = 1, y0 = mean(as.numeric(TtestPdata$EC_Early[TtestPdata$Experiment == 'Terminal']), na.rm= TRUE), x1 = 2, y1 = mean(TtestPdata$EC_Late[TtestPdata$Experiment == 'Terminal'], na.rm= TRUE), col = colorT)
  
  
  points(PEmean, x = c(1.1,2.1),pch = 15,  col = colorE)
  arrows(x0 = c(1.1,2.1), y0 = (PEmean) - PESE*2, x1 = c(1.1,2.1), y1 = (PEmean) + PESE*2, code = 3, angle = 90, length = .02, col = colorE)
  points(y=TtestPdata$EC_Early[TtestPdata$Experiment == 'Exposure'], pch= 16,x = c(rep(1.1, times = length(TtestPdata$EC_Early[TtestPdata$Experiment == 'Exposure']))),col = colorE_trans)
  points(y=TtestPdata$EC_Late[TtestPdata$Experiment == 'Exposure'],pch= 16, x = c(rep(2.1, times = length(TtestPdata$EC_Late[TtestPdata$Experiment == 'Exposure']))),col = colorE_trans)
  segments(x0 = 1.1, y0 = mean(as.numeric(TtestPdata$EC_Early[TtestPdata$Experiment == 'Exposure']), na.rm = TRUE), x1 = 2.1, y1 = mean(TtestPdata$EC_Late[TtestPdata$Experiment == 'Exposure'], na.rm=TRUE), col = colorE)
  
  

  abline(h = 0, lty = 2)
  axis(2, at = c(-30,-15,0,15,30), cex.axis = 1.5, las = 2)
  axis(1, at = c(1,2),labels = c("1st 16 \n trials", "last 16 \n trials"), cex.axis = 1.5)
  legend(1.3,-10, legend = c("Continous", "Terminal", "Exposure"), col = c(colorPA, colorT, colorE), lty = c(1), lwd = c(2), cex = 1.2, bty = "n")
}
plotREAANOVA<- function(){
  
  
  ANOVAdata<- PrepdataforANOVA(passive_reaches,terminal_reaches)
  Rebounds<- PrepANOVARebounds(passive_reaches,terminal_reaches,exposure_reaches)
  data<- Rebounds[Rebounds$Experiment == "Exposure",]
  data$Time<- rep("EC", times = 32)
  data<- data[, c("EC_Late", "Time", "ID", "Experiment")]
  colnames(data)[1] <- "Deviations"
  
  TtestPdata<- rbind(ANOVAdata, data)
  TtestPdata$Deviations<- TtestPdata$Deviations*-1

  PPmean1<- c()
  PPmean1[1]<- mean(as.numeric(TtestPdata$Deviations[TtestPdata$Time == "R1_early" & TtestPdata$Experiment == "Passive"]), na.rm = TRUE)
  PPmean1[2]<- mean(as.numeric(TtestPdata$Deviations[TtestPdata$Time == "R1_late" & TtestPdata$Experiment == "Passive"]), na.rm = TRUE)
  PPmean1[3]<- mean(as.numeric(TtestPdata$Deviations[TtestPdata$Time == "R2D2" & TtestPdata$Experiment == "Passive"]), na.rm = TRUE) 
  PPmean1[4]<- mean(as.numeric(TtestPdata$Deviations[TtestPdata$Time == "EC" & TtestPdata$Experiment == "Passive"]), na.rm = TRUE)
  
  PPSE1<- c()
  PPSE1[1]<- sd(as.numeric(TtestPdata$Deviations[TtestPdata$Time == "R1_early" & TtestPdata$Experiment == "Passive"]), na.rm = TRUE)/sqrt(32)
  PPSE1[2]<- sd(as.numeric(TtestPdata$Deviations[TtestPdata$Time == "R1_late" & TtestPdata$Experiment == "Passive"]), na.rm = TRUE)/sqrt(32)
  PPSE1[3]<- sd(as.numeric(TtestPdata$Deviations[TtestPdata$Time == "R2D2" & TtestPdata$Experiment == "Passive"]), na.rm = TRUE) /sqrt(32)
  PPSE1[4]<- sd(as.numeric(TtestPdata$Deviations[TtestPdata$Time == "EC" & TtestPdata$Experiment == "Passive"]), na.rm = TRUE)/sqrt(32)
  
  
  PTmean1<- c()
  PTmean1[1]<- mean(as.numeric(TtestPdata$Deviations[TtestPdata$Time == "R1_early" & TtestPdata$Experiment == "Terminal"]), na.rm = TRUE)
  PTmean1[2]<- mean(as.numeric(TtestPdata$Deviations[TtestPdata$Time == "R1_late" & TtestPdata$Experiment == "Terminal"]), na.rm = TRUE)
  PTmean1[3]<- mean(as.numeric(TtestPdata$Deviations[TtestPdata$Time == "R2D2" & TtestPdata$Experiment == "Terminal"]), na.rm = TRUE) 
  PTmean1[4]<- mean(as.numeric(TtestPdata$Deviations[TtestPdata$Time == "EC" & TtestPdata$Experiment == "Terminal"]), na.rm = TRUE)
  
  PTSE1<- c()
  PTSE1[1]<- sd(as.numeric(TtestPdata$Deviations[TtestPdata$Time == "R1_early" & TtestPdata$Experiment == "Terminal"]), na.rm = TRUE)/sqrt(32)
  PTSE1[2]<- sd(as.numeric(TtestPdata$Deviations[TtestPdata$Time == "R1_late" & TtestPdata$Experiment == "Terminal"]), na.rm = TRUE)/sqrt(32)
  PTSE1[3]<- sd(as.numeric(TtestPdata$Deviations[TtestPdata$Time == "R2D2" & TtestPdata$Experiment == "Terminal"]), na.rm = TRUE) /sqrt(32)
  PTSE1[4]<- sd(as.numeric(TtestPdata$Deviations[TtestPdata$Time == "EC" & TtestPdata$Experiment == "Terminal"]), na.rm = TRUE)/sqrt(32)
  
  PEmean1<- c()
  PEmean1[1]<- mean(TtestPdata$Deviations[TtestPdata$Time == "EC" & TtestPdata$Experiment == "Exposure"], na.rm = TRUE)

  
  PESE1<- c()
  PESE1[1]<- (sd(TtestPdata$Deviations[TtestPdata$Time == "EC" & TtestPdata$Experiment == "Exposure"], na.rm = TRUE))/sqrt(32)

  
  plot(y = PPmean1, x =c( .3,.85,1.4,1.95), pch = 15, axes = FALSE, xlab = "", ylab = "Reach Direction [°]", col = colorPA, cex.lab = 1.5, ylim = c(-35,35),xlim = c(0,4.2), main = "", cex.main = 1.5)
  #points(y=PPmean[1:3]*1,x = c(.85,1.85,2.85), pch = 15,  col = colorPA)
  arrows(x0 = c(.3,.85,1.4,1.95), y0 = (PPmean1) - PPSE1*3, x1 = c(.3,.85,1.4,1.95), y1 = (PPmean1) + PPSE1*3, code = 3, angle = 90, length = .02, col = colorPA)
  points(y=TtestPdata$Deviations[TtestPdata$Time == "R1_early" & TtestPdata$Experiment == "Passive"], pch= 16,x = c(rep(.35, times = 32)),col = colorPA_trans)
  points(y=TtestPdata$Deviations[TtestPdata$Time == "R1_late" & TtestPdata$Experiment == "Passive"], pch= 16,x = c(rep(.9, times = 32)),col = colorPA_trans)
  points(y=TtestPdata$Deviations[TtestPdata$Time == "R2D2" & TtestPdata$Experiment == "Passive"], pch= 16,x = c(rep(1.45, times = 32)),col = colorPA_trans)
  points(y=TtestPdata$Deviations[TtestPdata$Time == "EC" & TtestPdata$Experiment == "Passive"], pch= 16,x = c(rep(2, times = 32)),col = colorPA_trans)
 
  
  points(y=PTmean1,x = c(.4,0.95,1.5,2.05), pch = 15,  col = colorT)
  arrows(x0 = c(.4,.95,1.5,2.05), y0 = (PTmean1) - PTSE1*3, x1 = c(.4,.95,1.5,2.05), y1 = (PTmean1) + PTSE1*3, code = 3, angle = 90, length = .02, col = colorT)
  points(y=TtestPdata$Deviations[TtestPdata$Time == "R1_early" & TtestPdata$Experiment == "Terminal"], pch= 16,x = c(rep(.45, times = 32)),col = colorT_trans)
  points(y=TtestPdata$Deviations[TtestPdata$Time == "R1_late" & TtestPdata$Experiment == "Terminal"], pch= 16,x = c(rep(1, times = 32)),col = colorT_trans)
  points(y=TtestPdata$Deviations[TtestPdata$Time == "R2D2" & TtestPdata$Experiment == "Terminal"], pch= 16,x = c(rep(1.55, times = 32)),col = colorT_trans)
  points(y=TtestPdata$Deviations[TtestPdata$Time == "EC" & TtestPdata$Experiment == "Terminal"], pch= 16,x = c(rep(2.1, times = 32)),col = colorT_trans)
 
  
  
  points(PEmean1, x = c(2.15),pch = 15,  col = colorE)
  arrows(x0 = c(2.15), y0 = (PEmean1) - PESE1*3, x1 = c(2.15), y1 = (PEmean1) + PESE1*3, code = 3, angle = 90, length = .02, col = colorE)
  points(y=TtestPdata$Deviations[TtestPdata$Time == "EC" & TtestPdata$Experiment == "Exposure"], pch= 16,x = c(rep(2.2, times = 32)),col = colorE_trans)
  
  
  
  
  #abline(h = 0, lty = 2)
  lines(c(0, 1.2, 1.2, 1.75,1.75),
        c(30, 30, -30, -30, 0),
        col = rgb(0., 0., 0.))
  lines(c(1.75, 2.2),
        c(0, 0),
        lty = 2,
        col = rgb(0., 0., 0.))
  axis(2, at = c(-30,-15,0,15,30), cex.axis = 1.5, las = 2)
  axis(1, at = c(.35,.9,1.45,2.1),labels = c("Early \n 65-68", "Late \n 221-224", "Reversed \n 237-240", "Clamped \n 273-288"), cex.axis = 1.25, las = 2)
  #legend(.5,-3, legend = c("Continous", "Terminal", "Exposure"), col = c(colorPA, colorT, colorE), lty = c(1), lwd = c(2), cex = 1.2, bty = "n")
}

Localizations <- function (pl, tl , expl) {
  PlotoutLine(pl, c(1:3), 1:3,title =  "Hand Localizations", ylabel = "Hand Direction Change [°]")
  PlotData(pl, 1, 1,1)
  PlotData(tl, 2, 2,1)
  PlotData(expl, 3, 3,1)
  
  abline(v = 66,col = colorPA)
  abline(v = 69, col = colorT)
  abline(v = 67, col = colorE) 
  # reaches <- getreachesformodel(passive_reaches)
  # reach_par <-
  #   fitTwoRateReachModel(
  #     reaches = reaches$meanreaches,
  #     schedule = reaches$distortion,
  #     oneTwoRates = 2,
  #     grid = 'restricted',
  #     checkStability = TRUE
  #   )
  # reach_model <- twoRateReachModel(par = reach_par, schedule = reaches$distortion)
  # lines(reach_model$total * -1, col = colorPA,lty = 4)
  # lines(reach_model$slow * -1, col = colorPA,lty = 2)
  # lines(reach_model$fast * -1, col = colorPA,lty = 3)
  
}





Reachmodel <- function(data, name, grid = 'restricted', condition = 'Reach', ncdata = NA, loc_data = NA, color, yaxis = 'Hand Direction [°]') {
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
    Plotncmodel(data[33:320, ], name, color, yaxis)
    lines(reach_model$total * -1, col = color,lty = 4)
    lines(reach_model$slow * -1, col = color,lty = 2)
    lines(reach_model$fast * -1, col = color,lty = 3)
    ncreaches <- getreachesformodel(ncdata)
    lines(x = 33:288, y = ncreaches$meanreaches * -1, col = color)
    
  } else if (condition == 'loc') {
    Plotlocmodel(data, name, color, yaxis)
    lines(reach_model$total * -1, col = 'black',lty = 4)
    lines(reach_model$slow * -1, col = color,lty = 2)
    lines(reach_model$fast * -1, col = color,lty = 3)
    lines(rowMeans(loc_data[, 2:ncol(loc_data)], na.rm = TRUE), col = color)
  } else{
    Plotmodel(data, name, color, yaxis)
    lines(reach_model$total * -1, col = 'black',lty = 4)
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
  Plotncmodel(data[33:320, ], name, color, "Hand Direction [°]")
  lines(reach_model$total * -1, col = 'black',lty = 4)
  lines(reach_model$slow * -1, col = color,lty = 2)
  lines(reach_model$fast * -1, col = color,lty = 3)
  ncreaches <- getreachesformodel(ncdata)
  lines(x = 33:288, y = ncreaches$meanreaches * -1, col = color)
  return(reach_par)
}

Reachmodelslownc <- function(data, ncdata, name, color) {
  reaches <- getreachesformodel(ncdata)
  reach_par <-
    twoRateNCFit(
      reaches = reaches$meanreaches,
      schedule = reaches$distortion)
  reach_model1 <-
    twoRateNCModel(par = reach_par, schedule = reaches$distortion)
  #reach_model <- reach_model1[33:320, ]
  Plotncmodel(data[33:320, ], name, color, "Hand Direction [°]")
  lines(x = c(33:288), y = reach_model1$total * -1, col = 'black',lty = 4)
  lines(x = c(33:288), y =reach_model1$slow * -1, col = color,lty = 2)
  lines(x = c(33:288), y =reach_model1$fast * -1, col = color,lty = 3)
  lines(x = 33:288, y = reaches$meanreaches * -1, col = color)
  return(reach_par)
}



Plotmodel <- function(dataset, name, color, yaxis) {
  title <- sprintf('%s', name)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[, 2:ncol(dataset)], na.rm = TRUE)
  plot(
    dataset$Mean * -1,
    ylim = c(-35, 35),
    xlab = "Trial",
    lwd = 2,
    ylab = yaxis,
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
    legend = c('Reaches', 'model', 'fast', 'slow'),
    col = c(
      rgb(0.44, 0.51, 0.57),
      'black',
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



Plotncmodel <- function(dataset, name, color, yaxis) {
  title <- sprintf('%s', name)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[, 2:ncol(dataset)], na.rm = TRUE)
  plot(
    dataset$Mean * -1,
    ylim = c(-35, 35),
    xlab = "Trial",
    lwd = 2,
    ylab =  yaxis,
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
    legend = c('Reaches', 'No-cursors', 'model', 'fast', 'slow'),
    col = c(
      color,
      color,
      color,
      color,
      color
    ),
    lty = c(1, 1, 4, 3, 2),
    lwd = c(2, 2, 2, 2, 2),
    bty = 'n',
    ncol = 2, 
    cex = 1.2
  )
  axis(
    1,
    at = c(1, 64, 224, 240, 288),
    cex.axis = 1.5,
    las = 2
  )
  axis(2, at = c(-30, -15, 0, 15, 30), cex.axis = 1.5, las = 2)
  lines(dataset$Mean * -1, col = color)
}

Plotlocmodel <- function(dataset, name, color, yaxis) {
  title <- sprintf('%s', name)
  dataset["distortion"][is.na(dataset["distortion"])] <- 0
  dataset$Mean <- rowMeans(dataset[, 2:ncol(dataset)], na.rm = TRUE)
  plot(
    dataset$Mean * -1,
    ylim = c(-35, 35),
    xlab = "Trial",
    lwd = 2,
    ylab =  yaxis,
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
    legend = c('Reaches', 'Localizations', 'model', 'fast', 'slow'),
    col = c(
      rgb(0.44, 0.51, 0.57),
      color,
      'black',
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





test<- function() {
  
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

  return(unlist(pargrid[bestpar]))
  
}

