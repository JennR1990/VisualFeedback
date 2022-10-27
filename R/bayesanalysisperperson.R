
schedule<- rep(-1, times = 161)

ppars<-c()
for (i in 2:33){
setdata<- c(0,passive_localization[65:224,i])
par<- asymptoticDecayFit(schedule = schedule, signal = setdata, setAsymptote = FALSE )
ppars<- rbind(ppars,par)
}



for (i in 2:33){
  setdata<- unlist(c(0,terminal_localization[65:224,i]))
  par<- asymptoticDecayFit(schedule = schedule, signal = setdata, setAsymptote = FALSE )
  ppars<- rbind(ppars,par)
}


for (i in 2:33){
  setdata<- unlist(c(0,exposure_localization[65:224,i]))
  par<- asymptoticDecayFit(schedule = schedule, signal = setdata, setAsymptote = FALSE )
  ppars<- rbind(ppars,par)
}

ppars<- data.frame(ppars)
ppars$experiment<- c(rep("Continuous", times = 32),rep("Terminal", times = 32),rep("Exposure", times = 32))


ttestBF(ppars$lambda[ppars$experiment == 'Continuous'], ppars$lambda[ppars$experiment == 'Terminal'], paired = FALSE)
ttestBF(ppars$lambda[ppars$experiment == 'Continuous'], ppars$lambda[ppars$experiment == 'Exposure'], paired = FALSE)

ppars$experiment<- as.factor(ppars$experiment)
anovaBF(lambda ~ experiment, data= ppars)
anovaBF(N0 ~ experiment, data= ppars)

ttestBF(ppars$N0[ppars$experiment == 'Continuous'], ppars$N0[ppars$experiment == 'Terminal'], paired = FALSE)
ttestBF(ppars$N0[ppars$experiment == 'Continuous'], ppars$N0[ppars$experiment == 'Exposure'], paired = FALSE)
ttestBF(ppars$N0[ppars$experiment == 'Terminal'], ppars$N0[ppars$experiment == 'Exposure'], paired = FALSE)

ttestBF(ppars$N0[ppars$experiment == 'Continuous'], paired = FALSE)
ttestBF(ppars$N0[ppars$experiment == 'Exposure'], paired = FALSE)
ttestBF(ppars$N0[ppars$experiment == 'Terminal'], paired = FALSE)




schedule<- rep(1, times = 160)

pars<-c()
for (i in 2:33){
  setdata<- c(passive_reaches[65:224,i])
  par<- asymptoticDecayFit(schedule = schedule, signal = setdata, setAsymptote = FALSE )
  pars<- rbind(pars,par)
}



for (i in 2:33){
  setdata<- c(terminal_reaches[65:224,i])
  par<- asymptoticDecayFit(schedule = schedule, signal = setdata, setAsymptote = FALSE )
  pars<- rbind(pars,par)
}

pars<- data.frame(pars)
pars$experiment<- c(rep("Continuous", times = 32),rep("Terminal", times = 32))

ttestBF(pars$lambda[pars$experiment == 'Continuous'], ppars$lambda[ppars$experiment == 'Continuous'], paired = FALSE)
ttestBF(pars$lambda[pars$experiment == 'Terminal'], ppars$lambda[ppars$experiment == 'Terminal'], paired = FALSE)





