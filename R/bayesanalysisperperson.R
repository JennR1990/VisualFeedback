


pars<-c()
for (i in 2:33){
setdata<- c(0,passive_localization[65:224,i])
par<- asymptoticDecayFit(schedule = schedule, signal = setdata, setAsymptote = FALSE )
pars<- rbind(pars,par)
}



for (i in 2:33){
  setdata<- c(0,terminal_localization[65:224,i])
  par<- asymptoticDecayFit(schedule = schedule, signal = setdata, setAsymptote = FALSE )
  pars<- rbind(pars,par)
}


for (i in 2:33){
  setdata<- c(0,exposure_localization[65:224,i])
  par<- asymptoticDecayFit(schedule = schedule, signal = setdata, setAsymptote = FALSE )
  pars<- rbind(pars,par)
}

pars<- data.frame(pars)
pars$experiment<- c(rep("Continuous", times = 32),rep("Terminal", times = 32),rep("Exposure", times = 32))


ttestBF(pars$lambda[pars$experiment == 'Continuous'], pars$lambda[pars$experiment == 'Terminal'], paired = FALSE)
ttestBF(pars$lambda[pars$experiment == 'Continuous'], pars$lambda[pars$experiment == 'Exposure'], paired = FALSE)
