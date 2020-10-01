
svglite(file='figs/MLMC_2020 fig 2.svg', width=15, height=8,pointsize =13, system_fonts=list(sans = "Arial"))
layout(matrix(c(1,1,2,2,3,3,4,4,5,5,0,0), nrow=2, byrow=TRUE) )
Reachmodel(data = active_reaches, name =  "Active", condition = "loc", loc_data = active_localization, color = colorA)
Reachmodel(data = passive_reaches, name =  "Passive", condition = "loc", loc_data = passive_localization, color = colorPA)
Reachmodel(data = terminal_reaches, name =  "Terminal", condition = "loc", loc_data = terminal_localization, color = colorT)
ExposureDatas(exposure_localization, exposure_reaches)
plotpropmodels()
dev.off()


svglite(file='figs/MLMC_2020 fig 3.svg', width=15, height=10,pointsize = 13, system_fonts=list(sans = "Arial"))
layout(matrix(c(1,1,2,2,3,3,4,4,5,6,0,0), nrow=2, byrow=TRUE) )
Reachmodel(data = newnocursor_reaches, name =  "No-Cursor", condition = "nc", ncdata = newnocursor_nocursors, color = colorNC)
Reachmodel(data = newnocursor_reaches, name =  "No-Cursor", condition = "nc", ncdata = newnocursor_nocursors, color = colorNC)
Reachmodel(data = newnocursor_reaches, name =  "No-Cursor", condition = "nc", ncdata = newnocursor_nocursors, color = colorNC)
Reachmodel(data = newnocursor_reaches, name =  "No-Cursor", condition = "nc", ncdata = newnocursor_nocursors, color = colorNC)
Reachmodel(data = newnocursor_reaches, name =  "No-Cursor", condition = "nc", ncdata = newnocursor_nocursors, color = colorNC)
dev.off()


svglite(file='figs/MLMC fig 2_test.svg', width=15, height=10,pointsize = 13, system_fonts=list(sans = "Arial"))
layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE), heights=c(1,1))
Reachmodel(data = active_reaches, name =  "Active", condition = "loc", loc_data = active_localization, color = colorA)
Reachmodel(data = passive_reaches, name =  "Passive", condition = "loc", loc_data = passive_localization, color = colorPA)
ExposureDatas(exposure_localization, exposure_reaches)
plotpropmodels()
dev.off()

