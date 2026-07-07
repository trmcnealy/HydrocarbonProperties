#Header
options(scipen = 999)
# Enable debug mode for a function
debug(ls)
# Exit debug mode after testing
undebug(ls)

rm(list = ls())       # Clear all variables
graphics.off()        # Close all plots
cat("\014")           # Clear console

if (!require("HydrocarbonProperties", quietly = TRUE)) {
  message("Package 'HydrocarbonProperties' is not installed. Installing now...")
  install.packages('HydrocarbonProperties')
  #install.packages('HydrocarbonProperties', repos = c('https://hwborchers.r-universe.dev', 'https://cloud.r-project.org'))
  library(HydrocarbonProperties)  # Load after installation
}

PrintTable <- function(colNames, values) {
  print(paste(unlist(colNames), collapse = ","))

  #for (i in seq_along(values[1])){
    for (j in seq_along(values[,1])){
      print(paste(unlist(values[j,]), collapse = ","))
    }
  #}
}

PlotHydrocarbonProperties <- function(pressures, ylabel, func,...) {
  property_values <- array(dim = c(length(pressures)))

  for (i in seq_along(property_values)){

    property_values[i] <- func(pressures[i],...)
  }

  min_x<-0
  min_y<-0
  max_x<-10000
  max_y<-max(property_values)

  bordercol <- "#0000007F"
  labelcex <- 1

  plot_x<-pressures
  plot_y<-property_values

  #Hydrocarbon Properties Plot
  plot(plot_x, plot_y,
       xlab="Pressure, psi", ylab=ylabel,
       main=ylabel)
  grid()


  #points(plot_x, plot_y, col = "red", lwd = 1, cex = 1)
  lines(plot_x, plot_y, col = "red", lwd = 5)

}


Soi <- 0.4
Sgi <- 0.001 #(1 - Soi - Swi)
Swi <- (1 - Soi - Sgi)


Pr <- 5700
TVD <- 9500
Reserv.Temp <- 160

oilAPI <- 43
Gammao <- APItoSG(oilAPI)
Gammag <- 0.77

Rsi <- 1000 #TVD*0.11111

Reserv.TempR <- Reserv.Temp + 459.67


Zi <- DranchukAbuKassemGasCompressibilityFactor(Pr, Reserv.Temp, Gammag)

HydrocarbonPropertiesPressure <- seq(from = 100, to = 6000, by = 10)

#PlotHydrocarbonProperties(HydrocarbonPropertiesPressure, "Pb", WolfcampBubblepointPressure, Reserv.Temp, oilAPI, Gammag, TVD*0.11111)
Pbp <- WolfcampBubblepointPressure(Reserv.Temp, oilAPI, Gammag, Rsi)

# PlotHydrocarbonProperties(HydrocarbonPropertiesPressure, "Bw", McCainWaterFormationVolumeFactor, Reserv.Temp)
# PlotHydrocarbonProperties(HydrocarbonPropertiesPressure, "Muw", McCainWaterViscosity, Reserv.Temp, Salinity)
# PlotHydrocarbonProperties(HydrocarbonPropertiesPressure, "Rsw", McCainWaterSolutionGas, Reserv.Temp)

PlotHydrocarbonProperties(HydrocarbonPropertiesPressure, "Muo", KhanOilViscosity, Pbp, FtoR(Reserv.Temp), Gammao, Gammag, Rsi)

#for (api_oil in seq(from = 30, to = 60, by = 1)){
  PlotHydrocarbonProperties(HydrocarbonPropertiesPressure, "Rs", VelardeSolutionGasOilRatio, Reserv.Temp, Pbp, oilAPI, Gammag)
  PlotHydrocarbonProperties(HydrocarbonPropertiesPressure, "Rs", DindorukChristmanSolutionGasOilRatio, Reserv.Temp, Pbp, oilAPI, Gammag)
  PlotHydrocarbonProperties(HydrocarbonPropertiesPressure, "Bo", OilFormationVolumeFactor, Reserv.Temp, Pbp, oilAPI, Gammag)

  # PlotHydrocarbonProperties(HydrocarbonPropertiesPressure, OilDensity, "Rhoo", Bo, Gammao, Gammag)

  PlotHydrocarbonProperties(HydrocarbonPropertiesPressure, "Z", DranchukAbuKassemGasCompressibilityFactor, Reserv.Temp, Gammag)
  PlotHydrocarbonProperties(HydrocarbonPropertiesPressure, "Mug", CarrKobayashiBurrowsGasViscosity, Reserv.Temp, Gammag, 0, 0, 0)
  PlotHydrocarbonProperties(HydrocarbonPropertiesPressure, "Bg", GasFormationVolumeFactor, Reserv.Temp, Pbp, Zi, 0, Gammag)

  PlotHydrocarbonProperties(HydrocarbonPropertiesPressure, "Rv", VolatileOilGasRatio, Reserv.Temp, Pbp, 1, oilAPI, Gammag)

  PlotHydrocarbonProperties(HydrocarbonPropertiesPressure, "Rhog", GasDensityFromZ, Reserv.Temp, Gammag, Zi)
#}

#bg <- GasFormationVolumeFactor(Pr, Reserv.Temp, Pbp, Zi, oilAPI, Gammag)
#dBgdP <- CentralDifferenceMethod(GasFormationVolumeFactor, Pr, Reserv.Temp, Pbp, Zi, oilAPI, Gammag)

#print((((-1/bg)*dBgdP)))


#StandingOilSolutionGas <- function(SGo_API, SGg)

############

# stop("Exit")
#
#
# pressures <- seq(from = 100, to = 6000, by = 10)
#
# Rsi <- array(dim = c(length(pressures)))
# Boi <- array(dim = c(length(pressures)))
# Muoi <- array(dim = c(length(pressures)))
# Zi <- array(dim = c(length(pressures)))
# Bgi <- array(dim = c(length(pressures)))
# Mugi <- array(dim = c(length(pressures)))
# Bwi <- array(dim = c(length(pressures)))
# Muwi <- array(dim = c(length(pressures)))
# Rswi <- array(dim = c(length(pressures)))
# Bti <- array(dim = c(length(pressures)))
# CGR <- 1
# Rvi <- array(dim = c(length(pressures)))
#
# co_compressibilities <- array(dim = c(length(pressures)))
# cg_compressibilities <- array(dim = c(length(pressures)))
# cw_compressibilities <- array(dim = c(length(pressures)))
# cf_compressibilities <- array(dim = c(length(pressures)))
# ct_compressibilities <- array(dim = c(length(pressures)))
#
# for (i in seq_along(pressures)){
#
#   pressure <- pressures[i]
#
#   Rsi[i] <- VelardeSolutionGasOilRatio(pressure, Reserv.Temp, Pbp, oilAPI, Gammag)
#   #Rsi <- DindorukChristmanSolutionGasOilRatio(pressure, Reserv.Temp, Pbp, oilAPI, Gammag)/3
#   Boi[i] <- OilFormationVolumeFactor(pressure, Reserv.Temp, Pbp, oilAPI, Gammag)
#   Muoi[i] <- KhanOilViscosity(pressure, Pbp, FtoR(Reserv.Temp), Gammao, Gammag, Rsi)
#
#   Zi[i] <- DranchukAbuKassemGasCompressibilityFactor(pressure, Reserv.Temp, Gammag)
#   Bgi[i] <- GasFormationVolumeFactor(pressure, Reserv.Temp, Pbp, Zi, oilAPI, Gammag) #(0.005035 * Z * Temperature) / (Pr)
#
#   yCO2 <- 0.0; yN2 <- 0.0; yH2S <- 0.0
#   Mugi[i] <- CarrKobayashiBurrowsGasViscosity(pressure, Reserv.Temp, Gammag, yCO2, yN2, yH2S)
#
#   Bwi[i] <- McCainWaterFormationVolumeFactor(pressure, Reserv.Temp)
#   Muwi[i] <- McCainWaterViscosity(pressure, Reserv.Temp)
#   Rswi[i] <- McCainWaterSolutionGas(pressure, Reserv.Temp)
#
#   Bti[i] <- TotalFormationVolumeFactor(Boi, Bgi, Rsi, Rsi)
#
#   CGR <- 1
#   Rvi[i] <- VolatileOilGasRatio(pressure, Reserv.Temp, Pbp, CGR, oilAPI, Gammag)
#
#   compressibilities <- TotalCompressibility(pressure, Reserv.Temp, Pbp, oilAPI, Gammag, Soi, Sgi, Swi, cf=default_cf, salinity=0)
#
#   co_compressibilities[i] <- compressibilities$co
#   cg_compressibilities[i] <- compressibilities$cg
#   cw_compressibilities[i] <- compressibilities$cw
#   cf_compressibilities[i] <- compressibilities$cf
#   ct_compressibilities[i] <- compressibilities$ct
#
# }
#
#
# colNames <- c("OilAPI", "GasSG", "Pr","Pbp","Rsi","Boi","Muoi","Zi","Bgi","Mugi","Bwi","Muwi","Rswi","co","cg","cw","cf","ct")
# values <- data.frame(oilAPI, Gammag,
#                      pressures,Pbp,
#                      Rsi,Boi,Muoi,
#                      Zi,Bgi,Mugi,
#                      Bwi,Muwi,Rswi,
#                      co_compressibilities,cg_compressibilities,cw_compressibilities,cf_compressibilities,ct_compressibilities)
#
#
#
# PrintTable(colNames, values)
#
#
# #print(StandingOilSolutionGas(44, 0.8))

















