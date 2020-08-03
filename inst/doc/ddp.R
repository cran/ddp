## -----------------------------------------------------------------------------
library(ddp)
data("simulasi")
kalori(simulasi, output = "energi")
#all output
kalori(simulasi)

## -----------------------------------------------------------------------------
skorpph(simulasi, wilayah = "Banten")
skorpph(simulasi, wilayah = "Maluku")

