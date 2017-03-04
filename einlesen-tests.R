# libraries
library(ggplot2)
library(productplots)
library(reshape2)
library(gridExtra)
library(knitr)
library(xtable)

# einlesen
vums2016 <- read.csv("VU PP 2016-varnamen.csv", header = TRUE)

table(vums2016$X01, useNA = "always")
table(vums2016$X02, useNA = "always")

# Alle Unfälle mit Radbeteiligung
radbet <- subset(vums2016, X01 %in% c(70:79) | X02 %in% c(70:79))
dim(radbet) # 984 Unfälle mit Radbeteiligung

# die zwei Unfälle mit einem Radfahrer-Toten:
subset(radbet, T == 1)

# Wer sind Hauptverursacher bei Unfällen mit Radfahrerbeteiligung?
table(radbet$X01)
table(radbet$X02, useNA = "always") # 86 NAs -> Alleinunfälle

radbetnall <- subset(radbet, !is.na(X02))
dim(radbetnall) # 898 Unfälle mit Radfahrerbeteiligung ohne Alleinunfälle
prop.table(table(radbetnall$X01)) # 


# Nur Unfälle Pkw (21) vs. Rad (71)
pkwrad <- subset(radbet, X01 == 21)
dim(pkwrad) # 384 Unfälle mit Hauptverursacher Pkw, 2. Beteiligter Rad
radpkw <- subset(radbet, X02 == 21)
dim(radpkw) # 240 Unfälle mit Hauptverursacher Rad, 2. Beteiligter Pkw

# alle Unfälle zwischen einem PKW und einem Radfahrer
pkwvsrad <- rbind(pkwrad, radpkw)
dim(pkwvsrad) # 624 Unfälle

prop.table(table(pkwvsrad$X01)) # Bei 62% dieser Unfälle ist der Pkw-Fahrer Hauptverursacher


pkwbet <- subset(vums2016, X01 == 21 | X02 == 21)
dim(pkwbet) # 9607 Unfälle mit Pkw-Beteiligung

table(pkwbet$T) # 3 Tote bei reinen Pkw-Unfällen
table(vums2016$T) # insgesamt 5 Unfalltote 2016

# Wer sind hauptverursacher bei Unfällen mit Pkw-Beteiligung?
table(pkwbet$X01)
table(pkwbet$X02, useNA = "always") # 667 NAs -> Alleinunfälle


#