# X01 und X02 vergröbert

X01g <- rep(NA, length(vums2016$X01))
X02g <- rep(NA, length(vums2016$X02))

X01g[vums2016$X01 %in% 70:79] <- "Rad"
X01g[vums2016$X01 %in%  0:59] <- "KFZ"
X01g[vums2016$X01 %in% 80:89] <- "Fuß"
X01g[vums2016$X01 %in% c(60:69, 90:99)] <- "sonst"

X02g[vums2016$X02 %in% 70:79] <- "Rad"
X02g[vums2016$X02 %in%  0:59] <- "KFZ"
X02g[vums2016$X02 %in% 80:89] <- "Fuß"
X02g[vums2016$X02 %in% c(60:69, 90:99)] <- "sonst"

vums2016g <- data.frame(vums2016, X01g, X02g)

# Kreuztabelle Verursacher Unfälle
# Zeile: 01, Spalte: 02
table(vums2016g$X01g, vums2016g$X02g, useNA = "always")

# X01 und X02 vergröbert nur Radbeteiligung

X01g <- rep(NA, length(radbet$X01))
X02g <- rep(NA, length(radbet$X02))

X01g[radbet$X01 %in% 70:79] <- "Rad"
X01g[radbet$X01 %in%  0:59] <- "KFZ"
X01g[radbet$X01 %in% 80:89] <- "Fuß"
X01g[radbet$X01 %in% c(60:69, 90:99)] <- "sonst"

X02g[radbet$X02 %in% 70:79] <- "Rad"
X02g[radbet$X02 %in%  0:59] <- "KFZ"
X02g[radbet$X02 %in% 80:89] <- "Fuß"
X02g[radbet$X02 %in% c(60:69, 90:99)] <- "sonst"

radbetg <- data.frame(radbet, X01g, X02g)

# Kreuztabelle Verursacher Unfälle mit Radbeteiligung
# Zeile: 01, Spalte: 02
table(radbetg$X01g, radbetg$X02g, useNA = "always")

# KFZ vs. Rad
KFZvsRad <- subset(vums2016g, (X01g == "KFZ" & X02g == "Rad") | (X01g == "Rad" & X02g == "KFZ"))
dim(KFZvsRad) # 707 Unfälle zwischen Rad und KFZ
prop.table(table(KFZvsRad$X01g)) # Bei 62.5% dieser Unfälle ist der KFZ-Fahrer Hauptverursacher

table(KFZvsRad$X01g, KFZvsRad$X02g)


#  