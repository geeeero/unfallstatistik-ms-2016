# X01 und X02 vergröbert: siehe Kommentare in "notizen.txt"

X01g <- rep(NA, length(vums2016$X01))
X02g <- rep(NA, length(vums2016$X02))

X01g[vums2016$X01 %in% 70:79] <- "Rad"
X01g[vums2016$X01 %in% c(10:59, 201, 221)] <- "KFZ"
X01g[(vums2016$X01 %in% c(80:81, 83:88)) | (vums2016$X01 == 89 & vums2016$X2..Ursache %in% 60:69)] <- "Fuß"
X01g[(vums2016$X01 %in% c(1:9, 60:69, 82, 90:99)) | (vums2016$X01 == 89 & !(vums2016$X2..Ursache %in% 60:69))] <- "sonst"

X02g[vums2016$X02 %in% 70:79] <- "Rad"
X02g[vums2016$X02 %in% c(10:59, 201, 221)] <- "KFZ"
X02g[(vums2016$X02 %in% c(80:81, 83:88)) | (vums2016$X02 == 89 & vums2016$X2..Ursache %in% 60:69)] <- "Fuß"
X02g[(vums2016$X02 %in% c(1:9, 60:69, 82, 90:99)) | (vums2016$X02 == 89 & !(vums2016$X2..Ursache %in% 60:69))] <- "sonst"

vums2016g <- data.frame(vums2016, X01g, X02g)

# Kreuztabelle Verursacher Unfälle
# Zeile: 01, Spalte: 02
table(vums2016g$X01g, vums2016g$X02g, useNA = "always")

# Hauptverursacher (X01) nach Opfer (X02)
allX01 <- ggplot(subset(vums2016g, !is.na(X02g)), aes(x = X01g)) + geom_bar() +
  facet_grid(X02g ~ ., scales = "free") +
  xlab("Hauptverursacher") + ylab("Anzahl Unfälle")
allX01

# Opfer (X02) nach Hauptverursacher (X01)
allX02 <- ggplot(subset(vums2016g, !is.na(X02g)), aes(x = X02g)) + geom_bar() +
#  facet_grid(X01g ~ ., scales = "free") +
  facet_grid(X01g ~ .) +
  xlab("Opfer") + ylab("Anzahl Unfälle")
allX02

# nur Unfälle mit Radbeteiligung
radbetg <- subset(vums2016g, X01g == "Rad" | X02g == "Rad")

# Kreuztabelle Verursacher Unfälle mit Radbeteiligung
# Zeile: 01, Spalte: 02
table(radbetg$X01g, radbetg$X02g, useNA = "always")
prop.table(table(radbetg$X01g)) # enthält auch die Alleinunfälle
prop.table(table(radbetg$X01g[!is.na(radbetg$X02g)]))

radbetX01 <- ggplot(subset(radbetg, !is.na(X02g)), aes(x = X01g)) + geom_bar() + 
  xlab("Hauptverursacher") + ylab("Anzahl Unfälle")
radbetX01

# KFZ vs. Rad
KFZvsRad <- subset(vums2016g, (X01g == "KFZ" & X02g == "Rad") | (X01g == "Rad" & X02g == "KFZ"))
dim(KFZvsRad) # 700 Unfälle zwischen Rad und KFZ
prop.table(table(KFZvsRad$X01g)) # Bei 62.6% dieser Unfälle ist der KFZ-Fahrer Hauptverursacher

table(KFZvsRad$X01g, KFZvsRad$X02g)
KFZvsRadX01 <- ggplot(KFZvsRad, aes(x = X01g)) + geom_bar() + 
  xlab("Hauptverursacher") + ylab("Anzahl Unfälle")
KFZvsRadX01


# KFZ vs all
KFZvsall <- subset(vums2016g, (X01g == "KFZ") | (X02g == "KFZ"))
dim(KFZvsall) # 10108 Unfälle mit KFZ-Beteiligung
table(KFZvsall$X01g, KFZvsall$X02g)
prop.table(table(KFZvsall$X01g)) # Bei 87.3% dieser Unfälle ist ein KFZ-Fahrer Hauptverursacher


#  