# Einlesen und umcodieren für Unfallstatistik Münster 2016
#
# einlesen
vums2016 <- read.csv("VU PP 2016-varnamen.csv", header = TRUE)
# neue variablen
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
Folgen <- rep(NA, length(vums2016$T))
Folgen[!is.na(vums2016$LV)] <- "LV"
Folgen[!is.na(vums2016$SV)] <- "SV"
Folgen[!is.na(vums2016$"T")] <- "T"
Folgen[is.na(Folgen)] <- "Sach"
#vums2016gnamed <- data.frame(vums2016gnamed, Folgen = Folgen)
Ursache <- rep(NA, length(vums2016$X2..Ursache))
Ursache[vums2016$X2..Ursache %in% c(1:4)] <- "nicht verkehrstüchtig"
Ursache[vums2016$X2..Ursache %in% c(8:11)] <- "falsche Straßenbenutzung"
Ursache[vums2016$X2..Ursache %in% c(12,13)] <- "zu schnell"
Ursache[vums2016$X2..Ursache %in% c(14,15)] <- "zu geringer Abstand"
Ursache[vums2016$X2..Ursache %in% c(16:23)] <- "Fehler beim Überholen"
Ursache[vums2016$X2..Ursache %in% c(24:26)] <- "Fehler beim Spurwechsel, Vorbeifahren"
Ursache[vums2016$X2..Ursache %in% c(27:33)] <- "Missachtung der Vorfahrt"
Ursache[vums2016$X2..Ursache %in% c(34:37)] <- "Fehler beim Abbiegen"
Ursache[vums2016$X2..Ursache %in% c(38:42)] <- "Fehlverhalten gegenüber Fußgängern"
Ursache[vums2016$X2..Ursache %in% c(43:45)] <- "unzulässiges Parken / Halten"
Ursache[vums2016$X2..Ursache %in% c(46,50)] <- "fehlendes / falsches Licht"
Ursache[vums2016$X2..Ursache %in% c(47,48)] <- "Überbesetzung / Überladung"
Ursache[vums2016$X2..Ursache %in% c(49,89)] <- "sonstige Fehler / Ursachen"
Ursache[vums2016$X2..Ursache %in% c(51:55)] <- "technische Mängel"
Ursache[vums2016$X2..Ursache %in% c(60:69)] <- "Fehler von Fußgängern"
Ursache[vums2016$X2..Ursache %in% c(70:79)] <- "Straßenverhältnisse"
Ursache[vums2016$X2..Ursache %in% c(80:84)] <- "Witterungseinflüsse"
Ursache[vums2016$X2..Ursache %in% c(85:88, 90)] <- "Straßenverhältnisse"
Ursache[is.na(Ursache)] <- "sonstige Fehler / Ursachen"
#vums2016gnamed <- data.frame(vums2016gnamed, Ursache = Ursache)
Typg <- rep(NA, length(vums2016$Typ))
Typg[vums2016$Typ %in% 101:199] <- "Fahrunfall"
Typg[vums2016$Typ %in% 201:299] <- "Abbiege-Unfall"
Typg[vums2016$Typ %in% 301:399] <- "Einbiegen/Kreuzen-Unfall"
Typg[vums2016$Typ %in% 401:499] <- "Überschreiten-Unfall"
Typg[vums2016$Typ %in% 501:599] <- "Unfall durch ruhenden Verkehr"
Typg[vums2016$Typ %in% 601:699] <- "Unfall im Längsverkehr"
Typg[vums2016$Typ %in% 701:799] <- "sonstiger Unfall"
vums2016gnamed <- data.frame(vums2016, Hauptverursacher = X01g, Zweitbeteiligter = X02g,
                             Folgen = Folgen, Ursache = Ursache, Typg = Typg)
vums2016gnamed$Folgen <- factor(vums2016gnamed$Folgen, levels = c("T", "SV", "LV", "Sach"))
vums2016gnamed$Typg <- factor(vums2016gnamed$Typg, levels = c("Fahrunfall", "Abbiege-Unfall", "Einbiegen/Kreuzen-Unfall",
                                                              "Überschreiten-Unfall", "Unfall durch ruhenden Verkehr",
                                                              "Unfall im Längsverkehr", "sonstiger Unfall"))

#
