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
vums2016gnamed <- vums2016g
names(vums2016gnamed)[c(42, 43)] <- c("Hauptverursacher", "Zweitbeteiligter")

Folgen <- rep(NA, length(vums2016$T))
Folgen[!is.na(vums2016$LV)] <- "LV"
Folgen[!is.na(vums2016$SV)] <- "SV"
Folgen[!is.na(vums2016$"T")] <- "T"
Folgen[is.na(Folgen)] <- "Sach"

vums2016gnamed <- data.frame(vums2016gnamed, Folgen = Folgen)
vums2016gnamed$Folgen <- factor(vums2016gnamed$Folgen, levels = c("T", "SV", "LV", "Sach"))


# ----------------

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

#fill <- list(
#  aes(fill = X02g),
#  #theme(legend.title = element_blank()),
#  scale_fill_brewer(palette = "Blues"))
#prodplot(subset(vums2016g, !is.na(X02g)), ~ X02g + X01g, mosaic()) + fill

verursacherMosaic <- prodplot(subset(vums2016gnamed, !is.na(Zweitbeteiligter)),
                              ~ Zweitbeteiligter + Hauptverursacher, mosaic()) + 
  aes(fill = Zweitbeteiligter) + scale_fill_brewer(palette = "Blues")
verursacherMosaic

verursacherMosaic2 <- prodplot(subset(vums2016gnamed, !is.na(Zweitbeteiligter)),
                               ~ Hauptverursacher + Zweitbeteiligter, mosaic()) + 
  aes(fill = Hauptverursacher) + scale_fill_brewer(palette = "Blues")
verursacherMosaic2

verursacherMosaic3 <- prodplot(subset(vums2016gnamed, !is.na(Zweitbeteiligter)),
                              ~ Hauptverursacher | Zweitbeteiligter, mosaic()) + 
  aes(fill = Hauptverursacher) + scale_fill_brewer(palette = "Blues")
verursacherMosaic3

# nur Unfälle mit Radbeteiligung
radbetg <- subset(vums2016g, X01g == "Rad" | X02g == "Rad")
radbetgnamed <- subset(vums2016gnamed, Hauptverursacher == "Rad" | Zweitbeteiligter == "Rad")

# Kreuztabelle Verursacher Unfälle mit Radbeteiligung
# Zeile: 01, Spalte: 02
table(radbetg$X01g, radbetg$X02g, useNA = "always")
prop.table(table(radbetg$X01g)) # enthält auch die Alleinunfälle
prop.table(table(radbetg$X01g[!is.na(radbetg$X02g)]))

radbetX01 <- ggplot(subset(radbetgnamed, !is.na(Zweitbeteiligter)), aes(x = Hauptverursacher)) + geom_bar() + 
  ylab("Anzahl Radunfälle (ohne Alleinunfälle)")
radbetX01

radbetX01Folgen <- ggplot(subset(radbetgnamed, !is.na(Zweitbeteiligter)), aes(x = Hauptverursacher, fill = Folgen)) +
  geom_bar(position = "stack") + scale_fill_brewer(palette = "Reds", direction = -1) +
  ylab("Anzahl Radunfälle (ohne Alleinunfälle)")
radbetX01Folgen

kfzbetg <- subset(vums2016g, X01g == "KFZ" | X02g == "KFZ")
kfzbetgnamed <- subset(vums2016gnamed, Hauptverursacher == "KFZ" | Zweitbeteiligter == "KFZ")

kfzbetX01 <- ggplot(subset(kfzbetgnamed, !is.na(Zweitbeteiligter)), aes(x = Hauptverursacher)) + geom_bar() + 
  ylab("Anzahl KFZ-Unfälle (ohne Alleinunfälle)")
kfzbetX01
prop.table(table(kfzbetgnamed$Hauptverursacher))

kfzbetX01Folgen <- ggplot(subset(kfzbetgnamed, !is.na(Zweitbeteiligter)), aes(x = Hauptverursacher, fill = Folgen)) +
  geom_bar(position = "stack") + scale_fill_brewer(palette = "Reds", direction = -1) +
  ylab("Anzahl KFZ-Unfälle (ohne Alleinunfälle)")
kfzbetX01Folgen


# KFZ vs. Rad
# -----------
KFZvsRad <- subset(vums2016gnamed, (Hauptverursacher == "KFZ" & Zweitbeteiligter == "Rad") | (Hauptverursacher == "Rad" & Zweitbeteiligter == "KFZ"))
KFZvsRad$Hauptverursacher <- factor(KFZvsRad$Hauptverursacher, levels = c("KFZ", "Rad"))
KFZvsRad$Zweitbeteiligter <- factor(KFZvsRad$Zweitbeteiligter, levels = c("KFZ", "Rad"))
dim(KFZvsRad) # 700 Unfälle zwischen Rad und KFZ
prop.table(table(KFZvsRad$Hauptverursacher)) # Bei 62.6% dieser Unfälle ist der KFZ-Fahrer Hauptverursacher

#table(KFZvsRad$Hauptverursacher, KFZvsRad$Zweitbeteiligter)
KFZvsRadX01 <- ggplot(KFZvsRad, aes(x = Hauptverursacher)) + geom_bar() + ylab("Anzahl Unfälle")
KFZvsRadX01

KFZvsRadX01Folgen <- ggplot(KFZvsRad, aes(x = Hauptverursacher, fill = Folgen)) + 
  geom_bar(position = "stack") + ylab("Anzahl Unfälle") + scale_fill_brewer(palette = "Reds", direction = -1)
KFZvsRadX01Folgen

# besser:
KFZvsRadX01Mosaic <- prodplot(KFZvsRad, ~ Hauptverursacher, mosaic(direction = "h"))
KFZvsRadX01Mosaic

KFZvsRadX01FolgenMosaic <- prodplot(KFZvsRad, ~ Folgen + Hauptverursacher, mosaic()) + 
  aes(fill = Folgen) + scale_fill_brewer(palette = "Reds", direction = -1)
KFZvsRadX01FolgenMosaic

table(KFZvsRad$Folgen)
table(KFZvsRad$X2..Ursache)


# KFZ vs. Fuß
# -----------
KFZvsFu <- subset(vums2016gnamed, (Hauptverursacher == "KFZ" & Zweitbeteiligter == "Fuß") | (Hauptverursacher == "Fuß" & Zweitbeteiligter == "KFZ"))
KFZvsFu$Hauptverursacher <- factor(KFZvsFu$Hauptverursacher, levels = c("KFZ", "Fuß"))
KFZvsFu$Zweitbeteiligter <- factor(KFZvsFu$Zweitbeteiligter, levels = c("KFZ", "Fuß"))
dim(KFZvsFu) # 117 Unfälle zwischen Fuß und KFZ
prop.table(table(KFZvsFu$Hauptverursacher)) # Bei 61.5% dieser Unfälle ist der KFZ-Fahrer Hauptverursacher

table(KFZvsFu$Hauptverursacher, KFZvsFu$Zweitbeteiligter)
KFZvsFuX01 <- ggplot(KFZvsFu, aes(x = Hauptverursacher)) + geom_bar() + ylab("Anzahl Unfälle")
KFZvsFuX01

KFZvsFuX01Folgen <- ggplot(KFZvsFu, aes(x = Hauptverursacher, fill = Folgen)) + 
  geom_bar(position = "stack") + ylab("Anzahl Unfälle") + scale_fill_brewer(palette = "Reds", direction = -1)
KFZvsFuX01Folgen

# besser:
KFZvsFuX01Mosaic <- prodplot(KFZvsFu, ~ Hauptverursacher, mosaic(direction = "h"))
KFZvsFuX01Mosaic

KFZvsFuX01FolgenMosaic <- prodplot(KFZvsFu, ~ Folgen + Hauptverursacher, mosaic()) + 
  aes(fill = Folgen) + scale_fill_brewer(palette = "Reds", direction = -1)
KFZvsFuX01FolgenMosaic

table(KFZvsFu$Folgen)






# KFZ vs all
KFZvsall <- subset(vums2016gnamed, (Hauptverursacher == "KFZ") | (Zweitbeteiligter == "KFZ"))
dim(KFZvsall) # 10108 Unfälle mit KFZ-Beteiligung
table(KFZvsall$Hauptverursacher, KFZvsall$Zweitbeteiligter)
prop.table(table(KFZvsall$Hauptverursacher)) # Bei 87.3% dieser Unfälle ist ein KFZ-Fahrer Hauptverursacher


#  