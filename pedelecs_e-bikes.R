# wieviele Pedelecs, E-Bikes und normale Räder in den Unfallzahlen?

table(vums2016$X01[vums2016$X01 %in% c(3, 70:79)])
table(vums2016$X02[vums2016$X02 %in% c(3, 70:79)])
# keine Eintragungen für 03 (E-Bikes), dafür 2 bzw. 3 Einträge für 73 (unbekannt)
# 71 = Rad
# 72 = Pedelec

X01rad <- vums2016$X01[vums2016$X01 %in% c(3, 70:79)]
X02rad <- vums2016$X02[vums2016$X02 %in% c(3, 70:79)]
radallein <- (vums2016$X01 %in% c(3, 70:79) & is.na(vums2016g$X02))[vums2016$X01 %in% c(3, 70:79)]
radarten <- data.frame(Radart = c(X01rad, X02rad),
                       Beteiligung = c(rep("Hauptverurusacher", length(X01rad)), rep("Zweitbeteiligter", length(X02rad))),
                       Alleinunfall = c(ifelse(radallein, "Ja", "Nein") , rep("Nein", length(X02rad))))

table(radarten$Radart)
table(radarten$Beteiligung)
table(radarten$Alleinunfall)

ggplot(radarten, aes(x = Radart)) + geom_bar() + ylab("Anzahl")
#  geom_text(data = data.frame(table(radarten$Radart)), aes(x = Var1), label = Freq, inherit.aes = FALSE)
ggplot(radarten, aes(x = Radart, fill = Alleinunfall)) + geom_bar(position = "stack") + ylab("Anzahl")

prodplot(radarten, ~ Alleinunfall | Radart, mosaic()) + aes(fill = Alleinunfall)
prodplot(radarten, ~ Beteiligung | Radart, mosaic()) + aes(fill = Beteiligung)


#