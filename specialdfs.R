# spezielle data.frames für Analysen Unfallstatistik Münster 2016
radbet <- subset(vums2016gnamed, Hauptverursacher == "Rad" | Zweitbeteiligter == "Rad")
kfzbet <- subset(vums2016gnamed, Hauptverursacher == "KFZ" | Zweitbeteiligter == "KFZ")
fusbet <- subset(vums2016gnamed, Hauptverursacher == "Fuß" | Zweitbeteiligter == "Fuß")
KFZvsRad <- subset(vums2016gnamed, (Hauptverursacher == "KFZ" & Zweitbeteiligter == "Rad") | 
                     (Hauptverursacher == "Rad" & Zweitbeteiligter == "KFZ"))
KFZvsRad$Hauptverursacher <- factor(KFZvsRad$Hauptverursacher, levels = c("KFZ", "Rad"))
KFZvsRad$Zweitbeteiligter <- factor(KFZvsRad$Zweitbeteiligter, levels = c("KFZ", "Rad"))
KFZvsFus <- subset(vums2016gnamed, (Hauptverursacher == "KFZ" & Zweitbeteiligter == "Fuß") | 
                     (Hauptverursacher == "Fuß" & Zweitbeteiligter == "KFZ"))
KFZvsFus$Hauptverursacher <- factor(KFZvsFus$Hauptverursacher, levels = c("KFZ", "Fuß"))
KFZvsFus$Zweitbeteiligter <- factor(KFZvsFus$Zweitbeteiligter, levels = c("KFZ", "Fuß"))
RadvsFus <- subset(vums2016gnamed, (Hauptverursacher == "Rad" & Zweitbeteiligter == "Fuß") | 
                     (Hauptverursacher == "Fuß" & Zweitbeteiligter == "Rad"))
RadvsFus$Hauptverursacher <- factor(RadvsFus$Hauptverursacher, levels = c("Rad", "Fuß"))
RadvsFus$Zweitbeteiligter <- factor(RadvsFus$Zweitbeteiligter, levels = c("Rad", "Fuß"))

#