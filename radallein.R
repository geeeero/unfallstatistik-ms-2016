# alleinunfälle bei Radfahrern

radallein <- subset(radbet, is.na(Zweitbeteiligter))
table(radallein$"X01...Alter.in.Jahren") # strange levels "", "?", "? " with zero observations
table(radallein$Folgen)
table(radallein$Ursache)
table(radallein$Typg)
table(radallein$"Alkohol.bei")
radallein <- data.frame(radallein,
                        Alter01 = as.numeric(radallein$"X01...Alter.in.Jahren"),
                        Alkohol = factor(!is.na(radallein$"Alkohol.bei"), labels = c("Nein", "Ja")))
table(radallein$Alter01, useNA = "ifany")
#hist(radallein$Alter01)

# rad allein Ursache & Alkohol im Spiel
raUr1  <- ggplot(radallein, aes(x = Ursache, fill = Alkohol)) +
  geom_bar(position = "stack") + scale_fill_manual(values = rep("light blue", 2)) +
  xlab("") + ylab("Anzahl Alleinunfälle Radfahrer") + coord_flip()
raUr1
raUr2 <- ggplot(radallein, aes(x = Ursache, fill = Alkohol)) +
  geom_bar(position = "stack") + scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  xlab("") + ylab("Anzahl Unfälle") + coord_flip()
raUr2

# rad allein Alter histogramm
raah1 <- ggplot(radallein, aes(x = Alter01)) + geom_histogram(binwidth = 10) +
  xlab("Alter") + ylab("Anzahl Alleinunfälle Radfahrer") +
  scale_x_continuous(breaks = (0:9)*10)
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
raah1
raah2 <- ggplot(radallein, aes(x = Alter01, fill = Ursache)) + geom_histogram(binwidth = 10) +
  xlab("Alter") + ylab("Anzahl Alleinunfälle Radfahrer") +
  scale_x_continuous(breaks = (0:9)*10) + scale_fill_brewer(palette = "YlOrRd", direction = -1)
raah2

#