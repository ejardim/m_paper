# settings
ages <- 0:6
age_max <- 6
length_at_age <- c(11.4,15.3,18.1,19.9,21.1,21.8,22.8)
weight_at_age <- c(21,43,52,67,71,76,82)

#From ICES WKPELA 2017
k <- 0.44
Linf <- 23.2

#Derived from values in WKPELA 2017
L50 <- 15.32

#Then
M1 <- 4.899 * age_max^(-0.916)

#Current SS3 model assumption for natural mortality
M2 <- c(0.98,0.61, 0.47, 0.40, 0.36, 0.35, 0.32) 

#Lorenzen
M3 <- 3 * weight_at_age^(-0.288)

#Charnov
M4 <- k * (Linf / length_at_age)^1.5

#Brodziak I don't know which formula is correct, the second is basically Charnov but w/ maturity
M5 <- k * L50/length_at_age     # código EJ
M5 <- k * (Linf / length_at_age)^1.5 * (L50 / Linf)^0.5     #internet
