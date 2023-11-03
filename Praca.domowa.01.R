ImiÄ: Eliasz
Nazwisko: Szczepski
Numer albumu: 29594

# --------------------------------------------------------
# Zadanie.01 
#
#	Wygeneruj wektor liczb losowych x1. 
#   Dziedzina losowania to liczby caĹkowite od 1 do 100.
#   Wektor posiada 100 elementĂłw. 
#   Elementy wektora nie mogÄ siÄ powtarzaÄ.
#	Podaj sumÄ wartoĹci elementĂłw wektora. 
#	WskazĂłwka: skorzystaj z funkcji 'sample'.
#
# --------------------------------------------------------
# tu wpisz swĂłj kod: 



x1 <- sample(1:100, 100, replace = FALSE)

#polecenie tego w zasadzie nie obejmuje, ale dodatkowo dodałem print() 
#aby upewnić się, że wektor losuje się poprawnie
print(x1)
cat("Suma wartości elementów wektora x1:", sum(x1), "\n")


# --------------------------------------------------------
# Zadanie.02
#
#	Wygeneruj wektor liczb losowych x2. 
#   Dziedzina losowania to liczby caĹkowite {1,2,3}.
#   Wektor posiada 50 elementĂłw. 
#   Elementy wektora MOGÄ siÄ powtarzaÄ.
#	Policz liczbÄ wystÄpieĹ kaĹźdej liczby w wektorze. 
#	WskazĂłwka: skorzystaj z funkcji 'table'.
#
# --------------------------------------------------------
# tu wpisz swĂłj kod: 


x2 <- sample(1:3, 50, replace = TRUE)
zestawienie_liczb <- table(x2)

print(zestawienie_liczb)


# --------------------------------------------------------
# Zadanie.03
#
#	StwĂłrz losowe sĹowo oĹmioliterowe. 
#   Dziedzina losowania to wektor 'letters'.
#   Elementy wektora nie mogÄ siÄ powtarzaÄ.
#	WskazĂłwka: skorzystaj z funkcji 'paste', aby poĹaczyÄ elementy wektora w sĹowo. 
#
# --------------------------------------------------------
# tu wpisz swĂłj kod: 



zbior_liter <- sample(letters, 8, replace = FALSE)
generowane_slowo <- paste(zbior_liter, collapse = "")
print(generowane_slowo)


# --------------------------------------------------------
# Zadanie.04
#
#	StwĂłrz wektor x4 kolejnych liczb caĹkowitych od 1 do 10. 
#   KaĹźdy z elemntĂłw wektora podstaw jako promieĹ r i wylicz dla nirgo
#	 - obwĂłd koĹa
#	 - pole koĹa
#	 - powierzchniÄ kuli (sferÄ)
#	 - objÄtoĹÄ kuli
#	Wszystkie wyniki zgromadĹş w macierzy o 10 wierszach i 5 kolumnach. 
#	wyĹwietl macierz wynikĂłw z dokĹadnoĹciÄ do dwĂłch miejsc dziesiÄtnych.
#
#      [,1]  [,2]   [,3]    [,4]    [,5]
# [1,]    1  6.28   3.14   12.57    4.19
# [2,]    2 12.57  12.57   50.27   33.51
# [3,]    3 18.85  28.27  113.10  113.10
# [4,]    4 25.13  50.27  201.06  268.08
# [5,]    5 31.42  78.54  314.16  523.60
# [6,]    6 37.70 113.10  452.39  904.78
# [7,]    7 43.98 153.94  615.75 1436.76
# [8,]    8 50.27 201.06  804.25 2144.66
# [9,]    9 56.55 254.47 1017.88 3053.63
# [10,]   10 62.83 314.16 1256.64 4188.79
#
# --------------------------------------------------------
# tu wpisz swĂłj kod: 



x <- 1:10
wyniki <- matrix(0, nrow = 10, ncol = 5)


for (i in x) {
  r <- x[i]
  obwod_kola <- 2 * pi * r
  pole_kola <- pi * r^2
  powierzchnia_kuli <- 4 * pi * r^2
  objetosc_kuli <- (4/3) * pi * r^3
  wyniki[i,] <- c(r, obwod_kola, pole_kola, powierzchnia_kuli, objetosc_kuli)
}

print(round(wyniki, 2))


# --------------------------------------------------------
# Zadanie.05
# 
# Policz ile jest wspĂłlnych wielokrotnoĹci liczb 2 i 3
# w zbiorze licz caĹkowitych od 1 do 1500.  
# WskazĂłwka: skorzystaj z funkcji 'intersect'.
# 
# --------------------------------------------------------
# tu wpisz swĂłj kod: 


wielokrotnosci_2 <- seq(2, 1500, by = 2)
wielokrotnosci_3 <- seq(3, 1500, by = 3)
wspolne_wielokrotnosci <- intersect(wielokrotnosci_2, wielokrotnosci_3)

print(length(wspolne_wielokrotnosci))




