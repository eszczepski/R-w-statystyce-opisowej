#Imie: Eliasz
#Nazwisko: Szczepski
#Numer albumu: 29594

# --------------------------------------------------------
# Zadanie.06 
#
#	Firma sprzedaje trzy kategorie produktĂłw A, B oraz C. KaĹźdy z nich
# jest obĹoĹźony innÄ stawkÄ podatku VAT (odpowiednio: 8%, 10% i 20%). Pobierz
# kategoriÄ i cenÄ. UĹźyj wyraĹźenia warunkowego, aby wyliczyÄ cenÄ z podatkiem.
# Zakomunikuj wynik do uĹźytkownika. {uĹźyj if else}
#
# --------------------------------------------------------

##> ObowiÄzuje stawka VAT 8%. Cena wraz z podatkiem wynosi 54.
##> ObowiÄzuje stawka VAT 8%. Cena wraz z podatkiem wynosi 55.
##> ObowiÄzuje stawka VAT 8%. Cena wraz z podatkiem wynosi 60.


kategoria <- readline(prompt = "Podaj kategorie (A, B lub C): ")
cena <- as.numeric(readline(prompt = "Podaj cene: "))

if(kategoria == "A") {
  cena_z_podatkiem <- cena * 1.08  # Stawka VAT 8%
} else if(kategoria == "B") {
  cena_z_podatkiem <- cena * 1.10  # Stawka VAT 10%
} else if(kategoria == "C") {
  cena_z_podatkiem <- cena * 1.20  # Stawka VAT 20%
} else {
  cat("Bledna kategoria")
}

if(exists("cena_z_podatkiem")) {
  cat("Obowiazuje stawka VAT", ifelse(kategoria == "A", "8%", ifelse(kategoria == "B", "10%", "20%")), ". Cena wraz z podatkiem wynosi", cena_z_podatkiem, "\n")
}



#Zadanie.07
#
#	Oblicz iloczyn elementĂłw dowolnego wektora x za pomocÄ pÄtli while oraz repeat 
# {uĹźyj while, repeat}
# 
#	x <- 1:5



x <- 1:5

#pętla while
i <- 1
iloczyn_while <- 1
while (i <= length(x)) {
  iloczyn_while <- iloczyn_while * x[i]
  i <- i + 1
}

#pętla repeat
j <- 1
iloczyn_repeat <- 1
repeat {
  iloczyn_repeat <- iloczyn_repeat * x[j]
  j <- j + 1
  if (j > length(x)) {
    break
  }
}

cat("Iloczyn elementów wektora x za pomocą pętli while:", iloczyn_while, "\n")
cat("Iloczyn elementów wektora x za pomocą pętli repeat:", iloczyn_repeat, "\n")




#Zadanie.08
#
# UĹźyj zbioru airquality. Odpowiedz na pytania: 		
# a) ile jest przypadkĂłw w zbiorze
# b) ile jest przypadkĂłw z brakami danych
# c) ile, i jakich, jest zmiennych w zbiorze
# d) ile, i jakich, jest zmiennych z brakami danych
# 
# SformuĹuj peĹne odpowiedzi tekstowe. Wklej kod obliczeĹ.




data(airquality)

# a) Ile jest przypadków w zbiorze
ile_przypadkow <- nrow(airquality)
cat("a) Ilość przypadków w zbiorze airquality:", ile_przypadkow, "\n")

# b) Ile jest przypadków z brakami danych
ile_brakow <- sum(is.na(airquality))
cat("b) Ilość przypadków z brakami danych w zbiorze airquality:", ile_brakow, "\n")

# c) Ile, i jakich, jest zmiennych w zbiorze
ile_zmiennych <- ncol(airquality)
nazwy_zmiennych <- names(airquality)
cat("c) Ilość zmiennych w zbiorze airquality:", ile_zmiennych, "\n")
cat("   Nazwy zmiennych:", paste(nazwy_zmiennych, collapse = ", "), "\n")

# d) Ile, i jakich, jest zmiennych z brakami danych
brakujace_zmienne <- colnames(airquality)[apply(is.na(airquality), 2, any)]
ile_brakujacych_zmiennych <- length(brakujace_zmienne)
cat("d) Ilość zmiennych z brakami danych w zbiorze airquality:", ile_brakujacych_zmiennych, "\n")
cat("   Nazwy zmiennych z brakami danych:", paste(brakujace_zmienne, collapse = ", "), "\n")



#Zadanie.09
#
#	CiÄg Fibonacciego to ciÄg liczb naturalnych, taki Ĺźe kaĹźdy kolejny
# wyraz stanowi sumÄ dwĂłch poprzednich. 
#	https://pl.wikipedia.org/wiki/Ci%C4%85g_Fibonacciego
#	
#	UĹźyj pÄtli for, aby stworzyÄ 20 pierwszych wyrazĂłw ciÄgu.
#	Podaj ich sumÄ. {uĹźyj pÄtli for}


fibonacci <- numeric(20)
fibonacci[1] <- 0
fibonacci[2] <- 1


for (i in 3:20) {
  fibonacci[i] <- fibonacci[i - 1] + fibonacci[i - 2]
}


cat("Pierwsze 20 wyrazów ciągu Fibonacciego:", fibonacci, "\n")

suma_fibonacci <- sum(fibonacci)
cat("Suma pierwszych 20 wyrazów ciągu Fibonacciego:", suma_fibonacci, "\n")



#Zadanie.10
# 
# Wygeneruj 100-elementowy wektor z rozkĹadu Poisonna z parametrem 
# lambda=5 zapomocÄ komendy {rpois} i przypisz go do zmiennej x.
# Policz dla tego wektora: ĹredniÄ, sumÄ, odchylenie standardowe. 
# Zwizualizuj wektor za pomocÄ histogramu oraz wykresu pudeĹkowego (boxplot)
# (sĹupki w kolorze niebieskim, tytuĹ wykresu "RozkĹad Poissona"). 


lambda <- 5
x <- rpois(100, lambda)

srednia <- mean(x)
suma <- sum(x)
odchylenie_std <- sd(x)

cat("Średnia:", srednia, "\n")
cat("Suma:", suma, "\n")
cat("Odchylenie standardowe:", odchylenie_std, "\n")

hist(x, col = "blue", main = "Histogram - Rozkład Poissona", xlab = "Wartość", ylab = "Częstość")

boxplot(x, col = "blue", main = "Wykres pudełkowy - Rozkład Poissona", xlab = "Wartość")






Zadanie.11

# Wczytaj dane ze zbioru "countries of the world.csv" 
# Zapoznaj siÄ z danymi korzystajÄc np. z funkcji names(), summary(), str(), 
# dim(), itd., obejrzysz teĹź dane za pomocÄ funkcji View()
# ZamieĹ puste pola na wartoĹci NA: data[data==""] <- NA
# zmieĹ nazwy kolumn, zgodnie z nastÄpujÄcym wzorem:
# Area_sq_mi -> Area
# Pop_Density_per_sq_mi -> Pop_dens
# GDPper_capita -> GDP
# UsuĹ ze zbioru wszystkie zmienne poza: Country, Region, Population, Area, 
# Pop_dens, GDP, Literacy, Birthrate, Deathrate.
# UsuĹ spacje ze zmiennej Region (z przodu i z tyĹu nazw - funkcja str_trim)
#  
# Dla ilu krajĂłw wystÄpujÄ braki danych w zmiennej Literacy? UĹźyj funkcji is.na i sum
# WyĹwietl wszystkie kategorie zmiennej Region. Jaka jest dominanta? (RVAideMemoire::mod(?))
# SprawdĹş udziaĹ procentowy poszczegĂłlnych kategorii.
# Jaka jest Ĺrednia i mediana zmiennych Population, Area i GDP? 
# StwĂłrz histogram oraz wykres pudeĹkowy dla zmiennych GDP oraz Literacy. 
# Odpowiednio podpisz osie histogramu (uwzglÄdniajÄc teĹź jednostki)
# WyĹwietl kwartyle dla funkcji GDP. Teraz stwĂłrz nowÄ zmiennÄ - bÄdzie to zmienna 
# kategorycznÄ oparta na zmiennej GDP. Nowa zmienna:
# - powinna siÄ nazywaÄ "GDP_binned"
# - bÄdzie przyjmowaÄ cztery wartoĹci (kategorie):
# - pierwsza kategoria ma zawieraÄ wartoĹci od minimum do pierwszego kwartyla. Nazwij jÄ "Very_poor"
# - druga kategoria ma zawieraÄ wartoĹci od pierwszego kwartyla do mediany. Nazwij jÄ "Poor"
# - trzecia kategoria ma zawieraÄ wartoĹci od mediany do trzeciego. Nazwij jÄ "Medium"
# - czwarta kategoria ma zawieraÄ wartoĹci od mediany do trzeciego. Nazwij jÄ "Rich"
# - (uĹźyj funkcji ifelse albo case_when)  
# StwĂłrz nowÄ zmiennÄ logicznÄ, ktĂłra przyjmuje wartoĹÄ âTrueâ, kiedy wartoĹÄ 
# zmiennej âBirthrateâ jest wiÄksza lub rĂłwna od wartoĹci zmiennej âDeathrateâ,
# a wartoĹÄ âFalseâ w przeciwnym przypadku. WartoĹÄ âTrueâ w nowej zmiennej
# oznacza â najproĹciej ujmujÄc â Ĺźe w roku badania w danym kraju wiÄcej osĂłb siÄ
# urodziĹo niĹź umarĹo. StwĂłrz wykres sĹupkowy dla nowo powstaĹej zmiennej, 
#  pokazujÄc czÄstoĹci wystÄpowania pierwszej i drugiej kategorii.


install.packages("stringr")
install.packages("RVAideMemoire")

path_to_file <- "C:/Users/DELL G5/Downloads/countries of the world.csv"
data <- read.csv(file = path_to_file, header = TRUE, na.strings = "")

names(data) 
summary(data)
str(data)
dim(data)  
View(data) 

#Zamiana pustych pól na wartości NA
data[data == ""] <- NA

#Zmiana nazw kolumn
names(data) <- c("Country", "Region", "Population", "Area", "Pop_dens", "GDP", "Literacy", "Birthrate", "Deathrate")

#Usunięcie zbędnych zmiennych
data <- data[, c("Country", "Region", "Population", "Area", "Pop_dens", "GDP", "Literacy", "Birthrate", "Deathrate")]

#Usunięcie spacji z kolumny Region
library(stringr)
data$Region <- str_trim(data$Region)

#Dla ilu krajów występują braki danych w zmiennej Literacy?
braki_Literacy <- sum(is.na(data$Literacy))

#Wyświetlenie kategorii zmiennej Region oraz jej dominanta
library(RVAideMemoire)
kategorie_Region <- unique(data$Region)
dominanta_Region <- mod(data$Region)

#Sprawdzenie udziału procentowego kategorii Region
procent_kategorii <- prop.table(table(data$Region)) * 100

#Obliczenie średniej i mediany zmiennych Population, Area i GDP
srednia_Population <- mean(data$Population)
mediana_Population <- median(data$Population)
srednia_Area <- mean(data$Area)
mediana_Area <- median(data$Area)
srednia_GDP <- mean(data$GDP)
mediana_GDP <- median(data$GDP)

#Tworzenie histogramu i wykresu pudełkowego dla zmiennych GDP i Literacy
hist(data$GDP, main = "Histogram - GDP", xlab = "GDP ($ per capita)")
boxplot(data$Literacy, main = "Boxplot - Literacy", ylab = "Literacy (%)")

#Obliczenie kwartyli dla funkcji GDP
kwartyle_GDP <- quantile(data$GDP)

#Tworzenie nowej zmiennej kategorycznej GDP_binned
data$GDP_binned <- ifelse(data$GDP <= kwartyle_GDP[2], "Very_poor",
                          ifelse(data$GDP <= kwartyle_GDP[3], "Poor",
                                 ifelse(data$GDP <= kwartyle_GDP[4], "Medium", "Rich")))

#Tworzenie nowej zmiennej logicznej Birth_vs_Death
data$Birth_vs_Death <- data$Birthrate >= data$Deathrate

#Wykres słupkowy dla zmiennej Birth_vs_Death
barplot(table(data$Birth_vs_Death), main = "Częstość występowania Birth vs Death")






