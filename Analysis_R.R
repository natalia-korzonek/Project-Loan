install.packages("readxl")   
install.packages("dplyr")     
install.packages("ggplot2")   
install.packages("ggpubr")    
install.packages("magrittr")  

library(ggplot2)
library(magrittr) 
library(readxl)
library(dplyr)
library(ggpubr)

# Wczytanie danych 
file_path <- "C:/Users/Natalia/Downloads/Dane_pożyczki (1).xlsx" 
data <- read_excel("C:/Users/Natalia/Downloads/Dane_pożyczki (1).xlsx" )

# Wyświetlenie pierwszych kilku wierszy danych
print("Pierwsze kilka wierszy danych:")
head(data)

# Sprawdzenie struktury danych
str(data)

# Wyświetlenie unikalnych wartości w kolumnie miesiące
unique_months <- unique(data$data_spr)
print("Unikalne wartości w kolumnie 'Miesiąc':")
print(unique_months)

# Filtrowanie danych na czerwiec 2024
june_data <- data %>% filter(data_spr == '06.2024')

# Wyświetlenie danych z czerwca 2024
print("Dane z czerwca 2024:")
print(june_data)

# Statystyki opisowe
summary_stats <- summary(june_data$kw_pozyczki_pln)
print("Statystyki opisowe dla kwot w czerwcu 2024:")
print(summary_stats)

# Ustalanie wartości odstających
Q1 <- quantile(june_data$kw_pozyczki_pln, 0.25)
Q3 <- quantile(june_data$kw_pozyczki_pln, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Wartości odstające
outliers <- june_data %>% filter(kw_pozyczki_pln < lower_bound | kw_pozyczki_pln > upper_bound)

print("Wartości odstające:")
print(outliers)

# Wizualizacja rozkładu kwot pożyczek
ggplot(june_data, aes(x = kw_pozyczki_pln)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..count..), color = "blue") +
  geom_vline(xintercept = lower_bound, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = upper_bound, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Rozkład kwot pożyczek w czerwcu 2024", x = "Kwota", y = "Liczba pożyczek") +
  theme_minimal()

# Podsumowanie wyników analizy
if (nrow(outliers) > 0) {
  cat(sprintf("Zidentyfikowano %d wartości odstających w czerwcu 2024.\n", nrow(outliers)))
} else {
  cat("Nie zidentyfikowano wartości odstających w czerwcu 2024.\n")
}


# Podział danych na czerwiec 2024 i inne miesiące
june_data <- data %>% filter(data_spr == '06.2024')
other_months_data <- data %>% filter(data_spr != '06.2024')

# Obliczenie średniej kwoty dla czerwca 2024 i pozostałych miesięcy
mean_june <- mean(june_data$kw_pozyczki_pln)
mean_other_months <- mean(other_months_data$kw_pozyczki_pln)

cat("Średnia kwota pożyczek w czerwcu 2024:", mean_june, "\n")
cat("Średnia kwota pożyczek w pozostałych miesiącach:", mean_other_months, "\n")

# Test t-Studenta porównujący średnie kwoty w czerwcu 2024 z pozostałymi miesiącami
t_test_result <- t.test(june_data$kw_pozyczki_pln, other_months_data$kw_pozyczki_pln)

# Wyświetlenie wyników testu t-Studenta
print(t_test_result)

# Wizualizacja porównania średnich kwot - wykres pudełkowy
ggplot(data, aes(x = data_spr, y = kw_pozyczki_pln, color = data_spr)) +
  geom_boxplot() +
  labs(title = "Porównanie kwot pożyczek w czerwcu 2024 i innych miesiącach", 
       x = "Miesiąc", y = "Kwota") +
  theme_minimal()

#Wynik testu t-Studenta wskazuje, że istnieje istotna statystycznie różnica między średnimi kwotami pożyczek w czerwcu 2024 roku a pozostałymi miesiącami. 

# t = 17.061 – wartość statystyki t-Studenta. Im wyższa wartość t, tym większa różnica między średnimi w porównywanych grupach. Wartość 17.061 wskazuje na dużą różnicę.

# df = 128.74 – Liczba stopni swobody. Wyliczona liczba df (stopnie swobody) wynika z zastosowania testu Welcha, który pozwala na porównanie średnich z grup o różnej wariancji.

# p-value < 2.2e-16 – P-wartość jest znacznie mniejsza niż 0.05 (standardowy poziom istotności). Różnica między średnimi jest bardzo istotna statystycznie, co wyklucza przypadkowość różnicy.

# Test sprawdza hipotezę alternatywną, że średnie kwoty pożyczek są różne w czerwcu 2024 i pozostałych miesiącach. Wynik testu potwierdza tę hipotezę, co oznacza, że średnie nie są równe.

# Według przedziału ufności, z 95% pewnością rzeczywista różnica średnich kwot pożyczek między czerwcem 2024 a innymi miesiącami wynosi od 794,89 do 1003,45 PLN.

# srednia z czerwca 2024 wynosi 3588.506, natomiast z pozostalych miesiecy 2689.335. Średnia dla czerwca jest  znacznie wyższa niż w pozostałych miesiącach.

# Wysoka wartość t i bardzo niska p-wartość wskazują, że istnieje duza różnica między średnimi kwotami pożyczek w czerwcu 2024 a innych miesiącach. Ta różnica jest na tyle istotna, że można przypuszczać, że mogło dojść do manipulacji danymi lub innych nieprawidłowości w czerwcu.

# Średnia kwota pożyczek w czerwcu jest znacznie wyższa niż w pozostałych miesiącach (o około 900 PLN), co dodatkowo potwierdza tezę o możliwych anomaliach w danych.


