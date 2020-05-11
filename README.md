# Interpreter

W folderze ./grama znajdują się pliki wygenerowane
przez BNFC z pliku gramatyka.

W folderze src znajdują się pliki:
 - semantics.hs - zawiera wszystkie reguły semantyczne 
 - interpret.hs - parsuje drzewo i przekazuje je do właściwego interpretera
 - staticAnalysis.hs - analizuje statycznie drzewo przed wykonaniem programu
 - programTypes.hs - typy monad użytych w semantics.hs i funkcje pomocnicze

W folderach ./bad i ./good są przykłady programów złych i dobrych.


# Opis języka
Język jest rozszerzeniem okrojonego C.
Posiada:
 - 3 wbudowane typy (int, bool, string)
 - standardową arytmetykę, porównania
 - zmienne i standardowe operacje na nich
 - tablice wielowymiarowe dla 3 podstawowych typów
 - for, if, while
 - funkcje, rekurencje
 - obsługa błędów wykonania (wypisuje odpowiedni komunikat o błędzie)
 - statyczne typowanie
 - słowo kluczone "count"
 - funkcje zagnieżdżone

oraz wbudowane funkcje:
 - printStr(string str)
 - printInt(int i)
 - readInt() 
 - readStr() 


Słowo kluczowe "count" umożliwia aktualizacje licznika przy każdej operacji
odczytu/zapisu danej zmiennej lub użyciu operatora. Każda zmienna może mieć 
tylko jeden "koszt" odczytu/zapisu i może on być zapisawany tylko do jednej 
zmiennej w danej chwili (po jednej na odczyt i zapis).

##Składnia:

```count <VAR1> read_cost/write_cost <EXP> with <VAR2>```

Obliczane jest wyrażenie EXP, a jego wynik będzie dodawany 
do zmiennej VAR2 z każdą operacją odczytu/zapisu na zmiennej VAR1. 


```count <OPERATOR> cost <EXP> with <VAR1>```

Obliczane jest wyrażenie EXP, a jego wynik będzie dodawany do 
zmiennej VAR1 z każdym użyciem operatora OPERATOR.


```get read_cost/write_cost <VAR1>```

Zwraca obecny koszt odczytu/zapisu zmiennej VAR1. 


```get cost <OPERATOR>```

Zwraca obecny koszt operatora <OPRATOR>.


##Przykłady:

```
int x = 0, a = 0, counter = 0;
count x read_cost 1 with counter;
a = x + x;
print(counter);
>>> 2
```

```
int counter = 0;
count < cost 2 + 2 with counter;
if (3 < 2) {
	print("what?");
}
print(get cost <);
print(counter);
>>> 4
>>> 4
```

Taka konstrukcja może być wykorzystana na przykład w zliczaniu
porównań w algorytmie albo symulowania czasu jego wykonania w zależności od 
kosztu dostępu do pamięci. Większy przykład znajduje się w ./good/bubble-sort.in
