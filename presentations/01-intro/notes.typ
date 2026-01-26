= Speaker Notes - Prezentacja "Vim w OCamlu"

== Wstęp (Czas: ok. 1 min)

**Slajd 1: Tytuł**
- "Dzień dobry, nazywam się Filip Figzał. Tematem mojego projektu jest **Vim w
  OCamlu**. Chciałbym zaprezentować podejście do stworzenia modalnego edytora
  tekstu, ale w czysto funkcyjnym wydaniu."

**Slajd 2: Outline**
- "Oto krótki plan prezentacji. Przejdziemy od celów, przez architekturę, aż po
  największe wyzwania techniczne, czyli wydajność i zarządzanie stanem."
  - *Wskazówka: Nie czytaj punktów z tabeli, szkoda czasu. Przełącz dalej po
    5-10 sekundach.*

---

== Motywacja i Cele (Czas: ok. 2 min)

**Slajdy 3-6: Cel i Dlaczego?**
- "Jaki jest główny cel? Chcę stworzyć nie tylko edytor, ale dowód koncepcji.
  Chcę pokazać, że można zbudować niezawodny edytor modalny, wykorzystując siłę
  programowania funkcyjnego."
- "Dlaczego w ogóle to robię? Jeśli spojrzymy na kod źródłowy typowych edytorów,
  często znajdziemy tam »spaghetti« mutowalnego stanu. Ktoś, kto pisał pluginy
  do Vima czy VS Code, wie, jak łatwo zepsuć stan edytora. Moim celem jest
  architektura, która eliminuje te problemy u podstaw."

**Slajdy 7-11: Podstawowe funkcje**
- "Co ten edytor będzie potrafił? Oczywiście podstawy: wyświetlanie stanu w
  terminalu i operacje na plikach."
- "Ale kluczowe są tu dwie rzeczy:"
  1. "**Drzewiasta historia zmian**. W podejściu imperatywnym to trudne do
    zaimplementowania. W funkcyjnym, dzięki trwałym strukturom danych, historię
    (undo/redo) dostajemy niemal »za darmo«."
  2. "**Modalność**. Tryby (Normal, Insert, Command) oraz nawigacja klawiszami
    `hjkl` czy poruszanie się po słowach będą odwzorowywać zachowanie Vima."

---

== Architektura i Model (Czas: ok. 3 min) – *To jest najważniejsza część
techniczna!*

**Slajdy 12-16: Architektura (MVU)**
- "Przejdźmy do architektury. Zdecydowałem się na model **Jednego Źródła
  Prawdy**."
- "Cała aplikacja opiera się na cyklu:"
  1. **View:** Czysta funkcja renderująca stan na ekran terminala.
  2. **Update:** To serce edytora. Czysta funkcja typu
    `State -> Event -> State`.
- "Dzięki temu testowanie edytora jest trywialne – nie muszę mockować
  skomplikowanych obiektów, po prostu sprawdzam, czy dla danego stanu i klawisza
  otrzymuję oczekiwany stan wyjściowy."

**Slajdy 17-18: Struktury Danych**
- "Aby to zadziałało, potrzebuję odpowiednich struktur danych."
- "Po pierwsze: **Algebraiczne Typy Danych (ADT)** dla trybów. Dzięki temu
  kompilator OCamla pilnuje, abym nie znalazł się w niepoprawnym stanie."
- "Po drugie: **Reprezentacja tekstu**. Zamiast tablicy znaków czy listy linii,
  używam struktury **Zipper** (suwak). A konkretnie »Zipper of Zippers« dla
  optymalizacji wielu linii. Pozwala to na edycję w miejscu kursora w czasie
  stałym, bez konieczności kopiowania całego bufora tekstu."

---

== Technologie (Czas: ok. 1 min)

**Slajdy 19-21: Technologie**
- "Z czego korzystam? Nie wymyślam koła na nowo w kwestii renderowania – używam
  biblioteki **Notty**. Do obsługi I/O używam modułu **Unix**."
- "Mój wkład własny to cała logika biznesowa: zarządzanie buforem, algorytmy
  nawigacji (jak skok o słowo czy akapit) i zarządzanie historią zmian."

---

== Wyzwania i Rozwiązania (Czas: ok. 3 min) – *Tu przekonujesz profesora wiedzą*

**Slajd 23: Wyzwanie 1 - Wydajność**
- "Największe ryzyko w takim projekcie to wydajność. Pojawia się pytanie: jak
  edytować duży plik (np. 10 tys. linii) bez mutowania pamięci? Czy przy każdym
  wciśnięciu klawisza muszę kopiować całą tablicę?"
- "Rozwiązaniem są **Struktury Trwałe (Persistent Data Structures)**. Dzięki
  Zipperowi lokalne zmiany są $O(1)$, a współdzielenie struktury (structural
  sharing) sprawia, że zapamiętywanie historii zmian nie zjada nam całej pamięci
  RAM."

**Slajd 24: Wyzwanie 2 - Zarządzanie stanem**
- "Drugie wyzwanie to słynne »Vim hell«. Użytkownik nie wie, w jakim jest
  trybie, czy wpisuje komendę, czy tekst."
- "Tutaj OCaml błyszczy dzięki systemowi typów. Stany są modelowane jako ADT.
  Oznacza to, że niepoprawne przejścia między stanami są po prostu
  niereprezentowalne w kodzie – program się nie skompiluje, jeśli nie obsłużę
  wszystkich przypadków."

---

== Zakończenie (Czas: ok. 1 min)

**Slajd 25: Pytania**
- "Podsumowując: tworzę narzędzie, które łączy ergonomię Vima z niezawodnością
  OCamla. Chętnie odpowiem teraz na Państwa pytania."

**Slajd 26: Koniec**
- "Dziękuję za uwagę."

---

=== Wskazówki "Sceniczne":

1. **Nie spiesz się przy Zipperze (Slajd 18/23):** Jeśli zobaczysz, że masz
  zapas czasu, wyjaśnij Zippera dokładniej (metafora: "To jak dwie wieże z
  klocków - zdejmujemy z jednej i kładziemy na drugą, a kursor jest pośrodku").
  To pokazuje głębokie zrozumienie tematu.
2. **Mów do sali:** Profesor napisał: "Proszę wyobrazić sobie, że sala pełna
  jest inwestorów". Utrzymuj kontakt wzrokowy, nie czytaj ze slajdów.
3. **Entuzjazm:** Przy slajdzie o "spaghetti code" (Slajd 6) uśmiechnij się
  lekko – większość programistów na sali zna ten ból, to buduje więź z
  publicznością.
