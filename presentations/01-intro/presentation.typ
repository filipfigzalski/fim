#import "@preview/touying:0.6.1": *
#import themes.university: *
#import "@preview/cetz:0.4.2"
#import "@preview/fletcher:0.5.8" as fletcher: edge, node
#import "@preview/numbly:0.1.0": *

#let cetz-canvas = touying-reducer.with(
  reduce: cetz.canvas,
  cover: cetz.draw.hide.with(bounds: true),
)
#let fletcher-diagram = touying-reducer.with(
  reduce: fletcher.diagram,
  cover: fletcher.hide,
)

#show: university-theme.with(
  aspect-ratio: "16-9",
  config-info(
    title: [Vim w OCamlu],
    subtitle: [Modalny edytor tekstowy w funkcyjnym wydaniu.],
    date: datetime.today(),
    author: [Filip Figzał],
  ),
)

// #set heading(numbering: numbly("{1}.", default: "1.1"))

#let listp(..items) = items.pos().map(i => [- #i]).join(pause)

#title-slide()

== Outline <touying:hidden>

#components.adaptive-columns(outline(title: none, indent: 1em))


= Założenia projektu

== Cel

Stworzenie #strike[uproszczonego] *niezawodnego* edytora modalnego, który
prezentuje siłę programowania funkcyjnego.

Dlaczego?

#pause
- Większość edytorów to "spaghetti" mutowalnego stanu.

#pause
- Ciekawe wyzwanie.

== Podstawowe funkcje

- Wyświetlanie pełnego stanu w terminalu.

#pause
- Tryby edytora (normalny, insert, command) modelowane jako *ADT*.

#pause
- Drzewiasta historia zmian.

#pause
- Operacje I/O na plikach (otwieranie/zapisywanie).

#pause
- Nawigacja po tekście:
  - h/j/k/l (podstawowe poruszanie się);
  - w/b/e (nawigacja po słowach).

= Architektura

== Zarys

#pause
/ Model: Jedno źródło prawdy.

#pause
/ View: Deklaratywne renderowanie stanu do bufora terminala.

#pause
/ Update: Czysta funkcja `State -> Event -> State`. Łatwa do testowania.

== Struktury Danych

=== Zarządzanie stanem

- Algebraiczny Typ Danych dla trybów.
- Zapobiega niepoprawnym stanom.

#pause
=== Reprezentacja tekstu

- *Zipper* zamiast zwykłej tablicy.

- *Zipper of Zippers*: optymalizacja dle wielu linii.

== Technologie

- Biblioteka `Notty` #sym.arrow deklaratywne renderowanie w terminalu.

#pause
- Moduł `Unix`:
  - I/O na plikach
  - obsługa naciśnięć w terminalu

#pause

- Wkład własny:
  - Logika edytora
  - Algorytmy nawigacji
  - Zarządzanie buforem i stanem

= Ryzyka

== Wyzwania i Rozwiązania

=== Wyzwanie 1: Wydajność
- *Problem:* Jak edytować 10k linii tekstu bez mutowania pamięci (kopiowania
  tablic)?
- *Rozwiązanie:* Struktury trwałe (Persistent Data Structures).
- *Implementacja:* Zipper (suwak) - lokalne zmiany są O(1), a historia zmian
  jest "darmowa".

== Wyzwania i Rozwiązania

=== Wyzwanie 2: Zarządzanie stanem
- *Problem:* "Vim hell" - czy jestem w trybie Insert? Czy wpisuję komendę?
- *Rozwiązanie:* Algebraiczne Typy Danych (ADT).
- *Efekt:* Niepoprawne stany są niemożliwe do reprezentowania w kodzie.

= Pytania

= Koniec
Dziękuję za uwagę.
