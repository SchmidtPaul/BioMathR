# Design: MS-Teams-Anwesenheitsberichte zusammenfassen

Datum: 2026-05-27
Status: validiert (Brainstorming), bereit zur Umsetzung

## Ziel

Eine BioMathR-Funktion, die mehrere heruntergeladene MS-Teams-Anwesenheitsberichte
(eine CSV pro Workshoptag) zu einem Tibble zusammenfasst: ein eindeutiger Nutzer pro
Zeile, eine Spalte pro Tag mit der Anwesenheitsdauer.

Vorbild ist der bestehende Zoom-Workflow (`C:\GitHub\zoom_participation`): pro Tag eine
Spalte (`pivot_wider`), ein Nutzer pro Zeile, chronologisch sortiert. Unterschied: Teams
liefert keine API-Daten, sondern heruntergeladene CSV-Dateien.

## Eingabeformat (bestaetigt an echter, anonymisierter Datei)

Flache, **kommagetrennte** CSV mit einer Zeile pro Teilnahme-Segment (kein
Mehr-Abschnitt-Format). Relevante Spalten:

| Spalte | Bedeutung |
|---|---|
| `display` | Anzeigename, z.B. `Max Mustermann (FIRMA)` -> Schluesselspalte |
| `userName` | E-Mail/UPN (oft leer, v.a. bei anonymen Gaesten) -> Merge-Hilfe |
| `userId` | `teamsvisitor:<guid>` bei Gaesten - pro Meeting neu, NICHT taguebergreifend stabil |
| `joinDateTime` / `leaveDateTime` | Dauer = Differenz; Datum = `as.Date(joinDateTime)` |
| `participantStatus` | z.B. `Left` |

Header-Beispiel:
`userId,display,userName,userIdType,participantId,joinDateTime,leaveDateTime,participantStatus,hasLicense,...`

## Funktions-API (umgesetzt: `get_teams_attendance()`)

Eine Funktion, Rueckgabe ein Tibble; die unzusammengefuehrte Roh-Version haengt
als `attr(result, "unmerged")` dran (fuer ein zweites Excel-Blatt).

```r
get_teams_attendance(
  files,                         # character: Pfade zu den Tages-CSVs
  unit = c("minutes", "hours"),  # Einheit der Dauer-Zellen
  match_by_email = TRUE,         # angemeldete Nutzer per E-Mail zusammenfuehren
  merge_contained_names = TRUE   # Namens-Varianten per Praefix zusammenfuehren
)
```

### Namens-Merge (merge_contained_names, Default an)

Normalisierter Praefix-Vergleich: Name wird lowercased und auf reine
Alphanumerik reduziert (Komma, Unterstrich, Klammern, Leerzeichen fallen weg).
Zwei Namen werden zusammengefuehrt, wenn der kuerzere (normalisiert) ein
**Praefix** des laengeren ist UND mindestens **2 Wort-Tokens** hat. Merging ist
**transitiv** (Union-Find auf Integer-Indizes -- bewusst NICHT ueber String-Namen,
sonst `subscript out of bounds` bei leeren/`NA`-Keys). Kanonischer Name = laengster.

- Loest: `"Max Mustermann"` / `"Max Mustermann, MRI"` / `"Max Mustermann_FIRMA"` /
  `"Max Mustermann BLE 624"` / `"Max Mustermann BLE624"` -> eine Zeile.
- Die ≥2-Woerter-Schwelle verhindert, dass `"Max"` Fremde anzieht
  (`"Maximilian"`, `"Max Power"` bleiben getrennt).
- Bewusst akzeptiert: gleiche Vor-/Nachnamen koennen faelschlich gemergt werden ->
  Abgleich ueber das `"unmerged"`-Attribut.

Leere `display`-Werte -> Platzhalter `"(ohne Namen)"`.

## Datenfluss

1. `purrr::map(files, parse_one)` - jede CSV einzeln einlesen -> langes Tibble je Datei:
   `display`, `email` (= `userName`), `date` (aus `joinDateTime`), `minutes`
   (= `leaveDateTime - joinDateTime`).
2. `bind_rows()` -> ein langes Tibble.
3. Merge-Schluessel: `key = if (match_by_email && email != "") email else display`.
   "Max M." / "Max Mustermann" fusionieren nur bei gleicher E-Mail; anonyme Gaeste
   bleiben ueber `display` getrennt.
4. Kanonischer Anzeigename je `key`: haeufigster, bei Gleichstand laengster `display`.
5. `group_by(key, name, email, date) |> summarise(minutes = sum(minutes))` - Rejoins am
   selben Tag werden addiert.
6. `pivot_wider(names_from = date, values_from = minutes)` - eine Spalte je Tag,
   chronologisch sortiert.
7. `arrange(name)`; bei `unit = "hours"` umrechnen (`/60`, gerundet wie im Zoom-Workflow).

## Rueckgabe

Tibble mit `name`, `email` (oft leer, sichtbar), dann je eine Datumsspalte (numerisch).
`NA` in einer Tageszelle = an dem Tag nicht erschienen (bewusst nicht `0`, damit
"kurz drin" von "gar nicht da" unterscheidbar bleibt).

Zellinhalt = Dauer (Entscheidung: nur Dauer je Tag, keine zusaetzliche ja/nein-Ableitung).

## Encoding

Ohne Zusatzpaket: erste Bytes via `readBin` lesen, BOM pruefen
(`FF FE` -> UTF-16LE), sonst UTF-8. Eingelesen mit base `read.csv(..., fileEncoding=)`.
Entscheidung gegen `readr` und `data.table::fread`: `read.csv` ist dependency-frei und
bei UTF-16 zuverlaessiger als `fread`.

## Edge Cases & Fehlerbehandlung

- `files` existieren nicht / leerer Vektor -> klarer Fehler (cli/assertthat).
- Fehlende Schluesselspalten (`display`, `joinDateTime`, `leaveDateTime`) -> Fehler mit
  Dateiname + gefundenen Spalten (faellt auf, falls Teams das Format aendert).
- Zwei Personen, gleicher Name, keine E-Mail -> eine Zeile (technisch nicht trennbar) - dokumentiert.
- Gleicher Name, unterschiedliche E-Mails -> getrennte Zeilen mit identischem `name` - dokumentiert.
- Null-/Negativdauer (Join=Leave) -> 0 Minuten, Rohwert bleibt erhalten.
- Mehrere Berichte mit gleichem Datum -> auf dieselbe Datumsspalte summiert.

## Tests (testthat 3)

Fixtures: 2-3 kleine anonymisierte Mini-CSVs in `tests/testthat/fixtures/`, inkl.
anonymem `teamsvisitor` ohne E-Mail und einem Rejoin.

Faelle:
- Rejoins pro Tag korrekt summiert.
- Angemeldeter Nutzer mit zwei `display`-Varianten + gleicher E-Mail -> eine Zeile, kanonischer Name.
- Anonymer Gast ohne E-Mail -> Match nur ueber `display`.
- Datumsspalten chronologisch, `NA` fuer Abwesenheit.
- `unit = "hours"` rechnet korrekt um.
- Fehlende Schluesselspalte / nicht existierende Datei -> aussagekraeftiger Fehler.
- Snapshot des fertigen Tibbles.

## Dokumentation

- Roxygen mit `@examples` ueber Fixtures (`system.file()`), Hinweis auf erwartetes
  Teams-Format und bekannte Matching-Grenzen.
- NEWS-/pkgdown-Eintrag analog zu bestehenden Funktionen.

## Abhaengigkeiten

Keine neue Hard-Dependency. Nutzt vorhandene Imports (`dplyr`, `tidyr`, `purrr`,
`tibble`, `rlang`) + base `read.csv`.
