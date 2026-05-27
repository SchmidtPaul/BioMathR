# Design: Excel-Export für `get_teams_attendance()`

Datum: 2026-05-27
Status: abgestimmt, bereit zur Implementierung

## Ziel

`get_teams_attendance()` kann optional eine Excel-Datei mit zwei Tabellenblättern
schreiben:

1. **Zusammengeführt** - der zusammengeführte Tibble (mit E-Mail- und
   Namens-Merging), wie ihn die Funktion bisher zurückgibt.
2. **Roh** - der unkonsolidierte Tibble (eine Zeile je distinktem Anzeigenamen,
   kein Merging), bisher als Attribut `attr(result, "unmerged")` verfügbar.

Die Datei wird in den Ordner der importierten CSV-Dateien geschrieben.

## Signatur

```r
get_teams_attendance <- function(files,
                                 unit = c("hours", "minutes"),
                                 match_by_email = TRUE,
                                 merge_contained_names = TRUE,
                                 export = TRUE,
                                 xlsx_path = NULL)
```

Änderungen gegenüber dem Bestand:

- `unit`: Default ist jetzt **`"hours"`** (erstes Element von `match.arg`).
- `export = TRUE` (neu): schreibt standardmäßig eine xlsx und gibt den Tibble
  **unsichtbar** (`invisible()`) zurück, plus eine Hinweismeldung mit dem
  Zielpfad. `export = FALSE` stellt das alte Verhalten her (sichtbarer Tibble,
  keine Datei).
- `xlsx_path = NULL` (neu): Zielpfad. Bei `NULL` wird
  `file.path(dirname(files[1]), "teams_attendance.xlsx")` verwendet. Ein gesetzter
  Pfad **impliziert** `export = TRUE`. Fehlende `.xlsx`-Endung wird ergänzt.

Eine vorhandene Zieldatei wird ohne Nachfrage überschrieben
(`openxlsx`, `overwrite = TRUE`).

## Kontrollfluss

Die Kern-Berechnung (`result` samt `unmerged`-Attribut) bleibt unverändert.
Neuer Block am Funktionsende:

```r
if (export || !is.null(xlsx_path)) {
  path <- resolve_xlsx_path(xlsx_path, files)
  write_attendance_xlsx(result, attr(result, "unmerged"), path)
  rlang::inform(sprintf("Excel-Datei geschrieben: %s", path))
  return(invisible(result))
}
result
```

## Neue interne Helfer (`@noRd`)

### `resolve_xlsx_path(xlsx_path, files)`

- `NULL` → `file.path(dirname(files[1]), "teams_attendance.xlsx")`.
- Sonst: `.xlsx`-Endung ergänzen, falls fehlend (case-insensitive).

### `write_attendance_xlsx(merged, unmerged, path)`

Baut die Arbeitsmappe mit `openxlsx`:

1. `wb <- openxlsx::createWorkbook()`.
2. Zwei Blätter: `"Zusammengeführt"` (= `merged`), `"Roh"` (= `unmerged`).
3. Pro Blatt:
   - `writeData()`.
   - Kopfzeile fett (`createStyle(textDecoration = "bold")` auf Zeile 1).
   - Freeze pane erste Zeile (`freezePane(firstRow = TRUE)`).
   - Auto-Spaltenbreite (`setColWidths(widths = "auto")`).
   - Farbskala auf den Dauerspalten (alle Spalten außer `name` und `email`):
     `conditionalFormatting(type = "colourScale", style = c("#FFFFFF", "#63BE7B"))`.
     Weiß = niedrige, Grün = hohe Anwesenheit. `NA`-Zellen (abwesend) bleiben
     ungefärbt.
4. `saveWorkbook(wb, path, overwrite = TRUE)`.

Dauerspalten werden generisch als „alle Spalten außer `name`/`email`" bestimmt,
unabhängig von der Anzahl der Tage.

## Dokumentation

- `unit`-Text auf Stunden-Default anpassen.
- Neue `@param export` und `@param xlsx_path`.
- `@return`: Hinweis auf unsichtbare Rückgabe bei Export und auf die beiden
  Blätter „Zusammengeführt"/„Roh".
- `@details`: kurzer Absatz zu Blättern und Farbskala.
- Keine neue Dependency: `rlang` ist bereits Import; `rlang::inform()` für die
  Meldung. `openxlsx` ist bereits Import.

## Beispiele

- Default-`@examples` auf `export = FALSE` umstellen (kein Schreiben beim
  `R CMD check`).
- `\donttest{}`-Block mit `xlsx_path = tempfile(fileext = ".xlsx")`.

## Tests

- Bestehende Logik-Tests: `export = FALSE` ergänzen (keine Dateien schreiben).
- Neue Tests:
  - `xlsx_path = tempfile(...)` → Datei existiert; Rückgabe `expect_invisible`.
  - Genau zwei Blätter mit Namen `"Zusammengeführt"`/`"Roh"`
    (`openxlsx::getSheetNames`).
  - Inhalt Blatt 1 entspricht dem Tibble (Spaltenzahl/-namen via
    `openxlsx::read.xlsx`).
  - `xlsx_path` ohne Endung → `.xlsx` ergänzt.
  - `resolve_xlsx_path(NULL, files)` → erwarteter Default-Pfad.
  - Aufräumen via `withr::local_tempdir()` / `withr::defer()`.

## Sonstiges

- Version-Bump 0.8.3 → 0.8.4.
- `NEWS` / Commit beim Implementieren.
- `_pkgdown.yml`-Eintrag prüfen (ggf. bereits vorhanden).
```
