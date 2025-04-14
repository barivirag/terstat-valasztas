# térstat-valasztas

Ez a repó Bari Virág Anna szakdolgozatához készült, amely az EP-választási eredmények térstatisztikai elemzését végzi településszinten, R nyelven.

## Tartalom

- `szakdolgozat_feltölteni.R` – az összes modell, elemzés és térbeli diagnosztika egy fájlban
- Térbeli regressziók: SLM, SEM, SDM, SpHet
- Adattisztítás, vizualizáció, térképek

## Használat

1. Nyisd meg RStudio-ban a projektet
2. Futtasd a `szakdolgozat_feltölteni.R` fájlt
3. A fájlban szerepel minden elemzési lépés sorrendben

## Követelmények

A kód az alábbi R csomagokat használja:
library(raster)
library(gstat)
library(sjmisc)
library(sjlabelled)
library(sjPlot)
library(sf)
library(dplyr)
library(tibble)
library(ggplot2)
library(stringi)
library(stringr)
library(dplyr)
library(ggplot2)
library(lmtest)
library(spdep)
library(spatialreg)
library(sp)
library(sphet)
library(broom)
