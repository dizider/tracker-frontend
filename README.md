# Tracker frontend 
(semestrální práce z předmětu AFP)

## Motivace
Aplikace bude tvořit uživatelské rozhraní pro venkovní hru. Skupiny hráčů dostanou zařízení, tzv. tracker, který bude sledovat jejich polohu a odesílat ji přes internet na server. Zároveň budou zařízení porovnávat svou aktuální polohu s uloženými pozicemi. Pokud hráči dosáhnou cíle (pozice) s určitou přesností, tracker jim přehraje mluvené slovo, které je bude navigovat k další pozici. Cílem hráčů je co nejdříve dojít do posledního bodu.
## Technický popis
* Trackery odesílají své aktuální pozice na server. Přijímané pozice jsou serverem přiřazovány do tras, dle mappingu v databázi. Každá přijatá pozice je tak anotována identifikátorem trasy. 
* Jednotlivé trasy lze získat v GPX formátu ze serveru.
## Use cases
* Vytvoření a přiřazení nové trasy k trackeru
* Zobrazení jednotlivých tras do mapového podkladu
	* filtrování a CRUD oprace s trasami
* Mapový podklad `Mapy.cz` 
	* nutnost zobrazovat data do turistických map
* Zobrazení aktuální polohy všech aktivních trackerů do mapového podkladu
* Live aktualizace aktuálních pozic
	* odesílání dat přes WebSockets
* Autorizovaný přístup
	* Google OAuth

## Build

- `npm run build` - vytvoří produkční build
- `npm run start` - spustí server s produkčním buildem
- `npm run dev-server` - spustí dev server s projektem
- `npm run test` - spustí testy

Před spuštěním prudkční verze je nutné nastavit environment variables (lze i v souboru `.env`).