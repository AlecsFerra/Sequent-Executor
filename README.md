# SequentExecutor
Programma mirato alla classificazione di proposizioni logiche secondo la logica classica svogliendo la derivazione del
sequente.

## Funzionamento
Dopo l'inserimento del sequente da elaborare il programma stamperà sul terminale la derivazione logica del sequente
annotando alla sinistra la regola utilizzata, succesivamente in caso il sequente non si riveli una tautologia procederà
con lo sviluppo della negazione del sequente.

## Sintassi
```
t ⊥ ¬'A' ('A'&'B')&'C'⊢'A'&('B'&'C') 'A' ∨ 'B' 'A' → 'B'
```
La sintassi rispetta quella della logica classica tranne che per 2 aspetti:
- Le proposizioni atomiche vanno circondate da apici singoli
- Gli elementi delle liste di proposizioni non vanno separate da virgole

Gli spazi sono totalmente ignorati dall' interprete.
Le liste prima e dopo il sequente possono anche essere vuote addirirttura in ambo i lati
```
⊢ 'A'&('B'&'C')
⊢
'A' ⊢
```
Sono tutte e 3 espressioni accettate.

## Suddivisione del progetto
Nella cartella app è presente solamente la logica di I/O
Mentre nella cartella src è presente tutta la logica di parsing e derivazione dei sequenti
