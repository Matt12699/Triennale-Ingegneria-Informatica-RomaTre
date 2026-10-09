# Triennale in Ingegneria Informatica – Roma Tre

Raccolta del materiale che ho usato per preparare gli esami della laurea triennale in **Ingegneria Informatica** all'Università degli Studi **Roma Tre**: appunti, esercizi, esami passati, progetti e slide dei corsi.

## A cosa serve

L'idea è semplice: avere tutto in un unico posto, organizzato per materia, così chi inizia (o sta preparando un esame) non deve rincorrere appunti sparsi tra gruppi, drive e colleghi.

Usalo come supporto allo studio, non come sostituto delle lezioni: programmi, docenti e modalità d'esame possono cambiare da un anno all'altro, quindi controlla sempre le informazioni aggiornate sul sito del corso.

## Struttura del repository

Il materiale è diviso per materia. Ogni cartella può contenere:

| Dove | Cosa c'è |
|---|---|
| `Materia - ….pdf` | Appunti scritti in LaTeX, pronti da leggere |
| `LaTeX/` | Sorgenti dei PDF sopra: ogni documento è un progetto indipendente (`main.tex` + immagini) da ricompilare in locale o su [Overleaf](https://www.overleaf.com) |
| `Appunti/` | Altri appunti presi durante le lezioni, in PDF (gli originali Word sono in `Appunti/Word/`) |
| `Esami ed esercizi/`, `Esercizi svolti/`, `Esercitazioni/` | Tracce d'esame, esoneri ed esercizi, spesso svolti |
| `Slide/` | Slide e materiale dei docenti |
| `Codice/`, `Laboratori Kathara/` | Codice di esercitazioni, notebook e laboratori |

```
Fisica I/
├── Fisica I - Appunti.pdf
├── Fisica I - Ripetizioni.pdf
├── Esercizi svolti/
└── LaTeX/
    ├── Appunti/          (main.tex + immagini)
    └── Ripetizioni/
```

| Materia | Contenuto |
|---|---|
| [Algoritmi e Strutture Dati](Algoritmi%20e%20Strutture%20Dati) | Appunti scritti a mano |
| [Analisi dei Sistemi ad Eventi](Analisi%20dei%20Sistemi%20ad%20Eventi) | Appunti scritti a mano |
| [Analisi e Progettazione del Software](Analisi%20e%20Progettazione%20del%20Software) | Appunti e Teoria (LaTeX), contratti delle operazioni di homework ed esoneri |
| [Basi di Dati](Basi%20di%20Dati) | Cardinalità (LaTeX), prova parziale 2023/24, esercitazione SQL con soluzioni |
| [Calcolatori Elettronici](Calcolatori%20Elettronici) | Slide annotate ed esercizi, primo e secondo esonero |
| [Economia Applicata all'Ingegneria](Economia%20Applicata%20all'Ingegneria) | Appunti e Domande (LaTeX), appunti delle lezioni |
| [Elettrotecnica ed Elettronica](Elettrotecnica%20ed%20Elettronica) | Esame (LaTeX) |
| [Fisica I](Fisica%20I) | Appunti, Ripetizioni, Ripetizioni Parte 2 (LaTeX), esami passati ed esercizi svolti a mano |
| [Fondamenti d'Automatica](Fondamenti%20d'Automatica) | Appunti, ripasso, teoria, Matlab, parte integrativa di teoria, appunti di teoria di Martina Sasso |
| [Fondamenti di Telecomunicazioni](Fondamenti%20di%20Telecomunicazioni) | Appunti scritti a mano (divisi in 3 parti per le dimensioni) e teoria |
| [Intelligenza Artificiale e Machine Learning](Intelligenza%20Artificiale%20e%20Machine%20Learning) | Appunti ed esoneri (LaTeX), notebook Python (strutture dati, ricerca greedy/A*, hill climbing, simulated annealing) |
| [Programmazione Funzionale](Programmazione%20Funzionale) | Orale (LaTeX), esercitazioni ed esoneri in OCaml con soluzioni |
| [Programmazione Orientata agli Oggetti](Programmazione%20Orientata%20agli%20Oggetti) | Slide del corso |
| [Reti di Calcolatori](Reti%20di%20Calcolatori) | Appunti, Esame, Katharà (LaTeX), appunti delle lezioni, laboratori Katharà |
| [Ricerca Operativa I](Ricerca%20Operativa%20I) | Slide annotate, esami ed esoneri svolti, preparazione all'esame scritta a mano |
| [Sistemi Informativi su Web](Sistemi%20Informativi%20su%20Web) | Siw Book (LaTeX), slide del corso, esercizi HTML/CSS e Spring Boot |
| [Sistemi Operativi](Sistemi%20Operativi) | Appunti ed Esame (LaTeX), appunti delle lezioni, esercizi in C (processi, thread, produttore/consumatore, allocazione della memoria) |
> 📌 Altro materiale (appunti scritti a mano ed esercizi) verrà aggiunto nel tempo.

### Nota sul contenuto

Il materiale è stato scritto durante gli anni della triennale e non ne ricordo più perfettamente tutto il contenuto. Prima della pubblicazione i file sono stati riorganizzati e controllati con l'aiuto di un'intelligenza artificiale (per esempio: dati personali, file inutili, immagini non usate, errori di compilazione LaTeX), ma **non è stata fatta una revisione completa dei contenuti**. Potrebbero quindi esserci errori, imprecisioni o parti non aggiornate: usa questi appunti con spirito critico e confrontali sempre con il materiale ufficiale del corso.

### Homework, esoneri ed esercizi svolti

Le soluzioni di homework, esoneri ed esami sono le mie (o quelle raccolte durante il corso) e **non sono garantite corrette**. Usale come riferimento per capire come affrontare gli esercizi e per confrontarti dopo averli provati da solo, non per copiarle: è il modo migliore per imparare davvero e per evitare problemi con i docenti.

### Materiale mancante

Non tutti gli esami del corso di laurea sono presenti. Se una materia (o parte di essa) non c'è, molto probabilmente il materiale è andato perso nel tempo.

### Risorse esterne

- **Elettrotecnica ed Elettronica**: appunti completi di Giacomo Sturm, con sorgenti LaTeX e figure, disponibili nel suo repository: [00Darxk/Elettrotecnica-ed-Elettronica](https://github.com/00Darxk/Elettrotecnica-ed-Elettronica)
- **Sistemi Informativi su Web**: gli esempi ufficiali di Bootstrap usati durante il corso si scaricano da [getbootstrap.com/docs/5.3/examples](https://getbootstrap.com/docs/5.3/examples/)
- **Sistemi Informativi su Web**: il progetto d'esame TicketLuna (Spring Boot) è disponibile nel suo repository: [kiss0234/Progetto-SIW-2025-](https://github.com/kiss0234/Progetto-SIW-2025-)

## Licenza, diritti d'autore e materiale di terzi

Gli appunti, gli esercizi svolti e i progetti realizzati da me sono distribuiti con licenza [**Creative Commons BY-NC-SA 4.0**](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.it) (testo completo nel file [LICENSE](LICENSE)): puoi condividerli e modificarli liberamente, a patto di **citare la fonte**, **non usarli a scopo commerciale** e **ridistribuire eventuali versioni modificate con la stessa licenza**.

**Tutto il materiale che non è mio** (ad esempio slide, dispense, testi d'esame ed esercitazioni fornite dai docenti, o materiale di altri studenti) **resta di proprietà dei rispettivi autori**, che ne detengono tutti i diritti, e **non è coperto dalla licenza sopra**. È incluso qui solo a scopo didattico e senza fini di lucro.

Durante gli anni di studio ho raccolto anche appunti ed esercizi svolti da altri studenti e tutor, passati di mano in mano tra gruppi e colleghi (ad esempio parte degli esercizi svolti e dei tutoraggi di Fisica I e alcune soluzioni di Programmazione Funzionale). Di molti di questi **non conosco gli autori**: il merito è loro, e se riconosci un tuo lavoro puoi chiedermi di aggiungere il tuo nome o di rimuoverlo.

Un ringraziamento in particolare a:
- **Martina Sasso**, per gli appunti di teoria di Fondamenti d'Automatica

Se sei l'autore di uno di questi contenuti e preferisci che venga rimosso o che venga indicata una diversa attribuzione, apri una [issue](../../issues) o contattami: provvederò il prima possibile.

## Contribuire

Hai trovato un errore negli appunti o vuoi aggiungere materiale? Apri una issue o una pull request, ogni contributo è benvenuto.

---

*Se questo materiale ti è stato utile, lascia una ⭐ al repository: aiuta altri studenti a trovarlo!*
