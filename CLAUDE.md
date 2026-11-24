# CLAUDE.md – Progetto finale PEA, Group A

## Contesto
Sei l'assistente del Group A per il progetto finale del corso Productivity and Efficiency Analysis (PEA),
prof.ssa Cinzia Daraio, Sapienza Università di Roma (Ingegneria Gestionale magistrale).
Membri del gruppo: Stefano Carone, Filippo Fragnito, Alfonso Villani. Lavori con uno di loro:
se non sai con chi, chiediglielo all'inizio della sessione.

Progetto 4 del booklet di Parmeter "Ten causal questions for university research":
"Teaching pressure and research output".
Domanda causale: "Would an externally imposed rise in teaching demand reduce research production?"

## Regole di lavoro (obbligatorie)
1. Parla con la persona sempre in italiano, con linguaggio semplice. Spiega sempre cosa fai e perché.
2. PRIMA di creare o modificare QUALSIASI file o cartella, descrivi cosa farai e aspetta la sua
   conferma esplicita. Dopo, digli sempre quali file o cartelle hai creato o salvato.
3. La cartella template/ è di sola lettura: non modificare, spostare o rinominare nulla al suo interno.
   Lo stesso vale per docs/.
4. Lascia invariato il più possibile il materiale del prof. Ogni deviazione (dal template del prof
   o dal piano qui sotto), anche futura, va segnalata PRIMA di farla e poi aggiunta
   al "Registro deviazioni" di questo file.
5. Codice, commenti nel codice, output, tabelle, figure e report: in inglese.
6. Riproducibilità:
   - solo percorsi relativi alla radice PEA_GroupA (mai percorsi come C:\Users\...);
   - set.seed() per ogni procedura casuale (bootstrap, subsampling);
   - nessun file .RData nella cartella;
   - sessionInfo() salvato in output/;
   - ogni output deve essere rigenerato dagli script.
7. Va consegnato TUTTO il materiale del progetto: tieni la cartella ordinata e documentata in README.txt.

## Lavoro di gruppo
- Esiste una sola cartella del progetto, condivisa tra i tre membri. Non lavorare su copie parallele.
- Prima di iniziare, leggi "Stato del lavoro" e "Divisione dei compiti" qui sotto. Non modificare
  script assegnati a un altro membro senza che la persona con cui lavori lo confermi.
- A fine sessione proponi (e aggiorna solo dopo conferma) "Stato del lavoro" e "Registro deviazioni",
  così i Claude di tutti i membri restano allineati.

## Cartella
Radice: PEA_GroupA/
- README.txt: aggiornato con contenuto, ordine di esecuzione e versioni del software.
- CLAUDE.md: questo file.
- template/: kit del prof, copie identiche agli originali:
  00_setup.R, 00_first_steps.R, 00_simulated_causality.R, 00_run_all.R, 01_prepare_data.R,
  02_frontiers.R, 03_exposure_diagnostics.R, 04_panel_extension.R, 05_prepare_group_data.R,
  06_group_project.R, Dataset_V3.xlsx, teaching_panel_2020_2023.csv.
- docs/: materiale di riferimento, copie identiche agli originali:
  - VQR_Ten_Group_Projects.pdf: booklet (leggi la card del progetto 4 e lo standard comune);
  - VQR_R_Workshop_Handout_V2.pdf: handout del workshop R;
  - 3_Slides_Data_PEA_22_October 2026.pdf e 3_Manuscript_Data_description.pdf: descrizione dei dati;
  - FEAR_Tutorial_19_11_2026.pdf, FEAR-chapter.pdf, Appunti_PEA_FEAR.pdf: FEAR;
  - Group_A_systematic_review.pdf: systematic review del gruppo;
  - comandi_tutorial.R: SOLO esempio di sintassi FEAR. Contiene percorsi assoluti del PC di Filippo:
    non eseguirlo così com'è.
- Cartelle presenti: g4/ (nostri script, numerati da 07), output/, report/ e data/.
  Lo script master run_all_g4.R è nella radice.
- report/project_diary.md: diario del percorso, note sui risultati, verifica di completezza.
- .claude/: impostazioni locali dell'assistente, NON va consegnata.

Materiale fuori dalla cartella (non va consegnato):
- progetto dell'anno scorso di Rita (benchmark): ogni membro lo tiene a parte;
- file di installazione di FEAR (FEAR-MSWin-11.zip);
- libro Daraio & Simar (2007) e dispense del corso: ogni membro li ha già.

## Come girano gli script del prof
- Cercano Dataset_V3.xlsx e i CSV nella cartella di lavoro e scrivono lì i risultati.
  Vanno eseguiti in una cartella dove stanno tutti insieme.
- Soluzione adottata: il master copia template/ in output/template_run/ e lancia lì gli script.
  Così template/ resta identica all'originale.
- Ordine:
  1. 00_setup.R
  2. 00_simulated_causality.R
  3. 01_prepare_data.R
  4. 02_frontiers.R (esercizio comune)
  5. 05_prepare_group_data.R
  6. 06_group_project.R, con GROUP <- 4L e RUN_FRONTIER <- TRUE assegnati PRIMA di source().
     06 legge questi valori dalla sessione, quindi non va modificato.
- 03 è un esercizio comune, 04 e 00_first_steps sono facoltativi.
- 00_run_all.R è superato (cerca le cartelle R/ e results/ e il pacchetto frontier): non usarlo.
- Controlli attesi:
  - teaching_panel: 244 righe (61 università × 2020–2023);
  - project_panel con ritardi: 183 righe (2021–2023);
  - con le quote disciplinari 2018 congelate: 60 università e 180 righe.
  Verifica che il teaching_panel rigenerato da 01 coincida con l'originale.
- Pacchetti: sfa (risultati verificati con la 1.2.0), readxl, sandwich, lmtest (li installa 00_setup.R).
  Tutti i membri devono usare la stessa versione di R e di sfa.
- FEAR (P. Wilson):
  - non è su CRAN: si installa dal file zip del corso con
    install.packages("<percorso>/FEAR-MSWin-11.zip", repos = NULL, type = "win.binary");
  - registra la versione installata;
  - le variabili vanno per righe (usa t());
  - ORIENTATION 2 = output, METRIC 2 = Farrell.

## Cosa fa 06 per il gruppo 4
- Outcome: log_publications.
- Esposizione: lag_log_teaching_load = log(students/faculty) dell'anno prima.
- Controlli: lag_log_faculty e lag_lcurrent.
- OLS con effetti fissi di università e anno, errori clusterizzati per università.
- Con RUN_FRONTIER: SFA NHN (esposizione nella frontiera) e NHN_Z (esposizione in log sigma_u).
- 06 NON stima la versione senza effetti fissi di università: la aggiungiamo noi in g4/.

## Dati: attenzioni
- Fonte: Dataset_V3.xlsx (fogli Metadata, Dataset 20-23, Scopus, Mapping, Dati InCites, PATIRIS).
  Blank e N.A. NON vanno ricodificati come zero.
- Le variabili chiave (docenti, assegnisti, studenti, laureati, studenti regolari, costi correnti)
  non hanno valori mancanti nel 2020–2023.
- ADJUNCT FACULTY: vale 0 nel 2023 per tutte le 61 università, e non ci sono zeri nel 2020–2022.
  È un'anomalia da verificare alla fonte, non evidenza di assenza di docenti a contratto.
  Non usare il 2023 per un'analisi di adjunct prima della verifica. Nessuna estensione
  con adjunct è stata stimata; 07 documenta soltanto il problema.
- Il teaching_panel contiene già:
  - faculty, fellows, students, graduates;
  - current_management (euro nominali);
  - publications, citations, cnci, wos_documents;
  - labor = faculty + fellows;
  - lcurrent = log(current_management/1e6).

## Decisioni confermate
| Decisione         | Principale                                    | Robustezza           |
|-------------------|-----------------------------------------------|----------------------|
| Intervento        | più studenti a docenti fissi                  | —                    |
| Frontiera DEA/FDH | unica 2021–2023 (183 oss.)                    | una per anno         |
| Costi             | deflazionati con indice ISTAT prezzi consumo  | nominali             |
| Input lavoro      | solo docenti (coerente con 06)                | docenti + assegnisti |

Nel report (da scrivere più avanti, su richiesta di Stefano del 23 novembre):
- le stime sono associazioni: il target è un aumento di studenti a capacità docente iniziale fissa;
- senza costi si considera il possibile effetto totale rispetto a questo intervento; con costi
  il possibile effetto a docenti e spesa fissi. Nessuno dei due è identificato causalmente;
- studenti, docenti e costi sono tutti in t-1: il loro ordine rispetto all'intervento non è noto.
  Il ruolo dei costi come mediatore è un'ipotesi, non un fatto stabilito dal ritardo annuale.

Esecuzione e script (confermati il 22 novembre 2026):
- Il kit gira in output/template_run/, ricreata a ogni esecuzione di run_all_g4.R.
- Prima di 01 il master cancella teaching_panel nella copia template_run/, così il confronto
  usa il file scritto da 01; template/ non viene toccata.
- Il teaching_panel rigenerato si confronta per contenuto (read.csv + all.equal), non per byte.
- Controlli righe in tre punti: 244 dopo 01, 183 (righe con ritardo) dopo 05,
  180 righe / 60 università nel campione effettivo di 06.
- Il master si ferma con un messaggio chiaro se R non è la 4.5.x.
- 09: scomposizione sugli anni dell'esposizione ritardata (2020–2022), sia grezza sia al netto
  di effetti anno e università; segnalare quote negative o > 1.
- 10: stesso campione di 06, stessi errori (vcovCL HC1, df G−1). Sette colonne, di cui sei principali:
  (solo anno) / (+ quote disciplinari 2018) / (+ effetti fissi università), ognuna senza e con
  lag_lcurrent. Lo script si ferma se la colonna effetti fissi università + anno + lag_lcurrent
  non riproduce esattamente il coefficiente di 06. La (7) ripete la (6) usando gli studenti regolari.
- 12 (Blocco 2, tronco comune, FEAR): frontiera pooled 2021–2023, 61 atenei, 183 righe (anche
  Siena Stranieri, esclusa in 06); input in t−1 docenti e costi correnti deflazionati (NIC, euro
  2020); output pubblicazioni Scopus in t. ORIENTATION 2, METRIC 2, variabili per righe (t()).
  Regola dello stimatore fissata prima: convessità non respinta → DEA (RTS 3 se CRS non respinto,
  altrimenti RTS 1); convessità respinta → FDH. Livello 5%. set.seed(20261122).
  Intervalli con dea.subsample / fdh.subsample e subsample.sizes = NULL (ottimizzazione automatica
  di Simar e Wilson 2011a, come raccomanda il FEAR-chapter).
- Outlier: analisi principale con tutte le osservazioni (si tolgono solo errori di dati verificati);
  la versione senza gli outlier segnalati da ap si fa sempre come robustezza in 13.
- 13 (robustezze della frontiera): frontiera per anno, costi nominali, docenti + assegnisti,
  senza outlier.

## Registro deviazioni (da aggiornare sempre)
1. Il kit del prof sta tutto insieme in template/, invece che in data/ + code/prof/,
   perché gli script cercano i file nella cartella in cui girano.
2. 00_run_all.R è copiato ma non usato: lo sostituisce run_all_g4.R.
3. Non sono stati copiati i resti delle esercitazioni (.RData, .Rhistory, FEAR_tutorial.RData,
   results_*.csv). comandi_tutorial.R è in docs/ solo come esempio di sintassi FEAR.
4. La frontiera DEA/FDH usa solo i docenti e i costi deflazionati, mentre 01 e 02 del prof
   usano labor = docenti + assegnisti e costi nominali. La SFA di 06 resta invariata.
5. I valori di adjunct 2023 (tutti 0) sono trattati come mancanti.
6. Nuova cartella data/ (22 novembre 2026) per i dati esterni al kit del prof: indice ISTAT NIC
   (prezzi al consumo, intera collettività, medie annue, base 2015=100) per deflazionare i costi
   correnti. Contiene il file ISTAT originale scaricato da un membro del gruppo e
   data/istat_nic_annual_2020_2022.csv con fonte, tabella, URL e data di accesso.
   Fonte: https://www.istat.it/tavole-di-dati/prezzi-al-consumo-dati/ (tavola "Indici NIC").
7. (23 novembre 2026) Rispetto al punto (d) dei Prossimi passi (ora nel diario), in g4/13 sotto RUN_SENSITIVITY
   resta solo il test.convexity bootstrap (NSPLIT 10, NREP 200): il test.rts bootstrap è stato
   tolto perché supera i 30 minuti anche con NREP 200. Per i rendimenti di scala resta
   test.rts.fast con NSPLIT 1000 (in 12 e in ogni versione di 13).
8. (23 novembre 2026, Codex) Aggiunta al master l'opzione CLI --presentation-only:
   dopo un'esecuzione completa riuscita permette di rigenerare 15 e 16 dagli output
   esistenti per correzioni grafiche. Nessuna nuova stima; sessionInfo.txt resta quello
   dell'esecuzione completa. Per la consegna resta obbligatoria l'esecuzione completa
   predefinita con RUN_SENSITIVITY = TRUE. Si continua a lanciare solo run_all_g4.R.
9. (23 novembre 2026, revisione Codex) Il DAG di 16 esplicita ipotesi non dettagliate nel
   vecchio piano, che indicava solo reputazione -> studenti/ricerca. Aggiunti composizione,
   capacità iniziale, rapporto deterministico e possibile canale studenti -> costi, con
   ordine temporale dichiarato non verificato. Nessuna nuova variabile stimata o modello.
   Corrette formulazioni interpretative nella nota alla prof e nella nota R7 di 13:
   Scopus include anche libri; la mancata copertura dell'estratto non è dimostrata.
   I risultati numerici e le specificazioni di 07–13 non sono stati modificati.

## Piano: lavoro comune (prima della scelta tra idea A e B)
Blocco 1: dati e benchmark del template
1. Setup e simulazione randomizzata (da riprodurre nel report).
2. 01 e 05 invariati, con i controlli sul numero di righe.
3. Descrittive del rapporto studenti/docenti: variazione tra università e nel tempo
   (figura su variazione e timing + tabella del campione).
4. Scomposizione: Δlog(S/F) = Δlog S − Δlog F, con la quota di variazione dovuta
   agli studenti e quella dovuta ai docenti.
5. 06 con GROUP = 4, più un nostro script che aggiunge:
   - la versione con e senza effetti fissi di università;
   - la versione senza e con controllo sui costi (effetto totale vs a risorse fisse).
6. Testi:
   - DAG (reputazione → studenti e ricerca);
   - sezione dati (metadata, unità, mancanti, date);
   - collegamento con la systematic review;
   - richiesta di dati.

Blocco 2: frontiera "solo ricerca" (tronco comune di A e B)
7. Dataset della frontiera: input docenti + costi correnti deflazionati; output pubblicazioni Scopus.
8. Outlier con ap e ap.plot.
9. test.convexity e test.rts, per scegliere tra DEA e FDH e i rendimenti di scala.
10. Punteggi di efficienza orientati all'output, con intervalli via dea.subsample (seed fissato).
11. Grafico dell'efficienza "solo ricerca" contro il carico didattico.
12. SFA di 06 (NHN e NHN_Z) come gemelli parametrici.

Poi, scelta ancora da fare con il gruppo:
- Idea A, didattica come output:
  - frontiera congiunta con output pubblicazioni + laureati;
  - guardare CHI guadagna efficienza: a campione e ipotesi invariati, aggiungere un output
    può alzare meccanicamente 1/Farrell (ridurre il fattore Farrell), non necessariamente per tutti.
- Idea B, didattica come condizione Z:
  - efficienza condizionata calcolata con dea/fdh/orderm limitando il riferimento (XREF/YREF)
    agli atenei con Z vicino (correzione del 23 novembre 2026: in FEAR cdea/cfdh NON sono
    stimatori condizionati ma DEA/FDH con rendimenti costanti; FEAR 5.0 non ha una funzione
    pronta per l'efficienza condizionata);
  - test.sep.cont.fast e grafico del rapporto condizionata/non condizionata;
  - versione snella descritta nel punto (f): nessuna griglia order-m, cross-validation o
    test.means aggiuntivo. Stimatore e interpretazione da verificare dopo la scelta del campione.

Da ricordare nel report:
- l'efficienza condizionata non misura la qualità complessiva dell'università;
- le collocazioni SFA sono specificazioni mantenute, non una scomposizione identificata
  di un effetto causale.

## Standard del report (booklet)
Report breve in inglese + script che gira da una sessione R pulita, con:
- un DAG;
- una figura su variazione o timing;
- una tabella del campione;
- una tabella dei risultati.

Sei punti da coprire:
1. intervento, confronto, outcome, orizzonte, popolazione; effetto totale vs a risorse fisse;
2. metadata, unità, mancanti, date;
3. DAG;
4. benchmark trasparente con diagnostiche della frontiera;
5. variazione che identifica l'effetto, oppure una richiesta di dati;
6. riproduzione della simulazione randomizzata.
La significatività statistica non è richiesta.

Benchmark dell'anno scorso (progetto di Rita), errori da evitare:
- punteggi normalizzati min-max usati come input e output;
- "test" ridotti a un confronto tra DEA e FDH.
Noi usiamo risorse vere e test formali.

## Stato del lavoro (aggiornato al 24 novembre 2026)
- Ambiente: R 4.5.3, sfa 1.2.0, FEAR 5.0.
- Fatto: run_all_g4.R, g4/07–13, 15, 16; data/ (deflatore NIC); report/note_for_professor.md.
  g4/14 non creato: attende la conferma della prof sull'idea B.
- Ultima esecuzione completa (RUN_SENSITIVITY = TRUE): 23 novembre 2026, exit code 0.
- 24 novembre 2026: verifica dopo Codex (MD5 di template/ e docs/ intatti) e master con
  RUN_SENSITIVITY = FALSE, tutti gli 8 controlli superati. Quindi sessionInfo.txt non viene
  da un'esecuzione completa.
- Report e slide non scritti (rinviati da Stefano). Invio della nota alla prof e risposta
  non documentati.
- Risultati chiave (dettagli in report/project_diary.md):
  - OLS di 06 = colonna (6) di 10: 0.011 (SE cluster 0.350): stima imprecisa, non "zero";
    variazione dell'esposizione al netto di effetti ateneo e anno: 2.6%;
  - NHN con il carico nella frontiera: deriva verso sigma_v → 0, è una diagnostica, non un
    risultato. NHN_Z: 0.49 in log sigma_u, SE cluster 0.54;
  - frontiera pooled: DEA CRS, 3 efficienti su 183; senza i 3 atenei umanistici (R7) → FDH.
- Limiti da dichiarare nel report: sezione "Verifica di completezza" del diario.

## Decisioni aperte (prof e gruppo)
1. Solo tronco comune, oppure anche idea A o B?
2. Tre atenei umanistici nel campione principale (scelta attuale) o esclusi dopo una verifica
   di copertura? La scelta cambia lo stimatore da DEA CRS a FDH.
3. Lunghezza del report, durata e formato della presentazione del 18 dicembre.
4. Gruppo: cartella condivisa (OneDrive) e divisione dei compiti.

## Prossimi passi
1. Inviare la nota alla prof e registrare la risposta.
2. (f) g4/14_conditional_efficiency.R SOLO dopo la conferma dell'idea B, versione snella:
   una bandwidth dichiarata con sensibilità h×0.5 e h×2; grafico del rapporto condizionata/non
   condizionata contro Z; test.sep.cont.fast. ESTIMATOR = 3 (DEA CRS) vale per il campione
   attuale: se la scelta finale è FDH, rivedere stimatore e condizioni del test. Niente
   cross-validation np né griglia order-m. Serve la frase esatta di Daraio & Simar (2007)
   sull'interpretazione del rapporto nell'orientamento output (non è in docs/).
3. Report e slide, quando Stefano lo chiede: integrare systematic review, simulazione,
   richiesta dati e limiti.
4. Facoltativo: theory_QA.md per l'orale.
5. Prima della consegna: esecuzione completa di run_all_g4.R con RUN_SENSITIVITY <- TRUE da
   sessione R pulita, per rigenerare tutti gli output e sessionInfo.txt
   (--presentation-only non basta).

## Diario del percorso
Spostato in report/project_diary.md (24 novembre 2026). Per ogni passo: data, cosa, perché,
risultato, problemi. Le deviazioni vanno ANCHE nel Registro deviazioni.

## Divisione dei compiti
Da definire nel gruppo (esempio: un membro il Blocco 1, un altro il Blocco 2).

## Primo compito
1. Leggi questo file, il booklet in docs/ (card del progetto 4 e standard comune) e gli script
   in template/, senza modificare nulla.
2. Controlla l'ambiente: versione di R, pacchetti sfa, readxl, sandwich, lmtest, e FEAR installato.
3. Proponi, senza creare nulla, lo schema dello script master run_all_g4.R e dei primi
   script in g4/ per il Blocco 1. Poi aspetta la conferma.
