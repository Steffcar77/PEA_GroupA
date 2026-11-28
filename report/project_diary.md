# Diario del percorso – PEA Group A, progetto 4

Spostato da CLAUDE.md il 24 novembre 2026, senza modifiche al testo delle voci precedenti.
Contiene: il diario dei passi, le note sui risultati, i passi completati (a0)–(e) e la verifica
di completezza del 23 novembre 2026. Regole, decisioni, Registro deviazioni, Stato e Prossimi
passi restano in CLAUDE.md.

## Diario
22 novembre 2026 – Primo compito (Stefano)
- Cosa: lettura di CLAUDE.md, booklet (card 4 e standard comune) e script del prof; controllo
  ambiente; creati run_all_g4.R e g4/07_sample_checks.R; README aggiornato.
- Perché: avere un'esecuzione riproducibile del kit senza toccare template/, con controlli
  automatici, e documentare il campione (punto 2 dello standard).
- Risultato: R 4.5.3, sfa 1.2.0, FEAR 5.0. Controlli superati: teaching_panel rigenerato
  identico per contenuto; 244 / 183 / 180 righe; 60 università (esclusa Siena Stranieri).
  ADJUNCT 2023 tutto 0 (61/61), nessuno zero nel 2020–2022. OLS di 06: 0.011 (SE 0.350).
- Problemi: 01 sovrascrive teaching_panel nella cartella in cui gira → il kit gira in
  output/template_run/ e il master cancella il CSV copiato prima di 01. SFA NHN con Hessiana
  singolare: l'ipotesi iniziale (sigma_u → 0) è stata smentita dagli output (sigma_u = 0.359).

22 novembre 2026 – a0: lettura degli output già prodotti
- Cosa: coefficiente NHN_Z, simulazione randomizzata, scatter di 06.
- Perché: capire cosa dicono gli output del prof prima di aggiungere analisi.
- Risultato: NHN_Z +0.416 nell'equazione di sigma_u (tra atenei, SE ottimisti); la simulazione
  ritrova i valori veri (A: 0.108 / 0.118 vs 0.12; B: 0.110 vs 0.118, scala −0.353 vs −0.4);
  scatter senza relazione lineare chiara, variazione quasi tutta tra atenei.
- Problemi: nessuno.

22 novembre 2026 – g4/08, 09, 10 (Blocco 1)
- Cosa: 08 SD di log(S/F) tra e dentro gli atenei + figura; 09 scomposizione
  Δlog(S/F) = Δlog S − Δlog F (grezza, al netto degli effetti anno, livelli al netto di
  effetti ateneo e anno) + conteggio dei segni + figura; 10 sei colonne OLS.
- Perché: punto 5 dello standard (variazione che identifica l'effetto) e richieste della card
  (scomposizione del rapporto, risultati con e senza effetti fissi di università).
- Risultato:
  - 08: SD totale 0.229, tra atenei 0.223 (93.8% della somma dei quadrati), dentro 0.057 (6.2%),
    al netto di effetti ateneo e anno 0.037 (2.6%). Coincidono con l'esposizione di 06.
    Il rapporto medio scende ogni anno (S/F mediano 29.4 → 29.1 → 27.5).
  - 09: quota studenti 0.46 (2020–21), 0.49 (2021–22), 0.42 (insieme), 0.48 al netto degli
    effetti anno, 0.58 nei livelli al netto di effetti ateneo e anno. Nessuna quota fuori [0, 1].
    Segni su 120 variazioni: studenti giù e docenti su 56, entrambi su 44, entrambi giù 13,
    studenti su e docenti giù 4, docenti invariati 3. Il rapporto sale solo 17 volte su 120:
    nel periodo il rapporto cala soprattutto perché crescono i docenti.
  - 10: log(S/F) = −0.366 (0.479) e −0.493 (0.468) con soli effetti anno; 0.167 (0.090) e
    0.142 (0.087) con quote 2018; 0.005 (0.345) e 0.011 (0.350) con effetti fissi di ateneo.
    La colonna (6) riproduce 06 (tolleranza 1e-8). Con effetti fissi l'IC95 è circa [−0.69, 0.71].
- Problemi: in 08 la quota di varianza della riga "Total" usciva 19.11 (errore di formula),
  corretto; in fig_09 un'etichetta copriva un punto, spostata.
- Correzioni successive: in 08 le quote di varianza usano le somme dei quadrati
  (SS tra 93.8% + SS dentro 6.2% = 100%; al netto di effetti ateneo e anno 2.6%, parte del dentro);
  le SD restano con n−1. In 10 aggiunto l'R² within per le colonne (5)–(6).
- Idea da discutere (NON è una decisione): nel 2020–22 i docenti crescono anche per i piani
  straordinari di assunzione (variabile EXTRAORDINARY FUNDING PLANS). Potrebbe essere un
  intervento di tipo "teaching relief" da proporre nella richiesta di dati, con un limite:
  i criteri di riparto dei piani includono la VQR, quindi l'assegnazione non è indipendente
  dalla ricerca.

22 novembre 2026 – g4/11: sensibilità della NHN di 06 ai valori iniziali (punto b)
- Cosa: NHN di 06 (carico nella frontiera) da 32 partenze (20 griglia λ×σ da OLS, 10 perturbazioni
  ±10% della soluzione di 06 con set.seed(20261122), 1 dalla NHN_Z, 1 dalla soluzione di 06);
  profilo della logLik su λ fisso (1 → 1e5) con una logLik NHN nostra, verificata contro sfa;
  errori clusterizzati per ateneo (vcovCL HC1, df 59) per la NHN migliore e per la NHN_Z.
  Interruttore RUN_SENSITIVITY in run_all_g4.R (TRUE di default, obbligatorio per la consegna).
- Perché: punto 4 dello standard (sensibilità ai valori iniziali, confini della varianza).
- Risultato:
  - la logLik sale sempre verso σ_v → 0: profilo 37.56 (λ = 1) → 41.29 (λ = 9.37) → 47.48
    (λ = 1000) → 47.85 (λ = 1e5). Nessun massimo interno affidabile trovato nelle prove:
    l'ipotesi dei "due massimi locali" NON è confermata; il profilo esplorato tende al
    confine σ_v = 0 (frontiera deterministica), senza provare un risultato globale;
  - 23 partenze su 32 finiscono al confine (λ > 100 o σ_v < 0.001; logLik 45.9–47.85, quasi tutte
    con codice 52 di L-BFGS-B, σ_v fino a 1e-8), anche quella che parte esattamente dalla soluzione
    di 06; le altre 9 si fermano a logLik 40.9–43.7, sotto il profilo esplorato;
  - coefficiente del carico nella frontiera: da −0.04 a −0.15 nelle soluzioni interne,
    da −0.162 a −0.167 al confine: dipende dal punto in cui si ferma l'ottimizzatore;
  - errori clusterizzati della NHN migliore: non calcolabili (Hessiana non invertibile);
  - NHN_Z rifatta dai coefficienti di 06: logLik 44.856 (06: 44.742), carico in log σ_u = 0.491,
    SE del modello 0.175, SE clusterizzato 0.537, t = 0.91, p = 0.36.
- La soluzione al confine NON va adottata come risultato: è una diagnostica. La NHN con il carico
  nella frontiera non è identificata bene in questo campione.
- Con gli errori clusterizzati la NHN_Z perde la significatività (SE da 0.173 a 0.537).
  OLS e SFA insieme non mostrano un'associazione robusta, coerentemente con il 2.6% di
  variazione dentro gli atenei.
- La NHN_Z rifatta con keep_objective dà 0.491 invece di 0.416: anche il fit di 06 era fermo prima
  del massimo. Nel report si riporta 06 com'è e la nostra stima come sensibilità.
- Da verificare nel Blocco 2: se la deriva della NHN verso σ_v → 0 dipende dagli atenei molto piccoli.
- Problemi: il profilo con il solo BFGS a partenza calda si bloccava (logLik sotto i fit di sfm);
  risolto con tre partenze per ogni λ e rifinitura con nlminb. Etichetta della figura spostata.

23 novembre 2026 – g4/12: frontiera "solo ricerca" con FEAR (Blocco 2, tronco comune)
- Cosa: deflatore NIC da data/C16.pdf (Tavola 16.10: 2020 = 102.7, 2021 = 104.7, 2022 = 113.2,
  verificati anche con le variazioni −0.2, +1.9, +8.1); frontiera pooled 2021–2023, 61 atenei,
  183 righe (anche Siena Stranieri, esclusa in 06); ap (NDEL 12), test .fast, DEA/FDH con
  intervalli per subsampling, figura efficienza contro carico. 12 è sempre nel master (circa 3 min).
- Perché: punti 7–11 del piano; punto 4 dello standard (benchmark con diagnostiche).
- Prove dei tempi (costi nominali, solo prova): dea.subsample 44 s; ap NDEL 12 11 s;
  test.convexity bootstrap con NSPLIT 10 e NREP 200 46 s; test.convexity.fast NSPLIT 1000 24 s;
  test.rts.fast NSPLIT 1000 85 s. La prima prova si era bloccata sui test bootstrap con NREP 1000.
  Scelta: versioni .fast con NSPLIT = 1000, perché il FEAR-chapter dice che servono almeno 1000
  divisioni del campione per togliere l'ambiguità dovuta alla divisione casuale.
- Errore di lettura corretto: le versioni .fast NON danno p-value ma indicatori rifiuto (1) /
  non rifiuto (0) ai livelli 10%, 5%, 1% (disuguaglianza di Markov; codice della funzione e
  FEAR-chapter). Avevo letto "0 0 0" come p = 0 (convessità respinta): è il contrario.
  Con NSPLIT = 10 (bootstrap) la convessità non era respinta (p = 0.875 e 0.105, costi nominali)
  e con NSPLIT = 1000 (.fast) nemmeno: i due risultati concordano. Nelle tabelle si riporta
  "rejected / not rejected" ai tre livelli.
- Dimensione dei sottocampioni: subsample.sizes = NULL, cioè FEAR sceglie m per ogni osservazione
  con il metodo di Simar e Wilson (2011a), come raccomanda il FEAR-chapter (un m non ottimizzato
  dà intervalli con copertura scarsa). Candidati m = 2–37 (36 valori); m scelto al 95%:
  mediana 25, da 2 a 35.
- Risultato (costi deflazionati):
  - test: convessità non respinta a 10/5/1%; CRS non respinto a 10/5/1% → regola fissata:
    DEA con rendimenti costanti (RTS = 3). Non respingere non prova che l'ipotesi sia vera;
  - efficienti (punteggio 1): 3 su 183 (1.6%): Catanzaro 2021, Molise 2021, Bari Politecnico 2022;
    per anno 2 / 1 / 0;
  - Farrell output mediano 1.254 (2021), 1.310 (2022), 1.404 (2023); medio 1.87 / 2.01 / 2.18
    (medie tirate dai piccoli atenei umanistici: Napoli L'Orientale fino a 27, Siena Stranieri
    12–15, Perugia Stranieri circa 5). Ampiezza mediana dell'IC95: 0.07;
  - il peggioramento 2021 → 2023 è coerente con il possibile problema di indicizzazione Scopus
    (da controllare con la frontiera per anno in 13);
  - ap segnala 13 atenei-anno, quasi tutti grandi (Verona è di media dimensione: 838 docenti;
    correzione del 28 novembre 2026): Roma La Sapienza (3 anni), Napoli Federico II (3),
    Bologna 2022–23, Milano 2021 e 2023, Padova 2023, Torino 2023, Verona 2023. Nessuno è tra i
    piccoli dello scatter; i loro residui NHN di 06 sono nella media (percentili 0.22–0.91).
    Quindi ap non spiega la deriva della NHN con i piccoli atenei: quel punto resta aperto;
  - figura: relazione a campana (lowess, solo descrittiva), come lo scatter grezzo di 06;
    efficienza più alta a carichi intermedi (log S/F circa 3.2–3.6), più bassa agli estremi,
    dove stanno pochi atenei, spesso piccoli e specializzati. È un confronto tra atenei, non un
    effetto del carico.
- Problemi: residuals() di sfa non funziona su un fit salvato → residuo NHN calcolato come
  y − x'b sul campione di 06. Nella figura le etichette degli outlier si sovrapponevano:
  sostituite da una croce (nomi in table_12_outliers.csv).

23 novembre 2026 – g4/13: robustezze della frontiera (punto d) e nota per la prof (punto e)
- Cosa: 7 versioni, ognuna con la regola dello stimatore di 12 (test .fast, NSPLIT 1000):
  R1–R3 frontiera per anno; R4 costi nominali; R5 docenti + assegnisti; R6 senza gli outlier di ap;
  R7 senza Napoli L'Orientale, Siena Stranieri, Perugia Stranieri. Per ognuna: stimatore, Farrell
  mediano, quota di efficienti, Spearman con 12. Figura riassuntiva a punti (anche per le slide).
  Sotto RUN_SENSITIVITY solo il test.convexity bootstrap (deviazione n. 7).
- Perché: robustezze previste nelle decisioni confermate; sensibilità al riferimento annuale
  e alla possibile copertura degli atenei umanistici. Questi confronti non verificano da soli
  un ritardo di indicizzazione o un errore di misura (precisazione della revisione del 23 novembre).
- Risultato:
  - R1–R6: sempre DEA CRS; Farrell mediano 1.18–1.33; Spearman 0.91–1.00. Con la frontiera
    per anno il peggioramento del 2023 si attenua (mediane 1.18, 1.29, 1.28 contro 1.25, 1.31,
    1.40 della frontiera pooled). Gli assegnisti (R5) spostano un po' la classifica (0.906);
  - R6: si tolgono 13 osservazioni ateneo-anno di 7 atenei; Roma La Sapienza e Napoli
    Federico II sono segnalati in tutti e 3 gli anni, quindi escono del tutto (59 atenei);
  - R7: senza i 3 atenei umanistici la convessità è RESPINTA → FDH, 72 efficienti su 174 (41%),
    Farrell mediano 1.03, Spearman 0.805. Quei 3 atenei condizionano la scelta dello stimatore:
    va scritto nel report.
  - test.convexity bootstrap (NSPLIT 10, NREP 200, dati principali): p = 0.625 (statistica media)
    e 0.685 (Kolmogorov-Smirnov): convessità non respinta, coerente con il test .fast.
- Problemi: il test.rts bootstrap (NSPLIT 10, NREP 200) girava ancora dopo circa 30 minuti →
  tolto (deviazione n. 7). In R6 la nota iniziale "non atenei interi" era imprecisa → resa
  automatica ed esatta.
- Nota per la prof: report/note_for_professor.md (struttura approvata, 7 punti, con il limite di
  misura di Scopus e quattro domande nella versione aggiornata: idea B, scelta di Z,
  formato/durata e trattamento dei tre atenei umanistici).

23 novembre 2026 – colonna (7) in 10 e correzione del piano per l'idea B
- Cosa: in 10 aggiunta la colonna (7) = colonna (6) con log(studenti regolari / docenti) in t−1
  ("NUMBER OF REGULAR STUDENTS" dall'Excel, foglio Dataset 20-23). Il controllo di riproduzione
  di 06 resta sulla colonna (6) e passa. Nel piano: cdea/cfdh → efficienza condizionata via
  XREF/YREF (vedi idea B).
- Risultato: colonna (7) = 0.118 (SE 0.270; IC95 −0.42, 0.66), contro 0.011 (0.350) della (6);
  R² within 0.087. Stessa conclusione: stima imprecisa, nessuna associazione robusta.
- Note: verificato nell'help di FEAR che ESTIMATOR vale 1 = FDH, 2 = DEA VRS, 3 = DEA CRS
  (test.means, test.sep.cont.fast). Il pacchetto np 0.80.2 è stato installato sul PC di Stefano
  ma NON è usato dal progetto (14 senza cross-validation): non è una dipendenza.
  14 non creato: si aspetta la conferma della prof sull'idea B.

23 novembre 2026 – g4/15: figure e tabelle per report e presentazione (Codex)
- Cosa: dopo l'ok di Stefano a procedere, creato g4/15_final_figures_tables.R e aggiunto al
  master dopo 13. Sei tabelle CSV e un indice HTML leggibile in output/tables/; quattro
  figure 16:9, ciascuna in PNG e PDF vettoriale, in output/figures/ (prefisso fig_15_).
- Perché: rendere presentabili gli output già disponibili, senza nuove stime né pacchetti.
- Contenuto: campione e descrittive di 07, confronto campioni di 12, tutte le sette colonne
  OLS di 10, robustezze di 13; figure sulla variazione di 08, efficienza-carico di 12,
  robustezze di 13 e matrice efficienza-volume. Ogni punto della matrice è un ateneo:
  mediana di 1/Farrell e mediana delle pubblicazioni sui tre anni; soglie alle mediane
  dei 61 atenei, asse pubblicazioni logaritmico, etichette su pochi estremi deterministici.
- Interpretazione: confronti descrittivi; nessuna misura di qualità complessiva o effetto
  causale. Indicati il passaggio DEA/FDH in R7 e i limiti degli intervalli della frontiera
  pooled. I punteggi di frontiere diverse non sono direttamente confrontabili in livello.
- Verifica: master completo con RUN_SENSITIVITY = TRUE concluso da Rscript --vanilla,
  exit code 0 e tutti i controlli superati; risultati delle robustezze confermati.
  Resta l'avviso previsto per NREP = 200 nel test bootstrap (deviazione 7).
  Primo controllo visivo: spostata la legenda dello scatter, aumentato il margine
  sinistro della matrice a quadranti e rese esplicite le tacche dell'asse delle robustezze.
  Rigenerazione grafica tramite il master in modalita --presentation-only (deviazione 8).
  Esito finale: rigenerazione riuscita (exit code 0), quattro figure controllate a vista,
  PNG 2400 x 1350 (16:9) e collegamenti locali dell'indice HTML verificati.
- Problema di ambiente: il primo avvio ereditava LANG/LC_ALL/LC_CTYPE = C.UTF-8,
  non accettato da R su Windows; il confronto dei panel si fermava su quattro nomi.
  Riavviato il master eliminando queste variabili solo dalla shell di esecuzione:
  il controllo dei panel passa, senza modificare dati o controlli. Nota aggiunta al README.
- Prossimi passi di quella sessione: DAG e report; 14 solo dopo conferma della prof.
  Aggiornamento successivo: DAG completato in 16, report rinviato da Stefano.

23 novembre 2026 – g4/16 e revisione complessiva (Codex)
- Richiesta: creare il DAG, rinviare il report e chiarire se il lavoro è finito.
- Creato 16 in base R, senza pacchetti nuovi: otto nodi, dodici frecce, controllo di
  aciclicità, PNG/PDF 16:9 e tre CSV con nodi, frecce e limiti. Inserito nel master e
  nell'indice HTML di 15; --presentation-only include ora anche 16.
- Riletti booklet (pp. 1, 5 e 12), metadata Excel, note, script e tabelle; consultata
  la policy ufficiale Elsevier sulla copertura Scopus. Correzioni nella deviazione 9.
- Corretti anche commenti di 09/10: la variazione al netto dei soli effetti fissi precede
  gli altri controlli, e totale/risorse fisse sono target ipotetici. Nessun calcolo cambiato.
- Non creati report_draft.md né theory_QA.md. Nessuna nuova stima o estensione A/B.
- Verifica: modalità presentazione riuscita, DAG controllato a vista; master completo
  con RUN_SENSITIVITY = TRUE concluso da Rscript --vanilla, exit code 0. Otto controlli
  precedenti superati, 15 e 16 rigenerati, robustezze numericamente confermate. Resta
  l'avviso già documentato per NREP = 200. Verificati collegamenti HTML, formato 16:9,
  otto nodi/dodici frecce, identità degli script del kit con le copie eseguite e assenza di .RData.

24 novembre 2026 – Verifica dopo il lavoro di Codex e riordino della documentazione (Stefano)
- Cosa: verifica in sola lettura. MD5 dei 21 file di template/ e docs/: tutti identici agli
  originali. Riletti 15 e 16: usano solo output esistenti, percorsi relativi, figure in scala
  di grigi distinte da simboli e linee, numeri coerenti con 08, 10 e 13; DAG con 8 nodi e
  12 frecce come in CLAUDE.md. Nessun .RData, nessun percorso assoluto in g4/ e nel master,
  README coerente con g4_scripts, Registro deviazioni 1–9 completo.
- Esecuzione: master da sessione pulita (Rscript --vanilla, senza LANG/LC_ALL/LC_CTYPE), con
  RUN_SENSITIVITY = FALSE impostato solo in memoria da un wrapper fuori dal progetto
  (run_all_g4.R non modificato). Exit code 0, tutti gli 8 controlli superati, circa 518 s.
  Tabelle di 13 rigenerate identiche byte per byte alle precedenti.
- Correzioni approvate da Stefano:
  - nota alla prof: copertura probabilmente limitata delle monografie umanistiche in italiano
    (da verificare); attenuazione del 2023 con frontiere annuali coerente con uno shift comune,
    non dimostrato; "barely changes" sostituito da "less than their standard errors"
    (colonne 1→2: da −0.37 a −0.49, SE circa 0.47);
  - README: .claude/ esclusa dalla consegna; aggiunto questo file;
  - diario, note sui risultati e verifica di completezza spostati da CLAUDE.md a questo file;
    CLAUDE.md abbreviato.
- Problemi: output/sessionInfo.txt riflette ora un'esecuzione con RUN_SENSITIVITY = FALSE.
  Prima della consegna serve l'esecuzione completa con RUN_SENSITIVITY = TRUE.

28 novembre 2026 – Correzioni dell'audit del 27 novembre (Stefano)
- Cosa: correzioni proposte in report/audit_2026-11-27.md, approvate da Stefano (deviazioni 10–13).
  - 07: copia del gruppo del project panel con adjunct 2023 = NA
    (output/g4_project_panel_2020_2023.csv; cambiano solo 61 celle); il flusso del campione
    la usa. Nuova table_07_data_notes.csv: adjunct 2023; Perugia Stranieri docenti 55 → 69
    (+25.5%, la variazione annua più grande del panel); Molise con la quota di gestione
    corrente più bassa nel 2020 (17.9%), componenti dei costi coerenti, salto 2022 +38.1%.
  - 09: "più studenti a docenti fermi" = dlog S > 0 e |dlog F| ≤ 0.01: 4 casi su 120
    (esatti 1, entro ±2% 9). Nell'audit i "4 casi" erano studenti su e docenti giù:
    non coincidono con questa definizione (correzione della nota del 27/11).
  - 11: NHN_Z con ri-stima (0.491, logLik 44.856, SE cluster 0.537, cite = TRUE) e soluzione
    del kit (0.416, logLik 44.742, SE cluster 0.538 solo per confronto: scores fino a |1.56|);
    likelihood piatta nella direzione del carico. Elasticità NHN_Z 0.717 (0.094) e 0.302
    (0.072), somma 1.020 (0.026). Le elasticità 0.70 / 0.32 della NHN non identificata non si
    citano (regola in CLAUDE.md). Note di identificazione: asimmetria OLS +0.150, profilo
    monotono, 23 partenze su 32 al confine, matrice d'informazione numericamente singolare.
  - 12: gap_to_next = ratio[2,i] − ratio[1,i]; stacchi maggiori a 1 (0.026) e 4 (0.024)
    eliminazioni. 45 intervalli su 183 con m < 10 (16 / 12 / 17 per anno, 29 atenei; in tutti e
    tre gli anni Camerino, Cassino, Catanzaro, Sannio, Urbino): non concentrati in un anno.
    Nota sul perché l'intervallo sta sopra la stima. Colonna estimator_short letta da 13 e 15.
  - 13: bootstrap della convessità con NREP 1000: p = 0.641 e 0.648 (prima 0.625 e 0.685 con
    NREP 200); nessun avviso FEAR. La regola dello stimatore resta sui test .fast.
  - 15: legenda dello scatter in alto, titolo e sottotitolo dei quadranti staccati, piè di
    pagina sugli intervalli; "the main frontier", "The frontier sample".
  - Documentazione: CLAUDE.md, README, nota alla prof (updated 28 November 2026).
- Perché: dubbi e problemi dell'audit (punti 5, 7, 9, 12, 13, 16, 17, 21, 22 e robustezza del codice).
- Verifica: esecuzione completa da sessione pulita (Rscript --vanilla, senza LANG/LC_ALL/LC_CTYPE,
  RUN_SENSITIVITY = TRUE), exit code 0, 8 controlli superati. Confronto con gli output
  precedenti: campione, coefficiente di 06, colonne OLS, test .fast, punteggi, intervalli,
  mediane e robustezze identici; cambiano solo le tabelle e colonne nuove, le note, i p-value
  del bootstrap e le figure di 12 e 15. Fit SFA di 06 identici (cambia solo il tempo di calcolo).
- Problemi: nessuno. Variabile phd non toccata (passo separato).

## Note sui risultati (dagli output del 22 novembre 2026)
- Interpretazione del coefficiente (10 e 06): con lag_log_faculty sempre presente, il coefficiente
  di log(S/F) coincide con quello di log S a docenti fissi, cioè l'intervento scelto ("più studenti
  a docenti fissi"). I target senza/con costi sono rispettivamente totale a docenti fissi
  e a docenti/spesa fissi, solo sotto ipotesi identificative non dimostrate. Il canale
  più studenti → più tasse → più spesa resta un'ipotesi temporale da verificare.
- Campione di 06: 180 righe, 60 università; esclusa Siena Stranieri (mancano le quote 2018).
- Coefficiente OLS di 06: 0.011 (SE cluster 0.350, IC95 circa [-0.69, 0.71], df = 59).
  Non va descritto come "zero" ma come stima molto imprecisa.
- Variazione dell'esposizione: SD totale 0.229, SD al netto di effetti università e anno 0.037
  (calcolate con n-1). Da mostrare in 08/09: è il punto 5 dello standard.
- SFA NHN (esposizione nella frontiera): Hessiana non definita positiva (errori standard 0 / Inf).
  L'ipotesi "wrong skewness con sigma_u vicino a zero" NON è confermata:
  - sigma_u = 0.359, sigma_v = 0.038, lambda = 9.37;
  - logLik NHN = 41.15 contro logLik OLS (confine sigma_u = 0) = 37.11;
  - eppure l'asimmetria dei residui OLS della stessa specificazione è positiva (+0.149).
  Quindi la soluzione è lontana dal confine sigma_u = 0, ma la verosimiglianza è piatta,
  con sigma_v piccolo e lambda grande. Da riportare tra le diagnostiche (punto 4);
  da verificare la sensibilità ai valori iniziali. Non preferire NHN_Z solo perché converge
  senza avvisi.
  Verificato con g4/11: la logLik della NHN cresce sempre verso σ_v → 0 (frontiera deterministica,
  valore circa 47.85); non trovato un massimo interno affidabile nelle prove effettuate;
  la soluzione di 06 è un punto della salita esplorata. La NHN con il
  carico nella frontiera non è identificata bene: la soluzione al confine è una diagnostica,
  non un risultato.
  Un sigma_u grande può includere differenze stabili tra atenei non catturate dalle quote 2018
  (problema dell'eterogeneità nei panel). Possibile estensione: 04_panel_extension.R (TRE/Mundlak).
  Gli errori standard della SFA sono basati sul modello, non clusterizzati per ateneo
  (commento del prof in 06): anche quelli di NHN_Z vanno letti come ottimisti.

## Passi completati e decisioni di gruppo (fino al 23 novembre 2026)
Da decidere con il gruppo:
- priorità: cartella condivisa su OneDrive, così si lavora su un'unica versione;
- divisione dei compiti;
- idea A o B (o entrambe in sequenza).
Prossimi passi, in ordine:
(a0) FATTO (22 novembre 2026): letti gli output già prodotti.
     - NHN_Z: carico nell'equazione di log sigma_u = +0.416 (SE del modello 0.173, non cluster).
       Confronto soprattutto tra atenei (niente effetti fissi); non concorda con OLS EF (+0.011)
       né con NHN frontiera (−0.084): dipende da dove il modello colloca il carico.
     - Simulazione: OLS e SFA ritrovano i valori veri. A (frontiera, vero 0.12): OLS 0.108,
       NHN 0.118. B (inefficienza, scala vera −0.4, effetto medio vero 0.118): OLS 0.110,
       NHN_Z −0.353. L'OLS trova l'effetto medio ma non distingue il canale; la SFA lo distingue
       solo perché il modello è quello vero e l'assegnazione è casuale.
     - Scatter grezzo: nessuna relazione lineare chiara (forma a campana); punti a gruppi di tre
       quasi fermi (stesso ateneo nei 3 anni); alcuni atenei molto piccoli (log pubblicazioni
       circa 3.5–4) da controllare come outlier nel Blocco 2.
(a) FATTO (22 novembre 2026): g4/08, 09, 10 e loro aggiunta a g4_scripts in run_all_g4.R;
(b) FATTO (22 novembre 2026): g4/11_nhn_start_sensitivity.R (vedi "Diario del percorso");
(c) FATTO (23 novembre 2026): tronco comune del Blocco 2, g4/12 (vedi "Diario del percorso").
(d) FATTO (23 novembre 2026), con la deviazione n. 7. g4/13 robustezze della frontiera: frontiera per anno; costi nominali; docenti + assegnisti;
    senza gli outlier di ap; senza i 3 atenei con output Scopus quasi nullo (Napoli L'Orientale,
    Siena Stranieri, Perugia Stranieri: possibile problema di misura da verificare, perché
    Scopus include anche libri e monografie); solo test.convexity NON .fast con NREP 200
    sotto RUN_SENSITIVITY. test.rts resta .fast (deviazione 7).
(e) FATTO (23 novembre 2026): report/note_for_professor.md. Nota di una pagina per la prof: idea A contro idea B, nostra proposta B, richiesta di dati.
Nota per il report: la forma a campana efficienza-carico (fig_12) è un confronto tra atenei,
non un effetto del carico.

## Verifica di completezza e dubbi aperti (23 novembre 2026)

**Conclusione:** il nucleo empirico comune e gli elementi grafici sono implementati;
il progetto non è ancora pronto per la consegna. Il report è intenzionalmente rinviato.
Il booklet non richiede una stima causale significativa, né di implementare entrambe A e B.

| Requisito del booklet | Stato | Lavoro ancora necessario |
|---|---|---|
| Intervento, confronto, outcome, orizzonte, popolazione | Definiti nella nota e nel DAG | Scriverli nel report; mantenere studenti aggiuntivi a docenti iniziali fissi |
| Metadata, campione, date e mancanti | Tabelle 07, confronto campioni 12 e controlli disponibili | Sintesi nel report; verifiche di fonte elencate sotto |
| DAG | Completato in 16 | Spiegarne le ipotesi e il limite temporale |
| Figura di variazione e tabella dei risultati | Disponibili in 08–13 e 15 | Selezionarle e commentarle nel report/slides |
| Benchmark e diagnostiche OLS/SFA/DEA-FDH | Implementati, con limiti documentati | Esporre equazioni, distribuzione SFA, significato dei punteggi e diagnostiche |
| Identificazione / richiesta dati | Nessuna variazione esogena dimostrata; proposta nella nota | Precisare date, regole di assegnazione, confronto e canali alternativi |
| Simulazione randomizzata | Eseguita dal master, output in template_run | Spiegare perché lì l'interpretazione causale è più forte |
| Report e presentazione | Non prodotti | Report rinviato; formato e durata da concordare, poi slide |

Decisioni che richiedono la prof/gruppo:
1. Basta il tronco comune con discussione della produzione congiunta, oppure aggiungere A o B?
   14 non va avviato finché B non è confermata. Z, campione, stimatore e bandwidth vanno
   resi coerenti; con FDH non si può mantenere automaticamente ESTIMATOR = 3.
2. Tenere i tre atenei umanistici nell'analisi principale (scelta attuale) oppure cambiare
   campione dopo una verifica di copertura? La scelta cambia DEA CRS in FDH.
3. Lunghezza del report, durata e formato della presentazione del 18 dicembre.

Limiti da dichiarare, non motivi automatici per aggiungere altri modelli:
- Causalità: il ritardo annuale e gli effetti fissi non rendono casuale il carico. I costi
  potrebbero essere antecedenti o successivi alla domanda; il DAG assume solo come possibilità
  studenti -> costi. Non presentare le colonne OLS come effetti causali identificati.
- Variazione: il 2.6% è al netto di effetti ateneo e anno, PRIMA degli altri controlli;
  non coincide necessariamente con tutta la variazione residua che identifica il coefficiente.
- SFA: le prove trovano una soluzione verso sigma_v = 0 e instabilità, non dimostrano
  matematicamente l'assenza globale di massimi. NHN_Z agisce sulla scala di inefficienza:
  il suo coefficiente non è direttamente confrontabile con un'elasticità OLS.
- Frontiere: test e subsampling pooled ignorano la dipendenza delle tre osservazioni per
  ateneo; NREP = 200 è una scelta di tempo con un avviso FEAR. Sono limiti da dichiarare.
- La frontiera per anno cambia il riferimento; il miglioramento relativo nel 2023 non
  prova un ritardo di indicizzazione, né distingue uno shock comune da cambiamenti reali.
- Copertura: i metadata definiscono le pubblicazioni come documenti Scopus attribuiti per
  AF-ID. Verificare AF-ID, filtri per tipo di documento e data di estrazione dei tre atenei.
  Scopus include libri e monografie: non si può attribuire automaticamente il basso output
  alla loro esclusione. Fonte verificata il 23 novembre 2026:
  https://www.elsevier.com/products/scopus/content/content-policy-and-selection
- Adjunct 2023: tutti zero è un'anomalia, non una prova documentale di anno mancante.
  Nessuna analisi adjunct è richiesta per chiudere il nucleo del progetto.
- La composizione disciplinare 2018 è una proxy basata su assegnazioni di aree sovrapposte,
  non quote esclusive di pubblicazioni; reputazione e qualità degli studenti non sono misurate.

Prima della consegna:
- Scrivere report e slide; integrare systematic review, simulazione e richiesta dati.
- Preparare eventualmente theory_QA.md per l'orale: previsto nell'handoff, non nel minimo
  del booklet e non ancora realizzato. Non è una nuova analisi richiesta.
- Verificare organizzazione condivisa e divisione dei compiti, ancora non documentate.
- Eseguire il master completo sulla versione definitiva da consegnare; la modalità
  --presentation-only serve solo ad aggiornare la grafica da output già disponibili.

## Card 4 checklist (24 novembre 2026)
Fonte: docs/VQR_Ten_Group_Projects.pdf, p. 5 (card del gruppo 4). Per ogni richiesta: dove è
coperta (script, tabella o figura; tabelle in output/tables/, figure in output/figures/) oppure
"testo da scrivere nel report". "06" indica i file results_group_04_* in output/template_run/.

| Richiesta della card | Dove è coperta | Testo da scrivere nel report |
|---|---|---|
| Domanda causale: un aumento imposto della domanda didattica riduce la ricerca? | Nota alla prof §1; titolo e sottotitolo di fig_16_causal_dag | Sì: domanda, confronto, orizzonte, popolazione (punto 1 dello standard) |
| Intervento (sceglierne uno): più studenti a docenti iniziali fissi, oppure teaching relief | Scelto il primo (CLAUDE.md, Decisioni confermate); fig_16 e table_16_dag_notes; target totale / a risorse fisse in table_10_ols_wide (colonne senza e con costi). Teaching relief solo come alternativa nella richiesta dati (nota §6) | Sì: motivare la scelta e perché il teaching relief è un intervento diverso |
| Variabili: iscritti, docenti interni, costi correnti, pubblicazioni Scopus | 01 (teaching_panel); g4/07: table_sample_flow, table_sample_descriptives, table_missing_values; costi deflazionati in data/ e g4/12; studenti regolari nella colonna (7) di g4/10 | Sì: sezione dati (unità, mancanti, date, deflatore) |
| ADJUNCT FACULTY: 2023 tutto zero, da verificare | g4/07: table_adjunct_check (61/61 zero nel 2023, nessuno zero nel 2020–22); Registro deviazioni n. 5 | Sì: estensione adjunct NON fatta perché il 2023 è di fatto mancante; inserire la verifica della fonte 2023 nella richiesta di dati |
| Misura: lag_log_teaching_load in t−1, outcome log pubblicazioni in t | 05 e 06 del kit; controllo dell'esposizione in g4/07; variazione in g4/08: table_08_ratio_sd, table_08_ratio_by_year, fig_08_ratio_variation, fig_15_variation | Sì: commentare la variazione disponibile (2.6% al netto di effetti ateneo e anno) |
| Fonti diverse del rapporto: calo dei docenti ≠ aumento degli iscritti | g4/09: table_09_decomposition, table_09_change_sources, fig_09_change_sources; frecce "definition" S → L e F → L in fig_16 | Sì: interpretare le quote e il fatto che il rapporto cala soprattutto per la crescita dei docenti |
| Connessione SFA: una frontiera solo ricerca tratta gli obblighi didattici come inefficienza? | 06 (NHN e NHN_Z: results_group_04_sfa_coefficients, results_group_04_sfa_diagnostics); g4/11: table_11_*, fig_11_nhn_starts; g4/12: table_12_*, fig_12_efficiency_vs_load, fig_15_efficiency_vs_load; g4/13: table_13_*, fig_15_robustness | Sì: discussione esplicita. Sì: cosa aggiungerebbe un modello congiunto didattica-ricerca (idea A discussa nella nota §4, non stimata) |
| Disegno causale: regola di iscrizione, allocazione di capacità o teaching relief indipendenti dalle prospettive di ricerca | Nota alla prof §6 (bozza di richiesta dati: numero programmato, date, regole di allocazione) | Sì: richiesta dati e proposta di disegno complete (punto 5 dello standard) |
| Spillover attraverso tasse, finanziamento del personale e composizione degli studenti | Freccia tratteggiata S → C in fig_16 e table_16_dag_edges; nota §6 (una frase) | Sì: documentare i tre canali prima di affermare la restrizione di esclusione |
| Minacce: reputazione; qualità degli studenti, mix dei programmi e risposte del personale non osservati | Nodi e note del DAG (fig_16, table_16_dag_nodes, table_16_dag_notes); quote disciplinari 2018 come proxy (g4/10, colonne 3–4); effetti fissi di ateneo (colonne 5–7) | Sì: discutere le minacce. Sì: la demografia regionale non soddisfa automaticamente la restrizione di esclusione di uno strumento |
| Compito: scomporre le variazioni del rapporto studenti/docenti | g4/09 (vedi sopra) | Sì: breve commento (vedi riga "Fonti diverse") |
| Compito: risultati con e senza effetti fissi di ateneo | g4/10: table_10_ols_wide, table_10_ols_long; table_15_ols_results | Sì: commento (stima con effetti fissi molto imprecisa: 0.011, SE 0.350) |
| Compito: adjunct come estensione con verifica della fonte 2023 | Non fatta; documentata in g4/07 (table_adjunct_check) | Sì: vedi riga ADJUNCT FACULTY |
