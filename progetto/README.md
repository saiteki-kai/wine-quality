# Descrizione Progetto

## Struttura

```
progetto/
├───data/                           Contiene i dataset
├───output/                         Contiene i modelli trainati e i risultati del test
├───plots/                          
│   ├───comparison/                 Plot per i confronti tra i modelli
│   ├───eda/                        Plot legati alla parte di analisi
│   │   ├───correlation_matrix/
│   │   ├───outliers/
│   │   ├───pca/
│   │   ├───preliminar/
│   │   └───univariate/
│   ├───roc/
│   └───tuning/
└───src/
    ├───eda/
    |   ├───eda.R                   Esegue tutta l'analisi
    |   ├───multivariate.R
    |   ├───outliers.R     
    |   ├───preliminar.R
    |   └───univariate.R
    ├───config.R                    Specifica i percorsi dei risultati, i parametri di tuning e i modelli del train e del test
    ├───prepare.R                   Genera train e test set dal dataset originale
    ├───test.R                      Effettua i test per la specifica configurazione presente in config.R
    ├───train.R                     Effettua il training per la specifica configurazione presente in config.R 
    └───utils.R                     Funzioni di utility
```

## Esecuzione

Per l'analisi bisogna impostare come working directory la cartella ./src/eda/ ed eseguire il file eda.R che esegue tutti i file di analisi.
```r
setwd("$PROJECT_FOLDER/src/eda/")
```

Per il train e il test bisogna impostare come working directory la cartella ./src/
```r
setwd("$PROJECT_FOLDER/src/")
```

Il file train.R esegue il training sul trainset sui modelli specificati nel file config.R.  
I modelli trainati vengono salvati nella cartella output/ in formato RDS.  
I grafici riguardanti il tuning vengono salvati nella cartella plots/tuning/.  

Il file test.R esegue il test sul testset per modelli specificati nel file config.R, e genera i plot per il confronto dei modelli.

Nel file config.R è possibile specificare:
 - se tenere o meno gli outliers con la variabile booleana `keep_outliers`
 - i modelli da trainare tramite la lista `models` e la griglia di tuning 
 - i vari pre processing nella lista `preproc_types`.

## Auto-Consistenza

Il progetto è stato testato nei seguenti ambienti:

- Windows 10 (20H2), R v4.0.5, Rstudio v1.4.1106
- Linux Ubuntu (20.04), R v3.6.3

Tutti i pacchetti necessari vengono installati all'esecuzione di ogni file.
