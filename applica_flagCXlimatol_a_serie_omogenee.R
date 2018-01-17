#17 gennaio 2018: Ai file di output prodotti da Climatol, applica il flag 1 (ovvero riconverte in NA i dati "riempiti" mediante interpolazione da Climatol)
rm(list=objects())
library("tidyverse")
library("stringr")
options(error=recover,warn = 2)

PARAMETRO<-c("tmax","tmin")[2] #fissare se 1 per tmax o 2 per tmin


stringa1<-"^Tm[a][x]_1961-2015.+csv$"
stringa2<-"^Tm[i][n]_1961-2015.+csv$"

ifelse(PARAMETRO=="tmax",stringa1,stringa2)->stringa_pattern

if(!dir.exists("./datiFlaggati")) dir.create("datiFlaggati")

list.files(pattern=stringa_pattern)->ffile  #fissare Tm[i][n] per la minima
grep("^.+flg.csv$",ffile)->fileFlag
ffile[-fileFlag]->fileDati


#leggi i dati e applica i flag
purrr::walk(fileDati,.f=function(qualeFile){
  
  read_delim(qualeFile,delim=",",col_names = TRUE)->dati
  
  #crea il nome del file contenente i flag applicati da Climatol
  str_replace(qualeFile,"\\.csv","-flg.csv")->qualeFlagFile
  tryCatch({
    read_delim(qualeFlagFile,delim=",",col_names = TRUE)
  },error=function(e){
    
    stop(qualeFlagFile)
    
  })->flag
  
  #I file prodotti da Climatol contengono flag: 0, 1 e 2 (0 dato originale, 1 dato riempito, 2 dato corretto con omogeneizzazione)
  #i dati mancanti riempiti da Climatol li rimettiamo a NA
  which(flag$Value==1)->na_riempiti
  if(length(na_riempiti)) dati[na_riempiti,]$Value<-NA
  
  str_extract(qualeFile,"^.+_[0-9]+(-|\\.)")->nomeNuovo #alcuni nomi di file finiscono in codice.csv altri in codice-2.csv..evitiamo di scrivere un codice troppo specifico, cercando fi generalizzare
  str_sub(nomeNuovo,1,str_length(nomeNuovo)-1)->nomeNuovo #togliamo il punto o l'- finale
  paste0(nomeNuovo,"_homog.csv")->nomeNuovo

  dati %>% separate(Date,into=c("yy","mm","dd"),sep="-") %>%
    write_delim(.,paste0("datiFlaggati/",nomeNuovo),col_names=TRUE,delim=";")
  
})
