library(tidyverse)
library(dplyr)
library(stringr)
library(readxl)
source("./../../funzioni.R")

# DESIDERATA Anno;annoscolastico;CodiceComune;DescrizioneComune;Tipo_scuola;infanzia_alutot;primarie_alutot;secIgrado_alutot;secIIgrado_alutot;CM;
# ---------- CodiceUnione;DescrizioneUnione;CodiceDistrettoSanitario;DescrizioneDistrettoSanitario;CodiceAUSL;DescrizioneAUSL



colnam=read_delim("output/scuola(base).csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% colnames()
#1 Anagrafe ------------------------------------------------------------
  
import_all_generic("C:/Users/riccie/Desktop/Progetti/aggiornamento_SCUOLE_ATLANTE/Scuole")

ANAGRAFE<-bind_dt(dt_name_pattern = '.*ANAGRAFE.*') %>% filter(PROVINCIA=='BOLOGNA')



#2 Infanzia -----------------------------------------
 ## 2.a import e cleaning ---------------------------------

rm(list=setdiff(ls(envir = globalenv())[sapply(ls(globalenv()), function(x) find_df(x))],"ANAGRAFE"))

import_all_generic("C:/Users/riccie/Desktop/Progetti/Schede per territorio - dup/Scuola",pattern = ".*INFANZIA.*")


INFANZIA_PAR<-bind_dt(dt_name_pattern = '.*INFANZIACLAPAR') %>%
    mutate(Tipo_scuola=2) %>% 
    inner_join(ANAGRAFE) %>% 
  filter(!is.na(CLASSI))

INFANZIA_STA<-bind_dt(dt_name_pattern = '.*INFANZIACLAST') %>% 
  mutate(Tipo_scuola=1)%>% 
  right_join(ANAGRAFE)%>% 
  filter(!is.na(CLASSI))

INFANZIA<-rbind(INFANZIA_PAR,INFANZIA_STA)

  ##2.b elaborazione -------------------------------------------------

INFANZIA<-INFANZIA %>%   

  mutate(infanzia_alutot=BAMBINIMASCHI+BAMBINIFEMMINE,Anno=paste0('20',str_sub(start=-2,ANNOSCOLASTICO)),CM="Citt. metropolitana di Bologna") %>% 
  rename(annoscolastico=ANNOSCOLASTICO) %>% 
  group_by(Anno,annoscolastico,Tipo_scuola,CM,CODICECOMUNESCUOLA        ) %>% 
  summarise(infanzia_alutot=sum(infanzia_alutot)) %>% relocate(infanzia_alutot, .before = CM)
  


#3 Altri gradi di istruzione -----------------------------------------
## 3.a import e cleaning ---------------------------------


rm(list=setdiff(ls(envir = globalenv())[sapply(ls(globalenv()), function(x) find_df(x))],c("ANAGRAFE","INFANZIA")))

import_all_generic("C:/Users/riccie/Desktop/Progetti/Schede per territorio - dup/Scuola",pattern = ".*ALU.*")


SCUOLE_PAR<-bind_dt(dt_name_pattern = '.*PAR.*') %>%
  mutate(Tipo_scuola=2) %>% 
  inner_join(ANAGRAFE) 

SCUOLE_STA<-bind_dt(dt_name_pattern = '.*STA.*') %>% 
  mutate(Tipo_scuola=1)%>% 
  inner_join(ANAGRAFE )



SCUOLE<-rbind(SCUOLE_PAR,SCUOLE_STA) 

## 3.b elaborazione ---------------------------------


SCUOLE %>%  
  mutate(CODICECOMUNESCUOLA= ifelse(CODICECOMUNESCUOLA=='A558','M369',CODICECOMUNESCUOLA),
         DESCRIZIONECOMUNE= ifelse(CODICECOMUNESCUOLA=='A558','Alto Reno Terme',DESCRIZIONECOMUNE),
         Anno=paste0('20',str_sub(start=-2,ANNOSCOLASTICO)),CM="Citt. metropolitana di Bologna") %>% 
  rename(annoscolastico=ANNOSCOLASTICO) %>% 
  group_by(Anno,annoscolastico,Tipo_scuola,CM,ORDINESCUOLA,CODICECOMUNESCUOLA           ) %>% 
  summarise(alunni=sum(ALUNNI)) %>%pivot_wider(names_from = ORDINESCUOLA,values_from = alunni) %>%  
  rename(primarie_alutot=`SCUOLA PRIMARIA`,secIgrado_alutot=`SCUOLA SECONDARIA I GRADO`,
         secIIgrado_alutot=`SCUOLA SECONDARIA II GRADO`) %>% 
  
# 4 join tra i gradi e writing  -----------------------------
  
  full_join(INFANZIA) %>%
  full_join( read_excel("F:/DIREZIONE-GENERALE/DATA/DB/Territorio/territorio.xlsx"),by=c("CODICECOMUNESCUOLA"="Codice Catastale")) %>%
  select(all_of(colnam)) %>% 
  write.csv2("scuola(base).csv",row.names = F,sep = ";")
 











