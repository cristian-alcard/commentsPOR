library(dplyr) #data
library(ggplot2) #plots
library(ggthemes) #temas
library(readxl)
library(lubridate)   # package for read timestamps

#library(janeaustenr)
library(stringr)
library(tidytext)
library(stringdist)
library(sjmisc)

rm(list=ls()) #eliminar objetos del ws
#setwd("C:/RprojectsCAC/commentsPO")

start_time <- Sys.time()
df <- read_excel("2020.xlsx", sheet ="CONCU") 
df_red <- select(df, REFERENCE_LABEL, CREATION_TIME, ENROUTE_TIME, ARRIVAL_TIME,
                 FIELD_COMPLETION_TIME, COMMENTS)
df_red <- filter(df_red, COMMENTS != "") #comentario vacio no aporta
df_red$LARGO = nchar(df_red$COMMENTS)

df_red <- df_red[order(-df_red$LARGO),] #ordenamos por longitud comentario

#columna para guardar el comentario nuevo
df_red$comentario_depurado <- ""
df_red$ACTIVIDAD_1 <- ""
df_red$ACTIVIDAD_2 <- ""

#abr <- read_excel("filtradas.xlsx", sheet ="abreviaciones")
filtro <- read_excel("filtradas.xlsx", sheet ="palabras2") 
#si se parece en un %indicador a keyword, usar reemplazo. 
reemp <- read_excel("filtradas.xlsx", sheet ="reemplazo2")  #palabras para reemplazo

comb <- read_excel("filtradas.xlsx", sheet ="comb2") #combinaciones

#pasar a formato char las columnas
filtro$Palabra <- as.character(filtro$Palabra)
reemp$Palabra <- as.character(reemp$Palabra)
reemp$Reemplazo <- as.character(reemp$Reemplazo)
reemp$Indicador <- as.double(reemp$Indicador)


#parametro de los for 
nr <- dim(reemp)
nf <-dim(filtro)
N <- dim(df_red)[1]

#buscamos el comentario de la orden j, ordenadas por largo comentario 
#t <- 18

for (t in 1:N) {
  test_text <- tibble(line = 1:1 ,text=df_red[t,6]) #columna del comentario
  test_text <- test_text %>% 
    mutate_all(as.character) %>% 
    unnest_tokens(word, text)
  #comentario se tokeniza en palabras
  
  #eliminamos aquellas palabras de largo 2
  test_text <- filter(test_text, nchar(word)>2)
  #eliminamos palabras que son números
  test_text$ISNUMBER <- as.numeric(test_text$word)
  test_text <- filter(test_text, is.na(ISNUMBER)==TRUE) 
  test_text$ISNUMBER <- NULL
  
  #buscamos en la lista de palabras a eliminar
  for (i in 1:nf[1]) {
    test_text <- filter(test_text, word != filtro$Palabra[i]) 
  }
  #test_text$COMPARA <- test_text$word
  #reemplazo con la palabra correcta más parecida 
  #indicador de similitud según la palabra, algunas 0.7 otras 0.8
  #ejemplo, 0.8 en el caso de contact, para no confundir con palabra contacto. 
  for (j in 1:nr[1]) {
    test_text$word[stringsim(test_text$word,reemp$Palabra[j], method="lv") >=reemp$Indicador[j]] <- reemp$Reemplazo[j]
  }
  #eliminar duplicados antes de buscar combinaciones
  tabla_texto <- distinct(test_text)
  comment <- paste(paste(tabla_texto$word," "), collapse = "")
  df_red$comentario_depurado[t] <- comment
  
  nc <- dim(comb)[1]
  flag <- 0 
  for (w in 1:nc) {
    if (flag==1) {
      if (str_contains(comment, comb[w,1:2], logic="and")==TRUE ) {
        df_red$ACTIVIDAD_2[t] <- comb[w,3]
        break }
    }
    else {
      if (str_contains(comment, comb[w,1:2], logic="and")==TRUE ) {
        df_red$ACTIVIDAD_1[t] <- comb[w,3]
        flag <- 1 
      }
    }
  }
  
}  


df_revision <- filter(df_red, ACTIVIDAD_1=="")
df_red$ACTIVIDAD_1 <- as.character(df_red$ACTIVIDAD_1)
df_red$ACTIVIDAD_2 <- as.character(df_red$ACTIVIDAD_2)

write.csv(df_red, "resultado.csv", row.names = FALSE)

end_time <- Sys.time()
elapsed_time <- end_time-start_time
elapsed_time

###lectura de resultados
sat <- read_excel("sat.xlsx")
resultado <- read.csv("resultado.csv")
na <- dim(sat)[1]
no <- dim(resultado)[1]

resultado$SAT <-""

for (i in 1:no) {
  a1 <- resultado$ACTIVIDAD_1[i]
  a2 <- resultado$ACTIVIDAD_2[i]
  if (a1=="" & a2=="") {
    resultado$SAT[i] <- "sin_clasificar"
  }
  for (j in 1:na) {
    a_sat <- sat$`Actividades SAT`[j]
    if (a1==a_sat) {
      resultado$SAT[i] <- a1
      break
    }
    if (a2==a_sat) {
      resultado$SAT[i] <- a2
      break
    }
  }
  if (resultado$SAT[i]=="") {
    resultado$SAT[i] <- "otra_actividad"
  }
  
}

write.csv(resultado, "resultado2.csv", row.names = FALSE)

#reemplazamos con las abreviaciones y/o agrupaciones. 
#ejemplo arr.=arranque, inst.=instalacion
#azul, rojo, naranjo = color. 
#for (j in 1:na[1]) {
#  test_text$word[stringsim(test_text$word,abr$Abreviacion[j], method="lv") >0.95] <- abr$Resultado[j]
#}




