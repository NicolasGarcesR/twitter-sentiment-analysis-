library(stringr)
library(tokenizers)
library(stopwords)
library(tidyverse)
library(readxl)
library(quanteda)
library(tidytext)
library(tm)
library(zoo)
library(scales)
library(lubridate)
#############

###############################################
######### ANALISIS DE SENTIMIENTOS ############
###############################################


## DICCIONARIO ##

download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")
afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()


## FUNCION LIMPIADORA ##

limpiar<- function(texto){
  nuevo_texto <- tolower(texto)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  nuevo_texto <- str_replace_all(nuevo_texto, ".+[:digit:].+", " ")
  nuevo_texto <- str_replace_all(nuevo_texto, "[:digit:].+", " ")
  nuevo_texto <- str_replace_all(nuevo_texto,".+[:digit:]", " ")
  nuevo_texto <- str_replace_all(nuevo_texto, "[:digit:]", " ")
  nuevo_texto <- str_replace_all(nuevo_texto,"@\\S*", " ")
}

## LIMPIANDO LOS DATOS ##

raw_tweets<- read.csv2("Pe23MAY.csv", stringsAsFactors = F, fileEncoding = "latin1")

## Los tweets tiene fecha de publicacion, nombre del usuario, contenido del tweet y numero de retweets

limpiar(raw_tweets$text)  

## del contenido del tweet se eliminan los links, la mencion a otros usuarios, hashtags y otros 
## elementos que no son importantes para el analisis.



tweets_clean<- raw_tweets%>%
  unnest_tokens(input = "text", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) 

# se combina las palabras del diccionario con las palabras de los tweets, dejando solo las que coinciden
# en las dos bases. De esta forma, la nueva base tiene : las palabras, su clasificacion, su traduccion en ingles
# su puntaje y la misma informacion que tenia el tweet de donde proviene.


tweets_clean <-
  tweets_clean%>%
  group_by(created_at) %>% select_("screen_name","created_at","Palabra","Puntuacion","Tipo")
# se deja la informacion de interes


# Para eliminar la palabra no

tweets_clean <-
  tweets_clean%>%
  filter(Palabra != "no") 

view(tweets_clean)



mean(tweets_clean$Puntuacion)
# Este es el valor que toma el analsis de sentimientos para ese grupo de tweets

###################
write.table(tweets_clean,file = "xxxxxxxx.csv" ,row.names = FALSE,sep = ";")
#######################



