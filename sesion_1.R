##### SESION 1 ####
# Introducción
# Exploración
# librerias
# funciones
# obtener palabras
# frecuencia de palabras
# wc

rm(list = ls())

dir.create("sesion_1")
dir.create("sesion_1/script")
dir.create("sesion_1/out")


##### objetivo ####
# Este taller tiene como objetivo ofrecerles una idea de las cosas que pueden hacer con textos 
# y los tipos de análisis que pueden realizarse
# Tareas comunes y herramientas fáciles de usar.

library(tidyverse)
library(tidytext) 
library(readtext)
library(rvest)
library(stringi)
library(stringr)
library(stopwords)
library(wordcloud)

##### repaso ####
# tipos de datos (básicos)
# Vectores
# Factores
# Listas
# Matrices y arreglos
# Dataframes

# análisis de texto lo que usamos son caracteres, vectores, listas, df

##### Cadenas de caracteres ####
# objetos de clase de caracter son los que generalmente trabamos, no son numericos pj 
"Frida Garcia Celis"
vector <- c("Frida Garcia Celis", "33", "mujer", "chilanga")
vector
vector <- c("Frida Garcia Celis", 33, "mujer", "chilanga")
vector
class(vector)
length(vector)

mi_lista <- list(nombre = "Frida", edad = 33, info = c("mujer", "chilanga"))
str(mi_lista)
length(mi_lista)

rm(mi_lista, vector, x)

# no hay un límite en lo que se puede representar un vector, 
# en una columna de un DF puede haber una obra completa

##### dataframes ####
df <- data.frame("edad" = c(21,15,18), 
                 "nombre" = c("pepe", 
                            "juan",
                            "susana"))

str(df)
df$nombre <- as.factor(df$nombre)
str(df)

df <- data.frame("edad" = c(21,15,18), 
                 "nombre" = c("pepe", 
                              "juan",
                              "susana"),
                 stringsAsFactors = FALSE)

str(df)
is.character(df$nombre) 

rm(df)

##### Ejemplo práctico ####
pacman::p_load(tidyverse, rvest, stringr)

links_totales <- NULL #Objeto vacío
links <- NULL #Objeto vacío

for(i in 1:5){
  
  cat(i, "...") #Contador
  
  url <- paste0("https://lopezobrador.org.mx/transcripciones/page/", i)
  
  pg <- read_html(url) ### Lee todo el código fuente
  
  links <- pg %>% 
    html_nodes(".entry-title a") %>% # Ubicación del selector
    html_attr("href") %>%  # Enlaces a todos los discursos de AMLO
    as_tibble()
  
  links_totales <- bind_rows(links_totales, links)
}

write.csv(links_totales, "sesion_1/out/links_amlo.csv", row.names = F)

##### Obtener el texto de cada discurso

base <-  NULL
bind <- NULL

for (x in 1:nrow(links_totales)) {
  
  cat(x, "...") #Contador
  
  url <- links_totales$value[x]
  
  pg <- read_html(url) 
  
  titulo <- pg %>% 
    html_node(".entry-title") %>% 
    html_text()
  
  if(length(titulo) == 0) {
    titulo <- NA 
  }
  
  texto <- pg %>% 
    html_nodes(".entry-content") %>% ## Selector para el párrafo #### ¿dónde se encuentra?
    html_text() 
  
  if(length(texto) == 0) {
    texto <- NA 
  }
  
  bind <- tibble(titulo, texto)
  base <- bind_rows(base, bind)
}

base <- base %>% 
  na.omit()

rm(bind, links_totales, pg, texto, titulo, url, x)

write.csv(base, "sesion_1/out/estenograficas.csv", row.names = F)

# La propiedad "class" mostrará el nombre o nombres de las clases CSS asociadas 
# al elemento de texto. Estos nombres de clases se pueden encontrar entre comillas
# y estarán separados por espacios.

# explorar #
base <- read.csv("sesion_1/out/estenograficas.csv")
base <- base[1:15,]

glimpse(base)
str(base)

# lenght  vs nchar
nchar(base$texto) #base
stringr::str_length(base$texto) # stringr
length(base$texto[1]) #base

# casefolding
tolower(base$texto[1]) #base
toupper(base$texto[1]) #base
casefold(base$texto[1], upper = F) #base

# cambiar caracteres chartr()
chartr("a", "A", "manzana") #base

# contar cadena
str_count("gato Gato perrito", "gato")
str_count("gato Gato perrito", "[G|g]ato")
str_count("gato Gato perrito", boundary("word"))

# extraer
substr("gato", 1, 2) # caracteres
word("Hola, mundo, qué onda", end = 2)

# Reemplazo de caracteres o palabras específicas
# eliminar urls o numeros, espacios
# corregir typos

base %>% 
  mutate(n = str_count(texto, "bastón de mando")) 

# detectar patrones
base %>% 
  filter(!str_detect(texto, "bastón de mando")) %>% 
  select(titulo) 

# extraer palabras
base %>% 
  filter(!str_detect(texto, "bastón de mando")) %>% 
  select(texto) %>%
  pull(texto) %>% # we pull out the text vector only
  word(end = 5) 
  
str_replace_all(base$texto[1], "\n", "") 

tidy_base <- base %>%
  mutate(texto = str_replace_all(texto, "\n", "")) %>% 
  unnest_tokens("word", texto)

head(tidy_base)

stopword <- stopwords(language = "es")

tidy_base <- anti_join(tidy_base, data.frame(word = stopword), by = "word")

tidy_base %>%
  count(word, sort = TRUE) %>% 
  head()

amlo <- tidy_base %>%
  count(word, sort = TRUE) %>% 
  head(5) %>% 
  select(word) %>% 
  unlist()

tidy_base_amlo <- str_replace_all(tidy_base, paste(amlo, collapse = "|"), "") #### ojo con el resultado

tidy_base <- tidy_base %>%
  filter(!grepl(paste(amlo, collapse = "|"), word)) #### esta está ok

tidy_base %>%
  count(word, sort = TRUE) %>%
  filter(n > 400) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

tidy_base %>% 
  count(word) %>% 
  with(wordcloud(word, n, min.freq = 50, colors = brewer.pal(8, "Dark2")))
