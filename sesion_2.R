#### Sesión 2 ####
rm(list = ls())
options(scipen = 999)

# Limpieza de texto: eliminar puntuación, caracteres especiales y palabras irrelevantes.
# Obtención de n-gramas
# Análisis de sentimientos

# ¿Qué vimos la semana pasada?
# Mostramos cómo este formato se puede utilizar para abordar preguntas sobre la frecuencia de palabras.
# Analizar cuáles son las palabras más utilizadas en los documentos.

# ¿Qué vamos a ver hoy?
# Abordaremos el tema de la minería de opiniones o el análisis de sentimientos.
# Utilizamos nuestra comprensión de la intención emocional de las palabras.
# Inferimos si una sección de texto es positiva o negativa.
# Podemos identificar emociones más matizadas, como sorpresa o disgusto.

# 
# dir.create("sesion 2/inp")
# dir.create("sesion 2/out")
# dir.create("sesion 2/graph")
# dir.create("sesion 2/script")

# paqueterias
library(tidyverse)
library(tidytext) # get_sentiments en inglés
library(textdata) # donde viven los diccionarios
library(syuzhet) # sentiment en español
library(stopwords) # sentiment en español
# library(udpipe) # steming
library(tm)
library(lubridate)

#### importaciones ####
data <- read.csv("sesion 2/inp/tuits_candidatos.csv", stringsAsFactors = F, fileEncoding = "UTF-8")

#### limpieza ####
stopword <- stopwords::stopwords(language = "es")

limpiar_tokenizar <- function(texto){
        nuevo_texto <- tolower(texto)
        nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
        nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
        nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
        nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
        nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
        # Eliminación de tokens con una longitud < 2
        nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
        return(nuevo_texto)
}

str_detect("23 de septiembre", "\\d")
# f(x)

tempo <- data %>%
        mutate(texto_tokenizado = map(.x = text,
                                      .f = limpiar_tokenizar)) %>%
        mutate(section = row_number()) %>%
  unnest() %>%
  dplyr::rename(token = texto_tokenizado) %>%
  filter(!(token %in% stopword))

# write.csv(tempo, "sesion 2/out/tokens_tuits.csv", row.names = F)

##### analisis de frecuencias ####

tempo %>%
  count(token, sort = TRUE)

# sum(tempo_freq$proportion[tempo_freq$screen_name=="JoseAMeadeK"])

frequency <- tempo %>% 
  filter(screen_name %in% c("lopezobrador_","RicardoAnayaC" ,"JoseAMeadeK", "Mzavalagc")) %>% 
  count(screen_name, token) %>%
  group_by(screen_name) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = screen_name, values_from = proportion) %>%
  pivot_longer(`RicardoAnayaC`:`JoseAMeadeK`,
               names_to = "author", values_to = "proportion")

ggplot(frequency, aes(x = proportion, y = `lopezobrador_`, 
                      color = abs(`lopezobrador_` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "lopezobrador_", x = NULL, 
       caption = "Las palabras que están cerca de la línea en estos gráficos tienen frecuencias similares en ambos conjuntos de textos.
       Las palabras que están lejos de la línea son palabras que se encuentran más en un conjunto de textos que en otro.")

#### análisis de correlaciones ####
# Ahora, vamos a cuantificar cuán similares y diferentes son estas frecuencias de palabras utilizando una prueba de correlación.
# ¿Qué tan correlacionadas están las frecuencias de palabras entre los candidatos?
# proporcionará información sobre cuán similares o diferentes son las frecuencias de palabras entre estos conjuntos de textos

cor.test(data = frequency[frequency$author == "JoseAMeadeK",],
         ~ proportion + `lopezobrador_`)

cor.test(data = frequency[frequency$author == "RicardoAnayaC",],
         ~ proportion + `lopezobrador_`)

cor.test(data = frequency[frequency$author == "Mzavalagc",],
         ~ proportion + `lopezobrador_`)

# ¿cómo leerlo? 
# Si la correlación entre las frecuencias de palabras es alta y positiva, 
# significa que comparten muchas palabras en común y que sus textos son más similares en términos de
# las palabras que utilizan.
# Si la correlación es baja o negativa, indica que los textos y
# difieren significativamente en términos de las palabras que utilizan.

#### analisis de sentimientos ####
# tokens 
# AFINN = (n a p)-5 a 5
# BING = N / P
# NRC = N/P y emociones

# minería de opiniones
# análisis de sentimientos

# tempo <- read_csv("sesion 2/out/tokens_tuits.csv")

tempo <- tempo %>%
  separate(created_at, into = c("Fecha", "Hora"), sep = " ") %>%
  separate(Fecha, into = c("Dia", "Mes", "Periodo"), sep = "/",
           remove = FALSE) %>%
  mutate(Fecha = dmy(Fecha),
         Semana = week(Fecha) %>% as.factor(),
         text = tolower(text)) 

# usando nrc
# syuzhet es un paquete principalmente centrado en el léxico nrc
sentimientos_nrc <- syuzhet::get_nrc_sentiment(tempo$token[1:5], lang="spanish")
sentimientos_nrc <- cbind(tempo, sentimientos_nrc)
print(sentimientos_nrc)

# write.csv(sentimientos_nrc, "sesion 2/out/sentimiento_nrc.csv", row.names = F)
sentimientos_nrc <- read_csv("sesion 2/out/sentimiento_nrc.csv")

# recuendo de palabras con cada emoción
length(sentimientos_nrc$token[sentimientos_nrc$negative>0])
length(sentimientos_nrc$token[sentimientos_nrc$positive>0])

#método largo
sentimientos_tempo <- sentimientos_nrc %>%
  group_by(screen_name) %>%
  summarize(
    anger = sum(anger),
    anticipation = sum(anticipation),
    disgust = sum(disgust),
    fear = sum(fear),
    joy = sum(joy),
    sadness = sum(sadness),
    surprise = sum(surprise),
    trust = sum(trust),
    negative = sum(negative),
    positive = sum(positive)
  )

# método corto
# Función para sumar columnas específicas por grupo
sumar_columnas_por_grupo <- function(data, group_column, columns_to_sum) {
  result <- data %>%
    group_by({{ group_column }}) %>%
    summarize(across({{ columns_to_sum }}, sum))
  return(result)
}

sentimientos_tempo_nrc <- sumar_columnas_por_grupo(sentimientos_nrc, screen_name, 11:20)
summary(sentimientos_tempo_nrc)
# 
# write.csv(sentimientos_tempo_nrc, "sesion 2/out/sentimiento_nrc_group.csv", row.names = F)

sentimientos_viz <- reshape2::melt(sentimientos_tempo_nrc, id.vars = c("screen_name"))

ggplot(sentimientos_viz, aes(variable, value, fill = screen_name)) +
  geom_bar(stat = "identity") +
  # scale_fill_manual(values = c("#f4effa", "#2f184b")) +
  facet_wrap(~screen_name, scales = "free_x") +
  theme_minimal() +
  labs(x = "", y = "") +  # Etiquetas de ejes
  theme(
    legend.position = "none",
    strip.text.x = element_text(size = 18, face = "bold"),
    panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
    plot.background = element_rect(fill = "white"),
    axis.text.x = element_text(size = 13, angle = 45, hjust = 1),  # Ajuste de etiquetas del eje x
    axis.text.y = element_text(size = 13)
  )

ggsave("sesion 2/graph/sentimiento_nrc.jpg", width = 15, height = 12)

# usando afinn
dicc_afinn <- read_csv("sesion 2/inp/lexico_afinn.en.es.csv")

sentimiento_afinn <- tempo %>%
  # unnest_tokens(input = "text", output = "Palabra") %>%
  rename("Palabra" = token) %>% 
  inner_join(dicc_afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Candidato" = screen_name)

sentimiento_afinn <- sentimiento_afinn %>%
  group_by(status_id) %>%
  summarise(Puntuacion_tuit = mean(Puntuacion)) %>%
  left_join(sentimiento_afinn, ., by = "status_id") %>% 
  mutate(Puntuacion_tuit = ifelse(is.na(Puntuacion_tuit), 0, Puntuacion_tuit)) 

sentimiento_viz <- sentimiento_afinn %>% 
  group_by(Candidato, Tipo) %>%
  count(Palabra, sort = T) %>%
  top_n(n = 10, wt = n) %>% 
  ungroup()

sentimiento_viz <- sentimiento_viz %>% 
  arrange(Candidato, Tipo)

ggplot(sentimiento_viz[sentimiento_viz$Tipo=="Positiva",]) +
  aes(Palabra, n, fill = Candidato) +
  geom_col() +
  facet_wrap("Candidato", scales = "free_y") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(title = "Positiva")

ggplot(sentimiento_viz[sentimiento_viz$Tipo=="Negativ",]) +
  aes(Palabra, n, fill = Candidato) +
  geom_col() +
  facet_wrap("Candidato", scales = "free_y") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(title = "Negativo")

# fechas
tuits_afinn_fecha <-  sentimiento_afinn %>%
  group_by(status_id) %>%
  mutate(Suma = mean(Puntuacion)) %>%
  group_by(Candidato, Fecha) %>%
  summarise(Media = mean(Puntuacion))

tuits_afinn_fecha %>%
  ggplot() +
  geom_line(  aes(Fecha, Media, color = Candidato, group = Candidato)) +
  theme(legend.position = "top")

# separamos las líneas por candidato, será más fácil observar el las tendencias de los Candidatos.

tuits_afinn_fecha %>%
  ggplot() +
  geom_hline(yintercept = 0, alpha = .35) +
  geom_line(aes(Fecha, Media, color = Candidato, group = Candidato)) +
  facet_grid(Candidato~.) +
  theme(legend.position = "none")

# LOESS
tuits_afinn_fecha %>%
  ggplot(aes(Fecha, Media, color = Candidato)) +
  geom_smooth(aes(Fecha, Media, color = Candidato, group = Candidato), method = "loess", fill = NA) 

# comparando sentimientos
sentimiento_afinn %>%
  mutate(Fecha = as.Date(Fecha , format = "%d/%m/%Y")) %>%
  group_by(Candidato, Fecha) %>%
  count(Tipo) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Fecha, Proporcion, fill = Tipo) +
  geom_col(width = 5) +
  facet_grid(Candidato~.) +
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_x_date(labels = scales::date_format("%b")) +
  theme(legend.position = "top")

ggsave("sesion 2/graph/comparación_fechas.jpg", width = 15, height = 12)

tempo_bigrama <- tempo %>% 
  unnest_tokens(output = word, input = text, token = "ngrams", n=2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stopword) %>%
  filter(!word2 %in% stopword) %>% 
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep = " ")

