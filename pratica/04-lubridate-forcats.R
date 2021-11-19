library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)
# arrumando dados
cetesp <- readr::read_rds("data/cetesb.rds")
ozonio <- cetesp %>% 
  select(-lat, -long) %>% 
  filter(poluente == "O3", estacao_cetesb == "Pinheiros") %>% 
  mutate(
    ano = year(data),
    mes = month(data),
    ano_mes = make_date(ano, mes, 01),
    dia_semana = wday(data, label = TRUE, locale = "en_US.UTF-8")
  )
  # tidyr::unite()
# cetesp %>% distinct(estacao_cetesb)

cetesp %>% 
  ggplot(aes(data, concentracao))+
  geom_line()

# grafico por ano

ozonio %>% 
  group_by(ano) %>% 
  summarise(
    concentracao_media = mean(concentracao, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = ano, y = concentracao_media))+
  geom_line()

# grafico por ano/mes
ozonio %>% 
  group_by(ano_mes) %>% 
  summarise(
    concentracao_media = mean(concentracao, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = ano_mes, y = concentracao_media))+
  geom_line()

# qual o horario do dia com mais ozonio

ozonio %>% 
  group_by(hora) %>% 
  summarise(concentracao_media = mean(concentracao, na.rm = TRUE)) %>% 
  ggplot(aes(x = hora, y = concentracao_media)) +
  geom_line()

# por dia da semana e hora do dia
ozonio %>% 
  group_by(hora, dia_semana) %>% 
  summarise(concentracao_media = mean(concentracao, na.rm = TRUE)) %>% 
  ggplot(aes(hora, concentracao_media))+
  geom_line()+
  facet_wrap(vars(dia_semana))

# forcats -----------------------------------------------------------------
# qual o genero mais lucrativo?
imdb <- readr::read_rds("data/imdb.rds")

# arrumando dados
imdb <- imdb %>% 
  mutate(lucro =  receita - orcamento)

# grafico de series
imdb %>% 
  group_by(generos) %>% 
  summarise(lucro_medio = mean(lucro, na.rm = TRUE)) %>% 
  ggplot(aes(generos, lucro_medio))+
  geom_col()

# Arrumando generos
imdb_generos <- imdb %>% 
  mutate(
    generos = stringr::str_split(generos, "\\|")
  ) %>%
  tidyr::unnest(generos) 

# imdb_generos %>% distinct(generos)
# grafico com toodos os generos
imdb_generos %>% 
  group_by(generos) %>% 
  summarise(orcamento_media = mean(orcamento, na.rm = TRUE)) %>% 
  ggplot(aes(generos, orcamento_media))+
  geom_col()+
  coord_flip()

# Juntando os generos em "outros"
#usando fct_lump_n
imdb_generos %>% 
  mutate(
    generos_n = fct_lump_n(generos, n = 10, other_level = "Outros")
  ) %>% 
  group_by(generos_n) %>% 
  summarise(orcamento_media = mean(orcamento, na.rm = TRUE)) %>% 
  ggplot(aes(generos_n, orcamento_media))+
  geom_col()+
  coord_flip()

#usando fct_lump_min
imdb_generos %>% count(generos, sort = TRUE)
imdb_generos %>% 
  mutate(
    generos = fct_lump_min(generos, min = 406, other_level = "Outros")
  ) %>% 
  group_by(generos) %>% 
  summarise(orcamento_media = mean(orcamento, na.rm = TRUE)) %>% 
  ggplot(aes(generos, orcamento_media))+
  geom_col()+
  coord_flip()

#usando fct_lump_prop
imdb_generos %>% 
  mutate(
    generos = fct_lump_prop(generos, prop = 0.01, other_level = "Outros")
  ) %>% 
  group_by(generos) %>% 
  summarise(orcamento_media = mean(orcamento, na.rm = TRUE)) %>% 
  ggplot(aes(generos, orcamento_media))+
  geom_col()+
  coord_flip()

#usando fct_lump_lowfreq
imdb_generos %>% 
  mutate(
    generos = fct_lump_lowfreq(generos,  other_level = "Outros")
  ) %>% 
  group_by(generos) %>% 
  summarise(orcamento_media = mean(orcamento, na.rm = TRUE)) %>% 
  ggplot(aes(generos, orcamento_media))+
  geom_col()+
  coord_flip()

imdb_generos %>% count()

# arrumando a ordem dos generos
imdb_generos %>% 
  mutate(
    generos_n = fct_lump_n(generos, n = 10, other_level = "Outros")
  ) %>% 
  group_by(generos_n) %>% 
  summarise(orcamento_media = mean(orcamento, na.rm = TRUE)) %>% 
  mutate(
    generos_n = fct_reorder(generos_n, orcamento_media),
    generos_n = fct_relevel(generos_n, "Outros", after = 0)
  ) %>% 
  ggplot(aes(generos_n, orcamento_media))+
  geom_col()+
  coord_flip()

# melhorando o eixo y
imdb_generos %>% 
  mutate(
    generos_n = fct_lump_n(generos, n = 10, other_level = "Outros")
  ) %>% 
  group_by(generos_n) %>% 
  summarise(lucro_media = mean(lucro, na.rm = TRUE)) %>% 
  mutate(
    generos_n = fct_reorder(generos_n, lucro_media),
    generos_n = fct_relevel(generos_n, "Outros", after = 0),
    lucro_media = lucro_media/1000000
  ) %>% 
  ggplot(aes(generos_n, lucro_media))+
  geom_col()+
  scale_y_continuous(labels = scales::label_number(accuracy = 1))+
  labs(y = "Lucroo (em milhões)")+
  coord_flip()

  library(scales)
imdb_generos %>% 
  mutate(
    generos_n = fct_lump_n(generos, n = 10, other_level = "Outros")
  ) %>% 
  group_by(generos_n) %>% 
  summarise(lucro_media = mean(lucro, na.rm = TRUE)) %>% 
  mutate(
    generos_n = fct_reorder(generos_n, lucro_media),
    generos_n = fct_relevel(generos_n, "Outros", after = 0),
    # lucro_media = lucro_media/1000000
  ) %>% 
  ggplot(aes(generos_n, lucro_media))+
  geom_col()+
  scale_y_continuous(labels = comma)+
  labs(y = "Lucroo (em milhões)")+
  coord_flip()
  # theme(axis.text.x = element_blank())
