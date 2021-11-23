library(purrr)

vetor <- 100:200
vetor[85]
vetor[[85]]

lista <- list(1, 2, 3, 4)
lista[[2]][1]

lista <- list(
  elem1 = 1,
  elem2 = 2,
  elem3 = "c",
  elem4 = c(0, 2, 4, 6, 8),
  elem5 = TRUE,
  elem6 = list(elem1 = "a", elem2 = "b"),
  elem7 = dplyr::starwars
)
lista
str(lista)

df <- list(
  col1 = c(1, 2, 3),
  col2 = c("a", "b", "c"),
  col3 = c(TRUE, TRUE, FALSE)
)

# Todas as colunas têm que ter o mesmo número de elems.
# Colunas podem ter tipos diferentes

df$col1
str(df)

# |1(num)-------|2(chr)--------|3(lgl)----|4(lst)--------------------|
# |100, 200, 20 |"M", "P", "f" |TRUE      |1|1(barbie)---|2(ken)-----|
#                                           |"barbie"----|"ken"------|

rua <- list(
  num = c(100, 200, 20),
  chr = c("M", "P", "f"),
  lgl = TRUE,
  lst = list(
    barbie = "barbie silva",
    ken = "ken souza"
  )
)

# Casa (sub-lista)
rua[1:2]
str(rua[1:2])

rua[c(1, 3)]
str(rua[c(1, 3)])

# Integrantes da casa (elementos)
rua[[1]]
rua$num
rua[["num"]]

# Também serve pra data frame
dplyr::starwars[[3]]
dplyr::starwars$mass
dplyr::starwars[["mass"]]

# Indexação profunda
# rua -> casa 4 (lst) -> rua de bonecas -> casa 1 (barbie) -> "barbie silva"
rua[["lst"]][["barbie"]][1]

rua[[4]][1] -> rua[[4]][[1]]

rua[4][[1]] -> rua[[4]][[1]]

rua[["lst"]][1] -> rua[["lst"]][[1]]

rua[[lst$"barbie"]] -> rua[["lst"]]$barbie

rua$barbie[[1]] -> rua$lst$barbie

casa <- 1
rua$casa
rua[[casa]]

casa <- "chr"
rua[[casa]]

# Primeira função do purrr
pluck(rua, "lst", "barbie")
pluck(rua, 4, 1)

# Loop
n_pessoas <- list()
for (i in 1:length(rua)) {
  n_pessoas[[i]] <- length(rua[[i]])
}
n_pessoas

# Mais simples
map(rua, length)

# Efeito colateral
n_pessoas <- list()
for (i in 1:length(rua)) {
  n_pessoas[[i]] <- length(rua[[i]])
  rua[[i]] <- "erro" # Esqueci que mexo em `rua`
}
n_pessoas
rua

rua <- list(
  num = c(100, 200, 20),
  chr = c("M", "P", "f"),
  lgl = TRUE,
  lst = list(
    barbie = "barbie silva",
    ken = "ken souza"
  )
)

f_com_efeito <- function(casa) {
  saida <- length(casa)
  casa <- "erro" # Efeito colateral
  return(saida)
}

# Sem efeito colateral!
map(rua, f_com_efeito)
rua

# Achatando
map_dbl(rua, length)
map_chr(rua, length)

# Selecionando entradas
map_dbl(rua[1:2], length)
map_dbl(rua[[4]], length)
map_dbl(1:100, length) # Funciona com vetores também!

# Com predicado
comprimento_maior_que_1 <- function(vetor) {
  length(vetor) > 1
}
map_if(list(1:3, 4, 5, 6), comprimento_maior_que_1, length)

# Com lambda
map_if(list(1:3, 4, 5, 6), ~length(.x) > 1, length)

# E com 2 entradas?
nums <- list(
  pares = c(0, 2, 4, 6, 8),
  impares = c(1, 3, 5, 7, 9)
)
novos_elems <- c(10, 11)

# Chamamos essa operação de zip
map2(nums, novos_elems, c)

# Explicando lambda
soma_3 <- function(num) {
  num + 3
}
map_dbl(1:10, soma_3)

soma_3 <- function(num) { num + 3 }
map_dbl(1:10, soma_3)

soma_3 <- function(num) num + 3
map_dbl(1:10, soma_3)

map_dbl(1:10, function(num) num + 3)

map_dbl(1:10, function(.x) .x + 3)

# Se estiver usando uma função in-line (anônima)
# Se ela tiver só 1 argumento
# Se esse argumento se chamar .x
map_dbl(1:10, ~.x + 3)

# Se eu tiver 2 argumentos, vira .x e .y
map2_dbl(1:10, 11:20, ~.x + .y)

# Se eu tiver 3 vira... ..1, ..2, ..3
pmap(list(arg1 = 1:10, arg2 = 11:20, arg3 = 21:30), ~..1 + ..2 + ..3)

# Argumentos fixos
map(1:5, ~runif(.x, min = -1)) # -> function(val) runif(val, min = -1)
map(1:5, runif, min = -1)
