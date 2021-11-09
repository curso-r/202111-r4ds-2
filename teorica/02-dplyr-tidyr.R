
library(dplyr)

# Demonstração do where()
starwars %>% 
  mutate(across(
    .cols = where(is.character),
    .fns  = as.factor
  ))

# Demonstração do where() com outro operador
starwars %>% 
  mutate(across(
    .cols = where(is.character) & !name,
    .fns  = as.factor
  ))


col <- c("a", "b", "c")
is.character(col)

col <- c(1, 2, 3, 1, 2, 5)
n_distinct(col)

# Principais operadores
&
|
!

# Versão dupla (não funciona para vetores)
&&
||

# Demonstração
(1 < 3) && (2 > 1)
c(1 > 1, 1 > 2, 1 > 3) & c(3 > 1, 3 > 2, 3> 3)

# Notação lambda ou notação de fórmula
col <- c(1, 2, 3, NA, 5, 6, NA)
f <- function(col) {
  sum(is.na(col))
}

f <- function(col) { sum(is.na(col)) }

f <- function(col) sum(is.na(col))

across(
  where(is.character),
  function(col) sum(is.na(col))
)

across(
  where(is.character),
  function(.x) sum(is.na(.x))
)

across(
  where(is.character),
  ~ sum(is.na(.x))
)


# Demonstração do pivot
id colX colY colZ
1  a    c    e
2  b    d    f

id tratamento temperatura
1  colX       a
1  colY       c
1  colZ       e
2  colX       b
2  colY       d
2  colZ       f






