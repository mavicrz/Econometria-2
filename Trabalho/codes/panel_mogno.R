# Dados Mogno - Ariaster e Soares (2017) -------------------------------------
# Este código limpa os dados de exportação para observar a variação
# da exportação ilegal de mogno

# 0. Configuração---------------------------------------

rm(list=ls())
gc()

# Bibliotecas
xfun::pkg_attach(c('tidyverse','purrr', 'haven', 'tibble'), install=T)

# Input
mogno_file <- 'input/base_mogno.dta'

# 1. Base Mogno ----------------------------------------------------------------
mogno <- haven::read_dta(mogno_file,encoding = 'latin1') %>% 
  tibble::as_tibble()

# 2. ------------------------------------------------------------------------------
# 3. ------------------------------------------------------------------------------
# 4. ------------------------------------------------------------------------------
