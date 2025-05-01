# PNAD------------------------------------------------
# Este código cria um painel com dados de trabalho
# infantil para 1995 até 2013

# 0. Configuração---------------------------------------

rm(list=ls())
gc()

# Bibliotecas
xfun::pkg_attach(c('tidyverse','purrr','vroom', 'stringr'), install=T)

files_trab <- list.files(path = 'input/pnad', full.names = T, pattern = '^tx_')

# 1. Painel PNAD ---------------------------------------------------------------
taxa_trabalho_1995_2006 <- readr::read_csv2(file = files_trab[1], skip = 4,col_names = F,
                                        locale = locale(encoding = 'latin1'), n_max = 27) %>% 
  purrr::set_names(c('uf_name','1995','1996','1997','1998','1999','2001','2002',
                     '2003','2004','2005', '2006','total')) %>% 
  dplyr::filter(uf_name != 'Total') %>% 
  dplyr::select(-total) %>% 
  tidyr::pivot_longer(cols = !'uf_name',
                      names_to = 'year',
                      values_to = 'child_work_rate') %>% 
  naniar::replace_with_na_all(~.x=='*')

clean_pnad <- function(file_id, ano){
  readr::read_csv2(file = files_trab[file_id], 
                   skip = 4,col_names = F, col_select = 1:2,
                   locale = locale(encoding = 'latin1'),
                   n_max = 27) %>% 
    purrr::set_names(c('uf_name',ano)) %>% 
    dplyr::filter(uf_name != 'Total') %>% 
    tidyr::pivot_longer(cols = !'uf_name',
                        names_to = 'year',
                        values_to = 'child_work_rate') %>% 
    naniar::replace_with_na_all(~.x=='*') %>% 
    dplyr::mutate(child_work_rate = as.double(stringr::str_replace_all(string = child_work_rate,
                                                             pattern = ',',replacement = '.')))}

taxa_trabalho_2007_2013 <- purrr::map2(.x = 2:7, .y =c('2007', '2008',
                                                       '2009','2011',
                                                       '2012','2013'),
                                      .f = ~clean_pnad(file_id = .x, ano = .y)) %>% 
  purrr::reduce(bind_rows)

uf_ibge <- readxl::read_excel(path = 'input/uf_ibge.xlsx') %>% 
  dplyr::select(uf = "Código da UF", uf_name = "Estado")

taxa_trabalho_1995_2013 <- taxa_trabalho_1995_2006%>% bind_rows(taxa_trabalho_2007_2013) %>% 
  dplyr::left_join(uf_ibge, by = 'uf_name')

write.csv(taxa_trabalho_1995_2013,file = 'output/taxa_trabalho_1995_2013.csv')

taxa_trabalho_censo <- read_csv2(file = 'input/tx_trab_infantil_censo.csv', skip = 4, 
                                 locale= locale(encoding = 'latin1'), col_names = F, n_max = 27) %>% 
  purrr::set_names(c('uf_name','1991','2000','2010','total')) %>% 
  dplyr::filter(uf_name != 'Total') %>% 
  dplyr::select(-total) %>% 
  tidyr::pivot_longer(cols = !'uf_name',
                      names_to = 'year',
                      values_to = 'child_work_rate') %>% 
  dplyr::left_join(uf_ibge, by = 'uf_name')

write.csv(taxa_trabalho_censo,file = 'output/taxa_trabalho_censo.csv')


