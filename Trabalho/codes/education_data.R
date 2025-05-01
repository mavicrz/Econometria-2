# Censo Escolar-------------------------------------
# Esto código cria um painel com dados de matrícula
# do 6 ano (5 série) até 9 ano (8 série) para 1995 até 2013

# 0. Configuração---------------------------------------

rm(list=ls())
gc()

# Bibliotecas
xfun::pkg_attach(c('tidyverse','purrr','vroom', 'stringr'), install=T)

# Input
files <- list.files(path = 'input/censo_escolar/', pattern = '*.CSV', full.names = T)
files_2007_2013 <- list.files(path = 'input/censo_escolar', pattern='^microdados_ed_basica_', full.names=T)
files_matriculas <- list.files(path = 'output/censo_escolar/', pattern = '^matriculas_', full.names = T)
files_abandono <- list.files(path = 'output/censo_escolar/', full.names = T, pattern = '^abandono_')
files_aprov_reprov <- list.files(path = 'output/censo_escolar/', full.names = T, pattern = '^aprov_reprov_')
files_xls <- list.files(path = 'input/censo_escolar', pattern = '*.xls', full.names = T)

# 1. 1995 ------------------------------------------------------------------------------
# Variáveis Matrícula
# VEF2342 até VEF2349 matrículas na 5 série
# VEF2350 até VEF2356 matrículas na 6 série
# VEF2357 até VEF2362 matrículas na 7 série
# VEF2363 até VEF2367 matrículas na 8 série

censo_1995 <- vroom::vroom(files[1], delim = '|') %>% 
  dplyr::select(codmun = CO_IBGE, uf = SIGLA, mun = MUNIC, VEF2342,	VEF2343,	VEF2344,	VEF2345,	VEF2346,	VEF2347,
                VEF2348,	VEF2349,	VEF2350,	VEF2351,	VEF2352,	VEF2353,	VEF2354,
                VEF2355,	VEF2356,	VEF2357,	VEF2358,	VEF2359,	VEF2360,	VEF2361,	VEF2362,
                VEF2363,	VEF2364,	VEF2365,	VEF2366,	VEF2367, rede =DEP, VEM2101,	VEM2102,	VEM2103,
                VEM2104,	VEM2105,	VEM2106,	VEM2107,	VEM2108,	VEM2109,	VEM2110,	VEM2111,	VEM2112,
                VEM2113,	VEM2114,	VEM2115,	VEM2116,	VEM2117,	VEM2118,	VEM2119,	VEM2120,	VEM2121,
                VEM2122,	VEM2123,	VEM2124,	VEM2125,	VEM2126,	VEM2127,	VEM2128,	VEM2129,	VEM2130) %>% 
  tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun','rede'), 
                      names_to = 'serie', 
                      values_to = 'matriculas') %>% 
  dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                        stringr::str_sub(string = codmun,start = 8,end = 12)),
                mun = stringr::str_to_lower(mun),
                serie = case_when(serie %in% c('VEF2342',	'VEF2343',	'VEF2344',
                                               'VEF2345',	'VEF2346',	'VEF2347',
                                               'VEF2348',	'VEF2349') ~ 'EF_6ano',
                                  serie %in% c('VEF2350',	'VEF2351',	'VEF2352',
                                               'VEF2353',	'VEF2354',	'VEF2355',	'VEF2356') ~ 'EF_7ano',
                                  serie %in% c('VEF2357',	'VEF2358',	'VEF2359',
                                               'VEF2360',	'VEF2361',	'VEF2362') ~ 'EF_8ano',
                                  serie %in% c('VEF2363',	'VEF2364',	'VEF2365',
                                               'VEF2366',	'VEF2367') ~ 'EF_9ano',
                                  serie %in% c('VEM2101',	'VEM2102',	'VEM2103',
                                               'VEM2104',	'VEM2105',	'VEM2106',
                                               'VEM2107',	'VEM2108',	'VEM2109',
                                               'VEM2110',	'VEM2111',	'VEM2112',
                                               'VEM2113',	'VEM2114',	'VEM2115',
                                               'VEM2116',	'VEM2117',	'VEM2118',
                                               'VEM2119',	'VEM2120',	'VEM2121',
                                               'VEM2122',	'VEM2123',	'VEM2124',
                                               'VEM2125',	'VEM2126',	'VEM2127',
                                               'VEM2128',	'VEM2129',	'VEM2130') ~ 'EM'),
                ano = 1995) %>% 
  dplyr::filter(rede != 'Privada') %>% 
  dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
  dplyr::summarise(matriculas = sum(matriculas,na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = 'serie',names_prefix = 'matriculas_', values_from = matriculas)

readr::write_csv(censo_1995,file = 'output/censo_escolar/matriculas_1995.csv')
# 2. 1996 ----------------------------------------------------------------------
# Variáveis Matrícula
# VEF415 até VEF425 matrículas na 5 série
# VEF416 até VEF426 matrículas na 6 série
# VEF417 até VEF427 matrículas na 7 série
# VEF418 até VEF428 matrículas na 8 série

matriculas_1996 <- vroom::vroom(files[2], delim = '|', col_select = c(CODMUNIC, DEP, SIGLA, MUNIC, VEF415,	VEF416,
                           VEF417,	VEF418, VEF425,	VEF426,	VEF427,	VEF428)) %>% 
  dplyr::rename(codmun = CODMUNIC, uf =SIGLA, mun = MUNIC, rede =DEP) %>% 
  tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun','rede'), 
                      names_to = 'serie', 
                      values_to = 'matriculas') %>% 
  dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                        stringr::str_sub(string = codmun,start = 8,end = 12)),
                mun = stringr::str_to_lower(mun),
                serie = case_when(serie %in% c('VEF415',	'VEF425') ~ 'EF_6ano',
                                  serie %in% c('VEF416',	'VEF426') ~ 'EF_7ano',
                                  serie %in% c('VEF417',	'VEF427') ~ 'EF_8ano',
                                  serie %in% c('VEF418',	'VEF428') ~ 'EF_9ano'),
                ano = 1996) %>%
  dplyr::filter(rede != 'Privada') %>% 
  dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
  dplyr::summarise(matriculas = sum(matriculas,na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = 'serie',names_prefix = 'matriculas_', values_from = matriculas)

readr::write_csv(matriculas_1996,file = 'output/censo_escolar/matriculas_1996.csv')

abandono_1995 <- vroom::vroom(files[2], delim = '|', col_select = c(CODMUNIC,SIGLA, MUNIC, 
                                                                    DEP, VEF845, VEF846,VEF847,VEF848,
                                                                    'VEM941',	'VEM942',	'VEM943',	'VEM944',
                                                                    'VEM945',	'VEM946',	'VEM947',	'VEM948')) %>% 
  dplyr::rename(codmun = CODMUNIC, uf =SIGLA, mun = MUNIC, rede =DEP) %>% 
  tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun', 'rede'), 
                      names_to = 'serie', 
                      values_to = 'abandono') %>% 
  dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                        stringr::str_sub(string = codmun,start = 8,end = 12)),
                mun = stringr::str_to_lower(mun),
                serie = case_when(serie == 'VEF845' ~ 'EF_6ano',
                                  serie == 'VEF846' ~ 'EF_7ano',
                                  serie == 'VEF847' ~ 'EF_8ano',
                                  serie == 'VEF848' ~ 'EF_9ano',
                                  serie %in% c('VEM941',	'VEM942',	'VEM943',	'VEM944',
                                               'VEM945',	'VEM946',	'VEM947',	'VEM948') ~ 'EM'),
                ano = 1995) %>% 
  dplyr::filter(rede != 'Privada') %>% 
  dplyr::group_by(ano,codmun,mun,uf,serie) %>%
  dplyr::summarise(abandono = sum(abandono,na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = 'serie',names_prefix = 'abandono_', values_from = abandono)

readr::write_csv(abandono_1995,file = 'output/censo_escolar/abandono_1995.csv')


aprov_reprov_1995 <- vroom::vroom(files[2], delim = '|', col_select = c(CODMUNIC,SIGLA, DEP, MUNIC, VEF855, VEF856,VEF857,VEF858,
                                                                        VEF865, VEF866,VEF867,VEF868,
                                                                        'VEM951',	'VEM952',	'VEM953',	'VEM954',	'VEM955',	'VEM956',	
                                                                        'VEM957',	'VEM958',	'VEM961',	'VEM962',	'VEM963',	'VEM964',
                                                                        'VEM965',	'VEM966',	'VEM967',	'VEM968')) %>% 
  dplyr::rename(codmun = CODMUNIC, uf =SIGLA, mun = MUNIC, rede = DEP) %>% 
  tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun','rede'), 
                      names_to = 'serie', 
                      values_to = 'aprov_reprov') %>% 
  dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                        stringr::str_sub(string = codmun,start = 8,end = 12)),
                mun = stringr::str_to_lower(mun),
                serie = case_when(serie %in% c('VEF855','VEF865') ~ 'EF_6ano',
                                  serie %in% c('VEF856','VEF866') ~ 'EF_7ano',
                                  serie %in% c('VEF857','VEF867') ~ 'EF_8ano',
                                  serie %in% c('VEF858','VEF868') ~ 'EF_9ano',
                                  serie %in% c('VEM951',	'VEM952',	'VEM953',	'VEM954',	'VEM955',	'VEM956',	
                                               'VEM957',	'VEM958',	'VEM961',	'VEM962',	'VEM963',	'VEM964',
                                               'VEM965',	'VEM966',	'VEM967',	'VEM968') ~'EM'),
                ano = 1995) %>% 
  dplyr::filter(rede != 'Privada') %>% 
  dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
  dplyr::summarise(aprov_reprov = sum(aprov_reprov,na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = 'serie',names_prefix = 'aprov_reprov_', values_from = aprov_reprov)

readr::write_csv(aprov_reprov_1995,file = 'output/censo_escolar/aprov_reprov_1995.csv')



# 3. 1997 ----------------------------------------------------------------------
# Variáveis Matrícula
# VEF225 matrículas na 5 série
# VEF226 matrículas na 6 série
# VEF227 matrículas na 7 série
# VEF228 matrículas na 8 série

matriculas_1997 <- vroom::vroom(files[3], delim = '|', col_select = c(CODMUNIC,SIGLA,DEP, MUNIC,
                                                                      VEF225, VEF226, VEF227, VEF228)) %>% 
  dplyr::rename(codmun = CODMUNIC, uf =SIGLA, mun = MUNIC, rede= DEP) %>% 
  tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun', 'rede'), 
                      names_to = 'serie', 
                      values_to = 'matriculas') %>% 
  dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                        stringr::str_sub(string = codmun,start = 8,end = 12)),
                mun = stringr::str_to_lower(mun),
                serie = case_when(serie == 'VEF225' ~ 'EF_6ano',
                                  serie == 'VEF226' ~ 'EF_7ano',
                                  serie == 'VEF227' ~ 'EF_8ano',
                                  serie == 'VEF228' ~ 'EF_9ano'),
                ano = 1997) %>%
  dplyr::filter(rede != 'Privada') %>% 
  dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
  dplyr::summarise(matriculas = sum(matriculas,na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = 'serie',names_prefix = 'matriculas_', values_from = matriculas)

readr::write_csv(matriculas_1997,file = 'output/censo_escolar/matriculas_1997.csv')

abandono_1996 <- vroom::vroom(files[3], delim = '|', col_select = c(CODMUNIC,SIGLA, MUNIC, DEP,
                                                                    VEF845, VEF846, VEF847, VEF848,
                                                                    'VEM971',	'VEM972',	'VEM973',	'VEM974')) %>% 
  dplyr::rename(codmun = CODMUNIC, uf =SIGLA, mun = MUNIC,rede =DEP) %>% 
  tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun','rede'), 
                      names_to = 'serie', 
                      values_to = 'abandono') %>% 
  dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                        stringr::str_sub(string = codmun,start = 8,end = 12)),
                mun = stringr::str_to_lower(mun),
                serie = case_when(serie == 'VEF845' ~ 'EF_6ano',
                                  serie == 'VEF846' ~ 'EF_7ano',
                                  serie == 'VEF847' ~ 'EF_8ano',
                                  serie == 'VEF848' ~ 'EF_9ano',
                                  serie %in% c('VEM971',	'VEM972',	'VEM973',	'VEM974') ~ 'EM'),
                ano = 1996) %>%
  dplyr::filter(rede != 'Privada') %>% 
  dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
  dplyr::summarise(abandono = sum(abandono,na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = 'serie',names_prefix = 'abandono_', values_from = abandono)

readr::write_csv(abandono_1996,file = 'output/censo_escolar/abandono_1996.csv')

aprov_reprov_1996 <- vroom::vroom(files[3], delim = '|', col_select = c(CODMUNIC,SIGLA, MUNIC, VEF855, VEF856,VEF857,VEF858,
                                                                        VEF865, VEF866,VEF867,VEF868, DEP,
                                                                        'VEM9A1',	'VEM9A2',	'VEM9A3',	'VEM9A4',	'VEM9B1',	'VEM9B2',	'VEM9B3',	'VEM9B4')) %>% 
  dplyr::rename(codmun = CODMUNIC, uf =SIGLA, mun = MUNIC, rede = DEP) %>% 
  tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun','rede'), 
                      names_to = 'serie', 
                      values_to = 'aprov_reprov') %>% 
  dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                        stringr::str_sub(string = codmun,start = 8,end = 12)),
                mun = stringr::str_to_lower(mun),
                serie = case_when(serie %in% c('VEF855','VEF865') ~ 'EF_6ano',
                                  serie %in% c('VEF856','VEF866') ~ 'EF_7ano',
                                  serie %in% c('VEF857','VEF867') ~ 'EF_8ano',
                                  serie %in% c('VEF858','VEF868') ~ 'EF_9ano',
                                  serie %in% c('VEM9A1',	'VEM9A2',	'VEM9A3',
                                               'VEM9A4',	'VEM9B1',	'VEM9B2',
                                               'VEM9B3',	'VEM9B4') ~ 'EM'),
                ano = 1996) %>% 
  dplyr::filter(rede != 'Privada') %>% 
  dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
  dplyr::summarise(aprov_reprov = sum(aprov_reprov,na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = 'serie',names_prefix = 'aprov_reprov_', values_from = aprov_reprov)

readr::write_csv(aprov_reprov_1996,file = 'output/censo_escolar/aprov_reprov_1996.csv')


# 4. 1998 e 1999 ------------------------------------------------------------------------------
# Variáveis Matrícula
# VEF415 até VEF425 matrículas na 5 série
# VEF416 até VEF426 matrículas na 6 série
# VEF417 até VEF427 matrículas na 7 série
# VEF418 até VEF428 matrículas na 8 série

matriculas_1998_1999 <- purrr::map(.x = c(4,5), .f = ~ vroom::vroom(files[.x],
                                                               col_select = c(ANO,CODMUNIC,SIGLA, DEP,
                                                                              MUNIC, VEF845, VEF846, VEF847, VEF848)) %>% 
                                dplyr::rename(codmun = CODMUNIC, rede=DEP, uf =SIGLA, mun = MUNIC, ano=ANO)) %>%
  purrr::reduce(bind_rows) %>% 
  tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun','ano','rede'), 
                      names_to = 'serie', 
                      values_to = 'matriculas') %>% 
  dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                        stringr::str_sub(string = codmun,start = 8,end = 12)),
                mun = stringr::str_to_lower(mun),
                serie = case_when(serie == 'VEF845' ~ 'EF_6ano',
                                  serie == 'VEF846' ~ 'EF_7ano',
                                  serie == 'VEF847' ~ 'EF_8ano',
                                  serie == 'VEF848' ~ 'EF_9ano')) %>%
  dplyr::filter(rede != 'Privada') %>% 
  dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
  dplyr::summarise(matriculas = sum(matriculas,na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = 'serie',names_prefix = 'matriculas_', values_from = matriculas)

readr::write_csv(matriculas_1998_1999,file = 'output/censo_escolar/matriculas_1998_1999.csv')


abandono_1997_1998 <- purrr::map(.x = c(4,5), .f = ~ vroom::vroom(files[.x],
                                                                  col_select = c(ANO,CODMUNIC,SIGLA,  DEP,
                                                                                 MUNIC, VEF845, VEF846, VEF847, VEF848,
                                                                                 'VEM941',	'VEM942',	'VEM943',	'VEM944',	
                                                                                 'VEM945',	'VEM946',	'VEM947',	'VEM948',)) %>% 
                                   dplyr::rename(codmun = CODMUNIC, uf =SIGLA, mun = MUNIC, ano=ANO, rede = DEP)%>% 
                                   tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun','ano', 'rede'), 
                                                       names_to = 'serie', 
                                                       values_to = 'abandono') %>% 
                                   dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                                                         stringr::str_sub(string = codmun,start = 8,end = 12)),
                                                 mun = stringr::str_to_lower(mun),
                                                 serie = case_when(serie == 'VEF845' ~ 'EF_6ano',
                                                                   serie == 'VEF846' ~ 'EF_7ano',
                                                                   serie == 'VEF847' ~ 'EF_8ano',
                                                                   serie == 'VEF848' ~ 'EF_9ano',
                                                                   serie %in% c('VEM941',	'VEM942',	'VEM943',
                                                                                'VEM944',	'VEM945',	'VEM946',
                                                                                'VEM947',	'VEM948') ~ 'EM'),
                                                 ano = ano -1) %>% 
                                   dplyr::filter(rede != 'Privada') %>% 
                                   dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
                                   dplyr::summarise(abandono = sum(abandono,na.rm = T)) %>% 
                                   tidyr::pivot_wider(names_from = 'serie',names_prefix = 'abandono_', values_from = abandono)) %>%
  purrr::reduce(bind_rows) 

readr::write_csv(abandono_1997_1998,file = 'output/censo_escolar/abandono_1997_1998.csv')


aprov_reprov_1997_1998 <- purrr::map(.x = c(4,5), .f = ~ vroom::vroom(files[.x],
                                                                  col_select = c(ANO,CODMUNIC,SIGLA, DEP,
                                                                                 MUNIC, VEF855, VEF856,VEF857,VEF858,
                                                                                 VEF865, VEF866,VEF867,VEF868,
                                                                                 'VEM951',	'VEM952',	'VEM953',	'VEM954',
                                                                                 'VEM955',	'VEM956',	'VEM957',	'VEM958',
                                                                                 'VEM961',	'VEM962',	'VEM963',	'VEM964',
                                                                                 'VEM965',	'VEM966',	'VEM967',	'VEM968')) %>% 
                                   dplyr::rename(codmun = CODMUNIC, uf =SIGLA, mun = MUNIC, ano=ANO, rede =DEP) %>% 
                                     tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun','ano','rede'), 
                                                         names_to = 'serie', 
                                                         values_to = 'aprov_reprov') %>% 
                                     dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                                                           stringr::str_sub(string = codmun,start = 8,end = 12)),
                                                   mun = stringr::str_to_lower(mun),
                                                   serie = case_when(serie %in% c('VEF855','VEF865') ~ 'EF_6ano',
                                                                     serie %in% c('VEF856','VEF866') ~ 'EF_7ano',
                                                                     serie %in% c('VEF857','VEF867') ~ 'EF_8ano',
                                                                     serie %in% c('VEF858','VEF868') ~ 'EF_9ano',
                                                                     serie %in% c('VEM951',	'VEM952',	'VEM953',	'VEM954',
                                                                                   'VEM955',	'VEM956',	'VEM957',	'VEM958',
                                                                                   'VEM961',	'VEM962',	'VEM963',	'VEM964',
                                                                                   'VEM965',	'VEM966',	'VEM967',	'VEM968') ~ 'EM'),
                                                   ano = ano -1) %>% 
                                     dplyr::filter(rede != 'Privada') %>% 
                                     dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
                                     dplyr::summarise(aprov_reprov = sum(aprov_reprov,na.rm = T)) %>% 
                                     tidyr::pivot_wider(names_from = 'serie',names_prefix = 'aprov_reprov_', values_from = aprov_reprov)) %>%
  purrr::reduce(bind_rows)


readr::write_csv(aprov_reprov_1997_1998,file = 'output/censo_escolar/aprov_reprov_1997_1998.csv')

# 5. 2000 até 2003 -------------------------------------------------------------
matriculas_clean <- function(x){
  vroom::vroom(files[x],delim = '|',  col_select = c(CODMUNIC, ANO,SIGLA, MUNIC, DEP, VEF415,	VEF416,
                                                       VEF417,	VEF418, VEF425,	VEF426,	VEF427,	VEF428)) %>% 
    dplyr::rename(codmun = CODMUNIC, uf =SIGLA, mun = MUNIC, ano = ANO, rede =DEP) %>% 
    tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun','ano', 'rede'), 
                        names_to = 'serie', 
                        values_to = 'matriculas') %>% 
    dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                          stringr::str_sub(string = codmun,start = 8,end = 12)),
                  mun = stringr::str_to_lower(mun),
                  serie = case_when(serie %in% c('VEF415',	'VEF425') ~ 'EF_6ano',
                                    serie %in% c('VEF416',	'VEF426') ~ 'EF_7ano',
                                    serie %in% c('VEF417',	'VEF427') ~ 'EF_8ano',
                                    serie %in% c('VEF418',	'VEF428') ~ 'EF_9ano')) %>% 
    dplyr::filter(rede !='Privada') %>% 
    dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
    dplyr::summarise(matriculas = sum(matriculas,na.rm = T)) %>% 
    tidyr::pivot_wider(names_from = 'serie',names_prefix = 'matriculas_', values_from = matriculas)
}

matriculas_2000 <- matriculas_clean(x=6)
matriculas_2001 <- matriculas_clean(x=7)
matriculas_2002 <- matriculas_clean(x=8)
matriculas_2003 <- matriculas_clean(x=9)
                              
readr::write_csv(matriculas_2000,file = 'output/censo_escolar/matriculas_2000.csv')
readr::write_csv(matriculas_2001,file = 'output/censo_escolar/matriculas_2001.csv')
readr::write_csv(matriculas_2002,file = 'output/censo_escolar/matriculas_2002.csv')
readr::write_csv(matriculas_2003,file = 'output/censo_escolar/matriculas_2003.csv')

# 6. 2004 a 2006 ----------------------------------------------------------------------

matriculas_2004 <- data.table::fread(file = files[10],sep = '|',  select = c('CODMUNIC', 'ANO','SIGLA', 'MUNIC', 'DEP',
                                                                       'DEF11G', 'DEF11H', 'DEF11I', 'DEF11J',
                                                                       'NEF11G', 'NEF11H','NEF11I','NEF11J')) %>% 
  dplyr::rename(codmun = CODMUNIC, uf =SIGLA, mun = MUNIC, ano = ANO, rede = DEP) %>% 
  tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun','ano', 'rede'), 
                      names_to = 'serie', 
                      values_to = 'matriculas') %>% 
  dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                        stringr::str_sub(string = codmun,start = 8,end = 12)),
                mun = stringr::str_to_lower(mun),
                serie = case_when(serie %in% c('DEF11G',	'NEF11G') ~ 'EF_6ano',
                                  serie %in% c('DEF11H',	'NEF11H') ~ 'EF_7ano',
                                  serie %in% c('DEF11I',	'NEF11I') ~ 'EF_8ano',
                                  serie %in% c('DEF11J',	'NEF11J') ~ 'EF_9ano'))%>%
  dplyr::filter(rede != 'Privada') %>% 
  dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
  dplyr::summarise(matriculas = sum(matriculas,na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = 'serie',names_prefix = 'matriculas_', values_from = matriculas)

readr::write_csv(matriculas_2004,file = 'output/censo_escolar/matriculas_2004.csv')



matriculas_2005 <- data.table::fread(file = files[11],sep = '|', select = c('CODMUNIC', 'ANO','SIGLA', 'MUNIC',
                                                                            'DEF11G', 'DEF11H', 'DEF11I', 'DEF11J',
                                                                            'NEF11G', 'NEF11H','NEF11I','NEF11J','DEP')) %>% 
  dplyr::rename(codmun = CODMUNIC, uf =SIGLA, mun = MUNIC, ano = ANO, rede=DEP) %>% 
  tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun','ano','rede'), 
                      names_to = 'serie', 
                      values_to = 'matriculas') %>% 
  dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                        stringr::str_sub(string = codmun,start = 8,end = 12)),
                mun = stringr::str_to_lower(mun),
                serie = case_when(serie %in% c('DEF11G',	'NEF11G') ~ 'EF_6ano',
                                  serie %in% c('DEF11H',	'NEF11H') ~ 'EF_7ano',
                                  serie %in% c('DEF11I',	'NEF11I') ~ 'EF_8ano',
                                  serie %in% c('DEF11J',	'NEF11J') ~ 'EF_9ano')) %>% 
  dplyr::filter(rede != 'Privada') %>% 
  dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
  dplyr::summarise(matriculas = sum(matriculas,na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = 'serie',names_prefix = 'matriculas_', values_from = matriculas)

readr::write_csv(matriculas_2005,file = 'output/censo_escolar/matriculas_2005.csv')

matriculas_2006 <- data.table::fread(file = files[12],sep = '|', select = c('CODMUNIC', 'ANO','SIGLA', 'MUNIC','DEP',
                                                                            'DEF11G', 'DEF11H', 'DEF11I', 'DEF11J',
                                                                            'NEF11G', 'NEF11H','NEF11I','NEF11J')) %>% 
  dplyr::rename(codmun = CODMUNIC, uf =SIGLA, mun = MUNIC, ano = ANO, rede=DEP) %>% 
  tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun','ano','rede'), 
                      names_to = 'serie', 
                      values_to = 'matriculas') %>% 
  dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                        stringr::str_sub(string = codmun,start = 8,end = 12)),
                mun = stringr::str_to_lower(mun),
                serie = case_when(serie %in% c('DEF11G',	'NEF11G') ~ 'EF_6ano',
                                  serie %in% c('DEF11H',	'NEF11H') ~ 'EF_7ano',
                                  serie %in% c('DEF11I',	'NEF11I') ~ 'EF_8ano',
                                  serie %in% c('DEF11J',	'NEF11J') ~ 'EF_9ano')) %>% 
  dplyr::filter(rede != 'Privada') %>% 
  dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
  dplyr::summarise(matriculas = sum(matriculas,na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = 'serie',names_prefix = 'matriculas_', values_from = matriculas)


readr::write_csv(matriculas_2006,file = 'output/censo_escolar/matriculas_2006.csv')


abandono_2003 <- data.table::fread(file=files[10],
                                   select = c('ANO','CODMUNIC','SIGLA', 'DEP',
                                                  'MUNIC', 'DE9F829', 'NE9F82F', 'DE9F826', 'NE9F82G',
                                                  'DE9F827', 'NE9F82H', 'DE9F828', 'NE9F82I',
                                              'DCN721',	'DCN722',	'DCN723',	'DCN724','NCN725',	'NCN726',
                                              'NCN727',	'NCN728')) %>% 
  dplyr::rename(codmun = CODMUNIC, uf =SIGLA, mun = MUNIC, ano=ANO, rede = DEP)%>% 
  tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun','ano','rede'), 
                      names_to = 'serie', 
                      values_to = 'abandono') %>% 
  dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                        stringr::str_sub(string = codmun,start = 8,end = 12)),
                mun = stringr::str_to_lower(mun),
                serie = case_when(serie %in% c('DE9F826','NE9F82F') ~ 'EF_6ano',
                                  serie %in% c('DE9F827','NE9F82G') ~ 'EF_7ano',
                                  serie %in% c('DE9F828','NE9F82H') ~ 'EF_8ano',
                                  serie %in% c('DE9F829','NE9F82I') ~ 'EF_9ano',
                                  serie %in% c('DCN721',	'DCN722',
                                               'DCN723',	'DCN724',
                                               'NCN725',	'NCN726',
                                               'NCN727',	'NCN728') ~ 'EM'),
                ano = ano -1) %>% 
  dplyr::filter(rede != 'Privada') %>% 
  dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
  dplyr::summarise(abandono = sum(abandono,na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = 'serie',names_prefix = 'abandono_', values_from = abandono)

readr::write_csv(abandono_2003,file = 'output/censo_escolar/abandono_2003.csv')

abandono_2004 <- data.table::fread(file=files[11],
                                   select = c('ANO','CODMUNIC','SIGLA', 'MUNIC', 'DEP',
                                              'NE9F82F' ,'DE9F826','NE9F82G','DE9F827',
                                              'NE9F82H','DE9F828', 'NE9F82I','DE9F829',
                                              'DEM941',	'DEM942',	'DEM943',	'DEM944',
                                              'NEM945',	'NEM946',	'NEM947',	'NEM948')) %>% 
  dplyr::rename(codmun = CODMUNIC, uf =SIGLA, mun = MUNIC, ano=ANO, rede = DEP)%>% 
  tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun','ano','rede'), 
                      names_to = 'serie', 
                      values_to = 'abandono') %>% 
  dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                        stringr::str_sub(string = codmun,start = 8,end = 12)),
                mun = stringr::str_to_lower(mun),
                serie = case_when(serie %in% c('NE9F82F' ,'DE9F826') ~ 'EF_6ano',
                                  serie %in% c('NE9F82G','DE9F827') ~ 'EF_7ano',
                                  serie %in% c('NE9F82H','DE9F828') ~ 'EF_8ano',
                                  serie %in% c('NE9F82I','DE9F829') ~ 'EF_9ano',
                                  serie %in% c('DEM941',	'DEM942',	'DEM943',	'DEM944',
                                               'NEM945',	'NEM946',	'NEM947',	'NEM948')~'EM'),
                ano = ano -1) %>% 
  dplyr::filter(rede != 'Privada') %>% 
  dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
  dplyr::summarise(abandono = sum(abandono,na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = 'serie',names_prefix = 'abandono_', values_from = abandono)

readr::write_csv(abandono_2004,file = 'output/censo_escolar/abandono_2004.csv')


abandono_2005 <- data.table::fread(file=files[12],
                                   select = c('ANO','CODMUNIC','SIGLA', 'DEP',
                                              'MUNIC','DEF895' ,'NEF89D', 'DEF896','NEF89E',
                                              'DEF897','NEF89F', 'DEF898','NEF89G',
                                              'DEM941',	'DEM942',	'DEM943',	'DEM944',
                                              'NEM945',	'NEM946',	'NEM947',	'NEM948')) %>% 
  dplyr::rename(codmun = CODMUNIC, uf =SIGLA, mun = MUNIC, ano=ANO, rede = DEP)%>% 
  tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun','ano','rede'), 
                      names_to = 'serie', 
                      values_to = 'abandono') %>% 
  dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                        stringr::str_sub(string = codmun,start = 8,end = 12)),
                mun = stringr::str_to_lower(mun),
                serie = case_when(serie %in% c('DEF895' ,'NEF89D') ~ 'EF_6ano',
                                  serie %in% c('DEF896','NEF89E') ~ 'EF_7ano',
                                  serie %in% c('DEF897','NEF89F') ~ 'EF_8ano',
                                  serie %in% c('DEF898','NEF89G') ~ 'EF_9ano',
                                  serie %in% c('DEM941',	'DEM942',	
                                               'DEM943',	'DEM944',
                                               'NEM945',	'NEM946',
                                               'NEM947',	'NEM948') ~ 'EM'),
                ano = ano -1) %>% 
  dplyr::filter(rede != 'Privada') %>% 
  dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
  dplyr::summarise(abandono = sum(abandono,na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = 'serie',names_prefix = 'abandono_', values_from = abandono)

readr::write_csv(abandono_2005,file = 'output/censo_escolar/abandono_2005.csv')


aprov_reprov_2003 <- data.table::fread(file=files[10],
                                   select = c('ANO','CODMUNIC','SIGLA', 'DEP',
                                              'MUNIC',
                                              'DE9F846' , 'DE9F847','DE9F848', 'DE9F849',
                                              'DE9F856', 'DE9F857', 'DE9F858', 'DE9F859',
                                              'DE9F866', 'DE9F867', 'DE9F868', 'DE9F869',
                                              'NE9F84F', 'NE9F84G', 'NE9F84H', 'NE9F84I',
                                              'NE9F85F','NE9F85G', 'NE9F85H', 'NE9F85I',
                                              'NE9F86F', 'NE9F86G', 'NE9F86H', 'NE9F86I',
                                              'DEM961', 'DEM962','DEM963','DEM964',
                                              'DEM9D1', 'DEM9D2', 'DEM9D3', 'DEM9D4',
                                              'DEM9E1', 'DEM9E2', 'DEM9E3', 'DEM9E4',
                                              'NEM9D5', 'NEM9D6', 'NEM9D7', 'NEM9D8', 
                                              'NEM9E5', 'NEM9E6', 'NEM9E7', 'NEM9E8',
                                              'NEM965', 'NEM966', 'NEM967', 'NEM968'
                                              )) %>% 
  dplyr::rename(codmun = CODMUNIC, uf =SIGLA, mun = MUNIC, ano=ANO,rede=DEP)%>% 
  tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun','ano','rede'), 
                      names_to = 'serie',
                      values_to = 'aprov_reprov_') %>% 
  dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                        stringr::str_sub(string = codmun,start = 8,end = 12)),
                mun = stringr::str_to_lower(mun),
                serie = case_when(serie %in% c('DE9F846','DE9F856' ,'DE9F866','NE9F86F', 'NE9F85F','NE9F84F') ~ 'EF_6ano',
                                  serie %in% c('DE9F847','DE9F857','DE9F867','NE9F86G', 'NE9F85G','NE9F84G') ~ 'EF_7ano',
                                  serie %in% c('DE9F848','DE9F858','DE9F868','NE9F86H', 'NE9F85H','NE9F84H') ~ 'EF_8ano',
                                  serie %in% c('DE9F849','DE9F859','DE9F869','NE9F86I', 'NE9F85I','NE9F84I') ~ 'EF_9ano',
                                  serie %in% c('DEM961', 'DEM962','DEM963','DEM964',
                                               'DEM9D1', 'DEM9D2', 'DEM9D3', 'DEM9D4',
                                               'DEM9E1', 'DEM9E2', 'DEM9E3', 'DEM9E4',
                                               'NEM9D5', 'NEM9D6', 'NEM9D7', 'NEM9D8', 
                                               'NEM9E5', 'NEM9E6', 'NEM9E7', 'NEM9E8',
                                               'NEM965', 'NEM966', 'NEM967', 'NEM968') ~ 'EM'),
                ano = ano -1) %>% 
  dplyr::filter(rede != 'Privada') %>% 
  dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
  dplyr::summarise(aprov_reprov_ = sum(aprov_reprov_,na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = 'serie',names_prefix = 'aprov_reprov_', values_from = aprov_reprov_)

readr::write_csv(aprov_reprov_2003,file = 'output/censo_escolar/aprov_reprov_2003.csv')



aprov_reprov_2004 <- data.table::fread(file=files[11],
                                            select = c('ANO','CODMUNIC','SIGLA', 'DEP','MUNIC',
                                                       'DE9F846' ,'DE9F856','DE9F866', 'NE9F84F','NE9F85F', 'NE9F86F',
                                                       'DE9F847','DE9F857','DE9F867', 'NE9F84G', 'NE9F85G', 'NE9F86G',
                                                       'DE9F848','DE9F858','DE9F868', 'NE9F84H', 'NE9F85H', 'NE9F86H',
                                                       'DE9F849','DE9F859','DE9F869', 'NE9F84I', 'NE9F85I', 'NE9F86I',
                                                       'DEM9D1','DEM9D2','DEM9D3', 'DEM9D4',
                                                       'DEM9E1', 'DEM9E2', 'DEM9E3', 'DEM9E4',
                                                       'DEM961',	'DEM962',	'DEM963',	'DEM964',
                                                       'NEM9D5', 'NEM9D6', 'NEM9D7','NEM9D8',
                                                       'NEM9E5', 'NEM9E6','NEM9E7', 'NEM9E8',
                                                       'NEM965',	'NEM966',	'NEM967',	'NEM968')) %>% 
  dplyr::rename(codmun = CODMUNIC, uf =SIGLA, mun = MUNIC, ano=ANO, rede=DEP)%>% 
  tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun','ano','rede'), 
                      names_to = 'serie', 
                      values_to = 'aprov_reprov_') %>% 
  dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                        stringr::str_sub(string = codmun,start = 8,end = 12)),
                mun = stringr::str_to_lower(mun),
                serie = case_when(serie %in% c('DE9F846' ,'DE9F856','DE9F866', 'NE9F84F','NE9F85F', 'NE9F86F') ~ 'EF_6ano',
                                  serie %in% c('DE9F847','DE9F857','DE9F867', 'NE9F84G', 'NE9F85G', 'NE9F86G') ~ 'EF_7ano',
                                  serie %in% c('DE9F848','DE9F858','DE9F868', 'NE9F84H', 'NE9F85H', 'NE9F86H') ~ 'EF_8ano',
                                  serie %in% c('DE9F849','DE9F859','DE9F869', 'NE9F84I', 'NE9F85I', 'NE9F86I') ~ 'EF_9ano',
                                  serie %in% c('DEM9D1','DEM9D2','DEM9D3', 'DEM9D4',
                                               'DEM961',	'DEM962',	'DEM963',	'DEM964',
                                               'NEM9D5', 'NEM9D6', 'NEM9D7','NEM9D8',
                                               'NEM9E5', 'NEM9E6','NEM9E7', 'NEM9E8',
                                               'NEM965',	'NEM966',	'NEM967',	'NEM968',
                                               'DEM9E1', 'DEM9E2', 'DEM9E3', 'DEM9E4') ~ 'EM'),
                ano = ano -1) %>% 
  dplyr::filter(rede != 'Privada') %>% 
  dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
  dplyr::summarise(aprov_reprov_ = sum(aprov_reprov_,na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = 'serie',names_prefix = 'aprov_reprov_', values_from = aprov_reprov_)

readr::write_csv(aprov_reprov_2004,file = 'output/censo_escolar/aprov_reprov_2004.csv')



aprov_reprov_2005 <- data.table::fread(file=files[12],
                                       select = c('ANO','CODMUNIC','SIGLA', 'DEP','MUNIC',
                                                  'DEF8B5' ,'DEF8C5','DEF8D5', 'NEF8BD','NEF8CD', 'NEF8DD',
                                                  'DEF8B6','DEF8C6','DEF8D6', 'NEF8BE', 'NEF8CE', 'NEF8DE',
                                                  'DEF8B7','DEF8C7','DEF8D7', 'NEF8BF', 'NEF8CF', 'NEF8DF',
                                                  'DEF8B8','DEF8C8','DEF8D8', 'NEF8BG', 'NEF8CG', 'NEF8DG',
                                                  'DEM9D1','DEM9D2','DEM9D3', 'DEM9D4',
                                                  'DEM961',	'DEM962',	'DEM963',	'DEM964',
                                                  'NEM9D5', 'NEM9D6', 'NEM9D7','NEM9D8',
                                                  'NEM9E5', 'NEM9E6','NEM9E7', 'NEM9E8',
                                                  'NEM965',	'NEM966',	'NEM967',	'NEM968',
                                                  'DEM9E1', 'DEM9E2', 'DEM9E3', 'DEM9E4')) %>% 
  dplyr::rename(codmun = CODMUNIC, uf =SIGLA, mun = MUNIC, ano=ANO, rede=DEP)%>% 
  tidyr::pivot_longer(cols = !c('codmun', 'uf', 'mun','ano','rede'), 
                      names_to = 'serie', 
                      values_to = 'aprov_reprov_') %>% 
  dplyr::mutate(codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                        stringr::str_sub(string = codmun,start = 8,end = 12)),
                mun = stringr::str_to_lower(mun),
                serie = case_when(serie %in% c('DEF8B5' ,'DEF8C5','DEF8D5', 'NEF8BD','NEF8CD', 'NEF8DD') ~ 'EF_6ano',
                                  serie %in% c('DEF8B6','DEF8C6','DEF8D6', 'NEF8BE', 'NEF8CE', 'NEF8DE') ~ 'EF_7ano',
                                  serie %in% c('DEF8B7','DEF8C7','DEF8D7', 'NEF8BF', 'NEF8CF', 'NEF8DF') ~ 'EF_8ano',
                                  serie %in% c('DEF8B8','DEF8C8','DEF8D8', 'NEF8BG', 'NEF8CG', 'NEF8DG') ~ 'EF_9ano',
                                  serie %in% c('DEM9D1','DEM9D2','DEM9D3', 'DEM9D4',
                                               'DEM961',	'DEM962',	'DEM963',	'DEM964',
                                               'NEM9D5', 'NEM9D6', 'NEM9D7','NEM9D8',
                                               'NEM9E5', 'NEM9E6','NEM9E7', 'NEM9E8',
                                               'NEM965',	'NEM966',	'NEM967',	'NEM968',
                                               'DEM9E1', 'DEM9E2', 'DEM9E3', 'DEM9E4') ~ 'EM'),
                ano = ano -1) %>% 
  dplyr::filter(rede != 'Privada') %>% 
  dplyr::group_by(ano,codmun,mun,uf,serie) %>% 
  dplyr::summarise(aprov_reprov_ = sum(aprov_reprov_,na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = 'serie',names_prefix = 'aprov_reprov_', values_from = aprov_reprov_)

readr::write_csv(aprov_reprov_2005,file = 'output/censo_escolar/aprov_reprov_2005.csv')

# 7. 2007 a 2013 ------------------------------------------------------------------------------
# NU_ANO_CENSO
# SG_UF
# NO_MUNICIPIO
# CO_MUNICIPIO
# QT_MAT_FUND_AF

matriculas_2007_2013 <- purrr::map(.x = 1:7,
                                   .f = ~vroom::vroom(file = files_2007_2013[.x],
                                                      col_select = c(NU_ANO_CENSO, TP_DEPENDENCIA,SG_UF,
                                                                     NO_MUNICIPIO, CO_MUNICIPIO,
                                                                     QT_MAT_FUND_AF,QT_MAT_MED), locale = locale(encoding = 'latin1')) %>% 
                                     dplyr::rename(ano = NU_ANO_CENSO, uf = SG_UF, mun =NO_MUNICIPIO,rede=TP_DEPENDENCIA,
                                                   codmun = CO_MUNICIPIO, matriculas_6ano_9ano= QT_MAT_FUND_AF,
                                                   matriculas_EM = QT_MAT_MED) %>%
                                     dplyr::filter(rede != 'Privada') %>% 
                                     dplyr::mutate(mun = stringr::str_to_lower(mun)) %>% 
                                     dplyr::group_by(ano,mun,uf,codmun) %>% 
                                     dplyr::summarise(matriculas_6ano_9ano = sum(matriculas_6ano_9ano, na.rm = T),
                                                      matriculas_EM = sum(matriculas_EM,na.rm = T))) %>% 
  purrr::reduce(bind_rows)

readr::write_csv(matriculas_2007_2013,file = 'output/censo_escolar/matriculas_2007_2013.csv')


matriculas_1995_2013 <- purrr::map(.x = 1:12, 
                                   .f = ~vroom::vroom(file = files_matriculas[.x])) %>% 
  purrr::reduce(bind_rows) %>% 
  dplyr::select(-mun) %>% 
  dplyr::mutate(matriculas_6ano_9ano = matriculas_EF_6ano + matriculas_EF_7ano + matriculas_EF_8ano + matriculas_EF_9ano)


readr::write_csv(matriculas_1995_2013,file = 'output/censo_escolar/matriculas_1995_2013.csv')

# 8. Taxas de Abandono ---------------------------------------------------------
taxas_abandono_1999_2002 <- purrr::map(.x = 13:16,
                                       .f = ~vroom::vroom(file= files[.x], delim = '|',
                                                          col_select = c(ANO, CODMUNIC, SIGLA, DEP,
                                                                         IEF00091, IEF00096, IEF00097,
                                                                         IEF00098, IEF00099, IEM00047)) %>% 
                                         dplyr::rename(ano = ANO, codmun = CODMUNIC,
                                                       uf = SIGLA, rede =DEP) %>% 
                                         tidyr::pivot_longer(cols = !c('ano','codmun','uf','rede'),
                                                             names_to = 'serie', values_to = 'taxa_abandono') %>% 
                                         dplyr::mutate(serie = case_when(serie == 'IEF00091' ~ '6ano_9ano',
                                                                         serie == 'IEF00096' ~ 'EF_6ano',
                                                                         serie == 'IEF00097' ~ 'EF_7ano',
                                                                         serie == 'IEF00098' ~ 'EF_8ano',
                                                                         serie == 'IEF00099' ~ 'EF_9ano',
                                                                         serie == 'IEM00047' ~ 'EM'),
                                                       ano = ano-1,
                                                       codmun = stringr::str_c(stringr::str_sub(string = codmun,start = 1,end = 2),
                                                                               stringr::str_sub(string = codmun,start = 8,end = 12))) %>%
                                         dplyr::filter(rede != 'Privada') %>% 
                                         dplyr::group_by(ano,codmun,uf, serie) %>% 
                                         dplyr::summarise(taxa_abandono = mean(taxa_abandono, na.rm=T)) %>% 
                                         tidyr::pivot_wider(names_from = 'serie',
                                                            names_prefix = 'taxa_abandono_',
                                                            values_from = 'taxa_abandono')) %>% 
  purrr::reduce(bind_rows)

abandono_1995_2005 <- purrr::map(.x = 1:6,
                                 .f = ~vroom::vroom(file = files_abandono[.x])) %>% 
  purrr::reduce(bind_rows) %>% 
  dplyr::select(-mun)

aprov_reprov_1995_2005 <- purrr::map(.x = 1:6,
                                 .f = ~vroom::vroom(file = files_aprov_reprov[.x])) %>% 
  purrr::reduce(bind_rows) %>% 
  dplyr::select(-mun)

taxas_abandono_1995_2005 <- abandono_1995_2005 %>% 
  dplyr::inner_join(aprov_reprov_1995_2005, by = c('ano', 'codmun', 'uf')) %>% 
  dplyr::mutate(taxa_abandono_EF_6ano = abandono_EF_6ano/(abandono_EF_6ano+aprov_reprov_EF_6ano)*100,
                taxa_abandono_EF_7ano = abandono_EF_7ano/(abandono_EF_7ano+aprov_reprov_EF_7ano)*100,
                taxa_abandono_EF_8ano = abandono_EF_8ano/(abandono_EF_8ano+aprov_reprov_EF_8ano)*100,
                taxa_abandono_EF_9ano = abandono_EF_9ano/(abandono_EF_9ano+aprov_reprov_EF_9ano)*100,
                taxa_abandono_EM = abandono_EM/(abandono_EM+aprov_reprov_EM)*100,
                taxa_abandono_6ano_9ano = (abandono_EF_6ano+ abandono_EF_7ano + abandono_EF_8ano +abandono_EF_9ano)/((abandono_EF_6ano+aprov_reprov_EF_6ano)+(abandono_EF_7ano+aprov_reprov_EF_7ano)+(abandono_EF_8ano+aprov_reprov_EF_8ano)+(abandono_EF_9ano+aprov_reprov_EF_9ano))*100) %>%
  dplyr::select(ano, codmun, uf,taxa_abandono_EM, taxa_abandono_EF_6ano, taxa_abandono_EF_7ano,
                taxa_abandono_EF_8ano, taxa_abandono_EF_9ano, taxa_abandono_6ano_9ano) %>%
  dplyr::mutate(codmun = as.character(codmun)) %>% 
  dplyr::bind_rows(taxas_abandono_1999_2002)

taxas_abandono_2006_2009 <- purrr::map(.x= 3:6,.f =~readxl::read_excel(path = files_xls[.x], skip = 1) %>% 
  purrr::set_names(c('ano','uf', 'codmun', 'local', 'rede', 'EF1', 'EF2', 'EF3', 'EF4', 'EF5',
                     'taxa_abandono_EF_6ano', 'taxa_abandono_EF_7ano','taxa_abandono_EF_8ano' ,
                     'taxa_abandono_EF_9ano', 'EF14', 'taxa_abandono_6ano_9ano', 'EF','taxa_abandono_EM')) %>% 
  dplyr::filter(local == 'Total' & rede == 'Publico') %>% 
  dplyr::select('ano','uf', 'codmun',
                'taxa_abandono_EF_6ano', 'taxa_abandono_EF_7ano',
                'taxa_abandono_EF_8ano' ,'taxa_abandono_EF_9ano','taxa_abandono_6ano_9ano',
                'taxa_abandono_EM') %>% 
  naniar::replace_with_na_all(data = ., condition = ~.x == '--') %>% 
  dplyr::mutate(ano = ano -1, 
                codmun = as.character(codmun),
                across(.cols = !c('ano','uf', 'codmun'), ~as.double(.)))) %>% 
  purrr::reduce(bind_rows)


taxas_abandono_2010_2013 <- purrr::map(.x = c(7,8,1,2), .f = ~readxl::read_excel(path = files_xls[.x], skip = 1) %>% 
  purrr::set_names(c('ano','uf', 'codmun', 'local', 'rede', 'EF','EF14', 'taxa_abandono_6ano_9ano',
                     'EF1', 'EF2', 'EF3', 'EF4', 'EF5',
                     'taxa_abandono_EF_6ano', 'taxa_abandono_EF_7ano','taxa_abandono_EF_8ano' ,
                     'taxa_abandono_EF_9ano', 'taxa_abandono_EM')) %>% 
  dplyr::filter(local == 'Total' & rede == 'Publico') %>% 
  dplyr::select('ano','uf', 'codmun', 'taxa_abandono_EM',
                'taxa_abandono_EF_6ano', 'taxa_abandono_EF_7ano',
                'taxa_abandono_EF_8ano' ,'taxa_abandono_EF_9ano','taxa_abandono_6ano_9ano') %>% 
  naniar::replace_with_na_all(data = ., condition = ~.x == '--') %>% 
  dplyr::mutate(ano = ano -1, 
                codmun = as.character(codmun),
                across(.cols = !c('ano','uf', 'codmun'), ~as.double(.)))) %>% 
  purrr::reduce(bind_rows)

taxas_abandono_1995_2013 <- taxas_abandono_1995_2005 %>% 
  dplyr::bind_rows(taxas_abandono_2006_2009,taxas_abandono_2010_2013)

readr::write_csv(taxas_abandono_1995_2013,file = 'output/censo_escolar/taxas_abandono_1995_2013.csv')


# 9. Painel educação -----------------------------------------------------------
pop_files <- list.files(path = 'input', pattern = '^pop_', full.names = T)

pop_2012 <- readxl::read_xlsx(path = pop_files[1], skip = 5,col_names = F, 
                    col_types = rep("text", 19)) %>% 
  purrr::set_names('mun', '1995',	'1996',	'1997',	'1998',	'1999',
                   '2000',	'2001',	'2002',	'2003',	'2004',	'2005',
                   '2006',	'2007',	'2008',	'2009',	'2010',	'2011',	'2012') %>% 
  tidyr::pivot_longer(cols = !mun, names_to = 'ano', values_to = 'pop_10_14') %>%
  dplyr::filter(ano != 'Total',
                is.na(mun) == F) %>% 
  dplyr::mutate(mun = stringr::str_extract(mun, pattern = '[0-9]+')) %>% 
  naniar::replace_with_na_all(condition = ~.x == '-')

pop_2013 <- readxl::read_xlsx(path = pop_files[2], skip = 5, col_names = F, 
                         col_types =  rep("text", 3)) %>% 
  purrr::set_names('mun',	'pop_10_14', 'Total') %>% 
  dplyr::select(!'Total') %>%
  dplyr::mutate(mun = stringr::str_extract(mun, pattern = '[0-9]+'),
                ano = as.character(2013)) %>% 
  dplyr::filter(is.na(mun) == F) %>% 
  dplyr::mutate(mun = stringr::str_extract(mun, pattern = '[0-9]+')) %>% 
  naniar::replace_with_na_all(condition = ~.x == '-')

pop <- pop_2012 %>% bind_rows(pop_2013) %>% 
  dplyr::rename(codmun = mun) %>% 
  dplyr::mutate(ano = as.integer(ano))

matriculas <- read.csv(file = 'output/censo_escolar/matriculas_1995_2013.csv')
taxa_abandono <- read.csv(file = 'output/censo_escolar/taxas_abandono_1995_2013.csv')

painel_educ <- matriculas %>% full_join(taxa_abandono, by = c('ano', 'codmun', 'uf')) %>% 
  dplyr::mutate(codmun = stringr::str_sub(string = as.character(codmun), start = 1, end = 6)) %>% 
  full_join(pop, by = c('ano', 'codmun')) %>% 
  dplyr::mutate(across(.cols = !c('ano','uf', 'codmun'), ~as.double(.)),
    taxa_matriculas  = matriculas_6ano_9ano/pop_10_14*100)


readr::write_csv(painel_educ,file = 'output/censo_escolar/painel_educ.csv')
