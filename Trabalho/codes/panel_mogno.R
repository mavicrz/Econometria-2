# Dados Mogno - Ariaster e Soares (2017) -------------------------------------
# Este código limpa os dados de exportação para observar a variação
# da exportação ilegal de mogno

# 0. Configuração --------------------------------------------------------------

rm(list=ls())
gc()

# Bibliotecas
xfun::pkg_attach(c('tidyverse','purrr', 'haven', 'tibble', 'naniar'), install=T)

# Input
mogno_file <- 'input/base_mogno.dta'

# 1. Base Mogno ACM ----------------------------------------------------------------
painel_educ <- readr::read_csv(file = 'output/painel_educ.csv') %>% 
  dplyr::select(year =ano, code = codmun, drpt_rate = taxa_abandono_6ano_9ano,
                scl_rate =taxa_matriculas)

mogno <- haven::read_dta(mogno_file,encoding = 'latin1') %>% 
  tibble::as_tibble() %>% 
  dplyr::full_join(painel_educ, by = c('year', 'code')) %>% 
  dplyr::group_by(amc_code_1991_1997, year) %>% 
  dplyr::summarise(across(.cols = c('area','plant', 'heart', 'infecc', 'neop',
                                    'pop_und5', 'und5_mort', 'traff', 'suic', 'pol_deaths',
                                    'hom', 'pop', 'gdp', 'pop_male', 'pop_m_prime',
                                    'gdp_ag', 'hom_male', 'hom_m_prime', 'hom_m_sing',
                                    'hom_m_nothome', 'hom_m_firearm'),sum, na.rm = T),
                   across(.cols = c('uf', 'mmc_code', 'mahogx_state',
                                    'otherx_state', 'mahog_exp_pre', 'drpt_rate', 'scl_rate'), mean, na.rm = T),
                   across(.cols = mahog_area, max)) %>%
  dplyr::group_by(amc_code_1991_1997) %>% 
  dplyr::mutate(code = group_indices(), 
                code = case_when(amc_code_1991_1997 != '' ~ code -1, T~ as.double(NA)), # Tive que fazer uma gambiarra porque a função não é igual ao group do stata
                across(.cols = c('mahogx_state', 'otherx_state'), ~case_when(is.na(.)==T ~0, T~.)),
                total_mahogx = (otherx_state + mahogx_state)/1000000,
                gdp_pc = (gdp/pop),
                ln_gdp_pc = log(gdp_pc),
                gdpfr_ag = (gdp_ag/gdp),
                area_plant = (plant/(area*100)),
                ln_otherx_state = log(otherx_state),
                region_N = case_when(uf %in% c(12,16,13,15,11,14,17) ~ 1,
                                     T ~ 0),
                trend = year - 1995,
                ) %>% 
  dplyr::ungroup() %>% # Estes tratamentos são para as variáveis de outcome de criminalidade e mortalidade
  dplyr::mutate(across(.cols = c('hom_male', 'hom_m_prime',
                                 'hom_m_sing', 'hom_m_nothome', 'hom_m_firearm'), 
                       ~case_when(is.na(.) == T & year >1995 ~ 0, T~.)),
                hom = case_when(is.na(hom) ~ 0, T ~hom),
                across(.cols = c('heart', 'infecc', 'und5_mort',
                                 'neop', 'traff', 'suic', 'pol_deaths'), 
                       ~case_when(is.na(.) == T & year <2008 ~ 0, T~.)),
                hom_tx = (hom/pop)*100000,
                male_h = (hom_male/pop_male)*100000,
                male_h_prime = (hom_m_prime/pop_m_prime)*100000,
                male_h_nothome = (hom_m_nothome/pop_m_prime)*100000,
                male_h_sing = (hom_m_sing/pop_m_prime)*100000,
                male_h_firearm = (hom_m_firearm/pop_m_prime)*100000,
                und5_m = case_when(year<2008 ~(und5_mort/pop_und5)*1000, TRUE ~ as.double(NA)),
                heart_m = case_when(year <2008 ~(heart/pop)*1000, T ~ as.double(NA)),
                infecc_m = case_when(year<2008 ~(infecc/pop)*1000, T ~ as.double(NA)),
                neop_m = case_when(year<2008 ~ (neop/pop)*1000, T ~ as.double(NA)),
                suic_m = case_when(year<2008 ~ (suic/pop)*1000, T ~ as.double(NA)),
                traff_m = case_when(year<2008 ~ (traff/pop)*1000, T ~ as.double(NA)),
                pol_m = case_when(year<2008 ~ (pol_deaths/pop)*1000, T ~ as.double(NA)),
                pol_deaths_dummy = case_when(pol_deaths >0 & year <2008 ~ 1, 
                                             pol_deaths < 0 | pol_deaths ==0 & year <2008 ~ 0, 
                                             T ~ as.double(NA))) %>% 
  naniar::replace_with_na_all(data = ., condition = ~is.nan(.x)==T | is.infinite(.x)==T)


mogno_base_var <- mogno %>% 
  dplyr::mutate(hom_base = case_when(year == 1995 ~ hom_tx,
                                     T~ as.double(NA)),
                area_base = case_when(year == 1995 ~ area_plant,
                                      T~ as.double(NA)),
                lngdp_base = case_when(year == 1996 ~ ln_gdp_pc,
                                       T~ as.double(NA)),
                gdpag_base = case_when(year == 1996 ~ gdpfr_ag,
                                       T~ as.double(NA)),
                pol_base = case_when(year == 1995 ~ pol_m,
                                     T~ as.double(NA)),
                und5_base = case_when(year == 1995 ~ pol_m,
                                      T~ as.double(NA)),
                infecc_base = case_when(year == 1995 ~ infecc_m,
                                        T~ as.double(NA)),
                heart_base = case_when(year == 1995 ~ heart_m,
                                       T~ as.double(NA)),
                neop_base = case_when(year == 1995 ~ neop_m,
                                      T~ as.double(NA)),
                suic_base = case_when(year == 1995 ~ suic_m,
                                      T~ as.double(NA)),
                traff_base = case_when(year == 1995 ~ traff_m,
                                       T~ as.double(NA)),
                pop1995 = case_when(year == 1995 ~ pop,
                                    T~ as.double(NA)),
                drpt_rate_base = case_when(year == 1995 ~ drpt_rate,
                                           T~ as.double(NA)),
                scl_rate_base = case_when(year == 1995 ~ scl_rate,
                                           T~ as.double(NA))) %>% 
  dplyr::group_by(code) %>% 
  dplyr::summarise(across(.cols = c('hom_base', 'area_base', 'lngdp_base',
                                    'gdpag_base', 'pol_base', 'und5_base',
                                    'infecc_base', 'heart_base', 'neop_base',
                                    'suic_base', 'traff_base', 'pop1995',
                                    'drpt_rate_base', 'scl_rate_base'), mean, na.rm=T),
                   avg_pop = mean(pop))

mogno_reg <- mogno %>% 
  dplyr::left_join(mogno_base_var, by = 'code') %>% 
  naniar::replace_with_na_all(data = ., condition = ~is.nan(.x)==T | is.infinite(.x)==T) %>% 
  dplyr::arrange(code, year) %>% 
  dplyr::mutate(treat1 = case_when(mahog_area == 1 & year >= 1999 & year <= 2001 ~ 1, T~0),
                treat2 = case_when(mahog_area == 1 & year >= 2002 & year <= 2008 ~ 1, T~0),
                treat3 = case_when(mahog_area == 1 & year >=2009 ~ 1, T~0),
                treat_up = treat1 + treat2,
                treatx_area1 = case_when(mahog_area == 1 & year >= 1999 & year <= 2001 ~ mahog_exp_pre, 
                                         T ~0),
                treatx_area2 = case_when(mahog_area == 1 & year >= 2002 & year <= 2008 ~ mahog_exp_pre,
                                         T ~0),
                treatx_area3 = case_when(mahog_area ==1 & year >=2009 ~ mahog_exp_pre,
                                         T ~0),
                treatt_area1 = case_when(mahog_area == 1 &  year >= 1999 & year <= 2001 ~ total_mahogx,
                                        T ~0),
                treatt_area2 = case_when(mahog_area == 1 & year >= 2002 &year <= 2008 ~ total_mahogx,
                                         T ~0),
                treatt_area3 = case_when(mahog_area ==1 & year >=2009 ~ total_mahogx,
                                         T ~0),
                pre = case_when(mahog_area ==1 & year > 1996 & year < 1999 ~1, T~0),
                trend1 = case_when(year > 1998 & year < 2002 ~ year - 1999, T~ 0),
                trend2 = case_when(year > 2001 & year < 2009 ~ year - 2002, T~ 0),
                trend3 = case_when(year > 2008 ~ year - 2009, T~ 0),
                int1 = treat1*trend1,
                int2 = treat2*trend2,
                int3 = treat3*trend3)

readr::write_csv(mogno_reg, file = 'output/base_mogno_educ.csv')

# 2. Base regressões ------------------------------------------------------------------------------
mogno_2 <- haven::read_dta(mogno_file,encoding = 'latin1') %>% 
  tibble::as_tibble() %>%
  dplyr::mutate(pop = case_when(is.na(pop) == T ~ 0, T ~ pop),
                muni_exist_1995 = case_when(pop > 0 & year > 1995 ~ 1,T~0)) %>%
  dplyr::filter(muni_exist_1995 == 1) %>% 
  dplyr::mutate(across(.cols = c('mahogx_state', 'otherx_state'), ~case_when(is.na(.)==T ~0, T~.)),
                total_mahogx = (otherx_state + mahogx_state)/1000000,
                gdp_pc = (gdp/pop),
                ln_gdp_pc = log(gdp_pc),
                gdpfr_ag = (gdp_ag/gdp),
                area_plant = (plant/(area*100)),
                ln_otherx_state = log(otherx_state),
                trend = year - 1995,
  ) %>% # Estes tratamentos são para as variáveis de outcome de criminalidade e mortalidade
  dplyr::mutate(across(.cols = c('hom_male', 'hom_m_prime',
                                 'hom_m_sing', 'hom_m_nothome', 'hom_m_firearm'), 
                       ~case_when(is.na(.) == T & year >1995 ~ 0, T~.)),
                hom = case_when(is.na(hom) ~ 0, T ~hom),
                across(.cols = c('heart', 'infecc', 'und5_mort',
                                 'neop', 'traff', 'suic', 'pol_deaths'), 
                       ~case_when(is.na(.) == T & year <2008 ~ 0, T~.)),
                hom_tx = (hom/pop)*100000,
                male_h = (hom_male/pop_male)*100000,
                male_h_prime = (hom_m_prime/pop_m_prime)*100000,
                male_h_nothome = (hom_m_nothome/pop_m_prime)*100000,
                male_h_sing = (hom_m_sing/pop_m_prime)*100000,
                male_h_firearm = (hom_m_firearm/pop_m_prime)*100000,
                und5_m = case_when(year<2008 ~(und5_mort/pop_und5)*1000, TRUE ~ as.double(NA)),
                heart_m = case_when(year <2008 ~(heart/pop)*1000, T ~ as.double(NA)),
                infecc_m = case_when(year<2008 ~(infecc/pop)*1000, T ~ as.double(NA)),
                neop_m = case_when(year<2008 ~ (neop/pop)*1000, T ~ as.double(NA)),
                suic_m = case_when(year<2008 ~ (suic/pop)*1000, T ~ as.double(NA)),
                traff_m = case_when(year<2008 ~ (traff/pop)*1000, T ~ as.double(NA)),
                pol_m = case_when(year<2008 ~ (pol_deaths/pop)*1000, T ~ as.double(NA)),
                pol_deaths_dummy = case_when(pol_deaths >0 & year <2008 ~ 1, 
                                             pol_deaths < 0 | pol_deaths ==0 & year <2008 ~ 0, 
                                             T ~ as.double(NA))) %>% 
  naniar::replace_with_na_all(data = ., condition = ~is.nan(.x)==T | is.infinite(.x)==T)


mogno_base_var_2 <- mogno_2 %>% 
  dplyr::mutate(hom_base = case_when(year == 1995 ~ hom_tx,
                                     T~ as.double(NA)),
                area_base = case_when(year == 1995 ~ area_plant,
                                      T~ as.double(NA)),
                lngdp_base = case_when(year == 1996 ~ ln_gdp_pc,
                                       T~ as.double(NA)),
                gdpag_base = case_when(year == 1996 ~ gdpfr_ag,
                                       T~ as.double(NA)),
                pol_base = case_when(year == 1995 ~ pol_m,
                                     T~ as.double(NA)),
                und5_base = case_when(year == 1995 ~ und5_m,
                                      T~ as.double(NA)),
                infecc_base = case_when(year == 1995 ~ infecc_m,
                                        T~ as.double(NA)),
                heart_base = case_when(year == 1995 ~ heart_m,
                                       T~ as.double(NA)),
                neop_base = case_when(year == 1995 ~ neop_m,
                                      T~ as.double(NA)),
                suic_base = case_when(year == 1995 ~ suic_m,
                                      T~ as.double(NA)),
                traff_base = case_when(year == 1995 ~ traff_m,
                                       T~ as.double(NA)),
                pop1995 = case_when(year == 1995 ~ pop,
                                    T~ as.double(NA))) %>% 
  dplyr::group_by(code) %>% 
  dplyr::summarise(across(.cols = c('hom_base', 'area_base', 'lngdp_base',
                                    'gdpag_base', 'pol_base', 'und5_base',
                                    'infecc_base', 'heart_base', 'neop_base',
                                    'suic_base', 'traff_base', 'pop1995'), mean, na.rm=T),
                   avg_pop = mean(pop))

mogno_reg_2 <- mogno_2 %>% 
  dplyr::left_join(mogno_base_var_2, by = 'code') %>% 
  naniar::replace_with_na_all(data = ., condition = ~is.nan(.x)==T | is.infinite(.x)==T) %>% 
  dplyr::arrange(code, year) %>% 
  dplyr::mutate(treat1 = case_when(mahog_area == 1 & year >= 1999 & year <= 2001 ~ 1, T~0),
                treat2 = case_when(mahog_area == 1 & year >= 2002 & year <= 2008 ~ 1, T~0),
                treat3 = case_when(mahog_area == 1 & year >= 2009 ~ 1, T~0),
                treatx_area1 = case_when(mahog_area == 1 & year >= 1999 & year <= 2001 ~ mahog_exp_pre, 
                                         T ~0),
                treatx_area2 = case_when(mahog_area == 1 & year >= 2002 & year <= 2008 ~ mahog_exp_pre,
                                         T ~0),
                treatx_area3 = case_when(mahog_area ==1 & year >=2009 ~ mahog_exp_pre,
                                         T ~0),
                treatt_area1 = case_when(mahog_area == 1 &  year >= 1999 & year <= 2001 ~ total_mahogx,
                                         T ~0),
                treatt_area2 = case_when(mahog_area == 1 & year >= 2002 &year <= 2008 ~ total_mahogx,
                                         T ~0),
                treatt_area3 = case_when(mahog_area ==1 & year >=2009 ~ total_mahogx,
                                         T ~0),
                pre = case_when(mahog_area ==1 & year > 1996 & year < 1999 ~1, T~0),
                trend1 = case_when(year > 1998 & year < 2002 ~ year - 1999, T~ 0),
                trend2 = case_when(year > 2001 & year < 2009 ~ year - 2002, T~ 0),
                trend3 = case_when(year > 2008 ~ year - 2009, T~ 0),
                int1 = treat1*trend1,
                int2 = treat2*trend2,
                int3 = treat3*trend3)

readr::write_csv(mogno_reg_2, file = 'output/base_mogno_tables.csv')
haven::write_dta(mogno_reg_2, path = 'output/base_mogno_tables.dta')


base_tables <- haven::read_dta(file = 'output/base_mogno_tables_stata.dta',
                               encoding = 'latin1') %>%
  dplyr::full_join(painel_educ, by = c('year', 'code'))


