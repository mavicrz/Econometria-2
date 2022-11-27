# Dados Mogno - Ariaster e Soares (2017) -------------------------------------
# Este código limpa os dados de exportação para observar a variação
# da exportação ilegal de mogno

# 0. Configuração --------------------------------------------------------------

rm(list=ls())
gc()

# Bibliotecas
xfun::pkg_attach(c('tidyverse','purrr', 'haven', 'tibble', 'naniar','fixest', 'readr'), install=T)

# Input
mogno_file <- 'input/base_mogno.dta'

# 1. Base Mogno ----------------------------------------------------------------
mogno <- haven::read_dta(mogno_file,encoding = 'latin1') %>% 
  tibble::as_tibble() %>% 
  dplyr::group_by(amc_code_1991_1997, year) %>% 
  dplyr::summarise(across(.cols = c('area','plant', 'heart', 'infecc', 'neop',
                                    'pop_und5', 'und5_mort', 'traff', 'suic', 'pol_deaths',
                                    'hom', 'pop', 'gdp', 'pop_male', 'pop_m_prime',
                                    'gdp_ag', 'hom_male', 'hom_m_prime', 'hom_m_sing',
                                    'hom_m_nothome', 'hom_m_firearm'),sum, na.rm = T),
                   across(.cols = c('uf', 'mmc_code', 'mahogx_state',
                                    'otherx_state', 'mahog_exp_pre'), mean, na.rm = T),
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
                                    T~ as.double(NA))) %>% 
  dplyr::group_by(code) %>% 
  dplyr::summarise(across(.cols = c('hom_base', 'area_base', 'lngdp_base',
                                    'gdpag_base', 'pol_base', 'und5_base',
                                    'infecc_base', 'heart_base', 'neop_base',
                                    'suic_base', 'traff_base', 'pop1995'), mean, na.rm=T),
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
                pre = case_when(mahog_area ==1 & year > 1996 & year < 1999 ~1, T~0))

readr::write_csv(mogno_reg, file = 'output/base_mogno_treated.csv')

# 2. Panel A: Para and Other Mahogany Occuring States Separately ---------------
# Resultado ficou igual quando a coeficientes e nível de significância, mas erro padrão diferente
reg_A1 <- mogno_reg %>% 
  dplyr::filter(uf ==15) %>% 
  dplyr::mutate(year = factor(year)) %>% 
  fixest::feols(fml = hom_tx ~ treat1 + treat2 + treat3 + year | code,
                weights = ~avg_pop, cluster = ~code, data = .)

# Resultado ficou diferente para coeficientes
reg_A2 <- mogno_reg %>% 
  dplyr::filter(uf ==15) %>% 
  dplyr::mutate(year = factor(year)) %>% 
  fixest::feols(fml = hom_tx ~ treat1 + treat2 + treat3 + year + i(year,hom_base) +
                  i(year,area_base) + i(year,lngdp_base) + i(year,gdpag_base) +
                  i(year, pol_base) + i(year,und5_base) + i(year, infecc_base)+
                  i(year, heart_base) + i(year, neop_base) + i(year, suic_base) + 
                  i(year, traff_base)| code,
                weights = ~avg_pop, cluster = ~code, data = .)



# 3. Panel B: Parallel trends, demographics ------------------------------------

# Resultados iguais para coeficientes e nível de significância
reg_B1 <- mogno_reg %>% 
  dplyr::filter(uf ==15) %>% 
  dplyr::mutate(year = factor(year)) %>% 
  fixest::feols(fml = hom_tx ~ treat1 + treat2 + treat3 + pre + year | code,
                weights = ~avg_pop, cluster = ~code, data = .)

# Resultados iguais exceto para ano 
# em que a magnitude dos coeficientes é diferente

reg_B2 <- mogno_reg %>% 
  dplyr::filter(uf ==15) %>% 
  dplyr::mutate(year = factor(year),
                code = factor(code)) %>% 
  fixest::feols(fml = hom_tx ~ treat1 + treat2 + treat3 + i(f= code, ref =179, trend) + i(year,2013) | code,
                weights = ~avg_pop, cluster = ~code, data = .)

# Resultados iguais para coeficientes e nível de significância
reg_B3 <- mogno_reg %>% 
  dplyr::filter(uf ==15 & year > 1995) %>% 
  dplyr::mutate(year = factor(year)) %>% 
  fixest::feols(fml = male_h ~ treat1 + treat2 + treat3 + i(year, 2013) | code,
                weights = ~avg_pop, cluster = ~code, data = .)

# Resultados iguais para coeficientes e nível de significância
reg_B4 <- mogno_reg %>% 
  dplyr::filter(uf ==15) %>% 
  dplyr::mutate(year = factor(year)) %>% 
  fixest::feols(fml = male_h_prime ~ treat1 + treat2 + treat3 + i(year, 2013) | code,
                weights = ~avg_pop, cluster = ~code, data = .)

# Resultados iguais para coeficientes e nível de significância
reg_B5 <- mogno_reg %>% 
  dplyr::filter(uf ==15) %>% 
  dplyr::mutate(year = factor(year)) %>% 
  fixest::feols(fml = male_h_sing ~ treat1 + treat2 + treat3 + i(year, 2013) | code,
                weights = ~avg_pop, cluster = ~code, data = .)

# Resultados iguais para coeficientes e nível de significância
reg_B6 <- mogno_reg %>% 
  dplyr::filter(uf ==15) %>% 
  dplyr::mutate(year = factor(year)) %>% 
  fixest::feols(fml = male_h_nothome ~ treat1 + treat2 + treat3 + i(year, 2013) | code,
                weights = ~avg_pop, cluster = ~code, data = .)

# Resultados iguais para coeficientes e nível de significância
reg_B7 <- mogno_reg %>% 
  dplyr::filter(uf ==15) %>% 
  dplyr::mutate(year = factor(year)) %>% 
  fixest::feols(fml = male_h_firearm ~ treat1 + treat2 + treat3 + i(year, 2013) | code,
                weights = ~avg_pop, cluster = ~code, data = .)


# 4. ------------------------------------------------------------------------------
