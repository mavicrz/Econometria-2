# Regressões Mogno - Ariaster e Soares (2017) -------------------------------------
# Este código cria as regressões e gráficos

# 0. Configuração --------------------------------------------------------------

rm(list=ls())
gc()

# Bibliotecas
xfun::pkg_attach(c('tidyverse','purrr', 'haven', 'tibble', 'fixest'), install=T)

# Input
painel_educ <- readr::read_csv(file = 'output/censo_escolar/taxas_abandono_1995_2013.csv') %>% 
  dplyr::select(year =ano, code = codmun, drpt_rate_EF = taxa_abandono_6ano_9ano,
                drpt_rate_EM=  taxa_abandono_EM) %>% 
  filter(!(year == 1995 & code == 150200 & is.na(drpt_rate_EM) & is.na(drpt_rate_EF))) %>% 
  dplyr::mutate(code = as.double(stringr::str_sub(string = as.character(code), start = 1, end = 6)))

pnad_trab <- readr::read_csv(file = 'output/taxa_trabalho_1995_2013.csv') %>% 
  dplyr::select(uf,child_work_rate,year)

censo_trab <- readr::read_csv(file = 'output/taxa_trabalho_censo.csv') %>% 
  dplyr::select(uf,child_work_rate,year) %>% 
  dplyr::bind_rows(pnad_trab)

base_tables <- haven::read_dta(file = 'output/base_mogno_tables_stata.dta',
                               encoding = 'latin1') %>% 
  inner_join(painel_educ, by = c('year', 'code')) %>% 
  left_join(censo_trab, by = c('year', 'uf'))


# 1. Análise Descritiva e Gráficos  --------------------------------------------
exp_taxa_EF <- base_tables %>%
  dplyr::mutate(otherx_state =otherx_state/1000000) %>% 
  dplyr::group_by(year)  %>%
  dplyr::summarise(across(.cols = c(drpt_rate_EF, otherx_state, drpt_rate_EM,child_work_rate), 
                          ~mean(., na.rm=T))) %>%
  tidyr::pivot_longer(cols = !year,
                      names_to = 'variable',
                      values_to = 'values') %>%
  dplyr::mutate(variable = case_when(variable == 'drpt_rate_EF' ~ 'Taxa de\nAbandono\nEF II',
                                     variable == 'otherx_state' ~ 'Exp. "Outras\nespécies\nmadeiras tropicais"',
                                     variable == 'drpt_rate_EM' ~ 'Taxa de\nAbandono\nEM',
                                     variable == 'child_work_rate' ~ 'Taxa de\nTrabalho\nInfantil')) %>% 
  ggplot2::ggplot(mapping = aes(x= year, y = values, color = variable))+
  geom_line(size= 1.2) +
  theme_minimal()+
  theme(legend.text = element_text(size = 14),legend.position = 'bottom',
        legend.title = element_text(size=16))+
  geom_line(size= 1) + scale_x_continuous(breaks = c(1995,1999,2001,2005,2009,2013)) +
  geom_vline(xintercept = c(1999,2001,2009),
             linetype = 'dashed', size = 1)+
  labs(x='', y='', color='Painel A')+
  scale_color_manual(values = c("#cf3a36", "#5c66a8","#FAD510",
                                "#0B775E", "#E2D200","#F8AFA8"))
ggsave(exp_taxa_EF,
       filename = 'output/tables/evol_exp_taxa_abandono_EF.png',
       width = 25, height = 15, device = 'png', bg = 'white',
       units = 'cm')

tx_abandono_EF <- painel_educ %>%
  dplyr::mutate(mahog = case_when(code %in% base_tables$code ~ 1, T~0)) %>% 
  dplyr::group_by(year,mahog)  %>%
  dplyr::summarise(across(.cols = c(drpt_rate_EF, drpt_rate_EM), 
                          ~mean(., na.rm=T))) %>%
  dplyr::mutate(mahog =  case_when(mahog == 1 ~ 'Município com\nocorrência de Mogno',
                                   mahog == 0 ~ 'Município sem\nocorrência de Mogno')) %>% 
  ggplot2::ggplot(mapping = aes(x= year, y = drpt_rate_EF, color = mahog))+
  geom_line(size= 1.2) +
  theme_minimal()+
  theme(legend.text = element_text(size = 14),
        legend.position = 'bottom', legend.title = element_text(size=16))+
  geom_line(size= 1.3) + scale_x_continuous(breaks = c(1995,1999,2001,2005,2009,2013))  +
  geom_vline(xintercept = c(1999,2001,2009),
             linetype = 'dashed', size = 1)+
  labs(x = '', y = '',  color ='Painel B') +
  scale_color_manual(values = c("#F8AFA8", "#E2D200"))

tx_abandono_EM <- painel_educ %>%
  dplyr::mutate(mahog = case_when(code %in% base_tables$code ~ 1, T~0)) %>% 
  dplyr::group_by(year,mahog)  %>%
  dplyr::summarise(across(.cols = c(drpt_rate_EF, drpt_rate_EM), 
                          ~mean(., na.rm=T))) %>%
  dplyr::mutate(mahog =  case_when(mahog == 1 ~ 'Município com\nocorrência de Mogno',
                                   mahog == 0 ~ 'Município sem\nocorrência de Mogno')) %>% 
  ggplot2::ggplot(mapping = aes(x= year, y = drpt_rate_EM, color = mahog))+
  geom_line(size= 1.2) +
  theme_minimal()+
  theme(legend.text = element_text(size = 14),
        legend.position = 'bottom', legend.title = element_text(size=16))+
  geom_line(size= 1.3) + scale_x_continuous(breaks = c(1995,1999,2001,2005,2009,2013))  +
  geom_vline(xintercept = c(1999,2001,2009),
             linetype = 'dashed', size = 1)+
  labs(x = '', y = '',  color ='Painel C') +
  scale_color_manual(values = c("#F8AFA8", "#E2D200"))

tx_trabalho_infantil <- censo_trab %>%
  dplyr::mutate(mahog = case_when(uf %in%c(11,12,13,15,17,51) ~ 1, T~0)) %>% 
  dplyr::filter(year != 1991) %>% 
  dplyr::group_by(year,mahog)  %>%
  dplyr::summarise(across(.cols = c(child_work_rate), 
                          ~mean(., na.rm=T))) %>%
  dplyr::mutate(mahog =  case_when(mahog == 1 ~ 'UF com\nocorrência de Mogno',
                                   mahog == 0 ~ 'UF sem\nocorrência de Mogno')) %>% 
  ggplot2::ggplot(mapping = aes(x= year, y = child_work_rate, color = mahog))+
  geom_line(size= 1.2) +
  theme_minimal()+
  theme(legend.text = element_text(size = 14),
        legend.position = 'bottom', legend.title = element_text(size=15))+
  geom_line(size= 1.3) + scale_x_continuous(breaks = c(1995,1999,2001,2005,2009,2013))  +
  geom_vline(xintercept = c(1999,2001,2009),
             linetype = 'dashed', size = 1)+
  labs(x = '', y = '',  color ='Painel D') +
  scale_color_manual(values = c("#F8AFA8", "#E2D200"))

ggsave(tx_trabalho_infantil,
       filename = 'output/tables/evol_tx_trabalho_infantil.png',
       width = 25, height = 15, device = 'png', bg = 'white',
       units = 'cm')

images <- gridExtra::grid.arrange(exp_taxa_EF,tx_abandono_EF,tx_abandono_EM, tx_trabalho_infantil)

  ggsave(images,
         filename = 'output/tables/images.png',
         width = 15, height = 5, device = 'png', bg = 'white',
         units = 'cm')

# 2. Table 2 - Panel A ---------------------------------------------------------
reg_T2_1 <- base_tables %>% 
  dplyr::mutate(year = factor(year)) %>% 
  fixest::feols(fml =  drpt_rate_EM ~ treat1 + treat2 + treat3 + year | code,
                weights = ~avg_pop, cluster = ~code, data = .)

reg_T2_2 <- base_tables %>% 
  dplyr::mutate(year = factor(year),
                uf = factor(uf)) %>% 
  fixest::feols(fml = drpt_rate_EM ~ treat1 + treat2 + treat3 + year + i(uf,year)| code,
                weights = ~avg_pop, cluster = ~code, data = .)

reg_T2_3 <- base_tables %>% 
  dplyr::mutate(year = factor(year),
                uf = factor(uf)) %>% 
  fixest::feols(fml = drpt_rate_EM ~ treat1 + treat2 + treat3 + year +
                  i(year,area_base) +  i(year,gdpag_base)| code,
                weights = ~avg_pop, cluster = ~code, data = .)

reg_T2_4 <- base_tables %>% 
  dplyr::mutate(year = factor(year),
                uf = factor(uf)) %>% 
  fixest::feols(fml = drpt_rate_EM ~ treat1 + treat2 + treat3 +
                  year + int1 +int2 +int3 | code,
                weights = ~avg_pop, cluster = ~code, data = .)

reg_T2_5 <- base_tables %>% 
  dplyr::mutate(year = factor(year),
                uf = factor(uf)) %>% 
  fixest::feols(fml = drpt_rate_EM ~ treatx_area1 + treatx_area2 + treatx_area3
                + year + i(year,uf)| code,
                weights = ~avg_pop, cluster = ~code, data = .)

reg_T2_6 <- base_tables %>% 
  dplyr::mutate(year = factor(year),
                uf = factor(uf)) %>% 
  fixest::feols(fml = drpt_rate_EM ~ treatt_area1 + treatt_area2+ treatt_area3
                + year + i(year,uf)| code,
                weights = ~avg_pop, cluster = ~code, data = .)

table_2_panel_B <- etable(reg_T2_1,reg_T2_2,reg_T2_3,reg_T2_4,reg_T2_5,reg_T2_6) %>% 
  dplyr::mutate(names = row.names(.)) %>%
  dplyr::select(7,1:6)

writexl::write_xlsx(table_2_panel_B,path = 'output/tables/table_2_panel_B.xlsx')


# 3. Table 2 - Panel B ---------------------------------------------------------
reg_T2_1_B <- base_tables %>% 
  dplyr::mutate(year = factor(year)) %>% 
  fixest::feols(fml =  drpt_rate_EF ~ treat1 + treat2 + treat3 + year | code,
                weights = ~avg_pop, cluster = ~code, data = .)

reg_T2_2_B <- base_tables %>% 
  dplyr::mutate(year = factor(year),
                uf = factor(uf)) %>% 
  fixest::feols(fml = drpt_rate_EF ~ treat1 + treat2 + treat3 + year + i(uf,year)| code,
                weights = ~avg_pop, cluster = ~code, data = .)

reg_T2_3_B <- base_tables %>% 
  dplyr::mutate(year = factor(year),
                uf = factor(uf)) %>% 
  fixest::feols(fml = drpt_rate_EF ~ treat1 + treat2 + treat3 + year +
                  i(year,area_base) + i(year,gdpag_base) | code,
                weights = ~avg_pop, cluster = ~code, data = .)

reg_T2_4_B <- base_tables %>% 
  dplyr::mutate(year = factor(year),
                uf = factor(uf)) %>% 
  fixest::feols(fml = drpt_rate_EF ~ treat1 + treat2 + treat3 +
                  year + int1 +int2 +int3 | code,
                weights = ~avg_pop, cluster = ~code, data = .)

reg_T2_5_B <- base_tables %>% 
  dplyr::mutate(year = factor(year),
                uf = factor(uf)) %>% 
  fixest::feols(fml = drpt_rate_EF ~ treatx_area1 + treatx_area2 + treatx_area3
                + year + i(year,uf)| code,
                weights = ~avg_pop, cluster = ~code, data = .)

reg_T2_6_B <- base_tables %>% 
  dplyr::mutate(year = factor(year),
                uf = factor(uf)) %>% 
  fixest::feols(fml = drpt_rate_EF ~ treatt_area1 + treatt_area2+ treatt_area3
                + year + i(year,uf)| code,
                weights = ~avg_pop, cluster = ~code, data = .)

table_2_panel_A <- fixest::etable(reg_T2_1_B,reg_T2_2_B,reg_T2_3_B,reg_T2_4_B,reg_T2_5_B,reg_T2_6_B) %>%
  dplyr::mutate(names = row.names(.)) %>%
  dplyr::select(7,1:6)

writexl::write_xlsx(table_2_panel_A,path = 'output/tables/table_2_panel_A.xlsx')
