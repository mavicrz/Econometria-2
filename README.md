# Undergraduate Project on Causal Inference

This folder includes code and files used for the Econometrics II project at FEA-USP with Professor Fabiana Rocha.

*Title:* Growing in Illegality: Evidence of Child Exposure to the Illegal Mahogany Trade in Brazil

*Data Sources:*  
- Mahogany Exports: https://www.openicpsr.org/openicpsr/project/113679/version/V1/view  
- School Dropout Rates: https://www.gov.br/inep/pt-br/areas-de-atuacao/pesquisas-estatisticas-e-indicadores  
- Child Labor Rates: http://tabnet.datasus.gov.br/cgi/idb2000/fqb07.htm  

*Code:*  
In the folder "Trabalho > code" you will find the scripts used in the project:
- The `.do` files are replication codes from Ariaster and Soares (2017), published in AEJ (https://www.aeaweb.org/articles?id=10.1257/app.20160055).  
  We used these files as a base to replicate the treatment of the mahogany export dataset and the regression table.
- The script `panel_mogno.R` replicates the treatments performed in the Stata codes.
- The script `panel_censo.R` processes microdata from the School Census from 1995 to 2013 to obtain dropout rates for 
  the final years of primary education and for high school.
- The script `panel_trab_infantil.R` processes child labor rate data from 1995 to 2013 published by RIPSA.
- The script `regressions.R` generates the project’s figures and regression outputs.

*Files:*  
Raw data files are provided in "Trabalho > Input" and processed files in "Trabalho > Output".
