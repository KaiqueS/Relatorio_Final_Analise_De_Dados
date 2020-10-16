library( tidyverse )
library( haven )
library( stargazer )
library( scales )
library( tidyr )
library( ggplot2 )
library( devtools )
library( usethis )
library( dplyr )
library( apsrtable )
library( car )
library( sjPlot )
library( sjmisc )
library( sjlabelled )
library( dotwhisker )
library( downloader )
library( SAScii )

devtools::install_github( "benmarwick/wordcountaddin", type = "source", dependencies = TRUE )

setwd( "//media//kaique//Arquivos//Trabalho//Relatorio_Analise_De_Dados" )

### Primeiro Banco WorldBank - Pode ser encontrado no link https://databank.worldbank.org/reports.aspx?ReportId=121820&Type=Table#advancedDownloadOptions

worldbank_2018 <- readxl::read_xlsx( "WorldBank_Final.xlsx" )

worldbank_2018$`Country Code` <- NULL
worldbank_2018$`Series Code` <- NULL

worldbank_2018$`2018 [YR2018]` <- as.numeric( worldbank_2018$`2018 [YR2018]` )

worldbank_2018

glimpse( worldbank_2018 )

# pivot_wider is the secret!
worldbank_2018_formatado <- worldbank_2018 %>%
                            pivot_wider( names_from = `Series Name`, values_from = `2018 [YR2018]` )

colnames( worldbank_2018_formatado ) <- c( "País", "Fertilidade", "Pop_Fem", "Expec_Vida_H", "Expec_Vida_M",
                                           "For_Trab_F", "Pop_Urb", "PIB_PerCapita", "Agricultura", "Gasto_Gov",
                                           "Alfabetizados" )

worldbank_2018_formatado

glimpse( worldbank_2018_formatado )

### Segundo Banco Polity - Pode ser encontrado no link http://www.systemicpeace.org/inscr/p5v2018.xls

polity_online <- download.file( "http://www.systemicpeace.org/inscr/p5v2018.xls", "polity_online.xls", mode = 'wb' )

banco_polity <- readxl::read_xls( "polity_online.xls" )

#banco_polity <- readxl::read_xls( "Polity5_Annual_2018.xls" )

banco_polity <- banco_polity %>% 
                select( country, year, polity2 ) %>%
                na.omit( )

banco_polity <- banco_polity %>%
                filter( year == 2018 )

banco_polity$year <- NULL

colnames( banco_polity )[ 1 ] <- "País"

banco_polity

glimpse( banco_polity )

### Terceiro Banco V-Dem - Pode ser encontrado no link https://www.v-dem.net/en/data/data-version-10/#, mas não é possível baixá-lo diretamente
### Porém, é possível acessá-lo como um package do R, seguindo as instruções em https://github.com/vdeminstitute/vdemdata

banco_vdem <- read.csv( "V_Dem_Full_2019.csv" )

banco_vdem <- banco_vdem %>% 
              select( country_name,
                      year,
                      v2x_polyarchy,
                      v2x_libdem,
                      v2x_partipdem,
                      v2x_delibdem,
                      v2x_egaldem,
                      v2elfemrst,
                      v2fsuffrage )
              
banco_vdem <- banco_vdem %>% filter( year == 2018 )

banco_vdem$year <- NULL

colnames( banco_vdem )[ 1 ] <- "País"

banco_vdem

glimpse( banco_vdem )

### Banco Sufragio Feminino - Ano em que mulheres obtiveram sufragio, Our World in Data - pode ser acessado no link https://ourworldindata.org/grapher/universal-suffrage-granted-to-women?time=earliest..2017
### Porém, não é possível baixá-lo diretamente pelo R, já que não é fornecido um link para download.

sufragio_fem <- read_csv( "Universal_Suffrage_Women.csv" )

sufragio_fem$Code <- NULL

sufragio_fem <- sufragio_fem %>% filter( `Universal suffrage to women (OWID based on Paxton et al (2006))` == "Yes" )

sufragio_fem <- sufragio_fem[ !duplicated( sufragio_fem$Entity ), ]

colnames( sufragio_fem ) <- c( "País", "Ano_Sufragio_Fem", "Voto_Fem" )

sufragio_fem$Voto_Fem <- NULL

sufragio_fem

glimpse( sufragio_fem )

write_excel_csv( sufragio_fem, "Banco_Sufragio_Fem.xlsx" )

### Bancos Educação - Barro And Lee 2010 - pode ser acessado na página http://www.barrolee.com/, na seção Data

download.file( "https://barrolee.github.io/BarroLeeDataSet/BLData/BL2013_F_v2.2.csv", "banco_ed_mulher_online.csv", mode = 'wb' )
download.file( "https://barrolee.github.io/BarroLeeDataSet/BLData/BL2013_M_v2.2.csv", "banco_ed_homem_online.csv", mode = 'wb' )

banco_ed_mulher <- read.csv( "banco_ed_mulher_online.csv" )
banco_ed_homem <- read.csv( "banco_ed_homem_online.csv" )

#banco_ed_mulher <- read.csv( "BarroLee_Mulher.csv" )
#banco_ed_homem <- read.csv( "BarroLee_Homem.csv" )

banco_ed_mulher <- banco_ed_mulher %>% filter( year == 2010 ) %>%
                   mutate( sex = case_when( sex == F ~ "M" ) ) %>%
                   filter( ageto != 999 )

banco_ed_homem <- banco_ed_homem %>% filter( year == 2010 ) %>%
                  mutate( sex = case_when( sex == "M" ~ "H" ) ) %>%
                  filter( ageto != 999 )

banco_ed_mulher[ , c( 'BLcode', 'year', 'sex', 'agefrom', 'ageto',
                      'lu', 'lp', 'lpc', 'ls', 'lsc', 'lh', 'lhc', 'yr_sch_pri',
                      'yr_sch_sec', 'yr_sch_ter', 'pop', 'WBcode', 'region_code' ) ] <- NULL

banco_ed_homem[ , c( 'BLcode', 'year', 'sex', 'agefrom', 'ageto',
                      'lu', 'lp', 'lpc', 'ls', 'lsc', 'lh', 'lhc', 'yr_sch_pri',
                      'yr_sch_sec', 'yr_sch_ter', 'pop', 'WBcode', 'region_code' ) ] <- NULL

colnames( banco_ed_mulher ) <- c( "País", "Ed_Media" )
colnames( banco_ed_homem ) <- c( "País", "Ed_Media" )

países <- banco_ed_homem$País

dif_ed_media <- banco_ed_mulher$Ed_Media - banco_ed_homem$Ed_Media

banco_ed_mh <- data_frame( países, dif_ed_media )

colnames( banco_ed_mh  ) <- c( "País", "Dif_Ed_Media" )

banco_ed_mh <- aggregate( Dif_Ed_Media ~ País, data = banco_ed_mh, FUN = mean )

banco_ed_mulher
banco_ed_homem
banco_ed_mh

glimpse( banco_ed_mulher )
glimpse( banco_ed_homem )
glimpse( banco_ed_mh )

### Banco Completo + Ano de Sufrágio para Mulheres

banco_completo <- merge( worldbank_2018_formatado, banco_polity, by = "País", all.x = TRUE )

banco_completo <- merge( banco_completo, banco_vdem, by = "País", all.x = TRUE )

banco_completo <- merge( banco_completo, sufragio_fem, by = "País", all.x = TRUE )

banco_completo <- merge( banco_completo, banco_ed_mh, by = "País", all.x = TRUE )

glimpse( banco_completo )

write_excel_csv( banco_completo, "banco_completo.xlsx" )
write.csv( banco_completo, "banco_completo.csv" )

### Variáveis

diferenca_exp_vida <- banco_completo$Expec_Vida_M - banco_completo$Expec_Vida_H

log_nat_fertilidade <- log( banco_completo$Fertilidade )

log_nat_pib <- log( banco_completo$PIB_PerCapita )

porc_analfabetos <- 100 - banco_completo$Alfabetizados

log_nat_analfabetos <- log( porc_analfabetos )

### Regressão Polity

cor.test( banco_completo$Alfabetizados, banco_completo$Dif_Ed_Media )

regressao_fem_pop_polity <- lm( Pop_Fem ~ polity2 + Ano_Sufragio_Fem + Pop_Urb + log_nat_pib + Agricultura +
                                log_nat_analfabetos + Gasto_Gov, data = banco_completo )

summary( regressao_fem_pop_polity )

regressao_fem_exp_vid_polity <- lm( diferenca_exp_vida ~ polity2 + Ano_Sufragio_Fem + Pop_Urb + log_nat_pib +
                                    Agricultura + log_nat_analfabetos + Gasto_Gov, data = banco_completo )

summary( regressao_fem_exp_vid_polity )

regressao_fem_fertilidade_polity <- lm( log_nat_fertilidade ~ polity2 + Ano_Sufragio_Fem + Pop_Urb + log_nat_pib +
                                        Agricultura + log_nat_analfabetos + Gasto_Gov, data = banco_completo )

summary( regressao_fem_fertilidade_polity )

regressao_fem_ed_media_polity <- lm( Dif_Ed_Media ~ polity2 + Ano_Sufragio_Fem + Pop_Urb + log_nat_pib +
                                     Agricultura + log_nat_analfabetos + Gasto_Gov, data = banco_completo )

summary( regressao_fem_ed_media_polity )

regressao_fem_trabalho_polity <- lm( For_Trab_F ~ polity2 + Ano_Sufragio_Fem + Pop_Urb + log_nat_pib + Agricultura +
                                     log_nat_analfabetos + Gasto_Gov, data = banco_completo )

summary( regressao_fem_trabalho_polity )

dwplot( list( regressao_fem_pop_polity, regressao_fem_exp_vid_polity, regressao_fem_fertilidade_polity,
              regressao_fem_ed_media_polity, regressao_fem_trabalho_polity ),
              vline = geom_vline( xintercept = 0, colour = "grey60", linetype = 2 ) )

### Regressão V-Dem

regressao_fem_pop_vdem <- lm( Pop_Fem ~ v2x_polyarchy + Ano_Sufragio_Fem + v2x_libdem + v2x_partipdem + v2x_egaldem + Pop_Urb +
                              log_nat_pib + Agricultura + log_nat_analfabetos + Gasto_Gov, data = banco_completo )

summary( regressao_fem_pop_vdem )

regressao_fem_exp_vid_vdem <- lm( diferenca_exp_vida ~ v2x_polyarchy + Ano_Sufragio_Fem + v2x_libdem + v2x_partipdem + v2x_egaldem + Pop_Urb +
                                  log_nat_pib + Agricultura + log_nat_analfabetos + Gasto_Gov, data = banco_completo )

summary( regressao_fem_exp_vid_vdem )

regressao_fem_fertilidade_vdem <- lm( log_nat_fertilidade ~ v2x_polyarchy + Ano_Sufragio_Fem + v2x_libdem + v2x_partipdem + v2x_egaldem + Pop_Urb +
                                      log_nat_pib + Agricultura + log_nat_analfabetos + Gasto_Gov, data = banco_completo )

summary( regressao_fem_fertilidade_vdem )

regressao_fem_ed_media_vdem <- lm( Dif_Ed_Media ~ v2x_polyarchy + Ano_Sufragio_Fem + v2x_libdem + v2x_partipdem + v2x_egaldem + Pop_Urb +
                                   log_nat_pib + Agricultura + log_nat_analfabetos + Gasto_Gov, data = banco_completo )

summary( regressao_fem_ed_media_vdem )

regressao_fem_trabalho_vdem <- lm( For_Trab_F ~ v2x_polyarchy + Ano_Sufragio_Fem + v2x_libdem + v2x_partipdem + v2x_egaldem + Pop_Urb +
                                   log_nat_pib + Agricultura + log_nat_analfabetos + Gasto_Gov, data = banco_completo )

summary( regressao_fem_trabalho_vdem )

dwplot( list( regressao_fem_pop_vdem, regressao_fem_exp_vid_vdem, regressao_fem_fertilidade_vdem,
              regressao_fem_ed_media_vdem, regressao_fem_trabalho_vdem ),
              vline = geom_vline( xintercept = 0, colour = "grey60", linetype = 2 ) )

### Pressupostos da Regressão Polity

## Linearidade

plot( regressao_fem_pop_polity, 1 )
plot( regressao_fem_exp_vid_polity, 1 )
plot( regressao_fem_fertilidade_polity, 1 )
plot( regressao_fem_ed_media_polity, 1 )
plot( regressao_fem_trabalho_polity, 1 )

## Homocedasticidade

plot( regressao_fem_pop_polity, 3 )
plot( regressao_fem_exp_vid_polity, 3 )
plot( regressao_fem_fertilidade_polity, 3 )
plot( regressao_fem_ed_media_polity, 3 )
plot( regressao_fem_trabalho_polity, 3 )

## Autocorrelação

acf( regressao_fem_pop_polity$residuals )
acf( regressao_fem_exp_vid_polity$residuals )
acf( regressao_fem_fertilidade_polity$residuals )
acf( regressao_fem_ed_media_polity$residuals )
acf( regressao_fem_trabalho_polity$residuals )

## Normalidade dos Resíduos

plot( regressao_fem_pop_polity, 2 )
plot( regressao_fem_exp_vid_polity, 2 )
plot( regressao_fem_fertilidade_polity, 2 )
plot( regressao_fem_ed_media_polity, 2 )
plot( regressao_fem_trabalho_polity, 2 )

## Multicolinearidade

vif( regressao_fem_pop_polity )
vif( regressao_fem_exp_vid_polity )
vif( regressao_fem_fertilidade_polity )
vif( regressao_fem_ed_media_polity )
vif( regressao_fem_trabalho_polity )

### Pressupostos da regressao V-Dem

## Linearidade

plot( regressao_fem_pop_vdem, 1 )
plot( regressao_fem_exp_vid_vdem, 1 )
plot( regressao_fem_fertilidade_vdem, 1 )
plot( regressao_fem_ed_media_vdem, 1 )
plot( regressao_fem_trabalho_vdem, 1 )

## Homocedasticidade

plot( regressao_fem_pop_vdem, 3 )
plot( regressao_fem_exp_vid_vdem, 3 )
plot( regressao_fem_fertilidade_vdem, 3 )
plot( regressao_fem_ed_media_vdem, 3 )
plot( regressao_fem_trabalho_vdem, 3 )

## Autocorrelação

acf( regressao_fem_pop_vdem$residuals )
acf( regressao_fem_exp_vid_vdem$residuals )
acf( regressao_fem_fertilidade_vdem$residuals )
acf( regressao_fem_ed_media_vdem$residuals )
acf( regressao_fem_trabalho_vdem$residuals )

## Normalidade dos Resíduos

plot( regressao_fem_pop_vdem, 2 )
plot( regressao_fem_exp_vid_vdem, 2 )
plot( regressao_fem_fertilidade_vdem, 2 )
plot( regressao_fem_ed_media_vdem, 2 )
plot( regressao_fem_trabalho_vdem, 2 )

## Multicolinearidade

vif( regressao_fem_pop_vdem )

### Tabela com vários modelos

tabela_polity <- apsrtable( regressao_fem_pop_polity, regressao_fem_exp_vid_polity, regressao_fem_fertilidade_polity,
                            regressao_fem_ed_media_polity, regressao_fem_trabalho_polity, Sweave = TRUE, stars = "default", digits = 6 )

tabela_vdem <- apsrtable( regressao_fem_pop_vdem, regressao_fem_exp_vid_vdem, regressao_fem_fertilidade_vdem,
                          regressao_fem_ed_media_vdem, regressao_fem_trabalho_vdem, Sweave = TRUE, stars = "default", digits = 6 )

modelos_polity <- tab_model( regressao_fem_pop_polity, regressao_fem_exp_vid_polity, regressao_fem_fertilidade_polity,
                             regressao_fem_ed_media_polity, regressao_fem_trabalho_polity, digits = 3 )

modelos_vdem <- tab_model( regressao_fem_pop_vdem, regressao_fem_exp_vid_vdem, regressao_fem_fertilidade_vdem,
                           regressao_fem_ed_media_vdem, regressao_fem_trabalho_vdem, digits = 3 )

modelos_polity
modelos_vdem
