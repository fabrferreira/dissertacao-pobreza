# Importando pacotes ------------------------------------------------------

library(dplyr)
library(srvyr)
library(survey)
library(rlang)

# Análise exploratória da amostra -----------------------------------------

## Total de indivíduos na amostra

n_individuos <- pnadc_data |> 
  dplyr::summarise(n = srvyr::survey_total())

## Renda média na amostra

renda_media <- pnadc_data |>
  dplyr::summarise(renda_media = srvyr::survey_mean(VD5008, na.rm = TRUE))

## Estatísticas descritivas da amostra

## Descriptive statistics from household head in the sample

estatisticas_amostra <- pnadc_data |> 
  dplyr::filter(chefe_domicilio == "Sim") |> 
  dplyr::summarise(
    renda_media = srvyr::survey_mean(VD5008, na.rm = TRUE),
    renda_mediana = srvyr::survey_median(VD5008, na.rm = TRUE),
    dp_renda = srvyr::survey_sd(VD5008, na.rm = TRUE),
    media_idade = srvyr::survey_mean(V2009, na.rm = TRUE),
    mediana_idade = srvyr::survey_median(V2009, na.rm = TRUE),
    dp_idade = srvyr::survey_sd(V2009, na.rm = TRUE),
    media_anos_estudo = srvyr::survey_mean(VD3005, na.rm = TRUE),
    mediana_anos_estudo = srvyr::survey_median(VD3005, na.rm = TRUE),
    dp_anos_estudo = srvyr::survey_sd(VD3005, na.rm = TRUE),
    media_membros = srvyr::survey_mean(num_membros_familia, na.rm = TRUE),
    mediana_membros = srvyr::survey_median(num_membros_familia, na.rm = TRUE),
    dp_membros = srvyr::survey_sd(num_membros_familia, na.rm = TRUE),
    n_obs = srvyr::survey_total(na.rm = TRUE),
    .groups = "rowwise"
    ) |> 
  dplyr::mutate_if(is.numeric, round, 2)

## Função que permite otimizar o cálculo de estatísticas descritivas

compute_stats <- function(data, variable){
  data |> 
    dplyr::filter(chefe_domicilio == "Sim") |> 
    dplyr::group_by({{ variable }}) |> 
    srvyr::survey_count() |> 
    dplyr::ungroup() |> 
    dplyr::mutate(percent = ( n / sum(n) ) * 100)
}

## Percentual de indivíduos por raça

percentual_raca <- compute_stats(pnadc_data, raca)

## Percentual de indivíduos por sexo

percentual_sexo <- compute_stats(pnadc_data, sexo)

## Percentual de indivíduos por tipo de zona onde reside (rural ou urbana) 

percentual_dom_local <- compute_stats(pnadc_data, localizacao_domicio)

## Percentual de indivíduos por região onde reside

percentual_regiao <- compute_stats(pnadc_data, regiao)

## Percentual de indivíduos por situação de trabalho na agricultura 

percentual_sector <- compute_stats(pnadc_data, trabalha_agricultura)

## Renda domiciliar per capita média dos chefes de família

renda_media_cf <- pnadc_data |> 
  dplyr::filter(chefe_domicilio == "Sim") |> 
  dplyr::summarise(mean_income = srvyr::survey_mean(VD5008, na.rm = TRUE))
