# Importando pacotes ------------------------------------------------------

library(convey)
library(geobr)
library(dplyr)
library(ggplot2)

# Preparando a PNADC ------------------------------------------------------

## Adiciona a `PNAD Contínua` à função `convey_prep()`

pnadc_convey <- convey::convey_prep(pnadc_data)

## A função abaixo nos permite otimizar o fluxo de trabalho ao calcular
## o índice FGT, converter os resultados em um tibble e exportar como um
## arquivo .xlsx

compute_fgt <- function(poverty_line, 
                        variable_name, 
                        path_name = ""
                        ){
  # Nome da variável
  
  var_name <- variable_name
  
  # Índice FGT em formato raw
  
  fgt_raw <- survey::svyby(~ VD5008,
                           var_name,
                           pnadc_convey,
                           convey::svyfgt,
                           g = 2,
                           abs_thresh = {{poverty_line}},
                           type_thresh = "abs",
                           na.rm = TRUE
                           ) |>
    dplyr::as_tibble()
  
  writexl::write_xlsx(x = fgt_raw, path = path_name)
  
  return(fgt_raw)
}


# Linha de pobreza: 499,00 reais ------------------------------------------

# FGT por sexo

fgt_sexo_sm <- compute_fgt(
  poverty_line = 499,
  variable_name = ~sexo,
  path_name = "fgt-tables/fgt_sexo_sm.xlsx"
  )

# FGT por raça

fgt_raca_sm <- compute_fgt(
  poverty_line = 499,
  variable_name = ~raca,
  path_name = "fgt-tables/fgt_raca_sm.xlsx"
  )

# FGT por anos de estudo

fgt_anos_estudo_sm <- compute_fgt(
  poverty_line = 499,
  variable_name = ~anos_estudo,
  path_name = "fgt-tables/fgt_anos_estudo_sm.xlsx"
  )

# FGT por nível de instrução

fgt_nivel_instrucao_sm <- compute_fgt(
  poverty_line = 499,
  variable_name = ~nivel_instrucao,
  path_name = "fgt-tables/fgt_nivel_instrucao_sm.xlsx"
  )

# FGT pela quantidade de membros da família

fgt_num_membros_familia_sm <- compute_fgt(
  poverty_line = 499,
  variable_name = ~num_membros_familia,
  path_name = "fgt-tables/fgt_num_membros_familia_sm.xlsx"
  )

# FGT pelo setor de trabalho

fgt_trabalha_agricultura_sm <- compute_fgt(
  poverty_line = 499,
  variable_name = ~trabalha_agricultura,
  path_name = "fgt-tables/fgt_trabalha_agricultura_sm.xlsx"
  )

# FGT pela localização do domicílio

fgt_localizacao_domicio_sm <- compute_fgt(
  poverty_line = 499,
  variable_name = ~localizacao_domicio,
  path_name = "fgt-tables/fgt_localizacao_domicio_sm.xlsx"
  )

# FGT por região

fgt_regiao_sm <- compute_fgt(
  poverty_line = 499,
  variable_name = ~regiao,
  path_name = "fgt-tables/fgt_regiao_sm.xlsx"
  )

# FGT por UF

fgt_uf_estado_sm <- compute_fgt(
  poverty_line = 499,
  variable_name = ~UF,
  path_name = "fgt-tables/fgt_uf_estado_sm.xlsx"
  )

# FGT total

fgt_total_sm <- svyfgt(
  ~VD5008,
  design = pnadc_convey,
  g = 2,
  abs_thresh = 499,
  type_thresh = "abs",
  na.rm = TRUE
  ) |> 
  dplyr::as_tibble() 

# Expora os resultados

fgt_total_sm |> 
  writexl::write_xlsx(path = "fgt-tables/fgt_total_sm.xlsx")

# Linha de pobreza: 1.364,46 ----------------------------------------------

# FGT por sexo

fgt_sexo_rm <- compute_fgt(
  poverty_line = 1364.46,
  variable_name = ~sexo,
  path_name = "fgt-tables/fgt_sexo_rm.xlsx"
  )

# FGT por raça

fgt_raca_rm <- compute_fgt(
  poverty_line = 1364.46,
  variable_name = ~raca,
  path_name = "fgt-tables/fgt_raca_rm.xlsx"
  )

# FGT por anos de estudo

fgt_anos_estudo_rm <- compute_fgt(
  poverty_line = 1364.46,
  variable_name = ~anos_estudo,
  path_name = "fgt-tables/fgt_anos_estudo_rm.xlsx"
  )

# FGT por nível de instrução

fgt_nivel_instrucao_rm <- compute_fgt(
  poverty_line = 1364.46,
  variable_name = ~nivel_instrucao,
  path_name = "fgt-tables/fgt_nivel_instrucao_rm.xlsx"
  )

# FGT pela quantidade de membros da família

fgt_num_membros_familia_rm <- compute_fgt(
  poverty_line = 1364.46,
  variable_name = ~num_membros_familia,
  path_name = "fgt-tables/fgt_num_membros_familia_rm.xlsx"
  )

# FGT pelo setor de trabalho

fgt_trabalha_agricultura_rm <- compute_fgt(
  poverty_line = 1364.46,
  variable_name = ~trabalha_agricultura,
  path_name = "fgt-tables/fgt_trabalha_agricultura_rm.xlsx"
  )

# FGT pela localização do domicílio

fgt_localizacao_domicio_rm <- compute_fgt(
  poverty_line = 1364.46,
  variable_name = ~localizacao_domicio,
  path_name = "fgt-tables/fgt_localizacao_domicio_rm.xlsx"
  )

# FGT por região

fgt_regiao_rm <- compute_fgt(
  poverty_line = 1364.46,
  variable_name = ~regiao,
  path_name = "fgt-tables/fgt_regiao_rm.xlsx"
  )

# FGT por UF

fgt_uf_estado_rm <- compute_fgt(
  poverty_line = 1364.46,
  variable_name = ~UF,
  path_name = "fgt-tables/fgt_uf_estado_rm.xlsx"
  )

# FGT total

fgt_total_rm <- svyfgt(
  ~VD5008,
  design = pnadc_convey,
  g = 2,
  abs_thresh = 1364.46,
  type_thresh = "abs",
  na.rm = TRUE
  ) |> 
  dplyr::as_tibble() 

# Expora os resultados

fgt_total_rm |> 
  writexl::write_xlsx(path = "fgt-tables/fgt_total_rm.xlsx")