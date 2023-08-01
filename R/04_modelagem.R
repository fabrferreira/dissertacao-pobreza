# Importando pacotes ------------------------------------------------------

library(dplyr)
library(ggplot2)
library(showtext)
library(srvyr)
library(survey)
library(rlang)
library(margins)
library(convey)
library(xtable)
library(stargazer)

font_add_google(name = "Inter", family = "custom")

showtext_auto()

# Estimação econométrica --------------------------------------------------

## Criando variáveis necessárias para a estimação

pnadc <- pnadc_clean |> 
  dplyr::filter(chefe_domicilio == "Sim") |> 
  dplyr::mutate(
    renda_domiciliar_pc = VD5008,
    pobreza_sm = ifelse(VD5008 < 998 / 2, 1, 0),
    pobreza_rm = ifelse(VD5008 < renda_media$renda_media, 1, 0),
    sexo_cf = ifelse(V2007 == 1, 1, 0),
    idade_cf = V2009,
    raca_cf = ifelse(V2010 %in% c(2, 4), 1, 0),
    raca_cf = ifelse(V2010 == 2, 1, 0),
    ano_estudo_cf = VD3005,
    local_dom = ifelse(V1022 == 1, 1, 0),
    num_membros = V2001,
    trab_agric = ifelse(VD4010 == 1, 1, 0),
    regiao_dom = ifelse(regiao %in% c("Norte", "Nordeste"), 1, 0)
    )

## Colocando no formato amostral complexo

pnadc <- pnadc |> 
  srvyr::as_survey_design(
    ids = UPA, 
    strata = Estrato,
    weights = V1032, 
    nest = TRUE
    )

## Estimação econométrica utilizando 50% salário-mínimo com linha de pobreza

modelo_sm <- survey::svyglm(
  formula = pobreza_sm ~ sexo_cf + idade_cf + raca_cf + ano_estudo_cf + local_dom + num_membros + trab_agric + regiao_dom, 
  family = quasibinomial(link = "probit"), 
  design = pnadc
  )

## Sumário estatístico do modelo e teste de hipótese de significância estatística
## Dos coeficientes estimados

jtools::j_summ(modelo_sm)

survey::regTermTest(modelo_sm, test.terms = ~ sexo_cf + idade_cf + raca_cf + ano_estudo_cf + local_dom + num_membros + trab_agric + regiao_dom) 

## Pseudo-R2 Cox-Snell and Nagelkerke

pseudor2cs_sm <- survey::psrsq(modelo_sm, method = "Cox-Snell") |> round(2)

pseudor2ng_sm <- survey::psrsq(modelo_sm, method = "Nagelkerke") |> round(2)

## Estimation using as poverty line household mean income per capita

modelo_rm <- survey::svyglm(
  formula = pobreza_rm ~ sexo_cf + idade_cf + raca_cf + ano_estudo_cf + local_dom + num_membros + trab_agric + regiao_dom, 
  family = quasibinomial(link = "probit"), 
  design = pnadc
  )

## Sumário estatístico do modelo e teste de hipótese de significância estatística
## Dos coeficientes estimados

jtools::j_summ(modelo_rm)

survey::regTermTest(modelo_rm, test.terms = ~ sexo_cf + idade_cf + raca_cf + ano_estudo_cf + local_dom + num_membros + trab_agric + regiao_dom) 

## Pseudo-R2 Cox-Snell and Nagelkerke

pseudor2cs_rm <- survey::psrsq(modelo_rm, method = "Cox-Snell") |> round(2)

pseudor2ng_rm <- survey::psrsq(modelo_rm, method = "Nagelkerke") |> round(2)

## Exporta os parâmetros estimados dos dois modelos em \LaTeX

stargazer::stargazer(
  modelo_sm, modelo_rm,
  add.lines = list(
    c("Pseudo $R^2$ Cox-Snell", pseudor2cs_sm, pseudor2cs_rm),
    c("Pseudo $R^2$ Nagelkerke", pseudor2ng_sm, pseudor2ng_rm)),
  column.sep.width = "3pt",
  header = FALSE,
  single.row = FALSE,
  no.space = TRUE,
  font.size = "small",
  summary = TRUE,
  model.names = TRUE,
  model.numbers = TRUE,
  keep.stat = c("rsq", "n"),
  type = "latex",
  out = "misc/output.html",
  decimal.mark = ",",
  covariate.labels = c("sexo", "idade", "raca", "anosEstudo", "localDom", "numMembros", "trabAgric", "regiao")
  )

# Efeitos marginais -------------------------------------------------------

## Calculando os efeitos marginais

ef_modelo_sm <- margins::margins(model = modelo_sm, data = pnadc)

ef_modelo_rm <- margins::margins(model = modelo_rm, data = pnadc)

## Exportando os efeitos marginais em formato .xlsx e \LaTeX 

## Efeitos marginais do modelo 1

broom::tidy(ef_modelo_sm) |> 
  dplyr::arrange(desc(estimate)) |>  
  writexl::write_xlsx("tables/ef_modelo_sm.xlsx") # |> 
  #xtable::xtable()

## Efeitos marginais do modelo 2

broom::tidy(ef_modelo_rm) |> 
  dplyr::arrange(desc(estimate)) |> 
  writexl::write_xlsx("tables/ef_modelo_rm.xlsx") # |> 
  #xtable::xtable()

# Estimação do `conditional predicted average marginal effects`

## A função abaixo calcula os os efeitos marginais condicionais para ambas
## as linhas de pobreza e salva os gráficos

estimate_margins <- function(var_name = "",
                             xlabel = "", 
                             ylabel = "", 
                             plot_name = ""
                             ){
  # Modelo 1
  
  cplot_sm <- margins::cplot(modelo_sm, var_name, draw = FALSE)
  
  # Modelo 2
  
  cplot_rm <- margins::cplot(modelo_rm, var_name, draw = FALSE)
  
  # Combina os dois `tibbles`
  
  cplot_binned <- dplyr::bind_rows(
    "Linha de pobreza: Salário mínimo" = cplot_sm,
    "Linha de pobreza: Renda domiciliar *Per capita*" = cplot_rm,
    .id = "id"
    )
  
  # Gera o gráfico
  
  gen_plot <- cplot_binned |> 
    dplyr::group_by(id) |> 
    ggplot2::ggplot(aes(x = xvals))+
    ggplot2::geom_line(aes(y = yvals), color = "black", linewidth = 1) +
    ggplot2::geom_line(aes(y = upper), linetype = 2, color = "red", linewidth = 1) +
    ggplot2::geom_line(aes(y = lower), linetype = 2, color = "red", linewidth = 1) +
    ggplot2::geom_hline(yintercept = 0)+
    ggplot2::facet_wrap(~id)+
    ggplot2::theme_classic()+
    ggplot2::theme(
      strip.text = ggtext::element_markdown(size = 18),
      strip.background = element_blank(),
      axis.text = element_text(color = "black", family = "custom", size = 14),
      axis.title = element_text(color = "black", family = "custom", size = 16)
      )+
    ggplot2::ylim(0, 1)+
    ggplot2::labs(
      color = "Linha de pobreza",
      x = xlabel,
      y = ylabel
      )
  
  # Salva o gráfico
  
  ggplot2::ggsave(
    plot = gen_plot,
    filename = plot_name, 
    width = 14, 
    height = 8, 
    dpi = 200, 
    bg = "white",
    path = "plots/",
    units = "in"
    )
  
  return(gen_plot)
}


# Gera os gráficos dos efeitos marginais ----------------------------------

## Anos de estudo do chefe de família

estimate_margins(var_name = "ano_estudo_cf",
                 xlabel = "Anos de estudo",
                 ylabel = "Probabilidade",
                 plot_name = "efeitos_marginais_anos_estudo.png"
                 )

## Sexo do chefe de família

estimate_margins(var_name = "sexo_cf",
                 xlabel = "Sexo",
                 ylabel = "Probabilidade",
                 plot_name = "efeitos_marginais_sexo_cf.png"
                 )

## Idade do chefe de família

estimate_margins(var_name = "idade_cf",
                 xlabel = "Idade",
                 ylabel = "Probabilidade",
                 plot_name = "efeitos_marginais_idade_cf.png"
                 )

## Raça do chefe de família

estimate_margins(var_name = "raca_cf",
                 xlabel = "Raça",
                 ylabel = "Probabilidade",
                 plot_name = "efeitos_marginais_raca_cf.png"
                 )

## Quantidade de membros da família

estimate_margins(var_name = "num_membros",
                 xlabel = "Número de membros da família",
                 ylabel = "Probabilidade",
                 plot_name = "efeitos_marginais_num_membros.png"
                 )

## Trabalha na agricultura

estimate_margins(var_name = "trab_agric",
                 xlabel = "Trabalha na agricultura",
                 ylabel = "Probabilidade",
                 plot_name = "efeitos_marginais_trab_agric.png"
                 )

## Localização do domicílio

estimate_margins(var_name = "local_dom",
                 xlabel = "Localização do domicílio (Zona)",
                 ylabel = "Probabilidade",
                 plot_name = "efeitos_marginais_local_dom.png"
                 )

## Região de localização do domicílio

estimate_margins(var_name = "regiao_dom",
                 xlabel = "Região de localização do domicílio (Região)",
                 ylabel = "Probabilidade",
                 plot_name = "efeitos_marginais_regiao_dom.png"
                 )
