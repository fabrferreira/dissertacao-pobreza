# Importando pacotes ------------------------------------------------------

library(dplyr)
library(ggplot2)
library(srvyr)
library(survey)
library(rlang)
library(margins)
library(convey)
library(xtable)
library(stargazer)

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

efeitos_modelo_sm <- margins::margins(model = modelo_sm, data = pnadc)

efeitos_modelo_rm <- margins::margins(model = modelo_rm, data = pnadc)

## Exportando os efeitos marginais em formato \LaTeX



