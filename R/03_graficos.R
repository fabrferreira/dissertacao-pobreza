# Importando pacotes ------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(forcats)
library(srvyr)
library(survey)
library(geobr)
library(rlang)

# Distribuição espacial da renda domiciliar per capita dos chefes de família

## Importa e manipula shapefile

shp_estados_br <- geobr::read_state(code_state = "all", year = 2018)

dplyr::glimpse(shp_estados_br)

shp_estados_br <- shp_estados_br |> 
  dplyr::mutate(
    name_state = stringr::str_replace_all(
      string = name_state, 
      pattern = stringr::regex("\\b(Da|De|Do)\\b"),
      replacement = function(match)str_to_lower(match)
      )
    )

## Calcula a renda média do chefe de família por UF

renda_media_cf_uf <- pnadc_data |> 
  dplyr::filter(chefe_domicilio == "Sim") |>
  dplyr::group_by(UF) |>
  dplyr::summarise(renda_media = round(srvyr::survey_mean(VD5008, na.rm = TRUE), 2))

## Faz o `join` entre as duas informações

renda_media_uf_shp <- dplyr::inner_join(
  x = shp_estados_br, 
  y = renda_media_cf_uf, 
  by = c("name_state" = "UF")
  )

## Plota o mapa

renda_uf_map <- renda_media_uf_shp |> 
  dplyr::filter(name_state != "Distrito Federal") |> 
  ggplot2::ggplot()+
  ggplot2::geom_sf(aes(fill = renda_media), color = NA, linewidth = 0.15)+
  ggthemes::theme_map()+
  ggplot2::theme(
    legend.position = "left",
    legend.justification = c("right", "bottom"),
    legend.box.just = "right",
    legend.title = element_text(vjust = 0.5, size = 10),
    legend.box.background = element_rect(color = "black", linewidth = 0.8),
    legend.box.margin = margin(.1, .1, .1, .1),
    legend.text = element_text(size = 10)
    )+
  ggplot2::geom_sf_text(data = renda_media_uf_shp |> 
                          dplyr::filter(name_state != "Distrito Federal"), 
                        aes(label = name_state, 
                            hjust = "middle",
                            vjust = "middle"),
                        size = 3.5)+
  ggplot2::scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8,"Purples"),
                                na.value = "#A9A9A9",
                                labels = scales::dollar_format(prefix = "R$ ",
                                                               big.mark = ".",
                                                               decimal.mark = ",",
                                                               accuracy = 0.01
                                                               )
                                )+
  ggplot2::labs(fill = "Renda em reais")

plot(renda_uf_map)

ggplot2::ggsave(
  plot = renda_uf_map, 
  path = "plots/",
  filename = "renda_uf_map.png",
  width = 12, 
  height = 7, 
  dpi = 150,
  bg = "white",
  units = "in"
  )

# Renda média por características do chefe de família

## Esta função permite facilitar o calculo das estatísticas por característica do chefe

calc_agg_stats <- function(data, var1, var2){
  {{ data }} |> 
    dplyr::filter(chefe_domicilio == "Sim") |> 
    dplyr::group_by({{ var1 }}, {{ var2 }}) |> 
    dplyr::summarise(renda_media = srvyr::survey_mean(VD5008, na.rm = TRUE)) |> 
    tidyr::drop_na() |> 
    srvyr::mutate_if(is.numeric, round, 2)
}

## Renda média do chefe de família por sexo e raça

## Dados

renda_sexo_raca <- calc_agg_stats(data = pnadc_data, 
                                     var1 = sexo,
                                     var2 = raca
                                     )

## Plot

plot_renda_sexo_raca <- renda_sexo_raca |> 
  ggplot2::ggplot(aes(x = forcats::fct_reorder(raca, renda_media), y = renda_media, fill = sexo))+
  ggplot2::geom_col()+
  ggplot2::theme_classic()+
  ggplot2::theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 10, colour = "black"),
    axis.text.y = element_text(size = 10, colour = "black")
    )+
  ggplot2::facet_wrap(facets = ~ sexo, strip.position = "top")+
  ggplot2::geom_text(aes(label = scales::dollar(x = renda_media, 
                                                prefix = "R$",
                                                accuracy = 0.01,
                                                big.mark = ".", 
                                                decimal.mark = ",")
                         ),
                     vjust = -0.5, 
                     size = 3)+
  ggplot2::scale_fill_manual(values = c("Homem" = "#282f6b", "Mulher" = "#b22200"))+
  ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ",
                                                             big.mark = ".",
                                                             decimal.mark = ",", 
                                                             accuracy = 0.01
                                                             ))+
  ggplot2::labs(
    x = NULL,
    y = NULL
    )

plot(plot_renda_sexo_raca)

ggsave(
  plot = plot_renda_sexo_raca,
  path = "plots/",
  filename = "plot_renda_sexo_raca.png",
  width = 12,
  height = 7,
  dpi = 150,
  bg = "white",
  units = "in"
  )

# Renda média do chefe de família por nível de instrução

## Dados

renda_instrucao <- calc_agg_stats(data = pnadc_data,
                                     var1 = nivel_instrucao,
                                     var2 = NULL
                                     )

## Plot

plot_renda_instrucao <- renda_instrucao |> 
  ggplot2::ggplot(aes(x = nivel_instrucao, y = renda_media))+
  ggplot2::geom_col(fill = "#282f6b")+
  ggplot2::coord_flip()+
  ggplot2::theme_classic()+
  ggplot2::theme(
    axis.text.x = element_text(size = 10, colour = "black"),
    axis.text.y = element_text(size = 10, colour = "black")
    )+
  ggplot2::geom_text(aes(label = scales::dollar(x = renda_media,
                                                prefix = "R$ ", 
                                                accuracy = 0.01,
                                                big.mark = ".",
                                                decimal.mark = ",")
                         ),
                     hjust = "inward",
                     size = 4)+
  ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ",
                                                             big.mark = ".",
                                                             decimal.mark = ",", 
                                                             accuracy = 0.01)
                              )+
  ggplot2::labs(
    x = NULL,
    y = NULL
    )

plot(plot_renda_instrucao)

ggplot2::ggsave(
  plot = plot_renda_instrucao,
  path = "plots/",
  filename = "plot_renda_instrucao.png",
  width = 12,
  height = 7,
  dpi = 150,
  bg = "white",
  units = "in"
  )

# Renda média do chefe de família por nível de instrução e sexo

## Dados

renda_sexo_instrucao <- calc_agg_stats(data = pnadc_data,
                                       var1 = nivel_instrucao,
                                       var2 = sexo
                                       )

### Plot

plot_renda_sexo_instrucao <- renda_sexo_instrucao |> 
  ggplot2::ggplot(aes(x = nivel_instrucao, y = renda_media, fill = sexo))+
  ggplot2::geom_col()+
  ggplot2::facet_wrap(facets = ~ sexo)+
  ggplot2::coord_flip()+
  ggplot2::theme_classic()+
  ggplot2::theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.placement = "outside"
    )+
  ggplot2::geom_text(aes(label = scales::dollar(x = renda_media,
                                                prefix = "R$ ",
                                                accuracy = 0.01, 
                                                big.mark = ".",
                                                decimal.mark = ",")
                         ),
                     hjust = "inward")+
  ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ",
                                                             big.mark = ".",
                                                             decimal.mark = ",",
                                                             accuracy = 0.01)
                              )+
  ggplot2::scale_fill_manual(values = c("Homem" = "#282f6b", "Mulher" = "#b22200"))+
  ggplot2::labs(
    x = NULL,
    y = NULL
    )

plot(plot_renda_sexo_instrucao)

ggplot2::ggsave(
  plot = plot_renda_sexo_instrucao,
  path = "plots/",
  filename = "plot_renda_sexo_instrucao.png",
  width = 12,
  height = 7,
  dpi = 150,
  bg = "white",
  units = "in"
  )

# Household head mean income by instruction level and region

## Dados

renda_regiao_instrucao <- calc_agg_stats(data = pnadc_data,
                                            var1 = nivel_instrucao,
                                            var2 = regiao
                                            )

## Plot

plot_renda_regiao_instrucao <- renda_regiao_instrucao |> 
  ggplot2::ggplot(aes(x = nivel_instrucao, y = renda_media, fill = regiao))+
  ggplot2::geom_col()+
  ggplot2::facet_wrap(facets = ~ regiao)+
  ggplot2::coord_flip()+
  ggplot2::theme_classic()+
  ggplot2::theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.placement = "outside"
    )+
  ggplot2::geom_text(aes(label = scales::dollar(x = renda_media,
                                                prefix = "R$ ", 
                                                accuracy = 0.01, 
                                                big.mark = ".",
                                                decimal.mark = ",")
                         ), hjust = "inward")+
  ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ",
                                                             big.mark = ".", 
                                                             decimal.mark = ",",
                                                             accuracy = 0.01)
                              )+
  ggplot2::labs(
    x = NULL,
    y = NULL
    )

plot(plot_renda_regiao_instrucao)

ggplot2::ggsave(
  plot = plot_renda_regiao_instrucao,
  path = "plots/",
  filename = "plot_renda_regiao_instrucao.png",
  width = 12,
  height = 7,
  dpi = 150,
  bg = "white",
  units = "in"
  )