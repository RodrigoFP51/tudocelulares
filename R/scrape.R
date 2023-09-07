library(tidyverse)
library(rvest)
library(janitor)

get_info <- function(pag, css_path){
  pag %>%
    html_element(css_path) %>%
    html_text()
}

scrape_ano_mes <- function(pag){

  possible_paths <- c(
    "#phone_columns .phone_column_features:nth-child(1) li:nth-child(2)",
    "#phone_columns .phone_column_features:nth-child(1) li:nth-child(3)"
  )

  ano_x <- pag %>%
    html_element(possible_paths[1]) %>%
    html_text()

  if(ano_x != "Fold Out"){
    return(ano_x)
  } else {
    ano_x <- pag %>%
      html_element(possible_paths[2]) %>%
      html_text()
    return(ano_x)
  }

}

scrape_desempenho <- function(pag){

  possible_paths <- c(
    ".phone_column_features:nth-child(3) li:nth-child(5)",
    ".phone_column_features:nth-child(3) li:nth-child(4)"
  )

  nota_x <- get_info(pag, possible_paths[1])

  case_when(
    is.na(nota_x) ~ get_info(pag, possible_paths[2]),
    TRUE ~ nota_x
  )
}

url <- "https://www.tudocelular.com/celulares/fichas-tecnicas_1.html?o=2"
pag <- read_html(url)

n_pages <- 1:80
url_teste <-
  glue::glue("https://www.tudocelular.com/celulares/fichas-tecnicas_{n_pages}.html?o=2")

df_url <-
  map_df(url_teste, \(x){
    x %>%
      read_html() %>%
      html_nodes("a.pic") %>%
      html_attr("href") %>%
      tibble(urls = .)
  }) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(urls = str_c("https://www.tudocelular.com", urls))

tictoc::tic()
infos <- df_url %>%
  slice(1:600) %>%
  mutate(
    info = map(
      urls,
      function(x){
        page <- read_html(x)

        celular <- page %>%
          html_element("h2") %>%
          html_text()

        min_preco <- page %>%
          html_element("li:nth-child(1) .hoverred b") %>%
          html_text()

        ano <- scrape_ano_mes(page)

        dimensao <- page %>%
          html_element("#phone_columns .phone_column_features:nth-child(1) li:nth-child(3)") %>%
          html_text()

        peso <- page %>%
          html_element("#phone_columns .phone_column_features:nth-child(1) li:nth-child(4)") %>%
          html_text()

        custo_beneficio <- page %>%
          html_element("#phone_columns .phone_column_features:nth-child(3) li:nth-child(1)") %>%
          html_text()

        hardware_nota <- page %>%
          html_element("#phone_columns .phone_column_features:nth-child(3) li:nth-child(2)") %>%
          html_text()

        tela_nota <- page %>%
          html_element("#phone_columns .phone_column_features:nth-child(3) li:nth-child(3)") %>%
          html_text()

        camera_nota <- page %>%
          html_element(".phone_column_features:nth-child(3) li:nth-child(4)") %>%
          html_text()

        desempenho_nota <- scrape_desempenho(page)

        ram <- page %>%
          html_element("#phone_columns .phone_column_features:nth-child(5) li:nth-child(5)") %>%
          html_text()

        memoria_max <- page %>%
          html_element("#phone_columns .phone_column_features:nth-child(5) li:nth-child(6)") %>%
          html_text()

        info_tbl <- tibble(
          celular = celular,
          min_preco = min_preco,
          ano = ano,
          dimensao = dimensao,
          peso = peso,
          custo_beneficio = custo_beneficio,
          hardware_nota = hardware_nota,
          tela_nota = tela_nota,
          camera_nota = camera_nota,
          desempenho_nota = desempenho_nota,
          ram = ram,
          memoria_max = memoria_max
        )

        return(info_tbl)
      }
    )
  )
tictoc::toc()

infos %>%
  unnest(info) %>%
  readr::write_rds("dados/dados_raw.rds")

infos_parsed <- infos %>%
  unnest(info) %>%
  filter(!str_detect(celular, "Z [Flip|Fold]")) %>%
  mutate(
    min_preco = replace_na(min_preco, "0"),
    across(c(memoria_max, min_preco),
           \(x) parse_number(x, locale = locale(grouping_mark = "."))),
    peso = parse_number(peso, locale = locale(decimal_mark = ".")),
    ram = case_when(str_detect(ram, "GB") ~ parse_number(ram),
                    str_detect(ram, "MB") ~ NA_integer_), # nao serao usados ceulares com < 1gb de ram
    marca = str_extract(celular, "^[:alpha:]+"),
    across(c(custo_beneficio, ends_with("nota")),
           \(x) parse_number(x))
  ) %>%
  filter(peso <= 250, ram < 200,
         !is.na(ram)) %>%
  separate_wider_delim(ano, delim = "/",
                       names = c("ano", "mes")) %>%
  separate_wider_delim(dimensao, delim = "x",
                       names = c("altura", "largura", "espessura"), too_few = "align_end") %>%
  mutate(across(ano:espessura, readr::parse_number)) %>%
  #filter(!is.na(custo_beneficio)) %>%
  distinct(.keep_all = TRUE)

readr::write_rds(infos_parsed,
                 "dados/dados_arrumados.rds")

infos_parsed <- read_rds("dados/dados_arrumados.rds")

skimr::skim(infos_parsed)

infos_parsed %>%
  DataExplorer::plot_histogram(geom_histogram_args = list(fill = "darkorange", color = "white"))


# ram de 512mb sendo convertida para 512gb (criar função especifica)
#


infos_parsed %>%
  DataExplorer::plot_missing()

infos_parsed %>%
  keep(is.numeric) %>%
  filter(peso <= 250) %>%
  GGally::ggpairs(lower = list(continuous = "smooth"))

infos_parsed %>%
  #DataExplorer::dummify(maxcat = 30)
  select(marca, where(is.numeric)) %>%
  mutate(marca = fct_lump(marca, n = 9, other_level = "Outro")) %>%
  ppsr::visualize_pps(y = "custo_beneficio",
                      color_value_low = "#8abef2",
                      include_target = FALSE)

infos_parsed %>%
  drop_na() %>%
  keep(is.numeric) %>%
  lm(custo_beneficio ~ ., data=.) %>%
  summary()


