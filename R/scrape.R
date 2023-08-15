library(tidyverse)
library(rvest)
library(janitor)

url <- "https://www.tudocelular.com/celulares/fichas-tecnicas_1.html?o=2"
pag <- read_html(url)

n_pages <- 1:30
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
  distinct(.keep_all = TRUE)

tictoc::tic()
infos <- df_url %>%
  head(250) %>%
  mutate(urls = str_c("https://www.tudocelular.com", urls)) %>%
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

        ano <- page %>%
          html_element("#phone_columns .phone_column_features:nth-child(1) li:nth-child(2)") %>%
          html_text()

        dimensao <- page %>%
          html_element("#phone_columns .phone_column_features:nth-child(1) li:nth-child(3)") %>%
          html_text()

        peso <- page %>%
          html_element("#phone_columns .phone_column_features:nth-child(1) li:nth-child(4)") %>%
          html_text()

        custo_beneficio <- page %>%
          html_element(".phone_column_features:nth-child(2) li:nth-child(1)") %>%
          html_text()

        hardware_nota <- page %>%
          html_element(".phone_column_features:nth-child(2) li:nth-child(2)") %>%
          html_text()

        tela_nota <- page %>%
          html_element(".phone_column_features:nth-child(2) li:nth-child(3)") %>%
          html_text()

        camera_nota <- page %>%
          html_element(".phone_column_features:nth-child(2) li:nth-child(4)") %>%
          html_text()

        desempenho_nota <- page %>%
          html_element(".phone_column_features:nth-child(2) li:nth-child(5)") %>%
          html_text()

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
  mutate(
    across(c(memoria_max, min_preco),
           \(x) parse_number(x, locale = locale(grouping_mark = "."))),
    peso = parse_number(peso, locale = locale(decimal_mark = ".")),
    ram = case_when(str_detect(ram, "GB") ~ parse_number(ram),
                    str_detect(ram, "MB") ~ NA_integer_), # nao serao usados ceulares com < 1gb de ram
    marca = str_extract(celular, "^[:alpha:]+"),
    across(c(custo_beneficio, ends_with("nota")),
           \(x) parse_number(x)),
  ) %>%
  filter(!is.na(ram)) %>%
  distinct(.keep_all = TRUE)

skimr::skim(infos_parsed)

infos_parsed %>%
  DataExplorer::plot_histogram()


# ram de 512mb sendo convertida para 512gb (criar função especifica)

read_html("https://www.tudocelular.com/Redmi/fichas-tecnicas/n7730/Redmi-K50.html") %>%
  html_elements("#phone_columns .phone_column_features:nth-child(3) li:nth-child(1)") %>%
  html_text()



