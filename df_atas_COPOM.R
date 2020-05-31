library(jsonlite)
library(RCurl)
library(rvest)
library(tidyverse)
library(pdftools)
library(glue)
library(magrittr)

url <- "https://www.bcb.gov.br"
api_antigas <- "/api/servico/sitebcb/atascopom-conteudo/ultimas?quantidade=1000&filtro="
api_novas <- "/api/servico/sitebcb/atascopom/ultimas?quantidade=1000&filtro="

json_antigas <- getURL(glue("{url}{api_antigas}"))
json_novas <- getURL(glue("{url}{api_novas}"))

df_atas_antigas <- fromJSON(json_antigas, flatten = TRUE) %>%
  .$conteudo %>%
  select(DataReferencia, Titulo, LinkPagina) %>%
  mutate(Tipo = "txt") %$%
  glimpse(.)

df_atas_novas <- fromJSON(json_novas, flatten = TRUE) %>%
  .$conteudo %>%
  select(DataReferencia, Titulo, LinkPagina = Url) %>%
  mutate(Tipo = "pdf") %$%
  glimpse(.)

df_atas <- bind_rows(df_atas_novas, df_atas_antigas) %$%
  glimpse(.)

conteudo_txt <- function(link, url = "https://www.bcb.gov.br") {
  api <- "/api/servico/sitebcb/atascopom-conteudo/principal?filtro=IdentificadorUrl"
  codigo <- str_extract_all(link, "\\d+")
  codigo <- URLencode(glue(" eq '{codigo}'"), reserved = T)
  
  json <- glue("{url}{api}{codigo}") %>%
    fromJSON()
  
  txt <- read_html(json$conteudo$OutrasInformacoes) %>%
    html_text() %>%
    str_squish()
  
  return(txt)
}

conteudo_pdf <- function(link, url = "https://www.bcb.gov.br") {
  pdf <- pdf_text(glue("{url}{link}")) %>%
    as_vector() %>%
    glue_collapse()
  
  return(pdf)
}

final <- df_atas %>%
  mutate(
    integra = map2_chr(Tipo, LinkPagina, ~ {
      if (.x == "pdf") {
        return(conteudo_pdf(.y))
      }

      return(conteudo_txt(.y))
    })
  )

glimpse(final)

fst::write_fst(final, "./output/df_atas.fst")