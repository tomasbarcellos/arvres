#' Particiona os dados dado uma questao
#'
#' @param .data um data.frame
#' @param questao uma condicao a ser verificada
#'
#' @return uma lista correspondendo as particoes
#' @export
#'
#' @examples
#' particionar(iris, Sepal.Length > 5)
#' particionar(dados_treino, label == "Apple")
particionar <- function(.data, questao) {
  predicado <- dplyr::enquo(questao)

  list(
      dplyr::filter(.data, !!predicado),
      dplyr::filter(.data, ! (!!predicado))
    )
}

#' Impuridade de Gini
#'
#' @param x um vetor
#'
#' @return um numerico
#' @export
#'
#' @examples
#' gini(c("Apple", "Apple"))
#' gini(c("Apple", "Orange"))
#' gini(c("Apple", "Orange", "Grape", "Grapefruit", "Blueberry"))
gini <- function(x) {
  contagem <- table(x)
  1 - sum((contagem / length(x))^2)
}


#' Title
#'
#' @param esq
#' @param dir
#' @param atual
#'
#' @return
#' @export
#'
#' @examples
#' atual <- gini(dados_treino$label)
#'
#' listas <- particionar(dados_treino, color == "Green")
#' ganho_info(listas[[1]], listas[[2]], atual)
#'
#' listas2 <- particionar(dados_treino, color == "Red")
#' ganho_info(listas2[[1]], listas2[[2]], atual)
ganho_info <- function(esq, dir, atual) {
  prob <- nrow(esq) / (nrow(esq) + nrow(dir))
  p_esq <- prob * gini(esq$label)
  p_dir <- (1 - prob) * gini(dir$label)

  atual - p_esq - p_dir
}

#' Melhor particao de um data.frame
#'
#' @param df um data.frame
#'
#' @return uma lista
#' @export
#'
#' @examples
#' res <- melhor_particao(dados_treino)
#' res[[2]]
#' eval(res[[2]], df)
melhor_particao <- function(df) {
  melhor_ganho <- 0
  melhor_questao <- NULL
  incerteza_atual <- gini(df$label)
  n_features <- length(df) - 1 # tira a coluna com as respostas

  for (col in seq_len(n_features)) {
    valores <- unique(df[[col]])

    for (valor in valores) {
      char <- is.character(valor)
      if (char) {
        predicado <- df[[col]] == valor
      } else {
        predicado <- df[[col]] >= valor
      }

      resp <- particionar(df, predicado)

      if (any(purrr::map_lgl(resp, ~nrow(.x) == 0))) next

      ganho <- ganho_info(resp[[1]], resp[[2]], incerteza_atual)

      if (ganho >= melhor_ganho) {
        melhor_ganho <- ganho
        melhor_questao <- parse(
          text = glue::glue(
            '{names(df)[col]} ', ifelse(char, "== '{valor}'", ">= {valor}")
          )
        )
      }
    }
  }

  list(melhor_ganho, melhor_questao)
}

folha <- function(df) {
  structure(
    table(df$label), class = c("folha", "table")
  )
}

print.folha <- function(x, ...) {
  cat("<folha>")
  NextMethod("print", x)
}

no_decisao <- function(questao, sim, nao) {
  questao <- as.character(questao)
  questao <- gsub('"', "'", questao)
  structure(
    list(questao = questao, sim = sim, nao = nao),
    class = "no"
  )
}

print.no <- function(x, ...) {
  cat("<nó>\n")
  cat(x$questao, "\n")
  cat("sim:")
  print(x$sim)
  cat("não:")
  print(x$nao)
}

#' Construir Arvore de Decisao
#'
#' @param df
#'
#' @return uma arvore de decisao
#' @export
#'
#' @examples
#' arvre <- construir_arvore(df)
#' arvre
construir_arvore <- function(df) {
  mpart <- melhor_particao(df)

  if (mpart[[1]] == 0) return(folha(df))

  partes <- particionar(df, eval(mpart[[2]]))

  galho_sim <- construir_arvore(partes[[1]])
  galho_nao <- construir_arvore(partes[[2]])

  structure(
    no_decisao(mpart[[2]], galho_sim, galho_nao),
    class = "no"
  )
}
