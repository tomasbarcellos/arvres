dados_treino <- tibble::tribble(
  ~color, ~diameter, ~label,
  "Green", 3, "Apple",
  "Yellow", 3, "Apple",
  "Red", 1, "Grape",
  "Red", 1, "Grape",
  "Yellow", 3, "Lemon"
)

usethis::use_data(dados_treino)
