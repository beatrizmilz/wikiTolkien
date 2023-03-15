da_characters <- tolkien_get_pages_by_book() |>
  dplyr::mutate(
    characters = purrr::map(link, tolkien_get_characters_by_page)
  ) |>
  tidyr::unnest(characters)

links_unicos <- unique(da_characters$link_character)

length(links_unicos)

# link <- sample(links_unicos, 1)

resultados <- purrr::map(
  links_unicos,
  tolkien_get_character,
  .progress = TRUE
)

resultados |>
  purrr::list_rbind(names_to = "id") |>
  janitor::clean_names() |>
  dplyr::glimpse()

# ... usethis::use_data()

