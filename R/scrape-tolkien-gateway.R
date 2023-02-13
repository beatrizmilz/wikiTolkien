tolkien_gateway_infobox <- function(url_page = "https://tolkiengateway.net/wiki/Tar-Ciryatan"){

  # URL of the webpage we want to scrape
  url_page

  # make a GET requisition
  get_result <- httr::GET(url_page)

  # read results from GET requisition
  get_content <- httr::content(get_result)


 raw_table <- get_content |>
    # get elements with class tginfobox
    rvest::html_nodes(".tginfobox") |>
   # extract table
    rvest::html_table() |>
   # get first element from list
    purrr::pluck(1)


 raw_table |>
   dplyr::rename(var = 1, value = 2) |>
   dplyr::mutate(character = unique(names(raw_table)),
                 webpage = url_page)

}

