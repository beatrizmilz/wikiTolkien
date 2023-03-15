tolkien_get_character <- function(link = "https://tolkiengateway.net/wiki/Tar-Ciryatan"){

  # URL of the webpage we want to scrape
  link

  # make a GET requisition
  get_result <- httr::GET(link)

  # read results from GET requisition
  get_content <- httr::content(get_result)


  tabela <- get_content |>
    # get elements with class tginfobox
    rvest::html_nodes(".tginfobox")

  if(length(tabela) == 0) return (tibble::tibble(
    character = URLdecode(basename(link)),
    webpage = url_page
  ))

  link_image <- tabela |>
    xml2::xml_find_first(".//img") |>
    xml2::xml_attr("src")

  raw_table <- tabela |>
    # extract table
    rvest::html_table() |>
    # get first element from list
    purrr::pluck(1)

  raw_table |>
    dplyr::rename(var = 1, value = 2) |>
    tibble::add_row(
      var = "img_link",
      value = link_image
    ) |>
    dplyr::mutate(
      character = unique(names(raw_table)),
      webpage = url_page
    )

}

#' Get Characters by Page from Tolkien Gateway
#'
#' This function retrieves the character names and their corresponding links from a specified Tolkien Gateway page.
#'
#' @param link_page The URL of the Tolkien Gateway page containing the list of character names.
#'
#' @return A tibble containing two columns: "character" with the character names and "link_character" with the corresponding links.
#' @export
tolkien_get_characters_by_page <- function(link_page) {
  # Get the content of the link_page
  r_page <- link_page |>
    httr::GET()

  # Find all the character links within the "mw-pages" section using XPath
  tag_links_page <- r_page |>
    httr::content() |>
    xml2::xml_find_all("//*[@id='mw-pages']//a")

  # Create a tibble with character names and their corresponding links
  links_characters <- tibble::tibble(
    character = xml2::xml_text(tag_links_page),
    link_character = paste0(
      "https://tolkiengateway.net",
      xml2::xml_attr(tag_links_page, "href")
    )
  )

  # Return the tibble containing character names and links
  links_characters
}

#' Get Pages by Book from Tolkien Gateway
#'
#' This function retrieves the book titles and their corresponding links from the Tolkien Gateway Portal: Characters page.
#'
#' @return A tibble containing two columns: "title" with the book titles and "link" with the corresponding links.
#' @export
tolkien_get_pages_by_book <- function() {

  url_base <- "https://tolkiengateway.net/wiki/Portal:Characters"

  r <- httr::GET(url_base)

  tag_links <- r |>
    httr::content() |>
    xml2::xml_find_all('//*[@id="mw-content-text"]/div/table[2]/tbody/tr/td[2]/div[1]/div[2]/div/div/div[2]/div//a')

  links_pags <- tibble::tibble(
    title = xml2::xml_text(tag_links),
    link = paste0(
      "https://tolkiengateway.net",
      xml2::xml_attr(tag_links, "href")
    )
  )

  links_pags
}


#' Tolkien Gateway: Get Characters by Page
#'
#' This function retrieves the character names and their
#' corresponding links from a specified Tolkien Gateway page.
#'
#' @param link_page The URL of the Tolkien Gateway page containing
#'   the list of character names.
#'
#' @return A tibble containing two columns: "character" with
#'   the character names and "link_character" with the corresponding
#'   links.
#'
#' @export
tolkien_get_characters_by_page <- function(link_page) {
  # Get the content of the link_page
  r_page <- link_page |>
    httr::GET()

  # Find all the character links within the "mw-pages" section using XPath
  tag_links_page <- r_page |>
    httr::content() |>
    xml2::xml_find_all("//*[@id='mw-pages']//a")

  # Create a tibble with character names and their corresponding links
  links_characters <- tibble::tibble(
    character = xml2::xml_text(tag_links_page),
    link_character = paste0(
      "https://tolkiengateway.net",
      xml2::xml_attr(tag_links_page, "href")
    )
  )

  # Return the tibble containing character names and links
  links_characters
}

