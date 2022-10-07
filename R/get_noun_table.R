
#' Get Noun Table
#'
#' Look up the Greek declension table for a given noun
#'
#' @param english_noun The English noun to lookup
#' @param greek_noun The Greek noun to lookup
#' @param add_vocative Logicial. The vocative case is usually not needed.
#'
#' @returns A tibble
#'
#' @details Greek nouns take on different endings or declensions depending
#' on case and number. This contrasts to English which usually changes by
#' number only, often with the addition of an s on the end. To learn Greek you
#' need to learn all the declensions.

get_noun_table <- function (english_noun, greek_noun, add_vocative = FALSE) {

  # Extract data from the noun table

  wiktionary_url <- paste0("https://el.wiktionary.org/wiki/", greek_noun)

  declension_raw_tbl <- wiktionary_url |>
    read_html() |>
    html_elements("table") |>
    html_table() |>
    extract2(1) |>
    clean_names()

  # Defines the cases. Reordered to match the order on anki

  cases <- c("nominative", "genitive", "accusative", "vocative")
  cases_reordered <- c("nominative", "accusative", "genitive", "vocative")

  # Figures out the noun gender, a feature of Greek

  genders <- c("m" = "ο","f" =  "η","n" = "το")
  gender <- unlist(declension_raw_tbl[1, "enikos"])
  gender <- names(genders)[genders == gender]

  # Build the declension table

  declension_tbl <- tibble(case = c(cases, cases),
                           number = rep(c("singular", "plural"), each = 4),
                           noun = c(declension_raw_tbl$enikos_2[1:4],
                                    declension_raw_tbl$plethyntikos_2[1:4])) |>
    mutate(noun = str_replace_all(noun, "[:punct:]", ""),
           case = factor(case, levels = cases_reordered)) |>
    arrange(case, desc(number)) |>
    pivot_wider(names_from = c(case, number), values_from = noun)

  # Usually the vocative doesnt make much sense to learn or is the same as
  # another case but sometimes it is useful

  if (add_vocative == FALSE) {

    declension_tbl$vocative_singular <- ""
    declension_tbl$vocative_plural <- ""

  }

  # Reformats into a good format for upload to anki

  declension_tbl <- tibble(gender, english_noun, noun_image = "") |>
    bind_cols(declension_tbl)

  return(declension_tbl)

}

