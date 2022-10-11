
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
#' @export

get_noun_table <- function (english_noun, greek_noun, add_vocative = FALSE) {

  # Find the noun table

  wiktionary_tables <- .download_tables(greek_noun)
  table_num <- .find_noun_table(wiktionary_tables)

  if (table_num <= length(wiktionary_tables)) {

    noun_table_raw <- wiktionary_tables[[table_num]] |> clean_names()
    table_format <- .find_table_format(noun_table_raw)

  } else {

    table_format <- "indeclinable"

  }


  if (table_format == "standard") {

    noun_table <- .format_standard(noun_table_raw, english_word, greek_word)

  } else if (table_format == "indeclinable") {

    noun_table <- .format_indeclinable(noun_table_raw, english_word,
                                           greek_word)

  } else if (table_format == "multi_gender") {

    noun_table <- .format_multi_gender(noun_table_raw, english_word,
                                           greek_word)

  }  else {

    warning(paste0("Unknown format: ", english_word, ", ", greek_noun))
    noun_table <- .format_unknown(noun_table_raw, english_noun, greek_noun)

  }

  # Usually the vocative doesnt make much sense to learn or is the same as
  # another case but sometimes it is useful

  if (add_vocative == FALSE) {

    noun_table$vocative_singular <- ""
    noun_table$vocative_plural <- ""

  }

  # Reformats into a good format for upload to anki

  return(noun_table)

}


.download_tables <- function (greek_noun) {

  wiktionary_url <- paste0("https://el.wiktionary.org/wiki/", greek_noun)
  wiktionary_html <- read_html(wiktionary_url)
  wiktionary_tables <- wiktionary_html |> html_elements("table") |> html_table()

  return(wiktionary_tables)

}





.find_noun_table <- function (wiktionary_tables) {

  num_tables <- length(wiktionary_tables)
  match_col_name <- "ενικός"

  for (i in 1:num_tables) {

    col_names <- colnames(wiktionary_tables[[i]])

    if (match_col_name %in% col_names)  break

    # No table found will class as indelcinable. Add 1 to i . When we use i
    # in the main function to figure out the position, i will be bigger than
    # the length so we known no table was found i.e. indeclinable.

    if (i == num_tables) i <- i + 1

  }

  return (i)

}

.find_table_format <- function () {

  if (table_num > length(wiktionary_tables)) {

    table_format <- "indeclinable"

  } else {

    noun_table_raw <- wiktionary_tables |> pluck(table_num) |> clean_names()

    # Figure out if table is standard, multi_gender or unknown

    standard_cols <- c("ptoseis", "enikos", "enikos_2", "plethyntikos",
                       "plethyntikos_2")

    multi_cols <- c("ptoseis", "enikos", "enikos_2", "enikos_3", "enikos_4",
                    "enikos_5", "enikos_6")

  }




}



.format_standard <- function (noun_table_raw, english_noun, greek_noun) {

  # Defines the cases. Reordered to match the order on anki

  cases <- c("nominative", "genitive", "accusative", "vocative")
  cases_reordered <- c("nominative", "accusative", "genitive", "vocative")

  # Figures out the noun gender, a feature of Greek

  genders <- c("m" = "ο","f" = "η","n" = "το")
  gender <- unlist(noun_table_raw[1, "enikos"])
  gender <- names(genders)[genders == gender]

  # Build the declension table

  case <- c(cases, cases)
  number <- rep(c("singular", "plural"), each = 4)
  noun <- c(noun_table_raw$enikos_2[1:4], noun_table_raw$plethyntikos_2[1:4])

  noun_table <- tibble(case, number, noun) |>

    # By setting case as a factor we can use the levels to reorder in the
    # desired way

    mutate(noun = str_replace_all(noun, "[:punct:]", ""),
           case = factor(case, levels = cases_reordered)) |>
    arrange(case, desc(number)) |>
    pivot_wider(names_from = c(case, number), values_from = noun)

  noun_table <- tibble(gender, english_noun, noun_image = "") |>
    bind_cols(noun_table)

  return(noun_table)

}

.format_multi_gender <- function (raw_tbl) {

}


#' Format indeclinable (Helper)
#'
#' Formats noun table for indeclinable nouns, typically foreign word
#'
#' @param english_noun The English noun to lookup
#' @param greek_noun The Greek noun to lookup
#'
#' @returns A tibble
#'
#' @details Foreign words are typically indeclineable in Greek. This function is
#' called when no declension table is found in the wiktionary page html.

.format_indeclinable <- function (english_noun, greek_noun) {

  tibble(gender = "n",
         english_noun,
         noun_image = "",
         nominative_singular = greek_noun,
         nominative_plural = greek_noun,
         accusative_singular = greek_noun,
         accusative_plural = greek_noun,
         genitive_singular = greek_noun,
         genitive_plural = greek_noun,
         vocative_singular = greek_noun,
         vocative_plural = greek_noun)

}

.test_format_noun <- function (english_noun, greek_noun, f) {

  wiktionary_tables <- .download_tables(greek_noun)
  table_num <- .find_noun_table(wiktionary_tables)

  if (table_num > length(wiktionary_tables)) {

    noun_table <- f(english_noun, greek_noun)

  } else {

    noun_table_raw <- clean_names(wiktionary_tables[[table_num]])
    noun_table <- f(noun_table_raw, english_noun, greek_noun)

  }

  return(noun_table)

}

