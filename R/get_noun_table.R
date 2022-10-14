
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

get_noun_table <- function (english_noun, greek_noun, add_vocative = FALSE,
                            pause = 0.25) {

  # Find the noun table

  wiktionary_tables <- .download_tables(greek_noun)
  table_num <- .find_noun_table_num(wiktionary_tables)
  table_format <- .find_noun_table_format(wiktionary_tables, table_num)

  if (table_format == "indeclinable") {

    noun_table <- .format_indeclinable(english_noun, greek_noun)

  } else {

    noun_table_raw <- wiktionary_tables[[table_num]] |> clean_names()

    if (table_format == "standard") {

      noun_table <- .format_standard(noun_table_raw, english_noun, greek_noun)

    }  else {

      warning(paste0("Unknown format: ", english_noun, ", ", greek_noun))
      noun_table <- .format_indeclinable(english_noun, greek_noun)

    }

  }

  # Usually the vocative doesnt make much sense to learn or is the same as
  # another case but sometimes it is useful

  if (add_vocative == FALSE) {

    noun_table$vocative_singular <- ""
    noun_table$vocative_plural <- ""

  }

  #noun_table <- mutate(noun_table, table_format, .before = english_noun)

  # Reformats into a good format for upload to anki

  Sys.sleep(pause)

  return(noun_table)

}

.download_tables <- function (greek_noun) {

  wiktionary_url <- paste0("https://el.wiktionary.org/wiki/", greek_noun)
  wiktionary_html <- read_html(wiktionary_url)
  wiktionary_tables <- wiktionary_html |> html_elements("table") |> html_table()

  return(wiktionary_tables)

}

.find_noun_table_num <- function (wiktionary_tables) {

  num_tables <- length(wiktionary_tables)

  # This mean plural in Greek. Noun declension tables will have this word in
  # their column names.

  match_col_name <- "πληθυντικός"

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

.find_noun_table_format <- function (wiktionary_tables, table_num) {

  if (table_num > length(wiktionary_tables)) {

    return ("indeclinable")

  } else {

    noun_table_raw <- wiktionary_tables |> pluck(table_num) |> clean_names()

    # Figure out if table is standard, multi_gender or unknown

    standard_cols <- c("ptoseis", "enikos", "enikos_2", "plethyntikos",
                       "plethyntikos_2")

    multi_gender_cols <- c("ptoseis", "enikos", "enikos_2", "enikos_3", "enikos_4",
                           "enikos_5", "enikos_6")

    col_names <- colnames(noun_table_raw)

    if (identical(standard_cols, col_names)) {

      return ("standard")

    } else if (identical(multi_gender_cols, col_names)) {

      return ("multi_gender")

    }

  }

  return ("unknown")

}

.clean_noun <- function (x, doubles = NA) {

  basic_word <- x[1]

  x <- x |>
    str_replace_all("[:punct:]", "") |>
    str_split(pattern = "\\s") |>
    map_chr(1)

  if (!is.na(doubles[1])) {

    x_doubles <- x[doubles]

    root <- str_sub(basic_word, 1, 3)

    root_start_positions <- x_doubles |> str_locate_all(root) |> pluck(1)

    if (nrow(root_start_positions) != 2) {

      warning (paste0("problem with ", basic_word))

      return (x)

    }

    x[doubles] <- str_sub(x_doubles, 1, root_start_positions[2, 1] - 1)

  }

  return (x)

}

.format_standard <- function (noun_table_raw, english_noun, greek_noun) {

  # Figures out the noun gender, a feature of Greek

  genders <- c("m" = "ο","f" = "η","n" = "το", "mf" ="ο/η")
  gender <- unlist(noun_table_raw[1, "enikos"])
  gender <- names(genders)[genders == gender]

  # Build the declension table

  number <- rep(c("singular", "plural"), each = 4)

  # By setting case as a factor we can use the levels to reorder in the
  # desired way

  case <- factor(rep(c("nominative", "genitive", "accusative", "vocative"), 2),
                 levels = c("nominative", "accusative", "genitive", "vocative"))

  # In some tables the entries are written over 2 lines. These get condensed
  # into a single cell with no space between them e.g. συγγραφέας
  # Need to identify these and remove the extra word.

  singular_doubles <- str_detect(noun_table_raw$enikos, "τουτου")
  plural_doubles <- rep(FALSE, 4) # for now related to the doubles

  noun_raw <- c(noun_table_raw$enikos_2[1:4],
                noun_table_raw$plethyntikos_2[1:4])

  doubles <- c(singular_doubles, plural_doubles)

  noun <- .clean_noun(noun_raw, doubles)

  noun_table <- tibble(case, number, noun) |> arrange(case, desc(number))


  if (gender == "mf") {

    noun_table <- mutate(noun_table, noun = paste0(noun, " / η ", noun))

  }

  noun_table <- pivot_wider(noun_table, names_from = c(case, number),
                            values_from = noun)

  noun_table <- tibble(gender, english_noun, noun_image = "") |>
    bind_cols(noun_table)

  return(noun_table)

}

# .format_multi_gender <- function (noun_table_raw, english_noun, greek_noun,
#                                   drop_neutral = TRUE) {
#
#   # Defines the cases. Reordered to match the order on anki
#
#   cases <- c("nominative", "genitive", "accusative", "vocative")
#   cases_reordered <- c("nominative", "accusative", "genitive", "vocative")
#
#   elements <- c(2:5, 8:11)
#
#   masc_nouns <- .clean_noun(noun_table_raw$enikos_2, elements)
#   femm_nouns <- .clean_noun(noun_table_raw$enikos_4, elements)
#   femm_article <- noun_table_raw$enikos_3[elements]
#
#   if (drop_neutral == TRUE) {
#
#     noun <- paste(masc_nouns, "/", femm_article, femm_nouns)
#
#   } else {
#
#     neut_nouns <- .clean_noun(noun_table_raw$enikos_6, elements)
#     neut_article <- noun_table_raw$enikos_5[elements]
#
#     noun <-  paste(masc_nouns, "/", femm_article, femm_nouns, "/", neut_article,
#                    neut_nouns)
#
#   }
#
#   number <- rep(c("singular", "plural"), each = 4)
#
#   # By setting case as a factor we can use the levels to reorder in the
#   # desired way
#
#   case <- factor(rep(c("nominative", "genitive", "accusative", "vocative"), 2),
#                  levels = c("nominative", "accusative", "genitive", "vocative"))
#
#
#   noun_table <- tibble(case, number, noun) |>
#     arrange(case, desc(number)) |>
#     pivot_wider(names_from = c(case, number), values_from = noun)
#
#   noun_table <- tibble(gender = "m", english_noun, noun_image = "") |>
#     bind_cols(noun_table)
#
#   return(noun_table)
#
# }




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
  table_num <- .find_noun_table_num(wiktionary_tables)

  if (table_num > length(wiktionary_tables)) {

    noun_table <- f(english_noun, greek_noun)

  } else {

    noun_table_raw <- clean_names(wiktionary_tables[[table_num]])
    noun_table <- f(noun_table_raw, english_noun, greek_noun)

  }

  return(noun_table)

}

