

# # Standard case
# english_verb <- "upgrade"
# greek_verb <- "αναβαθμίζω"
#
#
# # Edge case

# english_verb_tenses <- c("signed",
#                          "will sign",
#                          "was signing",
#                          "will be signing",
#                          "had signed",
#                          "have signed",
#                          "will have signed")


get_verb_table <- function (english_verb, greek_verb, english_verb_tenses) {

  wiktionary_url <- paste0("https://el.wiktionary.org/wiki/", greek_verb)

  wiktionary_tables <- wiktionary_url |>
    read_html() |>
    html_elements("table") |>
    html_table()

  # Find verb table number

  table_num <- .find_verb_table_num(wiktionary_tables)

  # And pull it out

  declension_raw_tbl <- wiktionary_tables |>
    extract2(table_num) |>
    clean_names()

  # Extract data from the raw tables and build the words

  present <- declension_raw_tbl$exakolouthetikoi_chronoi_2[2:7]
  past <- declension_raw_tbl$exakolouthetikoi_chronoi_3[10:15]
  future <-  declension_raw_tbl$exakolouthetikoi_chronoi_4[2:7]
  past_cont <- declension_raw_tbl$exakolouthetikoi_chronoi_3[2:7]
  future_cont <- present
  past_perfect <-  declension_raw_tbl$exakolouthetikoi_chronoi_2[18:23]
  present_perfect <- declension_raw_tbl$exakolouthetikoi_chronoi_3[18:23]
  future_perfect <- declension_raw_tbl$exakolouthetikoi_chronoi_4[18:23]

  declension_tbl <- tibble(present,
                           past,
                           future,
                           past_cont,
                           future_cont,
                           past_perfect,
                           present_perfect,
                           future_perfect) |>
    t() |>
    set_colnames(c("I", "you", "he/she/it", "we", "yous", "they")) |>
    as_tibble() |>
    mutate(across(.fns = clean_verb)) |>
    mutate(tense = c("present", "past", "future", "past cont", "future cont",
                     "past perfect", "present perfect", "future perfect"),
           english_verb = c(english_verb, english_verb_tenses),
           image = "",
           .before = I)

  return(declension_tbl)

}



clean_verb <- function (x) {

  x |> str_split("[:punct:]") |> map_chr(1)

}

.find_verb_table_num <- function (wiktionary_tables) {

  num_tables <- length(wiktionary_tables)

  # This word relates to verb tenses

  match_col_name <- "Εξακολουθητικοί χρόνοι"

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

