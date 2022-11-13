
get_verb_table <- function (english_verb, greek_verb, english_verb_tenses) {

  wiktionary_url <- paste0("https://el.wiktionary.org/wiki/", greek_verb)

  declension_raw_tbl <- wiktionary_url |>
    read_html() |>
    html_elements("table") |>
    html_table() |>
    extract2(1) |>
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
           .before = I)

  return(declension_tbl)

}



clean_verb <- function (x) {

  x |> str_split("[:punct:]") |> map_chr(1)

}


