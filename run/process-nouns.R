
# Process Nouns

# This script takes the nouns to do list from OneDrive and looks up their
# declension table on Greek wiktionary.

# ------------------------------------------------------------------------------

# Setup

suppressPackageStartupMessages(devtools::load_all())
message("Processing Nouns...")

# Nouns are updated on OneDrive excel sheet after lessons

nouns_to_do <- read_excel("~/OneDrive/Greek/greek_to_do.xlsx", sheet = "nouns")

# Loop through each noun and get the declension table

nouns_table <- vector("list", length = nrow(nouns_to_do))

for (i in 1:nrow(nouns_to_do)) {

  english_noun <- unlist(nouns_to_do[i, "english_noun"])
  greek_noun <- unlist(nouns_to_do[i, "greek_noun"])

  nouns_table[[i]] <- get_noun_table(english_noun, greek_noun)

}

closeAllConnections()

nouns_table <- bind_rows(nouns_table)

# Split the nouns by gender because Anki cards are split by gender

nouns_table_m <- nouns_table |> filter(gender == "m") |> select(-gender)
nouns_table_f <- nouns_table |> filter(gender == "f") |> select(-gender)
nouns_table_n <- nouns_table |> filter(gender == "n") |> select(-gender)

# Let the user know how many of each - if none then no need to upload

summary_message <- glue("Masculine: {nrow(nouns_table_m)}\n",
                        "Feminine: {nrow(nouns_table_f)}\n",
                        "Neutral: {nrow(nouns_table_n)}")

print(summary_message)
View(nouns_table)


# Write files with date for upload to anki - col names not wanted for anki

filenames <- glue('{file.path(here::here(), "output")}/',
                  'nouns_{Sys.Date()}_{c("m", "f", "n")}.csv')

write_csv(nouns_table_m, filenames[1], col_names = FALSE)
write_csv(nouns_table_f, filenames[2], col_names = FALSE)
write_csv(nouns_table_n, filenames[3], col_names = FALSE)
