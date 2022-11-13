
suppressPackageStartupMessages(devtools::load_all())
message("Processing Verbs...")

verbs_to_do <- read_excel("~/OneDrive/Greek/greek_to_do.xlsx", sheet = "verbs")

# SOme code to turn the tense columns into a list

english_verb = "learn"
greek_verb = "μαθαίνω"
english_verb_tenses = c("learned", "will learn", "was learning", "will be learning",
                         "had learned", "have learned", "will have learned")


