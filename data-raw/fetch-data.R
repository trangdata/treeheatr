library(pmlblite)
library(dplyr)

wine_quality_red <- pmlblite::fetch_data("wine-quality-red")
# mushroom <- pmlblite::fetch_data('mushroom')
# titanic <- titanic::titanic_train
# flags <- pmlblite::fetch_data('flags')
# saheart <- pmlblite::fetch_data('saheart')
# pollen <- pmlblite::fetch_data('529_pollen')
diabetes <- readr::read_csv("data-raw/diabetes.csv")
galaxy <- pmlblite::fetch_data("690_visualizing_galaxy")
penguins <- readr::read_csv(
  "https://github.com/allisonhorst/penguins/raw/master/data/penguins_size.csv",
  col_types = "ffnnnnf"
)

train_covid_raw <- readr::read_tsv(
  "https://raw.githubusercontent.com/trangdata/Pre_Surv_COVID_19/master/data/processed_covid_train.tsv"
)
colnames(train_covid_raw) <- iconv(colnames(train_covid_raw), from = "UTF-8", to = "ASCII")
attr(train_covid_raw, "spec") <- NULL

test_covid_raw <- readr::read_tsv(
  "https://raw.githubusercontent.com/trangdata/Pre_Surv_COVID_19/master/data/processed_covid_test.tsv"
)

train_covid <- train_covid_raw %>%
  select(
    LDH = "Lactate dehydrogenase",
    hs_CRP = "High sensitivity C-reactive protein",
    Lymphocyte = "(%)lymphocyte",
    Outcome = Type2
  ) %>%
  mutate(Outcome = as.factor(Outcome)) %>%
  na.omit()

test_covid <- test_covid_raw %>%
  select(
    LDH = "Lactate dehydrogenase",
    hs_CRP = "High sensitivity C-reactive protein",
    Lymphocyte = "(%)lymphocyte",
    Outcome = "outcome"
  ) %>%
  mutate(Outcome = as.factor(Outcome))

data(wine, package = "rattle")

usethis::use_data(wine_quality_red, diabetes,
  # flags, saheart, pollen, titanic,
  galaxy, wine, penguins, train_covid, test_covid,
  overwrite = TRUE
)

# cat(colnames(test_covid), sep = '}, \\code{')
usethis::use_data(train_covid, test_covid, overwrite = TRUE)
#
