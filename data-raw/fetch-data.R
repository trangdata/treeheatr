library(pmlblite)

wine_quality_red <- pmlblite::fetch_data('wine-quality-red')
# mushroom <- pmlblite::fetch_data('mushroom')
# titanic <- titanic::titanic_train
# flags <- pmlblite::fetch_data('flags')
# saheart <- pmlblite::fetch_data('saheart')
# pollen <- pmlblite::fetch_data('529_pollen')
diabetes <- readr::read_csv('data-raw/diabetes.csv')
galaxy <- pmlblite::fetch_data('690_visualizing_galaxy')
penguins <- readr::read_csv(
  'https://github.com/allisonhorst/penguins/raw/master/data/penguins_size.csv',
  col_types = 'ffnnnnf')
train_covid <- readr::read_tsv(
  'https://raw.githubusercontent.com/trang1618/Pre_Surv_COVID_19/master/data/processed_covid_train.tsv')
colnames(train_covid) <- iconv(colnames(train_covid), from="UTF-8", to="ASCII")
attr(train_covid, "spec") <- NULL

test_covid <- readr::read_tsv(
  'https://raw.githubusercontent.com/trang1618/Pre_Surv_COVID_19/master/data/processed_covid_test.tsv')

data(wine, package = 'rattle')

usethis::use_data(wine_quality_red, diabetes,
                  # flags, saheart, pollen, titanic,
                  galaxy, wine, penguins, train_covid, test_covid,
                  overwrite = TRUE)

# cat(colnames(test_covid), sep = '}, \\code{')
# usethis::use_data(train_covid, overwrite = TRUE)
