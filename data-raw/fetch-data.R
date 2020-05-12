library(pmlblite)

wine_quality_red <- pmlblite::fetch_data('wine-quality-red')
waveform <- pmlblite::fetch_data('waveform-40')
mushroom <- pmlblite::fetch_data('mushroom')
titanic <- titanic::titanic_train
flags <- pmlblite::fetch_data('flags')
diabetes <- pmlblite::fetch_data('diabetes')
saheart <- pmlblite::fetch_data('saheart')
pollen <- pmlblite::fetch_data('529_pollen')
galaxy <- pmlblite::fetch_data('690_visualizing_galaxy')

data(wine, package = 'rattle')

usethis::use_data(wine_quality_red, waveform, mushroom, titanic,
                  flags, diabetes, saheart, wine, pollen, galaxy,
                  overwrite = TRUE)
