library(pmlblite)

wine_quality_red <- pmlblite::fetch_data('wine-quality-red')
waveform <- pmlblite::fetch_data('waveform-40')
titanic <- titanic::titanic_train
flags <- pmlblite::fetch_data('flags')
diabetes <- pmlblite::fetch_data('diabetes')
saheart <- pmlblite::fetch_data('saheart')

data(wine, package = 'rattle')

usethis::use_data(wine_quality_red, waveform, titanic, flags,
                  diabetes, saheart, wine, overwrite = TRUE)
