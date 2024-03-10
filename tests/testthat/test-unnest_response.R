# 1: Vérification du nombre de lignes pour une prévision de 7 jours
  test_that("La fonction renvoie le bon nombre de lignes", {
    expect_equal(nrow(data_test), 168, info = "Le résultat devrait contenir 168 lignes.")
  })

# 2: Vérification du nom des colonnes en sortie  
    expected_columns <- c("date_heure", "temperature_celsius", "temperature_ressentie", "precipitation_proba", "precipitation_mm")
    expect_equal(names(data_test), expected_columns, info = "Les noms des colonnes devraient correspondre à ceux du dataframe.")
  })

# 3: Vérification des valeurs de la colonne temperature
test_that("Les valeurs de la colonne temperature correspondent aux valeurs proposées en entrée", {
  expect_equal(data_test$temperature_celsius, temperatures_celsius, 
               info = "Les valeurs de la colonne temperature devraient correspondre aux valeurs proposées en entrée.")
})

# 4: Vérification du nombre de colonnes en sortie
test_that("Le nombre de colonnes en sortie est correct", {
  expect_equal(ncol(data_test), length(expected_columns), 
               info = "Le nombre de colonnes en sortie devrait être égal à celui du vecteur 'expected_columns'.")
})



