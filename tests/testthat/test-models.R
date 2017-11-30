library(magrittr)
library(tibble)

context("Models lists")

vowels <- tribble(
  ~Dialect, ~Vowel, ~F1, ~F2,
  "Midland",  "ae",     799, 1927,
  "Midland",  "ae",     714, 1974,
  "Midland",  "ae",     718, 1893,
  "Midland",  "ae",     676, 1817,
  "Midland",  "ae",     743, 1886,
  "Midland",  "eh",     629, 2064,
  "Midland",  "eh",     615, 2036,
  "Midland",  "eh",     631, 2090,
  "Midland",  "eh",     628, 2114,
  "Midland",  "eh",     642, 2072,
  "North",    "ae",     702, 2126,
  "North",    "ae",     773, 2158,
  "North",    "ae",     800, 2153,
  "North",    "ae",     646, 2255,
  "North",    "ae",     700, 2280,
  "North",    "eh",     771, 1980,
  "North",    "eh",     797, 1976,
  "North",    "eh",     804, 1910,
  "North",    "eh",     800, 1960,
  "North",    "eh",     756, 1979
)




test_that("Models list and unlist", {

  m <- vowels %>%
    mutate(Vowel = as.character(Vowel)) %>%
    train_models()

  m2 <- m %>% list_models('Vowel') %>% unlist_models('Vowel')

  expect_equal(m$Vowel, m2$Vowel)
  expect_equivalent(m$model, m2$model)  # ignore names

})

test_that("Model lists nest correctly", {
  dialect_model_list <-
    vowels %>%
    group_by(Dialect) %>%
    train_models() %>%
    do(model=list_models(., 'Vowel')) %>%
    list_models('Dialect')

  # top level: Dialect
  expect_equal(names(dialect_model_list),
               unique(vowels$Dialect))
  # next level: Vowel
  expect_equal(sort(names(dialect_model_list[[1]])),
               sort(unique(vowels$Vowel)))

})

context("model_matrix")

mm2 <-
  vowels %>%
  train_models(formants = c('F1', 'F2')) %>%
  list_models('Vowel') %>%
  model_matrix(vowels)

mm1 <-
  vowels %>%
  train_models(formants = 'F1') %>%
  list_models('Vowel') %>%
  model_matrix(vowels)

test_that("Model matrix is numeric", {

  mm2 %T>%
    expect_type("double") %T>%
    expect_is("matrix")

})

test_that("Model matrix has correct dimensions", {

  mm2 %>%
    dim() %>%
    expect_equal(c(nrow(vowels), 2))

  mm1 %>%
    dim() %>%
    expect_equal(c(nrow(vowels), 1))

})

test_that("Dimension names are extracted from models when testing", {

  mm2 %>%
    colnames() %>% 
    expect_equal(c('F1', 'F2'))

  mm1 %>%
    colnames() %>% 
    expect_equal('F1')

})

test_that("Multiply nested models' dimnames are extracted correctly", {

  vowels %>%
    group_by(Dialect) %>%
    train_models(formants = c('F1', 'F2')) %>%
    do(model=list_models(., 'Vowel')) %>%
    list_models('Dialect') %>%
    model_matrix(vowels) %>%
    colnames %>%
    expect_equal(c('F1', 'F2'))


  vowels %>%
    group_by(Dialect) %>%
    train_models(formants = 'F2') %>%
    do(model=list_models(., 'Vowel')) %>%
    list_models('Dialect') %>%
    model_matrix(vowels) %>%
    colnames %>%
    expect_equal('F2')

})
