context("Models lists")

test_that("Model lists nest correctly", {
  dialect_model_list <-
    nsp_vows %>%
    group_by(Dialect) %>%
    train_models() %>%
    do(model=list_models(., 'Vowel')) %>%
    list_models('Dialect')

  # top level: Dialect
  expect_equal(names(dialect_model_list),
               unique(nsp_vows$Dialect))
  # next level: Vowel
  expect_equal(sort(names(dialect_model_list[[1]])),
               sort(levels(nsp_vows$Vowel)))

})

context("model_matrix")

test_that("Dimension names are extracted from models when testing", {

  nsp_vows %>%
    train_models(formants = c('F1', 'F2')) %>%
    list_models('Vowel') %>%
    model_matrix(nsp_vows) %>%
    colnames() %>% 
    expect_equal(c('F1', 'F2'))

  nsp_vows %>%
    train_models(formants = 'F1') %>%
    list_models('Vowel') %>%
    model_matrix(nsp_vows) %>%
    colnames() %>% 
    expect_equal('F1')

})

test_that("Multiply nested models' dimnames are extracted correctly", {

  nsp_vows %>%
    group_by(Dialect) %>%
    train_models(formants = c('F1', 'F2')) %>%
    do(model=list_models(., 'Vowel')) %>%
    list_models('Dialect') %>%
    model_matrix(nsp_vows) %>%
    colnames %>%
    expect_equal(c('F1', 'F2'))


  nsp_vows %>%
    group_by(Dialect) %>%
    train_models(formants = 'F2') %>%
    do(model=list_models(., 'Vowel')) %>%
    list_models('Dialect') %>%
    model_matrix(nsp_vows) %>%
    colnames %>%
    expect_equal('F2')

})
