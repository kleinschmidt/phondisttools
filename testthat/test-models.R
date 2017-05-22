library(magrittr)

context("Models lists")

test_that("Models list and unlist", {

  m <- nsp_vows %>%
    mutate(Vowel = as.character(Vowel)) %>%
    train_models()

  m2 <- m %>% list_models('Vowel') %>% unlist_models('Vowel')

  expect_equal(m$Vowel, m2$Vowel)
  expect_equivalent(m$model, m2$model)  # ignore names

})

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

mm2 <-
  nsp_vows %>%
  train_models(formants = c('F1', 'F2')) %>%
  list_models('Vowel') %>%
  model_matrix(nsp_vows)

mm1 <-
  nsp_vows %>%
  train_models(formants = 'F1') %>%
  list_models('Vowel') %>%
  model_matrix(nsp_vows)

test_that("Model matrix is numeric", {

  mm2 %T>%
    expect_type("integer") %T>%         # because of nsp_vows F1/F2 types
    expect_is("matrix")

})

test_that("Model matrix has correct dimensions", {

  mm2 %>%
    dim() %>%
    expect_equal(c(nrow(nsp_vows), 2))

  mm1 %>%
    dim() %>%
    expect_equal(c(nrow(nsp_vows), 1))

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
