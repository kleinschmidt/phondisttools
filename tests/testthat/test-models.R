library(magrittr)
library(tibble)
library(purrr)

context("Models lists")

vowels <- tribble(
  ~Talker, ~Dialect,   ~Vowel,   ~F1, ~F2,
  "M1",    "Midland",  "ae",     799, 1927,
  "M1",    "Midland",  "ae",     714, 1974,
  "M2",    "Midland",  "ae",     676, 1817,
  "M2",    "Midland",  "ae",     743, 1886,
  "M1",    "Midland",  "eh",     629, 2064,
  "M1",    "Midland",  "eh",     631, 2090,
  "M2",    "Midland",  "eh",     628, 2114,
  "M2",    "Midland",  "eh",     642, 2072,
  "N1",    "North",    "ae",     702, 2126,
  "N1",    "North",    "ae",     773, 2158,
  "N2",    "North",    "ae",     646, 2255,
  "N2",    "North",    "ae",     700, 2280,
  "N1",    "North",    "eh",     771, 1980,
  "N1",    "North",    "eh",     804, 1910,
  "N2",    "North",    "eh",     800, 1960,
  "N2",    "North",    "eh",     756, 1979
)




test_that("Models list and unlist", {

  m <- vowels %>%
    mutate(Vowel = as.character(Vowel)) %>%
    train_models(grouping="Vowel", cues=c("F1", "F2"))

  m2 <- m %>% list_models('Vowel') %>% unlist_models('Vowel')

  expect_equal(m$Vowel, m2$Vowel)
  expect_equivalent(m$model, m2$model)  # ignore names

})

test_that("Model lists nest correctly", {
  dialect_model_list <-
    vowels %>%
    group_by(Dialect) %>%
    train_models(grouping="Vowel", cues=c("F1", "F2")) %>%
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
  train_models(grouping = "Vowel", cues = c('F1', 'F2')) %>%
  list_models('Vowel') %>%
  model_matrix(vowels)

mm1 <-
  vowels %>%
  train_models(grouping = "Vowel", cues = 'F1') %>%
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
    train_models(grouping="Vowel", cues = c('F1', 'F2')) %>%
    do(model=list_models(., 'Vowel')) %>%
    list_models('Dialect') %>%
    model_matrix(vowels) %>%
    colnames %>%
    expect_equal(c('F1', 'F2'))


  vowels %>%
    group_by(Dialect) %>%
    train_models(grouping="Vowel", cues = 'F2') %>%
    do(model=list_models(., 'Vowel')) %>%
    list_models('Dialect') %>%
    model_matrix(vowels) %>%
    colnames %>%
    expect_equal('F2')

})

test_that("Train test splits", {
  
  vs_split <- vowels %>%
    train_test_split(holdout="Talker")

  expect_equal(colnames(vs_split), c("Talker", "data_train", "data_test"))
  expect_length(vs_split$Talker, length(unique(vowels$Talker)))
  expect_false(any(with(vs_split, map2_lgl(Talker, data_train, ~(.x %in% .y$Talker)))))

  vs_split_grouped <- vowels %>% 
    train_test_split(holdout="Talker", groups="Dialect")

  expect_equal(colnames(vs_split_grouped), c("Talker", "data_train", "Dialect", "data_test"))
  expect_length(vs_split_grouped$Talker, length(unique(vowels$Talker)))
  expect_false(any(with(vs_split_grouped, map2_lgl(Talker, data_train, ~(.x %in% .y$Talker)))))

  vs_split_grouped %>%
    pull(data_train) %>%
    map(groups) %>%
    map(unlist) %>%
    map(as.character) %>%
    map_lgl(~ .x == "Dialect") %>%
    all() %>%
    expect_true()
  

})
