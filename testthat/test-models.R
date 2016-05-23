context("Models structures")

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
