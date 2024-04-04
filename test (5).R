library(testthat)
context("Dataset Integrity Checks")

test_that("Dataset has 1000 rows", {
  expect_equal(nrow(simulated_data), 1000)
})


test_that("Dataset has 5 columns", {
  expect_equal(ncol(simulated_data), 5)
})


test_that("Column names are correct", {
  expect_equal(names(simulated_data), c("Support", "AgeGroup", "Gender", "IncomeGroup", "HighestEducation"))
})


test_that("Support column contains only 0s and 1s", {
  expect_equal(all(simulated_data$Support %in% c(0, 1)), TRUE)
})


test_that("AgeGroup column contains valid categories", {
  valid_age_groups <- c('18-24', '25-34', '35-44', '45-54', '55-64', '65+')
  expect_equal(all(simulated_data$AgeGroup %in% valid_age_groups), TRUE)
})


test_that("Gender column contains valid categories", {
  valid_genders <- c('Male', 'Female', 'Other')
  expect_equal(all(simulated_data$Gender %in% valid_genders), TRUE)
})


test_that("IncomeGroup column contains valid categories", {
  valid_income_groups <- c('Low', 'Medium', 'High')
  expect_equal(all(simulated_data$IncomeGroup %in% valid_income_groups), TRUE)
})


test_that("HighestEducation column contains valid categories", {
  valid_education_levels <- c('High School', 'Bachelor', 'Master', 'PhD')
  expect_equal(all(simulated_data$HighestEducation %in% valid_education_levels), TRUE)
})


test_that("There are no missing values", {
  expect_equal(sum(is.na(simulated_data)), 0)
})


test_that("Column data types are correct", {
  expect_is(simulated_data$Support, "numeric")
  expect_is(simulated_data$AgeGroup, "factor" or "character")
  expect_is(simulated_data$Gender, "factor" or "character")
  expect_is(simulated_data$IncomeGroup, "factor" or "character")
  expect_is(simulated_data$HighestEducation, "factor" or "character")
})
