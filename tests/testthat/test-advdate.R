# test-advdate.R


test_that("advdate calculates week range correctly without topic or assignment", {
  result <- advdate("2023-01-09", 1)
  expect_equal(result, "Module 01, 01/09 - 01/13")
})

test_that("advdate calculates week range correctly with topics", {
  topics <- c("Intro", "Advanced Topics", "More Advanced Topics")
  result <- advdate("2023-01-09", 2, topic = topics)
  expect_equal(result, "Module 02, 01/16 - 01/20 : Advanced Topics")
})

test_that("advdate handles specific assignment days correctly", {
  result <- advdate("2023-01-09", 1, assignment = "Friday")
  expect_equal(result, "01/13")

  result <- advdate("2023-01-09", 1, assignment = "Mon")
  expect_equal(result, "01/09")

  result <- advdate("2023-01-09", 1, assignment = "r") # Thursday
  expect_equal(result, "01/12")
})

test_that("advdate handles mixed-case assignment inputs", {
  result <- advdate("2023-01-09", 1, assignment = "MONDAY")
  expect_equal(result, "01/09")

  result <- advdate("2023-01-09", 1, assignment = "fRiDaY")
  expect_equal(result, "01/13")
})

test_that("advdate validates invalid assignment days", {
  expect_error(advdate("2023-01-09", 1, assignment = "InvalidDay"),
               "Invalid assignment day provided.")
  expect_error(advdate("2023-01-09", 1, assignment = "XX"),
               "Invalid assignment day provided.")
})

test_that("advdate validates weekonemonday format", {
  expect_error(advdate("09-01-2023", 1),
               "weekonemonday must be a Date object or a character string in 'YYYY-MM-DD' format.")
  expect_error(advdate("invalid_date", 1),
               "weekonemonday must be a valid date in 'YYYY-MM-DD' format.")
})

test_that("advdate validates topic vector length and advdate handles weeks without topics", {
  topics <- c("Intro", "Advanced Topics")
  # handles both warnings
  expect_warning(expect_warning(result <- advdate("2023-01-09", 3, topic = topics),
                 "Topic not defined for the specified week. Defaulting to 'No topic'."),
  "topic must be a vector with length at least as long as the week number.")
  expect_equal(result, "Module 03, 01/23 - 01/27 : No topic")
})

test_that("advdate validates week input", {
  expect_error(advdate("2023-01-09", -1),
               "week must be a non-negative integer.")
  expect_error(advdate("2023-01-09", "invalid"),
               "week must be a non-negative integer.")
})

test_that("advdate handles alternate unit labels", {
  result <- advdate("2023-01-09", 1, unit = "Week ")
  expect_equal(result, "Week 01, 01/09 - 01/13")
})



#test_that("advdate handles version 0 correctly", {
#  result <- advdate("2023-01-09", 1, version = 0)
#  expect_equal(result, "Module 01, 01/09 - 01/13")
#})
