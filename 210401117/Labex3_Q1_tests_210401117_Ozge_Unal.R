library(testthat)
test_that("Correct artist ID is retrieved for a given artist name", {
  artist_name <- "The Beatles"
  artist_id <- spotify_get_artist_id(artist_name)
  expect_equal(artist_id, "3WrFJ7ztbogyGnTHbHJFl2")
})