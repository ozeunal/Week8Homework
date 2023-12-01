library(testthat)
test_that("Doğru sanatçı adıyla doğru ID alınıyor mu?", {
  artist_name <- "The Beatles"
  artist_id <- spotify_get_artist_id(artist_name)
  expect_equal(artist_id, "3WrFJ7ztbogyGnTHbHJFl2")
})

test_that("Doğru şarkı bilgileri çekiliyor mu?", {
  artist_id <- "3WrFJ7ztbogyGnTHbHJFl2" 
  tracks_result <- spotify_artist_top_tracks(artist_id)
  expect_is(tracks_result$status_code, "integer")
  expect_is(tracks_result$tracks_data, "data.frame")
  # İlgili sütunların mevcut olup olmadığını kontrol etme
  expect_true("id" %in% colnames(tracks_result$tracks_data))
  expect_true("name" %in% colnames(tracks_result$tracks_data))
  expect_true("album" %in% colnames(tracks_result$tracks_data))
  expect_true("year" %in% colnames(tracks_result$tracks_data))
})