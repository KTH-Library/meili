
test_that("meili server is online", {
  skip_on_ci()
  is_online <- meili_health()
  expect_true(is_online)
})

test_that("data can be ingested into an index", {

  # we create an index, ingest it and check the status, then delete it
  skip_on_ci()

  df <- CO2
  task <- meili_ingest_csv("co2", df)
  res <- wait_for_task(task$taskUid)

  is_ingested <- all(meili_documents("co2")$rowid == as.character(1:20))

  task <- meili_deleteindex("co2")
  is_deleted <- wait_for_task(task$taskUid)
  expect_true(is_ingested && is_deleted)

})

test_that("fields can be filtered on", {

  df <- CO2
  task <- meili_ingest_csv("co2", df)
  res <- wait_for_task(task$taskUid)

  is_ingested <- all(meili_documents("co2")$rowid == as.character(1:20))


  fields <-
    meili_documents("co2", limit = 1e2) |>
    names()

  fields <- fields[-which("rowid" %in% fields)]

  task <- meili_index_create_filters("co2", fields)
  res <- wait_for_task(task$taskUid)

  search <- meili_documents("co2", filter = "Treatment = chilled")

  task <- meili_deleteindex("co2")
  is_deleted <- wait_for_task(task$taskUid)

  is_valid <- is_deleted & nrow(search) >= 20

  expect_true(is_valid)

})

test_that("name can be searched", {
  skip_on_ci()
  res <- search_name("Maguire")
  is_valid <- nrow(res) > 0
  expect_true(is_valid)
})
