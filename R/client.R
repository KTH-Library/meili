# Add MEILI_KEY to .Renviron
#file.edit("~/.Renviron")
#readRenviron("~/.Renviron")

meili_config <- function(verbose = FALSE) {

  meili_key <- Sys.getenv("MEILI_KEY")

  if (meili_key == "") {
    if (verbose) warning("Please provide MEILI_KEY in .Renviron!")
  } else {
    if (verbose) message("Found setting for MEILI_KEY environment variable!")
  }

  meili_base <- Sys.getenv("MEILI_URL")

  if (meili_base == "") {
    if (verbose) message("Please provide MEILI_URL in .Renviron!")
    meili_base <- "https://search.bibliometrics.lib.kth.se"
    if (verbose) message("Reverting to default value for MEILI_URL: ", meili_base)
  }

  meili_headers <-
    list(
      `Authorization` = paste("Bearer", meili_key)
    )

  list(
    base_url = meili_base,
    headers = meili_headers
  )
}

#' @import httr2
meili_req <- function(cfg = meili_config(), path = "/") {
  request(cfg$base_url) |>
    req_headers(!!!cfg$headers) |>
    req_url_path_append(path)
}

#' @import httr2
#' @import dplyr
meili_createindex <- function(index, idfield = NULL) {

  payload <-
    list(
      uid = index,
      primaryKey = idfield
    ) |>
    purrr::compact()

  res <-
    meili_req(path = "/indexes") |>
    req_body_json(data = payload) |>
    req_perform() |>
    resp_body_json()

  res |>
    bind_rows() |>
    dplyr::mutate(across(ends_with("At"), \(x) parse_ts(x)))

  # res <- POST(
  #   url = paste0(meili_base, "/indexes"),
  #   config = meili_cfg,
  #   body = list(uid = index, primaryKey = idfield),
  #   encode = "json"
  # )

}

meili_deleteindex <- function(index) {

  meili_req(path = paste0("/indexes/", index)) |>
    req_method("DELETE") |>
    req_perform() |>
    resp_body_json()

}

meili_indexes <- function() {

  res <- meili_req(path = "/indexes") |>
    req_perform() |>
    resp_body_json()

  res$results |> bind_rows() |>
    mutate(across(ends_with("At"), \(x) parse_ts(x)))
}

meili_index <- function(index) {

  route <- sprintf("/indexes/%s", index)

  res <-
    meili_req(path = route) |>
    req_perform() |>
    resp_body_json()

  res |> bind_rows() |>
    dplyr::mutate(across(ends_with("At"), \(x) parse_ts(x)))

}

meili_documents <- function(index, offset = 0, limit = 20, fields = NULL, filter = NULL) {

  route <- sprintf("/indexes/%s/documents/fetch", index)

  params <- list(
    offset = offset,
    limit = limit,
    fields = fields,
    filter = filter
  ) |> purrr::compact()

  res <-
    meili_req(path = route) |>
    req_body_json(params, null = "null") |>
    req_perform() |>
    resp_body_json()

  res$results |> bind_rows()
}

meili_document <- function(index, document, fields = "*") {

  route <- sprintf("/indexes/%s/documents/%s", index, document)

  res <-
    meili_req(path = route) |>
    req_url_query(fields = fields, .multi = "comma") |>
    req_perform() |>
    resp_body_json()

  res |> bind_rows()
}


#' Ingest data into Meili index
#'
#' @param index the name of the index to ingest data in
#' @param csvfile the name of a dataframe or a csv file to ingest into index
#' @import readr httr2
#' @export
meili_ingest_csv <- function(index, csvfile) {

  if (is.data.frame(csvfile)) {
    tf <- tempfile(fileext = ".csv")
    write_csv(csvfile, tf, na = "", quote = "all", escape = "double")
    on.exit(unlink(tf))
    csvfile <- tf
  }

  if (!file.exists(csvfile))
      stop("Cannot find csvfile and it is not a data frame")

  route <- sprintf("/indexes/%s/documents", index)

  res <-
    meili_req(path = route) |>
    req_headers(`Content-Type` = "text/csv") |>
    req_body_file(csvfile) |>
    req_perform() |>
    resp_body_json()

  res
}

wait_for_status <- function(task, verbose = FALSE) {
  t <- meili_task(task)
  while (t$overview$status != "succeeded") {
    if (nrow(t$error) > 0) {
      print(t)
      return(FALSE)
    }
    Sys.sleep(1)
    t <- meili_task(task)
    if (verbose) cat(".")
  }
  if (verbose) cat("\n Done \n")
  return(TRUE)
}

#' @importFrom dplyr bind_rows select any_of
#' @importFrom purrr map_df map
meili_tasks <- function() {

  res <-
    meili_req(path = "tasks") |>
    req_perform() |>
    resp_body_json()

  res$results |> map(parse_task)
#    purrr::modify_if(.p = \(x) is.null(x), .f = \(x) NA) #|>
#    bind_rows() |>
 #   select(-any_of(c("details")))
}

meili_task <- function(task) {

  res <-
    meili_req(path = sprintf("tasks/%s", task)) |>
    req_perform() |>
    resp_body_json()

  parse_task(res)

}

parse_task <- function(res) {

  details <- res$details |> bind_rows()

  error <- res$error |> bind_rows()

  overview <-
    res |> purrr::discard_at(c("details", "error")) |> bind_rows() |>
    dplyr::mutate(across(ends_with("At"), \(x) parse_ts(x)))

  list(details = details, error = error, overview = overview)

}

meili_settings <- function(index) {

  route <- sprintf("/indexes/%s/settings", index)

  res <-
    meili_req(path = route) |>
    req_perform(verbosity = 2) |>
    resp_body_json()

  res

}

parse_ts <- function(x)
  lubridate::parse_date_time(x, "YmdHMS")


#' @import dplyr tidyr
meili_stats <- function() {

  res <-
    meili_req(path = sprintf("stats")) |>
    req_perform() |>
    resp_body_json()

  res$indexes |> purrr::map(parse_stats) #|> bind_rows() |>
    #unnest_wider(col = "indexes") |>
    #unnest_longer(col = "fieldDistribution") |>
    #mutate(across(ends_with("Update"), \(x) parse_ts(x)))
}

meili_stats_index <- function(index) {

  res <-
    meili_req(path = sprintf("indexes/%s/stats", index)) |>
    req_perform() |>
    resp_body_json()

  res |> parse_stats() #|> bind_rows() #|>
    # unnest_wider(col = "indexes") |>
    # unnest_longer(col = "fieldDistribution") |>
    # mutate(across(ends_with("Update"), \(x) parse_ts(x)))

}

parse_stats <- function(json) {
  list(
    fieldDistribution = json$fieldDistribution |> bind_rows(),
    overview = json |> purrr::discard_at("fieldDistribution") |> bind_rows()
  )
}

meili_search <- function(
  index, query = "", offset = 0L, limit = 20L,
  hitsPerPage = 20L, page = 1,
  filter = NULL,
  facets = NULL,
  attributesToRetrieve = "*",
  attributesToCrop = NULL,
  cropLength = 200L,
  cropMarker = "...",
  attributesToHighlight = NULL,
  highlightPreTag = "<em>",
  highlightPostTag = "</em>",
  showMatchesPosition = FALSE,
  sort = NULL,
  matchingStrategy = "last",
  showRankingScore = TRUE,
  attributesToSearchOn = "*"
  #matches = TRUE
  ) {

  b <- purrr::compact(list(
    q = query, offset = offset, limit = limit, filter = filter,
    hitsPerPage = hitsPerPage, page = page,
    facets = facets,
    attributesToRetrieve = list(attributesToRetrieve),
    attributesToCrop = attributesToCrop,
    cropLength = cropLength,
    cropMarker = cropMarker,
    attributesToHighlight = attributesToHighlight,
    highlightPreTag = highlightPreTag,
    highlightPostTag = highlightPostTag,
    showMatchesPosition = showMatchesPosition,
    matchingStrategy = matchingStrategy,
    showRankingScore = showRankingScore,
    attributesToSearchOn = list(attributesToSearchOn)
    #, matches = matches
  ))

  res <-
    meili_req(path = sprintf("indexes/%s/search", index)) |>
    req_body_json(b) |>
    req_perform() |>
    resp_body_json()

  res$hits |> dplyr::bind_rows()
}

meili_health <- function() {
  res <-
    meili_req(path = "/health") |>
    req_perform() |>
    resp_body_json()

  res$status == "available"
}

#' Search for a name in hrfile (or other index)
#'
#' @param q the string to search for
#' @param index the index to search in, default hrfile
#' @importFrom purrr map_df
#' @export
search_name <- function(q, index = "hrfile") {
  s <- \(q) meili_search(index, query = q)
  q |> purrr::map_df(s)
}
