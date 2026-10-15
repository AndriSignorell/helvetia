fixture <- function(name) test_path("fixtures", name)

read_fixture_csv <- function(name) {
  helvetia:::.read_csv_text(paste(readLines(fixture(name), encoding = "UTF-8"),
                                  collapse = "\n"))
}

read_fixture_json <- function(name) {
  jsonlite::fromJSON(fixture(name), simplifyVector = FALSE)
}

skip_if_not_live <- function() {
  skip_if_not(identical(Sys.getenv("HELVETIA_LIVE"), "true"),
              "live tests disabled (set HELVETIA_LIVE=true)")
  skip_if_offline()
}
