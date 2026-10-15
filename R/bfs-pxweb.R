# BFS PXWeb API (v1) ----------------------------------------------------------

.pxweb_host <- "https://www.pxweb.bfs.admin.ch/api/v1"

.bfs_url <- function(px, lang = "de") {
  sprintf("%s/%s/%s/%s.px", .pxweb_host, lang, px, px)
}


# GET table metadata; returns list(title, variables) where each variable is a
# list(code, text, values, valueTexts, elimination, time).
.bfs_metadata <- function(url) {
  .parse_bfs_metadata(.http_json(url))
}

.parse_bfs_metadata <- function(js) {
  if (is.null(js$variables))
    stop("PXWeb metadata without 'variables' element", call. = FALSE)
  vars <- lapply(js$variables, function(v) list(
    code        = v$code,
    text        = v$text,
    values      = unlist(v$values),
    valueTexts  = unlist(v$valueTexts),
    elimination = isTRUE(v$elimination),
    time        = isTRUE(v$time)))
  list(title = js$title %||% NA_character_, variables = vars)
}


# Find exactly one variable by exact code, else by regex on code or text.
.bfs_variable <- function(metadata, name) {
  vars <- metadata$variables
  codes <- vapply(vars, `[[`, "", "code")
  texts <- vapply(vars, `[[`, "", "text")
  hit <- which(codes == name)
  if (!length(hit))
    hit <- which(grepl(name, codes, ignore.case = TRUE) |
                 grepl(name, texts, ignore.case = TRUE))
  if (length(hit) != 1L)
    stop(sprintf(paste0("PXWeb variable matching '%s' %s.\n",
                        "Available variables: %s"),
                 name, if (length(hit)) "is ambiguous" else "not found",
                 paste(sQuote(codes, FALSE), collapse = ", ")),
         call. = FALSE)
  vars[[hit]]
}


# Look up a value code by its value text (regex) and fail loudly if the
# metadata no longer contains it.
.bfs_value <- function(variable, pattern) {
  hit <- grep(pattern, variable$valueTexts, ignore.case = TRUE)
  if (length(hit) != 1L)
    stop(sprintf("value text matching '%s' %s in PXWeb variable '%s'",
                 pattern, if (length(hit)) "is ambiguous" else "not found",
                 variable$code), call. = FALSE)
  variable$values[hit]
}


# Maximum number of cells per request, from the API configuration.
.pxweb_env <- new.env(parent = emptyenv())

.bfs_max_cells <- function(lang = "de") {
  if (!is.null(.pxweb_env$max_cells)) return(.pxweb_env$max_cells)
  cfg <- tryCatch(.http_json(sprintf("%s/%s/", .pxweb_host, lang),
                             query = list(config = "")),
                  error = function(e) NULL)
  n <- suppressWarnings(as.numeric(cfg$maxValues))
  if (!length(n) || is.na(n) || n <= 0) n <- 1e5
  .pxweb_env$max_cells <- n
  n
}


# Split a selection (named list code -> values) into chunks so that each
# chunk requests at most max_cells cells. The largest variable is split.
.chunk_query <- function(query, max_cells) {
  n <- lengths(query)
  total <- prod(n)
  if (total <= max_cells) return(list(query))
  big <- which.max(n)
  per <- floor(max_cells / (total / n[big]))
  if (per < 1L)
    stop("PXWeb cell limit too small to split the query on one variable",
         call. = FALSE)
  idx <- split(seq_len(n[big]), ceiling(seq_len(n[big]) / per))
  lapply(unname(idx), function(i) {
    q <- query
    q[[big]] <- query[[big]][i]
    q
  })
}


.pxweb_body <- function(query) {
  list(query = unname(lapply(names(query), function(code) list(
         code = code,
         selection = list(filter = "item", values = I(query[[code]]))))),
       response = list(format = "json"))
}


# POST a selection and return a data.frame with one character column per
# returned dimension (named by variable code) and numeric column `value`.
# Non-numeric cell symbols ("...", "-", etc.) become NA, the raw symbol is
# kept in `value_raw`.
.bfs_query <- function(url, query, max_cells = .bfs_max_cells()) {
  chunks <- .chunk_query(query, max_cells)
  out <- vector("list", length(chunks))
  for (i in seq_along(chunks)) {
    if (i > 1L) Sys.sleep(0.4)            # API rate limit (calls per window)
    out[[i]] <- .parse_bfs_data(.http_json(url, post = .pxweb_body(chunks[[i]])))
  }
  do.call(rbind, out)
}


.parse_bfs_data <- function(js) {
  cols <- js$columns
  type <- vapply(cols, function(c) c$type %||% "d", "")
  dims <- vapply(cols[type != "c"], `[[`, "", "code")
  if (!length(js$data))
    return(stats::setNames(
      data.frame(matrix(character(), 0, length(dims) + 2L)),
      c(dims, "value_raw", "value")))
  key <- do.call(rbind, lapply(js$data, function(d) unlist(d$key)))
  val <- vapply(js$data, function(d) as.character(d$values[[1L]]), "")
  df <- as.data.frame(key, stringsAsFactors = FALSE)
  names(df) <- dims
  df$value_raw <- val
  df$value <- suppressWarnings(as.numeric(val))
  df
}
