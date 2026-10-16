# BFS PXWeb API (v1) ----------------------------------------------------------

.pxwebHost <- "https://www.pxweb.bfs.admin.ch/api/v1"

.bfsUrl <- function(px, lang = "de") {
  sprintf("%s/%s/%s/%s.px", .pxwebHost, lang, px, px)
}


# GET table metadata; returns list(title, variables) where each variable is a
# list(code, text, values, valueTexts, elimination, time).
.bfsMetadata <- function(url) {
  .parseBfsMetadata(.httpJson(url))
}

.parseBfsMetadata <- function(js) {
  if (is.null(js$variables))
    stop("PXWeb metadata without a 'variables' element.", call. = FALSE)
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
.bfsVariable <- function(metadata, name) {
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
.bfsValue <- function(variable, pattern) {
  hit <- grep(pattern, variable$valueTexts, ignore.case = TRUE)
  if (length(hit) != 1L)
    stop(sprintf("Value text matching '%s' %s in PXWeb variable '%s'.",
                 pattern, if (length(hit)) "is ambiguous" else "not found",
                 variable$code), call. = FALSE)
  variable$values[hit]
}


# Maximum number of cells per request, from the API configuration.
.pxwebEnv <- new.env(parent = emptyenv())

.bfsMaxCells <- function(lang = "de") {
  if (!is.null(.pxwebEnv$maxCells)) return(.pxwebEnv$maxCells)
  cfg <- tryCatch(.httpJson(sprintf("%s/%s/", .pxwebHost, lang),
                             query = list(config = "")),
                  error = function(e) NULL)
  n <- suppressWarnings(as.numeric(cfg$maxValues))
  if (!length(n) || is.na(n) || n <= 0) n <- 1e5
  .pxwebEnv$maxCells <- n
  n
}


# Split a selection (named list code -> values) into chunks so that each
# chunk requests at most maxCells cells. The largest variable is split.
.chunkQuery <- function(query, maxCells) {
  n <- lengths(query)
  total <- prod(n)
  if (total <= maxCells) return(list(query))
  big <- which.max(n)
  per <- floor(maxCells / (total / n[big]))
  if (per < 1L)
    stop("The PXWeb cell limit is too small to split the query on one variable.",
         call. = FALSE)
  idx <- split(seq_len(n[big]), ceiling(seq_len(n[big]) / per))
  lapply(unname(idx), function(i) {
    q <- query
    q[[big]] <- query[[big]][i]
    q
  })
}


.pxwebBody <- function(query) {
  list(query = unname(lapply(names(query), function(code) list(
         code = code,
         selection = list(filter = "item", values = I(query[[code]]))))),
       response = list(format = "json"))
}


# POST a selection and return a data.frame with one character column per
# returned dimension (named by variable code) and numeric column `value`.
# Non-numeric cell symbols ("...", "-", etc.) become NA, the raw symbol is
# kept in `valueRaw`.
.bfsQuery <- function(url, query, maxCells = .bfsMaxCells()) {
  chunks <- .chunkQuery(query, maxCells)
  out <- vector("list", length(chunks))
  for (i in seq_along(chunks)) {
    if (i > 1L) Sys.sleep(0.4)            # API rate limit (calls per window)
    out[[i]] <- .parseBfsData(.httpJson(url, post = .pxwebBody(chunks[[i]])))
  }
  do.call(rbind, out)
}


.parseBfsData <- function(js) {
  cols <- js$columns
  type <- vapply(cols, function(c) c$type %||% "d", "")
  dims <- vapply(cols[type != "c"], `[[`, "", "code")
  if (!length(js$data))
    return(setNames(
      data.frame(matrix(character(), 0, length(dims) + 2L)),
      c(dims, "valueRaw", "value")))
  key <- do.call(rbind, lapply(js$data, function(d) unlist(d$key)))
  val <- vapply(js$data, function(d) as.character(d$values[[1L]]), "")
  df <- as.data.frame(key, stringsAsFactors = FALSE)
  names(df) <- dims
  df$valueRaw <- val
  df$value <- suppressWarnings(as.numeric(val))
  df
}
