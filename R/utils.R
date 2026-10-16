# Internal utilities shared by all source modules ---------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x


.checkFlag <- function(x, name = deparse(substitute(x))) {
  if (!is.logical(x) || length(x) != 1L || is.na(x))
    stop(sprintf("Argument '%s' must be TRUE or FALSE.", name), call. = FALSE)
  invisible(x)
}


# Coerce a reference date argument (Date or "YYYY-MM-DD") to a single Date.
.asDate <- function(x, name = "date", default = Sys.Date()) {
  if (is.null(x)) return(as.Date(default))
  if (inherits(x, "POSIXt")) x <- as.Date(x)
  if (is.character(x)) x <- as.Date(x, optional = TRUE)
  if (!inherits(x, "Date") || length(x) != 1L || is.na(x))
    stop(sprintf("Argument '%s' must be a single Date or \"YYYY-MM-DD\".",
                 name), call. = FALSE)
  x
}


# Validate a single year; NULL yields the default.
.asYear <- function(x, name = "year", default = NULL) {
  if (is.null(x)) return(default)
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x != round(x))
    stop(sprintf("Argument '%s' must be a single whole number.", name),
         call. = FALSE)
  as.integer(x)
}


# Validate an optional vector of whole numbers (years, ids).
.checkWhole <- function(x, name) {
  if (is.null(x)) return(invisible(NULL))
  if (!is.numeric(x) || !length(x) || anyNA(x) || any(x != round(x)))
    stop(sprintf("Argument '%s' must be a vector of whole numbers without NA.",
                 name), call. = FALSE)
  invisible(x)
}


# Parse dates as delivered by the various sources (dd.mm.yyyy, dd-mm-yyyy,
# yyyy-mm-dd). Empty strings become NA.
.parseDateAny <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  out <- as.Date(rep(NA_character_, length(x)))
  for (fmt in c("%d.%m.%Y", "%d-%m-%Y", "%Y-%m-%d", "%d/%m/%Y")) {
    todo <- is.na(out) & !is.na(x)
    if (!any(todo)) break
    out[todo] <- as.Date(x[todo], format = fmt, optional = TRUE)
  }
  bad <- is.na(out) & !is.na(x)
  if (any(bad))
    stop(sprintf("Unparseable date value(s) in the source: %s.",
                 paste(head(unique(x[bad]), 5), collapse = ", ")),
         call. = FALSE)
  out
}


.asInt <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  suppressWarnings(as.integer(x))
}


# Stop with a schema error naming the missing columns and the source.
.requireCols <- function(x, cols, what) {
  miss <- setdiff(cols, names(x))
  if (length(miss))
    stop(sprintf(paste0("Unexpected schema of %s: column(s) %s missing.\n",
                        "Available: %s.\nThe source format may have changed."),
                 what, paste(sQuote(miss, FALSE), collapse = ", "),
                 paste(names(x), collapse = ", ")),
         call. = FALSE)
  invisible(TRUE)
}


# Official cantonal numbering (BFS) -> abbreviation. Stable since 1979 (JU).
.kantonAbbrev <- c(
  "ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", "ZG", "FR", "SO", "BS",
  "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG", "TI", "VD", "VS", "NE",
  "GE", "JU"
)


.resetRownames <- function(x) {
  rownames(x) <- NULL
  x
}
