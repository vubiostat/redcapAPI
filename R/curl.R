#' @keywords internal

.curlCompact <- function(x)
{
    lx <- vapply(x, length, numeric(1))
    x[lx != 0]
}

.curlConfig <- function(..., token = NULL)
{
    opt <- list(...)
    structure(list(
        method = NULL,
        url = NULL,
        headers = NULL,
        fields = NULL,
        options = .curlCompact(opt),
        auth_token = token,
        output = NULL
    ), class = "request")
}

.curlSetConfig <- function(config, override = FALSE)
{
    stopifnot(inherits(config, "request"))
    old <- getOption("httr_config")
    if(is.null(old)) old <- .curlConfig()
    if(!override) config <- c(old, config)
    options(httr_config = config)
    invisible(old)
}

.curlUploadFile <- function(path, type = NULL)
{
    stopifnot(is.character(path), length(path) == 1, file.exists(path))
    if (is.null(type)) {
        type <- mime::guess_type(path)
    }
    curl::form_file(path, type)
}

.curlTimeout <- function(seconds)
{
    if (seconds < 0.001) {
        stop("Timeout cannot be less than 1 ms", call. = FALSE)
    }
    config(timeout_ms = seconds * 1000)
}

.curlContent <- function(x, ...)
{
    stopifnot(inherits(x, "response"))
    if (is.path(x$content)) {
        raw <- readBin(x$content, "raw", file.info(x$content)$size)
    } else {
        raw <- x$content
    }
    if (length(raw) == 0) {
        return(NULL)
    }
    enc <- if(grepl("charset", x$headers[["content-type"]]))
      toupper(sub('.*charset=([^;]+).*', '\\1', x$headers[["content-type"]]))
    else
      'ISO-8859-1' # [Default if unspecified](https://www.w3.org/International/articles/http-charset/index)
    x <- iconv(readBin(raw, character()), from = env, to = 'UTF-8', '\U25a1')
    if(grepl('\U25a1', x)) warning("Project contains invalid characters. Mapped to '\U25a1'.")
    jsonlite::fromJSON(x, simplifyVector = FALSE, ...)
}

.curlDefaultUa <- function() {
    # cache this?
    versions <- c(libcurl = curl::curl_version()$version, `r-curl` = as.character(utils::packageVersion("curl")))
    paste0(names(versions), "/", versions, collapse = " ")
}

.curlRequestCombine <- function(x, y) {
    z <- structure(list(
        method = NULL,
        url = NULL,
        headers = NULL,
        fields = c(x$fields, y$fields),
        options = NULL,
        auth_token = NULL,
        output = NULL
    ), class = "request")
    h <- c(x$headers, y$headers)
    o <- c(x$options, y$options)
    z$headers <- h[!duplicated(names(h), fromLast = TRUE)]
    z$options <- o[!duplicated(names(h), fromLast = TRUE)]
    for(i in c('method','url','auth_token','output')) {
        if(!is.null(y[[i]])) {
            z[[i]] <- y[[i]]
        } else if(!is.null(x[[i]])) {
            z[[i]] <- x[[i]]
        }
    }
    z
}

.curlRequestDefault <- function() {
    dr <- structure(list(
        method = NULL,
        url = NULL,
        headers = c(Accept = "application/json, text/xml, application/xml, */*"),
        fields = NULL,
        options = list(useragent = .curlDefaultUa()),
        auth_token = NULL,
        output = structure(list(), class = c("write_memory", "write_function"))
    ), class = "request")
    .curlRequestCombine(dr, getOption("httr_config"))
}

.curlPost <- function(url, body, config = list()) {
    h <- curl::new_handle()
    body <- .curlCompact(body)
    nms <- names(body)
    if(is.null(nms) || any(is.na(nms) | nms == '')) {
        stop("All components of body must be named", call. = FALSE)
    }
    flds <- lapply(body, function(x) x)

    req <- structure(list(
        method = "POST",
        url = url,
        headers = config$headers,
        fields = c(flds, config$fields),
        options = config$options,
        auth_token = config$auth_token,
        output = config$output
    ), class = "request")
    req <- .curlRequestCombine(.curlRequestDefault(), req)
    req$options$post <- TRUE

    curl::handle_setopt(h, .list = req$options)
    if (!is.null(req$fields)) {
        curl::handle_setform(h, .list = req$fields)
    }
    curl::handle_setheaders(h, .list = req$headers)
    on.exit(curl::handle_reset(h), add = TRUE)

    resp <- curl::curl_fetch_memory(req$url, h)
    rh <- curl::parse_headers_list(resp$headers)
    structure(list(
        url = resp$url,
        status_code = resp$status_code,
        headers = rh,
        all_headers = resp$headers,
        cookies = curl::handle_cookies(h),
        content = resp$content,
        times = resp$times,
        request = req,
        handle = h
    ), class = "response")
}
