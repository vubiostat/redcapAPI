#' @keywords internal

.curlCompact <- function(x)
{
  x[vapply(x, length, numeric(1)) != 0]
}

.curlDefaultUa <- function()
{
  versions <- c(libcurl = curl::curl_version()$version, `r-curl` = as.character(utils::packageVersion("curl")))
  paste0(names(versions), "/", versions, collapse = " ")
}

.curlConfig <- function(url, token)
{
  cfg <- getOption('curl_config')
  
  if(is.null(cfg)) cfg <- list(headers=list(), fields=NULL, options=list())
  if(is.null(cfg$options)) cfg$options <- list()
   
  structure(list(
    method     = 'POST',
    url        = url,
    headers    = c(cfg$headers, Accept = "application/json, text/xml, application/xml, */*"),
    fields     = cfg$fields,
    options    = modifyList(list(timeout_ms = 3e5,
                                 useragent  = .curlDefaultUa(),
                                 post       = TRUE),
                            cfg$options),
    auth_token = token,
    output     = structure(list(), class = c("write_memory", "write_function"))
  ), class = "request")
}

.curlMergeConfig <- function(x, 
                             y)
{
  if(!is.null(y))
  {
    if(!is.null(y$options)) x$options <- modifyList(x$options, y$options)
    if(!is.null(y$headers)) x$headers[names(y$headers)] <- y$headers
    if(!is.null(y$fields))  x$fields[names(y$fields)] <- y$fields
  }
  x
}

.curlUploadFile <- function(path,
                            type = NULL)
{
  stopifnot(is.character(path), length(path) == 1, file.exists(path))
  if (is.null(type)) type <- mime::guess_type(path)
  curl::form_file(path, type)
}

as.character.form_file <- function(x, ...) x

.curlContent <- function(x, 
                         type = 'text/plain',
                         ...)
{
  stopifnot(inherits(x, "response"))
  raw <- if (inherits(x$content, 'path'))
         {
           readBin(x$content, "raw", file.info(x$content)$size)
         } else
         {
           x$content
         }
  if (length(raw) == 0) return("")

  enc <- if(grepl("charset", x$headers[["content-type"]]))
         {
           toupper(sub('.*charset=([^;]+).*', '\\1', x$headers[["content-type"]]))
         } else
         {
           'ISO-8859-1' # [Default if unspecified](https://www.w3.org/International/articles/http-charset/index)
         }
  x <- iconv(readBin(raw, character()), from = enc, to = 'UTF-8', '\U25a1')
  if(grepl('\U25a1', x)) warning("Project contains invalid characters. Mapped to '\U25a1'.")
  
  if(type == 'text/csv')
  {
    utils::read.csv(x, ...)
  } else if(type == 'application/json')
  {
    jsonlite::fromJSON(x, simplifyVector = FALSE, ...)
  } else
  {
    x
  }
}

.curlPost <- function(body,
                      config)
{
  h    <- curl::new_handle()
  body <- .curlCompact(body)
  
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_list(x = body, 
                         names = "named",
                         add = coll)
  checkmate::reportAssertions(coll)

  flds <- lapply(body, function(x) 
  {
    if(inherits(x, 'list') || inherits(x, 'character'))
    {
      x
    } else
    {
      as.character(x)
    }
  })
  
  config$fields <- c(flds, config$fields)

  curl::handle_setopt(h, .list = config$options)
  if (!is.null(config$fields)) curl::handle_setform(h, .list = config$fields)

  curl::handle_setheaders(h, .list = config$headers)
  on.exit(curl::handle_reset(h), add = TRUE)

  resp <- curl::curl_fetch_memory(config$url, h)
  rh   <- curl::parse_headers_list(resp$headers)
  structure(list(
    url         = resp$url,
    status_code = resp$status_code,
    headers     = rh,
    all_headers = resp$headers,
    cookies     = curl::handle_cookies(h),
    content     = resp$content,
    times       = resp$times,
    request     = config,
    handle      = h
  ), class = "response")
}
