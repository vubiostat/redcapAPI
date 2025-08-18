#' @keywords internal

.redcapCalculationEnvironment <- function() {
    mather <- function(f, na.rm = TRUE, ...) {
        args <- list(...)
        do.call(f, c(args, na.rm = na.rm))
    }

    numeric_functions <- list(
        round = function(number, digits = 0) round(number, digits),
        roundup = function(number, digits = 0) {
            base <- 10^digits
            ceiling(number * base) / base
        },
        rounddown = function(number, digits = 0) {
            base <- 10^digits
            floor(number * base) / base
        },
        sqrt = function(x) sqrt(x),
        abs = function(x) abs(x),
        exponential = function(...) mather(exp, na.rm = FALSE, ...),
        min = function(...) mather(min, ...),
        max = function(...) mather(max, ...),
        mean = function(...) mather(mean, ...),
        median = function(...) mather(median, ...),
        mod = function(dividend, divisor) dividend %% divisor,
        sum = function(...) mather(sum, ...),
        stdev = function(...) mather(sd, ...),
        log = function(number, base) log(number, base),
        isnumber = function(x) !is.na(suppressWarnings(as.numeric(x))),
        isinteger = function(x) grepl('^[0-9]$', x)
    )

    dater <- function(date) {
        if(inherits(date, 'character')) {
            date <- gsub('"|\'', '', date)
            useDT <- FALSE
            if(any(date %in% 'now')) {
                now <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
                date[date == 'now'] <- now
                useDT <- TRUE
            }
            if(any(date %in% 'today')) {
                today <- format(Sys.Date(), '%Y-%m-%d')
                date[date == 'today'] <- today
            }
            if(useDT) {
                date <- as.POSIXct(date)
            } else {
                date <- as.Date(date)
            }
        }
        date
    }

    date_functions <- list(
        year = function(date) format(dater(date), '%Y'),
        month = function(date) format(dater(date), '%m'),
        day = function(date) format(dater(date), '%d')
    )

    searcher <- function(haystack, needle) {
        grepl(needle, haystack, ignore.case = TRUE)
    }

    text_functions <- list(
        contains = function(h, n) searcher(h, n),
        not_contain = function(h, n) !searcher(h, n),
        starts_with = function(h, n) searcher(h, sprintf('^%s', n)),
        ends_with = function(h, n) searcher(h, sprintf('%s$', n)),
        left = function(x, n = 1) substr(x, 1, n),
        right = function(x, n = 1) substr(x, nchar(x) - n + 1, nchar(x)),
        length = function(x) nchar(x),
        find = function(needle, haystack) {
            m <- c(regexpr(needle, haystack, ignore.case = TRUE)[[1]])[1]
            m[m == -1] <- 0
            m
        },
        replace_text = function(h, s, r) gsub(s, r, h, fixed = TRUE),
        mid = function(x, start = 1, n = 1) substr(x, start, start + n - 1),
        concat = function(...) do.call(paste0, list(...)),
        concat_ws = function(sep, ...) do.call(paste0, list(list(...), collapse = sep)),
        upper = function(x) toupper(x),
        lower = function(x) tolower(x),
        trim = function(x) trimws(x)
    )

    misc_functions <- list(
        doif = function(test, yes, no) {
            ifelse(test, yes, no)
        },
        datediff = function(date1, date2, units = 'd', signed = FALSE) {
            date1 <- dater(date1)
            date2 <- dater(date2)
            # "Date" objects will take on 'UTC' timezone by default
            # force character conversion
            if(inherits(date1, 'Date')) {
                date1 <- as.POSIXct(format(date1, '%Y-%m-%d'))
            }
            if(inherits(date2, 'Date')) {
                date2 <- as.POSIXct(format(date2, '%Y-%m-%d'))
            }
            dt <- as.numeric(difftime(date2, date1, units = 'secs'))
            if(!signed) dt <- abs(dt)
            den <- switch(units,
                s = 1,
                m = 60,
                h = 3600,
                d = 86400,
                M = 2630016, # 86400*30.44
                y = 31556952 # 86400*365.2425
            )
            dt / den
        },
        isblankormissing = function(value, missing_value_code = NULL) {
            is.na(value) | is.null(value) | value %in% c('', missing_value_code)
        }
    )

    redcap_functions <- c(numeric_functions, date_functions, text_functions, misc_functions)
    list2env(redcap_functions)
}
