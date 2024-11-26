#' @keywords internal
.calculationToLanguage <- function(x) {
    if(grepl('(^|\\b)if\\(', x, ignore.case = TRUE)) {
        x <- gsub('(^|\\b)if\\(', 'doif(', x, ignore.case = TRUE)
        x <- gsub('(?<![!=])=(?!=)', '==', x, perl = TRUE)
        x <- gsub('<>', '!=', x)
    }
    y <- gregexpr('\\[[^[]+\\]', x)[[1]]
    if(length(y) > 1 || y[[1]] != -1) {
        ytxt <- mapply(substr, x, y + 1, y + attr(y, 'match.length') - 2, USE.NAMES = FALSE)
        yseq <- mapply(seq, y, length.out = attr(y, 'match.length'), USE.NAMES = FALSE, SIMPLIFY = FALSE)
        xseq <- setdiff(seq(nchar(x)), unlist(yseq))
        xbrk <- cumsum(c(FALSE, diff(xseq) > 1))
        xgrp <- split(strsplit(x, '')[[1]][xseq], xbrk)
        xstr <- vapply(xgrp, paste, character(1), collapse = '', USE.NAMES = FALSE)
        nn <- length(xstr) * 2 - 1
        x1 <- character(nn)
        x1[seq(1, nn, by = 2)] <- xstr
        x1[seq(2, nn, by = 2)] <- ytxt
        x2 <- paste(x1, collapse = '')
        str2lang(x2)
    } else {
        str2lang(x)
    }
}

#' @keywords internal
.calculationEval <- function(x, env, call_string) {
    tryCatch(eval(x, envir = env), error = function(e) {
        stop(sprintf('Error in %s: %s', call_string, e[[1]]))
    })
}

#' @keywords internal
.redcapCalculation <- function(expr, dat = NULL) {
    x <- .calculationToLanguage(expr)
    xstr <- as.character(as.expression(x))
    e <- new.env(parent = .redcapCalculationEnvironment())
    if(!is.null(dat)) {
        nr <- nrow(dat)
        res <- lapply(seq_len(nr), function(i) {
            list2env(dat[i,], envir = e)
            .calculationEval(x, e, xstr)
        })
        res <- unlist(res)
    } else {
        res <- .calculationEval(x, e, xstr)
    }
    res
}
