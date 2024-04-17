# These things are definable via system variables

options(width = 200)

EXPENDABLE_USER   <- Sys.getenv('EXPENDABLE_USER', "bstat_api_user")
EXPORT_REPORTS_ID <- as.numeric(strsplit(Sys.getenv('REPORT_IDS', '357209'), ',')[[1]])


# Determine if tests can be run based on the system variables
RUN_USER_TESTS <- length(EXPENDABLE_USER) == 1
RUN_REPORTS_TEST <- TRUE

tryCatch( {exportReportsTyped(rcon, report_id = EXPORT_REPORTS_ID[1]) }, 
          error = function(cond) RUN_REPORTS_TEST <<- FALSE)
