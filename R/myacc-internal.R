#' find input files
#' @param ... strings to append to getwd() to determine location of inputs
#' @return location of input files
inhome    <- function(...) paste0(getwd(),  ...)

#' find location of output files
#' @param ... relative path for output
#' @return location where output files should be placed
outhome   <- function(...)
  paste0(inhome(), '/processed', ...)

#' find config files
#' @param ... as name of config file
#' @return location of config file
known   <- function(...) {
    kf <- paste0(inhome(), '/config/', ...)
#    kf <- paste0(inhome(), '/known.', ...)
    message('known file is ', kf)
    kf
}

#' return command to invoke ledger
#' @return ledger command
ledgercmd <- function()
  '/usr/bin/ledger'

#' set font size for plots
#' @param size font scale for output plots
bigtext <- function(size = 24)
    ggplot2::theme(text = ggplot2::element_text(size = size))

.ifset <- function(t, c, v) {
    if (length(c)) t[c] <- v
    t
}

.tonumeric <- function(from)
  as.numeric(gsub(',', '',
                  gsub('\\)', '',
                       gsub(
                         '\\(', '-',
                         gsub('\\$', '',
                              as.character(from))
                       ))))


.knowf <- function() {

  kamap <-
    utils::read.table(
      header = TRUE,
      stringsAsFactors = FALSE,
      known('accounts')
    )
  knownaccounts <- dplyr::mutate(
    kamap,
    account = sub('.*?:', '', fullacc),
    accat   = sub(':.*', '', account)
  )
  function(party = NULL, field = NULL) {
    if (is.null(party)) return(knownaccounts)
    retval2 <-
      subset(knownaccounts, grepl(tolower(party), tolower(account)))
    retval <-
      subset(knownaccounts, tolower(party) == tolower(account))
    if (nrow(retval) == 1) return(retval[1, field])
    return(party)
  }
}

# try to match x in column fr
# for any row that matches, use column to to redefine x
# for any row that does not match, use column ax to redefine x
# so, no original value of x survives.
.aamf <- function(x, acmapfile, ax, fr, to) {
  aa <- utils::read.table(header = TRUE,
                          stringsAsFactors = FALSE,
                          acmapfile)
  aam <- match(x, aa[[fr]], 0)
  #        if (any(aam == 0)) {
  #            message('The following lines had unknown mapping\n')
  #            print(x[aam == 0])
  #        }
  x[aam != 0] <- aa[aam[aam != 0], to]
  x[aam == 0] <- ax[aam == 0]
  x
}

.cleanna <- function(x, dval) ifelse(is.na(x), dval, x)


.cleandates <- function(x) {
  x <- gsub(pattern = '-',
            replacement = '/',
            as.character(x))

  ## 2007
  n4 <- nchar(x) == 4
  x[n4] <- paste0(x[n4], '/12/31')

  ## 20081231
  n8 <- nchar(x) == 8 & !grepl('/', x)
  x[n8] <- paste0(substr(x[n8], 1, 4), '/',
                  substr(x[n8], 5, 6), '/',
                  substr(x[n8], 7, 8))
  ## 1/1/2006
  ## 1/12/2005
  ## 10/2/2004
  ## 10/12/2003
  n5 <- !grepl('/', substr(x, nchar(x) - 3, nchar(x)))
  x[n5] <-
    paste0(substr(x[n5], nchar(x[n5]) - 3, nchar(x[n5])), '/',
           substr(x[n5], 1,              nchar(x[n5]) - 5))

  n9 <- substr(x, nchar(x) - 1, nchar(x) - 1) == '/'
  x[n9] <- paste0(substr(x[n9], 1,            nchar(x[n9]) - 2), '/0',
                  substr(x[n9], nchar(x[n9]), nchar(x[n9])))

  n9 <- nchar(x) == 9 & substr(x, nchar(x) - 4, nchar(x) - 4) == '/'
  x[n9] <-
    paste0(substr(x[n9], 1,              nchar(x[n9]) - 5), '/0',
           substr(x[n9], nchar(x[n9]) - 3, nchar(x[n9])))
  x
  #    gsub('/', '-', x)
}

.kvof <- function(b) {
  strsplit(b, ' +') %>% unlist -> bs
  bs[1] <- paste0('startdate:', .cleandates(bs[1]))
  nobs <- paste(bs[!grepl(':', bs)], collapse = ' ')
  kvs <- bs[grepl(':', bs)]
  kv <- stats::setNames(sub('.*?:', '', kvs), sub(':.*', '', kvs))
  kv['note'] <- nobs
  as.list(kv)
}

.dig <- function(a, n) {
  for (x in names(a))
    if (is.numeric(a[[x]]))
      a[[x]] <- round(a[[x]], n)
    a
}

#########################################

## utility code below here - should not need to modify it normally.
## does not really belong in this library - hopefully innocuous

#' for debugging, limit str output to six elements
#' @param ... pass through to str
#' @export
lstr <- function(...)
  utils::str(..., list.len = 6)

#' for internal and for public use, mimic perl qw function
#' @param x string
#' @param split delimiter to split string x
#' @export
qw <- function(x, split = ',')
  unlist(strsplit(x, split))

