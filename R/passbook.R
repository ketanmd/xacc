#' construct empty passbook
#' @param year basis for dates in the passbook and default year for passbook name
#' @param ploc full path where passbook is to be written
#' @param qOverWrite should an existing passbook be overwritten if needed
#'
makenewpassbook <-
  function(year,
           ploc = inhome('/current/',year,'.passbook'), qOverWrite = FALSE) {

    knownaccounts <- .knowf()()
    if (!length(knownaccounts))
      stop('No known accounts, cannot make passbook\n')

    year <- as.numeric(year)
    if (!is.finite(year))
      stop('numeric year not provided, cannot make passbook\n')

    if (file.exists(ploc) & !qOverWrite)
      stop('passbook for ', year, ' already exists, not making passbook\n')

    cat("Making passbook for", year, "\n")

    eoms <-
      function(year)
        c(as.Date(paste0(year, '-', 2:12, '-1')) - 1, as.Date(paste0(year, '-12-31')))

    adates <-
      data.frame(date = rep(eoms(year), each = nrow(knownaccounts)),
                 stringsAsFactors = FALSE)

    cbind(knownaccounts, adates) %>%
      subset(qPB == 'Y') %>%
      plyr::dlply(., c('date', 'accat'), function(x) {
        paste0(
          collapse = '',
          x$date[1],
          ' bal:',
          stringr::str_pad(x$account, 20 , side = 'right'),
          '  =0.000',
          substr(x$date[1], 6, 7),
          '\n'
        )

      }) %>%
      unname %>%
      unlist %>%
      writeLines(ploc)
    cbind(knownaccounts, adates) %>%
      subset(qPB == 'Y') %>%
      plyr::dlply(., c('date'), function(xx) {
        c(
          list(paste0('################## Begin ',
                      substr(xx$date[1], 1, 7),
                      ' ###################################\n','\n\n')),
          plyr::dlply(xx, c('accat'), function(x) {
            paste0(
              collapse = '',
              x$date[1],
              ' bal:',
              stringr::str_pad(x$account, 20 , side = 'right'),
              '  =0.000',
              substr(x$date[1], 6, 7),
              '\n'
            )
          })
        )
      }) %>%
      unname %>%
      unlist %>%
      writeLines(ploc)

    if (file.exists(ploc))
      cat("Deposited new passbook for", year, "into", ploc, "\n")
  }

