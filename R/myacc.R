utils::globalVariables(c('.', '%>%', '%<>%', '%T>%'))

#' find all csvfiles to be processed
#'
#' @export
findcsvfiles <- function() Sys.glob(inhome('/*/*.csv'))

#' find all configfiles to be processed
#'
#' @export
findconfigfiles <- function() Sys.glob(inhome('/config/*')) %>% grep('~$', ., value = TRUE, invert = TRUE)

#' find all blocksfiles to be processed
#'
#' @export
findblocksfiles <- function() Sys.glob(inhome('/*/*.passbook'))

#' hints for data maintenance
#' @export
hu <- function() {
message("makenewpassbook(2030) : make new passbook
changes : check if any accounting info has changed
upd  : verbose update of accounting information
supd : silent update of accounting information
pyall : transaction blocks as lines
pycsv : csv form of all transactions
pypq  : all price lines
pyprx : all execution prices
")
}

#' Selector expression hints
#' @export
hs <- function() {
message("today    : todays date
iacc     : increase accounts
oacc     : not interesting accounts for de-selection
tacc     : tariffs accounts
until    : selection upto and including date [today]
since    : selection on and after date [today]
now      : transactions before (0) and after(0) today
acc      : select by account
notacc   : deselect by account
tunits   : select by units
tacctype : select by acctype
notunits : deselect by units
")
}

#' Transaction extraction hints
#' @export
hc <- function() {
message("
std  : all transactions | xfields | until | as_tibble

  filters:
  gacc : good accounts   ( Cash Broker Bonds Ren Retire )
  tacc : tariff accounts ( Tax Trips Expenses Equity Sink Family )
  iacc : income accounts ( Earnings Invest Income Interest )
  dollars : $ units
  bale : balance note entries
  bige : amount > threshold(1000)
  expe : expense note entries
  fixe : fixed deposit entries
  locke : locked up fixed deposit entries

  cby(X, byf) : summarized data
  pbyf(...)   : plot of cby output
")
}

#' check if any accounting info has changed
#' @export
anychanges <- function() length(changedfiles()) > 0

changedfiles <- function() {
    infiles <- c(findcsvfiles(), findblocksfiles(), findconfigfiles())
    outfile <- outhome('/csv.passbook')
    if (!file.exists(outfile)) return(infiles)
    newinfiles <- utils::file_test('-nt', setdiff(infiles, outfile), outfile)
    return(infiles[newinfiles])
}

#' invoke upd with messages suppressed, update accounting info by reprocessing all input files
#' @param reprocess reprocess files even if nothing changes
#' @export
supd <- function(reprocess = FALSE) suppressMessages(upd(reprocess, verbose = FALSE))

#' update accounting info by reprocessing all input files
#' @param reprocess reprocess files even if nothing changes
#' @param verbose turn on debugging output ?
#' @import readr
#' @export
upd <- function(reprocess = FALSE, verbose = FALSE) {
  #############

  outfile <- outhome('/csv.passbook')
  badcsvfile   <- inhome('/badcsv/', Sys.Date(), '.bad.csv')
  csvfiles     <- findcsvfiles()
  blocksfiles  <- setdiff(findblocksfiles(), outfile)

  if (!length(c(csvfiles,blocksfiles))) {
    message('Found no csv files or blocksfiles to process.',
            'files should match ./*/*.csv or ./*/*.passbook .')
    message('Nothing to do.')
    return(invisible(FALSE))
  }
  if (!dir.exists(outhome(''))) dir.create(outhome(''))

  if (!anychanges() && !reprocess) {
    message('No input files appear to have changed.')
    message('Nothing to do.')
    return(invisible(FALSE))
  }
  #############

  if (length(csvfiles)) {
    cat('== Processing csv files ==\n')
    (csvfiles
      %>% csvprocess(., badcsvfile)
      %T>% writeLines(., outfile)
      %>% grep('^P', ., value = TRUE)
      %>% grep(' q_', ., value = TRUE)
      %>% sub('\n\n', '', .)
    ) -> pyqlines
    unlink(outhome('/pyquotes'))
    if (length(pyqlines)) writeLines(pyqlines, outhome('/pyquotes'))
  }

  message('== Process blocks files ==')
  (union(blocksfiles, outfile)
    %>% grep('~$', . , value = TRUE, invert = TRUE)
    %>% blocksprocess(., verbose)
    %>% write(., outhome('/pyall'))
  )

  message('== Checking pyall for inconsistencies ==')
  suppressWarnings(emesses <-
                       system2(ledgercmd(), args = ' source -f - ',
                               stdin = outhome('/pyall'), stdout = TRUE, stderr = TRUE) )
  if (length(emesses) > 0) {
      erroringlines <- readr::parse_number(emesses[seq(1, length(emesses), 5)])
      print(list(emesses,
               emesses[seq(1, length(emesses), 5)],
               erroringlines
               ))
      lapply(erroringlines, pyall) -> emesslist
      message(paste(erroringlines, ':\n ',
                    unlist(emesslist), '\n',
                    emesses[4+seq(1, length(emesses), 5)], '\n'))
  }

  message('== Generating pycsv ==')
  system2(ledgercmd(), args = ' csv -f - ',
          stdin  = outhome('/pyall'), stderr = FALSE, stdout = outhome('/pycsv') )

  message('== Generating pyprx ==')
  system2(ledgercmd(), args = ' prices -f - ',
          stdin  = outhome('/pyall'), stderr = FALSE, stdout = outhome('/pyprx') )

  message('== Write pyprq ==')
  system(paste(
    'cat ', outhome('/pyall'),
    ' | egrep "^P" ',
    ' | egrep " q_" ',
    ' > ', outhome('/pyprq'))
  )

  return(message("\n\nReady to load.\n"))
}

#######################################################################################
#######################################################################################
#' for debugging and for internal use extract all transaction lines
#'
#' @return transactions lines as strings invisibly
#' @param pos which line is of interest (usually for debugging)
#' @export
pyall <- function(pos = 0) {
  outhome('/pyall') %>%
  readLines %>%
  paste0('\n') -> pyout
  bn <- -30
  en <- 30
  pinds <- pos + bn:en
  if (pos == 0) return(invisible(pyout))

  bn <- -min(match('\n', pyout[pos + -1:bn]))
  en <- min(match('\n', pyout[pos + 1:en]))
  pinds <- paste0('  ', pyout[pos + bn:en])
  pinds[1-bn] <- paste0('>>', pyout[pos])
  return( paste0(pinds, collapse = '') )
}

#' for debugging and for internal use return all transactions as csv lines
#'
#' @return transactions as csv lines
#' @export
pycsv <- function() {
  outhome('/pycsv') %>%
    utils::read.csv(header = FALSE) -> a
  names(a) <-
    c('date', 'unk', 'note', 'account', 'units', 'amount', 'comment')
  a <- a[c('note', 'date', 'account', 'units', 'amount')]
  levels(a$units) <- c('$', levels(a$units)[-1], 'reallycash')
  a$units[is.na(a$units)] <- 'reallycash'
  a$date <- as.Date(a$date)
  a
}

#' return all price lines
#' @param pypfile file containing all price quotes  pyquotes file
#' @return price lines as strings
#' @import dplyr
#' @export
pypq <- function(pypfile) {
  message('-', appendLF = FALSE)
  #message("loading info from ", pypfile, '\n')
  if (!file.exists(pypfile)) return(NULL)
    utils::read.table(pypfile, header = FALSE, stringsAsFactors = FALSE)  -> a
    if (!length(a)) return(NULL)
    a %>%
    stats::setNames(c(V1='foo', V2='date', V3 = 'units', V4 = 'price')) %>%
    rbind(., data.frame(foo = '', date = '1970-1-1', units = 'q_tbill', price = 1.0)) %>%
    dplyr::mutate(foo = NULL, date = as.Date(.cleandates(date)),
                  q_quote = grepl('q_', units), units = sub('^q_', '', units))
}

#' return all price lines
#'
#' @return price lines as strings
#' @import dplyr
#' @export
pyprx <- function() {
    utils::read.table(outhome('/pyprx'), header = FALSE, stringsAsFactors = FALSE) %>%
        stats::setNames(c(V1 = 'date', V2 = 'units', V3 = 'price')) %>%
            within({
                date = as.Date(date)
                q_quote = FALSE
            }) -> aa
    subset(aa, magrittr::not(duplicated(aa))) -> xprices
#            subset(., duplicated(.) %>% not) ->
#            xprices

    qprices <- pypq(outhome('/pyprq'))
    #utils::read.table(outhome('/pyprq'), header = FALSE, stringsAsFactors = FALSE) %>%
    #    stats::setNames(c(V1='foo', V2='date', V3 = 'units', V4 = 'price')) %>%
    #        dplyr::mutate(foo = NULL, date = as.Date(date),
    #                      q_quote = grepl('q_', units), units = sub('^q_', '', units)) ->
    #        #subset(., "["(., -1) %>% duplicated %>% not) ->
    #        qprices
    if (0) { # never seem to have needed to take this extreme step
        readLines(outhome('/pyall')) %>% grep(pattern = '^P', value = TRUE) %>%
            textConnection %>% utils::read.table(head = FALSE) %>%
            stats::setNames(c(V1 = 'foo', V2 = 'date', V3 = 'units', V4 = 'price')) %>%
            dplyr::mutate(foo = NULL, date = as.Date(date),
                          q_quote = grepl('q_', units), units = sub('^q_', '', units)) %>%
            subset(., "["(., -1) %>% duplicated %>% magrittr::not) %>%
            dplyr::arrange(units, date, q_quote) -> nprices
        nprices %>% saveRDS(outhome('/pynewprx'))

        #str(list(xprices, qprices, nprices))
    }

    if (!is.null(qprices))
        rbind(xprices, qprices[,names(xprices)]) -> xprices
    xprices %>%
        dplyr::arrange(units,date,q_quote)
}

#' assign cost to each transaction
#' @import plyr
#' @param a transactions object
#' @param p prices object
#' @export
costattach <- function(a, p) {
  a %>%
  plyr::ddply(c('units', 'account'), function(x) {
    p %>%  subset(., units == x$units[1] & .$q_quote == FALSE) -> plx
    hasp <- x$date %in% plx$date
    x$cost[hasp] <- plx$price[match(x$date, plx$date)[hasp]] * x$amount[hasp]
    x$cost[!hasp] <- 0
    #subset(x, amount != 0 | round(cumsum(amount), 8) != 0) -> x
    x
  })
}

#' assign value to each transaction
#' @import dplyr
#' @param a transactions object
#' @param p prices object
valattach <- function(a, p) {
  a$note <- as.character(a$note)
  a %>% costattach(p) %>%
    plyr::ddply(., c('units', 'account'), function(x) {
    plq <- subset(p, units == x$units[1]) # & q_quote == TRUE)
    x <- dplyr::arrange(x, date)

    newdates <- c()
    newdates <- plq$date ## add valuation dates for every date at which we have a price
    #            newdates <- c(seq.Date(x$date[1], tail(x$date, 1), 'month'), newdates)
    #newdates <- lubridate::ceiling_date(newdates, 'month') + lubridate::days(-1)
    newdates <- unique(newdates)
    olddates <- newdates %in% x$date
    if (!all(olddates)) {
      xnew <- x[rep(1, sum(!olddates)), , drop = FALSE]
      xnew$date <- newdates[!olddates]
      xnew$amount <- 0
      xnew$note <- 'price quote update'
      xnew$cost <- 0
      #str(list(x, xnew))
      x <- rbind(x, xnew)
      x <- dplyr::arrange(x, date)
    }

    x$lprice <- c(NA, plq$price)[1 + findInterval(x$date, plq$date)]
    x$val = diff(c(0, cumsum(x$amount) * x$lprice))
    x
  })
}

xfields <- function(a) {
  fixaccount <- function(account,units) {
    account %>%
      as.character %>%
      sub('Cash:(SCTY:.*|.*:cd|.*:fixed)', 'Bonds:\\1', .) %>%
      ifelse(grepl('^B_|eebond|tips|ibond|frn', units),
             sub('.*?:(.*)', 'Bonds:\\1', .),
             .) %>%
      ifelse(grepl('med|rtc|rida|ridge', units),
             sub('.*?:(.*)', 'Ren:\\1', .),
             .) %>%
      as.factor(.)
  }
  fixacctype <- function(account)
    factor(sub(':.*', '', account),
           levels = qw('Cash,Broker,Bonds,Ren,Retire,Equity,Earnings,Income,Interest,Tax,Trips,Expenses,Sink,Family'),
           ordered = TRUE)

  a %>%
    dplyr::mutate(.,
#                  month   = lubridate::ceiling_date(date, 'month') + lubridate::days(-1),
#                  year    = lubridate::ceiling_date(date, 'year')  + lubridate::days(-1),
                  account = fixaccount(.$account,.$units),
                  place   = sub('(.*?:.*?):.*', '\\1', fixaccount(.$account, .$units)),
                  acctype = fixacctype(fixaccount(.$account, .$units)))
}

#' list available note fields
#' @export
notes <- function() (std()$note
    %>% as.character
    %>% strsplit(., ' +')
    %>% unlist
    %>% grep(value = TRUE, ':', .)
    %>% sub(':.*', '', .)
    %>% sort
    %>% unique)

#' utility to extract note fields
#' @param x object from which to extract notefield
#' @param f name of notefield, should appear in the form f:<finfo> in the note
#' @export
fromnote <- function(x, f) {
    fname <- deparse(substitute(f))
    x %<>% subset(., grepl(paste0(' ', fname, ':'), .$note))
    x[[paste0('note.',fname)]] <-
        sub(paste0('.* ',fname,':(.*?) .*'), '\\1', x$note)
    x
}

#' general aggregation function, should be applied directly to raw data
#' @param a transactions object
#' @param byfields grouping variables for aggregation
#' @import gestalt
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import readr
sumt <- function(a, byfields = c('account', 'units')) {
  q_sync    <- 'sync' %in% byfields
  byfields  <- setdiff(byfields, 'sync')
  timetypes <- c('date','month','year')
  ntf       <- setdiff(byfields, timetypes)
  tf        <- intersect(byfields, timetypes)
  valf <- c('val', 'amount', 'lprice', 'cost')

  aatbl <- tibble::as.tibble(matrix(rep(0, length(valf)), 1, length(valf)))
  names(aatbl) <- valf

  if ('month' %in% tf) a <- dplyr::mutate(a, month = lubridate::ceiling_date(date, 'month') + lubridate::days(-1))
  if ('year' %in% tf)  a <- dplyr::mutate(a, year  = lubridate::ceiling_date(date, 'year')  + lubridate::days(-1))

  mf <-
    pre: (
      totibble : tibble::as.tibble %>>>%
      dplyr::mutate(Date = date) %>>>%
      dplyr::group_by_at(!!c(byfields, 'units')) %>>>%
      dplyr::arrange(Date) %>>>%

      dplyr::summarize(val    = sum(val),
                       amount = sum(amount),
                       cost   = sum(cost),
                       lprice = dplyr::last(lprice)) %>>>%

      dropzeros : dplyr::filter(round(amount, 4) != 0 | round(val, 4) != 0) %>>>%

      dplyr::group_by_at(!!byfields) %>>>%
      dplyr::summarize(val    = sum(val),
                       amount = sum(amount),
                       cost   = sum(cost),
                       lprice = dplyr::last(lprice)) %>>>%
      dplyr::select(c(tf, ntf, valf)) # 'val'))
      ) %>>>%

    proc: (
      tidyr::unite(gfac, !!ntf, sep = '+') %>>>%
      dplyr::ungroup %>>>%
      syncdates: tidyr::complete_(c(tf, 'gfac'), fill = aatbl) %>>>%

      cumulate : (dplyr::group_by(gfac) %>>>%
                  dplyr::arrange_(tf) %>>>%
                  dplyr::mutate(cval = cumsum(val)) %>>>%
                  dplyr::ungroup) %>>>%

      tidyr::separate(gfac, sep = '\\+', into = ntf)
      ) %>>>%
    post: (
        dplyr::mutate_if(is.numeric, fn(x ~ round(x, 2))) %>>>%
        dplyr::arrange_(c(tf, ntf))
      )

  if (!q_sync) mf$proc$syncdates <- identity
  if (!length(tf)) mf$proc <- identity

  a <- mf(a)
  attr(a, 'byfields') <- byfields
  attr(a, 'q_sync')   <- q_sync
  a
}

##########################################################################################
##########################################################################################
##########################################################################################

#' special dates
#' @export
today  <- Sys.Date()

############
## SELECTION

#' select transactions on or before date e
#' @param aca transactions object
#' @param e date as a string
#' @export
until  <- function(aca, e = today) {
  if ('date'  %in% names(aca)) return(subset(aca, date <= as.Date(e)))
  if ('month' %in% names(aca)) return(subset(aca, month <= as.Date(e)))
  if ('year'  %in% names(aca)) return(subset(aca, year <= as.Date(e)))
  return(aca)
}

#' select transactions on or after date b
#' @param aca transactions object
#' @param b date as a string or Date object, defaults to beginning of current year
#' @export
since  <- function(aca, b = lubridate::floor_date(today-10, 'year')) {
  if ('date'  %in% names(aca)) return(subset(aca, date >= as.Date(b)))
  if ('month' %in% names(aca)) return(subset(aca, month >= as.Date(b)))
  if ('year'  %in% names(aca)) return(subset(aca, year >= as.Date(b)))
  return(aca)
}

#' pick out transactions around today
#' @param aca transactions object
#' @param before number of days before today to select
#' @param after number of days after today to select
#' @export
now <- function(aca, before = 0, after = 0)
  aca %>% since(today - before) %>% until(today + after)

#' note selection
#' @param aca transactions object
#' @param a regular expression of note field to select
#' @export
note <- function(aca, a) {
  aca %>% subset(., grepl(a, .$note, ignore.case = TRUE))
}

#' account selection
#' @param aca transactions object
#' @param a regular expression of accounts to select
#' @export
acc <- function(aca, a)
    aca %>% subset(., grepl(a, .$account, ignore.case = TRUE))

#' account deselection
#' @param aca transactions object
#' @param a regular expression of accounts to deselect
#' @export
notacc <-
  function(aca, a)
    (!grepl(a, aca$account, ignore.case = TRUE)) %>%
    subset(aca, .)

#' acctype selection
#' @param aca transactions object
#' @param u regular expression of acctype to select
#' @export
tacctype  <- function(aca, u)
  aca %>% subset(., grepl(u, .$acctype, ignore.case = TRUE))

#' units selection
#' @param aca transactions object
#' @param u regular expression of units to select
#' @export
tunits  <- function(aca, u)
  aca %>% subset(., grepl(u, .$units, ignore.case = TRUE))

#' units de-selection
#' @param aca transactions object
#' @param u regular expression of units to de-select
#' @export
notunits  <- function(aca, u)
  aca %>% subset(., !grepl(u, .$units, ignore.case = TRUE))

######################################################################################
######################################################################################
######################################################################################

#' 'standard loading scheme
#' @param intrans transactions objecct
#' @param inprx   price info
#' @import tibble
#' @export
std   <- function(intrans = pycsv(), inprx = pyprx(), e = today)
    valattach(intrans, inprx) %>% xfields %>% until(e) %>% tibble::as_tibble(.)

#' good accounts for selection
#' @export
gacc   <- gestalt::fn(x ~ acc(x, 'Cash|Broker|Bonds|Ren|Retire'))

#' tariff accounts for selection
#' @export
tacc    <- gestalt::fn(x ~ acc(x, 'Tax|Trips|Expenses|Equity|Sink|Family'))

iacc    <- gestalt::fn(x ~ acc(x, 'Earnings|Invest|Income|Interest'))

#' dollar unit records
#' @export
dollars <- gestalt::fn(x ~ tunits(x, '\\$'))

#' see good accounts
#' @export
cg <- function(...)  std(...) %>% gacc

#' see tariff accounts :  (Tax, Trips, Expenses)
#' @export
ct <- function(...)  std(...) %>% tacc

#' see good accounts with dollar units
#' @export
cgc <- function(...)  std(...) %>% gacc %>% dollars

#' see investment entries : (all but dollar unit position)
#' @export
cgi <- function(...)  std(...) %>% gacc %>% notunits('\\$')

#' select balance entries
#' @param x output of std or equivalent
#' @export
bale <- gestalt::fn(x ~ subset(x, grepl('^balance', x$note)))

#' select big amount entries
#' @param threshold only adjustments absolutely bigger than threshold
#' @param x output of std or equivalent
#' @export
bige <- gestalt::fn(threshold = 1000 , x ~ subset(x, abs(x$amount) > threshold))

#' select xpense entries
#' @param x output of std or equivalent
#' @export
expe <- gestalt::fn(x ~ subset(x, grepl('Expenses', .$note) & .$account != 'Expenses'))

#' see fixed deposit entries
#' @param x output of std or equivalent
#' @export
fixe <- gestalt::fn(x ~ subset(x ~ grepl('type:(cd|ee|frn|tips|ibond|tbill|corp)', x$note)))

#' see fixed deposit instruments still locked up: absolutely bigger than threshold (default:1000)
#' @param x output of std or equivalent
#' @import dplyr
#' @export
locke <- gestalt::fn(x ~ fixe(x) %>% dplyr::mutate(edate = sub('.*matdate:(.*?) .*', '\\1', .$note))
                        %>% subset(as.Date(.$edate) > today & date <= today & grepl('start ', .$note) & .$amount > 0))

bfields <- function(iam) {
    imlets <- sort(unlist(strsplit(iam, '')))
    byfields <- c()
    if ('u' %in% imlets) byfields <- c('units',   byfields)
    if ('a' %in% imlets) byfields <- c('account', byfields)
    if ('p' %in% imlets) byfields <- c('place',   byfields)
    if ('n' %in% imlets) byfields <- c('note',    byfields)
    if ('t' %in% imlets) byfields <- c('acctype', byfields)

    if ('s' %in% imlets) byfields <- c('sync',    byfields)
    if ('d' %in% imlets) byfields <- c('date',    byfields) else
    if ('m' %in% imlets) byfields <- c('month',   byfields) else
    if ('y' %in% imlets) byfields <- c('year',    byfields)
    byfields
}

#' std utility to see various views of accounts
#' @param byf encoding of by fields
#' @param X transactions object
#' @export
cby <- function(byf, X = std() %>% gacc) {
  byf <- deparse(substitute(byf))
  byf <- bfields(byf)
  sumt(X, byf)
}

#' smart plot of extracted data,
#' stack plots,
#' pie charts,
#' multi-panel plots [deprecated?]
#' @import ggplot2
#' @import tidyr
#' @param ...  cby arguments
#' @export
pby <- function(...) {
  X <- cby(...)
  byf    <- attr(X, 'byfields')
  q_sync <- attr(X, 'q_sync')

  ttypes <- c('date','month','year')
  ntf <- setdiff(byf, ttypes)
  tf  <- intersect(byf, ttypes)

  if (!length(tf))
    return(graphics::pie(x = X$val, labels = interaction(subset(X, select = byf))))

  if (!length(ntf))
    return(ggplot(X, aes_string(x = tf, y = 'cval'))
           + geom_point() + geom_line())

  if (q_sync)
    return(ggplot(tidyr::unite(X, gfac, !!ntf, sep = '+'),
                  aes_string(x = tf, y = 'cval', group = 'gfac', color = 'gfac'))
           + geom_line(position = 'stack'))

  return(ggplot(X, aes_string(x = tf, y = 'cval'))
         + geom_point() + geom_line()
         + facet_wrap(stats::formula(paste('~ ', paste(collapse = '+', ntf)))))
}

#####################################################
##################################################

...examples <- function() {
  ## examples of usage:

  ## test invocation
  ## blockstest()

  ## make new passbook
  ## makenewpassbook(2030)

  ## process inputs, update accounting files
  upd() #-> a

  ## look at processed blocks for debugging
  ## pyall(3)

  ## load accounting data into perusable form
    ## std() # all but entries
    ## cs()  # all but Equity entries
    ## ct()  # all tariff entries
    ## ci()  # interesting entries
    ## cg()  # brokerage entries
    ## cc()  # cash entries
    ## ca()  # balance adjustments
    ## cax()  # xpense adjustments
    ## cf()  # fixed instrument entries

  ## cby(au) ...

  ## cg() to see just investments
  ## cc() to see cash balances
  ## ci() to see cash and investment balances
  ## ct() to see interesting balances: Tax, Trips, Expenses
  ## cs() to see all balances
  ## cf() to see fixed instrument transactions

}
########################################################################################
########################################################################################
## specific tasks


#' not yet functional test to make sure things are working as expected
#' @param n which csvfile to test
#' @param fil alternatively, explicitly give the name of the file to test
#' @export
blocktest <- function(fil = 'passbook.example', n = 0) {
  if (n != 0) fil <- findcsvfiles()[n]
  blocksprocess(fil)
}
