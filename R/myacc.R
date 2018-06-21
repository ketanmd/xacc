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
message("std  : all transactions
cs   : all     : std | xfields | until today
ct   : tariffs : cs | acc(tacc)
cb   : cs + notacc(oacc)
cc   : cash          : cs | notacc(oacc) | tunits($)
ci   : brokers       : cs | notacc(oacc) | notunits($)
ca   : adjustments   : cs | adj > thresh[1000]
cax  : adjustments   : ca | expenses
cf   : fixed         : cs | fixeddeposits
cl   : locked up now : cf | matdate>date
cby  : table(byf, c.[ci])
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

  cat('== Process blocks files ==\n')
  (union(blocksfiles, outfile)
    %>% grep('~$', . , value = TRUE, invert = TRUE)
    %>% blocksprocess(., verbose)
    %>% write(., outhome('/pyall'))
  )

  cat('== Generating pycsv ==\n')
  system2(
    ledgercmd(),
    args = ' csv -f - ',
    stdin  = outhome('/pyall'),
    stdout = outhome('/pycsv')
  )

  cat('== Generating pyprx ==\n')
  system2(
    ledgercmd(),
    args = ' prices -f - ',
    stdin  = outhome('/pyall'),
    stdout = outhome('/pyprx')
  )

  cat('== Write pyprq ==\n')
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
#' @param n how many lines around pos to be displayed
#' @export
pyall <- function(pos = 0, n = 5) {
  outhome('/pyall') %>%
  readLines %>%
  paste0('\n') -> pyout
  pinds <- pos + (-n/2):(n/2)
  if (pos != 0) return(cat(paste0(pinds, ' ', pyout[pinds]) ))
  invisible(pyout)
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
#' @export
pypq <- function(pypfile) {
  message("loading info from ", pypfile, '\n')
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
    account %<>%
      as.character %>%
      sub('Cash:((SCTY|UST):.*|.*:(cd|fixed))',
                  'Bonds:\\1', .) %>%
      ifelse(grepl('^B_', units),
             sub('.*?:(.*)', 'Bonds:\\1', .), .)
    runits <-grep('med|rtc|rida|ridge', units)
    account[runits] <- sub('.*?:(.*)', 'Ren:\\1', account[runits])
    as.factor(account)
  }
  fixacctype <- function(account)
    factor(sub(':.*', '', account),
           levels = qw('Cash,Broker,Bonds,Ren,Retire,Earnings,Income,Interest,Tax,Trips,Expenses,Equity'),
           ordered = TRUE)

  a %>%
    dplyr::mutate(.,
                  month   = lubridate::ceiling_date(date, 'month') + lubridate::days(-1),
                  year    = lubridate::ceiling_date(date, 'year')  + lubridate::days(-1),
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
notefield <- function(x, f) {
    fname <- deparse(substitute(f))
    x %<>% subset(., grepl(paste0(' ', fname, ':'), .$note))
    x[[fname]] <- sub(paste0('.* ',fname,':(.*?) .*'), '\\1', x$note)
    x
}



#' general aggregation function, should be applied directly to raw data
#' @param a transactions object
#' @param byfields grouping variables for aggregation
sumt <- function(a, byfields = c('account', 'units')) {
    q_has_cost <- !is.null(a$cost)
    if (!q_has_cost) a$cost <- 0.0
    #print(paste('summing and q_has_cost = ', q_has_cost, '\n'))
    (a %<>% identity
        %>% dplyr::arrange(., .$date)
        ## first, aggregate respecting units
        %>% plyr::ddply(., union('units', byfields), function(x) {
            with(x, {data.frame(amount = sum(x$amount),
                               val = sum(x$val),
                               lprice = dplyr::last(x$lprice),
                               Date = dplyr::last(x$date),
                               cost = sum(x$cost))})})
        %>% dplyr::mutate(., date = .$Date, Date = NULL)
        ## second, aggregate across units
        %>% subset(., round(.$amount, 4) != 0 |
                      round(.$val, 4) != 0 |
                      round(.$cost, 4) != 0)
        %>% plyr::ddply(., byfields, function(x) {
            with(x, {data.frame(val = sum(x$val),
                                Date = dplyr::last(x$date),
                                cost = sum(x$cost))})})
        ## compute cumulative quantities
        %>% dplyr::arrange_(., c(byfields, 'val', intersect('acctype', names(a))))
        %>% dplyr::mutate(.,
                          date = .$Date,
                          Date = NULL,
                          cval = cumsum(.$val),
                          ccost = cumsum(.$cost),
                          cret = cumsum(.$val-.$cost)/cumsum(.$cost))
    )

    ## if both time and non-time fields are being kept,
    ##   cumulate non-time fields distinctly as 'cbyval'
    tlike <- byfields %in% c('year','month','date')
    if (sum(tlike) > 0 && sum(tlike) != length(byfields)) {
        a %<>% plyr::ddply(., setdiff(byfields, c('year','month','date')),
                           function(x) x %>% dplyr::mutate(., cbyval = cumsum(.$val),
                                                           cbycost = cumsum(.$cost),
                                                           cbyret = cumsum(.$val-.$cost)/cumsum(.$cost)
                                                           )
                           )
    }

    a <- .dig(a, 2)

    if (!q_has_cost) {
        a %<>% dplyr::mutate(cost = NULL,
                             ccost = NULL,
                             cret = NULL,
                             cbycost = NULL,
                             cbyret = NULL)
    }

  if ('month' %in% byfields) a$date <- a$month
  if ('year' %in% byfields)  a$date <- a$year
  dplyr::mutate(a, month = NULL, year = NULL)
}


##########################################################################################
##########################################################################################
##########################################################################################

#' special dates
#' @export
today  <- Sys.Date()

#' special accounts for selection
iacc   <- 'Earnings|Invest|Income|Interest'
oacc   <- 'Earnings|Invest|Income|Interest|Tax|Trips|Expenses|Equity'
tacc   <- 'Tax|Trips|Expenses'
############
## SELECTION

#' select transactions on or before date e
#' @param aca transactions object
#' @param e date as a string
#' @export
until  <- function(aca, e = today)
  subset(aca, date <= as.Date(e))

#' select transactions on or after date b
#' @param aca transactions object
#' @param b date as a string or Date object, defaults to beginning of current year
#' @export
since  <- function(aca, b = lubridate::floor_date(today-10, 'year'))
  subset(aca, date >= as.Date(b))

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
note <- function(aca, a)
    aca %>% subset(., grepl(a, .$note, ignore.case = TRUE))

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
#' @export
std   <- function(intrans = pycsv(), inprx = pyprx())
    valattach(intrans, inprx)

#' see std entries : select all but 'bad' accounts
#' @export
cs <- function()
  std() %>% xfields %>% until

#' see tariff entries :  (Tax, Trips, Expenses)
#' @export
ct <- function()
  cs() %>% acc(tacc)

#' see both cash and investment entries : not 'bad' accounts and not interesting accounts
#' @export
cb <- function()
  cs() %>% notacc(oacc)

#' see cash entries : all dollar unit positions
#' @export
cc <- function()
  cs() %>% notacc(oacc) %>% tunits('\\$')

#' see investment entries : (all but dollar unit position)
#' @export
ci <- function()
    cs() %>% notacc(oacc) %>% notunits('\\$')

#' see adjustment entries : absolutely bigger than threshold
#' @param threshold only adjustments absolutely bigger than threshold
#' @export
ca <- function(threshold = 1000, X = cs())
    X %>% subset(grepl('^balance', .$note) & abs(.$amount) > threshold)

#' see xpense entries : absolutely bigger than threshold (default:1000)
#' @param threshold only expense adjustments bigger than threshold
#' @export
cax <- function(...)
    ca(...) %>% subset(grepl('Expenses', .$note) & .$account != 'Expenses')

#' see fixed deposit instruments : absolutely bigger than threshold (default:1000)
#' @export
cf <- function() cs() %>% subset(grepl('type:(cd|ee|frn|tips|ibond|tbill|corp)', .$note))

#' see fixed deposit instruments still locked up: absolutely bigger than threshold (default:1000)
#' @export
cl <- function() cf() %>% dplyr::mutate(edate = sub('.*matdate:(.*?) .*', '\\1', .$note)) %>% subset(as.Date(.$edate) > today & date <= today & grepl('start ', .$note) & .$amount > 0)

bfields <- function(iam) {
    imlets <- sort(unlist(strsplit(iam, '')))
    byfields <- c()
    if ('u' %in% imlets) byfields <- c('units',   byfields)
    if ('a' %in% imlets) byfields <- c('account', byfields)
    if ('p' %in% imlets) byfields <- c('place',   byfields)
    if ('n' %in% imlets) byfields <- c('note',    byfields)
    if ('t' %in% imlets) byfields <- c('acctype',    byfields)
    if ('d' %in% imlets) byfields <- c('date',    byfields)
    if ('m' %in% imlets) byfields <- c('month',   byfields)
    if ('y' %in% imlets) byfields <- c('year',    byfields)
    byfields
}

#' std utility to see various views of accounts
#' @param byf encoding of by fields
#' @param X transactions object
#' @export
cby <- function(byf, X = cb()) {
    byf <- deparse(substitute(byf))
    byf <- bfields(byf)
    X %<>% sumt(byf)
    attr(X, 'byfields') <- byf
    X
}

#' smart plot of extracted data, pie charts, multi-panel plots
#' @import ggplot2
#' @param ...  arguments are passed to cby
#' @export
pby <- function(...) {
  X <- cby(...)
  byf <- attr(X, 'byfields')
  timetypes <- c('date','month','year')
  if (!any(timetypes %in% byf)) {
    gvar <- setdiff(names(X), c('date','val', 'cval','cost','ccost','cret'))
    graphics::pie(X$val, interaction(subset(X, , gvar)))
  } else {
    if (all(byf %in% timetypes)) {
      (ggplot(X, aes(x = date, y = cval))
       + geom_point() + geom_line())
    } else {
      (ggplot(X, aes(x = date, y = cbyval))
       + geom_point() + geom_line()
       + facet_wrap(as.formula(paste('~ ', paste(collapse = '+', setdiff(byf, timetypes))))))
    }
  }
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
    ## cb()  # brokerage entries
    ## cc()  # cash entries
    ## ca()  # balance adjustments
    ## cax()  # xpense adjustments
    ## cf()  # fixed instrument entries

  ## cby(au) ...

  ## cb() to see just investments
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
