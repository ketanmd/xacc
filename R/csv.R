
# > cby(adu,std()) %>% subset(caval < 0) -> nst
# > subset(nst, !grepl('Cash|Interest|Earnings|Expenses|Income|Tax|Equity', account))
# # A tibble: 4 x 8
#    units       account    amount       val lprice       date       cval      caval
#   <fctr>        <fctr>     <dbl>     <dbl>  <dbl>     <date>      <dbl>      <dbl>
# 1      $         TDW:B   -1000.0   -1000.0      1 1990-12-01    1000.00   -1000.00
# 2      $         TDW:K   -1000.0   -1000.0      1 1990-12-01       0.00   -1000.00
# 3      $ Retire:TIAA:K -422069.2 -422069.2      1 2013-05-30  -31550.86 -394547.37
# 4      $  Retire:JPM:K -471085.7 -471085.7      1 2013-06-30 -504675.07  -49016.45
# these were all taken care of for now.

swa <- function(x) {
  x %>% dplyr::mutate(., foo = .$bar/2)
}

read.bad <- function(csvf) {
  cdata <- utils::read.csv(csvf, stringsAsFactors = FALSE)
  cdata$from <- paste0(cdata$from, basename(csvf))
  list(cdata)
}

read.fredzip <- function(csvf) {
  zfiles <- utils::unzip(csvf, list = TRUE)
  zans <- list()
  for (iz in zfiles$Name[-1]) { # first file is a readme file
    unz(csvf, iz) %>% utils::read.delim(stringsAsFactors = FALSE) -> izx
    for (nizx in names(izx)[-1]) {
      zdata <-
        data.frame(stringsAsFactors = FALSE,
                   Date       = as.Date(izx[, 1]),
                   Name       = nizx,
                   Symbol     = nizx,
                   Last.Price = izx[, nizx]
                   )
      zdata <- subset(zdata, Last.Price != '.' & !is.na(Last.Price))
      zdata$Last.Price <- as.numeric(zdata$Last.Price)
      zans[[length(zans)+1]] <- zdata
    }
  }
  cdata <- plyr::rbind.fill(zans)
  fidmapfile <- known('fredzip.seriesids')
  cdata %<>% subset(., .$Date > as.Date('2000-01-01') & !is.na(Last.Price))
  cdata <- within(cdata, {
    if (file.exists(fidmapfile))
      Symbol <- .aamf(Symbol, fidmapfile, Symbol, 'Title', 'myid')
    Symbol = paste0('q_', Symbol)
    from   = basename(csvf)
    Account   = 'unk'
    Value      = NaN
    Position   = NaN
    Name       = Symbol
    Last.Price = ifelse(Symbol == 'q_INR', 1/Last.Price, Last.Price)
  })
  list(cdata)
}

read.fred <- function(csvf) {
  cdata <- utils::read.csv(csvf, stringsAsFactors = FALSE)
  cdata <- within(cdata, {
    Symbol = paste0('q_', Symbol)
    Name   = Symbol
    from   = basename(csvf)
    Account   = 'unk'
    Value      = NaN
    Position   = NaN
  })
  list(cdata)
}
read.yahoo <- function(csvf) {
  filedate <- .filedate(csvf)
  cdata <- utils::read.csv(csvf, stringsAsFactors = FALSE)
  #str(list(cdata,filedate))
  if (filedate > as.Date('2017/11/20'))
      return(list(within(subset(cdata, Symbol != ''), {
          Symbol = paste0('q_', Symbol)
          Name   = Symbol
          Last.Price = Current.Price
          from   = basename(csvf)
          Account   = 'unk'
          Value      = NaN
          Position   = NaN
      })))
  cdata <- within(subset(cdata, Symbol != ''), {
    Symbol = paste0('q_', Symbol)
    Name   = Symbol
    from   = basename(csvf)
    Account   = 'unk'
    Value      = NaN
    Position   = NaN
  })
  list(cdata)
}
read.google <- function(csvf) {
  filedate <- .filedate(csvf)
    cdata <- utils::read.csv(csvf, stringsAsFactors = FALSE)
    if (!"Last.price" %in% names(cdata)) {
        warning("No last price found in google csv file ", csvf, " - skipping this file")
        return(NULL)
    }
    cdata %<>%
      subset(., .$Symbol != '') %>%
      transform(.,
                Date = filedate,
                Name = sub('Wellesley[^ ]+', 'Wellesley', .$Name),
                Last.Price = .$Last.price,
                Symbol = paste0('q_', .$Symbol),
                from = basename(csvf),
                Account = 'unk',
                Value = NaN,
                Position = NaN
                )
    list(cdata)
}

read.lt <- function(csvf) { # something broken here ?
  famount <- function(Action, Amount) {
    Amount <- .tonumeric(Amount)
    Amount <- ifelse(Action == 'Sell', -Amount, Amount)
    Amount[Action == 'REINVDIV'] <- '$ 0'
    Amount
  }
  ffromaccount <- function(Transaction.Type, acmapfile) {
    FromAccount <- rep('LTacct', length(Transaction.Type))
    FromAccount[grepl('Employer Match ', Transaction.Type)] <-  'LTmatch'
    FromAccount[grepl('Roth Deferrals ', Transaction.Type)] <-    'LTroth'
    FromAccount[grepl('Profit Sharing ', Transaction.Type)] <-    'LTsharing'
    if (file.exists(acmapfile))
      FromAccount <- .aamf(FromAccount, acmapfile, FromAccount, 'AccNum' , 'AccName')
    FromAccount
  }
  faccount <- function(Transaction.Type, acmapfile) {
    Account <- rep('LTacct', length(Transaction.Type))
    if (file.exists(acmapfile))
      Account <- .aamf(Account, acmapfile, Account, 'AccNum', 'AccName')
    Account
  }
  fsecurity <- function(Security) {
    Security[Security == 'RFLP'] = 'med'
    Security[Security == 'RIEFLLC'] = 'rief'
    Security
  }
  faction <- function(Action, Transaction.Type) {
    Action[grepl('Gain/Loss', Transaction.Type)] = 'REINVDIV'
    Action
  }
  cdata <- utils::read.csv(csvf, stringsAsFactors = FALSE)
  acmapfile = known('lt.accountids')
  cdata %<>% dplyr::mutate(.,
    Quantity = .tonumeric(.$Quantity),
    Price    = .tonumeric(.$Price),
    Action   = faction(.$Action, .$Transaction.Type),
    Amount   = famount(Action, .$Amount),
    Security = fsecurity(.$Security),
    Account  = faccount(.$Transaction.Type, acmapfile),
    FromAccount = ffromaccount(.$Transaction.Type, acmapfile),
    Date = .cleandates(.$Date),
    from = basename(csvf)
  )

#  str(list('new read.lt', cdata))
  list(cdata)
}
read.csvf <- function(csvf) {
  cdata <- utils::read.csv(csvf, stringsAsFactors = FALSE)
  cdata <- within(cdata, {
    from <- paste0(from, basename(csvf))
  })
  list(cdata)
}
read.jpm <- function(csvf) {
  filedate <- .filedate(csvf)
  cdata <- utils::read.csv(csvf, stringsAsFactors = FALSE, fileEncoding = 'latin1')
  cdata <- within(cdata, {
    Price    <- Price.USD
    Amount   <- .tonumeric(Amount.USD)
    Quantity <- .tonumeric(Quantity)
    Account  <- Account.Number

    acmapfile    <- known('jpm.accountids')
    if (file.exists(acmapfile))
      Account <-
      .aamf(Account, acmapfile, Account, 'AccNum', 'AccName')
    FromAccount <- Account

    Amount   <- ifelse(Type == 'Purchase', -Amount, Amount)

    Date     <-
      .cleandates(ifelse(
        Trade.Date == '' | is.na(Trade.Date) | Trade.Date == ' ',
        Post.Date,
        Trade.Date
      ))
    Type[Type == 'Purchase'] <- 'Buy'
    Type[Type == 'Sale'] <- 'Sell'

    cusipmapfile <- known('jpm.cusips')
    if (file.exists(cusipmapfile))
      Ticker <-
      .aamf(Cusip, cusipmapfile, Ticker, 'Cusip', 'Ticker')
    Action <- Type
    Security <-
      ifelse(Ticker == '' | is.na(Ticker), 'cash', Ticker)

    divaction <- Action %in%
      c(
        'Interest',
        'Dividend',
        'Transfers',
        'Free Delivery',
        'S. T. Capital Gain',
        'L.T. Capital Gain'
      )
    extaction <- Action %in%
      c(
        'Contributions',
        'Conversions',
        'Distributions',
        'Misc. Disbursement',
        'Misc. Receipt',
        'Misc Debit / Credit',
        'Misc. Debit / Credit'
      )

    Security[extaction] <- '$'
    Quantity[extaction] <- 0
    Amount[extaction] <- 0

    Security[divaction] <- ''
    Quantity[divaction] <- Amount[divaction]
    Amount[divaction] <- '$ 0'

    Quantity <- ifelse(Action == 'Name Change', 0, Quantity)
    Quantity <- ifelse(Action == 'Transfers', 0, Quantity)

    from <- basename(csvf)
  })
  cdata <- subset(cdata, as.Date(Date) <= as.Date(filedate))
  list(cdata)
}

#' process a csv file downloaded from hsbc
#' @param csvf csv file from hsbc to process
read.hsbc <- function(csvf) {
  filedate <- .filedate(csvf)
  cdata <- utils::read.csv(csvf, stringsAsFactors = FALSE, skip = 2)
  cdata <- within(cdata, {
    Net.Amount <- .tonumeric(Net.Amount)
    Quantity   <- .tonumeric(Quantity)
    Account    <- Account..

    acmapfile    <- known('hsbc.accountids')
    if (file.exists(acmapfile))
      Account <-
      .aamf(Account, acmapfile, Account, 'AccNum', 'AccName')
    FromAccount <- Account

    Date     <-
      .cleandates(ifelse(
        Trade.Date == '' | Trade.Date == ' '| is.na(Trade.Date),
        Date,
        Trade.Date
      ))

    Ticker <- CUSIP <- as.character(CUSIP)
    Action <- Description
    Type   <- as.character(Action)

    cusipmapfile <- known('hsbc.cusips')
    if (file.exists(cusipmapfile))
      Ticker <-
      .aamf(CUSIP, cusipmapfile, CUSIP, 'CUSIP', 'Ticker')
    Security <-
      ifelse(Ticker == '' | is.na(Ticker), 'cash', Ticker)

    divaction <- grep(paste0(collapse = "|",
      c('BOND INTEREST RECEIVED',
        'INCOME RECEIVED'
      )), Action)
    bankxaction <- grep(paste0(collapse = '|',
      c(#'MONEY FUND PURCHASE',
        #'MONEY FUND REDEMPTION',
        #'OUTGOING ACCOUNT TRANSFER FEE',
        'BANK SETTLEMENT ACTIVITY'
        )), Action)

    dolaction <- setdiff(grep("USD999997", CUSIP), bankxaction)
    xaction   <- setdiff(grep('YOUR ASSET TRANSFERRED', Type), dolaction)

    ## by construction, divaction, xaction and dolaction do not overlap

    Price <- Price/100
    Amount <- abs(Net.Amount)

    Quantity <- .ifset(Quantity, divaction, Net.Amount[divaction])
    Security <- .ifset(Security, divaction, '$')
    Price    <- .ifset(Price,    divaction, NA)
    Amount   <- .ifset(Amount,   divaction, 0)

    Quantity <- .ifset(Quantity, dolaction, Net.Amount[dolaction])
    Security <- .ifset(Security, dolaction, '$')
    Price    <- .ifset(Price,    dolaction, NA)
    Amount   <- .ifset(Amount,   dolaction, 0)

    Quantity <- .ifset(Quantity, bankxaction, Net.Amount[bankxaction])
    Security <- .ifset(Security, bankxaction, '$')
    Price    <- .ifset(Price,    bankxaction, NA) ##
    Amount   <- .ifset(Amount,   bankxaction, abs(Net.Amount[bankxaction]))

    Quantity <- .ifset(Quantity, xaction, -Quantity[xaction])
    Price    <- .ifset(Price,    xaction, NA)

##    Price <- .ifset(Price, extaction, 1.0)

    bankxaction <- NULL
    divaction <- NULL
    xaction <- NULL
    dolaction <- NULL
    from <- basename(csvf)
  })
  cdata <- subset(cdata, as.Date(Date) <= as.Date(filedate))
  list(cdata)
}
read.hsbcpos <- function(csvf) {
    lcdata <- read.hsbc(csvf)
    if (is.null(lcdata)) return(lcdata)
    lcdata[[1]] <- within(lcdata[[1]], {
        FromAccount = NULL
        Symbol = paste0('q_', Security)
        Position = NA
        Last.Price = Price
        Value = NA
        Name = Security
    })
    return(lcdata)
}
read.vanpos <- function(csvf) {
  clines <- readLines(csvf)
  clines <- clines[clines != '']
  begs <- grep('Account', substring(clines, 1, 7))
  ends <- c(begs[-1] - 1, length(clines))
  alld <- list()
  for (ii in 1:length(begs)) {
    cdata = utils::read.csv(textConnection(clines[begs[ii]:ends[ii]]))
    if (!('Total.Value' %in% names(cdata))) next
    acmapfile   <- known('van.accountids')
    cdata %<>% dplyr::mutate(Position = .$Shares,
                             Value    = .$Total.Value,
                             Last.Price = .$Share.Price,
                             Date = .cleandates(sub('\\..*', '', basename(csvf))),
                             Account = .$Account.Number,
                             Name = .$Investment.Name,
                             Symbol = as.character(.$Symbol))
    cdata <- within(cdata, {
#      Position    <- Shares
#      Value       <- Total.Value
#      Last.Price  <- Share.Price
#      Date        <- .cleandates(sub('\\..*', '', basename(csvf)))
#      Account     <- Account.Number
      if (file.exists(acmapfile))
        Account <- .aamf(Account, acmapfile, Account, 'AccNum', 'AccName')
#      Name <- Investment.Name

#      Symbol <- as.character(Symbol)
      inmapfile <- known('van.investment.names')
      if (file.exists(inmapfile)) {
          Symbol <- .fixknown(Symbol, Investment.Name, inmapfile)
          ## why is this conditional ?
          Last.Price[grepl('B_(C|M|T)', Symbol)] <- Last.Price[grepl('B_(C|M|T)', Symbol)]/100
      }
      inmapfile <- NULL
      from        <- basename(csvf)
    })
    alld[[length(alld) + 1]] <- cdata
    cdata[['Symbol']] <- paste0('q_', cdata[['Symbol']])
    alld[[length(alld) + 1]] <- cdata
  }
  return(alld)
}

.fixknown <- function(sym, iname, inmapfile, symdefault = '') {
    readLines(inmapfile) -> inmaptable
    inmapid    <- sub(' .*', '', inmaptable)
    inmaptable <- sub('.*? ', '', inmaptable)
    ## identify entries with sym == '' and for which we already
    ## recognize the Investment.Name
    badsym <- sym == ''
    goodn  <- match(iname[badsym],  inmaptable)

    fixable <- (!is.na(goodn)) & goodn != 0

#    badb <- data.frame(Symbol = sym, IName = iname, isin = iname %in% inmaptable)
#    saveRDS(badb, '/tmp/foo')

    newnames <- setdiff(unique(setdiff(iname[badsym], inmaptable)), '')


    if (length(newnames) > 0) {
      message("The following names are unknown:", newnames)
      outmapfile <- paste0(inmapfile, '.newnames')
      message('Writing new names to ', outmapfile)
      if (file.exists(outmapfile)) {
        oldnewnames <- readLines(outmapfile)
        newnames <- unique(sort(union(oldnewnames, newnames)))
      }
      writeLines(newnames, sep = '\n', con = outmapfile)
    }
    ## fix what we can and finish up
  #  str(list(sym,badsym,goodn,fixable,iname,inmapid,inmaptable))
    sym[badsym][fixable] <- inmapid[goodn[fixable]]

#    print(list(goodn,fixable,inmapid[goodn[fixable]], sym[badsym]))
    sym[sym == ''] <- symdefault
    return(sym)
}

read.van <- function(csvf) {
  clines <- readLines(csvf)
  clines <- clines[clines != '']
  begs <- grep('Account', substring(clines, 1, 7))
  ends <- c(begs[-1] - 1, length(clines))
  alld <- list()
  for (ii in 1:length(begs)) {
    cdata = utils::read.csv(textConnection(clines[begs[ii]:ends[ii]]))
    if ('Total.Value' %in% names(cdata)) next
    cdata <- cdata[nrow(cdata):1,]

#    print(as.data.frame(cdata[as.character(cdata$Symbol) == ''
#                             & as.character(cdata$Investment.Name) == 'VANGUARD FEDERAL MONEY MARKET FUND',c(2,4,5,6,7,8,9,10)]))

    acmapfile <- known('van.accountids')
    faccount <- function(Account, acmapfile) {
        if (file.exists(acmapfile))
            Account <- .aamf(Account, acmapfile, Account, 'AccNum', 'AccName')
        Account
    }
    inmapfile <- known('van.investment.names')
    cdata %<>% dplyr::mutate(
                          Account     = faccount(.$Account.Number, acmapfile),
                          FromAccount = faccount(.$Account.Number, acmapfile),
                          Action      = as.character(.$Transaction.Type),
                          Date        = .cleandates(.$Trade.Date),
                          Amount      = abs(.$Net.Amount),
                          Quantity    = .$Shares,
                          Price       = .$Share.Price,
                          Security    = as.character(.$Symbol)
                          )
    if (file.exists(inmapfile))
        cdata %<>% dplyr::mutate(Security = .fixknown(.$Security, .$Investment.Name, inmapfile, 'cash'))

#family@perfect:~/money/accounting$ cat */*.van*.csv | sort  | uniq | egrep -v "Account Number|,Contribution|,Dividend|,Sweep (in|out)|,Reinvestment \((LT|ST) gain\)|,Transfer \((Outgoing|incoming)\)|,Capital gain \((LT|ST)\)|,Interest|,Corp Action|,Funds Received|,(Buy|Sell),|,(Buy|Sell) \(exchange\),|,Share Conversion \((incoming|outgoing)\),|,Conversion \((outgoing|incoming)\)|,Transfer \(outgoing\)|,Rollover" | grep "/201"

#      "Funds Received"              #   ==  >      [ Shares =  Shares     , Net.Amount =  Net.Amount ]
#      "Transfer (Outgoing)"         #   ==  <      [ Shares =  Shares     , Net.Amount = -Net.Amount ]
#      "Transfer (outgoing)"         #   ==  <      [ Shares =  Shares     , Net.Amount = -Net.Amount ]
#      "Transfer (incoming)"         #   >   ==     [ Shares =  Shares     , Net.Amount = $ 0 ]

#      "Share Conversion (outgoing)" #   <   <      [ Shares =  Shares     , Net.Amount = -Net.Amount ]
#      "Share Conversion (incoming)" #   >   >      [ Shares =  Shares     , Net.Amount =  Net.Amount ]
#      "Conversion (outgoing)"       #   <   <      [ Shares =  Shares     , Net.Amount = -Net.Amount ]
#      "Conversion (incoming)"       #   >   >      [ Shares =  Shares     , Net.Amount =  Net.Amount ]

#      "Sell"                        #   <   >      [ Shares =  Shares     , Net.Amount =  Net.Amount ]
#      "Sell (exchange)"             #   <   >      [ Shares =  Shares     , Net.Amount =  Net.Amount ]
#      "Buy"                         #   >   <      [ Shares =  Shares     , Net.Amount = -Net.Amount ]
#      "Buy (exchange)"              #   >   <      [ Shares =  Shares     , Net.Amount = -Net.Amount ]

#      "Reinvestment (LT gain)"      #   >   <      [ Shares =  Shares     , Net.Amount = -Net.Amount ]
#      "Reinvestment (ST gain)"      #   >   <      [ Shares =  Shares     , Net.Amount = -Net.Amount ]
#      "Reinvestment"                #   >   <      [ Shares =  Shares     , Net.Amount = -Net.Amount ]
#      "Reinvestment"                #   ==  <      [ Shares = -Net.Amount , Net.Amount = -Net.Amount ] # units:VANMMF

#      "Sweep out"                   #   ==  >      [ Shares =  Net.Amount , Net.Amount =  Net.Amount ]
#      "Sweep in"                    #   ==  <      [ Shares = -Net.Amount , Net.Amount = -Net.Amount ]

#      "Interest"                    #   ==  >      [ Shares =  Net.Amount , Net.Amount = $ 0 ]
#      "Dividend"                    #   ==  >      [ Shares =  Net.Amount , Net.Amount = $ 0 ]
#      "Capital gain (LT)"           #   ==  >      [ Shares =  Net.Amount , Net.Amount = $ 0 ]
#      "Capital gain (ST)"           #   ==  >      [ Shares =  Net.Amount , Net.Amount = $ 0 ]

#      "Buy Cancel"                  #   >   <      [ Shares = -Shares     , Net.Amount = -Net.Amount ]
#      "Corp Action (Redemption)"    #   >   >      [ Shares = -Shares     , Net.Amount =  Net.Amount ]

    a1 <- c('Transfer (outgoing)',
            'Transfer (Outgoing)',
            'Transfer (incoming)')
    cdata[cdata$Action %in% a1,] %<>% dplyr::mutate(Amount = abs(.$Net.Amount))
    cdata[cdata$Action %in% a1 & cdata$Security == 'cash',] %<>% dplyr::mutate(Quantity = .$Net.Amount,
                                                                               Security = '$')

    cdata[cdata$Action %in% c('Sweep in', 'Sweep out',
                              'Conversion (incoming)',
                              'Conversion (outgoing)'),] %<>% dplyr::mutate(Amount = abs(.$Net.Amount),
                                                                            Quantity = -.$Net.Amount,
                                                                            Security = '$')
    
    a3 <- c('Reinvestment (LT gain)',
            'Reinvestment (ST gain)',
            'Reinvestment',
            'Contribution',
            'Share Conversion (outgoing)',
            'Share Conversion (incoming)')
    cdata[cdata$Action %in% a3,] %<>% dplyr::mutate(Amount = abs(.$Net.Amount))
    cdata[cdata$Action %in% a3 & cdata$Quantity == 0,] %<>% dplyr::mutate(Quantity = -.$Net.Amount,
                                                                          Security = '$')
    
    cdata[cdata$Action %in% c('Dividend'),] %<>% dplyr::mutate(Amount = '$ 0',
                                                               Quantity = abs(.$Net.Amount),
                                                               FromAccount = 'Dividends',
                                                               Security = '$')
    
    cdata[cdata$Action %in% c('Buy', 'Buy (exchange)', 'Sell', 'Sell (exchange)'),] %<>% 
        dplyr::mutate(Amount = abs(.$Net.Amount),
                      Quantity = .$Shares,
                      Price = .$Share.Price)
    
    cdata[cdata$Action %in% c('Interest', 'Capital gain (LT)', 'Capital gain (ST)'),] %<>%
        dplyr::mutate(Amount = '$ 0',
                      Quantity = abs(.$Net.Amount),
                      FromAccount = 'Dividends',
                      Security = '$')
    
    cdata[cdata$Action %in% c('Buy Cancel', 'Corp Action (Redemption)'),] %<>%
        dplyr::mutate(Amount = abs(.$Net.Amount),
                      Quantity = -.$Shares,
                      Price = .$Share.Price)
    
    cdata[cdata$Action %in% c('Funds Received'),] %<>% dplyr::mutate(Amount = abs(.$Net.Amount),
                                                                     Quantity = .$Net.Amount,
                                                                     Security = '$') %>%
        dplyr::mutate(Amount = 0,
                      Quantity = 0,
                      Price = 0)
    
    cdata[cdata$Action %in% c('Rollover (incoming)'),] %<>% dplyr::mutate(Amount = '$ 0',
                                                                          Quantity = abs(.$Net.Amount),
                                                                          Price = 0)
    
    cdata %<>% dplyr::mutate(from = basename(csvf))
    
    alld[[length(alld) + 1]] <- cdata
  }
  return(alld)
}

.filedate <- function(fname) {
    .cleandates(sub('\\..*', '', basename(fname)))
}
#' process csv file(s) for transactions
#' @param csvfiles character vector of csv files to process
#'
#' @export
transprocess <- function(csvfiles) {
  alld <- list()
  for (csvf in csvfiles) {
    message("Working on trans from file ", csvf, "\n")
    bcsvf <- basename(csvf)
    cdata <- NULL
    if (grepl('bad', bcsvf))  cdata <- read.bad(csvf)
    if (grepl('lt', bcsvf))   cdata <- read.lt(csvf)
    if (grepl('jpm', bcsvf))  cdata <- read.jpm(csvf)
    if (grepl('hsbc', bcsvf)) cdata <- read.hsbc(csvf)
    if (grepl('van', bcsvf))  cdata <- read.van(csvf)
    if (is.null(cdata)) next()
    for (ialld in 1:length(cdata)) {
      alld[[length(alld)+1]] <-
        cdata[[ialld]][, c('Date',
                           'Account',
                           'FromAccount',
                           'Action',
                           'Security',
                           'Quantity',
                           'Price',
                           'Amount',
                           'from')]
    }
  }
  return(alld)
}
#' process csv file(s) for positions and prices
#' @param csvfiles character vector of csv files to process
#'
#' @export
posprocess <- function(csvfiles) {
  alld <- list()
  for (csvf in csvfiles) {
    message("Working on pos/prices from file ", csvf, "\n")
    bcsvf <- basename(csvf)
    cdata <- NULL
    if (grepl('yahoo', bcsvf))   cdata <- read.yahoo(csvf)
    if (grepl('fredzip', bcsvf)) cdata <- read.fredzip(csvf)
    if (grepl('fred', bcsvf)) cdata <- read.fred(csvf)
    if (grepl('google', bcsvf))  cdata <- read.google(csvf)
    if (grepl('van', bcsvf))     cdata <- read.vanpos(csvf)
    if (grepl('hsbc', bcsvf))     cdata <- read.hsbcpos(csvf)
    if (is.null(cdata)) next()
    for (ialld in 1:length(cdata)) {
    #  str(list(bcsvf,cdata[[ialld]]))
      cdata[[ialld]][, c('Date',
                                  'Account',
                                  'Symbol',
                                  'Position',
                                  'Last.Price',
                                  'Value',
                                  'Name',
                                  'from')] %>%
        subset(., !is.na(.$Last.Price) & .$Last.Price != 0) ->
        alld[[length(alld)+1]]
#      alld[[length(alld)+1]] <-
#        subset(cdata[[ialld]][, c('Date',
#                           'Account',
#                           'Symbol',
#                           'Position',
#                           'Last.Price',
#                           'Value',
#                           'Name',
#                           'from')], !is.na(Last.Price) & Last.Price != 0)
    }
  }
  return(alld)
}
#######################################################################################
#######################################################################################
#' process inputs from csv files
#'   if badcsvfile != '', write malformed lines into badcsvfile
#'
#' @param csvfiles character vector of csv files to process
#' @param badcsvfile name of output csvfile to write malformed lines into
csvprocess <- function(csvfiles, badcsvfile = '') {
    options(scipen = 999)

  extracttrans <- function(lines, outfile) {
    lines %>%
      plyr::ldply(function(x)
      {
        if ('Amount' %in% names(x))
          return(x)
        return(NULL)
      }) ->
      trans
    if (!ncol(trans) || !nrow(trans))
      return(NULL)
    trans[do.call(order, trans),] -> trans
    plyr::ddply(trans, names(trans)[-9],
                function(x)
                  x[x$from == x$from[nrow(x)], , drop = FALSE]) %>%
      #identity %T>% str %>%
      subset(., !grepl('bad', .$from)) %>%
      subset(., !(.$Quantity == 0 & .$Price == 0 &
                    (.$Amount == 0 | .$Amount == '$ 0'))) -> a
    if (!is.null(outfile))
      utils::write.table(a, outfile, quote = FALSE,sep = ' : ', row.names = FALSE)
    return(a)
  }

  extractposprices <- function(X, outfile = NULL) {
    plyr::ldply(X,
                function(x) {
                  if ('Position' %in% names(x) ||
                      'Last.Price' %in% names(x))
                    return(x)
                  return(NULL)
                }) -> a
    if (!is.null(outfile))
      utils::write.table(a, outfile, sep = ' : ', quote = FALSE, row.names = FALSE)
    return(a)
  }

  transtoblocks <- function(trans, outfile = NULL) {
    ltrans <- c('')
    if (!is.null(trans) && !is.null(trans[['Account']]))
      for (r in 1:nrow(trans)) {
        x <- as.vector(trans[r,])
        ltrans[r] <- with(x, {
          Security <- as.character(Security)
          Security <- ifelse(Security == 'cash', '', Security)
          paste0(
            Date,
            ' * ',
            Action,
            ' from ',
            from,
            '\n',
            '         ',
            Account,
            '  ',
            Quantity,
            ' ',
            Security,
            ' { ',
            .cleanna(Price, 0.0),
            ifelse(Quantity == 0, ' } (@@) ', ' } @@ '),
            Amount,
            '\n',
            '         ',
            FromAccount,
            '\n\n'
          )
        })
      }
    if (!is.null(outfile))
      saveRDS(ltrans, outfile, ascii = TRUE)
    ltrans
  }
  pospricestoblocks <- function(trans, outfile) {
    ltrans <- c()
    if (is.null(trans))
      return(c(''))
    # if (is.null(trans[['Account']]))
    #    return(c(''))
    message('processing posprices with names ',nrow(trans))
    for (r in 1:nrow(trans)) {
      x <- as.vector(trans[r,])
      #str(x)
      ltrans[r] <- with(x, {
        Security <- as.character(Symbol)
        Price <- Last.Price
        #                Security <- as.character(Security)
        Security <- ifelse(Security == 'cash', '', Security)
        Account <- 'unk for now'
        subacc <- sub('.*?:(.*)', '\\1', Account)
        paste0('P ', Date, ' ', as.character(Security), ' ', Price,
               #                       '\n\n',
               #                       Date, ' bal:', subacc, '   == ', Position, ' ', as.character(Security),
               '\n\n')
      })
    }
    if (!is.null(outfile))
      saveRDS(ltrans, outfile, ascii = TRUE)
    message('wrote trans blocks to ', outfile)
    ltrans
  }

  T <- function(x, f, ...) {
    x %>% f(., ...)
    x
  }

  f_posprices <- function(.)
    (extractposprices(., outhome('/posprices.debug'))
    %T>% pospricestoblocks(., outhome('/posprices.blocks'))
    )

  f_trans <- function(.)
    (extracttrans(., outhome('/trans.debug'))
     %T>% transtoblocks(., outhome('/trans.blocks'))
    )

  transprocess(csvfiles) %>% f_trans -> alltrans
  posprocess(csvfiles) %>% f_posprices -> invisible

  if (is.null(alltrans)) {
    message('Really, No transactions found.')
    return(c(''))
  }
  alltrans %>% subset(., is.na(.$Quantity)) -> alerts
  if (nrow(alerts)) {
    if (badcsvfile != '') {
      message('Accepting bad lines into ', badcsvfile, '\n')
      if (!dir.exists(dirname(badcsvfile))) dir.create(dirname(badcsvfile))
      utils::write.csv(alerts, badcsvfile)
      csvfiles <- c(csvfiles, badcsvfile)
    } else {
      warning(
        "========== RED ALERT =========\n",
        'The following lines are problematic:\n',
        print(paste0(alerts, collapse = '\n'))
      )
      stop('Cannot proceed, must accept alerts or fix problems\n',
           'Stopping now\n')
    }

    transprocess(csvfiles) %>% f_trans -> alltrans
    posprocess(csvfiles) %>% f_posprices -> invisible

    alltrans %>% subset(., .$Quantity == 0) -> alerts
    if (nrow(alerts)) {
      warning(
        "========== OCHOA ALERT =========\n",
        'The following lines are problematic:\n',
        paste0(alerts, collapse = '\n')
      )

      warning('csv lines with Quantity=0 still passing through - investigate manually\n')
      stop('======= OCTOMARINE ALERT ==========')
    }
  }

  c(readRDS(outhome('/trans.blocks')),
    readRDS(outhome('/posprices.blocks')))
}
