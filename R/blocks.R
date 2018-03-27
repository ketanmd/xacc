#' process files, extract blocks, expand input blocks as needed
#' returns blocks ready for ledger
#' @param ff character vector of files to process
#' @param verbose turn on messages (default = FALSE)
#' @importFrom magrittr '%>%'
#'
blocksprocess <- function(ff, verbose = FALSE) {
  knowf = .knowf()

  loadone <- function(g) {
    message('Reading lines into blocks from ', g, '\n')
    a <- readLines(g, warn = FALSE) %>% sub('\\\xae', '', .)
    a[grep('^(;|\\s*$|#||\\*)', a, invert = TRUE)] -> a1
    message('Found ', length(a1), ' lines from file ', g, '\n')
    if (!length(a1))
      return(list())
    a1b <- grep ('^ ', a1, invert = TRUE)
    a1e <- c(a1b[-1] - 1, length(a1))
    adf <- data.frame(b = a1b, e = a1e)
    plyr::alply(adf, .margins = 1, function(x)
      a1[x$b:x$e]) %>% unname
  }

  prepost <- function(ab) {
    sapply(ab, length) > 1 -> qblocks

    grepl('^P ', ab) & !qblocks  -> qpricelines
    (ab[qpricelines]
      %<>% sub('^P +([^ ]+) +([^ ]+) +([^ ]+)',
                            '\\1 type:price ticker:\\2 price:\\3',
                             .)
    )
    grepl(' move:', ab)     -> qmovelines
    ab[qmovelines & !qblocks] %<>% paste0(., ' type:move')

    grepl(' bal:', ab)      -> qbalancelines
    (ab[qbalancelines]
      %<>% sub('^([^ ]+) +bal:([^ ]+) +== *', '\\1 type:assert to:\\2 ', .)
      %>%  sub('^([^ ]+) +bal:([^ ]+) += *', '\\1 type:balance to:\\2 ', .)
    )

    procdate <- function(bs) {
      b1 <- bs[1]
      bd <- sub(' .*', '', b1)
      be <- sub('.+? ', '', b1)
      bs[1] <- paste0(.cleandates(bd), ' ', be)
      bs
    }

    c(ab[!qbalancelines],
      ab[qbalancelines]) %>%
      plyr::llply(procdate)
  }

  seqs <- function(sdate, edate, intervals) {
    intervals <- gsub('-', ' ', intervals)
    stringi::stri_split_regex(intervals, ',') %>% unlist -> intervals
    lapply(intervals,  seqdates, sdate = sdate, edate = edate) %>%
      #            identity -> a ; str(a) ; a %>%
      "[["(., 1) %>%
      #            identity -> a ; cat(a) ; a %>%
      pmax(sdate) %>% pmin(edate) %>%
      c(sdate, edate) %>%
      sort %>% unique
  }

  seqdates <- function(sdate, edate, interval) {
    datesub <- 0
    if (substr(interval, nchar(interval), nchar(interval)) == '<') {
      interval <- substr(interval, 1, nchar(interval) - 1)
      datesub <- -1
    }

    if (substr(interval, 1, 1) == '>') {
      interval <- substr(interval, 2, nchar(interval))
      sdate <- lubridate::ceiling_date(sdate, interval)
    }

    if (sdate > edate)
      return(c(sdate, edate) + datesub)

    if (interval == 'midmonth')
      return(seq.Date(
        lubridate::ceiling_date(sdate, 'month') + lubridate::days(14) - lubridate::month(1),
        edate,
        'month'
      ))
    #        str(list(sdate, edate, interval, datesub, seq.Date(sdate,edate,interval),
    #                 seq.Date(sdate,edate,interval)+datesub))
    return(seq.Date(sdate, edate, interval) + datesub)
  }

  postone <- function(b, verbose = FALSE) {
    if (!grepl('type:', b[1])) {
      if (length(b) > 1) {
        for (lb in 2:length(b)) {
          bacc <- sub('^ +([^ ]+).*', '\\1', b[lb])
          brest <- sub('^ +[^ ]+(.*)', '\\1', b[lb])
          newlb <- paste0('  ', knowf(bacc, 'fullacc'), brest)
          #str(list(b, bacc,brest,knowf(bacc, 'fullacc'), newlb))
          b[lb] <- newlb
        }
      }
      return(paste0(c(b, ''), collapse = '\n'))
    }

    kv <- .kvof(b[1])
    nobs <- kv[['note']]

    if (is.null(kv[['type']])) {
      utils::str(list(b))
      return(list(b))
    }

    if (verbose) utils::str(list(b))

    defs <- list(
      wages   = list(
        interval = 'midmonth,>month<',
        rate = 100.0,
        from = '',
        to = ''
      ),
      xfer    = list(
        interval = 'month',
        rate = 100.0,
        from = '',
        to = '',
        punits = ''
      ),
      loan    = list(
        interval = '>month<',
        post_from = '_to',
        post_to = '_from'
      ),
      cd      = list(
        interval = '>quarter<',
        post_from = 'Income:Interest:cd'
      ),
      tbill   = list(
        interval = '100-years',
        punits = 'tbill_@_1',
        post_from = '',
        post_to = ''
      ),
      frn     = list(
        interval = '>3-months<',
        punits = 'frn_@_1',
        post_from = 'Income:Interest:frn'
      ),
      tips    = list(
        interval = '6-months',
        punits = 'tips',
        post_from = 'Income:Interest:tips'
      ),
      ibond   = list(
        interval = '>6-months<',
        punits = 'ibond_@_1',
        post_from = 'Income:Interest:ibond'
      ),
      corp    = list(
        interval = '>6-months<',
        punits = 'corp_@_1',
        post_from = 'Income:Interest:corp',
        to = '_from'
      ),
      assert  = list(from = '_to'),
      balance = list(from = '_to'),
      default = list(
        interval = 'missing--interval',
        from = 'missing--from',
        post_from = 'missing--return_from',
        to = 'missing--to',
        #post_to = 'missing--post_to',
        principal = 'missing--principal',
        note = 'missing--note',
        startdate = 'missing--start_date',
        matdate = 'missing--mat_date',
        punits = '',
        iunits = '',
        rate = 0.0,
        compound = '_type',
        return_to = '_from',
        post_to = '_to',
        epunits = '_punits',
        id = '_type'
      )
    )

    "defset<-" <-
      function(x, value) {
        for (y in names(value))
          if (is.null(x[[y]]))
            x[[y]] <- value[[y]]
          x

      }

    defset(kv) <-  defs[[kv[['type']]]]
    defset(kv) <-  defs[['default']]
    for (k in c('from',
                'to',
                'post_from',
                'post_to',
                'return_to',
                'compound',
                'id',
                'epunits')) {
      if (!is.null(kv[[k]])) {
        if (substr(kv[[k]], 1, 1) == '_') {
          ik <- substr(kv[[k]], 2, nchar(kv[[k]]))
          kv[[k]] <- kv[[ik]]
        }
      }
    }

    if (kv[['type']] == 'balance') {
      if (grepl('^0.000', nobs))
        return()
      return(with(kv, {
        paste0(
          startdate,
          '  * balance  ',
          knowf(to, 'cparty'),
          '\n',
          '  ',
          knowf(to, 'cparty'),
          '\n',
          '  ',
          knowf(to, 'fullacc'),
          '   = ',
          nobs,
          '\n'
        )
      }))
    }

    if (kv[['type']] == 'assert') {
      return(with(kv, {
        paste0(
          startdate,
          '  * balance no change',
          '\n',
          '  ',
          knowf(to, 'cparty'),
          '\n',
          '  ',
          knowf(to, 'fullacc'),
          '   0 = ',
          nobs,
          '\n'
        )
      }))
    }
    if (kv[['type']] == 'price')
      return(with(kv, {
        paste0('P ', startdate, '  ', ticker, '  ', price, '\n')
      }))

    if (kv[['type']] == 'move') {
      return(with(kv, {
        paste0(
          startdate,
          ' * ',
          note,
          '\n',
          '  ',
          knowf(from, 'fullacc'),
          '\n',
          '  ',
          knowf(to, 'fullacc'),
          '          ',
          gsub('_', '', move),
          '\n'
        )
      }))
    }

    with(kv, {
      startdate <- as.Date(.cleandates(startdate))
      matdate   <- as.Date(.cleandates(matdate))

      principal <- as.numeric(principal)
      punits    <- gsub('_', ' ', punits)
      iunits    <- gsub('_', ' ', iunits)
      epunits   <- gsub('_', ' ', epunits)

      if (!is.null(rate))
        rate <- as.numeric(rate) / 100.0
#                  cat('saw ',startdate,' ', matdate, ' interval = ', interval, '\n')
      dates     <- seqs(startdate, matdate, interval)

      #            if (qdebug) str(list(startdate, matdate, interval, as.character(dates)))
      yfrac     <- as.numeric(diff(dates)) / 365.0

      note <- paste0(note, ' id:', id)

      if (compound == 'xfer') {
        interest   <- principal
        fprincipal <- principal
        note <-
          paste0(note,
                 ' principal:',
                 principal,
                 ' interest:',
                 interest)
      } else if (compound == 'wages') {
        interest   <-
          round(principal * rate * rep(1.0 / length(yfrac), length(yfrac)), 2)
        fprincipal <- principal
      } else if (compound == 'corp') {
        interest   <- round(principal * rate * yfrac, 2)
        fprincipal <- principal
        note <-
          paste0(note,
                 ' matdate:',
                 matdate,
                 ' principal:',
                 principal,
                 ' finalprincip:',
                  tail(fprincipal, 1),
                 ' type:',
                 type)
      } else if (compound == 'ibond') {
        ## TODO this is a placeholder
        interest   <- round(principal * rate * yfrac, 2)
        fprincipal <- principal
        note <-
          paste0(note,
                 ' matdate:',
                 matdate,
                 ' principal:',
                 principal,
                 ' finalprincip:',
                  tail(fprincipal, 1),
                 ' type:',
                 type)
      } else if (compound == 'tips') {

        actyear <- lubridate::year(startdate)
        actdate <- as.Date(paste0(actyear, '-4-15'))
        dates     <- seqs(actdate, matdate, interval)

        sdates <- as.Date(paste0(actyear, c('-1-1','-2-1')))

        aa <- pypq(outhome('/pyquotes'))
        aatips <- subset(aa, units == 'tips')
        aatips <- aatips[order(aatips$date),]
        #srate <- c(NA, aatips$price)[1 + findInterval(startdate, aatips$date)]
        srate <- c(NA, aatips$price)[1 + findInterval(sdates, aatips$date)]
        srate <- mean(srate)

        irate <- c(NA, aatips$price)[1 + findInterval(dates[-1], aatips$date)]
        irateR <- c(NA, aatips$price)[1 + findInterval(lubridate::ceiling_date(dates[-1], 'month'), aatips$date)]
        irate <- (irate + irateR)/2

        frate <-  c(NA, aatips$price)[1 + findInterval(matdate, aatips$date)]
        if (frate < srate) frate <- srate
        punits <- paste0('tips @@ ', discount)
        principal <- round(principal / srate, 6)
        interest   <- round(principal * rate * irate / 2, 2)
        fprincipal <- principal
        epunits <- paste0('tips @ ', frate)

        note <-
          paste0(note,
                 ' matdate:',
                 matdate,
                 ' principal:',
                 principal,
                 ' srate:',
                 srate,
                 ' frate:',
                 frate,
                 ' rate:',
                 rate,
                 ' finalprincip:',
                  tail(fprincipal, 1),
                 ' type:',
                 type)

      } else if (compound == 'frn') {
        ## TODO modify frnrate to modify principal as a function of time
        ## TODO when returning principal, frn does not post a 'cost' but should
          frnrate <- 0.0
        interest   <- round(principal * (frnrate + rate) * yfrac, 2)
        fprincipal <- principal
        punits  <- paste0('frn @@ ', principal)
        epunits <- paste0('frn @@ ', fprincipal)

        note <-
          paste0(note,
                 ' matdate:',
                 matdate,
                 ' principal:',
                 principal,
                 ' finalprincip:',
                  tail(fprincipal, 1),
                 ' type:',
                 type)
      } else if (compound == 'tbill') {
        fprincipal <- principal
        punits <- paste0('tbill (@@) ', discount)
        epunits <- paste0('tbill (@@) ', principal)
        note <-
          paste0(note,
                 ' matdate:',
                 matdate,
                 ' principal:',
                 principal,
                 ' finalprincip:',
                  tail(fprincipal, 1),
                 ' type:',
                 type)
      } else if (compound == 'cd') {
        #pperyear   <- length(seqdates(startdate, startdate + 366, interval) - 1)
        pperyear   <- length(seqs(startdate, startdate + 366, interval) - 1)
        interest   <-
          round(principal * ((1.0 + rate / pperyear) ** (pperyear * yfrac) - 1.0), 2)
        fprincipal <- principal + cumsum(interest)
        note <-
          paste0(note,
                 ' matdate:',
                 matdate,
                 ' principal:',
                 principal,
                 ' finalprincip:',
                 tail(fprincipal,1),
                 ' type:',
                 type)
      } else if (compound == 'loan') {
        # needs work
        interest <- as.numeric(payment)
        rf <- 1 + rate / 12
        fprincipal <-
          round((principal + interest / (1 - rf)) * rf ** (length(yfrac) - 1) - interest /
                  (1 - rf),
                2)
      } else {
        stop('unhandled compound', compound, '\n')
      }

      blks <- list()
      if (to != '')
        blks[[1]] <-
        paste0(
          startdate,
          ' * start ',
          note,
          ' .\n',
          '  ',
          knowf(from, 'fullacc'),
          '\n',
          '  ',
          knowf(to, 'fullacc'),
          '          ',
          principal,
          ' ',
          punits,
          '\n'
        )
      if (post_to != '')
        blks <- c(
          blks,
          paste0(
            dates[-1],
            ' * in ',
            note,
            ' .\n',
            '  ',
            knowf(post_from, 'fullacc'),
            '\n',
            '  ',
            knowf(post_to, 'fullacc'),
            '           ',
            round(interest, 2),
            ' ',
            iunits,
            '\n'
          )
        )
      if (return_to != '')
        blks <- c(
          blks,
          paste0(
            matdate,
            ' * end ',
            note,
            ' .\n',
            '  ',
            knowf(return_to, 'fullacc'),
            '\n',
            '  ',
            knowf(to, 'fullacc'),
            '           ',
            -1 * fprincipal[length(fprincipal)],
            ' ',
            epunits,
            '\n'
          )
        )
      unlist(blks)
    })
  }

  line1key <- function(a, verbose = FALSE) {
    plyr::llply(a,
          function(k) {
            if (verbose) utils::str(k)
            k1 <- sub('^P ', '', k[[1]])
              if (verbose) print(k1)
            as.Date(strsplit(k1, ' ')[[1]][[1]])
          }) %>% unlist
  }


  blocksmsort <- function(a, verbose = FALSE) {
    if (verbose) utils::str(a)
    a[order(line1key(a, verbose))]
  }

  message('loading blocks from ', ff)
  ff <- plyr::llply(ff, loadone)
  ff <- unlist(ff, recursive = FALSE, use.names = FALSE)
  ff <- prepost(ff)
  ff <- plyr::llply(ff, postone)
  ff <- unlist(ff)
  ff <- blocksmsort(ff, verbose)
  ff
}