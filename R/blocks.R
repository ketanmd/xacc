
#knowf = .knowf()

loadone <- function(g) {
    message('.', appendLF = FALSE)
    message('Reading lines into blocks from ', g, '\n')
    ## remove poorly printing characters
    a <- readLines(g, warn = FALSE) %>% sub('\\\xae', '', .)
    ## remove unintersting lines
    a[grep('^(;|\\s*$|#||\\*)', a, invert = TRUE)] -> a1
    #message('Found ', length(a1), ' lines from file ', g, '\n')
    if (!length(a1)) {
      warning('Found NO lines from file ', ff)
      return(list())
    }
    ## identify block beginnings, lines not beginning with a space
    a1b <- grep ('^ ', a1, invert = TRUE)
    ## identify block endings
    a1e <- c(a1b[-1] - 1, length(a1))
    ## make a dataframe with the beginnings and ends
    adf <- data.frame(b = a1b, e = a1e)
    ## make a list of blocks
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
    lapply(intervals,  seqdates, sdate = sdate, edate = edate) -> thedates
      #            identity -> a ; str(a) ; a %>%
      #            # here, unlist destroys the as.Date nature of the answers
      lapply(thedates, as.character) %>% unlist %>% as.Date %>%
      #            identity ->a ; cat(a) ; a %>%
      pmax(sdate) %>%
        pmin(edate) %>%
      c(sdate, edate) %>%
      sort %>% unique -> thedates
      return(thedates)
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

    if (interval == 'midmonth') {
#      str(list(sdate,edate))
        sdate <- as.Date(lubridate::floor_date(sdate) + lubridate::days(14))
#      str(list(sdate, edate))
      return(seq.Date(
        sdate,
#        lubridate::ceiling_date(sdate, 'month') + lubridate::days(14) - lubridate::months(1),
#        lubridate::ceiling_date(sdate, 'month') + lubridate::days(14) + lubridate::ddays(-30),
        edate,
        'month'
      ))
    }
    #        str(list(sdate, edate, interval, datesub, seq.Date(sdate,edate,interval),
    #                 seq.Date(sdate,edate,interval)+datesub))
    return(seq.Date(sdate, edate, interval) + datesub)
  }

  postone <- function(b, verbose = FALSE) {
    knowf <- .knowf()
    if (!tibble::is.tibble(b)) b <- tibble(lines = b)

    b <- unlist(use.names = FALSE, b)

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
        punits = 'CD_@_1',
        iunits = 'CD_@_1',
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
      eebond   = list(
        interval = '6-months',
        punits = 'eebond_@_1',
        post_from = 'Income:Interest:ebond',
        rate = 4.0
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

      yfrac     <- as.numeric(diff(dates)) / 365.0

#      if (compound == 'wages') print(list(length(yfrac), startdate, matdate, interval, dates, as.Date(dates), as.character(dates)))

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
      } else if (compound == 'eebond') {
        dates     <- seqs(startdate, matdate, interval)

        loadyields <- function() {
          a <- read.table('passbooks/yields',
                          stringsAsFactors = FALSE,
                          header = TRUE)
          a$Date <- as.Date(strptime(a$Date, '%Y%m%d'))
          a
        }
        tyields <- loadyields()

        mungerates <- function(alld, qty, sdate) {
          alld %<>% subset(qty == qty) %>% dplyr::arrange(Date)
          tooearly <- alld$Date < sdate
          toolate <- alld$Date > sdate
          if (all(tooearly)) return(alld[rep(nrow(alld), 60), 'rate'])
          if (all(toolate)) warning('Rates to not start early enough\n')
          todrop <- sum(tooearly)-1
          if (todrop > 0) alld <- alld[-1:-todrop,, drop = FALSE]
          if (nrow(alld) < 60) alld <- rbind(alld, alld[rep(nrow(alld), 60-nrow(alld)), ,drop = FALSE])
          if (nrow(alld) > 60) alld <- alld[1:60,]
          return(alld[1:60, 'rate'])
        }


        if (startdate < as.Date('1993-03-01')) {
          g_rate <- c(0, rep(6, 24), rep(4, 36))
          m_rate <- c(0, 0.85*mungerates(tyields, '5yyield', startdate))
          minterest <- principal * cumprod(1 + m_rate/200)
          ginterest <- principal * cumprod(1 + g_rate/200)
            if (sum(minterest) > sum(ginterest))
                interest <- minterest else interest <- ginterest
        } else if (startdate < as.Date('1995-5-1')) {
          g_rate <- c(0, rep(4, 60))
          m_rate <- c(0, 0.85*mungerates(tyields, '5yyield', startdate))
          minterest <- principal * cumprod(1 + m_rate/200)
          ginterest <- principal * cumprod(1 + g_rate/200)
            if (sum(minterest) > sum(ginterest))
                interest <- minterest else interest <- ginterest
        } else if (startdate < as.Date('1997-5-1')) {
          m_rate <- rep(0, 61)
          m_rate[2:11]  <-      mungerates(tyields, 'eesrate', startdate)[1:10]
          m_rate[12:61] <- 0.85*mungerates(tyields, '5yyield', startdate)[11:60]
          interest <- principal * cumprod(1 + m_rate/200)
          if (interest[35] < principal) {
              interest[35] <- 2*principal
              interest[36:61] <- 2*principal * cumprod(1 + m_rate[36:61]/200)
          }
        } else if (startdate < as.Date('2003-6-1')) {
          m_rate <- c(0, 0.90 * mungerates(tyields, '5yyield', startdate))
          interest <- principal * cumprod(1 + m_rate/200)
          if (interest[35] < principal) {
              interest[35] <- 2*principal
              interest[36:61] <- 2*principal * cumprod(1 + m_rate[36:61]/200)
          }
        } else if (startdate <- as.Date('2005-5-1')) {
          m_rate <- c(0, 0.90 * mungerates(tyields, '5yyield', startdate))
          interest <- principal * cumprod(1 + m_rate/200)
          if (interest[41] < principal) {
              interest[41] <- 2*principal
              interest[42:61] <- 2*principal * cumprod(1 + m_rate[42:61]/200)
          }
        } else {
          m_rate <- c(0, mungerates(tyields, 'eefrate', startdate))
          interest <- principal * cumprod(1 + m_rate/200)
          if (interest[41] < principal) {
              interest[41] <- 2*principal
              interest[42:61] <- 2*principal * cumprod(1 + m_rate[42:61]/200)
          }
        }

        interest   <- round(diff(interest), 2)
        fprincipal <- principal + sum(interest)
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

        if (file.exists(outhome('/pyquotes'))) {
          aa <- pypq(outhome('/pyquotes'))
          aatips <- subset(aa, units == 'tips')
          aatips <- aatips[order(aatips$date),]
          #srate <- c(NA, aatips$price)[1 + findInterval(startdate, aatips$date)]
          srate <- c(NA, aatips$price)[1 + findInterval(sdates, aatips$date)]

          irate <- c(NA, aatips$price)[1 + findInterval(dates[-1], aatips$date)]
          irateR <- c(NA, aatips$price)[1 + findInterval(lubridate::ceiling_date(dates[-1], 'month'), aatips$date)]
          #str(list(sdates,srate,irate,irateR,matdate,dates))

          srate <- mean(srate)
          irate <- (irate + irateR)/2

          frate <-  c(NA, aatips$price)[1 + findInterval(matdate, aatips$date)]
        } else {
          frate = srate = irate = -100.00
        }
        if (frate < srate) frate <- srate
          punits <- paste0('tips @@ ', discount)
      #    punits <- paste0('tips {', srate, '} @@ ', discount)
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

  # newloadone <- function(g) {
  #   a <- readLines(g, warn = FALSE) %>% sub('\\\xae', '', .)
  #   a[grep('^(;|\\s*$|#||\\*)', a, invert = TRUE)] -> a1
  #   if (!length(a1)) {
  #     warning('Found NO lines from file ', ff)
  #     return(list())
  #   }
  #
  #   ## move balance lines to end
  #   balancelines <- grepl(' bal:', a1)
  #   a1 <- c(a1[!balancelines], a1[balancelines])
  #
  #   bbegins = grep ('^ ', a1, invert = TRUE)
  #   blengths = diff(c(bbegins, length(a1)+1))
  #
  #   fixfirst <- {
  #       sub('^P +([^ ]+) +([^ ]+) +([^ ]+)', '\\1 type:price ticker:\\2 price:\\3', .) %>>>%
  #           sub(' move:', ' type:move  move:', .) %>>>%
  #           sub('^([^ ]+) +bal:([^ ]+) +== *', '\\1 type:assert to:\\2 ', .) %>>>%
  #           sub('^([^ ]+) +bal:([^ ]+) += *',  '\\1 type:balance to:\\2 ', .) %>>>%
  #        #   sub('^(\\d.*?) .*', '\\1', .) %>>>%
  #           {tibble(data = .)} %>>>%
  #           tidyr::separate(., data, c('date', 'rest'), extra = 'merge', sep = ' ') %>>>%
  #           glimpse %>>>%
  #           dplyr::mutate(., date = !!.cleandates(date)) %>>>%
  #           glimpse %>>>%
  #           tidyr::unite(., sep = ' ') %>>>% unlist(., use.names = FALSE)
  #   }
  #
  #   a1[bbegins] %<>% fixfirst
  #
  #   tibble(lines = a1, file = g, id = rep(bbegins, blengths)) %>%
  #       group_by(., file, id) %>%
  #       tidyr::nest(.)
  # }

#' process files, extract blocks, expand input blocks as needed
#' returns blocks ready for ledger
#' @param ff character vector of files to process
#' @param verbose turn on messages (default = FALSE)
#' @importFrom magrittr '%>%'
#'
blocksprocess <- function(ff, verbose = FALSE) {


 message('loading blocks from ', length(ff), ' files:')
  ff <- plyr::llply(ff, loadone)
  ff <- unlist(ff, recursive = FALSE, use.names = FALSE)
  ff <- prepost(ff)
  ff <- plyr::llply(ff, postone)
  ff <- unlist(ff)
  ff <- blocksmsort(ff, verbose)
  message('\n')

  ff
}

# newblocksprocess <- function() {
#     g <- '/home/family/money/accounting/passbooks/2015.passbook'
#     g %>% lapply(newloadone) %>%
#         bind_rows %>% rowwise %>%
#             dplyr::mutate(data = postone(data))
#   }
#

