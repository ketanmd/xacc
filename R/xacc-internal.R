source('R/myacc-internal.R')
source('R/blocks.R')

#' @import gestalt
#' @import tibble
#' @import tidyr
cleandatecol <-
    fn(x ~ tibble(date = x)) %>>>%
    dplyr::mutate(date = as.character(date),
                  date = gsub('-', '/', date),
                  date = case_when(nchar(date) == 4
                                   ~ paste0(date, '/12/31'),
                                   (nchar(date) == 8 & !grepl('/', date))
                                   ~ paste0(substr(date,1,4),'/',
                                            substr(date,5,6),'/',
                                            substr(date,7,8)),
                                   TRUE ~ date)) %>>>%
    tidyr::extract(date, c('.year','.month','.day'), '(\\d+)/(\\d+)/(\\d+)') %>>>%
    dplyr::mutate(date = 0.5*(10001*(as.integer(.year)+as.integer(.day))
        + 200*as.integer(.month)
        + 9999*abs(as.integer(.year)-as.integer(.day))),
        date = as.character(date),
        date = sub('(....)(..)(..)', '\\1/\\2/\\3', date)) %>>>%
    select(date) %>>>% unlist(use.names = FALSE)
## yyyy       yyyymmdd
## yyyy/mm/dd yyyy/mm/d
## yyyy/m/dd  yyyy/m/d
## dd/mm/yyyy d/mm/yyyy
## dd/m/yyyy  d/m/yyyy

newloadone <- function(g) {
    a <- readLines(g, warn = FALSE) %>% sub('\\\xae', '', .)
    a[grep('^(;|\\s*$|#||\\*)', a, invert = TRUE)] -> a1
    if (!length(a1)) {
        warning('Found NO lines from file ', ff)
        return(list())
    }

    ## move balance lines to end
    balancelines <- grepl(' bal:', a1)
    a1 <- c(a1[!balancelines], a1[balancelines])

    tibble(data = a1,
           file = g,
           bbegin = !grepl('^ ', data),
           id = cumsum(bbegin)) -> da1

    prefirst <- {
        sub('^P +([^ ]+) +([^ ]+) +([^ ]+)', '\\1 type:price ticker:\\2 price:\\3', .) %>>>%
            sub(' move:', ' type:move  move:', .) %>>>%
            sub('^([^ ]+) +bal:([^ ]+) +== *', '\\1 type:assert to:\\2 ', .) %>>>%
            sub('^([^ ]+) +bal:([^ ]+) += *',  '\\1 type:balance to:\\2 ', .) }

    fixfirst <-
        mutate(data = case_when(bbegin ~ prefirst(data), TRUE ~ data)) %>>>%
        tidyr::separate(data, c('date', 'rest'), extra = 'merge', sep = ' ') %>>>%
        mutate(date = case_when(bbegin ~ cleandatecol(date), TRUE ~ date),
               bbegin = NULL) %>>>%
        tidyr::unite(data, date, rest, sep = ' ') %>>>%
        group_by(file, id) %>>>%
        tidyr::nest(., data, .key = 'block')

    fixfirst(da1)
}

posttibble <- fn(tx ~ list(tibble(data = list(postone(tx$data)))))
newblocksprocess <-
    lapply(newloadone) %>>>%
    bind_rows %>>>%
    rowwise %>>>%
    mutate(block = posttibble(block))

#####################################################
if (0) {
x <- c('20180104','2013','2013-3-4', '4-15-2011')
xf <- cleandatecol(x)
print(x)
print(xf)

g <- '/home/family/money/accounting/passbooks/2015.passbook'
ng <- newloadone(g)

npg <- newblocksprocess(g)
opg <- blocksprocess(g) # list of character vectors


str(opg[[1]])
str(npg$block[[1]]$data[[1]])

#####################################################

files = paste0('/home/family/money/accounting/passbooks/',2008:2018,'.passbook')

files %>% newblocksprocess
}
