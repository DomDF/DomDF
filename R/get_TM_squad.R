#' get_TM_squad
#'
#' A webscraping function for the website 'https://www.transfermarkt.com/'.
#' Returns a preprocessed tibble of any squad.
#'
#'
#' @param url A valid ()detailed) transfermarkt url. Navigate to a club page, click on the 'detailed' tab, and then paste the url - REQUIRED.
#' @param user_agent A character introducing yourself to the website - REQUIRED.
#' @param raw  Logical: if TRUE, returns the unprocessed table. if FALSE, returns a processed tibble - defaults to FALSE
#' @keywords
#' @export
#' @examples
#' Juventus squad <- get_TM_squad(url =  'https://www.transfermarkt.co.uk/juventus-fc/kader/verein/506/saison_id/2020/plus/1', user_agent = 'John Smith personal project')
#' Blackburn_Rovers_squad <- get_TM_squad(url = 'https://www.transfermarkt.co.uk/blackburn-rovers/kader/verein/164/saison_id/2020/plus/1', user_agent = 'John Smith personal project')

get_TM_squad <- function(url, agent, raw = FALSE){

  require(tidyverse); require(polite)

  session <- polite::bow(url = url, user_agent = agent)

  raw_squad <- session %>%
    polite::scrape() %>%
    rvest::html_nodes('#yw1 > table') %>%
    rvest::html_table(fill = TRUE)

  team_name <- session %>%
    polite::scrape() %>%
    rvest::html_nodes('#verein_head > div > div.dataHeader.dataExtended > div.dataMain > div > div.dataName > h1') %>%
    rvest::html_text() %>%
    stringr::str_replace_all(pattern = '\n', replacement = '') %>%
    stringr::str_trim()

  squad <- session %>%
    polite::scrape() %>%
    rvest::html_nodes('#yw1 > table') %>%
    rvest::html_table(fill = TRUE) %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    filter(!(is.na(X.) | X. == ''))

  cols <- c('number', NA, NA, 'player', 'position', 'DoB', NA, 'height_m', 'foot', 'joined', NA, 'contract_exp', 'market_value')
  colnames(squad) <- cols; squad <- squad[!is.na(names(squad))]

  squad <- squad %>%
    mutate(currency = case_when(
      grepl(pattern = '£', x = market_value) ~ '£',
      grepl(pattern = '€', x = market_value) ~ '€',
      grepl(pattern = '$', x = market_value) ~ '$'
    )) %>%
    mutate(market_value = sub(pattern = '£|€|$', replacement = '', x = market_value)) %>%
    mutate(market_value = case_when(
      grepl(pattern = 'm', x = market_value) ~ 1e6 * as.numeric(sub(pattern = 'm',
                                                                        replacement = '',
                                                                        x = market_value)),
      grepl(pattern = 'Th.', x = market_value) ~ 1e3 * as.numeric(sub(pattern = 'Th.',
                                                                          replacement = '',
                                                                          x = market_value))
    )) %>%
    mutate(contract_exp = lubridate::mdy(gsub(pattern = ' ', replacement = '-',
                                              x = gsub(pattern = ',', replacement = '', x = contract_exp)))) %>%
    mutate(joined = lubridate::mdy(gsub(pattern = ' ', replacement = '-',
                                        x = gsub(pattern = ',', replacement = '', x = joined)))) %>%
    mutate(height_m = as.numeric(gsub(pattern = ',', replacement = '.',
                               x = gsub(pattern = ' m', replacement = '', x = height_m)))) %>%
    mutate(DoB = lubridate::mdy(gsub(pattern = ' ', replacement = '-',
                                     x = gsub(pattern = ',', replacement = '',
                                              x = stringr::str_replace(string = DoB,
                                                                       pattern = " \\(.*",
                                                                       replacement = ''))))) %>%
    mutate(surname = stringr::str_trim(string = stringr::word(string =  player, sep = '[.]', start = -1), side = 'both'),
           name = stringr::word(string = player, sep = ' ', end = 1)) %>%
    mutate(Age = lubridate::interval(start = DoB, end = Sys.Date()) / lubridate::years(1))

  for (i in seq(from = 1, to = nrow(squad))){

    if(grepl(pattern = squad$name[i], x = squad$surname[i])) {

      squad$player_name[i] <- stringr::str_sub(string = squad$surname[i], start = 1,
                                               end = nchar(squad$surname[i])/2)

    } else {

      squad$player_name[i] <- paste(squad$name[i], squad$surname[i])

    }

  }

  squad <- squad %>%
    select(-c(player, name, surname)) %>%
    arrange(desc(market_value)) %>%
    mutate(squad_value = sum(market_value)) %>%
    mutate(player_name = fct_reorder(.f = as.factor(player_name),
                                     .x = market_value))

  squad$team <- team_name

  if(raw == FALSE){

    return(squad)

  } else {

    return(raw_squad)

  }

}



