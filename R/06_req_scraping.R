#### 06 REQ SCRAPING ###########################################################

source("R/01_startup.R")
library(RSelenium)


# Load data ---------------------------------------------------------------

# req_names <- qs::qread("output/req_scrape.qs")

req_names <- 
  qs::qread("output/req_names.qs") |> 
  mutate(name = str_remove_all(name, '¨|"|\\.|,'),
         name = str_trim(name), 
         name = if_else(name == "LE CLUBBONNIEBROOK INC", 
                        "LE CLUB BONNIEBROOK INC", name),
         result = NA,
         candidates = vector("list", n()),
         chosen_id = NA_character_,
         chosen_name = NA_character_,
         full_text = NA_character_,
         note = NA_character_)


# Start up ----------------------------------------------------------------

# Runs a docker image for scraping on an M1 Mac
# system("docker run -d -p 4448:4444 seleniarm/standalone-firefox:latest")
remDr <- remoteDriver(port = 4448L, browserName = "firefox")
remDr$open()



# Main for loop -----------------------------------------------------------

for (i in seq_along(req_names$name)) {
  
  print(paste0(i, ": ", Sys.time()))
  if (!is.na(req_names$result[[i]])) next
  
  
  # Navigate to page and click box ------------------------------------------
  
  remDr$navigate(paste0(
    "https://www.registreentreprises.gouv.qc.ca/RQAnonymeGR",
    "/GR/GR03/GR03A2_19A_PIU_RechEnt_PC/PageRechSimple.aspx"))
  Sys.sleep(1.5)
  
  # If at main page, click search link then reload
  if (remDr$getCurrentUrl()[[1]] == 
      "https://www.registreentreprises.gouv.qc.ca/fr/default.aspx") {
    search_start <- remDr$findElement(using = "xpath", 
                                      value = '//*[@id="rechreg"]')
    search_start$clickElement()
    Sys.sleep(0.4)
    remDr$navigate(paste0(
      "https://www.registreentreprises.gouv.qc.ca/RQAnonymeGR",
      "/GR/GR03/GR03A2_19A_PIU_RechEnt_PC/PageRechSimple.aspx"))
    Sys.sleep(2)
  }
  
  check_box <- remDr$findElement(using = "xpath", value = paste0(
    '//*[@id="CPH_K1ZoneContenu1_Cadr_IdSectionRechSimple_IdSectionRech',
    'Simple_CondUtil_CaseConditionsUtilisation_0"]'))
  
  if (length(check_box$getElementAttribute("checked")) == 0) {
    check_box$clickElement()
  } 
  
  
  # Enter search term -------------------------------------------------------
  
  search_field <- remDr$findElement(using = "xpath", value = paste0(
    '//*[@id="CPH_K1ZoneContenu1_Cadr_IdSectionRechSimple_IdSectionRechSimple_',
    'K1Fieldset1_ChampRecherche__cs"]'))
  search_field$clickElement()
  remDr$executeScript(paste0('arguments[0].value="', req_names$name[i], '";'), 
                      list(search_field))
  
  
  # Click search ------------------------------------------------------------
  
  search_button <- remDr$findElement(using = "xpath", value = paste0(
    '//*[@id="CPH_K1ZoneContenu1_Cadr_IdSectionRechSimple_IdSectionRechSimple_',
    'KRBTRechSimple_btnRechercher"]'))
  search_button$clickElement()
  
  Sys.sleep(1.8)
  
  
  # Test for no results -----------------------------------------------------
  
  no_result <- 
    tryCatch({suppressWarnings(suppressMessages(
      remDr$findElement(using = "xpath", 
                        value = '//*[@class="erreurfonctionnelle"]')))
      TRUE}, error = function(e) FALSE)
  
  if (no_result) {
    req_names$result[[i]] <- FALSE
    req_names$note[[i]] <- "No search result"
    next
  }


  # Construct candidates table ----------------------------------------------

  # Table helper function
  get_col <- function(n) {
    map_chr(rows, ~.x$findChildElement(
      using = "xpath", value = paste0('td[', n, ']'))$getElementText()[[1]])
  }
  
  # Get rows
  rows <- remDr$findElement(using = "xpath", value = paste0(
    '//*[@id="CPH_K1ZoneContenu1_Cadr_IdSectionResultat_IdSectionResultat_',
    'K1DetailsRecherche_K1GrilleDetail"]'))
  rows <- rows$findChildElement(using = "xpath", value = 'tbody')
  rows <- rows$findChildElements(using = "xpath", value = 'tr')
  
  # Get dossier number
  row_number <- get_col(1)
  
  # Get row names
  row_name <- get_col(2)
  
  # Capitalize row names, and remove accents
  row_name_fixed <- 
    row_name |> 
    toupper() |> 
    stringi::stri_trans_general("Latin-ASCII") |> 
    str_remove_all('[:punct:]')
  
  # Get remaining columns
  row_address <- get_col(3)
  row_status <- get_col(4)
  row_date_change <- as.Date(str_trim(get_col(5)))
  row_status_name <- get_col(6)
  row_date_start <- as.Date(str_trim(get_col(7)))
  row_date_end <- as.Date(str_trim(get_col(8)))
  
  # Assemble final candidates table
  candidates <- tibble(
    id = row_number,
    name = row_name,
    name_fixed = row_name_fixed,
    address = row_address,
    status = row_status,
    date_change = row_date_change,
    status_name = row_status_name,
    date_start = row_date_start,
    date_end = row_date_end
  )
  
  req_names$candidates[[i]] <- candidates
  
  
  # Choose correct result ---------------------------------------------------
  
  # Remove rows with no Numero de dossier (with Statut du nom == "Réservé)
  cand_choice <- 
    candidates |> 
    filter(!is.na(id), status_name != "Réservé")
  
  if (nrow(cand_choice) == 0) {
    req_names$result[[i]] <- FALSE
    req_names$note[[i]] <- "No non-reserved search result"
    next
  }
  
  # Remove rows with start date > 2020-11-30, if there are others
  if (sum(cand_choice$date_start <= "2020-11-30") != 0) {
    cand_choice <- 
      cand_choice |> 
      filter(date_start <= "2020-11-30")
  }
  
  # If there are rows with exact name matches, only keep them
  if (sum(str_remove_all(cand_choice$name_fixed, '[^[:digit:]|[:alpha:]]') ==
          str_remove_all(req_names$name[i], '[^[:digit:]|[:alpha:]]')) > 0) {
    cand_choice <- 
      cand_choice |> 
      filter(str_remove_all(name_fixed, '[^[:digit:]|[:alpha:]]') ==
               str_remove_all(req_names$name[i], '[^[:digit:]|[:alpha:]]'))
  }

  # Remove rows with end dates before 2020-11-30, if there are others
  if (sum(cand_choice$date_end < "2020-11-30", na.rm = TRUE) != 
      nrow(cand_choice)) {
    cand_choice <- 
      cand_choice |> 
      filter(date_end >= "2020-11-30" | is.na(date_end))
  } else req_names$note[[i]] <- "No 2020-11-30 result"
  
  # Take the earliest end date, if any are non-missing
  if (sum(is.na(cand_choice$date_end)) != nrow(cand_choice)) {
    cand_choice <- 
      cand_choice |> 
      filter(date_end == min(date_end))
  }
  
  # Choose the winner and click it
  cand_choice <- slice(cand_choice, 1)
  cand_id <- which(candidates$id == cand_choice$id)
  cand_name <- which(candidates$name == cand_choice$name)
  choice <- lubridate::intersect(cand_id, cand_name)[[1]]
  stopifnot(length(choice) == 1L)
  req_names$chosen_id[[i]] <- cand_choice$id
  req_names$chosen_name[[i]] <- cand_choice$name
  win <- rows[[choice]]$findChildElement(using = "xpath", value = 'td[1]/a')
  win$clickElement()
  
  
  # Get full text -----------------------------------------------------------
  
  Sys.sleep(1.8)
  full_text <- remDr$getPageSource() |> 
    pluck(1) |> 
    read_html() |> 
    html_element("body") |> 
    html_element("div") |> 
    html_element("form") |> 
    html_element(".sectiondroite") |> 
    html_element(".zonecontenu") |> 
    html_text()
  req_names$full_text[[i]] <- full_text
  
  
  # Save result and move on -------------------------------------------------

  req_names$result[[i]] <- TRUE
  qs::qsave(req_names, file = "output/req_scrape.qs")
  
}


# Clean up ----------------------------------------------------------------

remDr$close()
rm(remDr)
gc()


# Parse -------------------------------------------------------------------

new_parsed <- 
  req_names$name |>
  seq_along() |>
  map_dfr(parse_req)

qs::qsave(new_parsed, file = "output/req_parsed.qs")


# Collect all results -----------------------------------------------------

full_parsed <- 
  bind_rows(
    new_parsed_1, new_parsed_2, new_parsed_3, new_parsed_4, new_parsed_5,
    new_parsed_6, new_parsed_7, new_parsed_8
  ) |> 
  distinct(name, .keep_all = TRUE)

qsave(full_parsed, file = "output/full_parsed.qs")


# Prepare next round ------------------------------------------------------

req_names <- 
  new_parsed_8 |> 
  filter(!person, !is.na(new_name)) |> 
  filter(!new_name %in% full_parsed$name) |> 
  select(name = new_name) |> 
  mutate(full_text = vector("list", length(name))) |> 
  distinct()
