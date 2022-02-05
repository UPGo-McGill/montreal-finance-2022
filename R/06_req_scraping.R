#### 06 REQ SCRAPING ###########################################################

source("R/01_startup.R")
library(RSelenium)


# Load data ---------------------------------------------------------------

req_names <- qs::qread("output/req_names.qs")


# Get list to loop over ---------------------------------------------------

req_names <-
  req_names |> 
  mutate(name = str_remove_all(name, '¨|"|\\.|,'),
         name = str_trim(name), 
         name = if_else(name == "LE CLUBBONNIEBROOK INC", 
                        "LE CLUB BONNIEBROOK INC", name),
         full_text = vector("list", n()),
         types = vector("list", n()))


# Start up ----------------------------------------------------------------

system("docker run -d -p 4448:4444 selenium/standalone-firefox")
remDr <- remoteDriver(port = 4448L, browserName = "firefox")
remDr$open()


# Main for loop -----------------------------------------------------------

for (i in seq_along(req_names$name)) {
  
  print(paste0(i, ": ", Sys.time()))
  
  
  # Navigate to page and click box ------------------------------------------
  
  remDr$navigate(paste0(
    "https://www.registreentreprises.gouv.qc.ca/RQAnonymeGR",
    "/GR/GR03/GR03A2_19A_PIU_RechEnt_PC/PageRechSimple.aspx"))
  Sys.sleep(1.2)
  
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
  
  if (no_result) next
  
  
  # Choose correct result ---------------------------------------------------
  
  results <- remDr$findElement(using = "xpath", value = paste0(
    '//*[@id="CPH_K1ZoneContenu1_Cadr_IdSectionResultat_IdSectionResultat_',
    'K1DetailsRecherche_K1GrilleDetail"]'))
  results <- results$findChildElement(using = "xpath", value = 'tbody')
  
  rows <- results$findChildElements(using = "xpath", value = 'tr')
  
  # Capitalize row names, and remove accents
  row_name <- map_chr(rows, ~{
    .x$findChildElement(using = "xpath", value = 'td[2]')$getElementText()[[1]]
  })
  row_name <- toupper(row_name)
  row_name <- stringi::stri_trans_general(row_name, "Latin-ASCII")
  row_name <- str_remove_all(row_name, '[:punct:]')
  
  # Remove rows with no Numero de dossier (with Statut du nom == "Réservé)
  row_status <- map_chr(rows, ~{
    .x$findChildElement(using = "xpath", value = 'td[6]')$getElementText()[[1]]
  })
  rows <- rows[row_status != "Réservé"]
  row_name <- row_name[row_status != "Réservé"]
  
  # If there are rows with exact name matches, only keep them
  if (sum(str_remove_all(row_name, '[^[:digit:]|[:alpha:]]') ==
          str_remove_all(req_names$name[i], '[^[:digit:]|[:alpha:]]')) > 0) {
    rows <- rows[str_remove_all(row_name, '[^[:digit:]|[:alpha:]]') ==
                   str_remove_all(req_names$name[i], 
                                  '[^[:digit:]|[:alpha:]]')]
  }
  
  # Remove rows with start date > 2020-11-30, if there are others
  row_start <- map_chr(rows, ~{
    .x$findChildElement(using = "xpath", value = 'td[7]')$getElementText()[[1]]
  })
  if (sum(as.Date(row_start) > "2020-11-30") != length(rows)) {
    rows <- rows[as.Date(row_start) <= "2020-11-30"]  
  }
  
  # If there are rows with no end date, only keep them
  row_end <- map_chr(rows, ~{
    .x$findChildElement(using = "xpath", value = 'td[8]')$getElementText()[[1]]
  })
  if (mean(row_end == " ") > 0) {
    rows <- rows[row_end == " "]  
    # Otherwise, take the rows with the latest end date
  } else {
    rows <- rows[which.max(as.Date(row_end))]
  }
  
  # If there are no rows left, go to next result
  if (length(rows) == 0) next
  
  # Otherwise, choose the winner and click it
  row_winner <- rows[[1]]$findChildElement(using = "xpath", value = 'td[1]/a')
  row_winner$clickElement()
  
  
  # Get full text -----------------------------------------------------------
  
  Sys.sleep(1.8)
  boxes <- remDr$findElements(using = "xpath", 
                              value = '//*[@class="zonelibellechamp"]')
  full_text <- map_chr(boxes, ~.x$getElementText()[[1]])
  req_names$full_text[[i]] <- full_text
  
  
  # Get Type d'associé ------------------------------------------------------
  
  keep_trying <- TRUE
  n <- 0
  types <- vector("list")
  
  while (keep_trying && n < 10) {
    
    to_search <- paste0(
      '//*[@name="ctl00$CPH_K1ZoneContenu1_Cadr$Section01$Section01$ctl47$ctl0',
      n, '$ctl00$champTypeAssoc$_cs"]')
    
    keep_trying <- suppressWarnings(suppressMessages(
      tryCatch(expr = {
        types[[n + 1]] <- remDr$findElement(using = "xpath", value = to_search)
        n <- n + 1
        TRUE
      }, error = function(e) FALSE)
    ))
    
  }
  
  types <- map_chr(types, ~.x$getElementAttribute("value")[[1]])
  req_names$types[[i]] <- types
  
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
