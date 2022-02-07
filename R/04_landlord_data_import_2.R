#### 04 LANDLORD DATA IMPORT PART 2 ############################################

source("R/01_startup.R")
qload("output/LL.qsm", nthreads = availableCores())

# This script requires `postal_addresses_owners.xlsx` (the result of manual
# classification) to be present in `data`.


# Import addresses and owners after categorization ------------------------

landlord_names <- readxl::read_xlsx("data/postal_addresses_owners.xlsx")

landlords <-
  LL_2020_postal |> 
  group_by(landlord_name) |> 
  summarize(number_rental_units = sum(number_rental_units, na.rm = TRUE)) |> 
  arrange(desc(number_rental_units)) |> 
  filter(number_rental_units >= 1)

write_csv(landlords, "data/landlords.csv")

qsavem(landlord_names, LL_2020, LL_2020_postal, file = "output/LL.qsm", 
       nthreads = availableCores())

rm(landlords)
