# data.r - Download necessary data for ancestry census map

# Install required packages if not already installed
packages <- c("tidycensus", "tigris", "sf", "dplyr", "tidyr", "readr")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Set Census API key (you'll need to get one from https://api.census.gov/data/key_signup.html)
# Replace "YOUR_API_KEY" with your actual Census Bureau API key
# Sys.setenv(CENSUS_API_KEY = "YOUR_API_KEY")

# Option 1: Download Census ancestry data using tidycensus
# This requires an API key - you can request one at https://api.census.gov/data/key_signup.html

download_ancestry_data <- function() {
  # Get list of available ancestry variables from 2020 Census
  # Variables for ancestry typically start with "B04001" (Ancestry)
  
  # Example: Download 2020 Census ancestry data by county
  # This uses table B04001 which contains ancestry information
  
  ancestry_vars <- c(
    total_population = "B04001001",  # Total population (for percentage calculation)
    german = "B04001002",
    irish = "B04001003",
    english = "B04001004",
    italian = "B04001005",
    polish = "B04001006",
    french = "B04001007",
    scottish = "B04001008",
    dutch = "B04001009",
    norwegian = "B04001010",
    swedish = "B04001011",
    russian = "B04001012",
    greek = "B04001013",
    portuguese = "B04001014",
    jewish = "B04001015",
    mexican = "B04001016",
    puerto_rican = "B04001017",
    cuban = "B04001018",
    dominican = "B04001019",
    chinese = "B04001020",
    korean = "B04001021",
    indian = "B04001022",
    japanese = "B04001023",
    vietnamese = "B04001024",
    hawaiian = "B04001025",
    african_american = "B04001026",
    arab = "B04001027",
    spanish = "B04001028"
  )
  
  # Uncomment below when you have a Census API key
  # ancestry_data <- get_acs(
  #   geography = "county",
  #   variables = ancestry_vars,
  #   year = 2021,
  #   geometry = FALSE,
  #   survey = "acs5"
  # )
  
  # For now, save placeholder data structure
  # ancestry_data will be stored in data/ancestry_data.rds
  
  message("To download Census data, uncomment the get_acs() call above and set your Census API key")
  message("Get your free API key at: https://api.census.gov/data/key_signup.html")
}

# Download geographic boundaries (county/state shapefiles)
download_geographic_data <- function() {
  message("Downloading US county boundaries...")
  
  counties <- counties(cb = TRUE)  # cb = TRUE for simplified boundaries
  
  message("Downloading US state boundaries...")
  states <- states(cb = TRUE)
  
  # Save to data directory
  if (!dir.exists("data")) {
    dir.create("data")
  }
  
  saveRDS(counties, "data/counties.rds")
  saveRDS(states, "data/states.rds")
  
  message("Geographic data saved to data/counties.rds and data/states.rds")
}

# Main execution
main <- function() {
  message("Starting data download process...")
  
  # Download geographic boundaries
  download_geographic_data()
  
  # Download ancestry data (requires API key)
  download_ancestry_data()
  
  message("Data download complete!")
  message("\nNext steps:")
  message("1. Get a Census API key: https://api.census.gov/data/key_signup.html")
  message("2. Set it with: Sys.setenv(CENSUS_API_KEY = 'your_key_here')")
  message("3. Uncomment the get_acs() function in download_ancestry_data()")
  message("4. Run download_ancestry_data() to fetch Census data")
}

# Run main function
if (!interactive()) {
  main()
} else {
  message("Script loaded. Run main() to download data")
}
