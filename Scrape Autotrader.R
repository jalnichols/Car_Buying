
library(tidyverse)
library(rvest)

#

RADIUS <- 100
MIN_PRICE = 14000
MAX_PRICE = 28000
PULL_RECORDS = 100
FROM_X <- c(0,100,200,300,400,500,600,700,800,900,1000)

pages_list <- vector("list", 10)

for(i in 1:10) {
  
  FROM <- FROM_X[[i]]
  
  URL <- paste0('https://www.autotrader.com/rest/searchresults/base?vehicleStyleCodes=SEDAN&driveGroup=AWD4WD&listingTypes=USED&searchRadius=',
                RADIUS, 
                '&zip=18501&minPrice=', 
                MIN_PRICE, 
                '&maxPrice=',
                MAX_PRICE ,
                '&startYear=2017&endYear=2019&marketExtension=true&sortBy=distanceASC&numRecords=',
                PULL_RECORDS, 
                '&firstRecord=', 
                FROM)
  
#
  
  PAGE <- URL %>%
    jsonlite::fromJSON(url)
  
  
  LISTINGS <- PAGE$listings
  
  #
  
  listings_list <- vector("list", 100)
  
  for(v in 1:length(LISTINGS)) {
    
    features <- LISTINGS[[v]]$features %>%
      enframe(name = NULL) %>%
      nest()
    
    DealIndicator = LISTINGS[[v]]$pricingDetail$dealIndicator
    
    if(is.null(DealIndicator)) {
      
      DealIndicator = "None"
      
    }
    
    if(is.null(LISTINGS[[v]]$specifications$color$value)) {
      
      Color = "Missing"
      
    } else {
      
      Color = LISTINGS[[v]]$specifications$color$value
      
    }
    
    if(is.null(LISTINGS[[v]]$make)) {
      
      Make = "Missing"
      
    } else {
      
      Make = LISTINGS[[v]]$make
      
    }
    
    df <- tibble(
      
      Make = Make,
      Model = LISTINGS[[v]]$model,
      Dealer = LISTINGS[[v]]$ownerName,
      #Phone = LISTINGS[[v]]$phone$value,
      Price = LISTINGS[[v]]$pricingDetail$primary,
      Color = Color,
      #MPG = LISTINGS[[v]]$specifications$mpg$value,
      Engine = LISTINGS[[v]]$specifications$engine$value,
      DriveType = LISTINGS[[v]]$specifications$driveType$value,
      Mileage = LISTINGS[[v]]$specifications$mileage$value,
      Year = LISTINGS[[v]]$year,
      Style = LISTINGS[[v]]$style[[1]],
      ZipCode = LISTINGS[[v]]$zip,
      Features = features %>%
        .[[1]],
      DealIndicator = DealIndicator
    )
    
    listings_list[[v]] <- df
    
  }
  
  pages_list[[i]] <- bind_rows(listings_list)
  
}

#

listings_data <- bind_rows(pages_list) %>%
  
  mutate(Mileage = as.numeric(str_replace(Mileage, ",", "")),
         YearVar = Year - 2018)

#

library(lme4)

mod <- lmer(Price ~ (1 | Make) + Mileage + YearVar, data = listings_data)

# everything 1000 miles drops price by $55
# each year old drops price by $600
# GREAT deal = $1000 off
# None deal = $600 more
# Make impact
# Mazda, Ford, Subaru are $4000 off
# Genesis, Lexus, Benz are $2000+ more

makes <- ranef(mod)[[1]] %>% rownames_to_column()

#

preds <- cbind(
  
  pred = predict(mod, listings_data),
  
  listings_data
  
) %>%
  
  mutate(vs_pred = Price - pred)
