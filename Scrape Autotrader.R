
library(tidyverse)
library(rvest)

#

RADIUS <- 100
MIN_PRICE = 7000
MAX_PRICE = 40000
PULL_RECORDS = 100
FROM_X <- c(0,100,200,300,400,500,600,
            700,800,900,1000)

pages_list <- vector("list", length(FROM_X))

for(i in 1:length(FROM_X)) {
  
  FROM <- FROM_X[[i]]
  
  URL <- paste0('https://www.autotrader.com/rest/searchresults/base?vehicleStyleCodes=SEDAN&listingTypes=USED&searchRadius=',
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
    
    if(is.null(LISTINGS[[v]]$specifications$mileage$value)) {
      
      Mileage = NA
      
    } else {
      
      Mileage = LISTINGS[[v]]$specifications$mileage$value
      
    }
    
    if(is.null(LISTINGS[[v]]$model)) {
      
      Model = "Missing"
      
    } else {
      
      Model = LISTINGS[[v]]$model
      
    }
    
    if(is.null(LISTINGS[[v]]$ownerName)) {
      
      Dealer = "Missing"
      
    } else {
      
      Dealer = LISTINGS[[v]]$ownerName
      
    }
    
    if(is.null(LISTINGS[[v]]$pricingDetail$primary)) {
      
      Price = NA
      
    } else {
      
      Price = LISTINGS[[v]]$pricingDetail$primary
      
    }
    
    if(is.null(LISTINGS[[v]]$specifications$engine$value)) {
      
      Engine = "Missing"
      
    } else {
      
      Engine = LISTINGS[[v]]$specifications$engine$value
      
    }
    
    if(is.null(LISTINGS[[v]]$specifications$driveType$value)) {
      
      DriveType = "Missing"
      
    } else {
      
      DriveType = LISTINGS[[v]]$specifications$driveType$value
      
    }
    
    if(is.null(LISTINGS[[v]]$year)) {
      
      Year = "Missing"
      
    } else {
      
      Year = LISTINGS[[v]]$year
      
    }
    
    df <- tibble(
      
      Make = Make,
      Model = Model,
      Dealer = Dealer,
      #Phone = LISTINGS[[v]]$phone$value,
      Price = Price,
      Color = Color,
      #MPG = LISTINGS[[v]]$specifications$mpg$value,
      Engine = Engine,
      DriveType = DriveType,
      Mileage = Mileage,
      Year = Year,
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

mod <- lmer(Price ~ (1 | Car) + (1 | Make) + Mileage + YearVar + (DealIndicator), 
            
            data = listings_data %>%
              mutate(Car = paste0(Make, Model)))

# everything 1000 miles drops price by $75
# each year old drops price by $1350
# GREAT deal = $1400 off
# None deal = $1000 more
# Make impact
# Benz, Cadillac, Audi, BMW (5k)
# Genesis, Lincoln, Volvo (3k+)

# Hyundai, Volks, Kia, Chevy, Nissan (-4k)
# Toyota, Honda, Ford, Mazda (-2k)

models <- ranef(mod)[[1]] %>% rownames_to_column()
makes <- ranef(mod)[[2]] %>% rownames_to_column()

#

preds <- cbind(
  
  pred = predict(mod, listings_data %>%
                   mutate(Car = paste0(Make, Model))),
  
  listings_data %>%
    mutate(Car = paste0(Make, Model))
  
) %>%
  
  mutate(vs_pred = Price - pred)
