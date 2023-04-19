# Set up the data

rankings <- c('Unknown', 'Unlikely', 'Limited', 'Potential', 'Possible', 'Likely')

menuData <- read.csv("data-raw/menu-data.csv") %>%
  dplyr::mutate(
    alcohol = factor(alcohol, levels=0:5, labels=rankings),
    tobacco = factor(tobacco, levels=0:5, labels=rankings),
    cannabis = factor(cannabis, levels=0:5, labels=rankings),
    other = factor(other, levels=0:5, labels=rankings),
    alcoholUse = factor(alcoholUse, levels=0:5, labels=rankings),
    alcoholBehav = factor(alcoholBehav, levels=0:5, labels=rankings),
    alcoholHarm = factor(alcoholHarm, levels=0:5, labels=rankings),
    tobaccoUse = factor(tobaccoUse, levels=0:5, labels=rankings),
    tobaccoBehav = factor(tobaccoBehav, levels=0:5, labels=rankings),
    tobaccoQuit = factor(tobaccoQuit, levels=0:5, labels=rankings),
    cannabisUse = factor(cannabisUse, levels=0:5, labels=rankings),
    cannabisBehav = factor(cannabisBehav, levels=0:5, labels=rankings)
  )

usethis::use_data(menuData)
