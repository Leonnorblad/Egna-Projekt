
#install.packages("quantmod")
#install.packages("priceR")
library(priceR)
library(quantmod)
library(lubridate)

start_date <- ymd('2020-01-01')
end_date <- today()

getSymbols("TSLA", from = start_date,
           to = end_date,warnings = FALSE,
           auto.assign = TRUE)
close_price <- TSLA$TSLA.Close
dollar_price <- historical_exchange_rates(start_date = start_date,
                          end_date = end_date,
                          from = "USD",
                          to = "SEK")


tail(close_price)
tail(dollar_price)
sek_price <- close_price$TSLA.Close*dollar_price$one_USD_equivalent_to_x_SEK

