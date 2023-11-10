###clean the dataframe
rm(list = ls())
cat("\f")
packages <- c('tidyverse', 'corrplot', 'magrittr', 'zoo', 'RColorBrewer', 'gridExtra', 'MASS', "readxl", "DataExplorer", "reshape2")
for (i in 1:length(packages)) {
  if (!packages[i] %in% rownames(installed.packages())) {
    install.packages(packages[i])
  }
  library(packages[i], character.only = TRUE)
}
rm(packages,i)
setwd("C:/Users/Palvin/OneDrive/Desktop")
var <- read_excel("VAR.xlsx", col_types = c("date", "numeric", "numeric", "numeric","numeric", "numeric",
                                            "numeric", "numeric", "numeric"))
var$inf_ex <- var$TIPS_10yr - var$TIPS_inf
var$TIPS_10yr <- NULL
var$TIPS_inf <- NULL
var$date <- as.Date(var$date)
start_date <- as.Date("2005-01-03")
end_date <- as.Date("2023-09-26")
date_vector <- seq(start_date, end_date, by = "day")
timeframe <- data.frame(date = date_vector)
rm(date_vector, end_date, start_date)
df <- merge(timeframe, var
            , by.x = "date", by.y = "date"
            , all.x = TRUE, all.y = FALSE)
plot_missing(df)
df <- na.locf(df)
cbind(lapply(lapply(df, is.na), sum))
df1 <- df %>% 
  mutate(price = lead(price, n = 90))
library(fpp3)
tsibble_df <- df1[df1$date <= "2023-05-25", ] %>% 
  as_tsibble(key = NULL, index = "date")
plot_missing(tsibble_df)
####dcmp###fixed
dcmp <- tsibble_df %>%
  model(stl = STL(price))
components(dcmp) %>% autoplot()
###model building
train <- tsibble_df %>% filter(year(date) <= "2017-04-11")
test <- df %>% filter(year(date) > "2017-04-11") %>% 
  as_tsibble(key = NULL, index = "date")
###different timezone
train <- df1[df1$date <= "2018-02-11", ] %>% 
  as_tsibble(key = NULL, index = "date")
test <- df1[df1$date > "2018-02-11", ] %>% 
  dplyr::select(-price) %>%
  as_tsibble(key = NULL, index = "date")
#test <- df1[df1$date > "2018-02-11" & df1$date < "2023-06-29", ] %>% 
 # as_tsibble(key = NULL, index = "date")
###ARIMA
fitarima <- train %>%
  model(ARIMA = ARIMA(price ~ dollar_index + gas_price + inf_ex + BrentOil + SP500 + storage))
fcarima <- fitarima %>% forecast(new_data = test)
###testing
autoplot(fcarima)
###TSLM
fittslm <- train %>%
  model(TSLM = TSLM(price ~ dollar_index + BrentOil + SP500 + gas_price + storage + inf_ex))
fctslm <- fittslm %>% forecast(new_data = test)
###testing
autoplot(fctslm)
###comparison
bind_rows(
  fcarima %>% accuracy(test),
  fctslm %>% accuracy(test))
###results
test_ontime <- df %>% filter(year(date) >= 2023) %>% 
  as_tsibble(key = NULL, index = "date")
fcarima %>% accuracy(test_ontime)
fctslm %>% accuracy(test_ontime)
fcarima %>% filter(year(date) >= 2023) %>%
  autoplot() +
  autolayer(test_ontime %>% filter(year(date) >= 2023), price, level = NULL)
fctslm %>% filter(year(date) >= 2023) %>%
  autoplot() +
  autolayer(test_ontime %>% filter(year(date) >= 2023), price, level = NULL)
gg_tsresiduals(fittslm)
gg_tsresiduals(fitarima)
write.csv(as.data.frame(fcarima), "results.csv", row.names = FALSE)
