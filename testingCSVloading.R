uicfile <- read.csv("uicHalsted.csv", fileEncoding = "UTF-8-BOM")

harefile <- read.csv("oHare.csv", fileEncoding = "UTF-8-BOM")

jacksonfile <- read.csv("jackson.csv", fileEncoding = "UTF-8-BOM")

uicfile$date_mod <- as.Date(uicfile$date_ymd)

ggplot(data=uicfile, aes(x = date_mod, y=rides)) + geom_bar(stat="identity", fill = "#098CF9") + ggtitle("a") + scale_x_date(date_breaks = "1 month") + labs(x = "Day", y = "Rides")
