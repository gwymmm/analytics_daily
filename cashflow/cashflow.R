library("lubridate")
library("stringr")
# install.packages("stringr")

args <- commandArgs(TRUE)

file_name <- args[1]
income_base <- as.double(args[2])

raw_data <- read.csv(file_name, sep=";", dec=',')

needed_data <- raw_data[, c(2, 4, 12, 13, 15)]

colnames(needed_data) <- c("Event_Date", "Event_Description", "Trading_Party", "IBAN_of_Party", "Amount")

# remove dummy flows (where amount == 0)
needed_data <- needed_data[needed_data$Amount != 0.0, ]

no_iban_there <- needed_data$IBAN_of_Party=="0000000000"
needed_data$Trading_Party[no_iban_there] <- "None"

unique_IBANs <- unique(needed_data$IBAN_of_Party)

for (i in unique_IBANs) {

  positions <- which(needed_data$IBAN_of_Party == i)
  first_encounter <- needed_data$Trading_Party[ positions[1] ]
  needed_data$Trading_Party[positions] <- first_encounter

}


dates <- as.Date(needed_data$Event_Date, "%d.%m.%Y")

needed_data$Event_Date_Month <- month(dates)
needed_data$Event_Date_Year <- year(dates)


# https://r-coder.com/aggregate-r/
agg <- aggregate(Amount ~ Event_Date_Year + Event_Date_Month + Event_Description + Trading_Party + IBAN_of_Party, data = needed_data, FUN = sum)

result <- agg[order(agg$Event_Date_Year, agg$Event_Date_Month), ]

monthly_result <- aggregate(Amount ~ Event_Date_Year + Event_Date_Month, data = result, FUN = sum)

per_party_result <- aggregate(Amount ~ Trading_Party, data = needed_data, FUN = sum)


cat("-- Cashflow per Party --\n\n")

first <- str_pad("Party", 40, "left")
second <- str_pad("Amount", 10, "left")
third <- str_pad("Ratio", 10, "left")

Sep <- paste(strrep("-",80), "\n")

cat(first, second, third, "\n")

cat(Sep)

for (i in 1:nrow(per_party_result) ) {
  first <- str_pad(str_trunc(per_party_result$Trading_Party[i], 40, "right"), 40, "left")
  second <- str_pad(as.character(per_party_result$Amount[i]), 10, "left")
  third <- str_pad(as.character(round(per_party_result$Amount[i] / income_base, 2)), 10, "left")
  cat(first, second, third,  "\n")
}

first <- str_pad("Sum", 40, "left")
second <- str_pad(as.character(sum(per_party_result$Amount)), 10, "left")

cat(Sep)

cat(first, second, "\n")


cat("-- Cashflow per Month --\n")

first_header <- str_pad("Party", 40, "left")
second_header <- str_pad("Description", 25, "left")
third_header <- str_pad("Amount", 10, "left")
fourth_header <- str_pad("Ratio", 10, "left")

current_month <- 0
current_year <- 0

Sep <- paste(strrep("-",100), "\n")

for (i in 1:nrow(result)) {

  if (result$Event_Date_Year[i] > current_year) {
    current_year <- result$Event_Date_Year[i]
    cat("\n-- Year", current_year, "--\n")

    current_month <- result$Event_Date_Month[i]
    cat("\n-- Month", current_month, "--\n")
    sum_string <- str_pad("Sum", 66, "left")
    filter <- monthly_result$Event_Date_Year==current_year & monthly_result$Event_Date_Month==current_month
    sum_amount <- str_pad(as.character(monthly_result[filter, "Amount"]), 10, "left")
    cat(sum_string, sum_amount, "\n")
    cat(Sep)
    cat(first_header, second_header, third_header, fourth_header, "\n")
    cat(Sep) 

  }

  if (result$Event_Date_Month[i] > current_month) {
    current_month <- result$Event_Date_Month[i]
    cat("\n-- Month", current_month, "--\n")
    sum_string <- str_pad("Sum", 66, "left")
    filter <- monthly_result$Event_Date_Year==current_year & monthly_result$Event_Date_Month==current_month
    sum_amount <- str_pad(as.character(monthly_result[filter, "Amount"]), 10, "left")
    cat(sum_string, sum_amount, "\n")
    cat(Sep)
    cat(first_header, second_header, third_header, fourth_header, "\n")
    cat(Sep) 
  }

  first <- str_pad(str_trunc(result$Trading_Party[i], 40, "right"), 40, "left")
  second <- str_pad(as.character(result$Event_Description[i]), 25, "left")
  third <- str_pad(as.character(result$Amount[i]), 10, "left")
  fourth <- str_pad(as.character(round(result$Amount[i] / income_base, 3)), 10, "left")

  cat(first, second, third, fourth, "\n")

}

cat("\n")
cat(Sep)
cat("Overall sum:", sum(result$Amount), "\n")
cat("Monthly average:", mean(monthly_result$Amount), "\n")
cat(Sep)