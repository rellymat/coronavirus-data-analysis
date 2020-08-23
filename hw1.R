library(data.table)
library(dplyr)
dt = fread("c://Users/Harel/corona_lab_tests_ver005.csv")
dt[dt == "חיובי"] <- "positive"
dt[dt == "שלילי"] <- "negative"
dt <- dt[!(dt$corona_result != "positive" & dt$corona_result != "negative")]
dt$test_date <- as.Date(dt$test_date,"%d/%m/%y")
dt$result_date <- as.Date(dt$result_date,"%d/%m/%y")
test_by_day = table(dt$test_date)
result_by_day = table(dt$result_date)
#plot(test_by_day, type = "p", main = "combine" ,col = "red", ylab = "tests_by_day")
#lines(result_by_day ,type = "p", col = "blue")
#legend('topleft', c("test_by_day","result_by_day"),col=c("red","blue"), pch = c(1,1))
labs_tests = table(dt$lab_id)
#plot(labs_tests)
dt = dt[, times_diff := difftime(result_date,test_date, units = "days")]
dt = dt[, time_average := mean(times_diff),by=lab_id]
#plot(dt$lab_id,dt$time_average, xlab = "lab_id", ylab = "average_tests", type = "p")
dt = dt[,average_date_result := mean(times_diff),by=test_date]
#plot(dt$test_date, dt$average_date_result, xlab = "test_date", ylab = "average_date_result")
dt = dt[,total_cases_by_date := sum(corona_result == "positive"), by=test_date]
dt = dt[,total_test_by_day := sum(corona_result != "r"), by=test_date]
dt = dt[,ratio := total_cases_by_date/total_test_by_day]
r = data.table(date = dt$test_date, ratio = dt$ratio)
r = unique(r)
plot(r$date,r$ratio,xlab = "test_date", ylab = "ratio")