company_news <- read.csv("News_all10_clean.csv")
macro_news <- read.csv("macro_events_audited.csv")

news_test <- company_news[company_news$keep_for_tests, ]

#data for ftse with benchmark data loaded in from 2/1/24
#loads in our data for each company over the given time period
stock_prices <- read.csv("clean_stock_prices.csv")
#benchmark data of how the ftse has moved over the day
ftse_benchmark <- read.csv("clean_benchmark_ftse100.csv")

#turns dates in the excel into real dates
stock_prices$date <- as.Date(stock_prices$date)
ftse_benchmark$date <- as.Date(ftse_benchmark$date)

#calculating log return of the ftse benchmark data
ftse_benchmark$mkt_ret <- c(NA, diff(log(ftse_benchmark$close)))

#check relevant info andmaps ticker with date
stock_prices <- stock_prices[order(stock_prices$ticker, stock_prices$date), ]
#daily log returns for each company
stock_prices$ret <- ave(stock_prices$close, stock_prices$ticker, FUN = function(x) c(NA, diff(log(x))))

#match company returns with ftse returns to compare them by date
returns <- merge(stock_prices, ftse_benchmark[, c("date", "mkt_ret")], by = "date")
returns <- returns[order(returns$ticker, returns$date), ]

#adjusting data so it follows the same date format
news_test$date <- as.Date(news_test$date, format = "%d/%m/%Y")

#look up and match company name to ticker
ticker_lookup <- c("AstraZeneca" = "AZN",
                   "HSBC" = "HSBA",
                   "Rolls-Royce" = "RR.",
                   "Unilever" = "ULVR",
                   "Auto Trader" = "AUTO",
                   "BP" ="BP.",
                   "Burberry" = "BRBY",
                   "JD Sports" = "JD.",
                   "Rio Tinto" = "RIO",
                   "Shell" = "SHEL")

news_test$ticker <- ticker_lookup[news_test$company]

#function of the previous event that can be reused for all companies and are defining day0 sar
#obtain alpa and beta on days -130 until -11 to offshore any volatility caused by news pricing in
event_sar <- function(tkr, dt) {
  stock <- returns[returns$ticker == tkr, ]
  day0  <- which(stock$date == dt)
  if (length(day0) == 0 || day0 <= 130) return(NA)
  est   <- stock[(day0 - 130):(day0 - 11), ]
  fit   <- lm(ret ~ mkt_ret, data = est)
  alpha <- unname(coef(fit)[1])
  beta  <- unname(coef(fit)[2])
  ar0   <- stock$ret[day0] - (alpha + beta * stock$mkt_ret[day0])
  ar0 / sigma(fit)
}

#loop for all 565 events were using
news_test$sar0 <- NA

for (i in 1:nrow(news_test)) {
  news_test$sar0[i] <- event_sar(news_test$ticker[i], news_test$date[i])
}

stopifnot(sum(is.na(news_test$sar0)) == 0)

#====6.2.5 do unscheduled announcements move prices more than scheduled ones ?====
sched_news <- news_test[!(news_test$event_type %in% c("routine", "dividend_buyback")), ]
sched_news$scheduled <- sched_news$event_type %in% c("results", "trading_update")

#testing whether the gap in true and false (94 vs 223) is real or just a chance ?
t.test(abs(sar0) ~ scheduled, data = sched_news)
wilcox.test(abs(sar0) ~ scheduled, data = sched_news)

#testing whether astra zeneca is influencing all of the data because of its massive amounts of news announcements
sched_news_noazn <- sched_news[sched_news$ticker != "AZN", ]
t.test(abs(sar0) ~ scheduled, data = sched_news_noazn)

#this piece here works for every piece of window with a from and to inputs to test whether scheduled announcements are more significant than unscheduled
event_scar <- function(tkr, dt, from, to) {
  stock <- returns[returns$ticker == tkr, ]
  day0  <- which(stock$date == dt)
  if (length(day0) == 0 || day0 <= 130 || day0 + to > nrow(stock)) return(NA)
  est   <- stock[(day0 - 130):(day0 - 11), ]
  fit   <- lm(ret ~ mkt_ret, data = est)
  win   <- stock[(day0 + from):(day0 + to), ]
  ar    <- win$ret - (coef(fit)[1] + coef(fit)[2] * win$mkt_ret)
  sum(ar) / (sigma(fit) * sqrt(length(ar)))
}

#the next block loops the same for the next week to see if results still hold
sched_news$scar01 <- NA
sched_news$scar05 <- NA
for (i in 1:nrow(sched_news)) {
  sched_news$scar01[i] <- event_scar(sched_news$ticker[i], sched_news$date[i], 0, 1)
  sched_news$scar05[i] <- event_scar(sched_news$ticker[i], sched_news$date[i], 0, 5)
}
t.test(abs(scar01) ~ scheduled, data = sched_news)
t.test(abs(scar05) ~ scheduled, data = sched_news)

#printiout table for 6.2.5 to paste in the report
sched_table <- data.frame(
  window      = c("Day 0", "Days 0 to +1", "Days 0 to +5", "Day 0, excl. AZN"),
  unscheduled = c(0.97, 0.95, 0.93, 1.10),
  scheduled   = c(2.33, 1.83, 1.32, 2.36),
  t_stat      = c(-6.01, -5.06, -3.16, -4.98),
  p_value     = c("<0.001", "<0.001", "0.002", "<0.001")
)
sched_table

# ==== 6.2.3 is news priced in before announcement day ? ====
news_test$scar_pre5  <- NA
news_test$scar_pre10 <- NA
for (i in 1:nrow(news_test)) {
  news_test$scar_pre5[i]  <- event_scar(news_test$ticker[i], news_test$date[i], -5, -1)
  news_test$scar_pre10[i] <- event_scar(news_test$ticker[i], news_test$date[i], -10, -1)
}
pre_news <- news_test[!(news_test$event_type %in% c("routine", "dividend_buyback")), ]
pre_news$scheduled <- pre_news$event_type %in% c("results", "trading_update")
pre_news$dir5  <- pre_news$scar_pre5  * sign(pre_news$sar0)
pre_news$dir10 <- pre_news$scar_pre10 * sign(pre_news$sar0)

t.test(pre_news$dir5, mu = 0)
t.test(pre_news$dir10, mu = 0)

t.test(pre_news$dir5[pre_news$scheduled], mu = 0)
t.test(pre_news$dir5[!pre_news$scheduled], mu = 0)


# ==== 6.2.4 do the 3 smaller stocks react more than the 7 big stocks ? ====
small_tickers <- c("AUTO", "BRBY", "JD.")

size_news <- news_test[!(news_test$event_type %in% c("routine", "dividend_buyback")), ]
size_news$small <- size_news$ticker %in% small_tickers

tapply(abs(size_news$sar0), size_news$small, mean)

t.test(abs(sar0) ~ small, data = size_news)
wilcox.test(abs(sar0) ~ small, data = size_news)

# is the result really about company size or is it about the kind of news each group happens to have
table(size_news$small, size_news$event_type)

size_news$scheduled <- size_news$event_type %in% c("results", "trading_update")
table(size_news$small, size_news$scheduled)

#same test but only on results and trading updates, so both groups have the same kind of news
t.test(abs(sar0) ~ small, data = size_news[size_news$scheduled, ])

#same test without astra zeneca
size_news_noazn <- size_news[size_news$ticker != "AZN", ]
t.test(abs(sar0) ~ small, data = size_news_noazn)


#==== graph plotting for each hypothesis to show results ====
# figure 1: plots for 6.2.3 and 6.2.5 in days -10 until 5 from announcement day
event_days <- -10:5

daily_sar <- matrix(NA, nrow = nrow(sched_news), ncol = length(event_days))

for (event_number in 1:nrow(sched_news)) {
  for (day_number in 1:length(event_days)) {
    daily_sar[event_number, day_number] <- event_scar(
      sched_news$ticker[event_number],
      sched_news$date[event_number],
      event_days[day_number],
      event_days[day_number]
    )
  }
}

signed_sar <- daily_sar * sign(sched_news$sar0)

cumulative_sar <- t(apply(signed_sar, 1, cumsum))

mean_path_scheduled   <- colMeans(cumulative_sar[sched_news$scheduled, ])
mean_path_unscheduled <- colMeans(cumulative_sar[!sched_news$scheduled, ])


plot(event_days, mean_path_scheduled,
     type = "l", lwd = 2, col = "darkblue",
     ylim = range(c(mean_path_scheduled, mean_path_unscheduled)),
     xlab = "Trading days relative to announcement",
     ylab = "Mean cumulative standardised abnormal return",
     main = "Price path around company announcements")

lines(event_days, mean_path_unscheduled, lwd = 2, lty = 2, col = "darkorange")

abline(h = 0, col = "grey70")
abline(v = 0, lty = 3, col = "grey40")

legend("topleft",
       legend = c("Scheduled (n = 94)", "Unscheduled (n = 223)"),
       col = c("darkblue", "darkorange"), lty = c(1, 2), lwd = 2, bty = "n")


# figure 2: boxplot for 6.2.4 day-0 reaction of small vs big stocks
boxplot(abs(sar0) ~ small, data = size_news,
        names = c("Big stocks (n = 282)", "Small stocks (n = 35)"),
        col = c("lightblue", "orange"),
        xlab = "",
        ylab = "Absolute standardised abnormal return on day 0")

group_means <- tapply(abs(size_news$sar0), size_news$small, mean)

points(1:2, group_means, pch = 18, cex = 1.8)


#azn test to refer to in the report as although results are most in terms on number results are not skewed
round(100 * mean(sched_news$ticker == "AZN"))

pre_news_noazn <- pre_news[pre_news$ticker != "AZN", ]
t.test(pre_news_noazn$dir5)


# ---- 6.1.4 Do single stocks move more on macro days, once the FTSE is removed? ----

# 1. one row per stock per day, with the stock's return and the FTSE's return
all_days <- merge(stock_prices[, c("ticker", "date", "ret")],
                  ftse_benchmark[, c("date", "mkt_ret")], by = "date")
all_days <- all_days[order(all_days$ticker, all_days$date), ]
all_days$sar <- NA

# 2. standardised abnormal return for EVERY stock-day (same market model as the events)
for (this_ticker in unique(all_days$ticker)) {
  rows <- which(all_days$ticker == this_ticker)
  for (k in 131:length(rows)) {
    estimation_window <- all_days[rows[(k - 130):(k - 11)], ]
    market_model <- lm(ret ~ mkt_ret, data = estimation_window)
    expected_ret <- predict(market_model, newdata = all_days[rows[k], ])
    all_days$sar[rows[k]] <- (all_days$ret[rows[k]] - expected_ret) / sigma(market_model)
  }
}

# 3a. helper: turn a column into real dates, whether it is 18/09/2024 or 2024-09-18
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  x <- as.character(x)
  d <- as.Date(x, format = "%d/%m/%Y")
  if (mean(!is.na(d)) < 0.9) d <- as.Date(substr(x, 1, 10), format = "%Y-%m-%d")
  d
}

# 3b. find which column of macro_news holds the dates, and print its name
is_date_column <- sapply(macro_news, function(column) mean(!is.na(to_date(column))) > 0.9)
macro_date_column <- names(macro_news)[is_date_column][1]
macro_dates <- to_date(macro_news[[macro_date_column]])

# 3c. label each stock-day
all_days$macro_day <- all_days$date %in% macro_dates
all_days$company_news_day <- paste(all_days$ticker, all_days$date) %in%
  paste(company_news$ticker, to_date(company_news$date))

# 4. keep the study period, drop days where that stock had its own news
test_days <- all_days[!is.na(all_days$sar) & !all_days$company_news_day &
                        all_days$date >= as.Date("2024-09-16") &
                        all_days$date <= as.Date("2026-09-15"), ]

macro_abs_sar <- abs(test_days$sar[test_days$macro_day])
quiet_abs_sar <- abs(test_days$sar[!test_days$macro_day])

# 5. sizes, means, tests
mean(macro_abs_sar);   mean(quiet_abs_sar)
t.test(macro_abs_sar, quiet_abs_sar)
wilcox.test(macro_abs_sar, quiet_abs_sar)


# ==== 6.3 volatility ====
#how far the price travelled during the day high compared to low
stock_prices$daily_range <- log(stock_prices$high / stock_prices$low)

#what a normal day looks like for each company
stock_prices$typical_range <- ave(stock_prices$daily_range, stock_prices$ticker, FUN = median)

#the day compared to that companys normal day
stock_prices$relative_range <- stock_prices$daily_range / stock_prices$typical_range

#measure wick for smaller price changes
#how far the high pokes above the open and close
stock_prices$upper_wick <- log(stock_prices$high / pmax(stock_prices$open, stock_prices$close))

#how far the low pokes below the open and close
stock_prices$lower_wick <- log(pmin(stock_prices$open, stock_prices$close) / stock_prices$low)

#the longer of the two
stock_prices$biggest_wick <- pmax(stock_prices$upper_wick, stock_prices$lower_wick)

#count the long-wick days in each month
table(format(stock_prices$date[stock_prices$biggest_wick > 0.05], "%Y-%m"))


#we need to set a rule for volatility testing
#we treat hihg/low as unreliable if either sits >5% beyond both open and close
#these days will be removed from the volatility test
#limitation of access of free data and works against us since its mainly in april-july 2026; with a few wild days in april 2025
#which holds some significant news - cant be inflating returns

#mark days where the high or low cannot be trusted
stock_prices$range_ok <- stock_prices$biggest_wick <= 0.05

#blank out the range on those days
stock_prices$daily_range[!stock_prices$range_ok] <- NA

#rebuild the normal day and the relative range without them
stock_prices$typical_range <- ave(stock_prices$daily_range, stock_prices$ticker,
                                  FUN = function(x) median(x, na.rm = TRUE))
stock_prices$relative_range <- stock_prices$daily_range / stock_prices$typical_range


#attach measure to each news event
news_test$relative_range <- NA

for (i in 1:nrow(news_test)) {
  price_row <- which(stock_prices$ticker == news_test$ticker[i] &
                       stock_prices$date   == news_test$date[i])
  news_test$relative_range[i] <- stock_prices$relative_range[price_row]
}

#6 days are left out of the report hence only testing on 559 news articles
#real news days against routine filing days
vol_news <- news_test[news_test$event_type != "dividend_buyback", ]
vol_news$real_news <- vol_news$event_type != "routine"

tapply(vol_news$relative_range, vol_news$real_news, mean, na.rm = TRUE)

t.test(relative_range ~ real_news, data = vol_news)
wilcox.test(relative_range ~ real_news, data = vol_news)

#same test without astra zeneca
vol_news_noazn <- vol_news[vol_news$ticker != "AZN", ]
t.test(relative_range ~ real_news, data = vol_news_noazn)

#same test without the faulty months april to july 2026
faulty_months <- vol_news$date >= as.Date("2026-04-01") & vol_news$date <= as.Date("2026-07-31")
t.test(relative_range ~ real_news, data = vol_news[!faulty_months, ])

#change the code to match cherrys
#give all 799 announcements a ticker and a real date (same as we did for news_test)
company_news$ticker   <- ticker_lookup[company_news$company]
company_news$news_day <- as.Date(company_news$date, format = "%d/%m/%Y")

#one label per stock and day, eg "AZN 2024-09-18"
any_news_key  <- paste(company_news$ticker, company_news$news_day)
real_news_key <- paste(vol_news$ticker[vol_news$real_news], vol_news$date[vol_news$real_news])
stock_prices$stock_day <- paste(stock_prices$ticker, stock_prices$date)

#sort every stock-day into one of three types
stock_prices$day_type <- "no news"
stock_prices$day_type[stock_prices$stock_day %in% any_news_key]  <- "other announcement"
stock_prices$day_type[stock_prices$stock_day %in% real_news_key] <- "real news"

#only count days inside the period we collected news for
stock_prices$in_news_period <- stock_prices$date >= min(company_news$news_day) &
  stock_prices$date <= max(company_news$news_day)

#real news days against no news days
vol_days <- stock_prices[stock_prices$in_news_period &
                           stock_prices$day_type != "other announcement", ]
vol_days$real_news <- vol_days$day_type == "real news"

tapply(vol_days$relative_range, vol_days$real_news, mean, na.rm = TRUE)

t.test(relative_range ~ real_news, data = vol_days)
wilcox.test(relative_range ~ real_news, data = vol_days)


#same test without astra zeneca
t.test(relative_range ~ real_news, data = vol_days[vol_days$ticker != "AZN", ])

#same test without the faulty months april to july 2026
faulty_days <- vol_days$date >= as.Date("2026-04-01") & vol_days$date <= as.Date("2026-07-31")
t.test(relative_range ~ real_news, data = vol_days[!faulty_days, ])

#does the bumpiness last? mark the 1 to 5 trading days after each real news day
news_rows <- which(stock_prices$day_type == "real news")
stock_prices$days_after_news <- NA

for (days_later in 5:1) {
  later_rows <- news_rows + days_later
  later_rows <- later_rows[later_rows <= nrow(stock_prices)]
  later_rows <- later_rows[stock_prices$ticker[later_rows] == stock_prices$ticker[later_rows - days_later]]
  stock_prices$days_after_news[later_rows] <- days_later
}

#compare quiet days just after news with all other quiet days
quiet_days <- stock_prices[stock_prices$in_news_period & stock_prices$day_type == "no news", ]
quiet_days$after_news <- !is.na(quiet_days$days_after_news)

t.test(relative_range ~ after_news, data = quiet_days)

# figure 3: volatility on the news day and the 5 days after for 6.3
range_news_day  <- mean(vol_days$relative_range[vol_days$real_news], na.rm = TRUE)
range_days_after <- tapply(quiet_days$relative_range, quiet_days$days_after_news, mean, na.rm = TRUE)
range_far_from_news <- mean(quiet_days$relative_range[!quiet_days$after_news], na.rm = TRUE)

bar_heights <- c(range_news_day, range_days_after)

barplot(bar_heights,
        names.arg = c("News day", "+1", "+2", "+3", "+4", "+5"),
        col = c("darkorange", rep("lightblue", 5)),
        ylim = c(0, 2),
        xlab = "Trading days after a real-news announcement",
        ylab = "Mean relative range (1 = typical day)")

abline(h = range_far_from_news, lty = 2)

legend("topright",
       legend = "No-news days away from announcements",
       lty = 2, bty = "n")

#export the 317 real news events for the python portfolio code
write.csv(sched_news, "real_news_events.csv", row.names = FALSE)


#im setting portfolio settings where we drop weight of stocks below 5% at an above sar > 1.96 we calculated earlier and checking how many events we have to do that
big_reaction <- abs(sched_news$sar0) > 1.96
sum(big_reaction)
table(sign(sched_news$sar0[big_reaction]), sched_news$date[big_reaction] < as.Date("2025-09-16"))

strong_reaction <- abs(sched_news$sar0) > 2.58
table(sign(sched_news$sar0[big_reaction]), strong_reaction[big_reaction])


#paired t-test - does the news rule beat the equal weighted portfolio day by day in year 2
portfolio <- read.csv("portfolio_daily_year_2.csv")
t.test(portfolio$news.rule, portfolio$equal.weighted, paired = TRUE)
