#---Read csv, set tickers---
ftse <- read.csv("clean_benchmark_ftse100.csv")
ftse$date <- as.Date(ftse$date)
ftse <- ftse[order(ftse$date), ]
stock <- read.csv("clean_ftse100_subset.csv")
stock$date <- as.Date(stock$date)
stock <- stock[order(stock$ticker, stock$date), ]
macro <- read.csv("macro_events_audited_2.csv")
macro$announcement_date <- as.Date(macro$announcement_date, format = "%d/%m/%Y")
macro <- macro[order(macro$announcement_date), ]
ticker <- c("HSBA","SHEL","AZN","RR.","ULVR","RIO","BP.","JD.","AUTO","BRBY")
big_ticker <- c("HSBA","SHEL","AZN","RR.","ULVR","RIO","BP.")
small_ticker <- c("JD.","AUTO","BRBY")


#---Calculate returns in each day for ftse and each ticker---
returns <- data.frame(date = ftse$date[-1], market_return = diff(log(ftse$close)))
for (tk in ticker) {
  p <- merge(ftse[, "date", drop = FALSE], stock[stock$ticker == tk, c("date", "close")], by = "date", all.x = TRUE)
  p <- p[order(p$date), ]
  returns[[tk]] <- diff(log(p$close)) #A missing day stays NA, never a 2-day return
}
#---Identify day0---
dates <- returns$date
day0_of <- function(d) which(dates >= d)[1] #First trading day on/after the date

#---Building market model---
EST_LEN <- 120 #How many historical days to use when estimating company's normal relationship with ftse
GAP <-10 #Don't want the days surounding the announcement day influences the estimation of normal relationship


market_model <- function(tk, pos){
  if (is.na(pos) || pos - GAP - EST_LEN < 1) return(NULL)
  est <- (pos - GAP - EST_LEN):(pos - GAP - 1)
  y <- returns[[tk]][est] #Companies' returns
  x <- returns$market_return[est] #ftse returns
  ok <- !is.na(y) & !is.na(x); y <- y[ok]; x <- x[ok] #Remove missing values
  s <- summary(lm(y ~ x)) #Estimate model
  list(alpha = s$coefficients[1,1], beta = s$coefficients[2,1], sigma = s$sigma, sd_ma = sd(y - x))                    # sd of the old market-adjusted AR, for comparison
}

#---returns measurements of the company announcements---
event_measures <- function(tk, pos, a = -1, b = 1){
  f <- market_model(tk, pos)
  if (is.null(f) || pos + a < 1 || pos + b > length(dates)) return(NULL)
  win <- (pos + a):(pos + b); L <- length(win) #Identify dates and length of event window
  AR <- returns[[tk]] - (f$alpha + (f$beta * returns$market_return))  # market-model abnormal return
  data.frame(ticker = tk, date = dates[pos], pos = pos,
             alpha = f$alpha, beta = f$beta, sigma = f$sigma,
             AR_0  = AR[pos],
             SAR_0 = AR[pos] / f$sigma,
             CAR = sum(AR[win]),
             SCAR = sum(AR[win]) / (f$sigma * sqrt(L))
  )
}

#---Read company news---
company_news_full <- read.csv("News_all10_clean.csv")
for (i in 1:nrow(company_news_full)){
  if (company_news_full$company[i] == "AstraZeneca"){
    company_news_full$company[i] <- "AZN"
  } else if (company_news_full$company[i] == "HSBC") {
    company_news_full$company[i] <- "HSBA"
  } else if (company_news_full$company[i] == "Shell") {
    company_news_full$company[i] <- "SHEL"
  } else if (company_news_full$company[i] == "Rolls-Royce") {
    company_news_full$company[i] <- "RR."
  } else if (company_news_full$company[i] == "Unilever") {
    company_news_full$company[i] <- "ULVR"
  } else if(company_news_full$company[i] == "Auto Trader"){
    company_news_full$company[i] <- "AUTO"
  } else if (company_news_full$company[i] == "BP") {
    company_news_full$company[i] <- "BP."
  } else if (company_news_full$company[i] == "Burberry") {
    company_news_full$company[i] <- "BRBY"
  } else if (company_news_full$company[i] == "JD Sports") {
    company_news_full$company[i] <- "JD."
  } else if (company_news_full$company[i] == "Rio Tinto") {
    company_news_full$company[i] <- "RIO"
  }
}
names(company_news_full)[names(company_news_full) == "company"] <- "ticker"
company_news_full$date <- as.Date(company_news_full$date, format = "%d/%m/%Y")
company_news_full <- merge(company_news_full, stock, by=c("date","ticker"))
company_news_full <- company_news_full[!is.na(company_news_full$date) & !is.na(company_news_full$ticker) & company_news_full$date >= as.Date("2024-09-16") & company_news_full$date <= as.Date("2026-09-16"), ]
company_news_full$pos <- sapply(seq_len(nrow(company_news_full)), function(i) day0_of(company_news_full$date[i]))
company_news_full <- company_news_full[company_news_full$keep_for_tests == "TRUE",]

#---Events of interest---
event <- do.call(rbind, lapply(seq_len(nrow(company_news_full)), function(i) {
  r <- event_measures(company_news_full$ticker[i], company_news_full$pos[i])
  if (!is.null(r)) r$label <- company_news_full$label[i]
  if (!is.null(r)) r$event_type <- company_news_full$event_type[i]
  r
}))

#---Tidy macro data---
macro$announcement_date <- as.Date(macro$announcement_date, format = "%Y-%m-%d")
macro$pos <- sapply(seq_len(nrow(macro)), function(i) day0_of(macro$announcement_date[i]))
macro <- macro[macro$event_type != "" & !is.na(macro$pos), ] 
macro <- macro[!is.na(macro$announcement_date) & macro$announcement_date >= as.Date("2024-09-16") & macro$announcement_date <= as.Date("2026-09-16"), ]

#---Identify announcement periods and normal days---
news_key  <- unlist(lapply(seq_len(nrow(company_news_full)), function(i) paste(company_news_full$ticker[i], company_news_full$pos[i] + (-1:1))))
macro_pos <- unique(macro$pos)
normal <- list()
for (tk in unique(company_news_full$ticker)) {
  span <- range(company_news_full$pos[company_news_full$ticker == tk], na.rm = TRUE)
  for (p in max(span[1], GAP + EST_LEN + 2):min(span[2], length(dates) - 1)) {
    if (paste(tk, p) %in% news_key || p %in% macro_pos) next
    normal[[length(normal) + 1]] <- event_measures(tk, p)
  }
}
normal <- do.call(rbind, normal)

#===Macroeconomic news===
#---FTSE return on announcement and normal days---
ftse_SR <- function(pos, a = -1, b = 1) {
  if (is.na(pos) || pos - GAP - EST_LEN < 1 || pos + b > length(dates)) return(c(SR_0 = NA, SR_win = NA))
  est <- (pos - GAP - EST_LEN):(pos - GAP - 1)
  mu <- mean(returns$market_return[est]); s <- sd(returns$market_return[est]); win <- (pos + a):(pos + b)
  c(SR_0 = (returns$market_return[pos] - mu) / s, SR_win = (sum(returns$market_return[win]) - length(win) * mu) / (s * sqrt(length(win)))) #SR on event day and SCR for evenet window
}
mz <- t(sapply(macro$pos, ftse_SR)); macro$SR_0 <- mz[, "SR_0"]; macro$SR_win <- mz[, "SR_win"] #Apply ftse_SR to all macro announcement
non_macro <- setdiff((GAP + EST_LEN + 2):(length(dates) - 1), unlist(lapply(macro$pos, function(p) p + (-1:1)))) #Days not in macro
nz <- t(sapply(non_macro, ftse_SR))

#---Section 6.1.1---
print(wilcox.test(abs(macro$SR_0), abs(nz[, "SR_0"]), alternative = "greater")) #Compare ftse SR_0 on macro and non-macro announcement days

#---Section 6.1.2---
for (k in unique(macro$event_type)) {
  cat("\nFTSE on", k, "days\n")
  print(wilcox.test(abs(macro$SR_0[macro$event_type == k]), abs(nz[, "SR_0"]), alternative = "greater")) #Compare each macro event types to normal days
}

#---Section 6.1.3---
for (tk in ticker) {
  m <- do.call(rbind, lapply(macro_pos, function(p)
    if (!is.na(p) && !(paste(tk, p) %in% news_key)) event_measures(tk, p))) #Macro announcement days without company announcement
  print(nrow(m))
  n <- normal[normal$ticker == tk, ] #Days with no announcement
  print(nrow(n))
  if (is.null(m) || nrow(n) == 0) next #Skip if company has no macro announcement / normal days
  cat("\n", tk, "\n") #Print company names
  print(wilcox.test(abs(m$SAR_0), abs(n$SAR_0), alternative = "greater")) #Compare distribution of the SAR_0 for the 2 groups
}

#===Company news===
#---Section 6.2.1---
significant_return <- event$SCAR[event$label == "significant"]
moderate_return <- event$SCAR[event$label == "moderate"]
not_return <- event$SCAR[event$label == "not"]

event$label <- factor(event$label, levels= c("not","moderate","significant"), ordered = TRUE)
kruskal.test(abs(event$SCAR) ~ event$label)
pairwise.wilcox.test(abs(event$SCAR), event$label, p.adjust.method = "holm", alternative = "greater")

#---Section 6.2.2---
for (k in c("significant", "moderate", "not")) {
  cat("\n", k, "vs normal days\n")
  print(wilcox.test(abs(event$SCAR[event$label == k]), abs(normal$SCAR), alternative = "greater"))
}
