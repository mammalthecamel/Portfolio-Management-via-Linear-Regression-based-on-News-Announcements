import pandas as pd
import matplotlib.pyplot as plt

#loading the price data for the 10 stocks and reshaping it so each stock is its own column
stock_prices = pd.read_csv('clean_stock_prices.csv', parse_dates=['date'])
stock_closes = stock_prices.pivot(index='date', columns='ticker', values='close').sort_index()

#simple returns not log returns on purpose - a portfolio return is the weighted sum of simple returns
returns = stock_closes.pct_change()

#we split the time period of the 2 years
#year 1 is a backtester where we test how long we should hold each asset after the news announcement and come up with the best results
#year 2 is where we use the optimal year 1 result and run it once to see how it performs against the equal weighted portfolio
year_1 = returns.loc["2024-09-16":"2025-09-15"]
year_2 = returns.loc["2025-09-16":"2026-09-15"]

#daily return of a portfolio - weight times return for each stock, added up
def portfolio_daily_returns(year_returns, year_weights):
    daily_returns = 0
    for ticker in year_returns:
        daily_returns += year_returns[ticker] * year_weights[ticker]
    return daily_returns

#turns the daily returns into one number for the whole year
def total_return(daily_returns):
    total = (daily_returns +1).prod()
    total -= 1
    return total

#equal weighted portfolio - 10% in every stock every day, this is what we have to beat
weights_year_2 = pd.DataFrame(0.1, index=year_2.index, columns=year_2.columns)
benchmark_year_2 = portfolio_daily_returns(year_2, weights_year_2)

#load the news data in, only keep the announcements with a big reaction
news_events = pd.read_csv('real_news_events.csv', parse_dates=['date'])
big_reaction = news_events['sar0'].abs() > 1.96
signals = news_events[big_reaction].copy()

#the rule - how much of a stock to hold after its announcement
def target_weight(reaction):
    if reaction <= -1.96 and reaction >= -2.58:
        return 0.05
    elif reaction <= -2.58:
        return 0
    elif reaction >= 1.96 and reaction <= 2.58:
        return 0.15
    elif reaction >= 2.58:
        return 0.2
    else:
        return 0.1

signals['weight'] = signals['sar0'].apply(target_weight)

#second strategy to compare against - trade on every real news event, just up or down
#good news goes to 15% and bad news goes to 5%, no matter how big the reaction was
def direction_weight(reaction):
    if reaction > 0:
        return 0.15
    else:
        return 0.05

all_news = news_events.copy()
all_news['weight'] = all_news['sar0'].apply(direction_weight)

#splitting the signals into year 1 and year 2
signals_year_1 = signals[signals['date'] <= '2025-09-15']
signals_year_2 = signals[signals['date'] >= '2025-09-16']
all_news_year_2 = all_news[all_news['date'] >= '2025-09-16']

#this function does the whole rule for one year
#start everything at 10%, then for each signal change that stock's weight for the next N days
#then whatever is left over gets split equally across the stocks with no signal so each day adds up to 100%
def rule_weights(year_returns, year_signals, holding_period):
    rule = pd.DataFrame(0.1, index=year_returns.index, columns=year_returns.columns)

    for row_label, event in year_signals.iterrows():
        value = rule.index.get_loc(event['date'])
        holding_days = rule.index[value + 1 : value + 1 + holding_period]
        rule.loc[holding_days, event['ticker']] = event['weight']

    #rebalancing - signal stocks keep their weight, the rest share whats left
    active = rule != 0.1
    signal_total = (rule * active).sum(axis=1)
    leftover = 1 - signal_total
    free_stocks = (rule == 0.1).sum(axis=1)
    balancing_weights = leftover / free_stocks
    rule = rule.where(active, balancing_weights, axis=0)
    return rule

#year 1 - try different holding periods and keep whichever made the most money
best_holding_period = 1
best_return = -1
for holding_period in [1, 5, 10]:
    rule_year_1 = rule_weights(year_1, signals_year_1, holding_period)
    rule_total_year_1 = total_return(portfolio_daily_returns(year_1, rule_year_1))
    print('year 1, hold', holding_period, 'days:', round(rule_total_year_1, 4))
    if rule_total_year_1 > best_return:
        best_return = rule_total_year_1
        best_holding_period = holding_period

print('best holding period:', best_holding_period)

#year 2 - run it once with the holding period year 1 picked, this is the real test
rule_returns_year_2 = portfolio_daily_returns(year_2, rule_weights(year_2, signals_year_2, best_holding_period))
all_news_returns_year_2 = portfolio_daily_returns(year_2, rule_weights(year_2, all_news_year_2, best_holding_period))

#measuring performance - 252 trading days in a year
def volatility(daily_returns):
    return daily_returns.std() * 252 ** 0.5

def sharpe(daily_returns, risk_free_rate):
    yearly_return = daily_returns.mean() * 252
    return (yearly_return - risk_free_rate) / volatility(daily_returns)

#biggest fall from a high point to a low point during the year
def max_drawdown(daily_returns):
    value = (daily_returns + 1).cumprod()
    highest = value.cummax()
    drop = value / highest - 1
    return drop.min()

#average bank rate over year 2
risk_free_year_2 = 0.03

#results table for year 2
year_2_results = pd.DataFrame({
    'equal weighted': benchmark_year_2,
    'news rule': rule_returns_year_2,
    'all news': all_news_returns_year_2,
})

for name in year_2_results:
    daily = year_2_results[name]
    print(name, '| return', round(total_return(daily), 4), '| volatility', round(volatility(daily), 4),
          '| sharpe', round(sharpe(daily, risk_free_year_2), 2), '| worst drop', round(max_drawdown(daily), 4))

#saving the daily returns so we can do the paired t-test in R
year_2_results.to_csv('portfolio_daily_year_2.csv')

#graph for 6.4 - what 1 pound turns into over year 2, with the final value written at the end of each line
plt.figure(figsize=(9, 5))
for name in year_2_results:
    value = (year_2_results[name] + 1).cumprod()
    plt.plot(value, label=name)
    plt.text(value.index[-1], value.iloc[-1], '  £' + str(round(value.iloc[-1], 3)), va='center')

plt.axhline(1, color='grey', linewidth=0.8, linestyle='--')
plt.ylabel('value of £1')
plt.title('Year 2: value of £1 invested on 16 September 2025')
plt.legend(loc='upper left')
plt.xlim(right=value.index[-1] + pd.Timedelta(days=45))
plt.tight_layout()
plt.savefig('portfolio_year_2.pdf')
plt.show()