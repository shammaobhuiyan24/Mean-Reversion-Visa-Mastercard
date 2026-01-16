#Setup
install.packages("quantmod")
install.packages("urca")     
install.packages("TTR")      
install.packages("PerformanceAnalytics")
library(quantmod)
symbols <- c("V", "MA")
getSymbols(symbols, from = "2020-01-01", to = "2025-01-01")
v_prices <- Ad(V)
ma_prices <- Ad(MA)
pair_data <- merge(v_prices, ma_prices)
colnames(pair_data) <- c("Visa", "Mastercard")
pair_data <- na.omit(pair_data)
plot(pair_data$Visa, main = "Visa vs Mastercard", col="blue")
lines(pair_data$Mastercard, col="red")
model <- lm(Visa ~ Mastercard, data = pair_data)
beta <- coef(model)[2]
spread <- pair_data$Visa - beta * pair_data$Mastercard
plot(spread, main = "Spread", col = "purple", lwd = 1)
library(ggplot2)
ggplot(plot_df, aes(x = Date, y = Value)) +
  geom_line(color = "purple") +
  geom_hline(yintercept = mean(plot_df$Value), color = "red", linewidth = 1)
library(urca)
summary(ur.df(spread, type = "drift", selectlags = "BIC"))
z_score <- (spread - mean(spread)) / sd(spread)
z_df <- data.frame(
  Date = index(z_score),
  Z_Value = as.numeric(z_score)
)
ggplot(z_df, aes(x = Date, y = Z_Value)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 2, color = "red", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = -2, color = "green", linetype = "dashed", linewidth = 1) +
  labs(title = "Z-Score Signals") +
  theme_minimal()

#Backtesting
position <- ifelse(z_score < -2, 1, ifelse(z_score > 2, -1, 0))
position <- lag(position, 1)
spread_returns <- diff(spread) / abs(lag(spread, 1))
strategy_returns <- position * spread_returns
strategy_returns[is.na(strategy_returns)] <- 0
cumulative_returns <- cumprod(1 + strategy_returns) - 1
profit_df <- data.frame(
  Date = index(cumulative_returns),
  Profit = as.numeric(cumulative_returns)
)
ggplot(profit_df, aes(x = Date, y = Profit)) +
  geom_line(color = "orange", linewidth = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Returns via Mean Reversion",
       y = "Cumulative Return (%)") +
  theme_minimal()
total_return <- tail(cumulative_returns, 1)
sharpe <- sqrt(252) * mean(strategy_returns) / sd(strategy_returns)
print(paste("Total Strategy Return:", round(total_return * 100, 2), "%"))
print(paste("Annualised Sharpe Ratio:", round(sharpe, 2)))

