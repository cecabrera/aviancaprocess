# https://www.quantinsti.com/blog/forecasting-stock-returns-using-arima-model/
# https://github.com/daumann/r-stockPrediction
# http://colorado.rstudio.com:3939/commodities-quandl-flexdb/

require(readxl)
require(data.table)
require(plotly)

d <- data.table(readxl::read_excel(path = "Prueba_Técnica_Analítica.xlsx", sheet = 1))

# Gráfico del precio en plotly
p <- plot_ly(data = d, x = ~fecha, y = ~close, color = I("black")) %>%
  add_markers(y = ~close, text = ~fecha, showlegend = FALSE) %>%
  add_lines(y = ~fitted(loess(open ~ as.numeric(fecha))),
            line = list(color = 'rgba(7, 164, 181, 1)'),
            name = "Loess Smoother")

# Se calculan los retornos en función del lag (intervalo) que se quiera cortar. Es decir, 1 día antes, 2 días antes, etc.
lags = 1
d$returns <- c(rep(NA, lags), diff(d$open, lag = lags))

plotnprint <- function(x, y){
  print(adf.test(y))
  qplot(y = y, x = x, geom = "line")
}

plotnprint(x = d$fecha, y = d$close)
# Por resultado del Augmented Dickey-Fuller Test la serie no es estacional. A stationary time series means a time series without trend, one having a constant mean and variance over time, which makes it easy for predicting values. Tomado de: https://www.r-bloggers.com/forecasting-stock-returns-using-arima-model/
logical <- !is.na(d$returns)
plotnprint(x = d[logical, fecha], y = d[logical, returns])
# Ya es estacional por el p-valor inferior a cero.
rm(logical)

# Splitting the data
require(caTools)
set.seed(101) 
sample = sample.split(d$returns, SplitRatio = .80)
train = subset(returns, sample == TRUE)
test  = subset(returns, sample == FALSE)

plot(train, type = "l")
