# https://www.quantinsti.com/blog/forecasting-stock-returns-using-arima-model/
# https://github.com/daumann/r-stockPrediction
# http://colorado.rstudio.com:3939/commodities-quandl-flexdb/

require(readxl)
require(data.table)
require(plotly)

d <- data.table(readxl::read_excel(path = "Prueba_Técnica_Analítica.xlsx", sheet = 1))

# Gráfico del precio
plot_ly(data = d, x = ~fecha, y = ~open, color = I("black")) %>%
  add_markers(y = ~open, text = ~fecha, showlegend = FALSE) %>%
  add_lines(y = ~fitted(loess(open ~ as.numeric(fecha))),
            line = list(color = 'rgba(7, 164, 181, 1)'),
            name = "Loess Smoother")
