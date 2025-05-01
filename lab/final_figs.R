library(data.table)
library(ggplot2)
library(plotly)
library(widgetframe)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)
library(httr)
library(jsonlite)
library(data.table)
library(lubridate)
library(MASS)
library(sn)
library(mgcv)
library(patchwork)
library(plotly)
library(broom.mixed)
library(caret)

dt <- fread("https://raw.githubusercontent.com/e1444/JSC370_project/refs/heads/main/data/cleaned.csv")

dt_gam <- dt[, .(datetime_gmt, longitude, latitude, sample_measurement, outdoor_temperature, relative_humidity, wind_speed)]
dt_gam[, hour := hour(datetime_gmt)]
dt_gam[, dow := wday(datetime_gmt)]
dt_gam[, doy := yday(datetime_gmt)]
dt_gam[, time_num := as.numeric(difftime(datetime_gmt, min(datetime_gmt), units = "hours"))]

gam <- bam(sample_measurement ~
    s(longitude, latitude) +
    s(hour, bs = "cc", k = 24) +
    s(dow, bs = "cc", k = 7) +
    s(time_num, bs = "cc", k = 365) +
    s(time_num, bs = "cs", k = 100) +
    s(outdoor_temperature) +
    s(relative_humidity) +
    s(wind_speed),
    data = dt_gam,
    method = "fREML",
    discrete = TRUE
)

p1 <- plot_geo(
    data = dt_gam,
    lat = ~latitude,
    lon = ~longitude,
    color = ~sample_measurement,
    colors = "YlOrRd",
    size = ~sample_measurement,
    text = ~paste("Sample Measurement:", sample_measurement),
    hoverinfo = "text"
) %>%
    layout(
        title = "Sample Measurement by Location",
        geo = list(
            scope = "usa",
            showland = TRUE
        ),
        annotations = list(
            list(
                text = "Figure 1: Sample measurement by location. We can observe that there are only a couple sites (and locations) that collect sample measurements."
            )
        )
    )

dt_p2 <- dt_gam[, .(datetime_gmt, sample_measurement)]
dt_p2[, month := month(datetime_gmt)]

p2 <- plot_ly(
    data = dt_p2,
    x = ~month,
    y = ~sample_measurement,
    type = "box",
    marker = list(color = "rgba(0, 0, 0, 0.5)")
) %>%
    layout(
        title = "Sample Measurement by Month",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Sample Measurement"),
        annotations = list(
            list(
                text = "Figure 2: Sample measurement by month. We can observe that there are only a couple sites (and locations) that collect sample measurements."
            )
        )
    )


counties <- unique(dt$county)

# Create a list of histogram traces (one per county)
hist_traces <- lapply(counties, function(cnty) {
  list(
    x = dt %>% filter(county == cnty) %>% pull(sample_measurement),
    type = "histogram",
    name = cnty,
    visible = FALSE
  )
})

# Set first county to visible
hist_traces[[1]]$visible <- TRUE

# Create dropdown buttons to toggle each histogram
dropdown_buttons <- lapply(seq_along(counties), function(i) {
  vis <- rep(FALSE, length(counties))
  vis[i] <- TRUE
  list(
    method = "restyle",
    args = list("visible", vis),
    label = counties[i]
  )
})

p3 <- plot_ly()

# Add all traces correctly by unpacking each list of trace properties
for (trace in hist_traces) {
  p3 <- do.call(add_trace, c(list(p), trace))
}

# Apply layout and dropdown
p3 <- layout(p,
  title = "Sample Measurement Distribution by County",
  xaxis = list(title = "Sample Measurement"),
  yaxis = list(title = "Count"),
  updatemenus = list(
    list(
      buttons = dropdown_buttons,
      direction = "down",
      showactive = TRUE,
      x = 1.1,
      y = 1,
      xanchor = "left",
      yanchor = "top"
    )
  )
)

p3
