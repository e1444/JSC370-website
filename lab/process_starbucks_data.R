# code from week 11 lab

library(tidyverse)
library(plotly)
library(widgetframe)
library(tidytext)

### load Starbucks and state-level data ###
sb_locs <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/starbucks-locations.csv")

sb_nutr <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/starbucks-menu-nutrition.csv")

usa_pop <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/us_state_pop.csv")

usa_states <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/states.csv")


### Merge data ###
sb_usa <- sb_locs |> filter(Country == "US")

sb_locs_state <- sb_usa |>
  group_by(State = `State/Province`) |>
  summarize(count = n())

# need state abbreviations
usa_pop_abbr <- full_join(usa_pop, usa_states, by = c("state" = "State")) |>
  select(-state)

sb_locs_state <- full_join(sb_locs_state, usa_pop_abbr, by = c("State" = "Abbreviation"))


### Get topwords from menu items
sb_nutr_words <- sb_nutr |>
  unnest_tokens(word, Item) |>
  anti_join(stop_words) |>
  count(word, sort = TRUE) |>
  head(20)

items_top10 <- sb_nutr |>
  mutate(Item = tolower(Item)) |>
  filter(str_detect(Item, paste(head(sb_nutr_words$word, 10), collapse = "|")))

topwords <- sb_nutr |>
  unnest_tokens(word, Item, token="words") |>
  group_by(word) |>
  summarise(word_frequency = n()) |>
  arrange(across(word_frequency, desc)) |>
  head(10)

p1 <- plot_ly(
  data = items_top10,
  x = ~Calories,
  y = ~`Carb. (g)`,
  z = ~`Protein (g)`,
  color = ~Category,
  text = ~Item,  # Hover info
  type = "scatter3d",
  mode = "markers"
) %>%
  layout(
    title = "3D Scatterplot of Calories, Carbs, and Protein",
    scene = list(
      xaxis = list(title = "Calories"),
      yaxis = list(title = "Carbs (g)"),
      zaxis = list(title = "Protein (g)")
    )
  )

# Set up mapping details
set_map_details <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('steelblue')
)

# Create hover text
sb_locs_state$hover <- with(sb_locs_state, paste("Number of Starbucks: ", count, '<br>', "State: ", State, '<br>', "Population: ", population))

# Create the map
map1 <- plot_geo(sb_locs_state, locationmode = "USA-states") |>
  add_trace(
    z = ~count,
    locations = ~State,
    text = ~hover,
    hoverinfo = "text",
    colorscale = list(c(0, "lightblue"), c(1, "darkblue")),
    zmin = 0, zmax = 2000
  ) |>
  layout(
    title = "Number of Starbucks Stores by State",
    geo = set_map_details
  )

map2 <- plot_geo(sb_locs_state, locationmode = "USA-states") |>
  add_trace(
    z = ~population,
    locations = ~State,
    text = ~hover,
    hoverinfo = "text",
    colorscale = list(c(0, "pink"), c(1, "darkred")),
    zmin = 0, zmax = 37253956
  ) |>
  layout(
    title = "Starbucks per State (Left) vs. Population by State (Right).",
    geo = set_map_details
  )

p2 <- subplot(map1, map2)
