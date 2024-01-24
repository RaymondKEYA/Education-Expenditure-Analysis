library(tidyverse)
library(readxl)
library(plotly)

# function to join two data frames
join_df <- function(df1_path, df2_path, by) {
  # load excel file and convert to data frame
  df1 <- readxl::read_excel(df1_path) %>%  # read excel file
    as_tibble() # convert to tibble
  df2 <- readxl::read_excel(df2_path) %>%  # read excel file
    as_tibble() # convert to tibble
  # join data frames
  df = df1 %>% 
    inner_join(df2, by = by)
  return(df)
}

# function with comment to filter data frame by year and indicator name
filter_df <- function(df, year, indicator) {
  # filter data frame
  df = df %>% 
    filter(Year %in% year, `Indicator Name` == indicator)
  return(df)
}

# function to group data frame by location and year and summarise by median 
# then add a target column to the data frame
summarise_df <- function(df, location, target) {
  # group data frame by location and year and summarise by median
  df = df %>% 
    group_by(df[[location]], Year) %>% 
    summarise(median_value = median(Value, na.rm = TRUE)) %>% 
    mutate(Target = target) %>%
    rename(location = `df[[location]]`)
  return(df)
}

# function to group data frame by year and summarise by median to obtain 
# the cross location median
summarise_df_cross <- function(df) {
  # group data frame by year and summarise by median
  df = df %>% 
    group_by(Year) %>% 
    summarise(crossmedian = median(median_value, na.rm = TRUE)) 
  return(df)
}

# list of parameters for the plot_indicator function
params <- list(
  target = list(
    x = 2018,
    y = 0,
    label = "■ ■ ■ Education 2030 FFA Target",
    size = 1.2,
    color = "darkgreen",
    linetype = "dotted",
    hjust = 0.5,
    vjust = -0.5
  ),
  crossmedian = list(
    x = 2018,
    y = 0.5,
    label = "■ ■ ■ Cross-regional median value",
    size = 1.2,
    color = "black",
    linetype = "dotted",
    hjust = 0.5,
    vjust = -0.5
  ),
  gap = list(
    x = 2018,
    xend = 2018,
    xlabel = 2019,
    y = 0.5,
    yend = 0,
    ylabel = 0,
    label = "■ ■ ■ Gap between the least achieving location and the target line",
    size = 1.2,
    color = "red",
    linetype = "dotted",
    hjust = 0.5,
    vjust = -0.5
  ),
  color = c(
    "dodgerblue", "orange3", "purple3", "brown4", "darkblue", "darkgreen",  "pink3"
  )
)

# function to plot the Median value trends of an indicator
# with the cross location median and target line using ggplotly
plot_indicator <- function(
      df, cross_df, params = params, target = FALSE, size = 0.9
    ) {
  # plot the Median value trends of an indicator with the cross 
  # location median and target line using ggplotly
  p = ggplot(
    df
  ) +
    # add location median line
    geom_line(
      mapping =  aes(
        x = Year,
        y = median_value,
        color = location
      ),
      size = size
    ) +
    # add cross location median line
    geom_line(
      cross_df,
      mapping = aes(
        x = Year,
        y = crossmedian
      ),
      linetype = params$crossmedian$linetype,
      color = params$crossmedian$color,
      size = params$crossmedian$size
    ) +
    # add cross location median label
    geom_text(
      mapping = aes(
        x = params$crossmedian$x, 
        y = params$crossmedian$y,
        label = params$crossmedian$label
      ),
      hjust = params$crossmedian$hjust,
      vjust = params$crossmedian$vjust, 
      color = params$crossmedian$color
    ) +
    # add target line conditionally
    if (target) {
      NULL
    } else {
      geom_line(
        mapping = aes(
          x = Year,
          y = Target
        ),
        color = params$target$color,
        size = params$target$size,
        linetype = params$target$linetype
      ) +
        # add target label
        geom_text(
          mapping = aes(
            x = params$target$x,
            y = params$target$y,
            label = params$target$label
          ), 
          hjust = params$target$hjust,
          vjust = params$target$vjust,
          color = params$target$color
        )
    } +
    # add segment line conditionally to indicate the gap between 
    # the least achieving location and the target line
    if (target) {
      NULL
    } else {
      geom_segment(
        # add segment line
        mapping = aes(
          x = params$gap$x,
          xend = params$gap$xend,
          y = params$gap$y,
          yend = params$gap$yend
        ),
        color = pqrams$gap$color,
        linetype = params$gap$linetype,
      ) +
      # add segment label
      geom_text(
        mapping = aes(
          x = params$gap$xlabel,
          y = params$gap$ylabel,
          label = params$gap$label
        ),
        hjust = params$gap$hjust,
        vjust = params$gap$vjust,
        color = params$gap$color
      )
    } +
    # add theme
    theme(plot.background = element_rect(fill = "gray")) +
    # add color 
    scale_color_manual(
      values = params$color
    ) +
    # add labels
    labs(
      x = "Year",
      y = "Median Value"
    ) 
  p = ggplotly(p)
  return(p)
}

