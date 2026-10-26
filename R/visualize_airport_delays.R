#' Visualize Airport Delays
#'
#' Calculates mean arrival delays for flights and plots them by airport geographic coordinates.
#'
#' @export
visualize_airport_delays <- function() {
  flights <- nycflights13::flights
  airports <- nycflights13::airports

  # Calculate mean arrival delay per destination airport using explicit dplyr prefixes
  delay_df <- flights %>%
    dplyr::group_by(dest) %>%
    dplyr::summarise(mean_delay = mean(arr_delay, na.rm = TRUE)) %>%
    dplyr::inner_join(airports, by = c("dest" = "faa"))

  # Plot using explicit ggplot2 prefixes
  p <- ggplot2::ggplot(delay_df, ggplot2::aes(x = lon, y = lat, color = mean_delay)) +
    ggplot2::geom_point(size = 2, alpha = 0.8) +
    ggplot2::scale_color_continuous(name = "Mean Delay (min)") +
    ggplot2::labs(
      title = "Mean Flight Arrival Delay by Airport Location",
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme_minimal()

  return(p)
}
