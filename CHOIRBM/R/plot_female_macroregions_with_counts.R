#' Plot the male CHOIR Body Map
#'
#' Creates a new plot of the front and back of the female CHOIR body map.
#'
#' @param df data.frame
#' @param value string
#' @return ggrob
#' @export
#'
#' @examples
#' cbm_df <- gen_example_data()
#' plot_female_choirbm(cbm_df, "value", female_region_occurences$percent_occurrences, new_color_palette, body_map)
#'
#' @importFrom ggplot2 ggplot aes geom_polygon scale_y_reverse facet_wrap geom_text scale_fill_identity as_labeller annotate
#' @importFrom ggplot2 theme_void theme
#' @importFrom rlang .data
#'

# text_x_pixel_adjustment_map used to adjust x,y pixel location of label from calculated centroid position of microregion
text_x_pixel_adjustment_map <- list(
  '121' = 5,
  '122' = -5,
  '112' = 0,
  '113' = -5,
  '108' = 5,
  '109' = -5,
  '101' = 4,
  '102' = -2,
  '103' = 9,
  '104' = -9,
  '125' = 3, 
  '128' = -3,
  '223' = 5,
  '224' = -5,
  '218' = 5,
  '219' = -5,
  '212' = 5,
  '213' = -5,
  '215' = 3,
  '216' = -3,
  '208' = 10,
  '209' = -10,
  '203' = 9,
  '204' = -9,
  '201' = 4,
  '202' = -2,
  '227' = 3, 
  '230' = -3 
)

# y_equivalent map used to make sure y pixel coordinates are the same for mirroring microregions (ex. left hand/right hand)
y_equivalent_map <- list(
  '136' = '135', #feet
  '134' = '133',
  '132' = '131',
  '130' = '129',
  '127' = '126',
  '122' = '121',
  '123' = '120',
  '128' = '125',
  '124' = '119',
  '118' = '117',
  '116' = '115',
  '114' = '111',
  '110' = '107',
  '113' = '112',
  '109' = '108',
  '106' = '105',
  '104' = '103',
  '102' = '101',
  '238' = '237',
  '236' = '235',
  '234' = '233',
  '232' = '231',
  '229' = '228',
  '224' = '223',
  '225' = '222',
  '219' = '218',
  '213' = '212',
  '209' = '208',
  '230' = '227',
  '226' = '221',
  '220' = '217',
  '216' = '215',
  '214' = '211',
  '210' = '207',
  '206' = '205',
  '204' = '203',
  '202' = '201'
)


findBodyArea <- function(bodyMap, id) {
  # Function to find the macroregion a microregion belongs to using the bodyMap named list
  #
  # Parameters:
  #   bodyMap: named list mapping the microregion to the main macroregion 
  #   id: the microregion id
  #
  # Returns:
  #   the macroregion the microregion id is within
  
  areas <- names(bodyMap)
  for (area in areas) {
    if(id %in% bodyMap[[area]]) {
      return(area) # Return upon finding the matching area
    }
  }
  return(NA) # Return NA if no area matches the id
}

createBodyMapColors <- function(region_occurrences, body_map, new_color_palette) {
  # Function to assign macroregion colors for each region, based on the percentage within each region 
  #
  # Parameters:
  #   region_occurrences: named list with percent occurences for each macroregion - passed as parameter to plot_male_macroregions_with_counts
  #   body_map: named list mapping the microregion to the main macroregion - passed as parameter to plot_male_macroregions_with_counts
  #   new_color_palette: intuitive color palette that will be used to color macroregions (another parameter to plot_male_macroregions_with_counts)
  #   
  # Returns:
  #   named list with colors for each macroregion 

  ranked_indices <- rank(-region_occurrences, ties.method = "first")
  
  # Assign colors based on rankings
  body_map_colors <- sapply(ranked_indices, function(rank) {
    new_color_palette[rank]
  })
  
  # Convert the named vector to named list
  return(as.list(body_map_colors))
}

plot_female_macroregions_with_counts <- function(df, value, region_occurrences, new_color_palette, body_map) {
  # Exported function to create female body image map colored by macroregion percent and with microregion counts annotated
  #
  # Parameters:
  #   df: data.frame with id (microregion id), value (microregion counts), and group (Front/Back) 
  #   region_occurrences: named list with percent occurences by macroregion 
  #   new_color_palette: 11 value color palette used for coloring body image macroregion percent
  #   body_map: named list mapping microregion to macroregion
  #
  # Returns:
  #   gg_plot colored by macroregion (ranked by percent) and annotated microregion counts

  # assign body map colors for macroregion
  body_map_colors <- createBodyMapColors(region_occurrences, body_map, new_color_palette)

  # assign colors for each microregion 
  df$body_area <- sapply(df$id, function(id) findBodyArea(body_map, id))
  df$body_area_color <- sapply(df$body_area, function(area) body_map_colors[[area]])

  # assign female drawing coordinates
  positions <- rbind(female_front_poly_coords_df, female_back_poly_coords_df)
  datapoly <- merge(df, positions, by = "id")
  

  # Create microregion_labels dataframe that will estimate x,y pixel location, and then adjust according to text_x_pixel_adjustment_map
  split_data <- split(datapoly, datapoly$id)
  microregion_labels <- do.call(rbind, lapply(split_data, function(group_data) {
    # Default adjustment is 0
    adjustment <- 0

    # Check if the id is in the adjustment map
    id_str <- as.character(group_data$id[1])
    if(id_str %in% names(text_x_pixel_adjustment_map)) {
      adjustment <- text_x_pixel_adjustment_map[[id_str]]
    }
    
    # Apply adjustment to the x-coordinate
    adjusted_x <- mean(group_data$x) + adjustment


    data.frame(
        id = group_data$id[1], 
        x = adjusted_x, 
        y = mean(group_data$y), 
        value = round(group_data$value[1]), # Sum of the values as integer
        group = group_data$group[1]
      )
  }))

  # Create a lookup for y values to make sure y values match on mirroring microregion annotations
  y_lookup <- setNames(microregion_labels$y, microregion_labels$id)

  # Apply y adjustments based on y_equivalent_map
  microregion_labels$y <- sapply(microregion_labels$id, function(id) {
    id_str <- as.character(id)
    if(id_str %in% names(y_equivalent_map)) {
      equivalent_id_str <- y_equivalent_map[[id_str]]
      return(y_lookup[equivalent_id_str])
    } else {
      return(y_lookup[id_str])
    }
  })

  # Assign microregion_labels label as the count (value)
  microregion_labels$label <- sprintf("%d", microregion_labels$value)
  # use label to plot area total.. 

  # Create the plot
  # p2 <- ggplot(datapoly) +
  #   aes(x = x, y = y) +
  #   geom_polygon(
  #     aes(fill = body_area_color, group = .data[["id"]]),
  #     color = "#333333",
  #     size = 0.5
  #   ) +
  #   geom_text(
  #     data = microregion_labels, # Use the microregion_labels data frame
  #     aes(x = x, y = y, label = .data[["id"]]), # Use the formatted value_sum as the label
  #     color = "black",
  #     size = 2.5
  #   ) +
  #   scale_y_reverse() +
  #   facet_wrap(~group) +
  #   theme_void() +
  #   theme(legend.position = "None") +
  #   scale_fill_identity()
  # print(p2)
# }

  # define coordinates and labels for headings
  heading_annotations <- data.frame(
    x = c(140, 130), # Different x values for "Front" and "Back"
    y = c(-10, -10), # Same y value for both, adjust as needed
    label = c("Female Front", "Female Back"),
    group = c("Front", "Back")
  )
  heading_annotations$group <- factor(heading_annotations$group, levels = c("Front", "Back"))

  p <- ggplot(datapoly) +
    aes(x = x, y = y) +
    geom_polygon(
      aes(fill = body_area_color, group = .data[["id"]]),
      color = "#333333",
      size = 0.5
    ) +
    geom_text(
      data = microregion_labels, 
      aes(x = x, y = y, label = label), 
      color = "black",
      size = 3
    ) +
    geom_text(
    data = heading_annotations, 
    aes(x = x, y = y, label = label, group = group),
    size = 3.5,
    color = "black"
  ) +
    scale_y_reverse() +
    facet_wrap(~group, labeller = as_labeller(c(Front = "", Back = ""))) +
    theme_void() +
    theme(legend.position = "None") +
    scale_fill_identity()
  return(p)
}
