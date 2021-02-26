
# Well plate information --------------------------------------------------


valid_well_plates <- c("2x3 (6)", "3x4 (12)", "4x6 (24)", "6x8 (48)", "8x12 (96)")

well_plate_info <- 
  base::data.frame(
    type = valid_well_plates, 
    rows = c(2,3,4,6,8), 
    cols = c(3,4,6,8,12)
  )

# -----

# Regular expressions -----------------------------------------------------

well_regex <- "[A-Z]{1}\\d{1,2}"
well_image_regex <- "[A-Z]{1}\\d{1,2}_\\d{1}"
file_regex <- "[A-Z]{1}\\d{1,2}_\\d{1}\\.(csv|xls|xlsx)$"

# -----

# Column names ------------------------------------------------------------


original_ct_variables <- c("y-coordinate [pixel]", "x-coordinate [pixel]", "Frame number", "Cell ID",
                           "Distance from origin", "Distance from last point", "Instantaneous speed",
                           "Angle from origin", "Angle from last point")

short_ct_variables <- c("well_image", "condition", "cell_line", "cell_id", "x_coords",
                        "y_coords", "frame", "dfo", "dflp", "speed", "afo", "aflp")

# -----

# Miscellaneuos -----------------------------------------------------------

app_title <- "Cell Tracer"

ambiguity_colors <- c("Clear" = "#1CE35B", "Ambiguous" = "#E02424", "Dismissed" = "lightgrey")


colors_grey <- c("unknown" = "lightgrey", 
                 "Dismissed" = "lightgrey")

colors_unnamed <- c("#D4E8CF", "#EBAAAA", "#EBBCD6", "#A0E8CB", "#DEDEAB", "#B6A8E0", "#9FBA8E",
                    "#EBD1B0", "#BBC1F0", "#F2C2C2", "#AD9A9A", "#F2A9F5", "#E5CCF0", "#FFFC96",
                    "#CCFFFC", "#A0E8FA", "#C7B6FC", "#C5FCCC", "#EDFCB0", "#C8FAF0")


debug_ct <- FALSE

descr_variables <- c("cell_id", "cell_line", "condition")

filetypes <- c("csv$", "xls$", "xlsx$")

imp_filter_criteria <- c("total_meas", "skipped_meas", "first_meas", "last_meas")

interval_options <- c("weeks", "days", "hours", "minutes", "seconds", "miliseconds")

invalid_groups <- c("cell_id", "well_plate_name", "well_plate_index", "well", "well_image")

legend_titles <- c("ambiguity_status" = "Ambiguity Status", 
                   "cl_condition" = "Cell Line & Condition", 
                   "condition" = "Condition", 
                   "cell_line" = "Cell Line")

meta_variables <- c("cell_id", "cell_line", "condition", "cl_condition",
                    "well_plate_name", "well_plate_index", "well", "well_image")

numeric_stat_vars <- c("total_dist", "max_dist_fo", "avg_dist_fo", "max_dist_flp",
                       "avg_dist_flp", "max_speed", "avg_speed", "mgr_eff")

not_splitted <- c("No treatment", "From beginning")

shiny_bar_positions <- c("Stacked" = "stack", "Dodged" = "dodge", "Filled" = "fill")

shiny_discrete_vars <- c("Cell Line and Condition" = "cl_condition", 
                         "Cell Line" = "cell_line", 
                         "Condition" = "condition")

status_colors <- c("Missing" = "#B31010",
                   "Incomplete" = "#FFD700",
                   "Complete" = "darkgreen", 
                   "Dismissed" = "lightgrey",
                   "Ambiguous" = "#4C12E0")

storage_slots <- c("directory", "valid_directories", "missing_files")

testable_plottypes <- c("boxplot", "violinplot")

# -----


# Pretty names ------------------------------------------------------------

pretty_grouping_variables_list <-
  list("Cell Line &\n Condition" = "cl_condition", 
       "Condition" = "condition", 
       "Cell Line" = "cell_line")

pretty_grouping_variables_vec <-
  purrr::imap_chr(.x = pretty_grouping_variables_list, 
                  .f = ~ .x)

pretty_linetypes <- c("Solid" = "solid", 
                      "Twodash" = "twodash", 
                      "Longdash" = "longdash", 
                      "Dotted" = "dotted", 
                      "Dotdash" = "dotdash")

pretty_plottypes <- c("Violinplot" = "violinplot", 
                      "Ridgeplot" = "ridgeplot", 
                      "Densityplot" = "density",
                      "Boxplot" = "boxplot"
                      )

pretty_phases <- c("Before treatment" = "before_tmt",
                   "After first treatment" = "first_tmt", 
                   "Entire timespan" = "all")

pretty_stattests <- c("T-test" = "t.test", 
                      "Wilcox" = "wilcox.test", 
                      "ANOVA" = "anova", 
                      "Kruskal" = "kruskal.test")

pretty_stattests_pairwise <- c("None" = "none", pretty_stattests[1:2])
pretty_stattests_groupwise <- c("None" = "none", pretty_stattests[3:4])

pretty_stat_variables_list <-
  list("Total Distance" = "total_dist" ,
       "Max. distance from origin" = "max_dist_fo", 
       "Avg. distance from origin" = "avg_dist_fo",
       "Max. distance from last point" = "max_dist_flp", 
       "Avg. distance from last point" = "avg_dist_flp", 
       "Max. speed" = "max_speed", 
       "Avg. speed" = "avg_speed", 
       "Migration efficiancy" = "mgr_eff")

pretty_stat_variables_vec <-
  purrr::imap_chr(.x = pretty_stat_variables_list, 
                  .f = ~ .x) 

pretty_wp_variables_list <- 
  list("WP Name" = "well_plate_name", 
       "WP Index" = "well_plate_index", 
       "Well" = "well", 
       "Well-Image" = "well_image")

pretty_wp_variables_vec <- 
  purrr::imap_chr(.x = pretty_wp_variables_list, 
                  .f = ~ .x)


pretty_names_vec <- 
  c(pretty_stat_variables_vec,
    pretty_grouping_variables_vec, 
    pretty_wp_variables_vec)

pretty_names_list <- 
  c(pretty_stat_variables_list,
    pretty_grouping_variables_list, 
    pretty_wp_variables_list)

# -----



# Feedback lists ----------------------------------------------------------

ct_warnings <- list(
  
  # statistical tests
  "stat_test_requirements" = "In order to perform statistical tests please choose 'boxplot' or 'violinplot' as input for argument 'plot_type' and specify only one variable as input for argument 'variables'."
  
)


# -----

