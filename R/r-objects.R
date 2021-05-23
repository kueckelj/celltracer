




# Well plate information --------------------------------------------------


valid_well_plates <- c("2x3 (6)", "3x4 (12)", "4x6 (24)", "6x8 (48)", "8x12 (96)")

well_plate_info <- 
  data.frame(
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

non_data_track_variables <- c("frame_itvl", "frame_num", "frame_time", "cell_line",
                             "cl_condition", "condition", "phase", 
                             "x_coords", "y_coords") # keeps cell_id!!

original_ct_variables <- c("y-coordinate [pixel]", "x-coordinate [pixel]", "Frame number", "Cell ID",
                           "Distance from origin", "Distance from last point", "Instantaneous speed",
                           "Angle from origin", "Angle from last point")

short_ct_variables <- c("well_image", "condition", "cell_line", "cell_id", "x_coords",
                        "y_coords", "frame", "dfo", "dflp", "speed", "afo", "aflp")

# -----

# Miscellaneuos -----------------------------------------------------------

analysis_methods <- list(
  dim_red = c("pca", "tsne", "umap"), 
  clustering = c("hclust", "kmeans", "pam")
)

app_title <- "celltracer"

ambiguity_colors <- c("Clear" = "#1CE35B", "Ambiguous" = "#E02424", "Dismissed" = "lightgrey")

colors_grey <- c("unknown" = "lightgrey",
                 "unknown & unknown" = "lightgrey",
                 "Dismissed" = "lightgrey")

colors_unnamed <- c("#D4E8CF", "#EBAAAA", "#EBBCD6", "#A0E8CB", "#DEDEAB", "#B6A8E0", "#9FBA8E",
                    "#EBD1B0", "#BBC1F0", "#F2C2C2", "#AD9A9A", "#F2A9F5", "#E5CCF0", "#FFFC96",
                    "#CCFFFC", "#A0E8FA", "#C7B6FC", "#C5FCCC", "#EDFCB0", "#C8FAF0")

colors_information_status = c("Complete" = "forestgreen", 
                              "Incomplete" = "yellow", 
                              "Missing" = "red", 
                              "Discarded" = "lightgrey")

current_version <- list(major = 1, minor = 0, patch = 0)

debug_ct <- FALSE

descr_variables <- c("cell_id", "cell_line", "condition")

default_list <-
  list(
    clrp = "milo",
    clrsp = "viridis",
    make_pretty = TRUE, 
    method_aggl = "ward.D",
    method_corr = "pearson",
    method_dist = "euclidean",
    method_kmeans = "Hartigan-Wong",
    method_pam = "euclidean",
    phase = "first", 
    pt_alpha = 0.9, 
    pt_clr = "black",
    pt_clrp = "milo", 
    pt_clrsp = "viridis",
    pt_fill = "black",
    pt_size = 3, 
    verbose = TRUE, 
    well_plate = "", 
    with_cluster = TRUE, 
    with_meta = TRUE, 
    with_well_plate = TRUE
  )

default_character_values <- c("method_aggl", "method_corr", "method_dist", "method_kmeans", 
                       "method_pam", "phase", "pt_clr", "pt_clrp", "pt_clrsp", "pt_fill", 
                       "well_plate")

default_logical_values <- c("make_pretty", "with_cluster", "with_meta", "with_well_plate")

default_numeric_values <- c("pt_alpha", "pt_size")






filetypes <- c("csv$", "xls$", "xlsx$")

helper_content <- list(
  
  # designExperiment()
  overall_information = "Provide basic information such as the software the files derived from as well as the kind of files celltracer is supposed to load.
  The combination of experiment name and storage directory will result in the default directory under which the celltracer object is going to be stored. (You
  can change the directory afterwards at any time.)", 
  
  imaging_set_up = "Provide information about the imaging process that generated the images from which Cell Tracker or Cell Profiler again generated the data 
  tables you are about to analyze. 'Total Number of Images' refers to the number of times the imaging device made a picture (Cell Tracker refers to that as 'Frame Number'). 
  'Interval' and 'Interval Unit' refer to the timelapse between these images beeing made. If the track files you read in contain data that exceed the number of images specified 
  here the files will be filtered accordingly such that only data up to the 'Total Number of Images' is included.",
  
  experiment_phases = "If you have split your experiment in several phases (e.g. you started imaging with no treatment and after 24 hours you changed the condition of some wells and
  24 hours later you changed the conditions again - while maintaining the imaging process) you can specify the number of phases as well as their respective 
  starting point here. This will affect the way you can design the well plates (Step 4) which is why this step has to be done previously to that. Changing
  the experiment phases set up will result in all well plates saved up to that point beeing lost.", 
  
  well_plate_set_up = "This step is of great importance as it provides the information about how your data files will be read in as well as how the cell information
  of these files will be assigned to the different conditions and experiment phases. Clicking on 'New Well Plate' will create an empty well plate visualized on the right.
  'Covered Areas per Well' refers to the number of images made per well per imaging interval. The area's number is denoted as the last number of the respective file name
  (e.g. ~/A1_1.csv, ~/A1_2.csv, ~/A1_3.csv would require you to set 'Covered Areas per Well' to 3).
  Once the well plate is visualized you can interactively select wells which highlights them in dark red. Continue with specifiying their cell line and their conditions via the respective text inputs.
  After clicking on 'Add Info' you will see a small info box in the right lower corner telling you whether saving the information was successfull or not. If successfull click on the well plate plot again 
  and you should see that the color of the wells have changed. 
  
  Before continuing with the next well plate make sure to add the well plate via 'Add Well Plate'. Not doing that before continuing with 'New Well Plate' results in all specifications beeing lost. Info boxes
  as well as the table in the box 'Save Experiment Design & Proceed' will tell you whether your well plate set up has been saved successfully.",
  
  example = c(
  "Choose an example file from those that you want to read in with celltracer later on. As Cell Profiler & Co let you name the columns yourself celltracer needs to know which 
  columns refer to what exactly which is why it needs an example file. The modal you open by clicking on 'Example Data: Browse' lets you assign the columns of the example file you have loaded.",
  
  "   ",
  
  "Track files (as part of the celltracer terminology) refer to tables in which each row/observation represents a cell at a given moment (image taken) and in which each column/variable provides information about these cells (such 
  as current x- and y-coordinate, shape diameter, illuminance intensitry etc.). Each row must be uniquely identified by a column providing the cell's ID and a column providing the image number the row bases on.
  These two columns have to be specified. If you want celltracer to analyze migration data you have to denote the columns refering to the cell's x- and y-coordinates as well. If your files do not contain these columns you can
  leave them out which will result in a celltracer object that is incompatible with celltracer functions dealing with migration data.",
  
  "   ",
  
  "On top of the columns mentioned previously you can choose the columns that you want to be part of downstream analysis steps. If denoted in Step 2 celltracer will take the x- and y-coordinates and will calculate migration data.
  This includes the 'Distance from last point (dflp)', 'Distance from origin (dfo)' and the 'Speed (speed)'. As celltracer automatically calculates these three variables there is no need to make celltracer keep them additionally if your 
  example file already contains them. If your example files  contains columns referring to the cells morphology at a given image like 'Cell Diameter', 'Number of Connections' etc. you can denote them here. Eventually,
  while loading all files your experiment contains via loadData() celltracer will read in all files and keep the columns you've denoted in Step 2 and 3. The function processData() will summarize the tracking data columns via statistical
  measures like minimum, maximum, mean, median which you can compare later on across cell lines, conditions, clustering results etc. (e.g. 'Cell Diameter' of the track files will be summarized to 'mean_cell_diameter', 'min_cell_diamter' etc. 
  of the stat files.") ,
  
  
  # loadData()
  assign_folder = "In the left lower corner you see a select option that contains the names of the well plates you have set up in the module opened via 'designExperiment()'. 
  In order for celltracer to read in the data every well plate needs to be assigned to a directory in which celltracer finds the files to read in. Clicking on 'Assign Folder: Browse'
  opens a window that gives you access to the folders of your device. Select the one that contains the files of the respective well plate or that again contains (sub-)folders in which the files are stored.
  (If you want celltracer to look in subfolders, too, make sure that 'Include Subfolders' is enabled.) After you have assigned the folder by closing the window you will see a well plate plot appear right above the 
  button 'Assign Folder: Browse'. The wells will be colored according to the number of files that have been found in the folder you specified. Green/Complete means that all files have been found. 
  Yellow/Incomplete means that some files of the well have been found. (e.g. you set 'Covered Areas per Well' to 3 which made celltracer expect files 'A1_1.csv', 'A1_2.csv' and 'A1_3.csv'.
  However only 'A1_1.csv' and 'A1_3.csv' have been found). Missing/Red means that no files of that well have been found at all. Blue/Ambiguous might appear if you stored the files in 
  subfolders of the assigned folder and some folders contain files with equal names due to typos while naming the files. (e.g. ~/Ctrl/A1_1.csv, ~/Treatment/A1_1.csv) If you realized that you assigned the wrong folder 
  or that some files have been named incorrect you can fix what needs to be fixed and assign the directory again by clicking on 'Assign Folder: Browse'.", 
  
  well_plate_status = "This table provides summary information about the file availability for all well plates. It updates every time you assign a folder to a well plate. 'Number of Files' sums up the number of files found
  and 'Expected Number of Files' sets that in relation to the number of files expected according to number of covered areas per well and the number of wells for which you specified cell line and conditions. Missing or incomplete
  wells do not prevent you from reading in all other files found - no need to design the experiment again if you realize that some files are missing. Just click on 'Load Data' and the missing files will be ignored. In case of ambiguous
  wells you need to rename or delete the files that are doubled before loading the data.",
  
  load_files_and_proceed = "After clicking on 'Load Data' you should see a progress bar for every well plate giving information about the reading progress. Once all files are read in you obtain information about any errors that occured while 
  reading the files. In case of no errors you can simply click on 'Save & Proceed' and afterwards on 'Return Celltracer Object'. If the box hints at errors occured you can try and fix the cause for these errors and then click on 'Load Data' again
  which will repeat the reading process. On the other hand you can ignore the errors and click on 'Save & Proceed' anyway which makes celltracer ignore the files that were not suffessfully read in and continue with the rest."
  
  
  
)

imp_filter_criteria <- c("total_meas", "skipped_meas", "first_meas", "last_meas")

interval_options <- c("weeks", "days", "hours", "minutes", "seconds", "miliseconds")

invalid_groups <- c("cell_id", "well_plate_name", "well_plate_index", "well", "well_image")

legend_titles <- c("ambiguity_status" = "Ambiguity Status", 
                   "cl_condition" = "Cell Line & Condition", 
                   "condition" = "Condition", 
                   "cell_line" = "Cell Line")

meta_variables <- c("cell_id", "well_plate_name", "well_plate_index", "well", "well_image")

new_slots <- c("analysis", "compatibility", "default", "information", "name", "well_plates", "version")

numeric_stat_vars <- c("total_dist", "max_dist_fo", "avg_dist_fo", "max_dist_flp",
                       "avg_dist_flp", "max_speed", "avg_speed", "mgr_eff")

not_splitted <- c("No treatment", "From beginning")

object_class <- "cto"
base::attr(object_class, which = "package") <- "celltracer"

protected_vars <- c("cell_id", "cell_line", "condition",
                    "well_plate_name", "well_plate_index", "well",  "well_image")

set_up_funs <- list(experiment_design = "designExperiment()", 
                    load_data = "loadData()", 
                    quality_check = "checkDataQuality()", 
                    process_data = "processData()")

shiny_bar_positions <- c("Stacked" = "stack", "Dodged" = "dodge", "Filled" = "fill")

shiny_discrete_vars <- c("Cell Line and Condition" = "cl_condition", 
                         "Cell Line" = "cell_line", 
                         "Condition" = "condition")

stat_funs <- list("max" = base::max,
                  "mean" = base::mean,
                  "median" = stats::median,
                  "min" = base::min,
                  "sd" = stats::sd,
                  "var" = stats::var)

status_colors <- c("Missing" = "#B31010",
                   "Incomplete" = "#FFD700",
                   "Complete" = "darkgreen", 
                   "Dismissed" = "lightgrey",
                   "Ambiguous" = "#4C12E0")

storage_slots <- c("directory", "valid_directories", "missing_files")

testable_plottypes <- c("boxplot", "violinplot")

well_plate_vars <- c("well_plate_name", "well_plate_index", "well",  "well_image")



# -----


# Pretty names ------------------------------------------------------------

pretty_exp_types <- 
  list("one_time_imaging" = "One Time Imaging", 
       "time_lapse" = "Time Lapse", 
       "time_lapse_smrd" = "Time Lapse (summarized)"
       )

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
  
  # renaming
  "how_to_name_input" = "Input needs to be named like this: 'new_group_name' = 'old_group_name'",

  
  # statistical tests
  "stat_test_requirements" = "In order to perform statistical tests please choose 'boxplot' or 'violinplot' as input for argument 'plot_type' and specify only one variable as input for argument 'variables'."
  
  
)




# -----

