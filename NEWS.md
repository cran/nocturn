# nocturn 1.2.0

## App

* New "Comparison" tab to load multiple Sessions datasets and compare them (#14)
  * This is meant for comparing multiple recordings of the same sleep using multiple devices
* "Import filters" and "Export filters" buttons to the Filtering tab (#24)
  * Filters are exported in yaml format (can be opened and modified in any text editor)
  * "Import filters" applies all filters defined in the loaded yaml file to the current nocturn session
* Help menus are available through the app to give more details on plots and tables (#13)

## Bug fixes

* `plot_sleep_spiral()` is now correctly grey on awake timepoints when using non-default `color_by` (#30)

## General improvements

* Data parsing is more reliable to import data from different sources (#28)

# nocturn 1.1.3

* First CRAN release (no changes to code)
  * nocturn can now be installed from CRAN: `install.packages("nocturn")`

# nocturn 1.1.2

## Bug fixes

* Fixed an issue that prevented setting column names for Epochs in some cases

# nocturn 1.1.1

## Bug fixes

* Fixed an issue with the age filter in the app (filtering function unaffected)
* Allow "un-setting" column names in the app by selecting "-"

# nocturn 1.1.0

## App UI

* Added a button to collapse/expand the sidebar

## Data import and cleaning

* If `session_start` and `sleep_onset_latency` are available, they will be used to infer `time_at_sleep_onset` (if not already provided)
* Added a tooltip to the "Load example data" button

## Filtering

* `set_min_sleep_period()` created to keep sessions with a sleep period longer than a threshold (in hours)
* Added a filter in the app "sleep" tab: "Minimum Time Asleep"

## Plots

* Improved how plots handle continuous and time data in `colour_by`

## Sleep Report

* Use svgedit to generate sleep reports faster and more reliably

## Scripts

* Added explicit errors when a function is called, and the necessary column names are not set

# nocturn 1.0.3

## Set column names (app)

* Fixed a bug that prevented the "Set column names" menu from being displayed

# nocturn 1.0.2

## Sleep report

* Take current filters into account when creating a sleep report

# nocturn 1.0.1

## Data import

* Multiple files can be imported selected by clicking the "Browse..." button

# nocturn 1.0.0

* Name change: Ambient Viewer becomes nocturn!

## App interface

* Added a top bar for navigation between different app functionalities
* Added a log window (sidebar) to display relevant information to the user

## Data import

* Added an option to load files in batch (`load_batch()` in scripts)
  * All files must be in the same folder
  * A filename pattern can be used, e.g. ".csv" to only import csv files
* Added support for .xls, .xlsx, and .edf formats
  * For an .edf file to be imported as epochs, it needs to contain sleep stage annotations under the "annotation" field

## Filtering

* Added a `filters` column to the Filtering table, to show which filters caused a particular session to be removed (#5)
  * The "Filters to display" drop-down menu allows selecting which filters should be shown in the filtering table

## Deprecations

* `edfs_to_csv()` is removed, as .edf files can now be loaded with `load_sessions()`, `load_epochs()` or `load_batch()`
* `col_names` is removed from all functions that used it
  * Column names are now stored as an attribute of the sessions or epochs dataframes
  * They can be retrieved with `get_colnames()` and modified with `set_colnames()`

# Ambient Viewer 0.0.10

## Fixed

* For the "age" filter, age is now calculated from the time of data collection, and not the current date (#6)
* The compliance table shows sessions as duplicates only if they're from the same night *and* the same subject (#7)
* Subject ID was added as a column in the annotation table (#8)

# Ambient Viewer 0.0.9

## Data formats

* `edfs_to_csv()` is added to convert a batch of .edf files (from Continuous Positive Airway Pressure * CPAP devices) to a single .csv file
  * Example: `edfs_to_csv(folder_in = "path/to/edf_folder", file_out = "summary.csv")
* Added support for edf-derived data (to be input as "Sessions")
  * Note: at the moment, the Sleep Report and the Sleep Regularity tab are not functional with this type of data

## Annotations

* Added pagination options to deal with large numbers of sessions
* Added a search bar to help in selecting sessions (e.g. select all October 2024 sessions by searching "2024-10")

## Sleep Regularity

* Let Session-based metrics be displayed even if Epochs are not loaded

# Ambient Viewer 0.0.8

## Visualisations

* In the sleep clock, removed the "clock arms", which were not very useful and making the figure crowded

## Sleep Report

* Added the social jet-lag value

## Fixed

* Sleep clock: fixed display for sessions spanning midday

# Ambient Viewer 0.0.7

## Sleep Report

* The title of the Sleep Report can be set in the app via the corresponding text box, or by using the `title` argument with the `sleep_report` function
  * Default title is an empty string, which will only print "Sleep Report"
* Added a sentence in the "Sleep monitoring" section to let people know that the recording is not always fully accurate and can be influenced by external factors, e.g. pets

# Ambient Viewer 0.0.6

## New

* Added the `sleep_report` function to generate a sleep report for patients, using R markdown
  * Added a "Download subject report" button to the Export Data section of nocturn
* Added sleep efficiency to the Sessions summary table
* Added a "Sleep Times Distributions" tab that shows distributions of times at sleep onset, midsleep and wakeup. Three types of graph are available:
  * Boxplot (horizontal)
  * Histogram
  * Density (with median value highlighted as dashed line)

# Ambient Viewer 0.0.5

## New

* Added functions to calculate different metrics on sleep regularity:
  * Interdaily stability (IS)
  * Social jet-lag (SJL)
  * Chronotype
  * Composite Phase Deviation (CPD)
  * Sleep Regularity Index (SRI)

## Fixed

* In Somnofy data, sleep stage 5 (no presence) was incorrectly classed as "asleep" in the spiral

# Ambient Viewer 0.0.4

## New

* Time-series data from GGIR (raw output from part 5) can be loaded as Epochs
  * A new [wiki section](https://github.com/chronopsychiatry/AMBIENT-BD-nocturn/wiki/Using-GGIR-data) has been added to explain which outputs can be used

# Ambient Viewer 0.0.3

## New

* Summary outputs from GGIR (part 5) in csv format can be loaded as Sessions

## Fixed

* Setting column names with "Set Session Columns" and "Set Epoch Columns" work correctly
* Loading a CSV file that doesn't fit the pre-set formats doesn't crash the app anymore

# Ambient Viewer 0.0.2.1

* Patch to fix sleep clock display on the shiny server. No visible changes for users.

# Ambient Viewer 0.0.2

## New

### Annotation tab

The "Annotation" tab (next to Summary, Compliance and Filtering) allows the user to assign a custom tag to each session. This could be used for example to highlight specific episodes from a sleep journal. The custom annotations will be displayed in the different data tables, and can be used in the figures (see next section).

### Changable colormaps for the figures

Most figures now have a "Colour by" drop-down menu. Use this menu to change the colours on the figure, for example to colour data according to the custom annotations ("annotation" variable), or by work days vs. weekends ("is_workday").

### More filtering options

Added filtering options for:

* Age
* Sex
* Subject ID

These sliders are shown dynamically: they will only appear in the app if the relevant column is available in the data.

The corresponding R filtering functions are:

* `filter_by_age_range`
* `filter_by_sex`
* `select_subjects`

### Manage column names

The app now has "Set session columns" and "Set epoch columns" menus (in the Data input module) that allow setting different column names to the default.

All functions that accept sessions or epochs dataframes now take an optional `col_names` argument that can be used to override the default column names. Example:

```r
filtered_sessions <* example_sessions |> remove_sessions_no_sleep(col_names = list(sleep_period = "time_asleep"))
```

### New example data

Added example data for the v1 of the Somnofy API (data generated before 2025). Once nocturn is loaded, they are directly accessible via the variables:

* `example_sessions_v1`
* `example_epochs_v1`

# Ambient Viewer 0.0.1

## New

* Updated the Sleep clock to show the duration of sleep sessions
* Added "Sleep onset & wakeup", a horizontal bar graph that shows sleep onset and wakeup times aggregated either by night, days of the week, or weekend/weekday.
* Added an option to export plots in PDF format

## Fixed

* Defined better presets for dimensions of exported plots
* Fixed a timezone issue that was sometimes causing sleep onset and wakeup from sessions to be shifted by 1 hour
