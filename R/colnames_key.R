# Long names ----

#' Long names for sessions columns, for display in app
#' @returns A list matching canonical sessions column names to their long name
#' @keywords internal
.sessions_long <- list(
  id = "Session ID",
  subject_id = "Subject ID",
  sex = "Sex",
  birth_year = "Birth Year",
  device_id = "Device Serial Number",
  session_start = "Session Start",
  session_end = "Session End",
  time_at_sleep_onset = "Time at Sleep Onset",
  time_at_midsleep = "Time at Midsleep",
  time_at_wakeup = "Time at Wakeup",
  sleep_onset_latency = "Sleep Onset Latency",
  sleep_period = "Sleep Period",
  time_in_bed = "Time in Bed",
  is_workday = "Workdays/Free Days",
  night = "Night"
)

#' Long names for epochs columns, for display in app
#' @returns A list matching canonical epochs column names to their long name
#' @keywords internal
.epochs_long <- list(
  timestamp = "Timestamp",
  session_id = "Session ID",
  signal_quality = "Signal Quality",
  sleep_stage = "Sleep Stage",
  night = "Night",
  is_asleep = "Asleep/Awake"
)

# Help tooltip texts ----

#' Help tooltips for sessions columns, for display in app
#' @returns A list with a tooltip string for each canonical sessions column name
#' @keywords internal
.sessions_help <- list(
  id = "Unique identifier for each session.",
  subject_id = "Unique identifier for each subject.",
  sex = "Sex of the subject.",
  birth_year = "Birth year for the subject (full dates can be provided, only the year will be used).",
  device_id = "Unique identifier for the recording device.",
  session_start = "Start time of the session (YYYY-MM-DD HH:MM:SS, HH:MM:SS or HH:MM).",
  session_end = "End time of the session (YYYY-MM-DD HH:MM:SS, HH:MM:SS or HH:MM).",
  time_at_sleep_onset = "Time at Sleep Onset (YYYY-MM-DD HH:MM:SS, HH:MM:SS or HH:MM).",
  time_at_midsleep = paste0("Time at Midsleep (YYYY-MM-DD HH:MM:SS, HH:MM:SS or HH:MM).",
                            "If not provided, it will be calculated from time at sleep onset and sleep period."),
  time_at_wakeup = "Time at Wakeup (YYYY-MM-DD HH:MM:SS, HH:MM:SS or HH:MM).",
  sleep_onset_latency = paste0("Time between session start and sleep onset (in seconds).",
                               "If not provided, it will be calculated from session start and sleep onset."),
  sleep_period = paste0("Total time spent asleep during the session (in seconds).",
                        "If not provided, it will be calculated from time at sleep onset and time at wakeup."),
  time_in_bed = "Total time spent in bed during the session (in seconds). If not provided, it will be calculated from session start and session end.",
  is_workday = paste0("Logical variable indicating if the session is on a workday (TRUE) or weekend (FALSE).",
                      "If not provided, it will be calculated from the night variable."),
  night = "Night (from 12pm to 12pm) the session belongs to (in any date format). If not provided, it will be calculated from session_start."
)

#' Help tooltips for epochs columns, for display in app
#' @returns A list with a tooltip string for each canonical epochs column name
#' @keywords internal
.epochs_help <- list(
  timestamp = "Timestamp of the epoch (YYYY-MM-DD HH:MM:SS).",
  session_id = "Identifier linking the epoch to a session in the sessions dataframe.",
  signal_quality = "Signal quality of the epoch.",
  sleep_stage = "Sleep stage of the epoch.",
  night = "Night (from 12pm to 12pm) the epoch belongs to (in any date format). If not provided, it will be calculated from timestamp.",
  is_asleep = "Logical variable indicating if the epoch is classified as asleep (TRUE) or awake (FALSE)."
)

# Session column name presets ----

#' Preset values for session column names
#' @returns A list of canonical session column names, where each value is an array of possible matching columns in the data (strings)
#' @keywords internal
.sessions_col_presets <- list(
  id = c("id", "session_id", "window_number"),
  subject_id = c("participant_id", "subject_id", "user_id", "ID"),
  sex = c("sex"),
  birth_year = c("birth_year"),
  device_id = c("device_serial_number"),
  session_start = c("session_start", "startTime", "enter_bed", "try_sleep_time"),
  session_end = c("session_end", "endTime", "out_of_bed", "out_of_bed_time"),
  time_at_sleep_onset = c("time_at_sleep_onset", "sleeponset_ts", "calculate_sleeponset"),
  time_at_midsleep = c("time_at_midsleep"),
  time_at_wakeup = c("time_at_wakeup", "wakeup_ts", "final_awakening", "wake_time"),
  sleep_onset_latency = c("sleep_onset_latency", "onset_latency_min"),
  sleep_period = c("sleep_period", "dur_spt_sleep_min", "SleepDurationInSpt"),
  time_in_bed = c("time_in_bed"),
  is_workday = c("is_workday", "daytype"),
  night = c("calendar_date", "night", "written_date", "date")
)

#' Parsing functions to use for each sessions column type
#' @keywords internal
.sessions_parsers <- function() {
  list(
    session_start = parse_time,
    session_end = parse_time,
    time_at_sleep_onset = parse_time,
    time_at_midsleep = parse_time,
    time_at_wakeup = parse_time,
    sleep_onset_latency = as.numeric,
    night = parse_date,
    birth_year = as.numeric,
    time_in_bed = as.numeric,
    sleep_period = as.numeric,
    is_workday = as.logical
  )
}

#' Empty list of session column names used in nocturn
#' @returns A list of canonical session column names with all values set to NULL
#' @keywords internal
.sessions_col_none <- list(
  id = NULL,
  subject_id = NULL,
  sex = NULL,
  birth_year = NULL,
  device_id = NULL,
  session_start = NULL,
  session_end = NULL,
  time_at_sleep_onset = NULL,
  time_at_midsleep = NULL,
  time_at_wakeup = NULL,
  sleep_onset_latency = NULL,
  sleep_period = NULL,
  time_in_bed = NULL,
  is_workday = NULL,
  night = NULL
)

# Epoch column name presets ----

#' Preset values for epoch column names
#' @returns A list of canonical epoch column names, where each value is an array of possible matching columns in the data (strings)
#' @keywords internal
.epochs_col_presets <- list(
  timestamp = c("timestamp", "timenum", "Time"),
  session_id = c("session_id", "window"),
  signal_quality = c("signal_quality_mean"),
  sleep_stage = c("sleep_stage", "class_id", "Stage"),
  night = c("night"),
  is_asleep = c("is_asleep")
)

#' Parsing functions to use for each epochs column type
#' @keywords internal
.epochs_parser <- function() {
  list(
    timestamp = parse_time,
    is_asleep = as.logical,
    night = parse_date
  )
}

#' Empty list of epoch column names used in nocturn
#' @returns A list of canonical epoch column names with all values set to NULL
#' @keywords internal
.epochs_col_none <- list(
  timestamp = NULL,
  session_id = NULL,
  signal_quality = NULL,
  sleep_stage = NULL,
  night = NULL,
  is_asleep = NULL
)
