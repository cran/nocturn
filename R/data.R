#' Example Sessions data
#'
#' A data frame containing sessions recorded by a Somnofy device.
#'
#' @format ## `example_sessions`
#' A data frame with 124 rows and 60 columns.
#' Each row represents a session. Columns contain metadata about the session, including:
#' - session_start: The start time of the session in UTC.
#' - session_end: The end time of the session in UTC.
#' - subject_id: The ID of the subject.
#' - device_serial_number: The serial number of the device used.
#' - time_at_sleep_onset: The time at which the subject fell asleep.
#' - time_at_wakeup: The time at which the subject woke up.
#' Columns also include various metrics averaged over the session, such as:
#' - mean heart rate
#' - mean respiration rate
#' Finally, some columns contain environmental parameters, such as:
#' - Temperature
#' - Humidity
#' - Light intensity
#' - Noise level
#' - Atmospheric pressure
#' @source data-raw/example_sessions.csv
"example_sessions"

#' Example Epoch data
#'
#' A data frame containing epoch data recorded by a Somnofy device.
#'
#' @format ## `example_epochs`
#' A data frame with 18,755 rows and 15 columns.
#' Each row represents a time-point (or epoch) in a session. Epochs are 30 seconds long.
#' The columns are as follows:
#' - timestamp: The time at which the epoch was recorded in UTC.
#' - subject_id: The ID of the subject.
#' - signal_quality_mean: The mean signal quality of the epoch.
#' - movement_fast_mean: The mean movement detected during the epoch.
#' - movement_fast_nonzero_pct
#' - distance_mean: the distance of the subject from the device in meters.
#' - motion_data_count: The number of data points in the epoch (30).
#' - light_ambient_mean: The ambient light level during the epoch.
#' - sound_amplitude_mean: The sound amplitude during the epoch.
#' - temperature_ambient_mean: The ambient temperature during the epoch.
#' - humidity_mean: The ambient humidity during the epoch.
#' - pressure_mean: The ambient pressure during the epoch.
#' - indoor_air_quality_mean: The indoor air quality during the epoch.
#' - epoch_duration: The precise duration of the epoch (seconds).
#' - sleep_stage: The sleep stage as established with the VT algorithm. They are encoded as numbers 0-5
#' @source data-raw/example_epochs.csv
"example_epochs"

#' Example Sessions data (Somnofy API v1)
#'
#' A data frame containing sessions recorded by a Somnofy device.
#'
#' @format ## `example_sessions_v1`
#' A data frame with 87 rows and 70 columns.
#' Each row represents a session. Columns contain metadata about the session, including:
#' - user_id: The ID of the recorded subject.
#' - sex: The sex of the recorded subject.
#' - birth_year: The year of birth of the recorded subject.
#' - session_start: The start time of the session in UTC.
#' - session_end: The end time of the session in UTC.
#' - time_at_sleep_onset: The time at which the subject fell asleep.
#' - time_at_wakeup: The time at which the subject woke up.
#' Columns also include various metrics averaged over the session, such as:
#' - mean heart rate
#' - mean respiration rate
#' Finally, some columns contain environmental parameters, such as:
#' - Temperature
#' - Humidity
#' - Light intensity
#' - Noise level
#' - Atmospheric pressure
#' @source data-raw/example_sessions_v1.csv
"example_sessions_v1"

#' Example Epoch data (Somnofy API v1)
#'
#' A data frame containing epoch data recorded by a Somnofy device.
#'
#' @format ## `example_epochs_v1`
#' A data frame with 1,373 rows and 16 columns.
#' The corresponding session ID is contained in the file name.
#' Each row represents a time-point (or epoch) in a session. Epochs are 30 seconds long.
#' The columns are as follows:
#' - timestamp: The time at which the epoch was recorded in UTC.
#' - signal_quality_mean: The mean signal quality of the epoch.
#' - movement_fast_mean: The mean movement detected during the epoch.
#' - movement_fast_nonzero_pct
#' - distance_mean: the distance of the subject from the device in meters.
#' - motion_data_count: The number of data points in the epoch (30).
#' - light_ambient_mean: The ambient light level during the epoch.
#' - sound_amplitude_mean: The sound amplitude during the epoch.
#' - temperature_ambient_mean: The ambient temperature during the epoch.
#' - humidity_mean: The ambient humidity during the epoch.
#' - pressure_mean: The ambient pressure during the epoch.
#' - indoor_air_quality_mean: The indoor air quality during the epoch.
#' - epoch_duration: The precise duration of the epoch (seconds).
#' - sleep_stage: The sleep stage as established with the VT algorithm. They are encoded as numbers 0-5
#' @source data-raw/SEtXSxcMEhYXKQAA.example_epochs_v1.csv
"example_epochs_v1"
