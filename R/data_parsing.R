sessions_adapters <- list(
  list(
    name = "ggir_start_end_window",
    detects = \(df) "start_end_window" %in% names(df) && "calendar_date" %in% names(df),
    apply = \(df) {
      df |>
        dplyr::mutate(
          session_start = sub("-.*$", "", start_end_window) |> parse_time() |> update_date(calendar_date),
          session_end   = sub("^[^-]*-", "", start_end_window) |> parse_time() |> update_date(calendar_date) + lubridate::days(1)
        ) |>
        set_colnames(list(session_start = "session_start", session_end = "session_end"))
    }
  ),

  list(
    name = "ggir_workday",
    detects = \(df) identical(get_colnames(df)$is_workday, "daytype"),
    apply = \(df) dplyr::mutate(df, is_workday = ifelse(daytype == "WD", TRUE, FALSE))
  ),

  list(
    name = "ggir_part5_sleep_period_min_to_sec",
    detects = \(df) identical(get_colnames(df)$sleep_period, "dur_spt_sleep_min"),
    apply = \(df) dplyr::mutate(df, sleep_period = dur_spt_sleep_min * 60)
  ),

  list(
    name = "ggir_part4_sleep_period_hour_to_sec",
    detects = \(df) identical(get_colnames(df)$sleep_period, "SleepDurationInSpt"),
    apply = \(df) dplyr::mutate(df, sleep_period = SleepDurationInSpt * 60 * 60)
  ),

  list(
    name = "sleep_diary_sleep_onset_min_to_sec",
    detects = \(df) identical(get_colnames(df)$sleep_onset_latency, "onset_latency_min"),
    apply = \(df) dplyr::mutate(df, sleep_onset_latency = onset_latency_min * 60)
  )
)

sessions_rules <- list(
  list(
    requires = NULL,
    produces = "id",
    apply = \(df) dplyr::mutate(df, id = as.character(dplyr::row_number()))
  ),

  list(
    requires = "session_start",
    produces = "night",
    apply = \(df) group_sessions_by_night(df)
  ),

  list(
    requires = c("session_start", "sleep_onset_latency"),
    produces = "time_at_sleep_onset",
    apply = \(df) dplyr::mutate(df, time_at_sleep_onset = session_start + sleep_onset_latency)
  ),

  list(
    requires = c("session_start", "time_at_sleep_onset"),
    produces = "sleep_onset_latency",
    apply = \(df) dplyr::mutate(df, sleep_onset_latency = time_diff(session_start, time_at_sleep_onset, unit = "second"))
  ),

  list(
    requires = c("session_start", "session_end"),
    produces = "time_in_bed",
    apply = \(df) dplyr::mutate(df, time_in_bed = time_diff(session_start, session_end, unit = "second"))
  ),

  list(
    requires = c("time_at_sleep_onset", "time_at_wakeup"),
    produces = "sleep_period",
    apply = \(df) dplyr::mutate(df, sleep_period = time_diff(time_at_sleep_onset, time_at_wakeup, unit = "second"))
  ),

  list(
    requires = c("time_at_sleep_onset", "sleep_period"),
    produces = "time_at_midsleep",
    apply = \(df) dplyr::mutate(df, time_at_midsleep = time_at_sleep_onset + (sleep_period / 2))
  ),

  list(
    requires = "night",
    produces = "is_workday",
    apply = \(df) dplyr::mutate(df, is_workday = !(weekdays(night) %in% c("Saturday", "Sunday")))
  )
)

epochs_adapters <- list(
  list(
    name = "ggir_timenum_to_POSIXct",
    detects = \(df) identical("timenum", get_colnames(df)$timestamp),
    apply = \(df) {
      df |>
        dplyr::mutate(
          timestamp = as.POSIXct(.data$timestamp, origin = "1970-01-01", tz = "Europe/London")
        )
    }
  ),

  list(
    name = "ggir_create_is_asleep",
    detects = \(df) identical("class_id", get_colnames(df)$sleep_stage),
    apply = \(df) dplyr::mutate(df, is_asleep = ifelse(class_id == 0, 1, 0))
  ),

  list(
    name = "somnofy_create_is_asleep",
    detects = \(df) identical("sleep_stage", get_colnames(df)$sleep_stage),
    apply = \(df) dplyr::mutate(df, is_asleep = ifelse(sleep_stage %in% c(4, 5), 0, 1))
  )
)

epochs_rules <- list(
  list(
    requires = "timestamp",
    produces = "night",
    apply = \(df) group_epochs_by_night(df)
  )
)
