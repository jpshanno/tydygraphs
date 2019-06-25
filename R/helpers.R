as_xts <-
  function(data, time){
    if(any(class(dplyr::pull(data, !!time)) == "Date")){
      xts::xts(x = data[, -c(1)],
               order.by = data[[1]])
    } else {

      tzone <-
        data %>%
        dplyr::summarize(tz = unique(lubridate::tz(!!time))) %>%
        dplyr::pull(tz)

      xts::xts(x = data[, -c(1)],
               order.by = data[[1]],
               tzone = tzone)
    }
  }


# fetch_time <-
#   function(d, t){
#
#     time <-
#       rlang::enquo(t)
#
#     if(rlang::quo_is_null(time)){
#       time <-
#         purrr::map_lgl(d,
#                        lubridate::is.timepoint) %>%
#         purrr::keep(isTRUE) %>%
#         names() %>%
#         rlang::sym()
#     }
#   }
