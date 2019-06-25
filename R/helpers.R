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
