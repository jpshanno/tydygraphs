#' Methods for creating dygraphs from tidy data
#'
#' For many tidy datasets it is not convenient to use
#' \code{\link[dygraphs]{dygraph}} to create quick, interactive time series
#' plots. Methods are supplied for dataframes, tibbles, grouped tibbles, and
#' tsibbles. Currently the time column must be specified if data is of any of
#' the classes above.
#'
#' For any data object that is not a dataframe, tibble, grouped tibble, or
#' tsibble \code{\link[dygraphs]{dygraph}} is called directly. Otherwise a
#' dygraph is generated with a separate series for each column passed to
#' \code{...}. Additionally separate series are made for each unique group for
#' grouped tibbles and tsibbles (which are grouped based on key values).
#'
#' Be aware that because a separate series is made for each group and each
#' measure the plot can become overly busy quickly if too many groups or
#' measures are passed to the function.
#'
#' @param data The data
#' @param time The unquoted column name containing the time index. If not
#'   supplied the first column identified by
#'   \code{\link[lubridate]{is.timepoint}} will be used
#' @param ... Unquoted column name(s) of the desired series and any named
#'   arguments to \code{\link[dygraphs]{dygraph}}
#'
#' @return An interactive dygraph plot that can be modified with any other the
#' functions in the package dygraphs.
#' @export
#'
dygraph <-
  function(data, ..., time = NULL){

    UseMethod("dygraph", data)
  }

#' @rdname dygraph
#' @export
dygraph.default <-
  function(data, ...){
    dygraphs::dygraph(data, ...)
  }

#' @rdname dygraph
#' @export
dygraph.data.frame <-
  function(data,
           ...,
           time = NULL) {

    time <-
      rlang::enquo(time)

    if(rlang::quo_is_null(time)){
      time <-
        purrr::map_lgl(data,
                       lubridate::is.timepoint) %>%
        purrr::keep(isTRUE) %>%
        names() %>%
        .[1] %>%
        rlang::sym()
    }

    measures <-
      rlang::enquos(...)

    dy_args <-
      measures[names(measures) != ""]

    measures <-
      measures[names(measures) == ""]

    df <-
      data %>%
      dplyr::select(!!time,
                    !!!measures)

    df <-
      as_xts(df, time)

    dy_call <-
      rlang::expr(dygraphs::dygraph(df, !!!dy_args))

    rlang::eval_tidy(dy_call)
  }

#' @rdname dygraph
#' @export
dygraph.grouped_df <-
  function(data,
           ...,
           time = NULL) {

    time <-
      rlang::enquo(time)

    if(rlang::quo_is_null(time)){
      time <-
        purrr::map_lgl(data,
                       lubridate::is.timepoint) %>%
        purrr::keep(isTRUE) %>%
        names() %>%
        .[1] %>%
        rlang::sym()
    }

    measures <-
      rlang::enquos(...)

    dy_args <-
      measures[names(measures) != ""]

    measures <-
      measures[names(measures) == ""]

    group_var <-
      dplyr::groups(data)

    df <-
      data  %>%
      dplyr::ungroup() %>%
      dplyr::mutate(series = stringr::str_c(!!!group_var, sep = "_"))

    df <-
      purrr::map2(measures,
                  purrr::map(measures, rlang::as_name),
                  ~df %>%
                    dplyr::select(!!time,
                                  series,
                                  !!.x) %>%
                    dplyr::distinct(!!time,
                                    series,
                                    .keep_all = TRUE) %>%
                    tidyr::spread(series,
                                  !!.x,
                                  sep = "_") %>%
                    purrr::set_names(stringr::str_replace, "series", .y)) %>%
      purrr::reduce(dplyr::left_join,
                    by = rlang::as_name(time)) %>%
      as_xts(time)

    dy_call <-
      rlang::expr(dygraphs::dygraph(df, !!!dy_args))

    rlang::eval_tidy(dy_call)
  }

#' @rdname dygraph
#' @export
dygraph.tbl_ts <-
  function(data,
           ...) {

    time <-
      tsibble::index(data)

    measures <-
      rlang::enquos(...)

    dy_args <-
      measures[names(measures) != ""]

    measures <-
      measures[names(measures) == ""]

    group_var <-
      tsibble::key(data)

    df <-
      data  %>%
      dplyr::mutate(series = stringr::str_c(!!!group_var, sep = "_")) %>%
      tibble::as_tibble()

    df <-
      purrr::map2(measures,
                  purrr::map(measures, rlang::as_name),
                  ~df %>%
                    dplyr::select(!!time,
                                  series,
                                  !!.x) %>%
                    dplyr::distinct(!!time,
                                    series,
                                    .keep_all = TRUE) %>%
                    tidyr::spread(series,
                                  !!.x,
                                  sep = "_") %>%
                    purrr::set_names(stringr::str_replace, rlang::as_name("series"), .y)) %>%
      purrr::reduce(dplyr::left_join,
                    by = rlang::as_name(time)) %>%
      as_xts(time)

    dy_call <-
      rlang::expr(dygraphs::dygraph(df, !!!dy_args))

    rlang::eval_tidy(dy_call)
  }
