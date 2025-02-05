performance_data_example <- prepare_performance_data(
  probs = list(
    c(0.72, 0.63, 0.47, 0.45, 0.33, 0.31, 0.29, 0.18, 0.15, 0.11)
  ),
  reals = list(
    c(1, 1, 0, 1, 0, 1, 0, 0, 0, 0)
  ),
  by = 0.1, stratified_by = "ppcr"
)

example_curve_list <- rtichoke:::create_rtichoke_curve_list(
  performance_data_example, 
  "lift", 
  size = 300)


create_rtichoke_curve_by_frame <- function(performance_data, curve, frame) {
  
  example_curve_list <- rtichoke:::create_rtichoke_curve_list(
    performance_data, 
    curve, 
    size = 300)
  
  size_height <- example_curve_list$size[[1]] 
  
  interactive_marker <- list(
    size = 12, 
    line = list(width = 3, color = I("black")))
  
  interactive_marker$color <- "#f6e3be"
  
  plotly::config(plotly::layout(
    plotly::add_markers(
      plotly::add_trace(
        plotly::add_lines(
          plotly::plot_ly(
            x = ~x,
            y = ~y, height = size_height, width = example_curve_list$size[[1]],
            hoverinfo = "text", text = ~text, color = ~reference_group,
            colors = unlist(example_curve_list$group_colors_vec)
          ),
          data = example_curve_list$reference_data, line = list(dash = "dot")
        ),
        data = example_curve_list$performance_data_ready_for_curve,
        type = "scatter", mode = "lines+markers", line = list(dash = "solid")
      ),
      data = example_curve_list$performance_data_for_interactive_marker[frame,],
      marker = interactive_marker
    ),
    xaxis = list(
      showgrid = FALSE, fixedrange = TRUE, range = example_curve_list$axes_ranges$xaxis,
      title = example_curve_list$axes_labels$xaxis
    ),
    yaxis = list(
      showgrid = FALSE, fixedrange = TRUE, range = example_curve_list$axes_ranges$yaxis,
      title = example_curve_list$axes_labels$yaxis
    ),
    showlegend = FALSE, plot_bgcolor = "rgba(0, 0, 0, 0)",
    paper_bgcolor = "rgba(0, 0, 0, 0)"
  ),
  displayModeBar = FALSE
  )  
}

lift_example <- create_rtichoke_curve_by_frame(performance_data_example, "lift", 4)
gains_example <- create_rtichoke_curve_by_frame(performance_data_example, "gains", 4)

plotly::subplot(
  lift_example,
  gains_example, 
  nrows = 2
)




probs <- c(0.72, 0.63, 0.47, 0.45, 0.33, 0.31, 0.29, 0.18, 0.15, 0.11)
reals <- c(1, 1, 0, 1, 0, 1, 0, 0, 0, 0)

create_summary_report(
  probs = list(probs),
  reals = list(reals)
)

create_roc_curve(
  probs = list(example_dat$estimated_probabilities),
  reals = list(example_dat$outcome),
  stratified_by = "ppcr"
)

create_roc_curve(
  probs = list(
    "First Model" = example_dat$estimated_probabilities,
    "Second Model" = example_dat$random_guess
  ),
  reals = list(example_dat$outcome)
)


create_roc_curve(
  probs = list(
    "First Model" = example_dat$estimated_probabilities,
    "Second Model" = example_dat$random_guess
  ),
  reals = list(example_dat$outcome),
  stratified_by = "ppcr"
)


library(dplyr)

dat_c_index <- data.frame(
  probs, reals
) |> 
  mutate(
    reals_emoji = case_when(
      reals == 1 ~ "🤢",
      reals == 0 ~ "🤨"
    )
  )

library(plotly)
plot_ly(dat_c_index, x =~ probs) |> 
  add_text(y =~ reals_emoji, text =~ reals_emoji)


reals <- c(0, 1, 1, 0, 1, 1, 0, 0, 1, 0)

library(gt)
library(dplyr)
library(tibble)
library(glue)
library(tidyr)

original_data <-  tibble(
  reals = reals) %>%
  mutate(" " =ifelse(reals == 1, "🤢", "🤨"))



as.data.frame(
  matrix(rep(NA, 10), 10, 10)
) |>
  mutate(
    row_names_col = original_data$` `
  ) |>
  gt(rowname_col = "row_names_col") |>
  cols_label(
    V1 = "🤢",
    V2 = "🤢",
    V3 = "🤨",
    V4 = "🤢",
    V5 = "🤨",
    V6 = "🤢",
    V7 = "🤨",
    V8 = "🤨",
    V9 = "🤨",
    V10 = "🤨"
  ) |>
  sub_missing(
    missing_text = " "
  ) 


as.data.frame(
  matrix(rep(NA, 5), 5, 5)
) |>
  mutate(
    row_names_col = original_data$` `[original_data$` ` == "🤢"]
  ) |>
  gt(rowname_col = "row_names_col") |>
  cols_label(
    V1 = "🤨",
    V2 = "🤨",
    V3 = "🤨",
    V4 = "🤨",
    V5 = "🤨"
  ) |>
  sub_missing(
    missing_text = " "
  ) 



add_color_to_performance_metric_new <- function(performance_dat, metric, color) {
  performance_dat %>%
    dplyr::mutate(metric_plot = 100 * {{ metric }} , metric_plot = purrr::map2(metric_plot, {{ metric }}, .f = ~ bar_chart_new(
      value = .x, display = .y, color = color
    ))) %>%
    dplyr::mutate(`:=`({{ metric }}, .data$metric_plot)) %>%
    dplyr::select(-.data$metric_plot)
}


bar_chart_new <- function(value, display, color = "red"){
  
  display_metric <- glue::glue("{display} <br> ({round(display / 10, digits = 1)}%)")
  
  glue::glue("<span style=\"display: inline-block;direction: ltr;
             border-radius: 4px; padding-right: 2px;
             background-color: {color}; color: black;
             width: {value}%\">{display_metric}</span>") %>% 
    as.character() %>% 
    gt::html()
}


tibble::tibble(
  metric = c("**PPV** ", "**Prevalence**"),
  metric_value = c(
    0.3,
    0.4)) |>
  add_color_to_performance_metric_new(metric_value, "lightgreen") |> 
  gt::gt() |> 
  gt::cols_align(align = "left", columns = dplyr::everything()) |>
  gt::fmt_markdown(columns = metric)

library(magrittr)



performance_data_example <- prepare_performance_data(
  probs = list(
    c(0.72, 0.63, 0.47, 0.45, 0.33, 0.31, 0.29, 0.18, 0.15, 0.11)
  ),
  reals = list(
    c(1, 1, 0, 1, 0, 1, 0, 0, 0, 0)
  ),
  by = 0.1, stratified_by = "ppcr"
)

example_curve_list <- rtichoke:::create_rtichoke_curve_list(
  performance_data_example, 
  "lift", 
  size = 300)


create_rtichoke_curve_by_frame <- function(performance_data, curve, frame) {
  
  example_curve_list <- rtichoke:::create_rtichoke_curve_list(
    performance_data, 
    curve, 
    size = 300)
  
  size_height <- example_curve_list$size[[1]] 
  
  interactive_marker <- list(
    size = 12, 
    line = list(width = 3, color = I("black")))
  
  interactive_marker$color <- "#f6e3be"
  
  plotly::config(plotly::layout(
    plotly::add_markers(
      plotly::add_trace(
        plotly::add_lines(
          plotly::plot_ly(
            x = ~x,
            y = ~y, height = size_height, width = example_curve_list$size[[1]],
            hoverinfo = "text", text = ~text, color = ~reference_group,
            colors = unlist(example_curve_list$group_colors_vec)
          ),
          data = example_curve_list$reference_data, line = list(dash = "dot")
        ),
        data = example_curve_list$performance_data_ready_for_curve,
        type = "scatter", mode = "lines+markers", line = list(dash = "solid")
      ),
      data = example_curve_list$performance_data_for_interactive_marker[frame,],
      marker = interactive_marker
    ),
    xaxis = list(
      showgrid = FALSE, fixedrange = TRUE, range = example_curve_list$axes_ranges$xaxis,
      title = example_curve_list$axes_labels$xaxis
    ),
    yaxis = list(
      showgrid = FALSE, fixedrange = TRUE, range = example_curve_list$axes_ranges$yaxis,
      title = example_curve_list$axes_labels$yaxis
    ),
    showlegend = FALSE, plot_bgcolor = "rgba(0, 0, 0, 0)",
    paper_bgcolor = "rgba(0, 0, 0, 0)"
  ),
  displayModeBar = FALSE
  )  
}

lift_example <- create_rtichoke_curve_by_frame(performance_data_example, "lift", 4)
gains_example <- create_rtichoke_curve_by_frame(performance_data_example, "gains", 4)

plotly::subplot(
  lift_example,
  gains_example, 
  nrows = 2
)


# Old Possible Code

tibble::tibble(
  y = c(1, 1, 0, 1, 0, 1, 0, 0, 0, 0)
) |> 
  dplyr::mutate(
    emoji_trt = " ",
    emoji = ifelse( y == 1, "🤢", "🤨")
  ) |> 
  t() |>
  tibble::as_tibble() |> 
  gt::gt() |> 
  gt::tab_options(column_labels.hidden = TRUE) |> 
  gt::cols_width(
    gt::everything() ~ px(90)
  ) |>
  gt::tab_options(
    table.font.size = gt::px(50)
  ) |>
  gt::cols_align(align = "center", columns = dplyr::everything()) |> 
  gt::tab_style(
    style = list(
      "opacity: .2;"
    ),
    locations = gt::cells_body(columns = -c(V1, V2, V4, V6))
  )




turn_to_emoji <- function(binary_result) {
  if (binary_result == "TP") {
    "🤢"
  } else if (binary_result == "FP") {
    
  } else if (binary_result == "FN") {
    
  } else if (binary_result == "TN") {
    
  }
}  

tibble::tribble(
  ~"type", ~"predicted_positives", ~"predicted_negatives",
  "Real Positives", "TP", "FN",
  "Real Negatives", "FP", "TN"
) |>
  gt::gt() |> 
  gt::text_transform(
    locations = gt::cells_body(
      columns = c(predicted_positives)
    ),
    fn = function(x){
      glue::glue(
        "<div><span style='font-weight:bold;font-variant:small-caps;font-size:14px'>{x}</div>
        <div><span style ='font-weight:bold;color:grey;font-size:10px'>{x}</span></div>"
      )
    }
  )


dplyr::mutate(
  predicted_positives = ifelse(predicted_positive == "TP", "<br>🤢", "bla")
) |> 
  gt::gt()


performance_data_example <- prepare_performance_data(
  probs = list(
    c(0.72, 0.63, 0.47, 0.45, 0.33, 0.31, 0.29, 0.18, 0.15, 0.11)
  ),
  reals = list(
    c(1, 1, 0, 1, 0, 1, 0, 0, 0, 0)
  ),
  by = 0.1, stratified_by = "ppcr"
)

add_color_to_confusion_metric <- function(confusion_metric, color, n_obs = 10) {
  
  display_metric <- glue::glue("{confusion_metric} ({round(confusion_metric / n_obs, digits = 1)}%)")
  plot_metric <- rtichoke:::bar_chart(
    value = confusion_metric, display = display_metric, color = color, no_round = TRUE)
  
  plot_metric
  
}

bar_chart_new <- function(value, display, color = "red"){
  
  display_metric <- glue::glue("{display} ({round(display / 10, digits = 1)}%)")
  
  glue::glue("<span style=\"display: inline-block;direction: ltr;
             border-radius: 4px; padding-right: 2px;
             background-color: {color}; color: black;
             width: {value}%\">{display_metric}</span>") %>% 
    as.character() %>% 
    gt::html()
}

add_color_to_confusion_metric_new <- function(performance_dat, metric, color) {
  performance_dat %>%
    dplyr::mutate(metric_plot = 100 * {{ metric }} / 10, metric_plot = purrr::map2(metric_plot, {{ metric }}, .f = ~ bar_chart_new(
      value = .x, display = .y, color = color
    ))) %>%
    dplyr::mutate(`:=`({{ metric }}, .data$metric_plot)) %>%
    dplyr::select(-.data$metric_plot)
}

list_per_ppcr <- performance_data_example %>%
  split(., .$ppcr)

turn_performance_data_to_confusion_matrix_gt <- function(performance_data_gt) {
  
  tibble::tibble(
    "Predicted Positive" = c(
      performance_data_gt$TP,
      performance_data_gt$FN,
      3), 
    "Predicted Negative" = c(
      performance_data_gt$FP,
      performance_data_gt$TN, 
      6L
    ),
    " " = c(
      1L, 9L, 10L
    )) |> 
    gt::gt()
  
}


confusion_matrix_list <- performance_data_example |>
  dplyr::mutate(n_obs = 10,
                predicted_negatives = TN + FN,
                real_negatives = TN + FP,
                real_positives = TP + FN) |> 
  add_color_to_confusion_metric_new(predicted_negatives, "lightgray")|> 
  add_color_to_confusion_metric_new(real_negatives, "lightgray")|> 
  add_color_to_confusion_metric_new(real_positives, "lightgray")|> 
  add_color_to_confusion_metric_new(TP, "lightgreen") %>% 
  add_color_to_confusion_metric_new(TN, "lightgreen") %>% 
  add_color_to_confusion_metric_new(FP, "pink") %>% 
  add_color_to_confusion_metric_new(FN, "pink") |> 
  add_color_to_confusion_metric_new(n_obs, "lightgray")|>
  rtichoke:::add_color_to_predicted_positives() |> 
  dplyr::select(ppcr, TP, TN, FP, FN, 
                plot_predicted_positives, 
                real_negatives,
                real_positives,
                predicted_negatives,
                n_obs) 

turn_performance_data_to_confusion_matrix_gt <- function(performance_data_gt) {
  
  tibble::tibble(
    "Predicted Positive" = c(
      performance_data_gt$TP,
      performance_data_gt$FN,
      performance_data_gt$plot_predicted_positives), 
    "Predicted Negative" = c(
      performance_data_gt$FP,
      performance_data_gt$TN, 
      performance_data_gt$predicted_negatives
    ),
    " " = c(
      performance_data_gt$real_negatives, 
      performance_data_gt$real_positives, 
      performance_data_gt$n_obs
    )) |> 
    gt::gt()
  
}

confusion_matrix_list[1,] |>
  turn_performance_data_to_confusion_matrix_gt() |> 
  gt::cols_align(align = "left", columns = dplyr::everything())





