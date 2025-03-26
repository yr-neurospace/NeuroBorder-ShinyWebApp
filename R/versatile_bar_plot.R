versatile_bar_plot_ui <- function(id) {
    nav_panel(
        "Bar Plot",
        hr(),
        h2("1. Upload data"),
        fluidRow(
            column(4, fileInput(NS(id, "input_data_file"), "Upload a TSV file:"))
        ),
        hr(),
        h2("2. Preview data"),
        fluidRow(column(12, DT::dataTableOutput(NS(id, "output_data_table")))),
        fluidRow(
            column(4, uiOutput(NS(id, "output_x_column_options"))),
            column(4, uiOutput(NS(id, "output_y_column_options"))),
            column(4, uiOutput(NS(id, "output_fill_column_options")))
        ),
        fluidRow(
            column(4, uiOutput(NS(id, "output_color_column_options"))),
            column(4, uiOutput(NS(id, "output_alpha_column_options"))),
            column(4, uiOutput(NS(id, "output_shape_column_options")))
        ),
        hr(),
        h2("3. Set parameters"),
        card(
            card_header("Data-related parameters"),
            fluidRow(
                column(3, uiOutput(NS(id, "output_x_levels_options"))),
                column(3, uiOutput(NS(id, "output_fill_levels_options"))),
                column(3, uiOutput(NS(id, "output_color_levels_options"))),
                column(3, uiOutput(NS(id, "output_alpha_levels_options")))
            ),
            fluidRow(
                column(3, selectizeInput(NS(id, "error_bar_type"),
                    "Select error bar type:",
                    choices = c("sd", "sem"),
                    selected = "sd",
                    multiple = FALSE
                ))
            )
        ),
        card(
            card_header("Figure-related parameters"),
            fluidRow(
                column(3, uiOutput(NS(id, "output_font_families"))),
                column(3, numericInput(NS(id, "theme_base_font_size"),
                    "Base font size:",
                    value = 16, min = 0
                )),
                column(3, textInput(NS(id, "scale_y_expand_mult"),
                    "Y-axis expansions:",
                    value = "0,0"
                )),
                column(3, textInput(NS(id, "scale_y_breaks"),
                    "Y-axis breaks:",
                    value = "none"
                ))
            ),
            fluidRow(
                column(4, textInput(NS(id, "bar_fill_colors"),
                    "Bar fill colors:",
                    value = "none",
                    width = "100%"
                )),
                column(4, textInput(NS(id, "bar_color_colors"),
                    "Bar line colors:",
                    value = "none",
                    width = "100%"
                )),
                column(4, textInput(NS(id, "bar_alpha_values"),
                    "Bar transparencies:",
                    value = "none",
                    width = "100%"
                ))
            ),
            fluidRow(
                column(4, textInput(NS(id, "errorbar_color_colors"),
                    "Error bar line colors:",
                    value = "none",
                    width = "100%"
                )),
                column(4, textInput(NS(id, "errorbar_alpha_values"),
                    "Error bar transparencies:",
                    value = "none",
                    width = "100%"
                ))
            ),
            fluidRow(
                column(4, textInput(NS(id, "point_fill_colors"),
                    "Point fill colors:",
                    value = "none",
                    width = "100%"
                )),
                column(4, textInput(NS(id, "point_color_colors"),
                    "Point line colors:",
                    value = "none",
                    width = "100%"
                )),
                column(4, textInput(NS(id, "point_alpha_values"),
                    "Point transparencies:",
                    value = "none",
                    width = "100%"
                ))
            ),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "disable_bar_color_legend"),
                    "Disable bar color legend", TRUE,
                    width = "100%"
                )),
                column(3, checkboxInput(
                    NS(id, "disable_bar_fill_legend"),
                    "Disable bar fill legend", TRUE,
                    width = "100%"
                )),
                column(3, checkboxInput(
                    NS(id, "disable_bar_alpha_legend"),
                    "Disable bar transparency legend", TRUE,
                    width = "100%"
                ))
            ),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "disable_errorbar_color_legend"),
                    "Disable error bar color legend", TRUE,
                    width = "100%"
                )),
                column(3, checkboxInput(
                    NS(id, "disable_errorbar_alpha_legend"),
                    "Disable error bar transparency legend", TRUE,
                    width = "100%"
                ))
            ),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "disable_point_color_legend"),
                    "Disable point color legend", TRUE,
                    width = "100%"
                )),
                column(3, checkboxInput(
                    NS(id, "disable_point_fill_legend"),
                    "Disable point fill legend", TRUE,
                    width = "100%"
                )),
                column(3, checkboxInput(
                    NS(id, "disable_point_alpha_legend"),
                    "Disable point transparency legend", TRUE,
                    width = "100%"
                )),
                column(3, checkboxInput(
                    NS(id, "disable_point_shape_legend"),
                    "Disable point shape legend", TRUE,
                    width = "100%"
                ))
            ),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "theme_prism_panel_border"),
                    "Plot panel border", FALSE
                ))
            )
        ),
        card(
            card_header("Bar-related parameters"),
            fluidRow(
                column(2, numericInput(NS(id, "bar_just"),
                    "Bar just:",
                    value = 0.5, min = 0, max = 1
                )),
                column(2, numericInput(NS(id, "bar_width"),
                    "Bar width:",
                    value = 0.8, min = 0, max = 1
                )),
                column(2, numericInput(NS(id, "bar_linewidth"),
                    "Bar line width:",
                    value = 1, min = 0
                )),
                column(2, numericInput(NS(id, "bar_dodge_width"),
                    "Bar dodge width:",
                    value = 0.8, min = 0, max = 1
                )),
                column(4, selectizeInput(NS(id, "bar_dodge_preserve"),
                    "Select bar preserve type:",
                    choices = c("single", "total"),
                    selected = "single",
                    multiple = FALSE
                ))
            )
        ),
        card(
            card_header("Error bar-related parameters"),
            fluidRow(
                column(2, numericInput(NS(id, "errorbar_width"),
                    "Error bar width:",
                    value = 0.25, min = 0, max = 1
                )),
                column(3, numericInput(NS(id, "errorbar_linewidth"),
                    "Error bar line width:",
                    value = 1, min = 0
                )),
                column(3, numericInput(NS(id, "errorbar_dodge_width"),
                    "Error bar dodge width:",
                    value = 0.8, min = 0, max = 1
                )),
                column(4, selectizeInput(NS(id, "errorbar_dodge_preserve"),
                    "Select error bar preserve type:",
                    choices = c("single", "total"),
                    selected = "single",
                    multiple = FALSE
                ))
            )
        ),
        card(
            card_header("Point-related parameters"),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "show_point"),
                    "Show point", TRUE
                ))
            ),
            fluidRow(
                column(2, numericInput(NS(id, "point_x_offset"),
                    "X offset:",
                    value = -0.2
                )),
                column(2, numericInput(NS(id, "point_dodge_width"),
                    "Point dodge width:",
                    value = 0.8, min = 0, max = 1
                )),
                column(2, numericInput(NS(id, "point_size"),
                    "Point size:",
                    value = 2, min = 0
                )),
                column(2, numericInput(NS(id, "point_stroke"),
                    "Point stroke:",
                    value = 0.5, min = 0
                )),
                column(2, numericInput(NS(id, "point_jitter_width"),
                    "Point jitter width:",
                    value = 0, min = 0, max = 1
                )),
                column(2, numericInput(NS(id, "point_jitter_height"),
                    "Point jitter height:",
                    value = 0, min = 0, max = 1
                )),
            ),
            fluidRow(
                column(2, uiOutput(NS(id, "output_shape_levels_options"))),
                column(4, textInput(NS(id, "point_shape_values"),
                    "Point shapes (an integer 0-25 or a single character):",
                    value = "none",
                    width = "100%"
                ))
            )
        ),
        card(
            card_header("Miscellaneous outputs"),
            fluidRow(
                column(12, verbatimTextOutput(NS(id, "output_min_max_values")))
            ),
            fluidRow(
                column(12, verbatimTextOutput(NS(id, "output_stats_table")))
            )
        ),
        hr(),
        h2("5. Download figure"),
        fluidRow(column(3, downloadButton(NS(id, "download_figure"), "Download"))),
        hr(),
        h2("4. Preview figure"),
        fluidRow(
            column(2, numericInput(NS(id, "input_figure_width"),
                "Figure width:",
                value = 800, min = 0
            )),
            column(2, numericInput(NS(id, "input_figure_height"),
                "Figure height:",
                value = 800, min = 0
            ))
        ),
        fluidRow(column(12, imageOutput(NS(id, "output_preview_png_image"),
            width = "100%", height = "100%"
        )))
    )
}

versatile_bar_plot_server <- function(id, session, extra_params) {
    moduleServer(id, function(input, output, session) {
        input_data <- reactive({
            req(input$input_data_file)

            na.omit(vroom::vroom(input$input_data_file$datapath))
        })
        output$output_data_table <- DT::renderDataTable({
            req(input_data())

            DT::datatable(input_data())
        })
        output$output_x_column_options <- renderUI({
            req(
                input_data()
            )

            selectInput(NS(id, "x_column"),
                "Select x column:",
                names(input_data()),
                multiple = FALSE
            )
        })
        output$output_y_column_options <- renderUI({
            req(
                input_data()
            )

            selectInput(NS(id, "y_column"),
                "Select y column:",
                names(input_data()),
                multiple = FALSE
            )
        })
        output$output_fill_column_options <- renderUI({
            req(
                input_data(),
                input$x_column
            )

            x_column <- input$x_column

            selectInput(NS(id, "fill_column"),
                "Select fill column (also as group):",
                names(input_data()),
                multiple = FALSE,
                selected = x_column
            )
        })
        output$output_color_column_options <- renderUI({
            req(
                input_data()
            )

            selectInput(NS(id, "color_column"),
                "Select color column:",
                c("none", names(input_data())),
                multiple = FALSE,
                selected = "none"
            )
        })
        output$output_alpha_column_options <- renderUI({
            req(
                input_data()
            )

            selectInput(NS(id, "alpha_column"),
                "Select transparency column:",
                c("none", names(input_data())),
                multiple = FALSE,
                selected = "none"
            )
        })
        output$output_shape_column_options <- renderUI({
            req(
                input_data()
            )

            selectInput(NS(id, "shape_column"),
                "Select point shape column:",
                c("none", names(input_data())),
                multiple = FALSE,
                selected = "none"
            )
        })
        output$output_x_levels_options <- renderUI({
            req(input_data())

            x_column <- input$x_column

            selectInput(NS(id, "x_levels"),
                "Select x levels:",
                unique(input_data()[[x_column]]),
                multiple = TRUE,
                selected = unique(input_data()[[x_column]]),
                width = "100%"
            )
        })
        output$output_fill_levels_options <- renderUI({
            req(input_data())

            fill_column <- input$fill_column

            selectInput(NS(id, "fill_levels"),
                "Select fill levels:",
                unique(input_data()[[fill_column]]),
                multiple = TRUE,
                selected = unique(input_data()[[fill_column]]),
                width = "100%"
            )
        })
        output$output_color_levels_options <- renderUI({
            req(input_data())

            color_column <- input$color_column

            selectInput(NS(id, "color_levels"),
                "Select color levels:",
                if (color_column != "none") unique(input_data()[[color_column]]) else c(),
                multiple = TRUE,
                selected = if (color_column != "none") unique(input_data()[[color_column]]) else NULL,
                width = "100%"
            )
        })
        output$output_alpha_levels_options <- renderUI({
            req(input_data())

            alpha_column <- input$alpha_column

            selectInput(NS(id, "alpha_levels"),
                "Select transparency levels:",
                if (alpha_column != "none") unique(input_data()[[alpha_column]]) else c(),
                multiple = TRUE,
                selected = if (alpha_column != "none") unique(input_data()[[alpha_column]]) else NULL,
                width = "100%"
            )
        })
        output$output_shape_levels_options <- renderUI({
            req(input_data())

            shape_column <- input$shape_column

            selectInput(NS(id, "shape_levels"),
                "Select point shape levels:",
                if (shape_column != "none") unique(input_data()[[shape_column]]) else c(),
                multiple = TRUE,
                selected = if (shape_column != "none") unique(input_data()[[shape_column]]) else NULL,
                width = "100%"
            )
        })
        available_font_families <- sort(unique(systemfonts::system_fonts()[["family"]]))
        output$output_font_families <- renderUI({
            selectizeInput(NS(id, "font_family"),
                label = "Select font family:",
                choices = available_font_families,
                multiple = FALSE,
                selected = ifelse("Arial" %in% available_font_families, "Arial", available_font_families[1])
            )
        })
        output$output_min_max_values <- renderText({
            req(
                input_data(),
                input$y_column
            )

            data <- input_data()
            y_column <- input$y_column

            paste0(
                "Minimum value: ", min(data[[y_column]]), "\n",
                "Maximum value: ", max(data[[y_column]])
            )
        })
        grouped_stats_df <- reactive({
            req(
                input_data(),
                input$fill_column,
                input$y_column
            )

            suppressWarnings(suppressMessages(library(tidyverse)))

            data <- input_data()
            fill_column <- input$fill_column
            y_column <- input$y_column

            data %>%
                group_by(.data[[fill_column]]) %>%
                reframe(
                    mean = mean(.data[[y_column]]),
                    sd = sd(.data[[y_column]]),
                    n = n(),
                    sem = sd / sqrt(n)
                )
        })
        plot_data <- reactive({
            req(
                grouped_stats_df()
            )

            suppressWarnings(suppressMessages(library(tidyverse)))

            data <- input_data()
            grouped_stats_df <- grouped_stats_df()
            x_column <- input$x_column
            fill_column <- input$fill_column
            color_column <- input$color_column
            alpha_column <- input$alpha_column
            shape_column <- input$shape_column

            other_columns <- unique(c(
                x_column, fill_column,
                if (color_column != "none") color_column else NULL,
                if (alpha_column != "none") alpha_column else NULL,
                if (shape_column != "none") shape_column else NULL
            ))

            inner_join(distinct(data[, other_columns]),
                grouped_stats_df,
                by = fill_column
            ) %>% distinct()
        })
        output$output_stats_table <- renderText({
            req(
                plot_data()
            )

            data <- plot_data()

            paste0(capture.output(print(data)), collapse = "\n")
        })
        output_plot <- reactive({
            req(
                plot_data()
            )

            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(ggprism)))
            suppressWarnings(suppressMessages(library(ggnewscale)))

            data <- input_data()
            plot_data <- plot_data()

            x_column <- input$x_column
            y_column <- input$y_column
            fill_column <- input$fill_column
            color_column <- input$color_column
            alpha_column <- input$alpha_column
            shape_column <- input$shape_column

            x_levels <- input$x_levels
            fill_levels <- input$fill_levels
            color_levels <- input$color_levels
            alpha_levels <- input$alpha_levels
            shape_levels <- input$shape_levels

            error_bar_type <- input$error_bar_type

            scale_y_expand_mult <- as.numeric(trimws(strsplit(input$scale_y_expand_mult, ",", fixed = TRUE)[[1]]))
            scale_y_breaks <- input$scale_y_breaks
            if (scale_y_breaks != "none") {
                scale_y_breaks <- as.numeric(trimws(strsplit(scale_y_breaks, ",", fixed = TRUE)[[1]]))
                scale_y_limits <- c(min(scale_y_breaks), max(scale_y_breaks))
            }

            bar_just <- input$bar_just
            bar_width <- input$bar_width
            bar_linewidth <- input$bar_linewidth
            bar_dodge_width <- input$bar_dodge_width
            bar_dodge_preserve <- input$bar_dodge_preserve
            disable_bar_fill_legend <- input$disable_bar_fill_legend
            disable_bar_color_legend <- input$disable_bar_color_legend
            disable_bar_alpha_legend <- input$disable_bar_alpha_legend
            bar_fill_colors <- trimws(strsplit(input$bar_fill_colors, ",", fixed = TRUE)[[1]])
            bar_color_colors <- trimws(strsplit(input$bar_color_colors, ",", fixed = TRUE)[[1]])
            bar_alpha_values <- input$bar_alpha_values
            if (bar_alpha_values != "none") {
                bar_alpha_values <- as.numeric(trimws(strsplit(input$bar_alpha_values, ",", fixed = TRUE)[[1]]))
            }

            errorbar_linewidth <- input$errorbar_linewidth
            errorbar_width <- input$errorbar_width
            errorbar_dodge_width <- input$errorbar_dodge_width
            errorbar_dodge_preserve <- input$errorbar_dodge_preserve
            disable_errorbar_color_legend <- input$disable_errorbar_color_legend
            disable_errorbar_alpha_legend <- input$disable_errorbar_alpha_legend
            errorbar_color_colors <- trimws(strsplit(input$errorbar_color_colors, ",", fixed = TRUE)[[1]])
            errorbar_alpha_values <- input$errorbar_alpha_values
            if (errorbar_alpha_values != "none") {
                errorbar_alpha_values <- as.numeric(trimws(strsplit(input$errorbar_alpha_values, ",", fixed = TRUE)[[1]]))
            }

            show_point <- input$show_point
            point_x_offset <- input$point_x_offset
            point_dodge_width <- input$point_dodge_width
            point_size <- input$point_size
            point_stroke <- input$point_stroke
            point_jitter_width <- input$point_jitter_width
            point_jitter_height <- input$point_jitter_height
            disable_point_fill_legend <- input$disable_point_fill_legend
            disable_point_color_legend <- input$disable_point_color_legend
            disable_point_alpha_legend <- input$disable_point_alpha_legend
            disable_point_shape_legend <- input$disable_point_shape_legend
            point_fill_colors <- trimws(strsplit(input$point_fill_colors, ",", fixed = TRUE)[[1]])
            point_color_colors <- trimws(strsplit(input$point_color_colors, ",", fixed = TRUE)[[1]])
            point_alpha_values <- input$point_alpha_values
            if (point_alpha_values != "none") {
                point_alpha_values <- as.numeric(trimws(strsplit(input$point_alpha_values, ",", fixed = TRUE)[[1]]))
            }
            point_shape_values <- as.numeric(trimws(strsplit(input$point_shape_values, ",", fixed = TRUE)[[1]]))
            if (any(is.na(point_shape_values))) {
                point_shape_values <- trimws(strsplit(input$point_shape_values, ",", fixed = TRUE)[[1]])
            }

            theme_prism_panel_border <- input$theme_prism_panel_border
            font_family <- input$font_family
            theme_base_font_size <- input$theme_base_font_size

            # compared to fill_column, x_column has priority
            data[[fill_column]] <- factor(data[[fill_column]], levels = fill_levels)
            data[[x_column]] <- factor(data[[x_column]], levels = x_levels)

            # compared to fill_column, x_column has priority
            plot_data[[fill_column]] <- factor(plot_data[[fill_column]], levels = fill_levels)
            plot_data[[x_column]] <- factor(plot_data[[x_column]], levels = x_levels)

            # fill_column is also used as group column
            if (fill_column != x_column) {
                grouped <- TRUE
            } else {
                grouped <- FALSE
                x_offset_df <- data[, x_column] %>%
                    distinct() %>%
                    arrange(.data[[x_column]]) %>%
                    mutate(
                        x_column_rank = 1:n(),
                        x_offset_column = x_column_rank + point_x_offset
                    )
                data <- inner_join(x_offset_df, data, by = x_column)
            }

            p <- ggplot(plot_data, aes(
                x = .data[[x_column]],
                fill = .data[[fill_column]]
            )) +
                geom_bar(
                    mapping = aes(
                        y = mean,
                        color = if (color_column != "none") .data[[color_column]] else NULL,
                        alpha = if (alpha_column != "none") .data[[alpha_column]] else NULL
                    ),
                    stat = "identity",
                    position = if (grouped) {
                        position_dodge(
                            width = bar_dodge_width,
                            preserve = bar_dodge_preserve
                        )
                    } else {
                        position_identity()
                    },
                    just = bar_just,
                    width = bar_width,
                    linewidth = bar_linewidth
                )

            if (all(bar_fill_colors != "none")) {
                bar_fill_df <- tibble(color = bar_fill_colors, level = fill_levels)
                p <- p + scale_fill_manual(values = setNames(bar_fill_df[["color"]], bar_fill_df[["level"]]))
            }
            if (color_column != "none" && all(bar_color_colors != "none")) {
                bar_color_df <- tibble(color = bar_color_colors, level = color_levels)
                p <- p + scale_color_manual(values = setNames(bar_color_df[["color"]], bar_color_df[["level"]]))
            }
            if (alpha_column != "none" && all(bar_alpha_values != "none")) {
                bar_alpha_df <- tibble(value = bar_alpha_values, level = alpha_levels)
                p <- p + scale_alpha_manual(values = setNames(bar_alpha_df[["value"]], bar_alpha_df[["level"]]))
            }

            if (disable_bar_fill_legend) {
                p <- p + guides(fill = "none")
            }
            if (color_column != "none" && disable_bar_color_legend) {
                p <- p + guides(color = "none")
            }
            if (alpha_column != "none" && disable_bar_alpha_legend) {
                p <- p + guides(alpha = "none")
            }

            p <- p +
                new_scale("color") +
                new_scale("alpha") +
                geom_errorbar(
                    mapping = aes(
                        ymin = mean - .data[[error_bar_type]],
                        ymax = mean + .data[[error_bar_type]],
                        color = if (color_column != "none") .data[[color_column]] else NULL,
                        alpha = if (alpha_column != "none") .data[[alpha_column]] else NULL
                    ),
                    position = if (grouped) {
                        position_dodge(
                            width = errorbar_dodge_width,
                            preserve = errorbar_dodge_preserve
                        )
                    } else {
                        position_identity()
                    },
                    linewidth = errorbar_linewidth,
                    width = errorbar_width
                )

            if (color_column != "none" && all(errorbar_color_colors != "none")) {
                errorbar_color_df <- tibble(color = errorbar_color_colors, level = color_levels)
                p <- p + scale_color_manual(values = setNames(errorbar_color_df[["color"]], errorbar_color_df[["level"]]))
            }
            if (alpha_column != "none" && all(errorbar_alpha_values != "none")) {
                errorbar_alpha_df <- tibble(value = errorbar_alpha_values, level = alpha_levels)
                p <- p + scale_alpha_manual(values = setNames(errorbar_alpha_df[["value"]], errorbar_alpha_df[["level"]]))
            }

            if (color_column != "none" && disable_errorbar_color_legend) {
                p <- p + guides(color = "none")
            }
            if (alpha_column != "none" && disable_errorbar_alpha_legend) {
                p <- p + guides(alpha = "none")
            }

            if (show_point) {
                p <- p +
                    new_scale("fill") +
                    new_scale("color") +
                    new_scale("alpha") +
                    geom_jitter(
                        data = data,
                        mapping = aes(
                            x = if (grouped) .data[[x_column]] else .data[["x_offset_column"]],
                            y = .data[[y_column]],
                            fill = .data[[fill_column]],
                            color = if (color_column != "none") .data[[color_column]] else NULL,
                            alpha = if (alpha_column != "none") .data[[alpha_column]] else NULL,
                            shape = if (shape_column != "none") .data[[shape_column]] else NULL
                        ),
                        position = position_jitterdodge(
                            jitter.width = point_jitter_width,
                            jitter.height = point_jitter_height,
                            dodge.width = if (grouped) point_dodge_width else 0
                        ),
                        size = point_size,
                        stroke = point_stroke
                    )

                if (all(point_fill_colors != "none")) {
                    point_fill_df <- tibble(color = point_fill_colors, level = fill_levels)
                    p <- p + scale_fill_manual(values = setNames(point_fill_df[["color"]], point_fill_df[["level"]]))
                }
                if (color_column != "none" && all(point_color_colors != "none")) {
                    point_color_df <- tibble(color = point_color_colors, level = color_levels)
                    p <- p + scale_color_manual(values = setNames(point_color_df[["color"]], point_color_df[["level"]]))
                }
                if (alpha_column != "none" && all(point_alpha_values != "none")) {
                    point_alpha_df <- tibble(value = point_alpha_values, level = alpha_levels)
                    p <- p + scale_alpha_manual(values = setNames(point_alpha_df[["value"]], point_alpha_df[["level"]]))
                }
                if (shape_column != "none" && all(point_shape_values != "none")) {
                    point_shape_df <- tibble(value = point_shape_values, level = shape_levels)
                    p <- p + scale_shape_manual(values = setNames(point_shape_df[["value"]], point_shape_df[["level"]]))
                }

                if (disable_point_fill_legend) {
                    p <- p + guides(fill = "none")
                }
                if (color_column != "none" && disable_point_color_legend) {
                    p <- p + guides(color = "none")
                }
                if (alpha_column != "none" && disable_point_alpha_legend) {
                    p <- p + guides(alpha = "none")
                }
                if (shape_column != "none" && disable_point_shape_legend) {
                    p <- p + guides(shape = "none")
                }
            }

            p <- p + scale_y_continuous(
                expand = expansion(mult = scale_y_expand_mult),
                breaks = if (all(scale_y_breaks != "none")) scale_y_breaks else waiver(),
                limits = if (all(scale_y_breaks != "none")) scale_y_limits else NULL
            )

            p + theme_prism(
                border = theme_prism_panel_border,
                base_family = font_family,
                base_size = theme_base_font_size
            ) +
                theme(legend.title = element_text())
        })
        output$output_preview_png_image <- renderImage(
            {
                req(output_plot())

                input_figure_width <- input$input_figure_width
                input_figure_height <- input$input_figure_height

                output_figure_file <- file.path(
                    extra_params$temp_dir,
                    paste0(extra_params$uni_prefix, "_bar.png")
                )

                unigd::ugd()
                unigd::ugd_save_inline(
                    {
                        print(output_plot())
                    },
                    file = output_figure_file,
                    width = input_figure_width,
                    height = input_figure_height
                )
                unigd::ugd_close(all = TRUE)

                list(
                    src = output_figure_file,
                    width = paste0(input_figure_width, "px"),
                    height = paste0(input_figure_height, "px")
                )
            },
            deleteFile = TRUE
        )
        output$download_figure <- downloadHandler(
            filename = function() {
                paste0(extra_params$uni_prefix, "_bar.pdf")
            },
            content = function(file) {
                input_figure_width <- input$input_figure_width
                input_figure_height <- input$input_figure_height

                unigd::ugd()
                unigd::ugd_save_inline(
                    {
                        print(output_plot())
                    },
                    file = file,
                    width = input_figure_width,
                    height = input_figure_height
                )
                unigd::ugd_close(all = TRUE)
            }
        )
    })
}
