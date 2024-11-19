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
            column(4, uiOutput(NS(id, "output_alpha_column_options")))
        ),
        hr(),
        h2("3. Set parameters"),
        card(
            card_header("Data-related parameters"),
            fluidRow(
                column(6, uiOutput(NS(id, "output_x_levels_options"))),
                column(6, uiOutput(NS(id, "output_fill_levels_options")))
            ),
            fluidRow(
                column(6, uiOutput(NS(id, "output_color_levels_options"))),
                column(6, uiOutput(NS(id, "output_alpha_levels_options")))
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
                column(2, uiOutput(NS(id, "output_font_families"))),
                column(2, numericInput(NS(id, "theme_base_font_size"),
                    "Base font size:",
                    value = 16, min = 0
                )),
                column(2, textInput(NS(id, "scale_y_expand_mult"),
                    "Y-axis expansions:",
                    value = "0.05,0"
                )),
                column(2, textInput(NS(id, "scale_y_breaks"),
                    "Y-axis breaks:",
                    value = "none"
                ))
            ),
            fluidRow(
                column(4, textInput(NS(id, "fill_colors"),
                    "Fill colors:",
                    value = "none",
                    width = "100%"
                )),
                column(4, textInput(NS(id, "color_colors"),
                    "Color colors:",
                    value = "none",
                    width = "100%"
                )),
                column(4, textInput(NS(id, "alpha_values"),
                    "Transparencies:",
                    value = "none",
                    width = "100%"
                ))
            ),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "disable_color_legend"),
                    "Disable color legend", FALSE
                )),
                column(3, checkboxInput(
                    NS(id, "disable_fill_legend"),
                    "Disable fill legend", FALSE
                )),
                column(3, checkboxInput(
                    NS(id, "disable_alpha_legend"),
                    "Disable transparency legend", FALSE
                )),
                column(3, checkboxInput(
                    NS(id, "theme_prism_panel_border"),
                    "Plot panel border", FALSE
                ))
            )
        ),
        card(
            card_header("Bar-related parameters"),
            fluidRow(
                column(2, textInput(NS(id, "bar_color"),
                    "Bar color:",
                    value = "black"
                )),
                column(2, numericInput(NS(id, "bar_alpha"),
                    "Bar alpha:",
                    value = 1, min = 0, max = 1
                )),
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
            ),
            fluidRow(
                column(3, selectizeInput(NS(id, "bar_dodge_preserve"),
                    "Select bar preserve type:",
                    choices = c("single", "total"),
                    selected = "single",
                    multiple = FALSE
                ))
            ),
            fluidRow(
                column(6, checkboxInput(
                    NS(id, "disable_bar_color_alpha_aesthetics"),
                    "Disable color and transparency aesthetics", FALSE,
                    width = "100%"
                ))
            )
        ),
        card(
            card_header("Error bar-related parameters"),
            fluidRow(
                column(2, textInput(NS(id, "errorbar_color"),
                    "Error bar color:",
                    value = "black"
                )),
                column(2, numericInput(NS(id, "errorbar_alpha"),
                    "Error bar alpha:",
                    value = 1, min = 0, max = 1
                )),
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
            ),
            fluidRow(
                column(3, selectizeInput(NS(id, "errorbar_dodge_preserve"),
                    "Select error bar preserve type:",
                    choices = c("single", "total"),
                    selected = "single",
                    multiple = FALSE
                ))
            ),
            fluidRow(
                column(6, checkboxInput(
                    NS(id, "disable_errorbar_color_alpha_aesthetics"),
                    "Disable color and transparency aesthetics", FALSE,
                    width = "100%"
                ))
            )
        ),
        card(
            card_header("Point-related parameters"),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "show_point"),
                    "Show point", TRUE
                )),
                column(6, checkboxInput(
                    NS(id, "disable_point_color_alpha_aesthetics"),
                    "Disable color and transparency aesthetics", FALSE,
                    width = "100%"
                ))
            ),
            fluidRow(
                column(2, numericInput(NS(id, "point_x_offset"),
                    "X offset:",
                    value = -0.2
                )),
                column(2, textInput(NS(id, "point_color"),
                    "Point color:",
                    value = "black"
                )),
                column(2, numericInput(NS(id, "point_alpha"),
                    "Point alpha:",
                    value = 1, min = 0, max = 1
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
                ))
            ),
            fluidRow(
                column(2, numericInput(NS(id, "point_jitter_width"),
                    "Point jitter width:",
                    value = 0, min = 0, max = 1
                )),
                column(2, numericInput(NS(id, "point_jitter_height"),
                    "Point jitter height:",
                    value = 0, min = 0, max = 1
                )),
                column(4, textInput(NS(id, "point_shape"),
                    "Point shape (an integer 0-25 or a single character):",
                    value = "21",
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
                "Select fill column:",
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
        available_font_families <- sort(sysfonts::font_files()[["family"]])
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

            other_columns <- unique(c(
                x_column, fill_column,
                if (color_column != "none") color_column else NULL,
                if (alpha_column != "none") alpha_column else NULL
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
            suppressWarnings(suppressMessages(library(YRUtils)))

            data <- input_data()
            plot_data <- plot_data()

            x_column <- input$x_column
            y_column <- input$y_column
            x_levels <- input$x_levels

            error_bar_type <- input$error_bar_type

            fill_column <- input$fill_column
            fill_colors <- trimws(strsplit(input$fill_colors, ",", fixed = TRUE)[[1]])
            fill_levels <- input$fill_levels

            alpha_column <- input$alpha_column
            alpha_values <- input$alpha_values
            if (alpha_values != "none") {
                alpha_values <- as.numeric(trimws(strsplit(input$alpha_values, ",", fixed = TRUE)[[1]]))
            }
            alpha_levels <- input$alpha_levels

            color_column <- input$color_column
            color_colors <- trimws(strsplit(input$color_colors, ",", fixed = TRUE)[[1]])
            color_levels <- input$color_levels

            scale_y_expand_mult <- as.numeric(trimws(strsplit(input$scale_y_expand_mult, ",", fixed = TRUE)[[1]]))
            scale_y_breaks <- input$scale_y_breaks
            if (scale_y_breaks != "none") {
                scale_y_breaks <- as.numeric(trimws(strsplit(scale_y_breaks, ",", fixed = TRUE)[[1]]))
                scale_y_limits <- c(min(scale_y_breaks), max(scale_y_breaks))
            }

            bar_color <- input$bar_color
            bar_alpha <- input$bar_alpha
            bar_just <- input$bar_just
            bar_width <- input$bar_width
            bar_linewidth <- input$bar_linewidth
            disable_bar_color_alpha_aesthetics <- input$disable_bar_color_alpha_aesthetics
            bar_dodge_width <- input$bar_dodge_width
            bar_dodge_preserve <- input$bar_dodge_preserve

            errorbar_color <- input$errorbar_color
            errorbar_alpha <- input$errorbar_alpha
            errorbar_linewidth <- input$errorbar_linewidth
            errorbar_width <- input$errorbar_width
            disable_errorbar_color_alpha_aesthetics <- input$disable_errorbar_color_alpha_aesthetics
            errorbar_dodge_width <- input$errorbar_dodge_width
            errorbar_dodge_preserve <- input$errorbar_dodge_preserve

            show_point <- input$show_point
            point_x_offset <- input$point_x_offset
            point_color <- input$point_color
            point_alpha <- input$point_alpha
            point_dodge_width <- input$point_dodge_width
            point_size <- input$point_size
            point_shape <- as.numeric(input$point_shape)
            if (is.na(point_shape)) {
                point_shape <- input$point_shape
            }
            point_stroke <- input$point_stroke
            point_jitter_width <- input$point_jitter_width
            point_jitter_height <- input$point_jitter_height
            disable_point_color_alpha_aesthetics <- input$disable_point_color_alpha_aesthetics

            disable_color_legend <- input$disable_color_legend
            disable_fill_legend <- input$disable_fill_legend
            disable_alpha_legend <- input$disable_alpha_legend

            theme_prism_panel_border <- input$theme_prism_panel_border
            font_family <- input$font_family
            theme_base_font_size <- input$theme_base_font_size

            set_fonts(font_families = font_family)

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

            data[[x_column]] <- factor(data[[x_column]], levels = x_levels)
            data[[fill_column]] <- factor(data[[fill_column]], levels = fill_levels)
            if (color_column != "none") {
                data[[color_column]] <- factor(data[[color_column]], levels = color_levels)
            }
            if (alpha_column != "none") {
                data[[alpha_column]] <- factor(data[[alpha_column]], levels = alpha_levels)
            }

            plot_data[[x_column]] <- factor(plot_data[[x_column]], levels = x_levels)
            plot_data[[fill_column]] <- factor(plot_data[[fill_column]], levels = fill_levels)
            if (color_column != "none") {
                plot_data[[color_column]] <- factor(plot_data[[color_column]], levels = color_levels)
            }
            if (alpha_column != "none") {
                plot_data[[alpha_column]] <- factor(plot_data[[alpha_column]], levels = alpha_levels)
            }

            geom_bar_args <- list(
                disable_bar_color_alpha_aesthetics = list(
                    mapping = aes(
                        y = mean
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
                    color = bar_color,
                    alpha = bar_alpha,
                    just = bar_just,
                    width = bar_width,
                    linewidth = bar_linewidth
                ),
                enable_bar_color_alpha_aesthetics = list(
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
            )
            geom_errorbar_args <- list(
                disable_errorbar_color_alpha_aesthetics = list(
                    mapping = aes(
                        ymin = mean - .data[[error_bar_type]],
                        ymax = mean + .data[[error_bar_type]]
                    ),
                    position = if (grouped) {
                        position_dodge(
                            width = errorbar_dodge_width,
                            preserve = errorbar_dodge_preserve
                        )
                    } else {
                        position_identity()
                    },
                    color = errorbar_color,
                    alpha = errorbar_alpha,
                    linewidth = errorbar_linewidth,
                    width = errorbar_width
                ),
                enable_errorbar_color_alpha_aesthetics = list(
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
            )
            geom_point_args <- list(
                disable_point_color_alpha_aesthetics = list(
                    data = data,
                    mapping = aes(
                        x = if (grouped) .data[[x_column]] else .data[["x_offset_column"]],
                        y = .data[[y_column]],
                        fill = .data[[fill_column]]
                    ),
                    position = position_jitterdodge(
                        jitter.width = point_jitter_width,
                        jitter.height = point_jitter_height,
                        dodge.width = if (grouped) point_dodge_width else 0
                    ),
                    color = point_color,
                    alpha = point_alpha,
                    size = point_size,
                    shape = point_shape,
                    stroke = point_stroke
                ),
                enable_point_color_alpha_aesthetics = list(
                    data = data,
                    mapping = aes(
                        x = if (grouped) .data[[x_column]] else .data[["x_offset_column"]],
                        y = .data[[y_column]],
                        fill = .data[[fill_column]],
                        color = if (color_column != "none") .data[[color_column]] else NULL,
                        alpha = if (alpha_column != "none") .data[[alpha_column]] else NULL
                    ),
                    position = position_jitterdodge(
                        jitter.width = point_jitter_width,
                        jitter.height = point_jitter_height,
                        dodge.width = if (grouped) point_dodge_width else 0
                    ),
                    size = point_size,
                    shape = point_shape,
                    stroke = point_stroke
                )
            )

            p <- ggplot(plot_data, aes(
                x = .data[[x_column]],
                fill = .data[[fill_column]]
            )) +
                do.call(geom_bar, if (disable_bar_color_alpha_aesthetics) {
                    geom_bar_args[["disable_bar_color_alpha_aesthetics"]]
                } else {
                    geom_bar_args[["enable_bar_color_alpha_aesthetics"]]
                }) +
                do.call(geom_errorbar, if (disable_errorbar_color_alpha_aesthetics) {
                    geom_errorbar_args[["disable_errorbar_color_alpha_aesthetics"]]
                } else {
                    geom_errorbar_args[["enable_errorbar_color_alpha_aesthetics"]]
                })
            if (show_point) {
                p <- p + do.call(geom_jitter, if (disable_point_color_alpha_aesthetics) {
                    geom_point_args[["disable_point_color_alpha_aesthetics"]]
                } else {
                    geom_point_args[["enable_point_color_alpha_aesthetics"]]
                })
            }
            if (fill_column != "none" && all(fill_colors != "none")) {
                fill_df <- tibble(color = fill_colors, level = fill_levels)
                p <- p + scale_fill_manual(values = setNames(fill_df[["color"]], fill_df[["level"]]))
            }
            if (color_column != "none" && all(color_colors != "none")) {
                color_df <- tibble(color = color_colors, level = color_levels)
                p <- p + scale_color_manual(values = setNames(color_df[["color"]], color_df[["level"]]))
            }
            if (alpha_column != "none" && all(alpha_values != "none")) {
                alpha_df <- tibble(value = alpha_values, level = alpha_levels)
                p <- p + scale_alpha_manual(values = setNames(alpha_df[["value"]], alpha_df[["level"]]))
            }
            p <- p + scale_y_continuous(
                expand = expansion(mult = scale_y_expand_mult),
                breaks = if (all(scale_y_breaks != "none")) scale_y_breaks else waiver(),
                limits = if (all(scale_y_breaks != "none")) scale_y_limits else NULL
            )
            if (fill_column != "none" && disable_fill_legend) {
                p <- p + guides(fill = "none")
            }
            if (color_column != "none" && disable_color_legend) {
                p <- p + guides(color = "none")
            }
            if (alpha_column != "none" && disable_alpha_legend) {
                p <- p + guides(alpha = "none")
            }
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
                    paste0("neuroborder-shiny-app-plot-", session$token, ".png")
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
                paste0("neuroborder-shiny-app-plot-", session$token, ".pdf")
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
