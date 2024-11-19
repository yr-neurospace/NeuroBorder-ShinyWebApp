versatile_violin_plot_ui <- function(id) {
    nav_panel(
        "Violin Plot",
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
            column(4, uiOutput(NS(id, "output_group_column_options")))
        ),
        fluidRow(
            column(4, uiOutput(NS(id, "output_fill_column_options"))),
            column(4, uiOutput(NS(id, "output_color_column_options"))),
            column(4, uiOutput(NS(id, "output_alpha_column_options")))
        ),
        hr(),
        h2("3. Set parameters"),
        card(
            card_header("Data-related parameters"),
            fluidRow(
                column(6, uiOutput(NS(id, "output_x_levels_options"))),
                column(6, uiOutput(NS(id, "output_group_levels_options"))),
            ),
            fluidRow(
                column(6, uiOutput(NS(id, "output_fill_levels_options"))),
                column(6, uiOutput(NS(id, "output_color_levels_options")))
            ),
            fluidRow(
                column(6, uiOutput(NS(id, "output_alpha_levels_options")))
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
            card_header("Violin-related parameters"),
            fluidRow(
                column(3, selectizeInput(NS(id, "violin_scale"),
                    "Select scale:",
                    choices = c("area", "count", "width"),
                    selected = "area",
                    multiple = FALSE
                ))
            ),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "violin_trim"),
                    "Trim", FALSE
                ))
            )
        ),
        card(
            card_header("Boxplot-related parameters"),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "show_boxplot"),
                    "Show boxplot", TRUE
                )),
                column(3, checkboxInput(
                    NS(id, "boxplot_notch"),
                    "Show notch", TRUE
                )),
                column(3, checkboxInput(
                    NS(id, "boxplot_outliers"),
                    "Show outliers", TRUE
                ))
            ),
            fluidRow(
                column(2, numericInput(NS(id, "boxplot_width"),
                    "Width:",
                    value = 0.2,
                    min = 0,
                    max = 1
                )),
                column(2, textInput(NS(id, "boxplot_outlier_color"),
                    "Outlier color:",
                    value = "black"
                )),
                column(2, textInput(NS(id, "boxplot_outlier_fill"),
                    "Outlier fill color:",
                    value = "black"
                )),
                column(2, numericInput(NS(id, "boxplot_outlier_size"),
                    "Outlier size:",
                    value = 1.5,
                    min = 0
                )),
                column(2, numericInput(NS(id, "boxplot_outlier_stroke"),
                    "Outlier stroke:",
                    value = 0.5,
                    min = 0
                )),
                column(2, numericInput(NS(id, "boxplot_outlier_alpha"),
                    "Outlier transparency:",
                    value = 1,
                    min = 0,
                    max = 1
                ))
            ),
            fluidRow(
                column(4, textInput(NS(id, "boxplot_outlier_shape"),
                    "Outlier shape (an integer 0-25 or a single character):",
                    value = "19",
                    width = "100%"
                ))
            )
        ),
        card(
            card_header("Miscellaneous outputs"),
            fluidRow(
                column(12, verbatimTextOutput(NS(id, "output_min_max_values")))
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

versatile_violin_plot_server <- function(id, session, extra_params) {
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
        output$output_group_column_options <- renderUI({
            req(
                input_data()
            )

            selectInput(NS(id, "group_column"),
                "Select group column:",
                c("none", names(input_data())),
                multiple = FALSE,
                selected = "none"
            )
        })
        output$output_fill_column_options <- renderUI({
            req(
                input_data()
            )

            selectInput(NS(id, "fill_column"),
                "Select fill column:",
                c("none", names(input_data())),
                multiple = FALSE,
                selected = "none"
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
        output$output_group_levels_options <- renderUI({
            req(input_data())

            group_column <- input$group_column

            selectInput(NS(id, "group_levels"),
                "Select group levels:",
                if (group_column != "none") unique(input_data()[[group_column]]) else c(),
                multiple = TRUE,
                selected = if (group_column != "none") unique(input_data()[[group_column]]) else NULL,
                width = "100%"
            )
        })
        output$output_fill_levels_options <- renderUI({
            req(input_data())

            fill_column <- input$fill_column

            selectInput(NS(id, "fill_levels"),
                "Select fill levels:",
                if (fill_column != "none") unique(input_data()[[fill_column]]) else c(),
                multiple = TRUE,
                selected = if (fill_column != "none") unique(input_data()[[fill_column]]) else NULL,
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
        output_plot <- reactive({
            req(
                input_data()
            )

            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(ggprism)))
            suppressWarnings(suppressMessages(library(YRUtils)))

            data <- input_data()

            x_column <- input$x_column
            y_column <- input$y_column
            group_column <- input$group_column

            x_levels <- input$x_levels
            data[[x_column]] <- factor(data[[x_column]], levels = x_levels)

            group_levels <- input$group_levels
            if (group_column != "none") {
                data[[group_column]] <- factor(data[[group_column]], levels = group_levels)
            }

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

            violin_trim <- input$violin_trim
            violin_scale <- input$violin_scale

            show_boxplot <- input$show_boxplot
            boxplot_width <- input$boxplot_width
            boxplot_notch <- input$boxplot_notch
            boxplot_outliers <- input$boxplot_outliers
            boxplot_outlier_shape <- as.numeric(input$boxplot_outlier_shape)
            if (is.na(boxplot_outlier_shape)) {
                boxplot_outlier_shape <- input$boxplot_outlier_shape
            }
            boxplot_outlier_color <- input$boxplot_outlier_color
            boxplot_outlier_fill <- input$boxplot_outlier_fill
            boxplot_outlier_size <- input$boxplot_outlier_size
            boxplot_outlier_stroke <- input$boxplot_outlier_stroke
            boxplot_outlier_alpha <- input$boxplot_outlier_alpha

            disable_color_legend <- input$disable_color_legend
            disable_fill_legend <- input$disable_fill_legend
            disable_alpha_legend <- input$disable_alpha_legend

            theme_prism_panel_border <- input$theme_prism_panel_border
            font_family <- input$font_family
            theme_base_font_size <- input$theme_base_font_size

            set_fonts(font_families = font_family)

            p <- ggplot(data, aes(.data[[x_column]], .data[[y_column]],
                group = if (group_column != "none") .data[[group_column]] else NULL,
                fill = if (fill_column != "none") .data[[fill_column]] else NULL,
                color = if (color_column != "none") .data[[color_column]] else NULL,
                alpha = if (alpha_column != "none") .data[[alpha_column]] else NULL
            )) +
                geom_violin(trim = violin_trim, scale = violin_scale)
            if (show_boxplot) {
                p <- p + geom_boxplot(
                    width = boxplot_width,
                    notch = boxplot_notch,
                    outliers = boxplot_outliers,
                    outlier.color = boxplot_outlier_color,
                    outlier.fill = boxplot_outlier_fill,
                    outlier.shape = boxplot_outlier_shape,
                    outlier.size = boxplot_outlier_size,
                    outlier.stroke = boxplot_outlier_stroke,
                    outlier.alpha = boxplot_outlier_alpha,
                    position = position_dodge(0.9)
                )
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
