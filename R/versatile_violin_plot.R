versatile_violin_plot_ui <- function(id) {
    nav_panel(
        "Violin Plot",
        hr(),
        h2("1. Upload data"),
        fluidRow(
            column(4, fileInput(NS(id, "input_data_file"), "Upload a TSV/Excel file:")),
            column(4, fileInput(NS(id, "input_template_file"), "Upload a JSON (template) file:"))
        ),
        fluidRow(
            column(3, checkboxInput(
                NS(id, "use_template"),
                "Use template", FALSE,
                width = "100%"
            ))
        ),
        fluidRow(
            column(12, uiOutput(NS(id, "output_template_fields_options")))
        ),
        fluidRow(
            column(12, uiOutput(NS(id, "use_template_message")))
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
                    value = "0.05,0"
                )),
                column(3, textInput(NS(id, "scale_y_breaks"),
                    "Y-axis breaks:",
                    value = "none"
                ))
            ),
            fluidRow(
                column(4, textInput(NS(id, "violin_fill_colors"),
                    "Violin fill colors:",
                    value = "none",
                    width = "100%"
                )),
                column(4, textInput(NS(id, "violin_color_colors"),
                    "Violin color colors:",
                    value = "none",
                    width = "100%"
                )),
                column(4, textInput(NS(id, "violin_alpha_values"),
                    "Violin transparencies:",
                    value = "none",
                    width = "100%"
                ))
            ),
            fluidRow(
                column(4, textInput(NS(id, "boxplot_fill_colors"),
                    "Boxplot fill colors:",
                    value = "none",
                    width = "100%"
                )),
                column(4, textInput(NS(id, "boxplot_color_colors"),
                    "Boxplot color colors:",
                    value = "none",
                    width = "100%"
                )),
                column(4, textInput(NS(id, "boxplot_alpha_values"),
                    "Boxplot transparencies:",
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
                    "Point color colors:",
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
                    NS(id, "disable_violin_color_legend"),
                    "Disable violin color legend", TRUE,
                    width = "100%"
                )),
                column(3, checkboxInput(
                    NS(id, "disable_violin_fill_legend"),
                    "Disable violin fill legend", TRUE,
                    width = "100%"
                )),
                column(3, checkboxInput(
                    NS(id, "disable_violin_alpha_legend"),
                    "Disable violin transparency legend", TRUE,
                    width = "100%"
                ))
            ),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "disable_boxplot_color_legend"),
                    "Disable boxplot color legend", TRUE,
                    width = "100%"
                )),
                column(3, checkboxInput(
                    NS(id, "disable_boxplot_fill_legend"),
                    "Disable boxplot fill legend", TRUE,
                    width = "100%"
                )),
                column(3, checkboxInput(
                    NS(id, "disable_boxplot_alpha_legend"),
                    "Disable boxplot transparency legend", TRUE,
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
            ),
            fluidRow(
                column(3, selectizeInput(NS(id, "layer_order"),
                    label = "Select layer order:",
                    choices = c(
                        "Violin" = "NewNewNewGeomViolin",
                        "Boxplot" = "NewNewNewGeomBoxplot",
                        "Point" = "GeomPoint"
                    ),
                    multiple = TRUE,
                    selected = c("Violin", "Boxplot", "Point"),
                    width = "100%"
                ))
            )
        ),
        card(
            card_header("Violin-related parameters"),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "show_violin"),
                    "Show violin", TRUE
                )),
                column(3, checkboxInput(
                    NS(id, "violin_trim"),
                    "Trim tails", TRUE
                ))
            ),
            fluidRow(
                column(2, selectizeInput(NS(id, "violin_scale"),
                    "Select scale:",
                    choices = c("area", "count", "width"),
                    selected = "area",
                    multiple = FALSE
                )),
                column(2, numericInput(NS(id, "violin_linewidth"),
                    "Line width:",
                    value = 0.5, min = 0
                )),
                column(2, numericInput(NS(id, "violin_width"),
                    "Violin width:",
                    value = 0.75, min = 0
                )),
                column(2, textInput(NS(id, "violin_draw_quantiles"),
                    "Quantiles:",
                    value = "none"
                )),
                column(2, numericInput(NS(id, "violin_dodge_width"),
                    "Dodge width:",
                    value = 0.9, min = 0, max = 1
                )),
                column(2, selectizeInput(NS(id, "violin_dodge_preserve"),
                    "Select preserve type:",
                    choices = c("single", "total"),
                    selected = "total",
                    multiple = FALSE
                )),
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
                    "Show outliers", FALSE
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
                column(2, numericInput(NS(id, "boxplot_linewidth"),
                    "Line width:",
                    value = 0.5, min = 0
                )),
                column(2, numericInput(NS(id, "boxplot_dodge_width"),
                    "Dodge width:",
                    value = 0.9, min = 0, max = 1
                )),
                column(2, selectizeInput(NS(id, "boxplot_dodge_preserve"),
                    "Select preserve type:",
                    choices = c("single", "total"),
                    selected = "single",
                    multiple = FALSE
                )),
                column(4, textInput(NS(id, "boxplot_outlier_shape"),
                    "Outlier shape (an integer 0-25 or a single character):",
                    value = "19",
                    width = "100%"
                ))
            )
        ),
        card(
            card_header("Point-related parameters"),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "show_point"),
                    "Show point", FALSE
                ))
            ),
            fluidRow(
                column(2, numericInput(NS(id, "point_dodge_width"),
                    "Point dodge width:",
                    value = 0.9, min = 0, max = 1
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
            )
        ),
        hr(),
        h2("5. Download figure"),
        fluidRow(
            column(3, downloadButton(NS(id, "download_figure"), "Download Figure")),
            column(3, downloadButton(NS(id, "download_template"), "Download Template"))
        ),
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

            if (input$input_data_file$type == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet") {
                df <- na.omit(openxlsx::read.xlsx(input$input_data_file$datapath, sheet = 1, colNames = TRUE))
            } else {
                df <- na.omit(vroom::vroom(input$input_data_file$datapath))
            }

            df
        })
        output$output_data_table <- DT::renderDataTable({
            req(input_data())

            DT::datatable(input_data())
        })
        template_data <- reactive({
            req(input$input_template_file)

            jsonlite::read_json(input$input_template_file$datapath, simplifyVector = TRUE)
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

            validate(need(is.character(input_data()[[x_column]]), "X column needs to be of type character!"))

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

            validate(need(is.character(input_data()[[fill_column]]), "Fill column needs to be of type character!"))

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

            if (color_column != "none") {
                validate(need(is.character(input_data()[[color_column]]), "Color column needs to be of type character!"))
            }

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

            if (alpha_column != "none") {
                validate(need(is.character(input_data()[[alpha_column]]), "Alpha column needs to be of type character!"))
            }

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

            if (shape_column != "none") {
                validate(need(is.character(input_data()[[shape_column]]), "Shape column needs to be of type character!"))
            }

            selectInput(NS(id, "shape_levels"),
                "Select point shape levels:",
                if (shape_column != "none") unique(input_data()[[shape_column]]) else c(),
                multiple = TRUE,
                selected = if (shape_column != "none") unique(input_data()[[shape_column]]) else NULL,
                width = "100%"
            )
        })
        output$output_template_fields_options <- renderUI({
            req(
                input$use_template,
                template_data()
            )

            selectInput(NS(id, "template_fields"),
                "Select template fields to be reactive:",
                unique(names(template_data())),
                multiple = TRUE,
                selected = NULL,
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
        output$use_template_message <- renderUI({
            req(input$use_template)

            tags$b(
                "You are using a template file to set some of the figure parameters. ",
                "If you want to set them reactively again from the client, please uncheck the box ",
                "'Use template' above or select those template fields above you want to set them reactively ",
                "again from the client. Please remember to upload your JSON template file first."
            )
        })
        input_args_data <- reactive({
            req(
                input_data()
            )

            input_names <- names(input)
            args <- setNames(vector("list", length(input_names)), input_names)
            for (name in input_names) {
                args[[name]] <- input[[name]]
            }

            use_template <- input$use_template
            template_fields <- input$template_fields
            if (use_template) {
                validate(need(input$input_template_file, "Please upload your JSON (template) file!"))
                template <- template_data()
                if (!is.null(template_fields)) {
                    template <- template[!(names(template) %in% template_fields)]
                }
                for (name in names(template)) {
                    args[[name]] <- template[[name]]
                }
            }

            args
        })
        output_final_plot <- reactive({
            req(
                input_args_data(),
                output_plot()
            )

            suppressWarnings(suppressMessages(library(gginnards)))

            input_args <- input_args_data()
            p <- output_plot()

            layer_order <- input_args$layer_order
            layer_order <- layer_order[layer_order %in% sapply(p$layers, function(layer) {
                class(layer$geom)[1]
            })]

            for (i in layer_order) {
                p <- move_layers(p, match_type = i, position = "top")
            }

            p
        })
        output_plot <- reactive({
            req(
                input_data(),
                input_args_data()
            )

            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(ggprism)))
            suppressWarnings(suppressMessages(library(ggnewscale)))

            data <- input_data()
            input_args <- input_args_data()

            x_column <- input_args$x_column
            y_column <- input_args$y_column
            fill_column <- input_args$fill_column
            color_column <- input_args$color_column
            alpha_column <- input_args$alpha_column
            shape_column <- input_args$shape_column

            validate(need(is.character(data[[x_column]]), "X column needs to be of type character!"))
            validate(need(is.numeric(data[[y_column]]), "Y column needs to be of type numeric!"))
            validate(need(is.character(data[[fill_column]]), "Fill column needs to be of type character!"))
            if (color_column != "none") {
                validate(need(is.character(data[[color_column]]), "Color column needs to be of type character!"))
            }
            if (alpha_column != "none") {
                validate(need(is.character(data[[alpha_column]]), "Alpha column needs to be of type character!"))
            }
            if (shape_column != "none") {
                validate(need(is.character(data[[shape_column]]), "Shape column needs to be of type character!"))
            }

            x_levels <- input_args$x_levels
            fill_levels <- input_args$fill_levels
            color_levels <- input_args$color_levels
            alpha_levels <- input_args$alpha_levels
            shape_levels <- input_args$shape_levels

            # compared to fill_column, x_column has priority
            data[[fill_column]] <- factor(data[[fill_column]], levels = fill_levels)
            data[[x_column]] <- factor(data[[x_column]], levels = x_levels)

            # figure parameters below
            scale_y_expand_mult <- as.numeric(trimws(strsplit(input_args$scale_y_expand_mult, ",", fixed = TRUE)[[1]]))
            scale_y_breaks <- input_args$scale_y_breaks
            if (scale_y_breaks != "none") {
                scale_y_breaks <- as.numeric(trimws(strsplit(scale_y_breaks, ",", fixed = TRUE)[[1]]))
                scale_y_limits <- c(min(scale_y_breaks), max(scale_y_breaks))
            }

            show_violin <- input_args$show_violin
            violin_trim <- input_args$violin_trim
            violin_scale <- input_args$violin_scale
            violin_linewidth <- input_args$violin_linewidth
            violin_width <- input_args$violin_width
            violin_draw_quantiles <- input_args$violin_draw_quantiles
            if (violin_draw_quantiles != "none") {
                violin_draw_quantiles <- as.numeric(trimws(strsplit(violin_draw_quantiles, ",", fixed = TRUE)[[1]]))
            } else {
                violin_draw_quantiles <- NULL
            }
            violin_dodge_width <- input_args$violin_dodge_width
            violin_dodge_preserve <- input_args$violin_dodge_preserve
            disable_violin_fill_legend <- input_args$disable_violin_fill_legend
            disable_violin_color_legend <- input_args$disable_violin_color_legend
            disable_violin_alpha_legend <- input_args$disable_violin_alpha_legend
            violin_fill_colors <- trimws(strsplit(input_args$violin_fill_colors, ",", fixed = TRUE)[[1]])
            violin_color_colors <- trimws(strsplit(input_args$violin_color_colors, ",", fixed = TRUE)[[1]])
            violin_alpha_values <- input_args$violin_alpha_values
            if (violin_alpha_values != "none") {
                violin_alpha_values <- as.numeric(trimws(strsplit(input_args$violin_alpha_values, ",", fixed = TRUE)[[1]]))
            }

            show_boxplot <- input_args$show_boxplot
            boxplot_width <- input_args$boxplot_width
            boxplot_notch <- input_args$boxplot_notch
            boxplot_dodge_width <- input_args$boxplot_dodge_width
            boxplot_dodge_preserve <- input_args$boxplot_dodge_preserve
            boxplot_outliers <- input_args$boxplot_outliers
            boxplot_outlier_shape <- as.numeric(input_args$boxplot_outlier_shape)
            if (is.na(boxplot_outlier_shape)) {
                boxplot_outlier_shape <- input_args$boxplot_outlier_shape
            }
            boxplot_outlier_color <- input_args$boxplot_outlier_color
            boxplot_outlier_fill <- input_args$boxplot_outlier_fill
            boxplot_outlier_size <- input_args$boxplot_outlier_size
            boxplot_outlier_stroke <- input_args$boxplot_outlier_stroke
            boxplot_outlier_alpha <- input_args$boxplot_outlier_alpha
            boxplot_linewidth <- input_args$boxplot_linewidth
            disable_boxplot_fill_legend <- input_args$disable_boxplot_fill_legend
            disable_boxplot_color_legend <- input_args$disable_boxplot_color_legend
            disable_boxplot_alpha_legend <- input_args$disable_boxplot_alpha_legend
            boxplot_fill_colors <- trimws(strsplit(input_args$boxplot_fill_colors, ",", fixed = TRUE)[[1]])
            boxplot_color_colors <- trimws(strsplit(input_args$boxplot_color_colors, ",", fixed = TRUE)[[1]])
            boxplot_alpha_values <- input_args$boxplot_alpha_values
            if (boxplot_alpha_values != "none") {
                boxplot_alpha_values <- as.numeric(trimws(strsplit(input_args$boxplot_alpha_values, ",", fixed = TRUE)[[1]]))
            }

            show_point <- input_args$show_point
            point_dodge_width <- input_args$point_dodge_width
            point_size <- input_args$point_size
            point_stroke <- input_args$point_stroke
            point_jitter_width <- input_args$point_jitter_width
            point_jitter_height <- input_args$point_jitter_height
            disable_point_fill_legend <- input_args$disable_point_fill_legend
            disable_point_color_legend <- input_args$disable_point_color_legend
            disable_point_alpha_legend <- input_args$disable_point_alpha_legend
            disable_point_shape_legend <- input_args$disable_point_shape_legend
            point_fill_colors <- trimws(strsplit(input_args$point_fill_colors, ",", fixed = TRUE)[[1]])
            point_color_colors <- trimws(strsplit(input_args$point_color_colors, ",", fixed = TRUE)[[1]])
            point_alpha_values <- input_args$point_alpha_values
            if (point_alpha_values != "none") {
                point_alpha_values <- as.numeric(trimws(strsplit(input_args$point_alpha_values, ",", fixed = TRUE)[[1]]))
            }
            point_shape_values <- as.numeric(trimws(strsplit(input_args$point_shape_values, ",", fixed = TRUE)[[1]]))
            if (any(is.na(point_shape_values))) {
                point_shape_values <- trimws(strsplit(input_args$point_shape_values, ",", fixed = TRUE)[[1]])
            }

            theme_prism_panel_border <- input_args$theme_prism_panel_border
            font_family <- input_args$font_family
            theme_base_font_size <- input_args$theme_base_font_size

            # fill_column is also used as group column
            grouped <- ifelse(fill_column != x_column, TRUE, FALSE)

            p <- ggplot(data, aes(.data[[x_column]], .data[[y_column]]))

            if (show_violin) {
                p <- p +
                    geom_violin(
                        aes(
                            fill = .data[[fill_column]],
                            color = if (color_column != "none") .data[[color_column]] else NULL,
                            alpha = if (alpha_column != "none") .data[[alpha_column]] else NULL
                        ),
                        trim = violin_trim,
                        scale = violin_scale,
                        linewidth = violin_linewidth,
                        width = violin_width,
                        draw_quantiles = violin_draw_quantiles,
                        position = position_dodge(
                            width = violin_dodge_width,
                            preserve = violin_dodge_preserve
                        )
                    )

                if (all(violin_fill_colors != "none")) {
                    violin_fill_df <- tibble(color = violin_fill_colors, level = fill_levels)
                    p <- p + scale_fill_manual(values = setNames(violin_fill_df[["color"]], violin_fill_df[["level"]]))
                }
                if (color_column != "none" && all(violin_color_colors != "none")) {
                    violin_color_df <- tibble(color = violin_color_colors, level = color_levels)
                    p <- p + scale_color_manual(values = setNames(violin_color_df[["color"]], violin_color_df[["level"]]))
                }
                if (alpha_column != "none" && all(violin_alpha_values != "none")) {
                    violin_alpha_df <- tibble(value = violin_alpha_values, level = alpha_levels)
                    p <- p + scale_alpha_manual(values = setNames(violin_alpha_df[["value"]], violin_alpha_df[["level"]]))
                }

                if (disable_violin_fill_legend) {
                    p <- p + guides(fill = "none")
                }
                if (color_column != "none" && disable_violin_color_legend) {
                    p <- p + guides(color = "none")
                }
                if (alpha_column != "none" && disable_violin_alpha_legend) {
                    p <- p + guides(alpha = "none")
                }
            }

            if (show_boxplot) {
                p <- p +
                    new_scale("fill") +
                    new_scale("color") +
                    new_scale("alpha") +
                    geom_boxplot(
                        aes(
                            fill = .data[[fill_column]],
                            color = if (color_column != "none") .data[[color_column]] else NULL,
                            alpha = if (alpha_column != "none") .data[[alpha_column]] else NULL
                        ),
                        width = boxplot_width,
                        notch = boxplot_notch,
                        outliers = boxplot_outliers,
                        outlier.color = boxplot_outlier_color,
                        outlier.fill = boxplot_outlier_fill,
                        outlier.shape = boxplot_outlier_shape,
                        outlier.size = boxplot_outlier_size,
                        outlier.stroke = boxplot_outlier_stroke,
                        outlier.alpha = boxplot_outlier_alpha,
                        linewidth = boxplot_linewidth,
                        position = position_dodge(
                            width = boxplot_dodge_width,
                            preserve = boxplot_dodge_preserve
                        )
                    )

                if (all(boxplot_fill_colors != "none")) {
                    boxplot_fill_df <- tibble(color = boxplot_fill_colors, level = fill_levels)
                    p <- p + scale_fill_manual(values = setNames(boxplot_fill_df[["color"]], boxplot_fill_df[["level"]]))
                }
                if (color_column != "none" && all(boxplot_color_colors != "none")) {
                    boxplot_color_df <- tibble(color = boxplot_color_colors, level = color_levels)
                    p <- p + scale_color_manual(values = setNames(boxplot_color_df[["color"]], boxplot_color_df[["level"]]))
                }
                if (alpha_column != "none" && all(boxplot_alpha_values != "none")) {
                    boxplot_alpha_df <- tibble(value = boxplot_alpha_values, level = alpha_levels)
                    p <- p + scale_alpha_manual(values = setNames(boxplot_alpha_df[["value"]], boxplot_alpha_df[["level"]]))
                }

                if (disable_boxplot_fill_legend) {
                    p <- p + guides(fill = "none")
                }
                if (color_column != "none" && disable_boxplot_color_legend) {
                    p <- p + guides(color = "none")
                }
                if (alpha_column != "none" && disable_boxplot_alpha_legend) {
                    p <- p + guides(alpha = "none")
                }
            }

            if (show_point) {
                p <- p +
                    new_scale("fill") +
                    new_scale("color") +
                    new_scale("alpha") +
                    geom_jitter(
                        aes(
                            fill = .data[[fill_column]],
                            color = if (color_column != "none") .data[[color_column]] else NULL,
                            alpha = if (alpha_column != "none") .data[[alpha_column]] else NULL,
                            shape = if (shape_column != "none") .data[[shape_column]] else NULL
                        ),
                        position = position_jitterdodge(
                            jitter.width = point_jitter_width,
                            jitter.height = point_jitter_height,
                            dodge.width = ifelse(grouped, point_dodge_width, 0)
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

            validate(need(length(p$layers) > 0, "Select at least one geometric layer!"))

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
                req(
                    output_final_plot(),
                    input_args_data()
                )

                input_args <- input_args_data()

                input_figure_width <- input_args$input_figure_width
                input_figure_height <- input_args$input_figure_height

                output_figure_file <- file.path(
                    extra_params$temp_dir,
                    paste0(extra_params$uni_prefix, "_violin.png")
                )

                unigd::ugd()
                unigd::ugd_save_inline(
                    {
                        print(output_final_plot())
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
                paste0(extra_params$uni_prefix, "_violin.pdf")
            },
            content = function(file) {
                input_args <- input_args_data()

                input_figure_width <- input_args$input_figure_width
                input_figure_height <- input_args$input_figure_height

                unigd::ugd()
                unigd::ugd_save_inline(
                    {
                        print(output_final_plot())
                    },
                    file = file,
                    width = input_figure_width,
                    height = input_figure_height
                )
                unigd::ugd_close(all = TRUE)
            }
        )
        output$download_template <- downloadHandler(
            filename = function() {
                paste0(extra_params$uni_prefix, "_violin.json")
            },
            content = function(file) {
                vroom::vroom_write_lines(
                    jsonlite::toJSON(input_args_data(), pretty = TRUE),
                    file = file
                )
            }
        )
    })
}
