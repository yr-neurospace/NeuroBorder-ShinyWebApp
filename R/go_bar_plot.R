go_bar_plot_ui <- function(id) {
    nav_panel(
        "Bar Plot",
        hr(),
        h2("1. Upload data"),
        fileInput(NS(id, "input_data_file"), "Upload a TSV file:"),
        hr(),
        h2("2. Preview data"),
        fluidRow(column(12, DT::dataTableOutput(NS(id, "output_data_table")))),
        fluidRow(
            column(2, uiOutput(NS(id, "output_x_column_options"))),
            column(2, uiOutput(NS(id, "output_y_column_options"))),
            column(2, uiOutput(NS(id, "output_fill_column_options"))),
            column(2, uiOutput(NS(id, "output_ontology_column_options"))),
            column(2, uiOutput(NS(id, "output_y_label_column_options")))
        ),
        hr(),
        h2("3. Set parameters"),
        card(
            card_header("Data-related parameters"),
            fluidRow(
                column(12, textInput(
                    NS(id, "go_ids"),
                    "GO IDs:",
                    value = "",
                    placeholder = "Type GO IDs, e.g. GO:0000001,GO:0000002,GO:0000003",
                    width = "100%"
                ))
            ),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "plot_all"),
                    "Plot all items", TRUE
                )),
                column(3, checkboxInput(
                    NS(id, "reverse_x_direction"),
                    "Reverse x direction", FALSE
                ))
            )
        ),
        card(
            card_header("Figure-related parameters"),
            fluidRow(
                column(4, uiOutput(NS(id, "output_fill_levels_options"))),
                column(4, uiOutput(NS(id, "output_ontology_levels_options"))),
                column(4, uiOutput(NS(id, "output_y_label_levels_options")))
            ),
            fluidRow(
                column(4, textInput(
                    NS(id, "y_label_colors"),
                    "Description text colors:",
                    value = "#f8766d,#00ba38,#619cff"
                )),
                column(4, textInput(
                    NS(id, "fill_colors"),
                    "Fill colors:",
                    value = "#e06663,#327eba,#006400"
                )),
                column(4, selectizeInput(NS(id, "use_ontology_column"),
                    "Use ontology column for:",
                    choices = c("pool", "facet"),
                    selected = "pool",
                    multiple = FALSE
                ))
            ),
            fluidRow(
                column(4, uiOutput(NS(id, "output_sort_columns_options"))),
                column(4, uiOutput(NS(id, "output_decreasing_columns_options"))),
                column(4, selectizeInput(NS(id, "y_axis_position"),
                    "Select y axis position:",
                    choices = c("left", "right"),
                    selected = "left",
                    multiple = FALSE
                ))
            ),
            fluidRow(
                column(4, numericInput(
                    NS(id, "text_width"),
                    "Text width:",
                    value = 10000, min = 0
                )),
                column(4, numericInput(
                    NS(id, "theme_base_font_size"),
                    "Base font size:",
                    value = 11, min = 0
                )),
                column(4, uiOutput(NS(id, "output_font_families")))
            ),
            fluidRow(
                column(4, selectizeInput(NS(id, "figure_theme"),
                    "Select figure theme:",
                    choices = c(
                        "theme_prism", "theme_bw", "theme_classic", "theme_dark",
                        "theme_gray", "theme_grey", "theme_light", "theme_linedraw",
                        "theme_minimal", "theme_void"
                    ),
                    selected = "theme_prism",
                    multiple = FALSE
                )),
                column(3, textInput(NS(id, "scale_x_expand_add"),
                    "X-axis expansions:",
                    value = "0,0"
                ))
            ),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "theme_prism_panel_border"),
                    "Plot panel border", FALSE
                ))
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

go_bar_plot_server <- function(id, session, extra_params) {
    moduleServer(id, function(input, output, session) {
        input_data <- reactive({
            req(input$input_data_file)

            vroom::vroom(input$input_data_file$datapath)
        })
        output$output_y_label_column_options <- renderUI({
            req(input_data())

            selectInput(NS(id, "y_label_column"),
                "Select y label column:",
                names(input_data()),
                multiple = FALSE,
                selected = "ONTOLOGY"
            )
        })
        output$output_y_label_levels_options <- renderUI({
            req(
                input_data(),
                input$y_label_column
            )

            y_label_column <- input$y_label_column

            selectInput(NS(id, "y_label_levels"),
                "Select y label levels:",
                unique(input_data()[[y_label_column]]),
                multiple = TRUE,
                selected = unique(input_data()[[y_label_column]])
            )
        })
        extended_input_data <- reactive({
            req(
                input_data(),
                input$text_width,
                input$y_label_column,
                input$y_label_levels
            )

            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(glue)))
            suppressWarnings(suppressMessages(library(scales)))

            data <- input_data()
            text_width <- input$text_width

            y_label_column <- input$y_label_column
            y_label_levels <- input$y_label_levels
            y_label_colors <- trimws(strsplit(input$y_label_colors, ",", fixed = TRUE)[[1]])
            if (length(y_label_colors) != length(y_label_levels)) {
                y_label_colors <- pal_hue()(length(y_label_levels))
            }

            data <- data %>%
                mutate(
                    GeneRatio = sapply(GeneRatio, function(x) {
                        y <- as.numeric(strsplit(x, "/", fixed = TRUE)[[1]])
                        y[1] / y[2]
                    }),
                    Description = gsub("\\n", "<br>", str_wrap(Description, width = text_width)),
                    mlog10padj = -log10(p.adjust)
                )

            inner_join(data, tibble(
                y_label_color = y_label_colors,
                y_label_level = factor(y_label_levels, levels = y_label_levels)
            ), by = setNames("y_label_level", y_label_column)) %>%
                mutate(colored_description = glue('<span style="color:{y_label_color};">{Description}</span>'))
        })
        output$output_data_table <- DT::renderDataTable({
            req(extended_input_data())

            DT::datatable(extended_input_data())
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
        output$output_x_column_options <- renderUI({
            req(extended_input_data())

            selectInput(NS(id, "x_column"),
                "Select x column:",
                names(extended_input_data()),
                multiple = FALSE,
                selected = "GeneRatio"
            )
        })
        output$output_y_column_options <- renderUI({
            req(extended_input_data())

            selectInput(NS(id, "y_column"),
                "Select y column:",
                names(extended_input_data()),
                multiple = FALSE,
                selected = "colored_description"
            )
        })
        output$output_fill_column_options <- renderUI({
            req(extended_input_data())

            selectInput(NS(id, "fill_column"),
                "Select fill color column:",
                c("none", names(extended_input_data())),
                multiple = FALSE,
                selected = "ONTOLOGY"
            )
        })
        output$output_fill_levels_options <- renderUI({
            req(
                extended_input_data(),
                input$fill_column != "none" && !is.numeric(extended_input_data()[[input$fill_column]])
            )

            fill_column <- input$fill_column

            selectInput(NS(id, "fill_levels"),
                "Select fill levels:",
                unique(extended_input_data()[[fill_column]]),
                multiple = TRUE,
                selected = unique(extended_input_data()[[fill_column]])
            )
        })
        output$output_ontology_column_options <- renderUI({
            req(extended_input_data())

            selectInput(NS(id, "ontology_column"),
                "Select ontology column:",
                c("none", names(extended_input_data())),
                multiple = FALSE,
                selected = "ONTOLOGY"
            )
        })
        output$output_ontology_levels_options <- renderUI({
            req(
                extended_input_data(),
                input$ontology_column != "none"
            )

            ontology_column <- input$ontology_column

            selectInput(NS(id, "ontology_levels"),
                "Select ontology levels:",
                unique(extended_input_data()[[ontology_column]]),
                multiple = TRUE,
                selected = unique(extended_input_data()[[ontology_column]])
            )
        })
        output$output_sort_columns_options <- renderUI({
            req(
                extended_input_data(),
                input$x_column,
                input$fill_column,
                input$ontology_column
            )

            x_column <- input$x_column
            fill_column <- input$fill_column
            ontology_column <- input$ontology_column

            sort_columns <- unique(c(
                if (ontology_column != "none") ontology_column else NULL,
                x_column,
                if (fill_column != "none") fill_column else NULL
            ))

            selectInput(NS(id, "sort_columns"),
                "Select sort columns:",
                sort_columns,
                multiple = TRUE,
                selected = sort_columns
            )
        })
        output$output_decreasing_columns_options <- renderUI({
            req(
                extended_input_data(),
                input$x_column,
                input$fill_column,
                input$ontology_column
            )

            x_column <- input$x_column
            fill_column <- input$fill_column
            ontology_column <- input$ontology_column

            sort_columns <- unique(c(
                if (ontology_column != "none") ontology_column else NULL,
                x_column,
                if (fill_column != "none") fill_column else NULL
            ))

            decreasing_columns <- lapply(setNames(sort_columns, sort_columns), function(x) {
                setNames(
                    as.list(paste0(x, c(" True", " False"))),
                    paste0(x, c(" True", " False"))
                )
            })

            selectInput(NS(id, "decreasing_columns"),
                "Select decreasing columns:",
                decreasing_columns,
                multiple = TRUE,
                selected = paste0(sort_columns, " True")
            )
        })
        output_plot <- reactive({
            req(
                extended_input_data()
            )

            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(ggtext)))
            suppressWarnings(suppressMessages(library(ggprism)))

            data <- extended_input_data()

            plot_all <- input$plot_all

            if (!plot_all) {
                go_ids <- trimws(strsplit(input$go_ids, ",", fixed = TRUE)[[1]])

                if (!all(str_detect(go_ids, "^GO:[0-9]{7}$"))) {
                    validate("some input GO IDs are malformed")
                }

                data <- data %>% filter(ID %in% go_ids)
            }

            x_column <- input$x_column
            y_column <- input$y_column
            ontology_column <- input$ontology_column
            use_ontology_column <- input$use_ontology_column
            ontology_levels <- input$ontology_levels
            fill_column <- input$fill_column
            fill_levels <- input$fill_levels
            sort_columns <- input$sort_columns
            decreasing_columns <- input$decreasing_columns
            reverse_x_direction <- input$reverse_x_direction
            text_width <- input$text_width
            font_family <- input$font_family
            theme_prism_panel_border <- input$theme_prism_panel_border
            theme_base_font_size <- input$theme_base_font_size
            y_axis_position <- input$y_axis_position
            figure_theme <- input$figure_theme
            figure_theme_func <- get(figure_theme)
            scale_x_expand_add <- as.numeric(trimws(strsplit(input$scale_x_expand_add, ",", fixed = TRUE)[[1]]))

            fill_colors <- trimws(strsplit(input$fill_colors, ",", fixed = TRUE)[[1]])

            true_value_table <- setNames(
                rep(c(TRUE, FALSE), times = length(sort_columns)),
                paste0(rep(sort_columns, each = 2), c(" True", " False"))
            )
            decreasing_columns <- setNames(true_value_table[decreasing_columns], NULL)

            if (ontology_column != "none") {
                data[[ontology_column]] <- factor(data[[ontology_column]], levels = ontology_levels)
            }
            if (fill_column != "none" && !is.numeric(data[[fill_column]])) {
                data[[fill_column]] <- factor(data[[fill_column]], levels = fill_levels)
            }

            if (reverse_x_direction) {
                data[[x_column]] <- -data[[x_column]]
            }

            sort_columns_ls <- as.list(data[, sort_columns[sort_columns %in% names(data)]])
            names(sort_columns_ls) <- NULL
            sort_columns_ls[["decreasing"]] <- decreasing_columns[sort_columns %in% names(data)]
            data <- data[do.call(order, sort_columns_ls), ]
            data[["Description"]] <- factor(data[["Description"]], levels = rev(unique(data[["Description"]])))
            data[["colored_description"]] <- factor(data[["colored_description"]], levels = rev(unique(data[["colored_description"]])))

            p <- ggplot(data, aes(.data[[x_column]], .data[[y_column]],
                fill = if (fill_column != "none") .data[[fill_column]] else NULL
            )) +
                geom_col() +
                scale_x_continuous(
                    expand = expansion(add = scale_x_expand_add),
                    labels = if (reverse_x_direction) {
                        function(x) {
                            as.character(-x)
                        }
                    } else {
                        waiver()
                    }
                ) +
                scale_y_discrete(position = y_axis_position)
            if (figure_theme == "theme_prism") {
                p <- p + figure_theme_func(
                    border = theme_prism_panel_border,
                    base_family = font_family,
                    base_size = theme_base_font_size
                )
            } else {
                p <- p + figure_theme_func(
                    base_family = font_family,
                    base_size = theme_base_font_size
                )
            }
            p <- p +
                theme(
                    axis.text.y = element_markdown(),
                    legend.title = element_text()
                )
            if (fill_column != "none" && is.numeric(data[[fill_column]])) {
                p <- p + scale_fill_gradient(low = fill_colors[1], high = fill_colors[2])
            }
            if (fill_column != "none" && !is.numeric(data[[fill_column]])) {
                p <- p + scale_fill_manual(values = setNames(fill_colors, fill_levels))
            }
            if (ontology_column != "none" && use_ontology_column == "facet") {
                p <- p + facet_grid(rows = vars(get(ontology_column)), scales = "free", space = "free")
            }
            p
        })
        output$output_preview_png_image <- renderImage(
            {
                req(output_plot())

                input_figure_width <- input$input_figure_width
                input_figure_height <- input$input_figure_height

                output_figure_file <- file.path(
                    extra_params$temp_dir,
                    paste0(extra_params$uni_prefix, "_go_bar.png")
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
                paste0(extra_params$uni_prefix, "_go_bar.pdf")
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
