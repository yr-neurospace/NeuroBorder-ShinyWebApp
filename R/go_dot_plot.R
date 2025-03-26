go_dot_plot_ui <- function(id) {
    nav_panel(
        "Dot Plot",
        hr(),
        h2("1. Upload data"),
        fluidRow(
            column(4, fileInput(NS(id, "input_data_file"), "Upload a TSV file:")),
            column(4, fileInput(NS(id, "input_description_levels_file"), "Upload a description levels TSV file:"))
        ),
        hr(),
        h2("2. Preview data"),
        fluidRow(column(12, DT::dataTableOutput(NS(id, "output_data_table")))),
        fluidRow(column(12, DT::dataTableOutput(NS(id, "output_description_levels_table")))),
        fluidRow(
            column(3, uiOutput(NS(id, "output_x_column_options"))),
            column(3, uiOutput(NS(id, "output_point_size_column_options"))),
            column(3, uiOutput(NS(id, "output_point_color_column_options"))),
            column(3, uiOutput(NS(id, "output_ontology_column_options")))
        ),
        hr(),
        h2("3. Set parameters"),
        card(
            card_header("Data-related parameters"),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "cluster_mode"),
                    "Enable cluster mode", FALSE
                )),
                column(3, checkboxInput(
                    NS(id, "auto_sort_mode"),
                    "Enable auto-sorting mode", TRUE
                ))
            ),
            fluidRow(
                column(12, uiOutput(NS(id, "output_cluster_levels_options")))
            )
        ),
        card(
            card_header("Figure-related parameters"),
            fluidRow(
                column(2, uiOutput(NS(id, "output_font_families"))),
                column(2, numericInput(NS(id, "text_width"),
                    "Text width:",
                    value = 60, min = 0
                )),
                column(2, textInput(NS(id, "geom_point_colors"),
                    "Point colors:",
                    value = "#e06663,#327eba"
                )),
                column(2, textInput(NS(id, "panel_grid_major_line_color"),
                    "Panel grid major line color:",
                    value = "grey90"
                ))
            ),
            fluidRow(
                column(2, numericInput(NS(id, "theme_base_font_size"),
                    "Base font size:",
                    value = 16, min = 0
                ))
            ),
            fluidRow(
                column(12, sliderInput(NS(id, "scale_size_range"),
                    "Scale size range:",
                    min = 0, max = 50,
                    step = 1, width = "100%",
                    value = c(3, 8)
                ))
            ),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "theme_prism_panel_border"),
                    "Plot panel border", TRUE
                ))
            )
        ),
        card(
            card_header("Miscellaneous outputs"),
            fluidRow(
                column(12, verbatimTextOutput(NS(id, "output_non_overlap_description_levels")))
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

go_dot_plot_server <- function(id, session, extra_params) {
    moduleServer(id, function(input, output, session) {
        input_data <- reactive({
            req(input$input_data_file)

            vroom::vroom(input$input_data_file$datapath)
        })
        description_levels <- reactive({
            req(input$input_description_levels_file)

            vroom::vroom(input$input_description_levels_file$datapath,
                delim = "\t"
            )
        })
        output$output_data_table <- DT::renderDataTable({
            req(input_data())

            DT::datatable(input_data())
        })
        output$output_description_levels_table <- DT::renderDataTable({
            req(description_levels())

            DT::datatable(description_levels())
        })
        output$output_x_column_options <- renderUI({
            req(
                input_data()
            )

            selectInput(NS(id, "x_column"),
                "Select x column:",
                names(input_data()),
                multiple = FALSE,
                selected = "GeneRatio"
            )
        })
        output$output_point_size_column_options <- renderUI({
            req(input_data())

            selectInput(NS(id, "point_size_column"),
                "Select point size column:",
                names(input_data()),
                multiple = FALSE,
                selected = "Count"
            )
        })
        output$output_point_color_column_options <- renderUI({
            req(input_data())

            selectInput(NS(id, "point_color_column"),
                "Select point color column:",
                names(input_data()),
                multiple = FALSE,
                selected = "p.adjust"
            )
        })
        output$output_ontology_column_options <- renderUI({
            req(input_data())

            selectInput(NS(id, "ontology_column"),
                "Select ontology column:",
                c("none", names(input_data())),
                multiple = FALSE,
                selected = "none"
            )
        })
        output$output_cluster_levels_options <- renderUI({
            req(input$cluster_mode)

            x_column <- input$x_column

            selectInput(NS(id, "cluster_levels"),
                "Select cluster levels:",
                unique(input_data()[[x_column]]),
                multiple = TRUE,
                selected = unique(input_data()[[x_column]])
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
        ready_input_data <- reactive({
            req(input_data())

            suppressWarnings(suppressMessages(library(tidyverse)))

            data <- input_data()

            x_column <- input$x_column
            point_size_column <- input$point_size_column
            point_color_column <- input$point_color_column
            ontology_column <- input$ontology_column

            cluster_mode <- input$cluster_mode
            auto_sort_mode <- input$auto_sort_mode
            text_width <- input$text_width

            if (cluster_mode) {
                cluster_levels <- input$cluster_levels
                data[[x_column]] <- factor(data[[x_column]], levels = cluster_levels)
            }

            data <- data %>%
                mutate(
                    GeneRatio = sapply(GeneRatio, function(x) {
                        y <- as.numeric(strsplit(x, "/", fixed = TRUE)[[1]])
                        y[1] / y[2]
                    }),
                    Description = str_wrap(Description, width = text_width)
                )

            if (ontology_column != "none") {
                data[[ontology_column]] <- factor(data[[ontology_column]], levels = c("BP", "CC", "MF"))
            }

            if (auto_sort_mode) {
                if (cluster_mode) {
                    data <- data[order(data[[x_column]], -data[[point_size_column]], data[[point_color_column]]), ]
                    data[["Description"]] <- factor(data[["Description"]], levels = rev(unique(data[["Description"]])))
                } else {
                    data <- data[order(data[[x_column]]), ]
                    data[["Description"]] <- factor(data[["Description"]], levels = unique(data[["Description"]]))
                }
            } else {
                given_description_levels <- description_levels() %>%
                    pull(Description) %>%
                    unique() %>%
                    str_wrap(width = text_width)
                data[["Description"]] <- factor(data[["Description"]], levels = rev(given_description_levels))
            }

            list(
                data = data,
                description_levels = if (!auto_sort_mode) {
                    given_description_levels
                } else {
                    NULL
                }
            )
        })
        output$output_non_overlap_description_levels <- renderText({
            req(
                ready_input_data(),
                description_levels(),
                !input$auto_sort_mode
            )

            data <- ready_input_data()[["data"]]
            description_levels <- ready_input_data()[["description_levels"]]
            data_description_levels <- as.character(unique(data[["Description"]]))

            paste0(
                "Descriptions in description levels but not in data descriptions:\n\n",
                paste0(description_levels[!(description_levels %in% unique(data[["Description"]]))],
                    collapse = paste0("\n", paste0(rep("-", 80), collapse = ""), "\n")
                ),
                "\n\n", paste0(rep("=", 80), collapse = ""), "\n\n",
                "And vice versa:\n\n",
                paste0(data_description_levels[!(data_description_levels %in% description_levels)],
                    collapse = paste0("\n", paste0(rep("-", 80), collapse = ""), "\n")
                )
            )
        })
        output_plot <- reactive({
            req(
                ready_input_data()
            )

            suppressWarnings(suppressMessages(library(ggprism)))

            data <- ready_input_data()[["data"]]

            x_column <- input$x_column
            point_size_column <- input$point_size_column
            point_color_column <- input$point_color_column
            ontology_column <- input$ontology_column

            geom_point_colors <- trimws(strsplit(input$geom_point_colors, ",", fixed = TRUE)[[1]])
            theme_prism_panel_border <- input$theme_prism_panel_border
            scale_size_range <- input$scale_size_range
            font_family <- input$font_family
            theme_base_font_size <- input$theme_base_font_size
            panel_grid_major_line_color <- input$panel_grid_major_line_color

            p <- ggplot(data, aes(.data[[x_column]], Description,
                color = .data[[point_color_column]],
                size = .data[[point_size_column]]
            )) +
                geom_point() +
                scale_color_gradient(low = geom_point_colors[1], high = geom_point_colors[2]) +
                scale_size(range = scale_size_range) +
                theme_prism(
                    border = theme_prism_panel_border,
                    base_family = font_family,
                    base_size = theme_base_font_size
                ) +
                theme(
                    legend.title = element_text(),
                    panel.grid.major = element_line(color = panel_grid_major_line_color)
                )
            if (ontology_column != "none") {
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
                    paste0(extra_params$uni_prefix, "_go_dot.png")
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
                paste0(extra_params$uni_prefix, "_go_dot.pdf")
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
