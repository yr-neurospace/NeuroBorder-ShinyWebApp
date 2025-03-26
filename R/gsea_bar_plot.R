gsea_bar_plot_ui <- function(id) {
    nav_panel(
        "Bar Plot",
        hr(),
        h2("1. Upload data"),
        fileInput(NS(id, "input_data_file"), "Upload a TSV file:"),
        hr(),
        h2("2. Preview data"),
        fluidRow(column(12, DT::dataTableOutput(NS(id, "output_data_table")))),
        fluidRow(
            column(3, uiOutput(NS(id, "output_x_column_options"))),
            column(3, uiOutput(NS(id, "output_y_column_options"))),
            column(3, uiOutput(NS(id, "output_fill_column_options"))),
            column(3, uiOutput(NS(id, "output_ontology_column_options")))
        ),
        hr(),
        h2("3. Set parameters"),
        card(
            card_header("Data-related parameters"),
            fluidRow(
                column(3, uiOutput(NS(id, "output_fill_levels_options"))),
                column(9, textInput(
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
                ))
            )
        ),
        card(
            card_header("Figure-related parameters"),
            fluidRow(
                column(2, textInput(
                    NS(id, "y_label_colors"),
                    "Description text colors:",
                    value = "",
                    placeholder = "e.g. black,black"
                )),
                column(2, numericInput(
                    NS(id, "text_width"),
                    "Text width:",
                    value = 10000, min = 0
                )),
                column(2, numericInput(
                    NS(id, "theme_base_font_size"),
                    "Base font size:",
                    value = 11, min = 0
                )),
                column(2, uiOutput(NS(id, "output_font_families"))),
                column(2, textInput(
                    NS(id, "fill_colors"),
                    "Fill colors:",
                    value = "none"
                )),
                column(2, textInput(NS(id, "scale_x_expand_add"),
                    "X-axis expansions:",
                    value = "0,0"
                ))
            ),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "theme_prism_panel_border"),
                    "Plot panel border", TRUE
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

gsea_bar_plot_server <- function(id, session, extra_params) {
    moduleServer(id, function(input, output, session) {
        input_data <- reactive({
            req(input$input_data_file)

            vroom::vroom(input$input_data_file$datapath)
        })
        extended_input_data <- reactive({
            req(input_data())

            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(glue)))
            suppressWarnings(suppressMessages(library(scales)))

            data <- input_data()
            text_width <- input$text_width

            y_label_colors <- trimws(strsplit(input$y_label_colors, ",", fixed = TRUE)[[1]])
            if (length(y_label_colors) != 2) {
                y_label_colors <- pal_hue()(2)
            }

            data <- data %>%
                mutate(
                    Description = gsub("\\n", "<br>", str_wrap(Description, width = text_width)),
                    NES_sign = if_else(NES >= 0, "+", "-"),
                    NES_sign = factor(NES_sign, levels = c("-", "+")),
                    minus_log10_padj = -log10(p.adjust),
                    NES_abs = abs(NES),
                    enrichmentScore_abs = abs(enrichmentScore)
                )

            inner_join(data, tibble(
                y_label_color = y_label_colors,
                NES_sign = c("+", "-")
            ), by = "NES_sign") %>%
                mutate(colored_description = glue('<span style="color:{y_label_color};">{Description}</span>'))
        })
        output$output_data_table <- DT::renderDataTable({
            req(extended_input_data())

            DT::datatable(extended_input_data())
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
                selected = "NES_abs"
            )
        })
        output$output_y_column_options <- renderUI({
            req(extended_input_data())

            selectInput(NS(id, "y_column"),
                "Select y column:",
                names(extended_input_data()),
                multiple = FALSE,
                selected = "Description"
            )
        })
        output$output_fill_column_options <- renderUI({
            req(extended_input_data())

            selectInput(NS(id, "fill_column"),
                "Select fill color column:",
                c("none", names(extended_input_data())),
                multiple = FALSE,
                selected = "NES_sign"
            )
        })
        output$output_ontology_column_options <- renderUI({
            req(extended_input_data())

            selectInput(NS(id, "ontology_column"),
                "Select ontology column:",
                c("none", names(extended_input_data())),
                multiple = FALSE,
                selected = "none"
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
            fill_column <- input$fill_column
            fill_levels <- input$fill_levels
            font_family <- input$font_family
            text_width <- input$text_width
            scale_x_expand_add <- as.numeric(trimws(strsplit(input$scale_x_expand_add, ",", fixed = TRUE)[[1]]))
            theme_prism_panel_border <- input$theme_prism_panel_border
            theme_base_font_size <- input$theme_base_font_size

            fill_colors <- trimws(strsplit(input$fill_colors, ",", fixed = TRUE)[[1]])

            if (ontology_column != "none") {
                data[[ontology_column]] <- factor(data[[ontology_column]], levels = c("BP", "CC", "MF"))
            }

            data <- data[order(data[["NES_sign"]], abs(data[[x_column]])), ] %>%
                mutate(
                    Description = factor(Description, levels = unique(Description)),
                    colored_description = factor(colored_description, levels = unique(colored_description))
                )

            p <- ggplot(data, aes(.data[[x_column]], .data[[y_column]],
                fill = if (fill_column == "none") NULL else .data[[fill_column]]
            )) +
                geom_col() +
                scale_x_continuous(expand = expansion(add = scale_x_expand_add)) +
                theme_prism(
                    border = theme_prism_panel_border,
                    base_family = font_family,
                    base_size = theme_base_font_size
                ) +
                theme(
                    axis.text.y = element_markdown(),
                    legend.title = element_text()
                )
            if (fill_column != "none" && all(fill_colors != "none") && is.numeric(data[[fill_column]])) {
                p <- p + scale_fill_gradient(low = fill_colors[1], high = fill_colors[2])
            }
            if (fill_column != "none" && all(fill_colors != "none") && !is.numeric(data[[fill_column]])) {
                p <- p + scale_fill_manual(values = setNames(fill_colors, fill_levels))
            }
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
                    paste0(extra_params$uni_prefix, "_gsea_bar.png")
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
                paste0(extra_params$uni_prefix, "_gsea_bar.pdf")
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
