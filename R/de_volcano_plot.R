de_volcano_plot_ui <- function(id) {
    nav_panel(
        "Volcano Plot",
        hr(),
        h2("1. Upload data"),
        fluidRow(
            column(3, fileInput(NS(id, "input_data_file"), "Upload a TSV file:")),
            column(3, fileInput(NS(id, "input_anno_data_file"), "Upload an annotated TSV file:"))
        ),
        hr(),
        h2("2. Preview data"),
        fluidRow(column(12, DT::dataTableOutput(NS(id, "output_data_table")))),
        fluidRow(column(12, DT::dataTableOutput(NS(id, "output_anno_data_table")))),
        fluidRow(
            column(3, uiOutput(NS(id, "output_gene_id_column_options"))),
            column(3, uiOutput(NS(id, "output_diff_flag_column_options"))),
            column(3, uiOutput(NS(id, "output_logfc_column_options"))),
            column(3, uiOutput(NS(id, "output_padj_column_options")))
        ),
        hr(),
        h2("3. Set parameters"),
        card(
            card_header("Data-related parameters"),
            fluidRow(
                column(2, numericInput(NS(id, "logfc_threshold"),
                    "logFC threshold:",
                    value = 1, min = 0
                )),
                column(2, numericInput(NS(id, "padj_threshold"),
                    "p-value adjusted threshold:",
                    value = 0.05, min = 0
                )),
                column(2, numericInput(NS(id, "top_n"),
                    "Top N genes to label with:",
                    value = 0, min = 0, step = 1
                )),
                column(2, uiOutput(NS(id, "output_diff_flag_options"))),
                column(2, textInput(
                    NS(id, "diff_flag_colors"),
                    "Point/Label colors:",
                    value = "magenta4,cyan4,gray25"
                ))
            )
        ),
        card(
            card_header("Figure-related parameters"),
            fluidRow(
                column(2, uiOutput(NS(id, "output_font_families"))),
                column(2, numericInput(NS(id, "geom_point_size"),
                    "Point size:",
                    value = 2, min = 0
                )),
                column(2, numericInput(NS(id, "geom_text_size"),
                    "Label text size:",
                    value = 5, min = 0
                )),
                column(2, numericInput(NS(id, "min_segment_length"),
                    "Minimum segment length:",
                    value = 3, min = 0
                )),
                column(2, numericInput(NS(id, "geom_point_alpha"),
                    "Point transparency:",
                    value = 0.5, min = 0, max = 1
                )),
                column(2, numericInput(NS(id, "geom_line_linewidth"),
                    "Line width:",
                    value = 1, min = 0
                ))
            ),
            fluidRow(
                column(2, numericInput(NS(id, "geom_text_alpha"),
                    "Label text transparency:",
                    value = 1, min = 0, max = 1
                )),
                column(2, textInput(NS(id, "geom_line_color"),
                    "Line color:",
                    value = "grey25"
                )),
                column(2, uiOutput(NS(id, "output_geom_line_linetype_options"))),
                column(2, numericInput(NS(id, "theme_base_font_size"),
                    "Base font size:",
                    value = 16, min = 0
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

de_volcano_plot_server <- function(id, session, extra_params) {
    moduleServer(id, function(input, output, session) {
        raw_input_data <- reactive({
            req(input$input_data_file)

            vroom::vroom(input$input_data_file$datapath)
        })
        input_anno_data <- reactive({
            req(input_data())

            suppressWarnings(suppressMessages(library(magrittr)))
            suppressWarnings(suppressMessages(library(tidyverse)))

            top_n <- input$top_n
            gene_id_column <- input$gene_id_column
            diff_flag_column <- input$diff_flag_column
            logfc_column <- input$logfc_column
            padj_column <- input$padj_column
            diff_flag_levels <- input$diff_flag_levels

            if (isTruthy(input$input_anno_data_file)) {
                vroom::vroom(input$input_anno_data_file$datapath) %>%
                    select(all_of(c(gene_id_column, diff_flag_column, logfc_column, padj_column))) %>%
                    set_colnames(c("gene_id", "diff_flag", "logFC", "padj")) %>%
                    mutate(diff_flag = factor(diff_flag, levels = diff_flag_levels)) %>%
                    distinct()
            } else {
                input_data() %>%
                    filter(diff_flag != "NO") %>%
                    group_by(diff_flag) %>%
                    slice_max(abs(logFC), n = top_n)
            }
        })
        output$output_data_table <- DT::renderDataTable({
            req(raw_input_data())

            DT::datatable(raw_input_data())
        })
        output$output_anno_data_table <- DT::renderDataTable({
            req(input_anno_data())

            DT::datatable(input_anno_data())
        })
        output$output_diff_flag_options <- renderUI({
            req(
                raw_input_data(),
                input$diff_flag_column
            )

            raw_data <- raw_input_data()
            diff_flag_column <- input$diff_flag_column

            selectInput(NS(id, "diff_flag_levels"),
                "Select DE flag levels:",
                unique(raw_data[[diff_flag_column]]),
                multiple = TRUE
            )
        })
        output$output_gene_id_column_options <- renderUI({
            req(raw_input_data())

            selectInput(NS(id, "gene_id_column"),
                "Select gene ID column:",
                names(raw_input_data()),
                multiple = FALSE
            )
        })
        output$output_diff_flag_column_options <- renderUI({
            req(raw_input_data())

            selectInput(NS(id, "diff_flag_column"),
                "Select DE flag column:",
                names(raw_input_data()),
                multiple = FALSE
            )
        })
        output$output_logfc_column_options <- renderUI({
            req(raw_input_data())

            selectInput(NS(id, "logfc_column"),
                "Select logFC column:",
                names(raw_input_data()),
                multiple = FALSE
            )
        })
        output$output_padj_column_options <- renderUI({
            req(raw_input_data())

            selectInput(NS(id, "padj_column"),
                "Select p-value adjusted column:",
                names(raw_input_data()),
                multiple = FALSE
            )
        })
        input_data <- reactive({
            req(
                raw_input_data(), input$gene_id_column,
                input$diff_flag_column, input$logfc_column,
                input$padj_column, input$diff_flag_levels
            )

            suppressWarnings(suppressMessages(library(magrittr)))
            suppressWarnings(suppressMessages(library(tidyverse)))

            gene_id_column <- input$gene_id_column
            diff_flag_column <- input$diff_flag_column
            logfc_column <- input$logfc_column
            padj_column <- input$padj_column
            diff_flag_levels <- input$diff_flag_levels

            raw_input_data() %>%
                select(all_of(c(gene_id_column, diff_flag_column, logfc_column, padj_column))) %>%
                set_colnames(c("gene_id", "diff_flag", "logFC", "padj")) %>%
                mutate(diff_flag = factor(diff_flag, levels = diff_flag_levels)) %>%
                distinct()
        })
        output$output_geom_line_linetype_options <- renderUI({
            selectInput(NS(id, "geom_line_linetype"),
                "Select line type:",
                c(
                    "blank", "solid", "dashed", "dotted",
                    "dotdash", "longdash", "twodash"
                ),
                multiple = FALSE,
                selected = "dashed"
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
        output_plot <- reactive({
            req(
                input_data(),
                input_anno_data()
            )

            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(ggrepel)))
            suppressWarnings(suppressMessages(library(magrittr)))
            suppressWarnings(suppressMessages(library(ggprism)))

            data <- input_data()
            anno_data <- input_anno_data()

            diff_flag_colors <- trimws(str_split(input$diff_flag_colors, ",")[[1]])
            diff_flag_levels <- input$diff_flag_levels

            logfc_threshold <- input$logfc_threshold
            padj_threshold <- input$padj_threshold

            geom_point_alpha <- input$geom_point_alpha
            geom_point_size <- input$geom_point_size
            geom_line_linewidth <- input$geom_line_linewidth
            geom_line_linetype <- input$geom_line_linetype
            geom_line_color <- input$geom_line_color
            geom_text_size <- input$geom_text_size
            min_segment_length <- input$min_segment_length
            geom_text_alpha <- input$geom_text_alpha
            font_family <- input$font_family
            theme_prism_panel_border <- input$theme_prism_panel_border
            theme_base_font_size <- input$theme_base_font_size

            diff_flag_count_table <- count(data, diff_flag) %>%
                arrange(diff_flag)
            plot_title <- paste0(paste0(
                diff_flag_count_table$diff_flag, ": ",
                diff_flag_count_table$n
            ), collapse = "\n")

            p <- ggplot(data, aes(logFC, -log10(padj), color = diff_flag)) +
                geom_point(alpha = geom_point_alpha, size = geom_point_size) +
                geom_vline(
                    xintercept = c(-logfc_threshold, logfc_threshold),
                    linewidth = geom_line_linewidth,
                    color = geom_line_color,
                    linetype = geom_line_linetype
                ) +
                geom_hline(
                    yintercept = -log10(padj_threshold),
                    linewidth = geom_line_linewidth,
                    color = geom_line_color,
                    linetype = geom_line_linetype
                ) +
                geom_text_repel(
                    data = anno_data,
                    mapping = aes(logFC, -log10(padj), color = diff_flag, label = gene_id),
                    max.overlaps = 10000, show.legend = FALSE, alpha = geom_text_alpha,
                    min.segment.length = min_segment_length, size = geom_text_size
                ) +
                labs(
                    title = plot_title,
                    x = "logFC", y = "-log10(padj)",
                    color = paste0("padj < ", padj_threshold, "\nlogFC > ", logfc_threshold)
                ) +
                theme_prism(border = theme_prism_panel_border, base_family = font_family, base_size = theme_base_font_size) +
                theme(legend.title = element_text())
            if (length(diff_flag_colors) == length(diff_flag_levels)) {
                p + scale_color_manual(values = setNames(diff_flag_colors, diff_flag_levels))
            } else {
                p
            }
        })
        output$output_preview_png_image <- renderImage(
            {
                req(output_plot())

                input_figure_width <- input$input_figure_width
                input_figure_height <- input$input_figure_height

                output_figure_file <- file.path(
                    extra_params$temp_dir,
                    paste0(extra_params$uni_prefix, "_volcano.png")
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
                paste0(extra_params$uni_prefix, "_volcano.pdf")
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
