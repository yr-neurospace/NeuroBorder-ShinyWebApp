gsea_line_plot_ui <- function(id) {
    nav_panel(
        "Line Plot",
        hr(),
        h2("1. Upload data"),
        fileInput(NS(id, "input_data_file"), "Upload a RDS file:"),
        hr(),
        h2("2. Preview data"),
        fluidRow(column(12, DT::dataTableOutput(NS(id, "output_data_table")))),
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
            )
        ),
        card(
            card_header("Figure-related parameters"),
            fluidRow(
                column(3, textInput(
                    NS(id, "gseaplot_rel_heights"),
                    "Relative heights:",
                    value = "1.5,0.5,1"
                )),
                column(3, textInput(
                    NS(id, "gseaplot_subplots"),
                    "Show which subplots:",
                    value = "1,2,3"
                )),
                column(3, textInput(
                    NS(id, "gsea_line_color"),
                    "Line color:",
                    value = "green"
                )),
                column(3, numericInput(
                    NS(id, "gsea_base_font_size"),
                    "Base font size:",
                    value = 11, min = 0
                ))
            ),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "gsea_pvalue_table"),
                    "Show p-value table", FALSE
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
                value = 450, min = 0
            )),
            column(2, numericInput(NS(id, "input_figure_height"),
                "Figure height:",
                value = 270, min = 0
            )),
            fluidRow(
                column(4, checkboxInput(
                    NS(id, "use_recommended_figure_height"),
                    "Use recommended figure height", TRUE
                ))
            )
        ),
        fluidRow(column(12, imageOutput(NS(id, "output_preview_png_image"),
            width = "100%", height = "100%"
        )))
    )
}

gsea_line_plot_server <- function(id, session, extra_params) {
    moduleServer(id, function(input, output, session) {
        input_data <- reactive({
            req(input$input_data_file)

            readRDS(input$input_data_file$datapath)
        })
        output$output_data_table <- DT::renderDataTable({
            req(input_data())

            DT::datatable(input_data()@result)
        })
        output_plot <- reactive({
            req(
                input_data(),
                input$go_ids
            )

            suppressWarnings(suppressMessages(library(enrichplot)))
            suppressWarnings(suppressMessages(library(aplot)))
            suppressWarnings(suppressMessages(library(stringr)))

            data <- input_data()

            go_ids <- trimws(strsplit(input$go_ids, ",", fixed = TRUE)[[1]])

            if (!all(str_detect(go_ids, "^GO:[0-9]{7}$")) || length(go_ids) > 20) {
                validate("some input GO IDs are malformed and assure that the number of input GO IDs <= 20")
            }

            gseaplot_rel_heights <- as.numeric(trimws(strsplit(input$gseaplot_rel_heights, ",", fixed = TRUE)[[1]]))
            gseaplot_subplots <- as.numeric(trimws(strsplit(input$gseaplot_subplots, ",", fixed = TRUE)[[1]]))
            gsea_pvalue_table <- input$gsea_pvalue_table
            gsea_line_color <- input$gsea_line_color
            gsea_base_font_size <- input$gsea_base_font_size

            plot_ls <- vector("list", length(go_ids))
            for (i in seq_len(length(go_ids))) {
                plot_ls[[i]] <- gseaplot2(data,
                    geneSetID = go_ids[i],
                    title = data@result[data@result[["ID"]] == go_ids[i], ][["Description"]],
                    rel_heights = gseaplot_rel_heights,
                    subplots = gseaplot_subplots,
                    pvalue_table = gsea_pvalue_table,
                    color = gsea_line_color,
                    base_size = gsea_base_font_size
                )
            }

            plot_list(
                gglist = plot_ls,
                ncol = 1, byrow = FALSE
            )
        })
        output$output_preview_png_image <- renderImage(
            {
                req(output_plot())

                input_figure_width <- input$input_figure_width
                go_ids <- trimws(strsplit(input$go_ids, ",", fixed = TRUE)[[1]])
                if (input$use_recommended_figure_height == TRUE) {
                    input_figure_height <- 270 * length(go_ids)
                } else {
                    input_figure_height <- input$input_figure_height
                }

                output_figure_file <- file.path(
                    extra_params$temp_dir,
                    paste0(extra_params$uni_prefix, "_gsea_line.png")
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
                paste0(extra_params$uni_prefix, "_gsea_line.pdf")
            },
            content = function(file) {
                input_figure_width <- input$input_figure_width
                go_ids <- trimws(strsplit(input$go_ids, ",", fixed = TRUE)[[1]])
                if (input$use_recommended_figure_height == TRUE) {
                    input_figure_height <- 270 * length(go_ids)
                } else {
                    input_figure_height <- input$input_figure_height
                }

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
