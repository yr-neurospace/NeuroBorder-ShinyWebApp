versatile_pca_plot_ui <- function(id) {
    nav_panel(
        "PCA Plot",
        hr(),
        h2("1. Upload data"),
        fileInput(NS(id, "input_data_file"), "Upload a TSV file:"),
        hr(),
        h2("2. Preview data"),
        fluidRow(column(12, DT::dataTableOutput(NS(id, "output_data_table")))),
        fluidRow(column(12, uiOutput(NS(id, "output_data_table_columns")))),
        hr(),
        h2("3. Set parameters"),
        card(
            card_header("Data-related parameters"),
            fluidRow(
                column(3, numericInput(NS(id, "filter_data_threshold"),
                    "Filtering threshold:",
                    value = 1, min = 0
                )),
                column(3, numericInput(NS(id, "least_num_samples"),
                    "The minimum number of samples:",
                    value = 1, min = 1, step = 1
                )),
                column(3, uiOutput(NS(id, "output_pca_dims_1_options"))),
                column(3, uiOutput(NS(id, "output_pca_dims_2_options")))
            ),
            fluidRow(
                column(3, checkboxInput(
                    NS(id, "data_log_trans"),
                    "log2 transformation", TRUE
                )),
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
                column(2, numericInput(NS(id, "geom_mark_ellipse_shadow_alpha"),
                    "Ellipse shadow transparency:",
                    value = 0.25, min = 0, max = 1
                )),
                column(2, numericInput(NS(id, "theme_base_font_size"),
                    "Base font size:",
                    value = 16, min = 0
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

versatile_pca_plot_server <- function(id, session, extra_params) {
    moduleServer(id, function(input, output, session) {
        input_data <- reactive({
            req(input$input_data_file)

            na.omit(vroom::vroom(input$input_data_file$datapath))
        })
        output$output_data_table <- DT::renderDataTable({
            req(input_data())

            DT::datatable(input_data())
        })
        output$output_data_table_columns <- renderUI({
            req(input_data())

            selectInput(NS(id, "excluded_data_columns"),
                "Select columns you want to exclude from your data:",
                names(input_data()),
                multiple = TRUE
            )
        })
        pca_dims_options <- paste0("Dim.", 1:10)
        output$output_pca_dims_1_options <- renderUI({
            selectInput(NS(id, "pca_dims_1"),
                "Select the first PC:",
                pca_dims_options,
                multiple = FALSE,
                selected = "Dim.1"
            )
        })
        output$output_pca_dims_2_options <- renderUI({
            req(input$pca_dims_1)

            selectInput(NS(id, "pca_dims_2"),
                "Select the second PC:",
                pca_dims_options[pca_dims_options != input$pca_dims_1],
                multiple = FALSE,
                selected = "Dim.2"
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
            req(input$excluded_data_columns)

            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(magrittr)))
            suppressWarnings(suppressMessages(library(FactoMineR)))
            suppressWarnings(suppressMessages(library(ggforce)))
            suppressWarnings(suppressMessages(library(ggrepel)))
            suppressWarnings(suppressMessages(library(ggprism)))

            excluded_data_columns <- input$excluded_data_columns
            filter_data_threshold <- input$filter_data_threshold
            least_num_samples <- input$least_num_samples
            data_log_trans <- input$data_log_trans
            pca_dims <- c(input$pca_dims_1, input$pca_dims_2)

            font_family <- input$font_family
            geom_point_size <- input$geom_point_size
            geom_text_size <- input$geom_text_size
            min_segment_length <- input$min_segment_length
            geom_mark_ellipse_shadow_alpha <- input$geom_mark_ellipse_shadow_alpha
            theme_prism_panel_border <- input$theme_prism_panel_border
            theme_base_font_size <- input$theme_base_font_size

            data <- input_data() %>%
                select(-any_of(excluded_data_columns)) %>%
                na.omit()

            if (!all(sapply(data, is.numeric))) {
                stop("all columns must be numerical")
            }

            if (all(str_detect(names(data), "^[a-zA-Z]+[a-zA-Z0-9]*_rep[0-9]+$"))) {
                has_rep <- TRUE
                message("your data has replicates")
            } else {
                has_rep <- FALSE
                message("your data has no replicate")
            }

            if (has_rep) {
                sample_df <- str_split(names(data), fixed("_")) %>%
                    do.call(rbind, .) %>%
                    as.data.frame() %>%
                    set_colnames(c("group", "replicate")) %>%
                    mutate(sample = paste0(group, "_", replicate))

                expr_flag <- rep(FALSE, nrow(data))
                for (g in unique(sample_df[["group"]])) {
                    tmp_df <- filter(sample_df, group == g)
                    expr_flag <- expr_flag | (rowSums(data[, tmp_df[["sample"]]] > filter_data_threshold) == nrow(tmp_df))
                }
            } else {
                sample_df <- tibble(
                    sample = names(data),
                    group = sample
                )
                expr_flag <- rowSums(data > filter_data_threshold) >= least_num_samples
            }
            data <- data[expr_flag, ]

            if (data_log_trans) {
                data <- t(log2(data + 1))
            }
            pca <- PCA(data, ncp = 10, scale.unit = TRUE, graph = FALSE)

            pca_coord <- as.data.frame(pca$ind$coord)
            pca_coord$sample <- row.names(pca_coord)
            pca_coord <- inner_join(pca_coord, sample_df, by = "sample")
            pca_eig <- as.data.frame(pca$eig)

            comps <- gsub("^Dim\\.", "comp ", pca_dims)
            pcs <- gsub("^Dim\\.", "PC", pca_dims)

            p <- ggplot(pca_coord, aes(.data[[pca_dims[1]]], .data[[pca_dims[2]]])) +
                geom_point(aes(color = group), size = geom_point_size) +
                geom_text_repel(aes(label = sample),
                    size = geom_text_size, max.overlaps = 10000,
                    min.segment.length = min_segment_length,
                    family = font_family
                ) +
                xlab(paste0(
                    pcs[1], " (",
                    round(pca_eig[comps[1], "percentage of variance"]),
                    "%)"
                )) +
                ylab(paste0(
                    pcs[2], " (",
                    round(pca_eig[comps[2], "percentage of variance"]),
                    "%)"
                ))

            if (has_rep) {
                p <- p + geom_mark_ellipse(aes(fill = group), color = NA, alpha = geom_mark_ellipse_shadow_alpha)
            }

            p + theme_prism(border = theme_prism_panel_border, base_family = font_family, base_size = theme_base_font_size) +
                theme(legend.title = element_text())
        })
        output$output_preview_png_image <- renderImage(
            {
                req(output_plot())

                input_figure_width <- input$input_figure_width
                input_figure_height <- input$input_figure_height

                output_figure_file <- file.path(
                    extra_params$temp_dir,
                    paste0(extra_params$uni_prefix, "_pca.png")
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
                paste0(extra_params$uni_prefix, "_pca.pdf")
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
