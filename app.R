library(shiny)
library(bslib)
library(uuid)
library(digest)

# 100MB
options(shiny.maxRequestSize = 100 * 1024^2)

ui <- page_fluid(
    theme = bs_theme(preset = "united"),
    page_navbar(
        title = "NeuroBorder",
        home_page_ui("home_page"),
        nav_panel(
            "Graph Apps",
            navset_pill(
                nav_menu(
                    "Versatile",
                    versatile_pca_plot_ui("versatile_pca_plot"),
                    versatile_violin_plot_ui("versatile_violin_plot"),
                    versatile_bar_plot_ui("versatile_bar_plot")
                ),
                nav_menu(
                    "DE",
                    de_volcano_plot_ui("de_volcano_plot")
                ),
                nav_menu(
                    "GSEA",
                    gsea_line_plot_ui("gsea_line_plot"),
                    gsea_bar_plot_ui("gsea_bar_plot")
                ),
                nav_menu(
                    "GO",
                    go_dot_plot_ui("go_dot_plot"),
                    go_bar_plot_ui("go_bar_plot")
                ),
                graph_gallary_ui("graph_gallary_page")
            )
        ),
        about_page_ui("about_page")
    )
)

server <- function(input, output, session) {
    # bs_themer()
    uni_str <- paste0(c(session$token, UUIDgenerate(), UUIDgenerate(use.time = TRUE)), collapse = "_")
    extra_params <- list(
        # /data/tmp/neuroborder_shiny_app
        # /tmp
        temp_dir = "/data/tmp/neuroborder_shiny_app",
        uni_prefix = digest(uni_str, algo = "sha256", serialize = FALSE)
    )

    if (!dir.exists(extra_params$temp_dir)) {
        dir.create(extra_params$temp_dir, recursive = TRUE)
    }

    session$onSessionEnded(function() {
        temp_files <- list.files(
            path = extra_params$temp_dir,
            pattern = paste0("^", extra_params$uni_prefix, "_\\w+\\.\\w+$"),
            full.names = TRUE, recursive = FALSE
        )
        if (all(file.remove(temp_files))) {
            message("session ", session$token, " has ended, and its temporary files in ", extra_params$temp_dir, " (", paste0(temp_files, collapse = ","), ")", " has been deleted!")
        }
    })

    versatile_pca_plot_server("versatile_pca_plot", session, extra_params)
    de_volcano_plot_server("de_volcano_plot", session, extra_params)
    gsea_line_plot_server("gsea_line_plot", session, extra_params)
    gsea_bar_plot_server("gsea_bar_plot", session, extra_params)
    go_dot_plot_server("go_dot_plot", session, extra_params)
    versatile_violin_plot_server("versatile_violin_plot", session, extra_params)
    versatile_bar_plot_server("versatile_bar_plot", session, extra_params)
    go_bar_plot_server("go_bar_plot", session, extra_params)
}

shinyApp(ui = ui, server = server)
