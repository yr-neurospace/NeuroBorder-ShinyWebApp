library(shiny)
library(bslib)

# 100MB
options(shiny.maxRequestSize = 100 * 1024^2)

ui <- page_fluid(
    theme = bs_theme(preset = "cerulean"),
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
                )
            )
        ),
        about_page_ui("about_page")
    )
)

server <- function(input, output, session) {
    extra_params <- list(
        temp_dir = tempdir()
    )

    session$onSessionEnded(function() {
        unlink(extra_params$temp_dir, recursive = TRUE, expand = TRUE, force = FALSE)
        message("session ", session$token, " has ended, and the temporary directory ", extra_params$temp_dir, " has been deleted")
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
