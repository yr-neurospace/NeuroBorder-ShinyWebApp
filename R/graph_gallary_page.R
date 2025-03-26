graph_gallary_ui <- function(id) {
    nav_panel(
        "Graph Gallary",
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
        ),
        hr(),
        hr(),
        h2("1. Violin plot"),
        fluidRow(
            column(4, tags$img(src = "violin_template_1.png"), align = "center")
        ),
        fluidRow(
            column(4, tags$a(href = "violin_template_1.tar.gz", class = "btn-download", target = "_blank", "Download Template"), align = "center")
        ),
        hr(),
        hr(),
        h2("2. Volcano plot")
    )
}
