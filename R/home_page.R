home_page_ui <- function(id) {
    nav_panel(
        "Home",
        tags$h2("1. Introduction"),
        tags$p(
            "This Shiny web app contains some useful apps (e.g. Graph Apps), developed for various purposes. ", tags$br(),
            "The purposes of most apps and their parameters can be directly inferred from their names. ", tags$br(),
            "Anyway, you can try each parameter yourself freely! If you find any bug or want more apps/functionalities, ", tags$br(),
            "please let me know. I'll fix or develop it as I can. At present, Graph Apps is mainly developed to visualize ", tags$br(),
            "results retrieved from ", tags$b("NeuroBorder Galaxy"), ". Of course, you can apply it to any data with ", tags$b(),
            "compatiable format."
        ),
        tags$hr(),
        tags$h2("2. Documentations"),
        tags$p(
            "Some helpful guides can be found at ", tags$a("NeuroBorder", href = "https://www.neuroborder.com"), " under ",
            tags$b("Galaxy of Blogs"), ". ", tags$br(), "If you want more useful guides, please let me know. I'll consider it seriously. ", tags$br(),
            "If indeed necessary, I'll post one for it."
        ),
        tags$hr(),
        tags$h2("3. Notes"),
        tags$p(
            "As an alternative, you can download this Shiny web app from ",
            tags$a("NeuroBorder-ShinyWebApp", href = "https://github.com/yr-neurospace/NeuroBorder-ShinyWebApp"), ". ", tags$br(),
            "And then run it in your laptop."
        ),
        tags$hr(),
        tags$h2("4. Related Links"),
        tags$p(
            "This Shiny web app is developed to meet the most common needs due to limited energy and demands. ", tags$br(),
            "This means that only minimal functionality and flexibility are provided. If you want more features and greater flexibility, ", tags$br(),
            "using an advanced programming language is highly recommended, e.g. R, Python, Julia, etc., which gives you ", tags$br(),
            "full functionality and flexibility. And most of them are available for free access and use. ", tags$br(),
            "Of course, it is more convenient to directly use a software, such as GraphPad Prism, Origin, Excel, ", tags$br(),
            tags$a("ImageGP (old)", href = "https://www.bic.ac.cn/ImageGP"), ", ", tags$a("ImageGP (new)", href = "https://www.bic.ac.cn/BIC/#"), " etc., ",
            "which gives you some user-friendly operations and guides with the lack of full functionality and flexibility. ", tags$br(),
            "And most of them are proprietary softwares that require payment. Finally, trying to use an advanced programming language ", tags$br(),
            "will liberate yourself from the clutter of softwares once you have mastered it. Due to the remarkable progress of AI day by day, ", tags$br(),
            "it will be more convenient for you to learn and use an advanced programming language to meet most of your needs than ever before."
        ),
        tags$hr()
    )
}
