#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      header = dashboardHeader(title = "Gestión whatsapp"),
      sidebar = dashboardSidebar(#expand_on_hover = F,
        # Sidebar #####
        sidebarMenu(
          menuItem("Solicitudes",
                   tabName = "solicitudes",
                   icon = icon("dashboard")
          ),
          menuItem("Análisis whatsapp",
                   tabName = "whatsapp",
                   icon = icon("whatsapp")
                   )
        )
      ),
      body = dashboardBody(
        tabItems(
          tabItem(tabName = "solicitudes",
                  mod_solicitudes_ui("solicitudes_1")
          ),
          tabItem(tabName = "whatsapp",
                  mod_analisis_whats_ui("analisis_whats_1")
                  )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "whatsappListening"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
