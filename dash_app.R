
df_plot$hover_text <- paste(
  "<b>", df_plot$geo, "</b>",
  "<br>Student teacher ratio:", df_plot$student_teach_ratio,
  "<br>Expected schooling:", round(df_plot$schooling, 2),
  "<br>Bashelor's degree enrollment:", round(df_plot$enrollment, 2)
)


draw_map <- function (col) {
  bbox <- st_bbox(df_plot$geometry)
  
  fig <- plot_ly(
    data = df_plot,
    type = 'choropleth',
    locations = ~geo,
    locationmode = 'ISO-3',
    z = df_plot[[col]],
    text = ~hover_text,
    hoverinfo = "text",
    colorscale = "turbo",
    cmin = min(df_plot[[col]]),
    cmax = max(df_plot[[col]]),
    marker = list(line = list(color = 'white', width = 0.5))
  ) %>%
    layout(
      geo = list(
        lonaxis = list(range = c(bbox["xmin"], bbox["xmax"])),
        lataxis = list(range = c(bbox["ymin"], bbox["ymax"])),
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = 'natural earth')
      ),
      title = paste(col, "in Europe")
    )
  
  return (fig) 
}

htmlwidgets::saveWidget(draw_map('student_teach_ratio'), "temp_file.html")

app <- dash_app() %>% set_layout(
  html$main(
  div(
    html$h1("Projektinio darbo interaktyvus žemėlapis"),
    html$h3("Švietimo rodikliai Europoje"),
    html$p("Kintamųjų reikšmės:"),
    html$p("student_teacher_ratio - vienam mokytojui tenkantis mokinių skiačius"),
    html$p("schooling - vidutinis metų skaičius, skirtas mokslui"),
    html$p("enrollment - bakalauro studijas pasirinkusių jaunuolių procentas"),
    html$p("teacher_pay - mokytojų algos"),
    div(
      div(
        html$p("Išsirinkite kintamąjį:"),
        dccDropdown(
          id = 'column',
          options = c(
            "student_teach_ratio",
            "schooling",
            "enrollment",
            "teacher_pay"
          ),
          value = "student_teach_ratio",
        ),
        style=list(
          "width" = "75%"
        )
      ),
      style=list(
        "width" = "100%",
        "display" = "flex"
      )
    ),
    dccGraph(
      id = "main-graph",
      style=list(
        width = "75%",
        "margin-top" = "10pt"
      )  
    ),
    style=list(
     "width" = "75%",
     "font-family" = "Helvetica",
     "margin" = "auto"
    )
  )
  )
)


app %>% add_callback(
  output('main-graph', 'figure'),
  input('column', 'value'),
  draw_map
)

run_app(app)
