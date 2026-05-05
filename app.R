library(shiny)

parse_fasta_text <- function(text, source_file = "uploaded.fasta") {
  lines <- unlist(strsplit(text, "\\r?\\n", perl = TRUE), use.names = FALSE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  if (!length(lines)) {
    stop("No FASTA content was detected in ", source_file, ".", call. = FALSE)
  }

  records <- list()
  current_name <- NULL
  current_sequence <- character()
  ignored_before_header <- 0L

  flush_record <- function() {
    if (is.null(current_name)) {
      return(NULL)
    }

    sequence <- paste0(gsub("\\s+", "", current_sequence), collapse = "")
    data.frame(
      source_file = source_file,
      name = current_name,
      width = nchar(sequence),
      sequence = sequence,
      stringsAsFactors = FALSE
    )
  }

  for (line in lines) {
    if (startsWith(line, ">")) {
      record <- flush_record()
      if (!is.null(record)) {
        records[[length(records) + 1L]] <- record
      }
      current_name <- trimws(sub("^>\\s*", "", line))
      current_sequence <- character()
    } else if (is.null(current_name)) {
      ignored_before_header <- ignored_before_header + 1L
    } else {
      current_sequence <- c(current_sequence, line)
    }
  }

  record <- flush_record()
  if (!is.null(record)) {
    records[[length(records) + 1L]] <- record
  }

  if (!length(records)) {
    stop("No FASTA records were found in ", source_file, ".", call. = FALSE)
  }

  result <- do.call(rbind, records)
  result$has_empty_sequence <- result$width == 0L
  result$ignored_lines_before_first_header <- ignored_before_header
  rownames(result) <- NULL
  result
}

empty_records <- function() {
  data.frame(
    source_file = character(),
    name = character(),
    width = integer(),
    sequence = character(),
    has_empty_sequence = logical(),
    ignored_lines_before_first_header = integer(),
    stringsAsFactors = FALSE
  )
}

find_duplicate_rows <- function(data, column) {
  if (!nrow(data) || !column %in% names(data)) {
    return(data[0, , drop = FALSE])
  }

  values <- data[[column]]
  data[duplicated(values) | duplicated(values, fromLast = TRUE), , drop = FALSE]
}

csv_download <- function(data, filename) {
  downloadHandler(
    filename = function() filename,
    content = function(file) write.csv(data(), file, row.names = FALSE)
  )
}

ui <- fluidPage(
  tags$head(
    tags$title("FASTA Processor"),
    tags$style(HTML("
      body { background: #f8fafc; color: #172033; }
      .container-fluid { max-width: 1180px; }
      .app-shell { padding: 24px 0 36px; }
      .title-row { display: flex; align-items: flex-end; justify-content: space-between; gap: 16px; margin-bottom: 18px; }
      h1 { margin: 0; font-size: 32px; font-weight: 700; letter-spacing: 0; }
      .subtitle { margin: 6px 0 0; color: #526071; max-width: 760px; }
      .panel { background: #fff; border: 1px solid #dce3ea; border-radius: 8px; padding: 18px; margin-bottom: 16px; }
      .metrics { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 12px; }
      .metric { background: #eef6f5; border: 1px solid #d4e7e4; border-radius: 8px; padding: 12px; min-height: 84px; }
      .metric:nth-child(2) { background: #f4f0fa; border-color: #e2d7ef; }
      .metric:nth-child(3) { background: #fff7e8; border-color: #f2dfba; }
      .metric:nth-child(4) { background: #edf3fb; border-color: #d5e2f1; }
      .metric-label { color: #5d6878; font-size: 13px; margin-bottom: 4px; }
      .metric-value { color: #172033; font-size: 28px; font-weight: 700; line-height: 1.1; }
      .help-block { color: #5d6878; }
      .table { background: #fff; }
      .btn { border-radius: 6px; }
      @media (max-width: 760px) {
        .title-row { display: block; }
        .metrics { grid-template-columns: repeat(2, minmax(0, 1fr)); }
      }
      @media (max-width: 460px) {
        .metrics { grid-template-columns: 1fr; }
      }
    "))
  ),
  div(
    class = "app-shell",
    div(
      class = "title-row",
      div(
        h1("FASTA Processor"),
        p(
          class = "subtitle",
          "Upload FASTA files, convert records to a table, identify duplicate names or sequences, and download CSV output."
        )
      )
    ),
    sidebarLayout(
      sidebarPanel(
        class = "panel",
        fileInput(
          "fasta_files",
          "FASTA files",
          multiple = TRUE,
          accept = c(".fa", ".fasta", ".faa", ".fna", ".txt")
        ),
        checkboxInput("show_duplicates_only", "Show only duplicate entries in the sequence table", FALSE),
        tags$hr(),
        downloadButton("download_all", "Download all records"),
        downloadButton("download_duplicate_names", "Download duplicate names"),
        downloadButton("download_duplicate_sequences", "Download duplicate sequences"),
        width = 3
      ),
      mainPanel(
        width = 9,
        uiOutput("parse_error"),
        div(
          class = "panel",
          div(
            class = "metrics",
            div(class = "metric", div(class = "metric-label", "Files"), div(class = "metric-value", textOutput("file_count", inline = TRUE))),
            div(class = "metric", div(class = "metric-label", "Sequences"), div(class = "metric-value", textOutput("sequence_count", inline = TRUE))),
            div(class = "metric", div(class = "metric-label", "Duplicate names"), div(class = "metric-value", textOutput("duplicate_name_count", inline = TRUE))),
            div(class = "metric", div(class = "metric-label", "Duplicate sequences"), div(class = "metric-value", textOutput("duplicate_sequence_count", inline = TRUE)))
          )
        ),
        div(
          class = "panel",
          tabsetPanel(
            tabPanel("Sequences", tableOutput("sequence_table")),
            tabPanel("Duplicate names", tableOutput("duplicate_name_table")),
            tabPanel("Duplicate sequences", tableOutput("duplicate_sequence_table"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  parsed <- reactive({
    if (is.null(input$fasta_files) || !nrow(input$fasta_files)) {
      return(empty_records())
    }

    pieces <- Map(
      function(path, name) {
        text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        parse_fasta_text(text, name)
      },
      input$fasta_files$datapath,
      input$fasta_files$name
    )

    do.call(rbind, pieces)
  })

  safe_parsed <- reactive({
    tryCatch(
      parsed(),
      error = function(err) {
        records <- empty_records()
        attr(records, "parse_error") <- conditionMessage(err)
        records
      }
    )
  })

  duplicate_names <- reactive(find_duplicate_rows(safe_parsed(), "name"))
  duplicate_sequences <- reactive(find_duplicate_rows(safe_parsed(), "sequence"))

  output$parse_error <- renderUI({
    error <- attr(safe_parsed(), "parse_error")
    if (is.null(error)) {
      return(NULL)
    }
    div(class = "alert alert-danger", error)
  })

  output$file_count <- renderText({
    if (is.null(input$fasta_files)) "0" else length(input$fasta_files$name)
  })

  output$sequence_count <- renderText(nrow(safe_parsed()))
  output$duplicate_name_count <- renderText(nrow(duplicate_names()))
  output$duplicate_sequence_count <- renderText(nrow(duplicate_sequences()))

  output$sequence_table <- renderTable({
    data <- safe_parsed()
    if (isTRUE(input$show_duplicates_only)) {
      data <- unique(rbind(duplicate_names(), duplicate_sequences()))
    }
    head(data, 200)
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$duplicate_name_table <- renderTable({
    head(duplicate_names()[order(duplicate_names()$name), , drop = FALSE], 200)
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$duplicate_sequence_table <- renderTable({
    head(duplicate_sequences()[order(duplicate_sequences()$sequence), , drop = FALSE], 200)
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$download_all <- csv_download(safe_parsed, "fasta-records.csv")
  output$download_duplicate_names <- csv_download(duplicate_names, "fasta-duplicate-names.csv")
  output$download_duplicate_sequences <- csv_download(duplicate_sequences, "fasta-duplicate-sequences.csv")
}

shinyApp(ui, server)
