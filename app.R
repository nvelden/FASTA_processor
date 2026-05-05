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

sequence_preview <- function(sequence, limit = 72L) {
  preview <- substr(sequence, 1L, limit)
  ifelse(nchar(sequence) > limit, paste0(preview, "..."), preview)
}

format_records_for_display <- function(data) {
  if (!nrow(data)) {
    return(data.frame(
      source_file = character(),
      name = character(),
      width = integer(),
      sequence_preview = character(),
      has_empty_sequence = logical(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    source_file = basename(data$source_file),
    name = data$name,
    width = data$width,
    sequence_preview = sequence_preview(data$sequence),
    has_empty_sequence = data$has_empty_sequence,
    stringsAsFactors = FALSE
  )
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
      :root {
        --ink: #172033;
        --muted: #5c6a7d;
        --line: #d8e1ea;
        --page: #eef3f7;
        --panel: #ffffff;
        --teal: #16756f;
        --teal-dark: #0f5f5a;
        --blue: #2f6eb3;
        --amber: #b36a13;
      }
      html, body { min-height: 100%; }
      body {
        background:
          linear-gradient(180deg, #f7fafc 0%, var(--page) 62%, #e8eef4 100%);
        color: var(--ink);
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      }
      .container-fluid { max-width: 1220px; }
      .app-shell { padding: 28px 0 44px; }
      .title-row {
        border-bottom: 1px solid var(--line);
        margin-bottom: 20px;
        padding-bottom: 18px;
      }
      h1 { margin: 0; font-size: 34px; font-weight: 760; letter-spacing: 0; }
      .subtitle { margin: 8px 0 0; color: var(--muted); max-width: 780px; font-size: 15px; }
      .control-panel,
      .surface {
        background: rgba(255, 255, 255, 0.96);
        border: 1px solid var(--line);
        border-radius: 8px;
        box-shadow: 0 12px 28px rgba(23, 32, 51, 0.08);
      }
      .control-panel {
        background: rgba(255, 255, 255, 0.98) !important;
        padding: 18px;
        margin-bottom: 18px;
      }
      .surface {
        padding: 18px;
        margin-bottom: 18px;
      }
      .well:not(.control-panel) {
        background: transparent;
        border: 0;
        box-shadow: none;
      }
      label { color: var(--ink); font-weight: 700; }
      .form-control,
      .btn,
      .input-group .form-control,
      .input-group-btn .btn {
        border-radius: 6px;
        border-color: #cbd6e0;
      }
      .form-control:focus {
        border-color: var(--teal);
        box-shadow: 0 0 0 3px rgba(22, 117, 111, 0.15);
      }
      .checkbox { margin-top: 18px; }
      .control-panel hr { border-top-color: var(--line); margin: 18px 0; }
      .btn {
        font-weight: 650;
        background: #ffffff;
        color: var(--ink);
      }
      .btn:hover,
      .btn:focus {
        border-color: var(--teal);
        color: var(--teal-dark);
        background: #f5fbfa;
      }
      .shiny-download-link {
        display: block;
        width: 100%;
        margin: 8px 0;
        text-align: left;
      }
      .metrics { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 14px; }
      .metric {
        border: 1px solid var(--line);
        border-left: 5px solid var(--teal);
        border-radius: 8px;
        padding: 14px 14px 16px;
        min-height: 92px;
        background: linear-gradient(180deg, #f8fffe 0%, #edf8f7 100%);
      }
      .metric:nth-child(2) {
        border-left-color: var(--blue);
        background: linear-gradient(180deg, #fbfdff 0%, #edf4fb 100%);
      }
      .metric:nth-child(3) {
        border-left-color: var(--amber);
        background: linear-gradient(180deg, #fffdf8 0%, #fff4df 100%);
      }
      .metric:nth-child(4) {
        border-left-color: #6f4fb3;
        background: linear-gradient(180deg, #fcfbff 0%, #f2edfb 100%);
      }
      .metric-label { color: var(--muted); font-size: 13px; margin-bottom: 8px; font-weight: 650; }
      .metric-value { color: var(--ink); font-size: 32px; font-weight: 760; line-height: 1; }
      .nav-tabs { border-bottom-color: var(--line); }
      .nav-tabs > li > a {
        border-radius: 6px 6px 0 0;
        color: var(--blue);
        font-weight: 650;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: var(--ink);
        border-color: var(--line);
        border-bottom-color: #fff;
      }
      .tab-content { overflow-x: auto; padding-top: 2px; }
      .table {
        background: #fff;
        border: 1px solid var(--line);
        margin-top: 0;
        width: 100%;
        table-layout: fixed;
      }
      .table > thead > tr > th,
      .table > tbody > tr > th {
        background: #f7fafc;
        border-bottom: 1px solid var(--line);
        color: var(--ink);
        font-weight: 750;
        white-space: nowrap;
      }
      .table > tbody > tr > td {
        overflow-wrap: anywhere;
        word-break: break-word;
        vertical-align: top;
      }
      .table > thead > tr > th:nth-child(1),
      .table > tbody > tr > td:nth-child(1) { width: 24%; }
      .table > thead > tr > th:nth-child(2),
      .table > tbody > tr > td:nth-child(2) { width: 38%; }
      .table > thead > tr > th:nth-child(3),
      .table > tbody > tr > td:nth-child(3) { width: 8%; text-align: right; }
      .table > thead > tr > th:nth-child(4),
      .table > tbody > tr > td:nth-child(4) {
        width: 22%;
        font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
        font-size: 12px;
        word-break: break-word;
      }
      .table > thead > tr > th:nth-child(5),
      .table > tbody > tr > td:nth-child(5) {
        width: 8%;
        text-align: center;
      }
      @media (max-width: 760px) {
        .app-shell { padding-top: 20px; }
        h1 { font-size: 28px; }
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
        class = "control-panel",
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
          class = "surface",
          div(
            class = "metrics",
            div(class = "metric", div(class = "metric-label", "Files"), div(class = "metric-value", textOutput("file_count", inline = TRUE))),
            div(class = "metric", div(class = "metric-label", "Sequences"), div(class = "metric-value", textOutput("sequence_count", inline = TRUE))),
            div(class = "metric", div(class = "metric-label", "Duplicate names"), div(class = "metric-value", textOutput("duplicate_name_count", inline = TRUE))),
            div(class = "metric", div(class = "metric-label", "Duplicate sequences"), div(class = "metric-value", textOutput("duplicate_sequence_count", inline = TRUE)))
          )
        ),
        div(
          class = "surface",
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
    format_records_for_display(head(data, 200))
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$duplicate_name_table <- renderTable({
    format_records_for_display(head(duplicate_names()[order(duplicate_names()$name), , drop = FALSE], 200))
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$duplicate_sequence_table <- renderTable({
    format_records_for_display(head(duplicate_sequences()[order(duplicate_sequences()$sequence), , drop = FALSE], 200))
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$download_all <- csv_download(safe_parsed, "fasta-records.csv")
  output$download_duplicate_names <- csv_download(duplicate_names, "fasta-duplicate-names.csv")
  output$download_duplicate_sequences <- csv_download(duplicate_sequences, "fasta-duplicate-sequences.csv")
}

shinyApp(ui, server)
