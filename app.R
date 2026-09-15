library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(stringr)
library(htmltools)

DATA_PATH <- "data/ToBeAnnotated.csv"
NDAS_PATH <- "data/NDAs.csv"
LABELS_PATH <- "data/labels.csv"

PATTERN_CHOICES <- c(
  "P01 — Deontic Force" = "P01",
  "P02 — Negation" = "P02",
  "P03 — Exception / Carve-Out" = "P03",
  "P04 — Conditional Trigger" = "P04",
  "P05 — Temporal Dependency" = "P05",
  "P06 — Definition Dependency" = "P06",
  "P07 — Role Scope" = "P07",
  "P08 — Scope Restriction" = "P08",
  "P09 — Multi-Clause Evidence" = "P09",
  "P10 — Absence / Not-Mentioned Trap" = "P10"
)

EVIDENCE_STRUCTURE_CHOICES <- c("None", "Single-span", "Multi-span", "Unclear")
DIVERGENCE_CHOICES <- c("No divergence", "Possible divergence", "Needs review", "Not applicable")
CONFIDENCE_CHOICES <- c("High", "Medium", "Low")

REQUIRED_MAIN_COLS <- c(
  "Doc_ID", "Hypothesis_ID", "Gold_Label", "Evidence_Text",
  "Primary_Pattern", "Secondary_Patterns", "Cue_Words",
  "Evidence_Structure", "Divergence_Flag", "Confidence",
  "Complexity_Score", "Notes"
)

# BASIC HELPERS

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) > 1) return(x)
  if (is.na(x) || identical(x, "")) return(y)
  x
}

clean_scalar <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("")
  as.character(x[1])
}

is_blank <- function(x) {
  x <- clean_scalar(x)
  !nzchar(trimws(x)) || trimws(tolower(x)) %in% c("na", "nan", "null")
}

safe_read_csv <- function(path) {
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    na = character()
  ) |>
    mutate(across(everything(), ~ifelse(is.na(.x), "", as.character(.x))))
}

ensure_cols <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) df[[col]] <- ""
  }
  df
}

is_annotated_vector <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  nzchar(trimws(x)) & !(tolower(trimws(x)) %in% c("na", "nan", "null"))
}

# DATA HELPERS

load_main_data <- function() {
  local_data <- safe_read_csv(DATA_PATH)
  local_data <- ensure_cols(local_data, REQUIRED_MAIN_COLS)
  local_data[, REQUIRED_MAIN_COLS]
}

write_local_data <- function(df) {
  df <- ensure_cols(df, REQUIRED_MAIN_COLS)
  write_csv(df[, REQUIRED_MAIN_COLS], DATA_PATH, na = "")
}

save_data <- function(df, row_index) {
  df <- ensure_cols(df, REQUIRED_MAIN_COLS)
  df <- df[, REQUIRED_MAIN_COLS]
  write_local_data(df)
  TRUE
}

# ANNOTATION HELPERS

split_secondary <- function(x) {
  x <- clean_scalar(x)
  x <- gsub(";", ",", x)
  vals <- unlist(strsplit(x, ",", fixed = TRUE))
  vals <- trimws(vals)
  vals <- vals[nzchar(vals)]
  unique(vals)
}

collapse_secondary <- function(x) {
  x <- x[nzchar(x)]
  paste(unique(x), collapse = ", ")
}

selected_patterns <- function(primary, secondary) {
  unique(c(primary, secondary[nzchar(secondary)]))
}

parse_cued_patterns <- function(cue_words) {
  cue_words <- clean_scalar(cue_words)
  m <- gregexpr("P[0-9]{2}(?=\\s*=)", cue_words, perl = TRUE)
  hits <- regmatches(cue_words, m)[[1]]
  if (length(hits) == 1 && hits[1] == "-1") return(character())
  unique(hits)
}

calculate_complexity <- function(primary, secondary) {
  pats <- selected_patterns(primary, secondary)
  length(pats[nzchar(pats)])
}

default_evidence_structure <- function(gold_label, evidence_text) {
  if (identical(clean_scalar(gold_label), "NotMentioned")) return("None")
  if (is_blank(evidence_text) || clean_scalar(evidence_text) == "N/A") return("None")
  if (grepl("\\n---\\n|---", clean_scalar(evidence_text))) return("Multi-span")
  "Single-span"
}

html_escape_br <- function(x) {
  x <- htmlEscape(clean_scalar(x))
  x <- gsub("\n", "<br/>", x, fixed = TRUE)
  x
}

find_spans <- function(text, evidence) {
  evidence <- clean_scalar(evidence)
  text <- clean_scalar(text)

  if (!nzchar(evidence) || evidence == "N/A" || !nzchar(text)) {
    return(data.frame(start = integer(), end = integer()))
  }

  chunks <- unlist(strsplit(evidence, "\\n---\\n|\\r\\n---\\r\\n|---"))
  chunks <- trimws(chunks)
  chunks <- chunks[nzchar(chunks) & chunks != "N/A"]

  out <- data.frame(start = integer(), end = integer())

  for (chunk in chunks) {
    m <- regexpr(chunk, text, fixed = TRUE)
    loc <- as.integer(m[1])
    match_len <- attr(m, "match.length")[1]

    if (!is.na(loc) && loc > 0 && !is.na(match_len) && match_len > 0) {
      out <- rbind(
        out,
        data.frame(
          start = as.integer(loc),
          end = as.integer(loc + match_len - 1)
        )
      )
    }
  }

  if (nrow(out) == 0) return(out)

  out <- out[order(out$start, out$end), ]

  merged <- data.frame(start = integer(), end = integer())
  for (i in seq_len(nrow(out))) {
    if (nrow(merged) == 0 || out$start[i] > merged$end[nrow(merged)] + 1) {
      merged <- rbind(merged, out[i, ])
    } else {
      merged$end[nrow(merged)] <- max(merged$end[nrow(merged)], out$end[i])
    }
  }

  merged
}

highlight_contract <- function(text, evidence) {
  text <- clean_scalar(text)
  spans <- find_spans(text, evidence)

  if (nrow(spans) == 0) {
    return(html_escape_br(text))
  }

  pieces <- c()
  pos <- 1

  for (i in seq_len(nrow(spans))) {
    s <- spans$start[i]
    e <- spans$end[i]

    if (s > pos) {
      pieces <- c(pieces, html_escape_br(substr(text, pos, s - 1)))
    }

    evidence_piece <- html_escape_br(substr(text, s, e))
    pieces <- c(pieces, paste0("<mark class='evidence-mark'>", evidence_piece, "</mark>"))
    pos <- e + 1
  }

  if (pos <= nchar(text)) {
    pieces <- c(pieces, html_escape_br(substr(text, pos, nchar(text))))
  }

  paste0(pieces, collapse = "")
}

highlight_count <- function(text, evidence) {
  spans <- find_spans(text, evidence)
  if (is.null(spans) || nrow(spans) == 0) return(0)
  nrow(spans)
}

validate_annotation <- function(row, primary, secondary, cue_words, evidence_structure, complexity) {
  warnings <- c()
  selected <- selected_patterns(primary, secondary)
  cued <- parse_cued_patterns(cue_words)

  if (!nzchar(primary)) warnings <- c(warnings, "Primary pattern is blank.")

  if (row$Gold_Label == "NotMentioned" && primary != "P10") {
    warnings <- c(warnings, "NotMentioned rows should normally have P10 as Primary_Pattern.")
  }

  if (row$Gold_Label == "NotMentioned" && evidence_structure != "None") {
    warnings <- c(warnings, "NotMentioned rows should use Evidence_Structure = None.")
  }

  if (evidence_structure == "Multi-span" && !"P09" %in% selected) {
    warnings <- c(warnings, "Multi-span rows must include P09.")
  }

  if (primary == "P10" && grepl("P10\\s*=", cue_words)) {
    warnings <- c(warnings, "Never write P10=... in Cue_Words.")
  }

  unexpected_cues <- setdiff(cued, selected)
  if (length(unexpected_cues) > 0) {
    warnings <- c(warnings, paste0("Cue_Words includes pattern(s) not selected: ", paste(unexpected_cues, collapse = ", ")))
  }

  needs_cue <- setdiff(selected, c("P09", "P10"))
  missing_cues <- setdiff(needs_cue, cued)
  if (length(missing_cues) > 0) {
    warnings <- c(warnings, paste0("Selected pattern(s) missing cue group: ", paste(missing_cues, collapse = ", ")))
  }

  expected_complexity <- calculate_complexity(primary, secondary)
  if (as.character(complexity) != as.character(expected_complexity)) {
    warnings <- c(warnings, "Complexity score does not match number of selected patterns.")
  }

  warnings
}

# STATIC DATA

main_data_initial <- load_main_data()

ndas <- safe_read_csv(NDAS_PATH)
ndas <- ndas |>
  rename(Doc_ID = id, File_Name = file_name, Contract_Text = text) |>
  mutate(Doc_ID = as.character(Doc_ID))

labels <- safe_read_csv(LABELS_PATH)
labels <- labels |>
  rename(Hypothesis_ID = `nda id`, Hypothesis_Text = hypothesis, Short_Description = `short description`) |>
  mutate(Hypothesis_ID = as.character(Hypothesis_ID))

# UI COMPONENTS

top_header <- div(
  class = "topbar",
  div(
    class = "brand-wrap",
    div(class = "brand", "ContractNLI Annotation Dashboard"),
  )
)

summary_strip <- card(
  class = "summary-card",
  layout_columns(
    col_widths = c(2, 2, 2, 4, 2),
    
    div(class = "mini-label", "Doc ID", div(class = "mini-value", textOutput("doc_id", inline = TRUE))),
    div(class = "mini-label", "Hypothesis ID", div(class = "mini-value", textOutput("hypothesis_id", inline = TRUE))),
    div(class = "mini-label", "Gold Label", div(class = "mini-value", uiOutput("gold_label_badge"))),
    div(class = "mini-label", "Hypothesis", div(class = "hypothesis-text", textOutput("hypothesis_text", inline = TRUE))),
    div(class = "mini-label", "Status", div(class = "mini-value", uiOutput("status_badge")))
  )
)

guide_pattern_card <- function(id, name, text) {
  div(
    class = "pattern-card",
    span(class = "pattern-id", id),
    div(class = "pattern-name", name),
    div(class = "pattern-desc", text)
  )
}

guide_page_content <- div(
  class = "nice-guide",

  div(class = "guide-section",
      h3("What you are doing"),
      p("Each row is one contract + one legal hypothesis + one gold label + evidence text."),
      div(class = "guide-callout",
          strong("Main job: "),
          "Do not relabel the dataset. Explain what kind of language makes the row difficult for an AI evidence-extraction system."
      )
  ),

  div(class = "guide-section",
      h3("10 patterns"),
      div(
        class = "pattern-grid",
        guide_pattern_card("P01", "Deontic Force", "Obligation, permission, prohibition, or denial of right."),
        guide_pattern_card("P02", "Negation", "Negative wording reverses or limits meaning."),
        guide_pattern_card("P03", "Exception / Carve-Out", "A rule is narrowed by except/unless/provided that language."),
        guide_pattern_card("P04", "Conditional Trigger", "A rule switches on when an event happens."),
        guide_pattern_card("P05", "Temporal Dependency", "Timing, duration, termination, or survival matters."),
        guide_pattern_card("P06", "Definition Dependency", "You must check a defined term."),
        guide_pattern_card("P07", "Role Scope", "The actor or recipient category matters."),
        guide_pattern_card("P08", "Scope Restriction", "Small words like all, any, only, solely change breadth."),
        guide_pattern_card("P09", "Multi-Clause Evidence", "The answer needs more than one span or clause."),
        guide_pattern_card("P10", "Absence / Not-Mentioned Trap", "The contract is silent or only indirectly related.")
      )
  ),

  div(class = "guide-section",
      h3("Cue words format"),
      p("Cue words must be exact phrases from Evidence_Text, except special NotMentioned trap cases where cues may come from Contract_Text."),
      pre(class = "code-example", "P03=except|provided that; P02=shall not; P07=Representatives"),
      tags$ul(
        tags$li("Use = to bind pattern to cues."),
        tags$li("Use | between multiple cues for one pattern."),
        tags$li("Use ; between pattern groups."),
        tags$li("Every selected pattern needs a cue except P09 and P10."),
        tags$li("Never write P10=... .")
      )
  ),

  div(class = "guide-section",
      h3("Tricky decisions"),
      div(class = "rule-list",
          div(class = "rule-item", strong("P02 vs P03: "), "shall not disclose except... usually means P03 primary and P02 secondary."),
          div(class = "rule-item", strong("P04 vs P05: "), "if/upon request is a trigger; termination/survival/deadline is temporal."),
          div(class = "rule-item", strong("P06 vs P07: "), "defined term = P06; actor/recipient category = P07."),
          div(class = "rule-item", strong("P08: "), "usually secondary; use when breadth/narrowness affects the answer."),
          div(class = "rule-item", strong("P09: "), "required whenever Evidence_Structure is Multi-span."),
          div(class = "rule-item", strong("P10: "), "primary for NotMentioned rows.")
      )
  ),

  div(class = "guide-section",
      h3("Before saving"),
      tags$ul(
        tags$li("Primary pattern is filled."),
        tags$li("Evidence_Structure spelling is exact."),
        tags$li("Multi-span includes P09."),
        tags$li("Complexity score equals number of selected patterns."),
        tags$li("Notes explain why the row is difficult, not just what the clause is about.")
      )
  )
)

annotation_page <- nav_panel(
  "Annotate",
  div(class = "page-shell",
      top_header,
      uiOutput("annotation_progress_box"),
      summary_strip,

      layout_columns(
        col_widths = c(7, 5),

        card(
          class = "main-card document-card",
          card_header(
            div(class = "card-title-row",
                span("NDA text ", span(class = "subtle", "evidence highlighted")),
                uiOutput("highlight_status")
            )
          ),
          div(class = "contract-viewer", htmlOutput("contract_html")),
          div(class = "legend", span(class = "legend-box"), "Evidence highlight")
        ),

        card(
          class = "main-card annotation-card",
          card_header("Annotation"),

          layout_columns(
            col_widths = c(6, 6),
            selectInput("primary", "Primary Pattern", choices = c("Select..." = "", PATTERN_CHOICES), selected = ""),
            selectInput("secondary", "Secondary Patterns", choices = PATTERN_CHOICES, selected = NULL, multiple = TRUE)
          ),

          textAreaInput("cue_words", "Cue_Words", value = "", rows = 3, placeholder = "P03=except|provided that; P02=shall not"),

          layout_columns(
            col_widths = c(6, 6),
            selectInput("evidence_structure", "Evidence Structure", choices = EVIDENCE_STRUCTURE_CHOICES),
            selectInput("divergence", "Divergence Flag", choices = DIVERGENCE_CHOICES)
          ),

          layout_columns(
            col_widths = c(6, 6),
            selectInput("confidence", "Confidence", choices = CONFIDENCE_CHOICES),
            textInput("complexity", "Complexity Score", value = "0")
          ),

          textAreaInput("notes", "Notes", value = "", rows = 4, placeholder = "Explain why this row is difficult for an AI system."),

          uiOutput("validation_box")
        )
      ),

      card(
        class = "bottom-card sticky-actions",
        layout_columns(
          col_widths = c(2, 2, 2, 2, 2, 2),
          actionButton("prev_row", "← Previous", class = "btn-quiet"),
          actionButton("next_row", "Next →", class = "btn-quiet"),
          div(class = "goto-row", numericInput("goto_row", "Go to row", value = 1, min = 1, max = nrow(main_data_initial), step = 1)),
          downloadButton("download_csv", "Download CSV", class = "btn-soft"),
          actionButton("save", "Save", class = "btn-soft"),
          actionButton("save_next", "Save & Next →", class = "btn-primary")
        )
      )
  )
)

guide_page <- nav_panel(
  "Guide",
  div(class = "page-shell",
      top_header,
      card(
        class = "guide-card",
        card_header("Quick Annotation Guide"),
        guide_page_content
      )
  )
)

progress_page <- nav_panel(
  "Progress",
  div(class = "page-shell",
      top_header,

      layout_columns(
        col_widths = c(4, 4, 4),
        card(class = "metric-card", div(class = "metric-label", "Total rows"), div(class = "metric-value", textOutput("metric_total", inline = TRUE))),
        card(class = "metric-card", div(class = "metric-label", "Annotated rows"), div(class = "metric-value", textOutput("metric_done", inline = TRUE))),
        card(class = "metric-card", div(class = "metric-label", "Pending rows"), div(class = "metric-value", textOutput("metric_pending", inline = TRUE)))
      ),

      card(
        class = "progress-card",
        card_header("Progress by status"),
        div(class = "progress-table", tableOutput("progress_table"))
      ),

      card(
        class = "progress-card",
        card_header("Export"),
        p("Download the current annotation CSV after each session."),
        downloadButton("download_csv_progress", "Download current CSV", class = "btn-primary")
      )
  )
)

ui <- page_navbar(
  title = "KAIOPTIX AI Evidence Extraction Taxonomy",
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    primary = "#8EC4FF",
    base_font = font_google("Poppins")
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "theme.css"),
    tags$style(HTML("
                    .navbar-brand {
                      color: #7A7A7A !important;
                      font-weight: 1000 !important;
                    }
                    "))
  ),
  annotation_page,
  guide_page,
  progress_page
)

# SERVER

server <- function(input, output, session) {
  annotation_data <- reactiveVal(main_data_initial)

  start_index <- {
    pending <- which(!is_annotated_vector(main_data_initial$Primary_Pattern))
    if (length(pending) > 0) pending[1] else 1
  }
  current_index <- reactiveVal(start_index)

  display_data <- reactive({
    df <- annotation_data()
    df <- df |>
      mutate(Doc_ID = as.character(Doc_ID), Hypothesis_ID = as.character(Hypothesis_ID)) |>
      left_join(ndas, by = "Doc_ID") |>
      left_join(labels, by = "Hypothesis_ID")
    df
  })

  current_row <- reactive({
    display_data()[current_index(), , drop = FALSE]
  })

  observe({
    updateNumericInput(session, "goto_row", value = current_index(), min = 1, max = nrow(annotation_data()))
  })

  output$top_progress_text <- renderText({
    df <- annotation_data()
    done <- sum(is_annotated_vector(df$Primary_Pattern))
    total <- nrow(df)
    pending <- total - done
    
    paste0(done, " / ", total, " annotated · ", pending, " pending")
  })

  output$doc_id <- renderText(current_row()$Doc_ID)
  output$hypothesis_id <- renderText(current_row()$Hypothesis_ID)

  output$hypothesis_text <- renderText({
    row <- current_row()
    ht <- clean_scalar(row$Hypothesis_Text)
    sd <- clean_scalar(row$Short_Description)

    if (nzchar(sd) && nzchar(ht)) {
      paste0(toupper(sd), " — ", toupper(ht))
    } else if (nzchar(ht)) {
      toupper(ht)
    } else {
      "Hypothesis text not found."
    }
  })

  output$annotation_progress_box <- renderUI({
    df <- annotation_data()
    done <- sum(is_annotated_vector(df$Primary_Pattern))
    total <- nrow(df)
    pending <- total - done
    pct <- round((done / total) * 100)
    
    div(
      class = "annotation-progress-box",
      div(
        class = "annotation-progress-row",
        span("Annotation progress"),
        span(paste0(done, " / ", total, " annotated · ", pending, " pending"))
      ),
      div(
        class = "annotation-progress-track",
        div(
          class = "annotation-progress-fill",
          style = paste0("width: ", pct, "%;")
        )
      )
    )
  })
  
  output$gold_label_badge <- renderUI({
    label <- clean_scalar(current_row()$Gold_Label)
    cls <- switch(label,
                  "Entailment" = "badge label-entailment",
                  "Contradiction" = "badge label-contradiction",
                  "NotMentioned" = "badge label-notmentioned",
                  "badge label-neutral")
    span(class = cls, toupper(label))
  })

  output$status_badge <- renderUI({
    row <- current_row()
    if (is_annotated_vector(row$Primary_Pattern)) {
      span(class = "badge status-done", "ANNOTATED")
    } else {
      span(class = "badge status-pending", "PENDING")
    }
  })

  output$highlight_status <- renderUI({
    row <- current_row()
    n <- highlight_count(row$Contract_Text, row$Evidence_Text)
    if (row$Gold_Label == "NotMentioned" || is_blank(row$Evidence_Text) || clean_scalar(row$Evidence_Text) == "N/A") {
      span(class = "highlight-meta", "No evidence span")
    } else if (n > 0) {
      span(class = "highlight-meta good", paste(n, "span(s) found"))
    } else {
      span(class = "highlight-meta warn", "Evidence not found in text")
    }
  })

  output$contract_html <- renderUI({
    row <- current_row()
    HTML(highlight_contract(row$Contract_Text, row$Evidence_Text))
  })

  observeEvent(current_index(), {
    row <- current_row()

    updateSelectInput(session, "primary", selected = clean_scalar(row$Primary_Pattern))
    updateSelectInput(session, "secondary", selected = split_secondary(row$Secondary_Patterns))
    updateTextAreaInput(session, "cue_words", value = clean_scalar(row$Cue_Words))

    es <- clean_scalar(row$Evidence_Structure)
    if (!nzchar(es)) es <- default_evidence_structure(row$Gold_Label, row$Evidence_Text)
    updateSelectInput(session, "evidence_structure", selected = es)

    div_flag <- clean_scalar(row$Divergence_Flag)
    if (!nzchar(div_flag)) div_flag <- "No divergence"
    updateSelectInput(session, "divergence", selected = div_flag)

    conf <- clean_scalar(row$Confidence)
    if (!nzchar(conf)) conf <- "High"
    updateSelectInput(session, "confidence", selected = conf)

    selected_complexity <- clean_scalar(row$Complexity_Score)
    if (!nzchar(selected_complexity)) {
      selected_complexity <- calculate_complexity(clean_scalar(row$Primary_Pattern), split_secondary(row$Secondary_Patterns))
    }
    updateTextInput(session, "complexity", value = selected_complexity)

    updateTextAreaInput(session, "notes", value = clean_scalar(row$Notes))
  }, ignoreInit = FALSE)

  observe({
    comp <- calculate_complexity(input$primary %||% "", input$secondary %||% character())
    updateTextInput(session, "complexity", value = as.character(comp))
  })

  validation_warnings <- reactive({
    validate_annotation(
      row = current_row(),
      primary = input$primary %||% "",
      secondary = input$secondary %||% character(),
      cue_words = input$cue_words %||% "",
      evidence_structure = input$evidence_structure %||% "",
      complexity = input$complexity %||% ""
    )
  })

  output$validation_box <- renderUI({
    warns <- validation_warnings()
    if (length(warns) == 0) {
      div(class = "validation validation-ok", "✓ No validation warnings")
    } else {
      div(
        class = "validation validation-warn",
        strong("Validation warnings"),
        tags$ul(lapply(warns, tags$li))
      )
    }
  })

  save_current <- function(move_next = FALSE) {
    idx <- current_index()
    df <- annotation_data()

    df$Primary_Pattern[idx] <- input$primary %||% ""
    df$Secondary_Patterns[idx] <- collapse_secondary(input$secondary %||% character())
    df$Cue_Words[idx] <- input$cue_words %||% ""
    df$Evidence_Structure[idx] <- input$evidence_structure %||% ""
    df$Divergence_Flag[idx] <- input$divergence %||% ""
    df$Confidence[idx] <- input$confidence %||% ""
    df$Complexity_Score[idx] <- input$complexity %||% ""
    df$Notes[idx] <- input$notes %||% ""

    annotation_data(df)

    ok <- save_data(df, idx)

    showNotification(
      "Saved locally to data/ToBeAnnotated.csv.",
      type = if (ok) "message" else "warning"
    )

    if (move_next && current_index() < nrow(df)) {
      current_index(current_index() + 1)
    }
  }

  observeEvent(input$save, {
    save_current(move_next = FALSE)
  })

  observeEvent(input$save_next, {
    save_current(move_next = TRUE)
  })

  observeEvent(input$prev_row, {
    if (current_index() > 1) current_index(current_index() - 1)
  })

  observeEvent(input$next_row, {
    if (current_index() < nrow(annotation_data())) current_index(current_index() + 1)
  })

  observeEvent(input$goto_row, {
    val <- as.integer(input$goto_row)
    if (!is.na(val) && val >= 1 && val <= nrow(annotation_data())) {
      current_index(val)
    }
  }, ignoreInit = TRUE)

  output$metric_total <- renderText(nrow(annotation_data()))
  output$metric_done <- renderText(sum(is_annotated_vector(annotation_data()$Primary_Pattern)))
  output$metric_pending <- renderText(nrow(annotation_data()) - sum(is_annotated_vector(annotation_data()$Primary_Pattern)))

  output$progress_table <- renderTable({
    df <- annotation_data()
    done <- sum(is_annotated_vector(df$Primary_Pattern))
    pending <- nrow(df) - done
    possible <- sum(trimws(df$Divergence_Flag) == "Possible divergence", na.rm = TRUE)

    data.frame(
      Status = c("Annotated", "Pending", "Possible divergence"),
      Rows = c(done, pending, possible),
      check.names = FALSE
    )
  }, striped = FALSE, bordered = FALSE, spacing = "m")

  output$download_csv <- downloadHandler(
    filename = function() paste0("contractnli_annotations_", Sys.Date(), ".csv"),
    content = function(file) write_csv(annotation_data(), file, na = "")
  )

  output$download_csv_progress <- downloadHandler(
    filename = function() paste0("contractnli_annotations_", Sys.Date(), ".csv"),
    content = function(file) write_csv(annotation_data(), file, na = "")
  )
}

shinyApp(ui, server)
