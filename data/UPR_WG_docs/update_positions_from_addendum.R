# Update the `position` column of an already-extracted UPR recs .rds file
# using a state's Add.1 addendum.
#
# Source this AFTER extract_recs_function.R (v5) -- it reuses .resolve_input(),
# .POSITION_LEVELS, and the tidyverse libraries that file already loads.
#
#   update_positions_from_addendum(
#     addendum_input,       # docx / pdf path, or a docs.un.org URL
#     state_under_review,   # must match the saved "<state>_<session>.rds" name
#     upr_session,
#     rds_dir = here("data", "UPR_WG_docs", "extracted_recs")
#   )
#
# Four addendum shapes are auto-detected, in this priority order:
#   1. docx with a table containing a "N/Number/Para" column and a
#      "Position/Status" column (e.g. Georgia's annex docx). Cells in the
#      label column may be a single "N.M" or a "; "/", "-separated group.
#   2. PDF where most position lines are a single bare "N.M  <Position>"
#      pair on one line (e.g. Georgia's Add.1 PDF, Australia's per-theme
#      tables).
#   3. PDF built from bold section headers ("X supports/accepts the
#      following recommendations" / "X notes the following
#      recommendations") followed by comma-lists and en-dash ranges of
#      labels (e.g. Austria) -- position comes from which header a label
#      group sits under, not from a table cell.
#   4. PDF table where a single row lists several "N.M; N.M; N.M" labels
#      together with one Position word and a justification paragraph
#      (e.g. Mauritania) -- the fallback if nothing above matches.
#
# NOTE: paths 3 and 4 are a first pass. They were written against the raw
# text you pasted into the conversation, not against pdftools output from
# the actual files -- expect to need a debugging round similar to the
# footnote issue in the recs extractor before they're reliable. Paths 1
# and 2 are the safer ones to test first.

suppressPackageStartupMessages({
  library(xml2)
  library(stringr)
  library(tibble)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(here)
})

# ---- Shared: expand a label expression into individual "sec.num" labels ----
# "50.5-50.8"            -> "50.5" "50.6" "50.7" "50.8"
# "130.1; 130.2; 130.3"  -> "130.1" "130.2" "130.3"
# "50.23, 25, 26"        -> "50.23" "50.25" "50.26"  (bare numbers inherit
#                           the section prefix of the group's first label)

expand_para_labels <- function(x) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  x <- str_replace_all(x, "[\u2013\u2012\u2010]", "-")   # en/figure dash -> ascii hyphen
  parts <- str_split(x, "[,;]")[[1]] |> str_trim()
  parts <- parts[nzchar(parts)]
  
  section_prefix <- NA_character_
  out <- character(0)
  for (p in parts) {
    m_range <- str_match(p, "^(\\d+)\\.(\\d+)\\s*-\\s*(?:(\\d+)\\.)?(\\d+)$")
    m_full  <- str_match(p, "^(\\d+)\\.(\\d+)$")
    m_bare  <- str_match(p, "^(\\d+)$")
    
    if (!is.na(m_range[1, 1])) {
      sec  <- m_range[1, 2]
      from <- as.integer(m_range[1, 3])
      to   <- as.integer(m_range[1, 5])
      section_prefix <- sec
      out <- c(out, sprintf("%s.%d", sec, from:to))
    } else if (!is.na(m_full[1, 1])) {
      section_prefix <- m_full[1, 2]
      out <- c(out, p)
    } else if (!is.na(m_bare[1, 1]) && !is.na(section_prefix)) {
      out <- c(out, sprintf("%s.%s", section_prefix, m_bare[1, 2]))
    }
    # anything else (stray text) is silently skipped
  }
  out
}

# ---- Shared: normalize whatever position wording the addendum uses --------
# to your existing four-level factor.

.normalize_addendum_position <- function(x, verbose = TRUE) {
  raw <- x
  xc  <- str_squish(x) |> str_to_lower() |> str_remove_all("[./]")
  pos <- case_when(
    str_detect(xc, "accepted") & str_detect(xc, "noted")   ~ "Supported/Noted",
    str_detect(xc, "supported") & str_detect(xc, "noted")  ~ "Supported/Noted",
    str_detect(xc, "^partially\\s*(accepted|supported)")   ~ "Supported/Noted",
    str_detect(xc, "^(accepted|accepts|supported|support|supports)\\b") ~ "Supported",
    str_detect(xc, "^(noted|notes|note)$")                  ~ "Noted",
    str_detect(xc, "consideration|pending|examin")          ~ "Under consideration",
    .default = NA_character_
  )
  unmapped <- unique(raw[is.na(pos) & !is.na(raw) & nzchar(raw)])
  if (verbose && length(unmapped) > 0L) {
    cat(sprintf("WARNING: unrecognized addendum position value(s) mapped to NA: %s\n",
                paste(sQuote(unmapped), collapse = ", ")))
  }
  pos
}

# ---- Format 1: docx table (e.g. Georgia's annex) ---------------------------

.read_addendum_docx <- function(path, verbose = TRUE) {
  doc <- read_xml(unz(path, "word/document.xml"))
  ns  <- xml_ns(doc)
  
  cell_texts <- function(row) {
    vapply(xml_find_all(row, "./w:tc", ns = ns), function(tc) {
      str_squish(paste(xml_text(xml_find_all(tc, ".//w:t", ns = ns)), collapse = " "))
    }, character(1))
  }
  
  for (tbl in xml_find_all(doc, ".//w:tbl", ns = ns)) {
    rows <- xml_find_all(tbl, "./w:tr", ns = ns)
    if (length(rows) < 2L) next
    hdr <- cell_texts(rows[[1L]])
    label_col <- which(str_detect(hdr, regex("^\\s*N\\.?\\s*$|^\\s*No\\.?\\s*$|Number|Para",
                                             ignore_case = TRUE)))[1]
    pos_col   <- which(str_detect(hdr, regex("Position|Status", ignore_case = TRUE)))[1]
    if (is.na(label_col) || is.na(pos_col)) next
    
    if (verbose) {
      cat(sprintf("Detected format: docx addendum table (%d rows)\nColumns: %s\n",
                  length(rows) - 1L, paste(hdr, collapse = " | ")))
    }
    
    body <- lapply(rows[-1L], cell_texts)
    label_raw <- vapply(body, function(r) if (length(r) >= label_col) r[label_col] else NA_character_,
                        character(1))
    pos_raw   <- vapply(body, function(r) if (length(r) >= pos_col) r[pos_col] else NA_character_,
                        character(1))
    
    out <- tibble(label_raw = label_raw, pos_raw = pos_raw) |>
      filter(!is.na(label_raw), nzchar(label_raw)) |>
      mutate(
        paragraph = map(label_raw, expand_para_labels),
        position  = .normalize_addendum_position(pos_raw, verbose = verbose)
      ) |>
      unnest(paragraph) |>
      select(paragraph, position)
    return(out)
  }
  NULL
}

# ---- Format 2: simple "label position" line pairs (PDF) --------------------
# Covers Georgia's Add.1 PDF and Australia's repeated per-theme tables:
# every position-bearing line is exactly "<sec>.<num>  <Position>" with
# nothing else on it.

.read_addendum_simple_pdf <- function(lines, verbose = TRUE) {
  m <- str_match(lines,
                 regex("^\\s*(\\d+\\.\\d+)\\s+(Supported|Support|Noted|Accepted|Accepts|Notes?|Supported/Noted|Accepted/Noted|Partially\\s+accepted|Partially\\s+supported)\\b.*$",
                       ignore_case = TRUE))
  hit <- !is.na(m[, 1])
  if (sum(hit) < 5L) return(NULL)   # not enough hits to trust this format
  
  # A handful of strict "label + position, nothing else" hits can occur
  # incidentally inside an otherwise grouped-table document (e.g. rows with
  # no justification text, like Mauritania's "130.10 Accepted"). Require the
  # strict pattern to explain most numbered lines before committing to this
  # format, not just clear a low absolute floor.
  total_label_lines <- sum(str_detect(lines, "^\\s*\\d+\\.\\d+"))
  if (sum(hit) < 0.6 * total_label_lines) return(NULL)
  
  if (verbose) cat(sprintf("Detected format: simple label/position lines (%d rows)\n", sum(hit)))
  
  labels    <- m[hit, 2]
  positions <- .normalize_addendum_position(m[hit, 3], verbose = verbose)
  
  # A single label can be followed by SEVERAL standalone position lines with
  # no label of their own (e.g. Nepal's "supports in part / notes in part"
  # section: one "Supported ..." clause, then later a "Noted ..." clause,
  # all under the same paragraph number printed only once). Detect these
  # orphan lines, attribute each to the most recent label above it, and
  # reclassify any label that ends up with more than one distinct position
  # as genuinely mixed.
  orphan_re <- regex("^\\s*(Supported|Noted|Accepted|Accepts|Notes?|Partially\\s+accepted|Partially\\s+supported)\\b.*$",
                     ignore_case = TRUE)
  is_orphan <- !hit & str_detect(lines, orphan_re)
  
  if (any(is_orphan)) {
    label_lookup <- if_else(hit, m[, 2], NA_character_)
    for (i in seq_along(label_lookup)[-1]) {
      if (is.na(label_lookup[i])) label_lookup[i] <- label_lookup[i - 1L]
    }
    
    extra <- tibble(
      paragraph = label_lookup[is_orphan],
      position  = .normalize_addendum_position(str_extract(lines[is_orphan], orphan_re), verbose = FALSE)
    ) |>
      filter(!is.na(paragraph))
    
    mixed_labels <- bind_rows(tibble(paragraph = labels, position = positions), extra) |>
      group_by(paragraph) |>
      summarise(n_pos = n_distinct(position), .groups = "drop") |>
      filter(n_pos > 1L) |>
      pull(paragraph)
    
    if (length(mixed_labels) > 0L) {
      if (verbose) {
        cat(sprintf("Detected %d label(s) with mixed sub-positions (supported in part / noted in part): %s\n",
                    length(mixed_labels), paste(mixed_labels, collapse = ", ")))
      }
      positions[labels %in% mixed_labels] <- "Supported/Noted"
    }
  }
  
  tibble(paragraph = labels, position = positions) |> distinct(paragraph, .keep_all = TRUE)
}

# ---- Format 3: prose sections under bold position headers (e.g. Austria) --
# Labels appear as comma-lists / en-dash ranges at the start of a paragraph;
# the paragraph's position comes from the nearest preceding header line
# ("X supports/accepts the following recommendations" -> Supported,
#  "X notes the following recommendations" -> Noted), not from a table cell.

.read_addendum_prose_pdf <- function(lines, verbose = TRUE) {
  # Trust the document's own structure: find the header lines and treat
  # everything between one header and the next as that header's fixed
  # position. No per-paragraph inference, no state propagation.
  header_re <- regex(
    "(?:supports?|accepts?)\\s+the\\s+following\\s+recommendations|notes?\\s+the\\s+following\\s+recommendations",
    ignore_case = TRUE)
  is_header_line <- str_detect(lines, header_re)
  if (!any(is_header_line)) return(NULL)
  
  header_idx <- which(is_header_line)
  header_pos <- if_else(str_detect(lines[header_idx], regex("notes?", ignore_case = TRUE)),
                        "Noted", "Supported")
  
  if (verbose) {
    cat(sprintf("Detected format: prose sections under position headers (%d section(s): %s)\n",
                length(header_idx), paste(header_pos, collapse = ", ")))
  }
  
  out_list <- vector("list", length(header_idx))
  for (h in seq_along(header_idx)) {
    start <- header_idx[h] + 1L
    end   <- if (h < length(header_idx)) header_idx[h + 1L] - 1L else length(lines)
    if (end < start) next
    chunk_lines <- lines[start:end]
    
    starts <- str_detect(chunk_lines, "^\\s*\\d+\\.\\d+")
    if (!any(starts)) next
    starts[1] <- TRUE
    para_id <- cumsum(starts)
    chunk_paras <- vapply(split(chunk_lines, para_id),
                          function(x) str_squish(paste(x, collapse = " ")), character(1))
    chunk_paras <- str_replace_all(chunk_paras, "[\u2013\u2012\u2010]", "-")
    
    # A cross-reference like "...recommendation No. 50.271 and below on No.
    # 50.109." can wrap so that "50.109." lands on its own line and gets
    # mistaken for a new entry. Fold a paragraph that is JUST a bare label
    # back into the previous one if that previous paragraph ends in "No." --
    # a genuine standalone entry (e.g. "50.237" with no separate comment) is
    # never preceded by a paragraph ending in "No.", so this shouldn't merge
    # real entries together.
    i <- 2L
    while (i <= length(chunk_paras)) {
      is_bare <- str_detect(chunk_paras[i], "^\\d+(\\.\\d+)?\\.?\\s*$")
      prev_no <- str_detect(chunk_paras[i - 1L], regex("\\bNo\\.\\s*$", ignore_case = TRUE))
      if (is_bare && prev_no) {
        chunk_paras[i - 1L] <- str_squish(paste(chunk_paras[i - 1L], chunk_paras[i]))
        chunk_paras <- chunk_paras[-i]
      } else {
        i <- i + 1L
      }
    }
    
    label_expr <- str_match(chunk_paras,
                            "^\\s*((?:\\d+\\.\\d+)(?:\\s*\\.?\\s*[-,]\\s*\\d+(?:\\.\\d+)?)*)")[, 2]
    # Drop a stray "." that sometimes sits right before the dash (e.g.
    # Austria's "50.28.-50.30") so expand_para_labels()'s range regex still
    # matches cleanly.
    label_expr <- str_replace_all(label_expr, "\\.\\s*-", "-")
    
    out_list[[h]] <- tibble(label_expr = label_expr, position = header_pos[h]) |>
      filter(!is.na(label_expr)) |>
      mutate(paragraph = map(label_expr, expand_para_labels)) |>
      unnest(paragraph) |>
      select(paragraph, position)
  }
  bind_rows(out_list)
}

# ---- Format: flowing prose list under a heading (e.g. Sao Tome and Principe)
# No table, no per-line labels at all -- just a heading like "List of
# accepted recommendations (113 in total)" followed by ordinary prose
# containing a long comma-separated run of labels ("92.2, 92.3, ... and
# 92.190."). Some countries only enumerate ONE category this way and
# describe the other only in prose (e.g. "80 further recommendations were
# noted") without ever listing which ones -- this parser only returns what
# it can actually see; update_positions_from_addendum()'s fill_remaining_as
# argument is the explicit, opt-in way to fill the rest once you've
# confirmed the document's own numbers add up.

.read_addendum_list_pdf <- function(lines, verbose = TRUE) {
  heading_re <- regex("list\\s+of\\s+(accepted|supported|noted)\\s+recommendations",
                      ignore_case = TRUE)
  heading_idx <- which(str_detect(lines, heading_re))
  if (length(heading_idx) == 0L) return(NULL)
  
  heading_pos <- if_else(str_detect(lines[heading_idx], regex("noted", ignore_case = TRUE)),
                         "Noted", "Supported")
  
  out_list <- vector("list", length(heading_idx))
  for (h in seq_along(heading_idx)) {
    start <- heading_idx[h] + 1L
    if (start > length(lines)) next
    # Stop at the next top-level numbered paragraph AFTER the heading's own
    # intro sentence -- skip the first marker (which introduces the list
    # itself, e.g. "9. ... are as follows:") and stop at the second (the
    # paragraph that follows the list, e.g. "10. Lastly, ...").
    rel_markers <- which(str_detect(lines[start:length(lines)], "^\\s*\\d+\\.\\s+[A-Z]"))
    end <- if (length(rel_markers) >= 2L) start + rel_markers[2] - 2L else length(lines)
    if (end < start) next
    
    block_text <- str_squish(paste(lines[start:end], collapse = " "))
    labels <- str_extract_all(block_text, "\\d+\\.\\d+")[[1]]
    if (length(labels) < 5L) next
    
    out_list[[h]] <- tibble(paragraph = unique(labels), position = heading_pos[h])
  }
  
  out <- bind_rows(out_list)
  if (nrow(out) == 0L) return(NULL)
  
  if (verbose) {
    cat(sprintf("Detected format: flowing list under heading(s) (%d label(s) across %d section(s))\n",
                nrow(out), length(heading_idx)))
  }
  out
}

# ---- Format 4: grouped-label table with justification (e.g. Mauritania) ---
# Fallback: a "row" starts at a line beginning with a label or label-list;
# the row's Position is whichever recognized keyword appears first in the
# reassembled paragraph, before the justification prose.

.read_addendum_grouped_pdf <- function(lines, verbose = TRUE) {
  # Row boundaries are signaled by the Position column, not the label
  # column: every real row shows "Accepted"/"Noted" exactly once, on its
  # first line. Label-wrap continuation lines (e.g. Mauritania's 3-line
  # label lists) and justification-continuation lines both lack a position
  # keyword -- so "does this line contain a position keyword" is a reliable
  # row-start signal, found anywhere in the line rather than in a fixed
  # column slice (a fixed-width column slice breaks on wide label lists
  # like "130.19; 130.20; 130.21;", which extend further right than a
  # short single-label row and get truncated mid-token).
  pos_re  <- regex("Accepted/Noted|Supported/Noted|Accepted|Support(?:ed)?|Noted|Under consideration",
                   ignore_case = TRUE)
  has_pos <- str_detect(lines, pos_re)
  if (sum(has_pos) < 5L) return(NULL)
  
  # Label fragment: match the label-list pattern directly from the start of
  # the line, regardless of how wide it is -- no column-width assumption.
  label_frag <- str_match(lines,
                          "^\\s*((?:\\d+\\.\\d+)(?:\\s*[;,]\\s*\\d+\\.\\d+)*\\s*;?)")[, 2]
  label_frag <- if_else(is.na(label_frag), "", str_trim(label_frag))
  
  row_id <- cumsum(has_pos)
  row_id[row_id == 0L] <- NA_integer_   # lines before the first real row (page furniture, intro text)
  
  df <- tibble(row_id, label_frag,
               pos_raw = if_else(has_pos, str_extract(lines, pos_re), NA_character_)) |>
    filter(!is.na(row_id))
  
  out <- df |>
    group_by(row_id) |>
    summarise(
      label_expr = str_squish(paste(label_frag[nzchar(label_frag)], collapse = " ")),
      pos_raw    = pos_raw[!is.na(pos_raw)][1],
      .groups = "drop"
    ) |>
    filter(nzchar(label_expr))   # spurious "rows" from a position keyword inside justification prose
  
  if (verbose) {
    cat(sprintf("Detected format: grouped-label position table (%d rows)\n", nrow(out)))
  }
  
  out |>
    mutate(
      paragraph = map(label_expr, expand_para_labels),
      position  = .normalize_addendum_position(pos_raw, verbose = verbose)
    ) |>
    unnest(paragraph) |>
    select(paragraph, position)
}

# ---- Format 5: sequential bare-index table (e.g. Lebanon) ------------------
# No "N.M" label is printed anywhere -- just a bare row number, a position
# word, and a Remarks column: "25  PARTIALLY SUPPORTED  TO NOTE: ...". Row N
# corresponds to recommendation "<section_prefix>.N" purely by position in
# sequence, so the caller must supply section_prefix (inferred from the
# already-extracted recs' own paragraph labels).

.read_addendum_sequential_pdf <- function(lines, section_prefix, verbose = TRUE) {
  starts <- str_detect(lines, regex("^\\s*\\d+\\s+(?:NOTED|SUPPORTED|PARTIALLY)\\b",
                                    ignore_case = TRUE))
  if (!any(starts)) return(NULL)
  
  first <- which(starts)[1]
  idx    <- first:length(lines)   # compute once, before either vector is reassigned
  lines  <- lines[idx]
  starts <- starts[idx]
  starts[1] <- TRUE
  para_id <- cumsum(starts)
  paras <- vapply(split(lines, para_id), function(x) str_squish(paste(x, collapse = " ")),
                  character(1))
  
  m <- str_match(paras, regex("^\\s*(\\d+)\\s+(NOTED|SUPPORTED|PARTIALLY\\s+SUPPORTED)\\b",
                              ignore_case = TRUE))
  keep <- !is.na(m[, 1])
  m <- m[keep, , drop = FALSE]
  
  if (nrow(m) < 5L) return(NULL)   # not enough hits to trust this format
  if (verbose) {
    cat(sprintf("Detected format: sequential bare-index table (%d rows, section prefix '%s')\n",
                nrow(m), section_prefix))
  }
  
  position <- case_when(
    str_detect(m[, 3], regex("PARTIALLY", ignore_case = TRUE))        ~ "Supported/Noted",
    str_detect(m[, 3], regex("^SUPPORTED$", ignore_case = TRUE))      ~ "Supported",
    str_detect(m[, 3], regex("^NOTED$", ignore_case = TRUE))          ~ "Noted",
    .default = NA_character_
  )
  
  tibble(
    paragraph = sprintf("%s.%s", section_prefix, m[, 2]),
    position  = position
  )
}

# ---- PDF dispatcher ---------------------------------------------------------

.read_addendum_pdf <- function(path, section_prefix = NULL, verbose = TRUE) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Reading PDFs requires the 'pdftools' package.")
  }
  txt   <- pdftools::pdf_text(path)
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE))
  lines <- lines[nzchar(str_trim(lines))]
  
  out <- .read_addendum_simple_pdf(lines, verbose = verbose)
  if (!is.null(out)) return(out)
  
  out <- .read_addendum_list_pdf(lines, verbose = verbose)
  if (!is.null(out)) return(out)
  
  out <- .read_addendum_prose_pdf(lines, verbose = verbose)
  if (!is.null(out)) return(out)
  
  if (!is.null(section_prefix)) {
    out <- .read_addendum_sequential_pdf(lines, section_prefix = section_prefix, verbose = verbose)
    if (!is.null(out)) return(out)
  }
  
  out <- .read_addendum_grouped_pdf(lines, verbose = verbose)
  if (!is.null(out)) return(out)
  
  stop("Could not detect a known addendum format in this PDF.")
}

# ---- User-facing wrapper ----------------------------------------------------

update_positions_from_addendum <- function(
    addendum_input,
    state_under_review,
    upr_session,
    rds_dir           = here("data", "UPR_WG_docs", "extracted_recs"),
    mark_final        = FALSE,
    fill_remaining_as = NULL,   # e.g. "Noted" -- explicitly fill any label the
    # addendum didn't cover. Use only after you've
    # confirmed the document's own prose accounts
    # for every recommendation (e.g. "113 accepted,
    # 80 further noted" summing to the total) --
    # this is never inferred automatically.
    verbose    = TRUE
) {
  rds_path <- file.path(rds_dir, paste0(state_under_review, "_", upr_session, ".rds"))
  if (!file.exists(rds_path)) {
    stop("No saved recs file found at: ", rds_path)
  }
  recs <- readRDS(rds_path)
  
  section_prefix <- names(sort(table(str_extract(recs$paragraph, "^\\d+")),
                               decreasing = TRUE))[1]
  
  src <- .resolve_input(addendum_input, verbose = verbose)
  pos_df <- if (src$type == "docx") {
    .read_addendum_docx(src$path, verbose = verbose)
  } else {
    .read_addendum_pdf(src$path, section_prefix = section_prefix, verbose = verbose)
  }
  if (is.null(pos_df) || nrow(pos_df) == 0L) {
    stop("No (paragraph, position) pairs could be extracted from this addendum.")
  }
  if (verbose) {
    conflicts <- pos_df |>
      group_by(paragraph) |>
      filter(n_distinct(position) > 1L) |>
      ungroup()
    if (nrow(conflicts) > 0L) {
      cat(sprintf(
        "\nWARNING: %d label(s) got conflicting positions from this addendum (keeping the first); check for a cross-reference line-wrap: %s\n",
        dplyr::n_distinct(conflicts$paragraph),
        paste(unique(conflicts$paragraph), collapse = ", ")))
    }
  }
  pos_df <- distinct(pos_df, paragraph, .keep_all = TRUE)
  
  updated <- recs |>
    left_join(pos_df |> rename(position_new = position), by = "paragraph") |>
    mutate(position = coalesce(position_new, position)) |>
    select(-position_new) |>
    mutate(position = factor(position, levels = .POSITION_LEVELS))
  
  if (mark_final) updated$provisional <- FALSE
  
  if (!is.null(fill_remaining_as)) {
    stopifnot(fill_remaining_as %in% .POSITION_LEVELS)
    still_na <- is.na(updated$position)
    if (any(still_na)) {
      if (verbose) {
        cat(sprintf("Filling %d remaining unmatched recommendation(s) as '%s' (explicitly requested via fill_remaining_as).\n",
                    sum(still_na), fill_remaining_as))
      }
      updated$position[still_na] <- fill_remaining_as
    }
  }
  
  n_matched <- sum(recs$paragraph %in% pos_df$paragraph)
  unmatched <- setdiff(pos_df$paragraph, recs$paragraph)
  missing   <- setdiff(recs$paragraph, pos_df$paragraph)
  
  if (verbose) {
    cat(sprintf("\nAddendum supplied positions for %d/%d saved recommendations.\n",
                n_matched, nrow(recs)))
    if (length(unmatched) > 0L) {
      cat(sprintf("WARNING: %d addendum label(s) have no match in the saved recs (typo, or a different session?): %s\n",
                  length(unmatched), paste(head(unmatched, 15), collapse = ", "),
                  if (length(unmatched) > 15L) ", ..." else ""))
    }
    if (length(missing) > 0L) {
      cat(sprintf("WARNING: %d saved recommendation(s) got no position from this addendum: %s%s\n",
                  length(missing), paste(head(missing, 15), collapse = ", "),
                  if (length(missing) > 15L) ", ..." else ""))
    }
    print(table(updated$position, useNA = "ifany"))
  }
  
  saveRDS(updated, rds_path)
  if (verbose) cat("Updated:", rds_path, "\n")
  invisible(updated)
}
update_positions_from_addendum(
  addendum_input     = "https://docs.un.org/en/A/HRC/62/16/Add.1",
  state_under_review = "Sao Tome and Principe",
  upr_session        = 51,
  mark_final         = FALSE,
  fill_remaining_as  = "Noted"
)

