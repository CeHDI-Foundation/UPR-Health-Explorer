# UPR Health Explorer
# Author: Anshu Uppal

# 1. SETUP: LOAD LIBRARIES AND DATA =========================================
if (!require("pacman")) install.packages("pacman")

# Load packages
pacman::p_load(
  shiny,
  bslib, # Modern UI for Shiny dashboard
  here,
  dplyr, forcats, ggplot2, magrittr, readr, readxl, stringr, tibble, tidyr, lubridate,
  ggnewscale,
  ggtext, # allow dynamically wrapped plot titles
  janitor,
  DT, # interactive tables
  openxlsx, # download as xlsx
  sf, # mapping features
  # necountries,
  patchwork,
  pdftools,
  tinytex,
  plotly,
  leaflet
  # quarto
)

# tinytex::install_tinytex()

## Data ---------------------------------------------------------------
source(here("code", "external_data_GBD.R"))

# Read in pre-processed datasets
sdg_data <- readRDS(here("output", "UHRI_UPR_classified.rds")) |> 
  mutate(response_upr = fct_recode(response_upr, 
                                   "Noted" = "Partially supported"),
         response_upr = fct_relevel(response_upr, "Noted")) |> 
  droplevels()
state_geo <- readRDS(here("output", "state_geo_enhanced.rds"))
nearest_neighbors_list <- readRDS(here("output", "nearest_neighbors_list.rds"))
source(here("code", "theme_labels.R"))
theme_labels <- theme_labels |> 
  filter(!variable %in% c(
    "SRHR", "health_related", "SOCED",
    "essential_medicines","TB_malaria", "NTD","vaccinations"
  ))

sdg_data <- sdg_data |> select(-any_of(c("SRHR", "SRHR_any", "Communicable_combined", "SOCED",
                                         "essential_medicines","TB_malaria", "abortion_maternal",
                                         "NTD","vaccinations")))

# Loop through API-generated files
for (file_name in list.files(path = here("data", "API_data"), pattern = "\\.rds$")) {
  object_name <- gsub("\\.rds$", "", file_name)
  assign(object_name, readRDS(here("data", "API_data", file_name)))
}

# Make a vertical line that cuts the mapping geometries, for adjusting the maps to allow for shifting the center to the Pacific region
polygon_shift <- st_polygon(x = list(rbind(
  c(-0.000001, 90),
  c(0, 90),
  c(0, -90),
  c(-0.000001, -90),
  c(-0.000001, 90)
))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# A helper function from utils.R for the app to run standalone
relabel_na <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "No data"
  factor(x)
}

map_insetting <- function(
    p1, 
    plot_dat,
    bbox_SUR_region_dynamic, bbox_sur, sur_area, 
    p_title_text="Update this text", 
    p_caption_text = "Update this text",
    title_size=14, title_margin=17,
    caption_size=14
) {
  p2 <- p1 + 
    geom_sf(
      # data=plot_dat, 
      color="grey80", fill="transparent")+
    coord_sf(
      # expand = FALSE,
      xlim = c(bbox_SUR_region_dynamic[[1]],bbox_SUR_region_dynamic[[3]]),
      ylim = c(bbox_SUR_region_dynamic[[2]], bbox_SUR_region_dynamic[[4]])
      
      # Alternative mapping centered on the selected State
      # xlim = c(max(-180, bbox_selected_SUR()[[1]] - 20), min(180, bbox_selected_SUR()[[3]] + 20)),
      # ylim = c(max(-55.67295, bbox_selected_SUR()[[2]] - 20), min(83.6341, bbox_selected_SUR()[[4]] + 20))
    )+
    geom_rect(
      aes(
        xmin = bbox_sur["xmin"] - 0.7,
        xmax = bbox_sur["xmax"] + 0.7,
        ymin = bbox_sur["ymin"] - 0.7,
        ymax = bbox_sur["ymax"] + 0.7
      ),
      fill = "transparent",
      color = "red",
      linewidth = 0.5
    )
  
  p3 <- p1 + guides(fill="none")+
    ggnewscale::new_scale_fill()+ 
    geom_sf(
      # data=plot_dat, 
      aes(geometry=polygon, fill = selected_sur),
      color = "transparent")+ 
    scale_fill_manual(values=c("transparent", "white"))+
    # scale_linewidth_manual(values = c(0.2, 0.1)) +
    coord_sf(
      expand=FALSE,
      xlim = c(bbox_sur[[1]]-.11, bbox_sur[[3]]+.11),
      ylim = c(bbox_sur[[2]]-.11, bbox_sur[[4]]+.11)
    ) +
    theme_void()+
    theme(
      panel.background = element_rect(fill="white"), 
      panel.border = element_rect(color = "red")
    )+
    geom_rect(
      aes(
        xmin = bbox_sur["xmin"]-.1,
        xmax = bbox_sur["xmax"]+.1,
        ymin = bbox_sur["ymin"]-.1,
        ymax = bbox_sur["ymax"]+.1
      ),
      fill = "transparent",
      color = "red",
      linewidth = 0.5
    )+
    guides(fill = "none") +
    labs(
      # subtitle = "(map zoom)", 
      caption = NULL)
  
  p_title <- plot_annotation(
    title = p_title_text,
    caption = p_caption_text,
    theme = theme(
      plot.background = element_rect(color = "#1c164d", fill=NA),
      plot.title = element_textbox_simple(
        size = title_size,
        margin = margin(t = title_margin, b = title_margin, r=0, l=0, unit = "pt")
      ),
      plot.caption = element_textbox_simple(
        size = caption_size, halign = 0.5,
        margin = margin(t = 10, b = 2, r=0, l=0, unit = "pt")
      )
    )
  )
  
  # Calculate Inset dimensions for lower left
  a<-ggplot_build(p2)
  b<-ggplot_build(p3)
  multiplier_a <- abs(a$layout$coord$limits$x[1]-a$layout$coord$limits$x[2])/abs(a$layout$coord$limits$y[1]-a$layout$coord$limits$y[2])
  multiplier_b <- abs(b$layout$coord$limits$x[1]-b$layout$coord$limits$x[2])/abs(b$layout$coord$limits$y[1]-b$layout$coord$limits$y[2])
  
  area_a <- abs(a$layout$coord$limits$x[1]-a$layout$coord$limits$x[2])*abs(a$layout$coord$limits$y[1]-a$layout$coord$limits$y[2])
  area_b <- abs(b$layout$coord$limits$x[1]-b$layout$coord$limits$x[2])*abs(b$layout$coord$limits$y[1]-b$layout$coord$limits$y[2])
  
  key_dim <- 0.3/(multiplier_a/multiplier_b)
  inset_dimensions <- if(multiplier_b<=3){c(0,0,key_dim,key_dim*multiplier_a/multiplier_b)}else{c(0,0,key_dim*multiplier_a/multiplier_b,0.2)}
  
  if (
    # sur_area > 11^11 | 
    area_b > 0.02*area_a
  ) {
    p2 + p_title
  } else {
    p2 + inset_element(p3, inset_dimensions[1], inset_dimensions[2],inset_dimensions[3],inset_dimensions[4]) + p_title
  }
}

leaflet_function <- function(data, pal_object, fill_outcome, hover_labels, legend_title = "Legend text", 
                             coord_selected_SUR, zoom_level, legend_type = "factor", bins_num = 10){
  leaflet(data = data) |>
    setView(lng = coord_selected_SUR[[1]][1], lat = coord_selected_SUR[[1]][2], zoom = zoom_level) %>%
    addPolygons(
      fillColor = ~pal_object(data[[fill_outcome]]),
      weight = 1,
      opacity = 1,
      color = "grey",
      dashArray = "3",
      fillOpacity = 0.9,
      # Interaction: Highlight on hover
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 1,
        bringToFront = FALSE
      ),
      # Interaction: Tooltip content
      label = hover_labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) |> 
    # Add border around the selected SUR
    addPolygons(
      data = data |> filter(selected_sur == TRUE),
      fill = FALSE,          # No fill (transparent), so the layer below shows through
      color = "red",         # Red border
      weight = 3,            # Thicker than the base layer
      opacity = 1,
      options = pathOptions(clickable = FALSE) # Make it "click-through" so hover works on layer below
    )|> 
    addLegend(
      pal = pal_object,
      values = ~data[[fill_outcome]],
      opacity = 0.7,
      bins=bins_num,
      title = legend_title,
      position = "bottomright"
    ) |> 
    htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        var lockedCenter = map.getCenter();
        map.on('zoomend', function() {
          map.panTo(lockedCenter, { animate: true });
        });
      }
    ")
}

## UPR helper functions ------------------------------------------
# Shared logic for the "UPR recommendations" pages (By Region / By State).
# Each behavior is defined once here and parameterized by the filtered dataset
# (filtered_upr_region() or filtered_upr()) and the page's title input.

# Themes excluded from the theme-level plots (most are also dropped from
# sdg_data and theme_labels at load time; "health_related" is the overall flag)
upr_excluded_themes <- c(
  "SRHR", "health_related", "SOCED",
  "essential_medicines", "TB_malaria", "NTD", "vaccinations"
)

# Count Supported/Noted recommendations per theme (optionally per cycle) and
# compute the percentages and "N (% supported)" labels used by the bar charts.
#
# One pass: turn each theme column into a logical (tagged with the theme or
# not), pivot to long, then count tagged/untagged per response (per cycle),
# per theme. Themes are ordered by total tagged count, ties broken by the
# original column order.
summarise_upr_themes <- function(data, by_cycle = FALSE) {
  # Theme columns, in document order (used to break ties in the theme ranking)
  theme_cols <- names(dplyr::select(data, health_related:other_health_related))

  prepped <- data |>
    mutate(
      response_upr = fct_relevel(response_upr, "Noted"),
      across(all_of(theme_cols), ~ .x != "Other")   # TRUE = tagged with the theme
    )

  if (by_cycle) {
    prepped    <- mutate(prepped, cycle2 = fct_recode(
      cycle, "1" = "Cycle 1", "2" = "Cycle 2", "3" = "Cycle 3", "4" = "Cycle 4"))
    id_cols    <- c("cycle", "cycle2", "response_upr")
    stat_group <- c("cycle2", "theme")
  } else {
    id_cols    <- "response_upr"
    stat_group <- "theme"
  }

  prepped |>
    select(all_of(c(id_cols, theme_cols))) |>
    pivot_longer(all_of(theme_cols), names_to = "theme", values_to = "tagged") |>
    # n = recommendations tagged with the theme; n_other = untagged
    group_by(across(all_of(c(id_cols, "theme")))) |>
    summarise(n = sum(tagged), n_other = sum(!tagged), .groups = "drop") |>
    group_by(across(all_of(stat_group))) |>
    mutate(
      n_tot       = sum(n) + sum(n_other),   # total recommendations in the group
      n_tot_theme = sum(n),                  # recommendations tagged with the theme
      perc        = n / n_tot * 100,
      perc_theme  = n_tot_theme / n_tot * 100,
      # "(% supported)" label: share of this response among recs that got a
      # response (Supported/Noted); blank when there is no response or no recs
      n_sup = paste0("(", sprintf("%1.0f",
               n / sum(n[response_upr %in% c("Supported", "Noted")]) * 100), "%)"),
      n_sup = case_when(
        response_upr == "Response not available" ~ "",
        n_tot_theme == 0 ~ "",
        n_sup == "(NaN%)" ~ "",
        .default = n_sup)
    ) |>
    ungroup() |>
    filter(!theme %in% upr_excluded_themes) |>
    left_join(theme_labels, by = c("theme" = "variable")) |>
    # keep an English and a French label; fall back to the raw variable name
    mutate(theme_label    = coalesce(theme_label, theme),
           theme_label_fr = coalesce(theme_label_fr, theme)) |>
    # Order themes by total tagged count (across cycles/responses), ties by
    # original column order, then lock that order into the label factors
    group_by(theme) |>
    mutate(order_metric = sum(n_tot_theme)) |>
    ungroup() |>
    arrange(desc(order_metric), match(theme, theme_cols)) |>
    mutate(theme_label    = fct_inorder(theme_label),
           theme_label_fr = fct_inorder(theme_label_fr))
}

# Horizontal bar chart of all themes ("Health-related Recommendations" tab);
# takes summarise_upr_themes() output plus the total N for the axis label.
# lang = "fr" produces a French version for the PNG download (French theme
# labels, legend, and x-axis); on-screen plotly stays English (lang = "en").
build_upr_themes_plot <- function(theme_summary, total_n, lang = "en") {
  fr <- lang == "fr"
  theme_summary <- theme_summary |>
    mutate(
      theme_label_disp = if (fr) theme_label_fr else theme_label,
      response_disp = if (fr) fct_recode(response_upr,
                        "Acceptées" = "Supported", "Notées" = "Noted",
                        "Réponse non disponible" = "Response not available")
                      else response_upr
    )
  x_lab <- if (fr) {
    paste0("% de toutes les recommandations\n(N total = ",
           format(total_n, big.mark = ","), ")")
  } else {
    paste0("% of all recommendations\n(Total N = ",
           format(total_n, big.mark = ","), ")")
  }
  theme_summary |>
    ggplot(aes(
      x = perc, y = fct_rev(theme_label_disp),
      customdata = paste(theme_label, response_upr, sep = "|"),
      text = paste0(response_upr, ": n = ", n, " ", n_sup,"\n(click to view text)")
    )) +
    geom_col(aes(fill = response_disp), alpha = 0.8, width = 0.85) +
    scale_fill_manual(values = c("#ec5557", "#1c164d", "grey"))+
    labs(
      x = x_lab,
      y = NULL,
      fill = NULL
    ) +
    theme_classic() +
    scale_x_continuous(
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.01))
    ) +
    coord_cartesian(clip = "off")+
    guides(fill=guide_legend(reverse=T))+
    theme(
      legend.position = c(0.9, 0.1),
      legend.justification = c("right", "bottom"),
      legend.margin = margin(0,0,0,0),
      legend.frame = element_blank(),
      legend.text = element_text(size = 9, color = "#1c164d"),
      legend.title = element_text(size = 11, color = "#1c164d"),
      legend.background = element_blank(),
      legend.key.size = unit(10,"pt"),
      axis.text.y = element_text(size = 9, color = "#1c164d"),
      axis.text.x = element_text(size = 10, color = "#1c164d", angle=30),
      plot.title = element_text(hjust = 0.5, face = "bold", color = "#1c164d"),
      axis.title.y = element_blank(),
      axis.title.x = element_text(color = "#1c164d", hjust = 0.5),
      plot.title.position = "plot",
      panel.grid = element_blank(),
      plot.caption = element_text(color = "#1c164d"),
      plot.background = element_rect(color = "#1c164d", fill = NA),
      panel.background = element_blank()
    )
}

# Per-cycle bar chart faceted by theme ("Per UPR Cycle" tab);
# takes summarise_upr_themes(by_cycle = TRUE) output
build_upr_cycle_plot <- function(theme_summary) {
  max_a <- max(theme_summary$perc_theme)
  theme_summary |>
    ggplot(aes(x = perc, y = fct_rev(cycle2),
               customdata = paste(theme_label, response_upr, cycle, sep = "|"),
               text = paste0(cycle, " - ", response_upr,  ": n = ", n, " ", n_sup,"\n(click to view text)")
    ))+
    geom_col(aes(fill = response_upr), alpha = 0.8, width = 0.95)+
    facet_grid(
      rows = vars(theme_label), switch = "y"
    )+
    labs(x = "% of all recommendations in cycle", y = NULL,
         fill = "State's response",
         caption = "*Numbers after the bars indicate N (% supported, where a response is available)")+
    theme_classic()+
    scale_fill_manual(values = c("#ec5557", "#1c164d", "grey"))+
    scale_x_continuous(labels = function(x) paste0(x, "%"),
                       limits = c(0,max_a+1),
                       expand = expansion(mult = c(0, 0.05)) # 0 exactly on axis
    )+
    guides(fill=guide_legend(reverse=T))+
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.75,0.5),
      legend.justification = c("right", "center"),
      legend.frame = element_rect(color = "black"),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      plot.title = element_text(hjust = 0.5),
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0, vjust = 0.5, color = "#1c164d"),
      panel.grid = element_blank(),
      panel.spacing = unit(0.01, "lines"),
      plot.background = element_rect(color = "#1c164d", fill = NA),
      panel.background = element_blank()
    )
}

# Top recommending states for the SRHR themes ("Recommending states" tab).
# relabel_iran and fixed_aspect preserve small differences between the
# By State page (TRUE) and the By Region page (FALSE)
build_upr_recommending_plot <- function(data, top_n = 20,
                                        relabel_iran = FALSE, fixed_aspect = FALSE) {
  upr_rec_countries <- data |>
    filter(!is.na(recommending_state_upr)) |>
    filter(response_upr == "Supported") |>
    filter(if_any(any_of(c(theme_labels$variable)), ~ .x != "Other")) |>
    separate_longer_delim(cols = c(recommending_state_upr), delim="-") |>
    mutate(recommending_state_upr = str_trim(recommending_state_upr)) |>
    group_by(cycle, recommending_state_upr) |> count(across(any_of(theme_labels$variable))) |>
    pivot_longer(cols = !c(cycle, recommending_state_upr, n),
                 names_to = "variable",
                 values_to = "theme") |>
    filter(theme!="Other") |>
    left_join(theme_labels) |>
    group_by(cycle, recommending_state_upr, variable) |>
    mutate(n=sum(n)) |>
    ungroup() |>
    distinct()

  c_plot <- upr_rec_countries |>
    filter(variable %in% c("abortion",
                           "maternal_health",
                           "contraception",
                           "sexual_health",
                           "sexual_education"
    )) |>
    select(-cycle) |>
    group_by(recommending_state_upr, theme) |>
    mutate(n=sum(n)) |>
    ungroup() |>
    distinct() |>
    arrange(theme, -n)

  if (relabel_iran) {
    c_plot <- c_plot |>
      mutate(recommending_state_upr = case_when(
        recommending_state_upr == "Iran (Islamic Republic of)" ~  "Islamic Republic of Iran",
        .default = recommending_state_upr
      ))
  }

  c_plot <- c_plot |>
    mutate(recommending_state_upr_raw = recommending_state_upr,
           recommending_state_upr = str_wrap(recommending_state_upr, 20)) |>
    group_by(recommending_state_upr) |>
    mutate(n_tot = sum(n)) |>
    ungroup() |>
    arrange(-n_tot) |>
    ungroup()

  ccp <- c_plot |> select(recommending_state_upr, n_tot) |> distinct() |>
    arrange(-n_tot) |>
    slice_head(n=top_n)

  p <- c_plot |>
    filter(recommending_state_upr %in% c(ccp |> pull(recommending_state_upr))) |>
    ggplot(aes(x= reorder(recommending_state_upr, n_tot), y=n,fill=theme_label
               ,customdata = paste(theme_label, "Supported", NA, recommending_state_upr_raw, sep = "|"),
               text = paste0(recommending_state_upr, " - ", theme_label,  ": n = ", n,"\n(click to view text)")
    ))+
    geom_col(alpha = 1, width = 0.8)+
    scale_fill_manual(values = c(
      "Maternal health" = "#8dd3c7",
      "Family planning" = "#fed9a6",
      "Abortion" = "#bebada",
      "Sexual health and wellbeing" = "#fb8072",
      "Sexual education" = "#80b1d3"
    ))+
    scale_y_continuous(expand = c(0, 0.1)) +
    tidytext::scale_x_reordered() +
    coord_flip()+
    theme_minimal() +
    guides(fill=guide_legend(reverse=TRUE))+
    theme(
      strip.text.y = element_text(angle = 270, face = "bold"),
      strip.placement = "outside",
      panel.grid.major.y = element_blank(),
      legend.position = "top",
      legend.justification = c("left", "bottom"),
      legend.margin = margin(0,0,0,0),
      legend.box.margin = margin(0,0,0,0),
      legend.box.spacing = unit(0,"pt"),
      legend.background = element_blank(),
      plot.background = element_rect(color = "#1c164d", fill = NA),
      panel.background = element_blank()
    )+
    labs(y="Supported recommendations (N)", x=NULL,
         fill=NULL)

  if (fixed_aspect) {
    p <- p + theme(aspect.ratio = 0.09*n_distinct(ccp$recommending_state_upr))
  }
  p
}

# Convert a UPR ggplot to plotly with the shared layout (transparent
# background, bottom-right reversed legend, left-aligned title).
# fix_strip_labels rotates the facet strip labels of the per-cycle plot to
# horizontal and moves them into the left margin (pass margins = list(l = 270))
upr_ggplotly <- function(p, title_text, fix_strip_labels = FALSE,
                         margins = list(l = 0, r = 40, b = 0, t = 30)) {
  fig <- ggplotly(
    p,
    tooltip = c("text"),
    source = "click"
  )

  if (fix_strip_labels) {
    # ggplotly renders side strips with an angle (usually -90 or 90);
    # catch those, make them horizontal, and move them to the left
    fig$x$layout$annotations <- lapply(fig$x$layout$annotations, function(a) {
      if (!is.null(a$textangle) && a$textangle != 0) {
        a$textangle <- 0       # Make text horizontal
        a$x <- -0.01           # Move to Left (Adjust this value based on label length)
        a$xanchor <- "right"   # Align text against the axis
        a$yanchor <- "middle"  # Center vertically
        a$align <- "center"
      }
      return(a)
    })
  }

  fig |>
    plotly::layout(
      plot_bgcolor = 'rgba(0,0,0,0)',
      legend = list(
        traceorder = "reversed",
        x = 0.99,
        y = 0.01,
        xanchor = 'right',
        yanchor = 'bottom',
        bgcolor = 'rgba(0,0,0,0)', # Transparent background
        bordercolor = 'rgba(0,0,0,0)'
      ),
      title = list(
        text = title_text,
        automargin = TRUE,
        x = 0               # Left-align the title (0 = left, 0.5 = center, 1 = right)
      ),
      xaxis = list(
        automargin = TRUE # Automatically creates space for the title
      ),
      margin = margins
    )
}

# PNG downloadHandler for the "Health-related Recommendations" plots.
# plot_reactive is the plot-object reactive; name_reactive supplies the
# selected region/state for the filename and title. lang = "fr" writes a
# French title/caption/filename (the plot object itself must be built with
# build_upr_themes_plot(..., lang = "fr")).
upr_png_download <- function(plot_reactive, name_reactive, expand_right = 0.2,
                             lang = "en") {
  fr <- lang == "fr"
  downloadHandler(
    filename = function() {
      prefix <- if (fr) "recommandations-sante-" else "health-recommendations-"
      paste0(prefix, name_reactive(), ".png")
    },
    content = function(file) {
      p <- plot_reactive()
      title_txt <- if (fr) {
        paste0("Recommandations liées à la santé de l'EPU\n", name_reactive())
      } else {
        paste0("Health-related recommendations of the UPR\n", name_reactive())
      }
      caption_txt <- if (fr) {
        "*Les nombres après les barres indiquent N (% accepté, lorsqu'une réponse est disponible)"
      } else {
        "*Numbers after the bars indicate N (% supported, where a response is available)"
      }
      ggsave(
        file,
        plot = p +
          theme(
            plot.background = element_rect(color = "#1c164d", fill = "transparent"),
            panel.background = element_rect(color = NA, fill="transparent")
          )+
          scale_x_continuous(
            expand = expansion(mult = c(0, expand_right))
          ) +
          labs(title = title_txt, caption = caption_txt)+
          geom_text(
            data = p@data |> filter(response_upr == "Supported"),
            aes(label = paste0(n_tot_theme, " ", n_sup), x = perc_theme),
            hjust = -0.05,
            size = 3, color = "#1c164d"
          )
        ,width = 7,
        height = 5,
        dpi = 400,
        units = "in"
      )
    }
  )
}

# Stacked bar chart of the (median) number of recommendations per UPR cycle,
# split into health-related vs other, used by the "Proportion Health-Related"
# tabs. by_state = FALSE is the regional plot (median across the region's
# states, a single facet named region_name); by_state = TRUE is the By-State
# plot (faceted per selected state). lang = "fr" translates the legend, title,
# and y-axis; hr_color sets the health-related fill (on-screen orange, the PNG
# downloads red). Callers add their own theme() overrides on top.
build_health_proportion_plot <- function(data, by_state = FALSE,
                                         region_name = NULL, lang = "en",
                                         hr_color = "#E69F00") {
  fr <- lang == "fr"
  hr_lab <- if (fr) "Liées à la santé" else "Health-related"
  ot_lab <- if (fr) "Autres" else "Other"

  med_group <- if (by_state) c("cycle", "state_under_review", "health_related")
               else          c("cycle", "health_related")
  tot_group <- if (by_state) c("cycle", "state_under_review") else "cycle"

  d <- data |>
    droplevels() |>
    group_by(cycle, state_under_review) |>
    count(health_related, .drop = FALSE) |>
    group_by(across(all_of(med_group))) |>
    mutate(med_n = median(n)) |>
    select(all_of(c(tot_group, "health_related", "med_n"))) |>
    distinct() |>
    group_by(across(all_of(tot_group))) |>
    mutate(
      n_tot = sum(med_n),
      perc = (med_n / n_tot) * 100,
      perc = case_when(
        health_related == "Other" ~ paste0(med_n),
        .default = paste0(med_n, " (", sprintf("%1.0f", perc), "%)")
      )
    ) |>
    ungroup() |>
    mutate(
      health_disp = fct_recode(health_related, !!hr_lab := "Health-related",
                               !!ot_lab := "Other"),
      facet_var = if (by_state) state_under_review else region_name
    )

  title_txt <- if (by_state) NULL
               else if (fr) "Nombre médian de recommandations reçues par les États"
               else "Median recommendations received by States"
  y_lab <- if (by_state) {
    if (fr) "Nombre de recommandations" else "Number of recommendations"
  } else {
    if (fr) "Nombre médian de recommandations" else "Median # of recommendations"
  }

  d |>
    ggplot(aes(x = cycle, y = med_n, fill = health_disp)) +
    scale_fill_manual(values = setNames(c(hr_color, "grey80"), c(hr_lab, ot_lab))) +
    geom_bar(stat = "identity") +
    labs(y = y_lab, x = "UPR Cycle", title = title_txt, fill = NULL) +
    geom_text(aes(label = perc), position = position_stack(vjust = 0.5),
              size = if (by_state) 5 else 4.5) +
    theme_bw() +
    facet_wrap(. ~ facet_var, nrow = if (by_state) 2 else 1)
}

# PNG downloadHandler for the "Proportion Health-Related" plots. data_reactive
# is the filtered-data reactive; name_reactive supplies the region/state for
# the filename. Builds a fresh plot with the download styling (red fill,
# framed export theme). y_lab overrides the axis label; keep_strip shows the
# facet strip (the region name, on the regional plot).
proportion_png_download <- function(data_reactive, name_reactive, by_state,
                                    lang = "en", y_lab = NULL, keep_strip = FALSE) {
  fr <- lang == "fr"
  downloadHandler(
    filename = function() {
      prefix <- if (fr) "recommandations-sante-" else "health-recommendations-"
      paste0(prefix, name_reactive(), ".png")
    },
    content = function(file) {
      p <- build_health_proportion_plot(
        data_reactive(), by_state = by_state,
        region_name = if (by_state) NULL else name_reactive(),
        lang = lang, hr_color = "#ec5557")
      if (!is.null(y_lab)) p <- p + labs(y = y_lab)
      ggsave(
        file,
        plot = p +
          theme(
            panel.grid = element_blank(),
            axis.text.x = element_text(size = 12, color = "#1c164d"),
            axis.text.y = element_text(size = 12, color = "#1c164d"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14, color = "#1c164d"),
            legend.position = c(0, 1),
            legend.justification = c("left", "top"),
            legend.text = element_text(size = 11, colour = "#1c164d"),
            legend.key.size = unit(15, "pt"),
            plot.background = element_rect(color = "#1c164d", fill = NA),
            panel.border = element_rect(color = "#1c164d"),
            axis.ticks = element_line(color = "#1c164d"),
            panel.background = element_blank(),
            legend.background = element_blank(),
            strip.background = element_blank(),
            strip.text = if (keep_strip) element_text(size = 14, color = "#1c164d")
                         else element_blank(),
            plot.title = element_blank()
          ),
        width = 5, height = 3.4, dpi = 300, bg = "transparent"
      )
    }
  )
}

# Table of recommendation texts shown next to the plots, driven by clicks on
# any of the plotly charts (shared "click" event source). Call inside
# DT::renderDT() — it reads event_data() and req()
upr_click_table <- function(data) {
  plot_data <- data |>
    select(text_2, state_under_review, response_upr, cycle, health_related:other_health_related, document_code,
           recommending_state_upr, recommending_state_upr_comma) |>
    pivot_longer(cols = health_related:other_health_related) |>
    mutate(
      value = case_when(is.na(value) ~ FALSE,
                        value == "Other" ~ FALSE,
                        value != "Other" ~ TRUE)
    ) |>
    filter(value, name != "health_related") |>
    left_join(theme_labels, by = c("name" = "variable"))

  event.data <- event_data("plotly_click", source = "click")
  req(event.data)

  # Split the customdata back into its parts: theme|response|cycle|recommending
  clicked_info <- strsplit(event.data$customdata, "|", fixed = TRUE)[[1]]
  clicked_theme <- clicked_info[1]
  clicked_response <- clicked_info[2]
  clicked_cycle <- clicked_info[3]
  clicked_recommending <- clicked_info[4]

  res <- plot_data |>
    filter(
      theme_label == clicked_theme,
      response_upr == clicked_response
    )
  if (!(is.na(clicked_cycle) | clicked_cycle == "NA")) {
    res <- res |> filter(cycle == clicked_cycle)
  }
  res <- res |>
    select(text_2, state_under_review, cycle, response_upr, recommending_state_upr, recommending_state_upr_comma) |>
    mutate(state_under_review = factor(state_under_review)) |>
    rename(
      `Recommendation text` = text_2,
      SUR = state_under_review,
      Cycle = cycle,
      Response = response_upr
    )

  if (is.na(clicked_recommending) | clicked_recommending == "NA") {
    res2 <- res |> select(-recommending_state_upr, -recommending_state_upr_comma)
  } else {
    res2 <- res |>
      filter(str_detect(`Recommendation text`, clicked_recommending)
             | str_detect(recommending_state_upr, clicked_recommending)
             | str_detect(recommending_state_upr_comma, clicked_recommending)
      ) |> select(-recommending_state_upr, -recommending_state_upr_comma)
  }

  DT::datatable(res2,
                filter = "top",
                extensions = 'FixedHeader',
                caption = tags$caption(
                  style = "caption-side: top; text-align: left;",
                  paste0("Theme: ", clicked_theme)
                ),
                options = list(
                  pageLength = 10
                  , fixedHeader = TRUE
                  , selectize = list(on = 'change')
                ),
                rownames = FALSE,
                class = 'cell-border stripe hover compact'
  )
}

# 2. UI: BSLIB USER INTERFACE (with page_navbar) ==============================

# The theme definition remains the same
app_theme <- bs_theme(
  version = 5,
  bg = "#F9F9F6",
  fg = "#1c164d",
  primary = "#1c164d",
  base_font = font_google("Lato", local = FALSE)
) |>
  bslib::bs_add_variables(
    "grid-breakpoints" = "(xs: 0, sm: 576px, md: 768px, lg: 1342px, xl: 1423px, xxl: 1640px)",
    .where = "declarations"
)

bs_theme(
  version = 5,
  bg = "#F9F9F6",
  fg = "#1c164d",
  primary = "#1c164d",
  base_font = font_google("Lato", local = FALSE)
)  

# Switch to page_navbar for a top navigation bar
ui <- page_navbar(
  id = "main_navbar",
  theme = app_theme,
  # Rearrange the tags so the link only wraps the image
  
  title = span(
    tags$a(
      href = "https://www.cehdi.org/",
      # label = "Go to CeHDI homepage",
      # target = "_blank",
      img(src = "logo_5.png"
          , height = "50px"
          # , style = "margin-right:10px;"
      )
    )
  ),
  bg = "#1c164d",
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom_bslib.css"),
    # Custom CSS for the PDF download button.
    tags$style(HTML("
      /* The ID selector '#' must match the downloadButton's outputId */
      #download_report {
        background-color: #4a86e8; /* color */
        color: white;             /* White text */
        border-radius: 5px;       /* Rounded corners */
        border: 2px solid #4a86e8;
        font-weight: bold;        /* Make the text bold */
        transition: background-color 0.3s, border-color 0.3s; /* Smooth transition for hover effect */
      }
      
      /* Style for when you hover over the button */
      #download_report:hover {
        background-color: #ec5557; /*  */
        border-color: #ec5557;
        cursor: pointer;          /* Show a 'hand' cursor on hover */
      }
    ")),
    
    tags$style(HTML("
      .selectize-dropdown {
        width: auto !important; /* Let the content define the width */
        min-width: 200px;       /* Set a minimum for good measure */
      }
    ")),
    tags$style(HTML("
  /* ===== Tier 2: 1423px–1639.98px ===== */
  @media (min-width: 1423px) and (max-width: 1639.98px) {
    .navbar-nav > li > a.nav-link,
    .navbar-nav > li > a.dropdown-toggle {
      font-size: 0.75rem !important;
      padding-left: 0.50rem !important;
      padding-right: 0.50rem !important;
    }
    .navbar-nav .dropdown-menu .dropdown-item {
      font-size: 0.75rem !important;
    }
    .navbar-brand img { height: 38px !important; }
  }

  /* ===== Tier 3: 1342px–1422.98px ===== */
  @media (min-width: 1342px) and (max-width: 1422.98px) {
    .navbar-nav > li > a.nav-link,
    .navbar-nav > li > a.dropdown-toggle {
      font-size: 0.70rem !important;
      padding-left: 0.30rem !important;
      padding-right: 0.30rem !important;
    }
    .navbar-nav .dropdown-menu .dropdown-item {
      font-size: 0.70rem !important;
    }
    .navbar-brand img { height: 33px !important; }
  }
"))

),
  
  
  ## Sidebar for Controls ----------------------------------------------------
  # This sidebar is now accessible via a button on the navbar
  sidebar = sidebar(
    width = 400,
    bg = "#1c164d",
    # title = "Controls & Map", # Give the sidebar a title
    
    selectInput("selected_regional_grouping", "Select Regional Grouping:",
                choices = c("Global", "WHO regions", "World Bank regions", 
                            "Sub-regions (UN M49)",
                            # "ECSA-HC Membership", 
                            "CARICOM Membership",
                            "South Centre Membership",
                            "OACPS Membership", "OACPS Member regions",
                            "COMESA Membership",
                            "Fragile and Conflict-affected States (2026)"
                ),
                selected = "Global"),
    
    selectInput("selected_region", "Select Region:",
                choices = c("Global"),
                selected = "Global"),
    
    selectInput("selected_SUR", "Select State:",
                choices = state_geo |> 
                  # Remove non-member states (no data, causes crash)
                  filter(!country %in% c("Western Sahara","Greenland" ,
                                         "Palestine","Vatican",
                                         "Siberian Artifact")) |>  
                  select(country) |> distinct() |> arrange(country) |> 
                  pull(country),
                multiple = FALSE),
    
    card(
      class = "bg-light",
      full_screen = TRUE,
      # card_header("Selected Region on Global Map"),
      card_body(
        plotOutput("global_map", height = "160px"),
        padding = 0
      )
    ),
    
    accordion(
      open = FALSE, 
      width = 390,
      accordion_panel(
        "Disclaimer and data sources",
        markdown("This dashboard displays the results of a **preliminary** analysis regarding recommendations from the first four cycles of the Universal Periodic Review (UPR). ***Results are subject to change as the classification methodology continues to be refined and as data sources are updated***.

UPR recommendations were downloaded directly from the [Universal Human Rights Index](https://uhri.ohchr.org/en/our-data-api), maintained by the Office of the High Commissioner for Human Rights.

Data related to various indicators (e.g. maternal mortality ratio and estimated abortion rates) were accessed via the [Global Health Observatory's API](https://www.who.int/data/gho/info/gho-odata-api), and data regarding the causes of maternal death were obtained using the [IHME's GBD Results tool](https://vizhub.healthdata.org/gbd-results/).

Grouping by Fragile/Conflict-affected Situations (**FCS status**) was made according to the [FCS grouping obtained from the World Bank](https://thedocs.worldbank.org/en/doc/5c7e4e268baaafa6ef38d924be9279be-0090082025/original/FCSListFY26.pdf).

**Map disclaimer:** CeHDI makes no statement or judgment about the legal status or borders of any country, territory, or city shown on these maps. The information is for reference only.")
      ,  ### PDF downloader ------------------------
      downloadButton(
        outputId = "download_report",
        label = "Download Country Profile"
        # ,style = "width: 100%;" # Make the button full-width
      ),
      
      ### JESSE's PDF downloader ------------------------
      # downloadButton(
      #   outputId = "download_report_JESSE",
      #   label = "Download Country Profile"
      #   # ,style = "width: 100%;" # Make the button full-width
      # ),
      
      #### qmd --------------------------
      # downloadButton(
      #   outputId = "download_report_qmd",
      #   label = "Download Report QMD (IN DEVELOPMENT)"
      #   # ,style = "width: 100%;" # Make the button full-width
      # )
        )
    )
  ),
  
  ## Main Content Pages ------------------------------------------------------
  # Each nav_panel is now a separate page accessible from the top navbar
  ### Landing page ------------------
  nav_panel(title = "Health & Rights Observatory", icon = icon("info-circle"),
            # nav_panel(title = "About", icon = icon("info-circle"),
            # card(
            #   fill = FALSE,
            #   card_body(
            markdown(
              "Welcome to the **Health & Rights Observatory**. This platform has been designed and created by the **Global Center for Health Diplomacy and Inclusion (CeHDI)**, to advance and amplify the mainstreaming of the right to health in the Human Rights Council processes, treaty bodies and special procedures as a gateway for universal health coverage and global health equity. The platform is intended to empower diplomats and policymakers across the health, foreign affairs, and related sectors, as well as civil society actors, to advance the Right to Health within global and national human rights discussions."),
            card(
              fill = FALSE,
              card_header("The Right to Health"),
              card_body(markdown(
                "The <a href='https://www.ohchr.org/en/health' target='_blank'>**Right to Health**</a>, as enshrined in <a href='https://www.ohchr.org/en/instruments-mechanisms/instruments/international-covenant-economic-social-and-cultural-rights#article-12' target='_blank'>Article 12 of the International Convenant on Economic, Social and Cultural Rights</a>, is an inclusive human right that extends beyond timely and appropriate health care to encompass the underlying determinants of health. It forms an essential part of States’ obligations under international human rights law and provides a binding normative framework for advancing well-being, equity, and dignity across all sectors of society.  
                           
Under the Right to Health, States have the following obligations:  
-  **Respect**: refrain from directly or indirectly interfering with the enjoyment of the right to health.  
-  **Protect**: take effective measures to prevent third parties from undermining or violating the guarantees of the right to health.  
-  **Fulfill**: adopt appropriate legislative, administrative, budgetary, judicial, promotional, and other measures toward the full realization of the right to health."
                # ))
              ))),
            
            card(
              fill = FALSE,
              card_header("The Right to Health and the Universal Periodic Review"),
              
              card_body(
                
                # 1. The Wrapper: This is a standard div that contains the floating image and text.
                div(
                  style = "overflow: hidden;",
                  
                  # 2. The Floated Clickable Image (using R actionLink)
                  # We put the actionLink here, and use R's tag$div to wrap the image 
                  # and apply the float styles.
                  tags$div(
                    class = "image-float-wrapper",
                    style = "float: left; max-width: 30%; height: auto; margin: 0px 15px 0px 0px;", # Apply float styles here
                    actionLink(
                      inputId = "upr_image_expand", # This ID triggers the modal in the server
                      label = img(
                        src = "WHO_UPR.png",
                        style = "height: auto; width: 100%; object-fit: contain; cursor: pointer;"
                      )
                    ),
                    tags$figcaption(
                      HTML("<small><a href='https://iris.who.int/handle/10665/277114' target='_blank'>Image: WHO</a>  
                                          More than 90,000 recommendations have been issued during the first three cycles of the UPR.</small>"),
                      style = "text-align: center; color: #666; font-size: 0.8em; margin-top: 5px;"
                    )
                  ),
                  
                  # 3. All Text Content (using HTML to ensure it wraps the floated div)
                  HTML(
                    "
        <p>This platform presents data on the Right to Health within the context of the <a href='https://www.ohchr.org/en/hr-bodies/upr/basic-facts' target='_blank'><b>Universal Periodic Review (UPR)</b></a>. This <b>State-led mechanism</b> evaluates each state’s human rights obligations and commitments. The review process is participatory and includes interactive discussions during which any UN Member State may issue recommendations to the State under review, which may then choose to ‘support’ or ‘note’ those recommendations.</p>
        <p>Contact us at info[at]cehdi.org for more information or to give feedback.</p>
        "
                  )
                )
              )
            )
  ),
  
  ### UPR Impact ------------------
  nav_panel(title = "UPR impact", icon = icon("square-poll-vertical"),
            card(
              fill = FALSE,
              card_header("Do UPR recommendations impact health outcomes? (a preliminary analysis)"),
              
              card_body(
                
                # 1. The Wrapper: This is a standard div that contains the floating image and text.
                div(
                  style = "overflow: hidden;",
                  
                  # 2. The Floated Clickable Image (using R actionLink)
                  # We put the actionLink here, and use R's tag$div to wrap the image 
                  # and apply the float styles.
                  tags$div(
                    class = "image-float-wrapper",
                    style = "float: left; max-width: 40%; height: auto; margin: 5px 5px 5px 5px;", # Apply float styles here
                    actionLink(
                      inputId = "upr_analysis", # This ID triggers the modal in the server
                      label = img(
                        src = "full_plot.png",
                        style = "height: auto; width: 100%; object-fit: contain; cursor: pointer;"
                      )
                    )
                  ),
                  
                  # 3. All Text Content (using HTML to ensure it wraps the floated div)
                  HTML(
                    "
        <p>We investigated the potential relationship between <a href='https://www.ohchr.org/en/hr-bodies/upr/basic-facts' target='_blank'>Universal Periodic Review (UPR)</a> recommendations and health outcomes, using maternal mortality as a key indicator. Specifically, we examined whether supporting UPR recommendations on maternal health from the first three cycles was associated with changes in the maternal mortality ratio (MMR) across countries.</p>
        <p>Our preliminary analysis indicates that countries with a higher proportion of accepted recommendations, as well as higher  number of UPR recommendations related to maternal health, show a significant correlation with reductions in MMR over time.</p>
        <br>
        <h3>Summary of methodology</h3>
                <b>Identification of health-related recommendations and thematic classification</b>
        <p>We first developed a rule-based text classification algorithm to identify health-related themes from all available UPR recommendations (obtained from the <a href='https://uhri.ohchr.org/en/our-data-api' target='_blank'>Universal Human Rights Index</a>). We used a <a href='https://iris.who.int/handle/10665/277114' target='_blank'>2019 report from the WHO</a> to further classify the recommendations into thematic groups. For each thematic group, a dictionary of keywords and term combinations was developed and matched against the recommendation text. A single recommendation could match several thematic groups.
        
        <p>The “Maternal health” thematic category including the following  dictionary of keywords: \"obstetric,\" \"prenatal,\" \"postnatal,\" \"miscarriage,\" and \"maternal mortality.\" The algorithm also identified recommendations containing specific combinations of terms linking pregnancy to healthcare access (e.g., \"pregnant\" appearing in conjunction with \"healthcare,\" \"medical care,\" or \"free access\"). Recommendations primarily addressing abortion were conditionally classified as maternal health if they explicitly contextualized abortion access within the framework of saving the woman’s life or preserving her physical health. Linguistic false positives were explicitly excluded from the matches.</p>

        <b>Statistical analysis</b>
        <p>Using a linear mixed-effects model to account for individual country trends, we estimated trends in MMR over time as a function of engagement with UPR recommendations related to maternal health. We specifically tested for a three-way interaction between time, the number of recommendations received, and the proportion of recommendations supported, which allowed us to observe whether engagement with the UPR process was associated with faster rates of MMR reduction.</p>
        <p>We visualized these results by comparing the predicted rates of decline for countries with varying levels of recommendation volume (e.g. 5 vs. 15 recommendations) and with varying levels of recommendation acceptance (e.g., 50% vs. 90% support). Pairwise slope comparisons were calculated to assess the statistical differences. All analyses were performed using R.</p>

        <b>Summary of Results</b>
        <p>We found that a country's support of UPR recommendations on maternal health was associated with a faster decline in MMR. This effect was much stronger when a country received a higher number of the recommendations. When a country received only 5 recommendations (left panel of the figure above), the difference in the rate of MMR reduction between supporting 90% and supporting 50% was minimal but still statistically significant (p = 0.042). However, when a country received 15 recommendations (right panel), supporting 90% of the recommendations was associated with a significantly faster reduction in MMR over time compared to supporting only 50% (<b>p = 0.013</b>).</p>
        <b>Conclusion</b>
        <p>This preliminary analysis suggests that the UPR process may have impact in contributing towards positive health outcomes as demonstrated by the relationship between the UPR recommendations pertaining to maternal health and reduction of MMR over time.</p>
        <p>It is important to note that these results should be interpreted with caution, as this analysis cannot establish causality. Nevertheless, it signifies a potentially important role of engagement with the UPR process and its associated peer review process in enhancing political support and attention for critical health challenges.</p>
        "
                  )
                )
              )
            )
  ),
  ### UPR recommendations ----------------
  nav_menu(title = "UPR recommendations", icon = icon("people-arrows"),
           #### UPR: Regional -----------------------
           nav_panel(title = "By Region", icon = icon("globe-africa"),
                     markdown("Click on a bar chart to view the text of the relevent UPR recommendations"),
                     layout_column_wrap(
                       style = css(grid_template_columns = "1fr 1fr"),
                       navset_card_tab(
                         full_screen = TRUE,
                         # title = "Regional Recommendation Themes",
                         nav_panel("Health-related Recommendations", 
                                   card(
                                     # fill = FALSE,
                                     card_body(
                                       min_height = 450,
                                       plotlyOutput("plotly_UPR_regional")
                                     ),
                                     card_footer(
                                       downloadButton(
                                         outputId = "download_plotly_UPR_regional",
                                         label = "Download as PNG"
                                       ),
                                       downloadButton(
                                         outputId = "download_plotly_UPR_regional_fr",
                                         label = "Télécharger (français)"
                                       )
                                     )
                                   )),
                         nav_panel("Per UPR Cycle",
                                   card(
                                     card_body(
                                       min_height = 550,
                                       plotlyOutput("plotly_UPR_regional_cycle")
                                     )
                                   )
                         ),
                         nav_panel("Proportion Health-Related",
                                   card(
                                     full_screen = TRUE,
                                     fill = FALSE,
                                     card_body(plotOutput("global_plot")),
                                     card_footer(
                                       downloadButton(
                                         outputId = "download_global_plot",
                                         label = "Download as PNG"
                                       ),
                                       downloadButton(
                                         outputId = "download_global_plot_fr",
                                         label = "Télécharger (français)"
                                       )
                                     )
                                   )
                         ),
                         nav_panel("Recommending states",
                                   card(
                                     # fill = FALSE,
                                     full_screen = TRUE,
                                     # card_header("Recommending States (top 20)"),
                                     card_body(
                                       plotlyOutput("plotly_UPR_regional_recommending")
                                     )
                                   )
                         )
                       ),
                       layout_column_wrap(
                         card(
                           full_screen = TRUE,
                           fill=TRUE,
                           card_body(DTOutput("plotly_table_regional"))
                         )
                       )
                     )
           ),
           
           ## UPR - SuR2 ----------------------------------------------------------
           nav_panel(title = "By State", icon = icon("flag"),
                     markdown("Click on a bar chart to view the text of the relevent UPR recommendations"),
                     layout_column_wrap(
                       style = css(grid_template_columns = "1fr 1fr"),
                       navset_card_tab(
                         full_screen = TRUE,
                         nav_panel("Health-related Recommendations", 
                                   card(
                                     # fill = FALSE,
                                     card_body(
                                       min_height = 450,
                                       plotlyOutput("plotly_UPR_SUR")
                                     ),
                                     card_footer(
                                       downloadButton(
                                         outputId = "download_plotly_UPR_SUR",
                                         label = "Download as PNG"
                                       ),
                                       downloadButton(
                                         outputId = "download_plotly_UPR_SUR_fr",
                                         label = "Télécharger (français)"
                                       )
                                     )
                                   )),
                         nav_panel("Per UPR Cycle", 
                                   card(
                                     card_body(
                                       min_height = 550,
                                       plotlyOutput("plotly_UPR_SUR_cycle")
                                     )
                                   )
                         ),
                         nav_panel("Proportion Health-Related",
                                   card(
                                     full_screen = TRUE,
                                     fill = FALSE,
                                     card_body(plotOutput("plot")),
                                     card_footer(
                                       downloadButton(
                                         outputId = "download_rec_plot_object",
                                         label = "Download as PNG"
                                       ),
                                       downloadButton(
                                         outputId = "download_rec_plot_object_fr",
                                         label = "Télécharger (français)"
                                       )
                                     )
                                   )
                         ),
                         nav_panel("Data Table",
                                   card(fill=TRUE,
                                        card_body(DTOutput("DT_table")),
                                        card_footer(
                                          downloadButton(
                                            outputId = "download_data_csv",
                                            label = "State data (csv)"
                                          ),
                                          downloadButton(
                                            outputId = "download_data_xlsx",
                                            label = "State data (xlsx)"
                                          ),
                                          downloadButton(
                                            outputId = "download_data_csv_all",
                                            label = "All data (csv)"
                                          )
                                        )
                                   )),
                         nav_panel("Recommending states",
                                   card(
                                     fill = FALSE,
                                     full_screen = TRUE,
                                     # card_header("Recommending States (top 20)"),
                                     card_body(
                                       plotlyOutput("plotly_UPR_SUR_recommending")
                                     )
                                   )
                         )
                       ),
                       layout_column_wrap(
                         card(
                           full_screen = TRUE,
                           fill=TRUE,
                           card_body(DTOutput("plotly_table_SUR"))
                         )
                       )
                     )
           )
  ),
  
  ### UHC ---------------------
  nav_panel(title = "Universal Health Coverage", icon = icon("umbrella"),
            # markdown("UHC: Universal Health Coverage"),
            layout_column_wrap(
              layout_column_wrap(
                width=1,
                # card(full_screen = TRUE,card_header("UHC Service Coverage Index (2021)"), plotOutput("UHC_map")),
                card(full_screen = TRUE, 
                     # fill=FALSE,
                     card_header("UHC Service Coverage Index (2023)"),
                     markdown("Data: <a href='https://www.who.int/data/gho/data/indicators/indicator-details/GHO/uhc-index-of-service-coverage' target='_blank'>WHO</a>"),
                     leafletOutput("UHC_map_interactive")),
                card(full_screen = TRUE, 
                     # fill=FALSE,
                     card_header("UHC sub-index on reproductive, maternal, newborn, and child health (2023)"),
                     markdown("Data: <a href='https://www.who.int/data/gho/data/indicators/indicator-details/GHO/uhc-sci-components-reproductive-maternal-newborn-and-child-health' target='_blank'>WHO</a>"),
                     leafletOutput("UHC_RMNCH_map_interactive"))
                # card(full_screen = TRUE,card_header("UHC sub-index on RMNCH (2021)"), plotOutput("UHC_RMNCH_map"))
              ),
              card(full_screen = TRUE, card_header("UHC indices over time"), plotOutput("UHC_trend"))
            )
  ),
  
  ### Cervical Cancer ------------------------------
  nav_menu(title = "Cervical Cancer", icon = icon("ribbon"),
           
           #### Tab 1: Mortality -----------------------------------
           nav_panel(
             title = "Mortality Rate",
             layout_columns(
               # col_widths = breakpoints(sm = c(12, 12), md = c(6, 6)),
               
               ##### Left: map + trend -------------------------------------------
               card(full_screen = TRUE,
                    card_header("Cervical Cancer Mortality Rate"),
                    markdown("Age-standardized mortality rate per 100,000 women.
  
  Data: <a href='https://vizhub.healthdata.org/gbd-results/?params=gbd-api-2023-public/f68172d51927f14d97fe06a2caf6cc5b' target='_blank'>GBD (2023)</a>"),
                    leafletOutput("ccmortality_map_interactive"),
               ),
               
               ##### Right: explanation -------------------------------------------
               card(full_screen = TRUE,
                    card_header("Historical Cervical Cancer Mortality Rate"),
                 markdown(
                   "The dashed vertical line on the trend chart below, when shown, marks the year the selected country introduced HPV vaccination into its national immunization program."
                 ),
                 plotlyOutput("ccmr_trend"),
               )
             )
           ),
           
           ##### Tab 2: HPV Vaccination -----------------------------------
           nav_panel(
             title = "HPV Vaccination",
             layout_columns(
               card(full_screen = TRUE,
                    card_header("HPV Vaccination Coverage (2025)"),
                    leafletOutput("hpv_coverage_map_interactive", height = "400px")
               ),
               card(fill = FALSE,
                    card_header(shiny::icon("circle-info")),
                    markdown(
                      "**HPV Vaccination Coverage (%)**: Coverage of girls fully vaccinated against HPV by age 15 (final dose of the recommended schedule).

As of 2025, 32 countries had not reported a vaccination coverage level and are shown as no data on the map.

- **Fully vaccinated**: completion of the recommended schedule, one or two doses depending on the national schedule in place.
- **At least 1 dose**: girls who received at least one dose of the HPV vaccine by age 15, whether or not they completed the full recommended schedule.

Data: <a href='https://immunizationdata.who.int/global/wiise-detail-page/human-papillomavirus-(hpv)-vaccination-coverage?CODE=Global&GROUP=Countries&ANTIGEN=&YEAR=' target='_blank'>WHO — HPV vaccination coverage</a> and <a href='https://immunizationdata.who.int/global/wiise-detail-page/introduction-of-hpv-(human-papilloma-virus)-vaccine?ISO_3_CODE=&YEAR=' target='_blank'>WHO — HPV vaccine introduction status</a>"
                    )
               )
             )
           ),
           
           #### Tab 3: Cervical Screening -----------------------------------
           nav_panel(
             title = "Cervical Cancer Screening",
             layout_columns(
               card(full_screen = TRUE,
                    card_header(
                      div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
                          span("Cervical Screening Coverage (2019)"),
                          selectInput(
                            "screening_period", NULL,
                            choices = c(
                              "Lifetime" = "TIMEPERIOD_TIMEPERIOD_EVER_LIFETIME",
                              "Last 5 years"             = "TIMEPERIOD_TIMEPERIOD_LAST05YEARS",
                              "Last 3 years"             = "TIMEPERIOD_TIMEPERIOD_LAST03YEARS",
                              "Last year"                = "TIMEPERIOD_TIMEPERIOD_LAST01YEAR"
                            ),
                            selected = "TIMEPERIOD_TIMEPERIOD_LAST05YEARS",
                            width = "220px"
                          )
                      )
                    ),
                    leafletOutput("screening_coverage_map_interactive", height = "400px")
               ),
               card( fill = FALSE,
                 card_header(shiny::icon("circle-info")),
                 markdown(
                   "**Cervical Cancer Screening Coverage (%)**: Percentage of women aged 30–49 who report being screened for cervical cancer, using any WHO-recognized test : HPV DNA testing, cytology, or visual inspection with acetic acid (VIA). 
                   
**Screening period**: The dropdown above the map controls the recall period used to define screening status. *Lifetime* reflects the proportion of women ever screened, while *Last 5 years*, *Last 3 years*, and *Last year* restrict this to screening occurring within the specified period.

                   
Note: this indicator captures screening with *any* test type; the 70% target specifically calls for a *high-performance* (HPV DNA) test.

Data: <a href='https://www.who.int/data/gho/info/gho-odata-api' target='_blank'>WHO GHO</a>"
                 )
               )
             )
           )
           ),
  ### Maternal health ------------------------------
  nav_menu(title = "Maternal health", icon = icon("person-pregnant"),
           #### Maternal mortality -----------------------
           nav_panel(title = "Maternal Mortality", icon = icon("house-medical"),
                     markdown("<strong>Maternal Mortality Ratio (MMR):</strong> Number of maternal deaths per 100,000 live births."),
                     layout_column_wrap(
                       full_screen = TRUE,
                       style = css(grid_template_columns = "1fr 1fr"),
                       layout_column_wrap(
                         width=1,
                         style = css(grid_template_rows = "6fr 5fr"),
                         # card(full_screen = TRUE,card_header("MMR Estimate Map (2023)"), plotOutput("mmr_map")),
                         card(
                           full_screen = TRUE,
                           # fill=FALSE,
                           card_header("MMR estimates in 2023"), 
                           markdown("Data: <a href='https://www.who.int/data/gho/data/indicators/indicator-details/GHO/maternal-mortality-ratio-(per-100-000-live-births)' target='_blank'>WHO</a>"),
                           leafletOutput("mmr_map_interactive")
                         ),
                         card(full_screen = TRUE,card_header("MMR trends"), plotOutput("mmr_time_plot_neighbors"))
                       ),
                       navset_card_tab(
                         # title = "Causes of Maternal Death",
                         full_screen = TRUE,
                         nav_panel("Causes of Maternal Death", plotOutput("mmr_causes", height = "600px")),
                         nav_panel("Causes Over Time", plotOutput("mmr_causes_longitudinal", height = "600px")),
                         nav_panel(
                           shiny::icon("circle-info"),
                           markdown("The below *abbreviated definitions* were compiled from the **IHME's** <a href='https://www.healthdata.org/research-analysis/diseases-injuries-risks/factsheets-hierarchy' target='_blank'>factsheets pages for the level 4 causes of maternal disorders</a>:  
                  
**Maternal haemorrhage** includes both postpartum haemorrhage (defined as blood loss ≥500 ml for vaginal delivery and ≥1000 ml for caesarean delivery) and antepartum haemorrhage (defined as vaginal bleeding from any cause at or beyond 20 weeks of gestation).  

**Maternal sepsis** is defined as a temperature <36°C or >38°C and clinical signs of shock (systolic blood pressure <90 mmHg and tachycardia >120 bpm). **Other maternal infections** are defined as any maternal infections excluding HIV, STI, or not related to pregnancy. 

**Maternal hypertensive disorders** include gestational hypertension (onset after 20 weeks gestation), pre-eclampsia, severe preeclampsia, and eclampsia, but exclude chronic hypertension (onset prior to pregnancy or prior to 20 weeks gestation) unless superimposed preeclampsia or eclampsia develop.

**Maternal obstructed labour and uterine rupture** aggregates obstructed labour (arrest in the first or second stage of active labour despite sufficient contractions), uterine rupture (non-surgical breakdown of uterine wall), and fistula (an abnormal opening between the vagina and the bladder or rectum following childbirth). 

**Abortion** is defined as elective or medically indicated termination of pregnancy at any gestational age. **Miscarriage** is defined as spontaneous loss of pregnancy before 24 weeks of gestation with complications requiring medical care.

**Ectopic pregnancy** is defined as pregnancy occurring outside of the uterus.

**Indirect maternal deaths** are due to existing diseases that are exacerbated by pregnancy. Examples include maternal infections and parasitic diseases complicating pregnancy, childbirth, and the puerperium, and diabetes in pregnancy, childbirth, and the puerperium. 

**Late maternal deaths** are deaths that occur six weeks to one year after the end of pregnancy, excluding incidental deaths.

**Maternal deaths aggravated by HIV/AIDS** are deaths occurring in HIV-positive women whose pregnancy has exacerbated their HIV/AIDS, leading to death.

**Other direct maternal disorders** encompasses a wide range of maternal disorders that do not map to other diseases in the GBD cause list, including other fatal or non-fatal complications occurring during pregnancy, childbirth, and the postpartum period.")
                         )
                       )
                     )
           ),
           
           #### Skilled birth attendance ------------------------
           nav_panel(title = "Skilled birth attendance", icon=icon("user-nurse"),
                     layout_column_wrap(
                       layout_column_wrap(
                         width=1,
                         card(
                           full_screen = TRUE,
                           card_header("Births attended by skilled health personnel (%), latest year"),
                           markdown("Data: <a href='https://www.who.int/data/gho/data/indicators/indicator-details/GHO/births-attended-by-skilled-health-personnel-(-)' target='_blank'>WHO</a>"),
                           leafletOutput("skilled_birth_map_interactive")
                           # ,plotOutput("skilled_birth")
                         ),
                         card(
                           full_screen = TRUE,
                           card_header("Trends vs. Neighbors"), 
                           plotOutput("skilled_birth_plot_neighbors")
                         )
                       ),
                       layout_column_wrap(
                         width=1,
                         card(
                           full_screen = TRUE,
                           card_header("Proportion of births delivered in a health facility (%), latest year"),
                           markdown("Data: <a href='https://www.who.int/data/gho/data/indicators/indicator-details/GHO/institutional-births-(-)' target='_blank'>WHO</a>"),
                           leafletOutput("institutional_birth_map_interactive")
                           # ,plotOutput("births_facility")
                         ),
                         card(
                           full_screen = TRUE,
                           card_header("Trends vs. Neighbors"), 
                           plotOutput("births_facility_plot_neighbors")
                         )
                       )
                     )
           ),
           
           #### Abortion ------------------------
           nav_panel(title = "Abortion", icon = icon("prescription-bottle-medical"),
                     layout_column_wrap(
                       card(
                         # fill = FALSE,
                         full_screen = TRUE,
                         card_header("Abortion Laws (evaluated June 2023)"),
                         leafletOutput("abortion_laws_map_interactive"),
                         # plotOutput("abortion_map_sur"),
                         markdown("Data: <a href='https://reproductiverights.org/maps/worlds-abortion-laws/' target='_blank'>Center for Reproductive Rights</a>")
                       ),
                       layout_column_wrap(
                         width = 1,
                         card(
                           full_screen = TRUE,
                           card_header("Estimated abortion rate, per 1,000 (annual estimate for the period 2015-2019)"),
                           markdown("Data: <a href='https://www.who.int/data/gho/data/indicators/indicator-details/GHO/SRH_ABORTION_RATE' target='_blank'>WHO</a>"),
                           leafletOutput("abortion_rate_map_interactive")
                           # plotOutput("abortion_rate")
                         ),
                         card(
                           full_screen = TRUE,
                           card_header("Maternal deaths attributed to abortion and miscarriage in 2021 (per 100,000)"),
                           markdown("Data: <a href='https://gbd2021.healthdata.org/gbd-results/' target='_blank'>GBD Study</a>"),
                           leafletOutput("abortion_deaths_map_interactive")
                           # plotOutput("abortion_rate")
                         )
                       )
                     )
           )
  ),
  ## Family planning ------------------
  nav_panel(title = "Family planning", icon = icon("people-group"),
            # "Family planning",
            layout_column_wrap(
              full_screen = TRUE,
              style = css(grid_template_columns = "1fr 1fr"),
              # card(
              #   fill = FALSE,
              #   full_screen = TRUE,
              #   card_header("Met Need for Family planning (%)"),
              #   plotOutput("family_planning")
              # )
              card(full_screen = TRUE, 
                   # fill=FALSE,
                   card_header("Met need for family planning using modern methods (%, latest year available)"),
                   markdown("Women of reproductive age (aged 15-49 years) who have their need for family planning satisfied with modern methods (%). Data: <a href='https://www.who.int/data/gho/data/indicators/indicator-details/GHO/proportion-of-women-of-reproductive-age-who-have-their-need-for-family-planning-satisfied-with-modern-methods' target='_blank'>WHO</a>"),
                   leafletOutput("family_planning_map_interactive")),
              layout_column_wrap(
                width=1,
                style = css(grid_template_rows = "1fr 1fr"),
                card(
                  full_screen = TRUE,
                  card_header("Adolescent birth rate, per 1000 women aged 15-19 (latest year available)"),
                  markdown("Data: <a href='https://www.who.int/data/gho/data/indicators/indicator-details/GHO/adolescent-birth-rate-(per-1000-women-aged-15-19-years)' target='_blank'>WHO</a>"),
                  leafletOutput("adolescent_birth_map_interactive")
                  # card_header("Estimated Unintended Pregnancy Rate (2015-2019)"),
                  # plotOutput("unintended_pregnancy")
                ),
                card(
                  full_screen = TRUE,
                  card_header("Estimated unintended pregnancy rate, per 1,000 (annual estimate for the period 2015-2019)"),
                  markdown("Data: <a href='https://www.who.int/data/gho/data/indicators/indicator-details/GHO/SRH_PREGNANCY_UNINTENDED_RATE' target='_blank'>WHO</a>"),
                  leafletOutput("unintended_pregnancy_map_interactive")
                  # card_header("Estimated Unintended Pregnancy Rate (2015-2019)"),
                  # plotOutput("unintended_pregnancy")
                )
              )
            )
  ),
  
  ## Legal framework  ------------------
  nav_panel(title = "Legal framework", icon = icon("building-columns"),
            # "Family planning",
            layout_column_wrap(
              full_screen = TRUE,
              style = css(grid_template_columns = "1fr 1fr"),
              card(
                # fill = FALSE,
                full_screen = TRUE,
                card_header("Does the constitution explicitly guarantee an approach to the right to health? (as of June 2024)"),
                markdown("Approaches to health include the right to health, public health, or medical care. Data: <a href='https://www.worldpolicycenter.org/policies/does-the-constitution-explicitly-guarantee-an-approach-to-the-right-to-health' target='_blank'>World Policy Center</a>"),
                leafletOutput("const_anyhealth_map_interactive")
                # plotOutput("constitution_const_anyhealth")
              ),
              # card(
              #   full_screen = TRUE,
              #   card_header("Status of ratification of the International Covenant on Economic, Social and Cultural Rights (as of October 2025)"),
              #   markdown("Data: <a href='https://indicators.ohchr.org' target='_blank'>OHCHR</a>"),
              #   leafletOutput("ICESCR_status_map_interactive")
              # ),
              navset_card_tab(
                # title = "Causes of Maternal Death",
                full_screen = TRUE,
                nav_panel("International Covenant on Economic, Social and Cultural Rights", 
                          markdown("Status of ratification (as of October 2025). Data: <a href='https://indicators.ohchr.org' target='_blank'>OHCHR</a>"),
                          leafletOutput("ICESCR_status_map_interactive")),
                nav_panel(
                  shiny::icon("circle-info"),
                  markdown(
                    "The <a href='https://www.ohchr.org/en/health' target='_blank'>**Right to Health**</a>, as enshrined in <a href='https://www.ohchr.org/en/instruments-mechanisms/instruments/international-covenant-economic-social-and-cultural-rights#article-12' target='_blank'>Article 12 of the International Convenant on Economic, Social and Cultural Rights</a>, is an inclusive human right that extends beyond timely and appropriate health care to encompass the underlying determinants of health. It forms an essential part of States’ obligations under international human rights law and provides a binding normative framework for advancing well-being, equity, and dignity across all sectors of society.  
                           
Under the Right to Health, States have the following obligations:  
-  **Respect**: refrain from directly or indirectly interfering with the enjoyment of the right to health.  
-  **Protect**: take effective measures to prevent third parties from undermining or violating the guarantees of the right to health.  
-  **Fulfill**: adopt appropriate legislative, administrative, budgetary, judicial, promotional, and other measures toward the full realization of the right to health."
                  )
                )
            )
  )
  ),
  
  nav_spacer(),
  nav_item(
    tags$a(
      shiny::icon("github", class = "fa-1x"), # The GitHub icon  
      # "Source",              # Optional text next to the icon
      href = "https://github.com/CeHDI-Foundation/UPR-Health-Explorer", # <-- REPLACE with your repo URL
      target = "_blank"      # Opens the link in a new tab
    )
  )
)

# 3. SERVER: REACTIVE LOGIC ============================
server <- function(input, output, session) {
  
  ## Plot width expressions -----------
  # This reactive expression captures the real-time width of our plot container.
  # It's debounced to wait 500ms after a resize before updating.
  plot_width_mmr_neighbors <- reactive({
    session$clientData$output_mmr_time_plot_neighbors_width
  }) |> debounce(500)
  
  plot_width_skilled_birth_neighbors <- reactive({
    session$clientData$output_skilled_birth_plot_neighbors_width
  }) |> debounce(500)
  
  plot_width_births_facility_neighbors <- reactive({
    session$clientData$output_births_facility_plot_neighbors_width
  }) |> debounce(500)
  
  plot_width_UHC_neighbors <- reactive({
    session$clientData$output_UHC_trend_width
  }) |> debounce(500)
  
  ## About page options ------------------------
  
  # observeEvent(input$home_button, {
  #   updateNavbarPage(
  #     session = session,
  #     inputId = "main_navbar",
  #     selected = "HaRO" # The title of the first nav_panel
  #   )
  # })
  
  observeEvent(input$upr_image_expand, {
    showModal(modalDialog(
      # title = "Graphical overview of the UPR process",
      img(src = "WHO_UPR.png", style = "width: 100%"),
      size = "xl",           # Make the modal large
      easyClose = TRUE,     # Allow closing by clicking outside
      footer = NULL         # Remove the default buttons
    ))
  })
  
  observeEvent(input$upr_analysis, {
    showModal(modalDialog(
      # title = "Trend analysis of States’ maternal mortality ratios (MMR) over time compared with engagement with UPR recommendations related to maternal health",
      img(src = "full_plot.png", style = "width: 100%"),
      size = "l",           # Make the modal large
      easyClose = TRUE,     # Allow closing by clicking outside
      footer = NULL         # Remove the default buttons
    ))
  })
  
  ## Reactive Expressions for Data Filtering ---------------------------------
  state_geo_reactive <- reactive({
    if (input$selected_regional_grouping == "Sub-regions (UN M49)") {
      state_geo |> mutate(region_dashboard = subregion)
    } else if (input$selected_regional_grouping == "World Bank regions") {
      state_geo |> mutate(region_dashboard = wbregion)
    } else if (input$selected_regional_grouping == "WHO regions") {
      state_geo |> mutate(region_dashboard = WHO_region)
    }else if (input$selected_regional_grouping == "ECSA-HC Membership") {
      state_geo |> mutate(region_dashboard = ECSA_status)
    } else if (input$selected_regional_grouping == "Fragile and Conflict-affected States (2026)") {
      state_geo |> mutate(region_dashboard = FCS_status)
    } else if (input$selected_regional_grouping == "CARICOM Membership") {
      state_geo |> mutate(region_dashboard = CARICOM_status)
    } else if (input$selected_regional_grouping == "OACPS Membership") {
      state_geo |> mutate(region_dashboard = OACPS_status)
    } else if (input$selected_regional_grouping == "OACPS Member regions") {
      state_geo |> mutate(region_dashboard = OACPS_region)
    } else if (input$selected_regional_grouping == "COMESA Membership") {
      state_geo |> mutate(region_dashboard = COMESA_status)
    } else if (input$selected_regional_grouping == "South Centre Membership") {
      state_geo |> mutate(region_dashboard = SC_status)
    } else {
      state_geo |> mutate(region_dashboard = "Global")
    }
  })
  
  
  sdg_data_dashboard <- reactive({
    sdg_data |>
      left_join(state_geo_reactive() |>
                  st_drop_geometry() |>
                  select(country, region_dashboard),
                by = c("state_under_review" = "country"))
  })
  
  # Filter for the selected State Under Review (SUR)
  filtered_upr <- reactive({
    req(input$selected_SUR)
    sdg_data_dashboard() |>
      filter(state_under_review == input$selected_SUR)
  })
  
  # Filter for the selected Region
  filtered_upr_region <- reactive({
    if (input$selected_region == "Global") {
      sdg_data_dashboard()
    } else {
      sdg_data_dashboard() |>
        filter(region_dashboard == input$selected_region)
    }
  })
  
  region_selection <- reactive({
    if (input$selected_region == "Global") {
      state_geo_reactive()
    } else {
      state_geo_reactive() |>
        filter(region_dashboard == input$selected_region)
    }
  })
  
  ## Observers for Dynamic UI Updates --------------------------------------
  observeEvent(input$selected_regional_grouping, {
    if (input$selected_regional_grouping == "Global") {
      # If the top-level grouping is "Global", force the region to "Global" as well.
      updateSelectInput(
        session, "selected_region",
        choices = "Global",
        selected = "Global"
      )
    } else {
      # Otherwise, populate the region dropdown with the appropriate sub-regions.
      updateSelectInput(
        session, "selected_region",
        choices = levels(state_geo_reactive()$region_dashboard)
      )
    }
  })
  
  observeEvent(input$selected_region, {
    choices <- region_selection() |> 
      # Remove non-member states (no data, causes crash)
      filter(!country %in% c("Western Sahara","Greenland" ,
                             # "Palestine",
                             "Vatican",
                             "Siberian Artifact")) |>  
      select(country) |> distinct() |> arrange(country) |> 
      pull(country)
    updateSelectInput(
      session, "selected_SUR",
      choices = choices,
      selected = choices[1]
    )
  })
  
  ## Helper Reactives for Plotting -----------------------------------------
  SUR_region <- reactive({
    req(input$selected_SUR)
    state_geo_reactive()[state_geo_reactive()$country == input$selected_SUR, ]$region_dashboard
  })
  
  SUR_WHOregion <- reactive({
    req(input$selected_SUR)
    state_geo_reactive()[state_geo_reactive()$country == input$selected_SUR, ]$WHO_region
  })
  
  SUR_subregion <- reactive({
    req(input$selected_SUR)
    state_geo_reactive()[state_geo_reactive()$country == input$selected_SUR, ]$subregion
  })
  
  sur_area <- reactive({
    req(input$selected_SUR)
    state_geo_reactive() |>
      filter(country %in% c(input$selected_SUR)) |>
      st_area() |> as.numeric()
  })
  
  coord_selected_SUR <- reactive({
    req(input$selected_SUR)
    state_geo_reactive() |> 
      filter(country %in% c(input$selected_SUR)) |>
      pull(point_centroid)
  })
  
  m_zoom <- reactive({
    req(input$selected_SUR)
    if(sur_area()<10^8){8} else if(
      sur_area()<10^9){7} else if(
        sur_area() < 10^10){5} else if(
          sur_area() < 10^11){4} else if(
            sur_area() < 10^12){3} else if(
              sur_area() < 10^13){2} else if(
                sur_area() < 10^15){2} else{1}
  }) 
  
  bbox_selected_SUR <- reactive({
    req(input$selected_SUR)
    (if(SUR_WHOregion() == "Western Pacific Region (WPR)"){
      state_geo_reactive() |> 
        st_difference(polygon_shift) |> 
        st_shift_longitude()} else{state_geo_reactive()}) |>
      filter(country %in% c(input$selected_SUR)) |>
      st_bbox()
  })
  
  bbox_SUR_region <- reactive({
    req(SUR_region())
    (if(SUR_WHOregion() == "Western Pacific Region (WPR)"){
      state_geo_reactive() |> 
        st_difference(polygon_shift) |> 
        st_shift_longitude()} else{state_geo_reactive()}) |>
      filter(region_dashboard %in% c(SUR_region())) |>
      st_bbox()
  })
  
  bbox_SUR_region_dynamic <- reactive({
    req(SUR_region())
    if(input$selected_regional_grouping == "Global"){
      (if(SUR_WHOregion() == "Western Pacific Region (WPR)"){
        state_geo |> 
          st_difference(polygon_shift) |> 
          st_shift_longitude()} else{state_geo}) |>
        filter(WHO_region %in% c(SUR_WHOregion())) |>
        st_bbox()} else{
          (if(SUR_WHOregion() == "Western Pacific Region (WPR)"){
            state_geo_reactive() |> 
              st_difference(polygon_shift) |> 
              st_shift_longitude()} else{state_geo_reactive()}) |>
            filter(region_dashboard %in% c(SUR_region())) |>
            st_bbox()}
  })
  
  bbox_SUR_subregion <- reactive({
    (if(SUR_WHOregion() == "Western Pacific Region (WPR)"){
      state_geo |> 
        st_difference(polygon_shift) |> 
        st_shift_longitude()} else{state_geo}) |>
      filter(subregion %in% c(SUR_subregion())) |>
      st_bbox()
  })
  
  bbox_SUR_WHOregion <- reactive({
    (if(SUR_WHOregion() == "Western Pacific Region (WPR)"){
      state_geo |> 
        st_difference(polygon_shift) |> 
        st_shift_longitude()} else{state_geo}) |>
      filter(WHO_region %in% c(SUR_WHOregion())) |>
      st_bbox()
  })
  
  
  ## PDF Country profile -------------------------------------------------------
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("CeHDI-Profile-", input$selected_SUR, ".pdf")
    },
    
    content = function(file) {
      withProgress(message = 'Generating your report', value = 0, {
        
        # incProgress(0.1, detail = "--> Preparing template")
        
        # Logic to dynamically get flag image. Flags downloaded from:
        # https://stefangabos.github.io/world_countries/#custom-download
        country_iso2 <- state_geo |> 
          filter(country == input$selected_SUR) |> 
          mutate(iso2=tolower(iso2)) |> 
          pull(iso2) |> 
          unique()
        flag_filename <- paste0(country_iso2, ".png")
        source_flag <- here::here("flags", flag_filename)
        
        # Create a temporary directory for Quarto to work in.
        temp_dir <- tempdir()
        
        # Copy Rmd and other relevant files into that directory (comment out unused versions as needed).
        ## V1
        # temp_report_path <- file.path(temp_dir, "report-template.Rmd")
        # file.copy("report-template.Rmd", temp_report_path, overwrite = TRUE)
        ## V2
        # temp_report_path <- file.path(temp_dir, "report-template-2.Rmd")
        # file.copy("report-template-2.Rmd", temp_report_path, overwrite = TRUE)
        ## V3
        temp_report_path <- file.path(temp_dir, "report-template-3.Rmd")
        file.copy("report-template-3.Rmd", temp_report_path, overwrite = TRUE)
        
        # Remaining code
        file.copy("preamble.tex", temp_dir, overwrite = TRUE)
        file.copy(here("logos", "logo.png"), temp_dir, overwrite = TRUE)
        file.copy(here("logos", "logo2.png"), temp_dir, overwrite = TRUE)
        file.copy(here("www", "WHO_UPR_nobg.png"), temp_dir, overwrite = TRUE)
        file.copy(here("www", "WHO_UPR-removebg3.png"), temp_dir, overwrite = TRUE)
        file.copy(source_flag, temp_dir, overwrite = TRUE)
        file.rename(file.path(temp_dir, flag_filename), 
                    file.path(temp_dir, "countryflag.png"))
        
        incProgress(0.6, detail = "--> Rendering PDF (this may take a moment)")
        # Render the document inside the temporary directory.
        rmarkdown::render(
          input = temp_report_path,
          # output_file = file,
          output_dir = temp_dir,
          output_file = "report.pdf",
          params = list(
            country_name = input$selected_SUR,
            upr_all = upr_themes_all_object(),
            rec_plot = rec_plot_object(),
            # mmr_map_plot = mmr_map_object(),
            bbox_selected_SUR = bbox_selected_SUR(),
            sur_area = sur_area(),
            bbox_SUR_region_dynamic = bbox_SUR_region_dynamic()
          ),
          envir = new.env(parent = globalenv())
        )
        
        # Copy the generated PDF from the temporary directory to the final
        #    download path that Shiny expects.
        file.copy(file.path(temp_dir, "report.pdf"), file, overwrite = TRUE)
        
        incProgress(1, detail = "Done!")
      })
    }
  )
  
  ## PDF JESSE Country profile -------------------------------------------------------
  output$download_report_JESSE <- downloadHandler(
    filename = function() {
      paste0("CeHDI-Profile-", input$selected_SUR, ".pdf")
    },
    
    content = function(file) {
      withProgress(message = 'Downloading your report', value = 0, {
        
        # Copy the generated PDF to the final download path that Shiny expects.
        file.copy(file.path("report_pdfs", paste0(input$selected_SUR,".pdf")), file, overwrite = TRUE)
        
        incProgress(1, detail = "Done!")
      })
    }
  )
  
  ## MAPS (from original sidebar) ------------------------------------------
  output$global_map <- renderPlot({
    req(input$selected_SUR)
    p1 <- state_geo_reactive() |>
      mutate(selected_sur = factor(case_when(country == input$selected_SUR ~ input$selected_SUR,
                                             region_dashboard == input$selected_region ~ "Region",
                                             .default = "Other"),
                                   levels = c(input$selected_SUR,"Region", "Other"))) |>
      ggplot(aes(geometry = polygon, color = selected_sur, fill = selected_sur, lwd = selected_sur)) +
      geom_sf() +
      scale_color_manual(values = c("tomato2", "grey60", "grey85")) +
      scale_linewidth_manual(values = c(0.8,0.3, 0.3)) +
      scale_fill_manual(values = c("tomato2","grey70", "grey95")) +
      scale_alpha_manual(values = c(1,1,0.3))+
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank()
      ) +
      labs(
        title = NULL,
        fill = NULL,
        color = NULL, lwd = NULL, alpha=NULL
      ) +
      guides(
        fill = "none", lwd = "none", color = "none", alpha="none"
      )
    
    if (sur_area() > 10^11) {
      p2 <- p1
    } else {
      p2 <- p1 + geom_rect(
        aes(
          xmin = bbox_selected_SUR()["xmin"] - 2,
          xmax = bbox_selected_SUR()["xmax"] + 2,
          ymin = bbox_selected_SUR()["ymin"] - 2,
          ymax = bbox_selected_SUR()["ymax"] + 2
        ),
        fill = "transparent",
        color = "tomato2",
        linewidth = 0.5
      )
    }
    
    p2 + theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
  })
  
  output$regional_map <- renderPlot({
    p1 <- state_geo_reactive() |>
      mutate(selected_region = factor(case_when(region_dashboard == input$selected_region ~ input$selected_region,
                                                .default = "Other"),
                                      levels = c(input$selected_region, "Other"))) |>
      ggplot(aes(geometry = polygon, color = selected_region, fill = selected_region, lwd = selected_region)) +
      geom_sf() +
      scale_color_manual(values = c("green4", "grey80")) +
      scale_linewidth_manual(values = c(0.8, 0.3)) +
      scale_fill_manual(values = c("green4", "grey90")) +
      theme_bw() +
      theme(
        plot.background = element_rect(fill="transparent", color = NA), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank()
      ) +
      labs(
        title = NULL,
        fill = NULL,
        color = NULL, lwd = NULL
      ) +
      guides(
        fill = "none", lwd = "none", color = "none"
      )
    p1 + theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
  })
  
  ## UPR: REGIONAL Outputs ----------------------------------------------------
  ### General plot ----------------
  output$global_plot <- renderPlot({
    req(nrow(filtered_upr_region()) > 0) # pause execution until filtered data is ready

    build_health_proportion_plot(
      filtered_upr_region(), by_state = FALSE,
      region_name = input$selected_region, lang = "en"
    ) +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 0.8,
                                   ,size = 12
        ),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 18),
        # plot.caption = element_text(size = 14),
        legend.position = "bottom",
        # legend.position = c(0, 1),
        # legend.justification = c("left", "top"),
        legend.text = element_text(size = 18),
        legend.background = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        plot.title = ggtext::element_textbox_simple(
          margin = margin(t = 5, b = 10, unit = "pt")
        )
      )
  })

  #### Plot downloaders -------------------
  output$download_global_plot <- proportion_png_download(
    filtered_upr_region, reactive(input$selected_region), by_state = FALSE,
    lang = "en", keep_strip = TRUE
  )
  output$download_global_plot_fr <- proportion_png_download(
    filtered_upr_region, reactive(input$selected_region), by_state = FALSE,
    lang = "fr", keep_strip = TRUE
  )

  ### Plotly ---------------------
  
  #### All  ---------------
  
  ##### Plot object -------------------
  plotly_UPR_regional_object <- reactive({
    req(nrow(filtered_upr_region()) > 0)
    filtered_upr_region() |>
      summarise_upr_themes() |>
      build_upr_themes_plot(total_n = nrow(filtered_upr_region()))
  })

  ##### Plot output --------------------
  output$plotly_UPR_regional <- renderPlotly({
    upr_ggplotly(plotly_UPR_regional_object(), input$selected_region)
  })

  ##### Plot downloader -------------------
  output$download_plotly_UPR_regional <- upr_png_download(
    plotly_UPR_regional_object,
    reactive(input$selected_region),
    expand_right = 0.2
  )

  # French version (for PNG download only)
  plotly_UPR_regional_object_fr <- reactive({
    req(nrow(filtered_upr_region()) > 0)
    filtered_upr_region() |>
      summarise_upr_themes() |>
      build_upr_themes_plot(total_n = nrow(filtered_upr_region()), lang = "fr")
  })
  output$download_plotly_UPR_regional_fr <- upr_png_download(
    plotly_UPR_regional_object_fr,
    reactive(input$selected_region),
    expand_right = 0.2,
    lang = "fr"
  )

  #### Cycles  ---------------
  
  ##### Plot object -------------------
  plotly_UPR_regional_cycle_object <- reactive({
    req(nrow(filtered_upr_region()) > 0)
    filtered_upr_region() |>
      summarise_upr_themes(by_cycle = TRUE) |>
      build_upr_cycle_plot()
  })

  ##### Plot output --------------------
  output$plotly_UPR_regional_cycle <- renderPlotly({
    upr_ggplotly(plotly_UPR_regional_cycle_object(), input$selected_region,
                 fix_strip_labels = TRUE, margins = list(l = 270))
  })
  
  #### Recommending States  ---------------
  
  ##### Plot object -------------------
  plotly_UPR_regional_recommending_object <- reactive({
    req(nrow(filtered_upr_region()) > 0)
    build_upr_recommending_plot(filtered_upr_region())
  })

  ##### Plot output --------------------
  output$plotly_UPR_regional_recommending <- renderPlotly({
    upr_ggplotly(plotly_UPR_regional_recommending_object(), input$selected_region,
                 margins = list(l = 0, r = 0, b = 0, t = 30))
  })

  #### Table -------------------
  output$plotly_table_regional <- DT::renderDT({
    upr_click_table(filtered_upr_region())
  })
  
  ## UPR: SUR Outputs --------------------------------------------------------
  
  ### Plotly ---------------------
  
  #### All  ---------------
  
  ##### Plot object -------------------
  plotly_UPR_SUR_object <- reactive({
    req(nrow(filtered_upr()) > 0)
    filtered_upr() |>
      summarise_upr_themes() |>
      build_upr_themes_plot(total_n = nrow(filtered_upr()))
  })

  ##### Plot output --------------------
  output$plotly_UPR_SUR <- renderPlotly({
    upr_ggplotly(plotly_UPR_SUR_object(), input$selected_SUR)
  })

  ##### Plot downloader -------------------
  output$download_plotly_UPR_SUR <- upr_png_download(
    plotly_UPR_SUR_object,
    reactive(input$selected_SUR),
    expand_right = 0.25
  )

  # French version (for PNG download only)
  plotly_UPR_SUR_object_fr <- reactive({
    req(nrow(filtered_upr()) > 0)
    filtered_upr() |>
      summarise_upr_themes() |>
      build_upr_themes_plot(total_n = nrow(filtered_upr()), lang = "fr")
  })
  output$download_plotly_UPR_SUR_fr <- upr_png_download(
    plotly_UPR_SUR_object_fr,
    reactive(input$selected_SUR),
    expand_right = 0.25,
    lang = "fr"
  )

  #### Cycles  ---------------
  
  ##### Plot object -------------------
  plotly_UPR_SUR_cycle_object <- reactive({
    req(nrow(filtered_upr()) > 0)
    filtered_upr() |>
      summarise_upr_themes(by_cycle = TRUE) |>
      build_upr_cycle_plot()
  })

  ##### Plot output --------------------
  output$plotly_UPR_SUR_cycle <- renderPlotly({
    upr_ggplotly(plotly_UPR_SUR_cycle_object(), input$selected_SUR,
                 fix_strip_labels = TRUE, margins = list(l = 270))
  })
  
  #### Recommending States  ---------------
  
  ##### Plot object -------------------
  plotly_UPR_SUR_recommending_object <- reactive({
    req(nrow(filtered_upr()) > 0)
    build_upr_recommending_plot(filtered_upr(),
                                relabel_iran = TRUE, fixed_aspect = TRUE)
  })

  ##### Plot output --------------------
  output$plotly_UPR_SUR_recommending <- renderPlotly({
    upr_ggplotly(plotly_UPR_SUR_recommending_object(), input$selected_SUR,
                 margins = list(l = 0, r = 0, b = 0, t = 30))
  })

  #### Table -------------------
  output$plotly_table_SUR <- DT::renderDT({
    upr_click_table(filtered_upr())
  })
  
  ### General plot --------------------------
  #### Plot object ----------------------------
  rec_plot_object <- reactive({
    req(nrow(filtered_upr()) > 0)
    build_health_proportion_plot(filtered_upr(), by_state = TRUE, lang = "en") +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 18),
        legend.position = "bottom",
        # legend.position = c(0.01, 0.99),
        # legend.justification = c("left", "top"),
        legend.text = element_text(size = 18),
        legend.background = element_blank(),
        # plot.title = ggtext::element_textbox_simple(
        #   margin = margin(t = 5, b = 10, r=0, l=0, unit = "pt")
        # ),
        plot.title= element_blank()
      )
  })

  #### Output --------------------
  output$plot <- renderPlot({
    rec_plot_object()
  })
  
  #### Plot downloader -------------------
  output$download_rec_plot_object <- downloadHandler(
    filename = function() {
      # Create a dynamic filename
      paste0("health-recommendations-", input$selected_SUR, ".png")
    },
    content = function(file) {
      # Use ggsave to save the reactive plot object to the temp file
      ggsave(
        file,
        plot = rec_plot_object()+
          labs(y="Recommendations (N)")+
          # geom_text(aes(label = sprintf("%1.0f", n_tot), y = n_tot, vjust = -0.2), size = 5, fontface = "bold", color = "white") +
          scale_fill_manual(values = c("Health-related" = "#ec5557", "Other" = "grey80"))+
          theme(
            panel.grid = element_blank(),
            axis.text.x = element_text(size = 12, color = "#1c164d"),
            axis.text.y = element_text(size = 12, color = "#1c164d"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14, color = "#1c164d"),
            # legend.position = "bottom",
            legend.position = c(0, 1),
            legend.justification = c("left", "top"), 
            legend.text = element_text(size = 11,colour = "#1c164d"),
            legend.key.size = unit(15,"pt"),
            # plot.background = element_blank(),
            plot.background = element_rect(color = "#1c164d", fill = NA),
            panel.border = element_rect(color = "#1c164d"),
            # panel.border = element_blank(),
            axis.ticks = element_line(color = "#1c164d"),
            panel.background = element_blank(),
            legend.background = element_blank(),
            strip.background = element_blank(),
            strip.text = element_blank(),
            plot.title = element_blank()
          )
        ,
        width = 5,
        height = 3.3,
        dpi = 300,
        # units = "in",
        bg="transparent"
      )
    }
  )

  # French version (for PNG download only)
  output$download_rec_plot_object_fr <- proportion_png_download(
    filtered_upr, reactive(input$selected_SUR), by_state = TRUE, lang = "fr",
    y_lab = "Recommandations (N)", keep_strip = FALSE
  )


  ### All cycles themes ----------------------------
  #### Plot object -------------------
  upr_themes_all_object <- reactive({
    req(nrow(filtered_upr()) > 0)
    a <- summarise_upr_themes(filtered_upr())

    p <- a |>
      ggplot(aes(x = perc, y = fct_rev(theme_label))) +
      geom_col(aes(fill = response_upr)) +
      labs(
        x = "Proportion of all recommendations (%)", y = NULL,
        fill = "State's response",
        title = paste("All health-related recommendations of the UPR\n", input$selected_SUR),
        caption = "*Numbers after the bars indicate N (% supported, where a response is available)"
      ) +
      theme_classic() +
      scale_x_continuous(
        labels = function(x) paste0(x, "%"),
        # limits = c(0, max_a + 2),
        expand = expansion(mult = c(0, 0.15))
      ) +
      coord_cartesian(clip = "off")+
      theme(
        plot.margin = margin(l=2,t=2,b=2, r = 30, unit = "pt"),
        legend.position = c(0.99, 0.01),
        legend.justification = c("right", "bottom"),
        legend.frame = element_rect(color = "black"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 11),
        legend.background = element_rect(fill = "transparent"),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_blank(),
        plot.title.position = "plot",
        panel.grid = element_blank()
      ) +
      geom_text(
        data = a |> filter(response_upr == "Supported"),
        aes(label = paste0(n_tot_theme, " ", n_sup), x = perc_theme),
        hjust = -0.15, size = 3, vjust = 0.25, color = "#1c164d"
      )
    p
  })
  
  ### Data table ---------------------------
  
  #### Table object ----------------
  DT_table_object <- reactive({
    req(nrow(filtered_upr()) > 0)
    theme_labels_test <- theme_labels |> 
      filter(!variable %in% c("TB_malaria_NTD"))
    
    table_upr <- 
      # filtered_upr() |>
      sdg_data_dashboard() |>
      # filter(state_under_review == input$selected_SUR)
      mutate(state_under_review = factor(state_under_review)) |>
      select(
        # text, cycle, response_upr, health_related:other_health_related,
        # state_under_review,recommending_state_upr_comma, document_code, paragraph
        
        text_2, cycle, response_upr,
        any_of(theme_labels_test$variable), 
        state_under_review, recommending_state_upr_comma, 
        document_code, paragraph #, affected_persons, themes
      ) |>
      rename(
        `Recommending State(s)` = recommending_state_upr_comma,
        `State under Review` = state_under_review,
        Document  = document_code,
        Paragraph = paragraph,
        Recommendation = text_2, 
        Cycle = cycle, 
        `State's response` = response_upr
      )
    
    rename_map <- setNames(theme_labels_test$variable, theme_labels_test$theme_label)
    table_upr |>
      dplyr::rename(any_of(rename_map))
    
    # table_upr |> 
    #   DT::datatable(
    #     # extensions = "Responsive",
    #     filter = "top",
    #     options = list(
    #       pageLength = 100,
    #       deferRender = TRUE,
    #       scrollY = 800,
    #       scrollX = TRUE,
    #       scroller = TRUE,
    #       autoWidth = TRUE,
    #       columnDefs = list(
    #         list(width = '500px', targets = c(0))
    #         # list(width = '200px', targets = c(1))
    #       )
    #     ),
    #     rownames = FALSE,
    #     class = 'cell-border stripe hover compact'
    #   )
  })
  
  
  #### Render table ---------------------
  output$DT_table <- DT::renderDT({
    req(nrow(filtered_upr()) > 0)
    DT_table_object() |> 
      filter(`State under Review` == input$selected_SUR) |> 
      DT::datatable(
        # extensions = "Responsive",
        filter = "top",
        options = list(
          pageLength = 100,
          deferRender = TRUE,
          scrollY = 800,
          scrollX = TRUE,
          scroller = TRUE,
          autoWidth = TRUE,
          columnDefs = list(
            list(width = '350px', targets = c(0)),
            list(width = '150px', targets = c(5,8,10)),
            list(width = '100px', targets = c(4,17,26))
            , list(width = '150px', targets = c(21))
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover compact'
      )
  })
  
  #### Table downloader -----------------------------
  ##### CSV --------------------
  output$download_data_csv <- downloadHandler(
    filename = function() {
      # Create a dynamic filename
      paste0("UPR-recommendations-", input$selected_SUR, ".csv")
    },
    content = function(file) {
      # Use ggsave to save the reactive plot object to the temp file
      write_csv(DT_table_object() |> filter(`State under Review` == input$selected_SUR), file)
    }
  )
  
  ##### CSV - ALL --------------------
  output$download_data_csv_all <- downloadHandler(
    filename = function() {
      # Create a dynamic filename
      paste0("UPR-recommendations-all", ".csv")
    },
    content = function(file) {
      # Use ggsave to save the reactive plot object to the temp file
      write_csv(DT_table_object(), file)
    }
  )
  
  ##### XLSX ---------------------
  output$download_data_xlsx <- downloadHandler(
    filename = function() {
      # Create a dynamic filename
      paste0("UPR-recommendations-", input$selected_SUR, ".xlsx")
    },
    content = function(file) {
      withProgress(message = 'Generating xlsx file', value = 0, {
        
        # incProgress(0.1, detail = "Formatting....")
        
        wb <- createWorkbook()
        addWorksheet(wb, "recommendations")
        writeDataTable(wb, "recommendations", 
                       DT_table_object() |> filter(`State under Review` == input$selected_SUR), 
                       tableStyle = "TableStyleMedium15")
        
        setColWidths(
          wb,
          sheet = "recommendations",
          cols = 1,      # Target the first column
          widths = 70    # Set its width
        )
        setColWidths(
          wb,
          sheet = "recommendations",
          cols = 2:(ncol(DT_table_object())-4),      #rest of the columns
          widths = 12    # Set its width
        )
        setColWidths(
          wb,
          sheet = "recommendations",
          cols = c(5,6,9,11,18,22),      #rest of the columns
          widths = 20    # Set its width
        )
        setColWidths(
          wb,
          sheet = "recommendations",
          cols = ncol(DT_table_object())-2,      #rest of the columns
          widths = 20    # Set its width
        )
        
        wrap_style <- createStyle(wrapText = TRUE)
        addStyle(
          wb,
          sheet = "recommendations",
          style = wrap_style,
          rows = 1:(nrow(DT_table_object()) + 1), # all rows
          cols = 1:ncol(DT_table_object()),          # all columns
          gridExpand = TRUE                 # Ensure style is applied to all specified cells
        )
        
        freezePane(
          wb,
          sheet = "recommendations",
          firstActiveRow = 2, # The second row is the first one that moves
          firstActiveCol = 2  # The second column is the first one that moves
        )
        
        incProgress(0.7, detail = "--> Saving output (this may take a moment)")
        saveWorkbook(wb, file)
        
        incProgress(1, detail = "Done!")
      })
    }
  )
  
  ## UHC Outputs ----------------------------------------------------------
  ### UHC Index -------------------
  
  #### Map - interactive ---------------------
  output$UHC_map_interactive <- renderLeaflet({
    
    UHC_dat <- UHC_all |>
      filter(YEAR == 2023) |> 
      filter(SpatialDimType == "COUNTRY") |> 
      filter(IndicatorCode == "UHC_INDEX_REPORTED") |> 
      right_join(state_geo_reactive(), by = c("COUNTRY" = "iso3")) |>
      mutate(selected_sur = case_when(country == input$selected_SUR ~ TRUE, 
                                      .default = FALSE)) |> 
      st_as_sf() |> 
      st_set_geometry("polygon")
    
    pal <- colorNumeric(
      palette = "RdYlBu"
      , domain = NULL
    )
    
    hover_labels <- sprintf(
      "<strong>%s</strong><br/>%s",
      UHC_dat$country,
      format(UHC_dat$Value, big.mark = ",", scientific = FALSE)
    ) %>% lapply(htmltools::HTML)
    
    leaflet_function(data =  UHC_dat, pal_object = pal, hover_labels = hover_labels, 
                     legend_title = "%",
                     coord_selected_SUR = coord_selected_SUR(),
                     zoom_level = m_zoom(),
                     fill_outcome =  "NumericValue")
    
  })
  
  ### RMNCH sub-index ---------------
  
  #### Map - interactive ---------------------
  output$UHC_RMNCH_map_interactive <- renderLeaflet({
    
    UHC_RMNCH_dat <- UHC_all |>
      filter(YEAR == 2023) |> 
      filter(SpatialDimType == "COUNTRY") |> 
      filter(IndicatorCode == "UHC_SCI_RMNCH") |> 
      right_join(state_geo_reactive(), by = c("COUNTRY" = "iso3")) |>
      mutate(selected_sur = case_when(country == input$selected_SUR ~ TRUE, 
                                      .default = FALSE)) |> 
      st_as_sf() |> 
      st_set_geometry("polygon")
    
    pal <- colorNumeric(
      palette = "RdYlBu"
      , domain = NULL
    )
    
    hover_labels <- sprintf(
      "<strong>%s</strong><br/>%s",
      UHC_RMNCH_dat$country,
      format(UHC_RMNCH_dat$Value, big.mark = ",", scientific = FALSE)
    ) %>% lapply(htmltools::HTML)
    
    leaflet_function(data =  UHC_RMNCH_dat, pal_object = pal, hover_labels = hover_labels, 
                     legend_title = "%",
                     coord_selected_SUR = coord_selected_SUR(),
                     zoom_level = m_zoom(),
                     fill_outcome =  "NumericValue")
    
  })
  
  ### UHC trend ---------------
  
  output$UHC_trend <- renderPlot({
    # Set a default number of columns
    num_cols <- 3
    # If the plot width is available and less than 600px (i.e., a phone screen),
    # switch to a single column.
    if (!is.null(plot_width_UHC_neighbors()) && plot_width_UHC_neighbors() < 300) {
      num_cols <- 2
    }
    start_year <- "2005"
    UHC_all |> 
      filter(IndicatorCode %in% c("UHC_SCI_RMNCH",
                                  "UHC_INDEX_REPORTED")) |> 
      mutate(selected_sur = factor(case_when(
        country_name == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      )) |>
      filter(country_name %in% c(
        input$selected_SUR,
        nearest_neighbors_list[, input$selected_SUR][1:5]
      )) |>
      filter(year >= ymd(paste0(start_year, "-01-01"))) |>
      mutate(country_name = str_wrap(country_name, 19)) |> 
      mutate(country_name = fct_relevel(country_name, str_wrap(input$selected_SUR,19))) |>
      mutate(IndicatorName = str_wrap(IndicatorName,40))|> 
      ggplot(aes(x=YEAR, y = NumericValue, color = IndicatorName, shape = IndicatorName))+
      geom_point(size = 3)+
      geom_line(linewidth=1.5)+
      labs(y = "Index value",
           x = NULL,
           title = "UHC Service Coverage", color = NULL, shape = NULL)+
      # facet_wrap(.~country_name, ncol=num_cols)+
      ggh4x::facet_wrap2(~country_name, ncol = num_cols
                         ,strip = ggh4x::strip_themed(
                           text_x = list(element_text(color="white", face = "bold"),
                                         NULL, NULL, NULL, NULL, NULL),
                           background_x = list(element_rect(fill = "grey30"), 
                                               NULL, NULL, NULL, NULL, NULL)
                         )
      )+
      theme_bw()+
      scale_x_continuous(breaks = c(2005,2010, 2015, 2020))+
      scale_y_continuous(limits = c(0,100), expand = c(0,0))+
      theme(
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 11),
        legend.background = element_rect(fill = "transparent"),
        legend.position = "bottom",
        strip.text = element_text(size = 12),
        axis.text = element_text(angle=30, size = 10, hjust=0.5),
        axis.title = element_text(size = 12),
        panel.grid.minor.y = element_blank()
      )+
      # scale_y_continuous(sec.axis = dup_axis(name = NULL))+
      guides(color=guide_legend(nrow=2,byrow=TRUE))
  })
  
  ## Cervical health outputs --------------------------------

  ### Cervical cancer mortality rate map -------
  
  output$ccmortality_map_interactive <- renderLeaflet({
    
    ccmr_data <- cervical_cancer_deaths |> 
      filter(year==2023) |> 
      right_join(state_geo_reactive(), by=join_by(location_name==country)) |> 
      mutate(selected_sur = case_when(location_name == input$selected_SUR ~ TRUE, .default = FALSE)) |> 
      st_as_sf() |> 
      st_set_geometry("polygon")
    
    pal <- colorNumeric(
      palette = "YlOrRd"
      , domain = NULL
    )
    
    hover_labels <- sprintf(
      "<strong>%s</strong><br/>Cervical cancer <br/> mortality rate in 2023: %s",
      ccmr_data$location_name,
      sprintf("%.2f", ccmr_data$val) 
    ) %>% lapply(htmltools::HTML)
    
    leaflet_function(data =  ccmr_data, pal_object = pal, hover_labels = hover_labels, 
                     legend_title = "",
                     coord_selected_SUR = coord_selected_SUR(),
                     zoom_level = m_zoom(),
                     fill_outcome =  "val")
    
  })
  
  ### Cervical cancer mortality trend (single country + global) -------
  
  ccmr_trend_object <- reactive({
    req(input$selected_SUR)
    
    ccmr_hist_plot <- cervical_cancer_deaths |>
      filter(location_name %in% c(input$selected_SUR, "Global")) |>
      mutate(series = factor(
        case_when(location_name == "Global" ~ "Global average",
                  .default = input$selected_SUR),
        levels = c(input$selected_SUR, "Global average")
      ))
    
    # Vaccine introduction year for the selected country, if known
    intro_year <- HPV_national_alt |>
      filter(ISO_3_CODE == state_geo_reactive() |>
               filter(country == input$selected_SUR) |> pull(iso3)) |>
      pull(year_intr)
    
    intro_year <- if (length(intro_year) == 0 || is.infinite(intro_year)) NA else intro_year
    
    p <- ccmr_hist_plot |>
      ggplot(aes(x = year, y = val, color = series, fill = series, group = series,
                 text = paste0(series, " - ", year, ": ", sprintf("%.2f", val)))) +
      # geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.15) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = c("tomato3", "grey40")) +
      # scale_fill_manual(values = c("tomato3", "grey40")) +
      labs(
        x = NULL,
        y = "Cervical cancer mortality rate\n(age-standardized, per 100,000 women)",
        color = NULL, fill = NULL,
        title = paste0("Trends in cervical cancer mortality — ", input$selected_SUR)
      ) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = ggtext::element_textbox_simple(margin = margin(t = 5, b = 10, unit = "pt"))
      )
    
    if (!is.na(intro_year)) {
      p <- p +
        geom_vline(aes(xintercept = intro_year, linetype = "Vaccine introduced"),
                   color = "steelblue4", linewidth = 0.7) +
        scale_linetype_manual(name = NULL, values = c("Vaccine introduced" = "dashed"))
    }
    
    p
  })
  
  output$ccmr_trend <- renderPlotly({
    fig <- ggplotly(ccmr_trend_object(), tooltip = "text") |>
      plotly::layout(
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25,
                      bgcolor = 'rgba(0,0,0,0)', bordercolor = 'rgba(0,0,0,0)'),
        margin = list(l = 0, r = 20, b = 40, t = 40)
      )
    
    fig$x$data <- lapply(fig$x$data, function(trace) {
      if (!is.null(trace$name)) {
        trace$name <- trace$name |>
          gsub("^\\(|\\)$", "", x = _) |>
          strsplit(",") |>
          sapply(`[`, 1) |>
          trimws()
      }
      trace
    })
    
    fig
  })
  
  ###  HPV Vaccination coverage Interactive Map -------------------
  output$hpv_coverage_map_interactive <- renderLeaflet({
    
    hpv_dat <- HPV_coverage_alt_wide |>
      group_by(iso3) |>
      slice_max(order_by = year, n = 1, with_ties = FALSE) |>
      ungroup() |>
      left_join(HPV_national_alt |> select(ISO_3_CODE, INTRO_text),
                by = c("iso3" = "ISO_3_CODE")) |>
      right_join(state_geo_reactive(), by = c("iso3" = "iso3")) |>
      mutate(selected_sur = case_when(country == input$selected_SUR ~ TRUE, .default = FALSE)) |>
      st_as_sf() |>
      st_set_geometry("polygon")
    
    pal <- colorNumeric(palette = "RdYlBu", domain = NULL)
    
    hover_labels <- sprintf(
      "<strong>%s</strong><br/>Fully vaccinated: %s<br/>At least 1 dose: %s<br/><span style='font-size:12px;color:#555;'>%s</span>",
      hpv_dat$country,
      ifelse(is.na(hpv_dat$`Full recommended schedule`), 
             "No data", 
             sprintf("%.0f%%", hpv_dat$`Full recommended schedule`)),
      ifelse(is.na(hpv_dat$`First dose`), 
             "No data", 
             sprintf("%.0f%%", hpv_dat$`First dose`)),
      ifelse(!is.na(hpv_dat$INTRO_text),
             paste0("Vaccine introduced: ", hpv_dat$INTRO_text),
             "Introduction status unknown")
    ) %>% lapply(htmltools::HTML)
    
    leaflet_function(data = hpv_dat, pal_object = pal, hover_labels = hover_labels,
                     legend_title = "%",
                     coord_selected_SUR = coord_selected_SUR(),
                     zoom_level = m_zoom(),
                     fill_outcome = "Full recommended schedule")
  })
  

  

  ### HPV Screening coverage Interactive Map -------------------
  output$screening_coverage_map_interactive <- renderLeaflet({
    
    screening_dat <- HPV_screening |>
      filter(Dim3 == input$screening_period) |>
      right_join(state_geo_reactive(), by = c("COUNTRY" = "iso3")) |>
      mutate(selected_sur = case_when(country == input$selected_SUR ~ TRUE, .default = FALSE)) |>
      st_as_sf() |>
      st_set_geometry("polygon")
    
    pal <- colorNumeric(palette = "RdYlBu", domain = NULL)
    
    hover_labels <- sprintf(
      "<strong>%s</strong><br/>Screening coverage (2019): %s%%",
      screening_dat$country,
      format(screening_dat$Value, big.mark = ",", scientific = FALSE)
    ) %>% lapply(htmltools::HTML)
    
    leaflet_function(data = screening_dat, pal_object = pal, hover_labels = hover_labels,
                     legend_title = "%",
                     coord_selected_SUR = coord_selected_SUR(),
                     zoom_level = m_zoom(),
                     fill_outcome = "NumericValue")
  })
  ## Maternal health outputs --------------------------------
  
  ### MMR Outputs -------------------------------------------------------------
  #### Map - interactive ---------------------
  output$mmr_map_interactive <- renderLeaflet({
    
    mmr_data <- MMR |> 
      filter(YEAR==2023) |> 
      right_join(state_geo_reactive(), by=join_by(COUNTRY==iso3)) |> 
      mutate(selected_sur = case_when(country == input$selected_SUR ~ TRUE, .default = FALSE)) |> 
      st_as_sf() |> 
      st_set_geometry("polygon")
    
    pal <- colorFactor(
      palette = "YlOrRd"
      , domain = NULL
    )
    
    hover_labels <- sprintf(
      "<strong>%s</strong><br/>MMR in 2023: %s",
      mmr_data$country,
      format(mmr_data$Value, big.mark = ",", scientific = FALSE)
    ) %>% lapply(htmltools::HTML)
    
    leaflet_function(data =  mmr_data, pal_object = pal, hover_labels = hover_labels, 
                     legend_title = "MMR",
                     coord_selected_SUR = coord_selected_SUR(),
                     zoom_level = m_zoom(),
                     fill_outcome =  "mmr_cat")
    
  })
  
  #### Neighbors comparison over time -------------------------
  output$mmr_time_plot_neighbors <- renderPlot({
    
    # Set a default number of columns
    num_cols <- 3
    # If the plot width is available and less than 600px (i.e., a phone screen),
    # switch to a single column.
    if (!is.null(plot_width_mmr_neighbors()) && plot_width_mmr_neighbors() < 300) {
      num_cols <- 2
    }
    start_year <- "2005"
    dat_plot <- MMR |>
      mutate(selected_sur = factor(case_when(
        country_name == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      )) |>
      filter(country_name %in% c(
        input$selected_SUR,
        nearest_neighbors_list[, input$selected_SUR][1:5]
      )) |>
      filter(year >= ymd(paste0(start_year, "-01-01"))) |>
      mutate(country_name = fct_relevel(country_name, input$selected_SUR)) |>
      group_by(country_name) |>
      mutate(
        num_stand = NumericValue - NumericValue[YEAR == start_year],
        num_low = Low - NumericValue[YEAR == start_year],
        num_high = High - NumericValue[YEAR == start_year]
      ) |>
      ungroup()
    
    hline_data <- dat_plot |>
      filter(YEAR == as.numeric(start_year))
    
    dat_plot |>
      mutate(country_name = fct_relevel(country_name, input$selected_SUR)) |>
      ggplot(aes(x = YEAR, y = NumericValue)) +
      labs(
        title = paste0("Trends in Maternal Mortality Ratio (MMR)"),
        x = NULL, y = "MMR estimate (per 100,000 live births)",
        color = NULL,
        fill = NULL
      ) +
      scale_x_continuous(breaks = c(2005, 2020))+
      geom_line(lwd = 1, aes(color = selected_sur)) +
      geom_ribbon(aes(ymin = Low, ymax = High, fill = selected_sur), color = NA, alpha = 0.4) +
      scale_color_manual(values = c("tomato3", "grey30")) +
      scale_fill_manual(values = c("tomato3", "grey30")) +
      guides(color = "none", lwd = "none", fill = "none") +
      facet_wrap(. ~ country_name, ncol=num_cols) +
      geom_hline(data = hline_data, aes(yintercept = NumericValue), lty = 2) +
      theme_bw()
  })
  
  #### Causes --------------------------------
  ##### 2023 -------------------------
  output$mmr_causes <- renderPlot({
    maternal_disorders_deaths |>
      filter(country %in% c("Global", input$selected_SUR)) |>
      filter(!cause_name %in% c("Maternal disorders")) |>
      filter(age_name %in% c("Age-standardized")) |>
      mutate(country = fct_relevel(country, "Global")) |>
      arrange(country, val) |>
      mutate(cause_name = fct_inorder(str_wrap(cause_name,30))) |>
      droplevels() |>
      ggplot(aes(y = cause_name, x = val, fill = country)) +
      scale_fill_manual(values = c("grey40", "tomato3")) +
      geom_col(position = position_dodge()) +
      geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.5, position = position_dodge(width = 0.9)) +
      labs(
        y = "Maternal cause of death", x = "Age-standardized rate, 2023\n(per 100,000)", fill = NULL
        # , title = "Distribution of causes of maternal deaths (2021)"
      ) +
      scale_x_continuous(
        expand = expansion(mult = c(0, 0.05))
      ) +
      theme_classic() +
      theme(
        panel.grid.major.y = element_line(color = "grey"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position = c(0.99, 0.01),
        legend.justification = c("right", "bottom"),
        legend.key.size = unit(0.6, "cm"),
        legend.text = element_text(size = 16)
      )
  })
  
  ##### Over time --------------------------
  output$mmr_causes_longitudinal <- renderPlot({
    maternal_disorders_deaths_longitudinal |>
      filter(country %in% c("Global", input$selected_SUR)) |>
      mutate(country = fct_relevel(country, "Global")) |>
      filter(cause_name != "Maternal disorders") |>
      arrange(country, -year, -val) |>
      mutate(cause_name = fct_inorder(cause_name)) |>
      droplevels() |>
      ggplot(aes(x = year, y = val, color = country)) +
      scale_linewidth_binned(n.breaks = 8) +
      scale_color_manual(values = c("grey40", "tomato3")) +
      geom_line(
        aes(lwd = val)
      ) +
      facet_grid(
        rows = vars(cause_name),
        switch = "y", scales = "free",
        labeller = labeller(cause_name = label_wrap_gen(30))
      ) +
      labs(
        x = NULL, y = NULL,
        lwd = "Rate\n(per 100,000)", color = NULL,
        title = "Longitudinal trends in the causes of maternal deaths\n(Caution: y-axes are variable)"
      ) +
      theme_bw() +
      scale_x_continuous(breaks = c(2005, 2020))+
      theme(
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, vjust = 1, size = 11),
        strip.background = element_rect(fill = NA, linewidth = 1, color = "black", linetype = 1),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  })
  
  ### Skilled birth outputs --------------------------------------------
  
  #### Skilled birth attendance ----------------
  ##### Map - interactive ---------------------
  output$skilled_birth_map_interactive <- renderLeaflet({
    
    skilled_birth_dat <- skilled_birth |>
      filter(!is.na(COUNTRY)) |>
      group_by(COUNTRY) |>
      slice_max(order_by = year, n = 1) |>
      ungroup() |>
      right_join(state_geo_reactive(), by=join_by(COUNTRY==iso3)) |> 
      mutate(selected_sur = case_when(country == input$selected_SUR ~ TRUE, .default = FALSE)) |> 
      st_as_sf() |> 
      st_set_geometry("polygon")
    
    pal <- colorNumeric(
      palette = "RdYlBu"
      # , reverse = TRUE
      , domain = NULL
    )
    
    hover_labels <- sprintf(
      "<strong>%s</strong><br/>%s&#37; in %s",
      skilled_birth_dat$country,
      format(skilled_birth_dat$Value, big.mark = ",", scientific = FALSE),
      skilled_birth_dat$YEAR
    ) %>% lapply(htmltools::HTML)
    
    leaflet_function(data =  skilled_birth_dat, pal_object = pal, hover_labels = hover_labels, 
                     legend_title = "%",
                     coord_selected_SUR = coord_selected_SUR(),
                     zoom_level = m_zoom(),
                     fill_outcome =  "NumericValue")
    
  })
  
  
  
  ##### Neighbors --------------
  output$skilled_birth_plot_neighbors <- renderPlot({
    
    # Set a default number of columns
    num_cols <- 3
    # If the plot width is available and less than 600px (i.e., a phone screen),
    # switch to a single column.
    if (!is.null(plot_width_skilled_birth_neighbors()) && plot_width_skilled_birth_neighbors() < 300) {
      num_cols <- 2
    }
    start_year <- "2005"
    dat_plot <- skilled_birth |>
      mutate(selected_sur = factor(case_when(
        country_name == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      )) |>
      filter(country_name %in% c(
        input$selected_SUR,
        nearest_neighbors_list[, input$selected_SUR][1:5]
      )) |>
      # filter(year >= ymd(paste0(start_year, "-01-01"))) |>
      mutate(country_name = fct_relevel(country_name, input$selected_SUR))
    
    hline_data <- dat_plot |>
      group_by(country_name) |> 
      filter(YEAR == min(YEAR)) |> ungroup()
    
    dat_plot |>
      mutate(country_name = fct_relevel(country_name, input$selected_SUR)) |>
      ggplot(aes(x = YEAR, y = NumericValue)) +
      labs(
        title = paste0("Trends in births attended by skilled health personnel"),
        x = NULL, y = "Births attended by skilled health personnel (%)",
        color = NULL,
        fill = NULL
      ) +
      geom_line(lwd = 1.5, aes(color = selected_sur)) +
      geom_point(aes(color=selected_sur), size=2.5)+
      scale_color_manual(values = c("tomato3", "grey30")) +
      scale_fill_manual(values = c("tomato3", "grey30")) +
      scale_x_continuous(breaks = c(2005, 2020))+
      guides(color = "none", lwd = "none", fill = "none") +
      facet_wrap(. ~ country_name, ncol=num_cols) +
      geom_hline(data = hline_data, aes(yintercept = NumericValue), lty = 2) +
      theme_bw()+
      theme(
        panel.grid.minor.y = element_blank(),
        plot.title = ggtext::element_textbox_simple(
          margin = margin(t = 5, b = 10, unit = "pt")
        )
      )
  })
  
  #### Proportion of births delivered in a health facility ----------------
  ##### Map - interactive ---------------------
  output$institutional_birth_map_interactive <- renderLeaflet({
    
    institutional_birth_dat <- institutional_birth |>
      filter(!is.na(COUNTRY)) |>
      group_by(COUNTRY) |>
      slice_max(order_by = year, n = 1) |>
      ungroup() |>
      right_join(state_geo_reactive(), by=join_by(COUNTRY==iso3)) |> 
      mutate(selected_sur = case_when(country == input$selected_SUR ~ TRUE, .default = FALSE)) |> 
      st_as_sf() |> 
      st_set_geometry("polygon")
    
    pal <- colorNumeric(
      palette = "RdYlBu"
      # , reverse = TRUE
      , domain = NULL
    )
    
    hover_labels <- sprintf(
      "<strong>%s</strong><br/>%s&#37; in %s",
      institutional_birth_dat$country,
      format(institutional_birth_dat$Value, big.mark = ",", scientific = FALSE),
      institutional_birth_dat$YEAR
    ) %>% lapply(htmltools::HTML)
    
    leaflet_function(data =  institutional_birth_dat, pal_object = pal, hover_labels = hover_labels, 
                     legend_title = "%",
                     coord_selected_SUR = coord_selected_SUR(),
                     zoom_level = m_zoom(),
                     fill_outcome =  "NumericValue")
    
  })
  
  ##### Neighbors --------------
  output$births_facility_plot_neighbors <- renderPlot({
    
    # Set a default number of columns
    num_cols <- 3
    # If the plot width is available and less than 600px (i.e., a phone screen),
    # switch to a single column.
    if (!is.null(plot_width_births_facility_neighbors()) && plot_width_births_facility_neighbors() < 300) {
      num_cols <- 2
    }
    start_year <- "2005"
    dat_plot <- institutional_birth |>
      mutate(selected_sur = factor(case_when(
        country_name == input$selected_SUR ~ input$selected_SUR,
        .default = "Other"
      ),
      levels = c(input$selected_SUR, "Other")
      )) |>
      filter(country_name %in% c(
        input$selected_SUR,
        nearest_neighbors_list[, input$selected_SUR][1:5]
      )) |>
      # filter(year >= ymd(paste0(start_year, "-01-01"))) |>
      mutate(country_name = fct_relevel(country_name, input$selected_SUR))
    
    hline_data <- dat_plot |>
      group_by(country_name) |> 
      filter(YEAR == min(YEAR)) |> ungroup()
    
    dat_plot |>
      mutate(country_name = fct_relevel(country_name, input$selected_SUR)) |>
      ggplot(aes(x = YEAR, y = NumericValue)) +
      labs(
        title = paste0("Trends in births delivered in a health facility"),
        x = NULL, y = "Births delivered in a health facility (%)",
        color = NULL,
        fill = NULL
      ) +
      geom_line(lwd = 1.5, aes(color = selected_sur)) +
      geom_point(aes(color=selected_sur), size=2.5)+
      scale_color_manual(values = c("tomato3", "grey30")) +
      scale_fill_manual(values = c("tomato3", "grey30")) +
      scale_x_continuous(breaks = c(2005, 2020))+
      guides(color = "none", lwd = "none", fill = "none") +
      facet_wrap(. ~ country_name, ncol=num_cols) +
      geom_hline(data = hline_data, aes(yintercept = NumericValue), lty = 2) +
      theme_bw()+
      theme(
        panel.grid.minor.y = element_blank(),
        plot.title = ggtext::element_textbox_simple(
          margin = margin(t = 5, b = 10, unit = "pt")
        )
      )
  })
  
  ### Abortion Outputs -------------------------------------------------
  #### Laws ----
  ##### Map - interactive ---------------------
  output$abortion_laws_map_interactive <- renderLeaflet({
    
    world_abortion_laws_data <- world_abortion_laws |>
      right_join(state_geo_reactive()) |>
      mutate(selected_sur = case_when(country == input$selected_SUR ~ TRUE, .default = FALSE)) |> 
      st_as_sf() |> 
      st_set_geometry("polygon")
    
    pal <- colorFactor(
      # palette = "YlOrRd"
      palette = c("chartreuse4", "cyan3", "gold", "chocolate1", "red3", "purple")
      , domain = NULL
    )
    
    hover_labels <- sprintf(
      "<strong>%s</strong><br/>%s",
      world_abortion_laws_data$country,
      world_abortion_laws_data$category
    ) %>% lapply(htmltools::HTML)
    
    leaflet_function(data =  world_abortion_laws_data, pal_object = pal, hover_labels = hover_labels, 
                     legend_title = NULL,
                     coord_selected_SUR = coord_selected_SUR(),
                     zoom_level = m_zoom(),
                     fill_outcome =  "category")
    
  })
  
  #### Rate ----
  ##### Map - interactive ---------------------
  output$abortion_rate_map_interactive <- renderLeaflet({
    
    abortion_rate_data <- abortion_rate |>
      filter(!is.na(COUNTRY)) |>
      filter(Dim1 == "UNCERTAINTY_INTERVAL_UI95") |>
      right_join(state_geo_reactive(), by=join_by(COUNTRY==iso3)) |> 
      mutate(selected_sur = case_when(country == input$selected_SUR ~ TRUE, .default = FALSE)) |> 
      st_as_sf() |> 
      st_set_geometry("polygon")
    
    pal <- colorNumeric(
      palette = "RdYlBu"
      , reverse = TRUE
      , domain = NULL
    )
    
    hover_labels <- sprintf(
      "<strong>%s</strong><br/>%s",
      abortion_rate_data$country,
      format(abortion_rate_data$Value, big.mark = ",", scientific = FALSE)
    ) %>% lapply(htmltools::HTML)
    
    leaflet_function(data =  abortion_rate_data, pal_object = pal, hover_labels = hover_labels, 
                     legend_title = NULL,
                     coord_selected_SUR = coord_selected_SUR(),
                     zoom_level = m_zoom(),
                     fill_outcome =  "NumericValue")
    
  })
  
  #### Unintended pregnancy ----
  ##### Map - interactive ---------------------
  output$unintended_pregnancy_map_interactive <- renderLeaflet({
    
    unintended_pregnancy_data <- unintended_pregnancy |>
      filter(!is.na(COUNTRY)) |>
      filter(Dim1 == "UNCERTAINTY_INTERVAL_UI95") |>
      right_join(state_geo_reactive(), by=join_by(COUNTRY==iso3)) |> 
      mutate(selected_sur = case_when(country == input$selected_SUR ~ TRUE, .default = FALSE)) |> 
      st_as_sf() |> 
      st_set_geometry("polygon")
    
    pal <- colorNumeric(
      palette = "RdYlBu"
      , reverse = TRUE
      , domain = NULL
    )
    
    hover_labels <- sprintf(
      "<strong>%s</strong><br/>%s",
      unintended_pregnancy_data$country,
      format(unintended_pregnancy_data$Value, big.mark = ",", scientific = FALSE)
    ) %>% lapply(htmltools::HTML)
    
    leaflet_function(data =  unintended_pregnancy_data, pal_object = pal, hover_labels = hover_labels, 
                     legend_title = NULL,
                     coord_selected_SUR = coord_selected_SUR(),
                     zoom_level = m_zoom(),
                     fill_outcome =  "NumericValue")
    
  })
  
  #### Abortion deaths ----
  ##### Map - interactive ---------------------
  output$abortion_deaths_map_interactive <- renderLeaflet({
    
    abortion_deaths_data <- maternal_disorders_deaths_longitudinal |> 
      filter(cause_name == "Maternal abortion and miscarriage") |> 
      rename(abortion_deaths = val) |> 
      group_by(country) |> slice_max(year) |> ungroup() |> 
      select(abortion_deaths, lower, upper, country) |> 
      mutate(country = case_match(
        country,
        "Turkey" ~ "T\u00fcrkiye",
        "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland",
        .default = country
      )) |> 
      right_join(state_geo_reactive(), by=join_by(country==country)) |> 
      mutate(selected_sur = case_when(country == input$selected_SUR ~ TRUE, .default = FALSE)) |> 
      mutate(
        abortion_lab = case_when(
          abortion_deaths < 0.001 ~ sprintf("%1.4f", abortion_deaths),
          abortion_deaths < 0.01 ~ sprintf("%1.3f", abortion_deaths),
          .default = sprintf("%1.2f", abortion_deaths)
        ),
        lower = case_when(
          lower < 0.001 ~ sprintf("%1.4f", lower),
          lower < 0.01 ~ sprintf("%1.3f", lower),
          .default = sprintf("%1.2f", lower)
        ),
        upper = case_when(
          upper < 0.001 ~ sprintf("%1.4f", upper),
          upper < 0.01 ~ sprintf("%1.3f", upper),
          .default = sprintf("%1.2f", upper)
        ),
        full_estimate = paste0(abortion_lab, " [", lower, "-",upper,"]"),
        abortion_deaths_cat = case_when(
          abortion_deaths < 0.01 ~ "<0.01",
          abortion_deaths < 0.1 ~ "0.01 - 0.09",
          abortion_deaths < 0.5 ~ "0.1 - 0.49",
          abortion_deaths < 1 ~ "0.5 - 0.99",
          abortion_deaths < 2 ~ "1 - 1.99",
          abortion_deaths >= 2 ~ "2+"
        )
      ) |> 
      st_as_sf() |> 
      st_set_geometry("polygon")
    
    # pal <- colorNumeric(
    #   palette = "RdYlBu"
    #   , reverse = TRUE
    #   , domain = NULL
    # )
    
    pal <- colorFactor(
      palette = "RdYlBu"
      , reverse = TRUE
      # palette = c("chartreuse4", "cyan3", "gold", "chocolate1", "red3", "purple")
      , domain = NULL
    )
    
    hover_labels <- sprintf(
      "<strong>%s</strong><br/>%s",
      abortion_deaths_data$country,
      abortion_deaths_data$full_estimate
    ) %>% lapply(htmltools::HTML)
    
    leaflet_function(data =  abortion_deaths_data, pal_object = pal, hover_labels = hover_labels, 
                     legend_title = NULL,
                     coord_selected_SUR = coord_selected_SUR(),
                     zoom_level = m_zoom(),
                     fill_outcome =  "abortion_deaths_cat")
    
  })
  
  ## Family planning outputs -------------------------------------
  
  #### Map - interactive ---------------------
  output$family_planning_map_interactive <- renderLeaflet({
    
    family_planning_dat <- family_planning |>
      filter(!is.na(COUNTRY)) |>
      group_by(COUNTRY) |>
      slice_max(order_by = year, n = 1) |>
      ungroup() |>
      right_join(state_geo_reactive(), by = c("COUNTRY" = "iso3")) |>
      mutate(selected_sur = case_when(country == input$selected_SUR ~ TRUE, 
                                      .default = FALSE)) |> 
      st_as_sf() |> 
      st_set_geometry("polygon")
    
    pal <- colorNumeric(
      palette = "RdYlBu"
      , domain = NULL
    )
    
    hover_labels <- sprintf(
      "<strong>%s</strong><br/>%s&#37; in %s",
      family_planning_dat$country,
      format(family_planning_dat$Value, big.mark = ",", scientific = FALSE),
      family_planning_dat$YEAR
    ) %>% lapply(htmltools::HTML)
    
    leaflet_function(data =  family_planning_dat, pal_object = pal, hover_labels = hover_labels, 
                     legend_title = "%",
                     coord_selected_SUR = coord_selected_SUR(),
                     zoom_level = m_zoom(),
                     fill_outcome =  "NumericValue")
    
  })
  
  ## Adolescent birth rate outputs -------------------------------------
  
  #### Map - interactive ---------------------
  output$adolescent_birth_map_interactive <- renderLeaflet({
    
    adolescent_birth_rate_dat <- adolescent_birth_rate |>
      filter(!is.na(COUNTRY)) |>
      filter(Dim2 == "AGEGROUP_YEARS15-19") |> 
      group_by(COUNTRY) |>
      slice_max(order_by = year, n = 1) |>
      ungroup() |>
      right_join(state_geo_reactive(), by = c("COUNTRY" = "iso3")) |>
      mutate(selected_sur = case_when(country == input$selected_SUR ~ TRUE, 
                                      .default = FALSE)) |> 
      st_as_sf() |> 
      st_set_geometry("polygon")
    
    pal <- colorNumeric(
      palette = "RdYlBu"
      , reverse = TRUE
      , domain = NULL
    )
    
    hover_labels <- sprintf(
      "<strong>%s</strong><br/>%s in %s",
      adolescent_birth_rate_dat$country,
      format(adolescent_birth_rate_dat$Value, big.mark = ",", scientific = FALSE),
      adolescent_birth_rate_dat$YEAR
    ) %>% lapply(htmltools::HTML)
    
    leaflet_function(data =  adolescent_birth_rate_dat, pal_object = pal, hover_labels = hover_labels, 
                     legend_title = NULL,
                     coord_selected_SUR = coord_selected_SUR(),
                     zoom_level = m_zoom(),
                     fill_outcome =  "NumericValue")
    
  })
  
  ## Constitutions ---------
  ### Right to health ----------
  
  #### Map - interactive ---------------------
  output$const_anyhealth_map_interactive <- renderLeaflet({
    
    constitution_data <- constitutions |> 
      select(-country) |> 
      right_join(state_geo_reactive(), by = c("iso3" = "iso3")) |>
      mutate(selected_sur = case_when(country == input$selected_SUR ~ TRUE, .default = FALSE)) |> 
      st_as_sf() |> 
      st_set_geometry("polygon")
    
    pal <- colorFactor(
      # palette = "YlOrRd"
      palette = c("#de5e33", "#de9133", "#e3d191", "#548555")
      , domain = NULL
    )
    
    hover_labels <- sprintf(
      "<strong>%s</strong><br/>%s",
      constitution_data$country,
      constitution_data$const_anyhealth
    ) %>% lapply(htmltools::HTML)
    
    leaflet_function(data =  constitution_data, pal_object = pal, hover_labels = hover_labels, 
                     legend_title = NULL,
                     coord_selected_SUR = coord_selected_SUR(),
                     zoom_level = m_zoom(),
                     fill_outcome =  "const_anyhealth")
    
  })
  
  
  ### ICESCR - Status ------
  
  #### Map - interactive -------
  
  output$ICESCR_status_map_interactive <- renderLeaflet({
    
    ICESCR_data <- ICESCR |> 
      rename(convent_status = status) |> # avoid confusion with "status" column from state_geo_reactive()
      select(-country) |> 
      right_join(state_geo_reactive(), by = c("iso3" = "iso3")) |>
      mutate(selected_sur = case_when(country == input$selected_SUR ~ TRUE, .default = FALSE)) |> 
      st_as_sf() |> 
      st_set_geometry("polygon")
    
    pal <- colorFactor(
      palette = c("#548555", "#e3d191", "#de5e33")
      , domain = NULL
    )
    
    hover_labels <- sprintf(
      "<strong>%s</strong><br/>%s",
      ICESCR_data$country,
      ICESCR_data$label_text
    ) %>% lapply(htmltools::HTML)
    
    leaflet_function(data =  ICESCR_data, pal_object = pal, hover_labels = hover_labels, 
                     legend_title = NULL,
                     coord_selected_SUR = coord_selected_SUR(),
                     zoom_level = m_zoom(),
                     fill_outcome =  "convent_status")
    
  })
  
}

# 4. RUN APP ==================================================================
shinyApp(ui, server)