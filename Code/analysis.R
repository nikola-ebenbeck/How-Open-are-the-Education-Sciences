# 1. Packages ------------------------------------------------------------------

library(tidyverse)
library(tidyr)
library(rcartocolor)
library(flowchart)
library(brms)
library(scales)
# 2. Data wrangling ------------------------------------------------------------

data <- read.csv("data/all_papers_data.csv") |>
  filter(publication_year >= 2020, publication_year <= 2025) |>
  select(
    -pdf_local_path,
    -tei_local_path,
    -pdf_download_status,
    -tei_process_status,
    -oa_urls,
    -doi,
    -journal_id,
    -journal_short,
    -journal_long,
    -osf_links_raw,
    -git_links_raw,
    -all_osf_links,
    -all_git_links
  )

write.csv2(data, 'data/all_papers_final.csv') # Here, more data is manually coded.

# 3. Manual coding of repository link content and methodology ------------------

data3 <- read.csv2("data/all_papers_manual_final.csv") |>
  filter(publication_year >= 2020, publication_year <= 2025) |>
  mutate(
    notes2 = as.character(notes2),
    has_working_link = has_any_link
  ) |>
  mutate(
    has_working_link = ifelse(
      !is.na(notes2) &
        str_detect(str_to_lower(str_trim(notes2)), "^(raus|not|no)"),
      FALSE,
      has_working_link
    ),
    has_working_git_link = has_git & has_working_link,
    has_working_osf_link = has_osf & has_working_link
  ) |>
  mutate(
    link_type = case_when(
      (str_trim(notes2) == 'raus_not_open' |
        str_trim(notes2) == "no_files" |
        str_trim(notes2) == 'raus_missing' |
        str_trim(notes2) == "not_open") ~ "not_open",
      str_trim(notes2) == 'raus_leer' ~ "empty",
      (str_trim(notes2) == 'raus_software' |
        str_trim(notes2) == 'raus_r_package' |
        str_trim(notes2) == 'raus_externe_software' |
        str_trim(notes2) == 'raus_demoversion') ~ "software",
      has_any_link ~ "working"
    ),
    link_type = factor(
      link_type,
      levels = c("working", "empty", "not_open", "software")
    )
  ) |>
  mutate(
    link_count_final = sum(has_working_link),
    link_count_git = sum(has_working_git_link),
    link_count_osf = sum(has_working_osf_link)
  ) |>
  mutate(
    study_type = case_when(
      notes2 == "review" ~ "review",
      method_quanti == 1 & method_quali == 0 ~ "quantitative",
      method_quanti == 0 & method_quali == 1 ~ "qualitative",
      method_quanti == 1 & method_quali == 1 ~ "mixed"
    )
  )

# 4. Overview of paper selection process ---------------------------------------
nrow(data3) # 15953
nrow(data3 |> filter(has_working_link)) # 223

# 4.1 Flowchart for canva numbers

journal_counts <- data3 |>
  dplyr::count(journal_name, name = "n") |>
  dplyr::mutate(
    pct = 100 * n / sum(n)
  ) |>
  dplyr::arrange(desc(n))

journal_counts

safo <- data3

excluded_counts <- safo |>
  filter(!has_working_link) |>
  filter(link_type != 'working') |>
  filter(link_type != 111 | !is.na(link_type)) |>
  count(link_type)

excluded_label <- paste0(
  "Papers assessed for eligibility:\n",
  paste0(
    excluded_counts$link_type,
    ": n = ",
    excluded_counts$n,
    collapse = "\n"
  )
)

Journal_label <- paste0(
  "Records identified from:\n",
  paste0(
    journal_counts$journal_name,
    ": n = ",
    journal_counts$n,
    collapse = "\n"
  )
)
safo |>
  mutate(group = ifelse(link_type == "working", "working", "other")) |>
  as_fc(label = Journal_label) |>
  fc_filter(
    has_any_link,
    label = "Records identified with repository link:",
    show_exc = TRUE,
    label_exc = "No repository link:"
  ) |>
  fc_filter(
    group == "working",
    label = "Papers included",
    show_exc = TRUE,
    label_exc = excluded_label
  ) |>
  fc_draw()


# 5. Manual coding of repository content type ----------------------------------

data3new <- read.csv2('data/data_3_new.csv') |> ## This file is the second manual coding (data_3_new.csv)
  filter(publication_year >= 2020, publication_year <= 2025)

nrow(data3new)

data4 <- data3 |>
  left_join(
    data3new |> select(openalex_id, study_type),
    by = "openalex_id",
    suffix = c("", ".fix")
  ) |>
  mutate(
    study_type = study_type.fix
  ) |>
  select(-study_type.fix)

open_cols <- c(
  "open_data",
  "open_code",
  "open_material",
  "open_analysis",
  "prerig"
)

data5 <- data4 |>
  mutate(
    across(all_of(open_cols), ~ ifelse(has_working_link == TRUE, ., 0))
  ) |>
  filter(has_working_link) |>
  select(
    openalex_id,
    study_type,
    open_data,
    open_code,
    open_material,
    open_analysis,
    prerig,
    link_count_final,
    link_count_git,
    link_count_osf
  ) |>
  mutate(
    across(
      c(open_data, open_code, open_material, open_analysis, prerig),
      ~ replace_na(.x, 0)
    ),
  ) |>
  pivot_longer(
    cols = c(open_data, open_code, open_material, open_analysis, prerig),
    names_to = "open_category",
    values_to = "available"
  )

data6 <- data5 |>
  group_by(study_type, open_category) |>
  mutate(available = as.numeric(na_if(available, "X"))) |>
  summarise(
    n_total = n(),
    n_available = sum(available, na.rm = TRUE),
    percent = 100 * n_available / n_total,
    link_count_final = first(link_count_final),
    .groups = "drop",
  ) |>
  mutate(
    Perc_total = 100 * n_total / link_count_final,
    open_category = factor(
      open_category,
      levels = c(
        "prerig",
        "open_code",
        "open_data",
        "open_material",
        "open_analysis"
      ),
      labels = c(
        "Preregistration",
        "Open code",
        "Open data",
        "Open material",
        "Open analysis"
      )
    ),
    study_type = factor(
      study_type,
      levels = c(
        "quantitative",
        "qualitative",
        "mixed",
        "review",
        "conceptional",
        "methodical"
      )
    )
  )

# 6. Plots ---------------------------------------------------------------------

save_plot <- function(
  plot,
  filename,
  dirs = c(
    "results/figures",
    "{network-drive}"
  ),
  formats = c("pdf", "png"),
  width = NULL,
  height = NULL,
  units = "cm",
  ...
) {
  for (d in dirs) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)

    for (ext in formats) {
      args <- list(
        filename = file.path(d, paste0(filename, ".", ext)),
        plot = plot,
        width = width,
        height = height,
        units = units,
        ...
      )

      if (ext == "png" && is.null(args$dpi)) {
        args$dpi <- 300
      }
      args <- Filter(Negate(is.null), args)
      do.call(ggplot2::ggsave, args)
    }
  }

  invisible(plot)
}

combined_df <- data4 |>
  group_by(journal_name, publication_year) |>
  summarise(
    total_papers = n_distinct(openalex_id),
    unique_linked_papers = sum(has_working_link, na.rm = TRUE),
    proportion_linked = unique_linked_papers / total_papers,
    .groups = "drop"
  ) %>%
  group_by(journal_name) %>%
  mutate(
    FigureName = str_glue("{journal_name} (N = {sum(total_papers)})")
  ) %>%
  ungroup() %>%
  mutate(publication_year_centered = publication_year - 2020) |>
  mutate(
    journal_name = case_when(
      journal_name ==
        "International Journal of Educational Technology in Higher Education" ~ "IJETHE",
      TRUE ~ journal_name
    )
  )

plot3a <- ggplot(
  data6,
  aes(y = reorder(study_type, -n_total), x = n_total)
) +
  geom_col(position = "dodge", alpha = 0.3) +
  geom_text(
    aes(label = paste0(Perc_total, " %")),
    position = position_dodge(width = 0.8),
    hjust = -0.2
  ) +
  theme_bw() +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.01)),
    limits = c(0, 205)
  ) +
  labs(x = "Article type", y = "N Articles")
plot3a

plot3b <- ggplot(
  data6 |> filter(!is.na(study_type)),
  aes(x = open_category, y = percent, fill = open_category, na.rm = TRUE)
) +
  geom_col(position = "dodge", color = "black", alpha = 0.8) +
  facet_wrap(~study_type) +
  theme_bw() +
  scale_fill_carto_d(name = "Open science practice", palette = "Safe") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(x = element_blank(), y = "Articles (%)")

plot3b
save_plot(plot3b, "plot3b", width = 20, height = 10)

plot3c <- ggplot(
  combined_df |> filter(journal_name != "SAGE Open"),
  aes(x = publication_year, y = unique_linked_papers)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5, show.legend = FALSE) +
  facet_wrap(~journal_name) +
  labs(
    x = "Publication Year",
    y = "Number of Papers with Link"
  ) +
  scale_x_continuous(
    breaks = seq(
      min(combined_df$publication_year, na.rm = TRUE),
      max(combined_df$publication_year, na.rm = TRUE),
      by = 2
    )
  ) +
  theme_bw() +
  scale_fill_carto_d(name = "Open science practice", palette = "Safe")

plot3c
save_plot(plot3c, 'plot3c', width = 20, height = 10)

control <- list(
  adapt_delta = 0.99,
  max_treedepth = 15
)

model <- brm(
  unique_linked_papers | trials(total_papers) ~
    publication_year_centered +
    (publication_year_centered | journal_name),
  data = combined_df,
  family = binomial(),
  iter = 6000,
  warmup = 3000,
  chains = 4,
  cores = 16,
  control = list(
    adapt_delta = 0.999,
    max_treedepth = 15
  )
)

summary(model)

## Plot 4 ----

plot4 <- ggplot(
  combined_df,
  aes(
    x = publication_year,
    y = unique_linked_papers,
    fill = str_wrap(FigureName, 20)
  )
) +
  geom_col(color = "white", linewidth = 0.2) +
  scale_y_continuous(
    name = "Anzahl verlinkter Paper",
    limits = c(
      0,
      max(
        aggregate(
          unique_linked_papers ~ publication_year,
          combined_df,
          sum
        )$unique_linked_papers
      ) *
        1.1
    )
  ) +
  scale_x_continuous(breaks = unique(combined_df$publication_year)) +
  labs(
    title = "Vergleich: Anzahl der Artikel mit Repositoriums-Links",
    subtitle = str_glue("2017-2025, N = {sum(combined_df$total_papers)}"),
    x = "Erscheinungsjahr",
    fill = "Journal"
  ) +
  scale_fill_carto_d(name = "Open science practice", palette = "Safe") +
  theme(
    legend.background = element_rect(fill = "white", colour = "black"),
    legend.position = c(0.125, 0.675),
    legend.box = "vertical",
    legend.direction = "vertical",
    panel.grid.minor = element_blank()
  )

plot4

# Plot 5 ----

plot5 <- ggplot(
  combined_df,
  aes(x = publication_year, y = proportion_linked, group = journal_name)
) +
  geom_line(aes(color = journal_name), alpha = 0.8, size = 2) +
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "line",
    color = "black",
    size = 3
  ) +
  scale_x_continuous(breaks = unique(combined_df$publication_year)) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(
    title = "Proportion of Linked Papers Per Year by Journal",
    x = "Publication Year",
    y = "Proportion Linked",
    color = "Journal"
  ) +
  scale_fill_carto_d(name = "Open science practice", palette = "Safe")

plot5

# 6. Country analysis ----------------------------------------------------------

country_data <- data4 |>
  filter(has_working_link) |>
  distinct(openalex_id, country) |>
  group_by(country) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(
    n_total = sum(n),
    perc = 100 * n / n_total
  ) |>
  arrange(desc(n))

print(country_data, n = 55)

# 7. Categories ----------------------------------------------------------------

## 7.1 Iterations of ChatGPT model 5.5 thinking ----

data_2 <- list.files(
  # Read all iterations
  "data/cat_runs",
  pattern = ".csv",
  full.names = TRUE
) |>
  set_names(basename) |>
  map(read.csv) |>
  list_rbind(names_to = "iteration") |>
  pivot_longer(cols = C1:C14, names_to = "Category", values_to = "Code") |>
  # Which categories were rated most?
  count(OpenAlex_ID, Category, wt = Code, name = "Rating") |>
  # Only >= 70% IRR
  filter(Rating > 6) |>
  right_join(data3new, by = c("OpenAlex_ID" = "openalex_id")) |>
  filter(!is.na(Category)) |>
  mutate(
    Category = recode(
      Category,
      "C1" = "EdTech & AI",
      "C2" = "STEM Education",
      "C3" = "Higher Education",
      "C4" = "Assessment",
      "C5" = "Learning Psychology",
      "C6" = "Teacher Education",
      "C7" = "Inclusion & Diversity",
      "C8" = "Digital Learning",
      "C9" = "Curriculum & Instruction",
      "C10" = "Open Science",
      "C11" = "Policy & Systems",
      "C12" = "Literacy & Language",
      "C13" = "Informal Learning",
      "C14" = "Child Development"
    )
  )

## 7.2 Overview of Categories ----

table(data_2$Category)
