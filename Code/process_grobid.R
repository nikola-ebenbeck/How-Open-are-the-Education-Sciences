# This script processes the TEI files from GROBID into R tables for metacheck in parallel. Note that this is only reproducible if the TEI files are available in the paths specified in the index, i.e. the downloads were run.

# This is also supposed to be run from experiment/paper1 as this is the osf version.

library(RSQLite)
library(dplyr)
library(tidyverse)
library(stringr)
library(tidyr)
library(metacheck)
library(parallel)

db_index_path <- "example_path.db" # "../../db/index.merged.db"


conn <- dbConnect(RSQLite::SQLite(), db_index_path)
query <- "SELECT openalex_id, journal_id, tei_local_path FROM works;"
data_all <- dbGetQuery(conn, query)

dbExecute(
  conn,
  "CREATE TABLE IF NOT EXISTS paper_objects (
  openalex_id TEXT PRIMARY KEY,
  object_path TEXT NOT NULL,
  updated_at TEXT NOT NULL
);"
)

dbExecute(
  conn,
  "CREATE TABLE IF NOT EXISTS paper_links (
  openalex_id TEXT PRIMARY KEY,
  osf_links TEXT,
  git_links TEXT,
  updated_at TEXT NOT NULL
);"
)
paper_cache <- dbGetQuery(
  conn,
  "SELECT openalex_id, object_path FROM paper_objects;"
)
dbDisconnect(conn)
data <- data_all |>
  mutate(tei_id = tei_local_path) |>
  mutate(
    tei_local_path = if_else(
      str_starts(tei_local_path, "/"),
      tei_local_path,
      paste0("../../db/teis/", tei_local_path)
    )
  )

journal_map <- tribble(
  ~short , ~id           , ~full_name                                                                              ,
  "mdpi" , "S2738008561" , "Education Sciences"                                                                    ,
  "fe"   , "S2596526815" , "Frontiers in Education"                                                                ,
  "cog"  , "S2764918247" , "COGENT EDUCATION"                                                                      ,
  "flr"  , "S4210191100" , "Frontline Learning Research"                                                           ,
  "aero" , "S2738252563" , "AERA Open"                                                                             ,
  "sage" , "S148277943"  , "SAGE Open"                                                                             ,
  "ijet" , "S4210201537" , "International Journal of Educational Technology in Higher Education (Springer), 99.8%" ,
)

reg <- journal_map |>
  select(id, journal_short = short, journal_long = full_name)

data <- data |> left_join(reg, by = c("journal_id" = "id"))

data <- data |> left_join(paper_cache, by = "openalex_id")

cache_root <- "data/paper_objs"
dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)

read_and_cache_paper <- function(openalex_id, tei_path, extract_links = TRUE) {
  obj_path <- file.path(cache_root, paste0(openalex_id, ".Rds"))

  paper_obj <- NULL
  object_path <- NA_character_
  osf_text <- NA_character_
  git_text <- NA_character_

  path_string <- if (!is.null(tei_path)) trimws(tei_path) else ""
  is_missing_path <- function(path) {
    is.na(path) || !nzchar(path) || path %in% c("NA", "NULL", "na", "null", "")
  }

  if (file.exists(obj_path)) {
    object_path <- obj_path
    message(sprintf(
      "SKIP: %s - cache already exists at %s",
      openalex_id,
      obj_path
    ))

    if (extract_links) {
      paper_obj <- readRDS(obj_path)
    }
  } else if (is_missing_path(path_string)) {
    message(sprintf(
      "SKIP: %s - missing or invalid TEI path (%s)",
      openalex_id,
      path_string
    ))
  } else {
    if (!file.exists(path_string)) {
      message(sprintf(
        "FAIL: %s - TEI file not found at %s",
        openalex_id,
        path_string
      ))
      return(list(openalex_id = openalex_id, object_path = NA_character_))
    }

    paper_obj <- tryCatch(
      {
        metacheck::read(path_string)
      },
      error = function(e) {
        message(sprintf("FAIL: %s - parsing error: %s", openalex_id, e$message))
        NULL
      }
    )

    if (!is.null(paper_obj)) {
      saveRDS(paper_obj, obj_path)
      if (file.exists(obj_path)) {
        object_path <- obj_path
        message(sprintf("SUCCESS: %s - saved %s", openalex_id, obj_path))
      } else {
        message(sprintf(
          "FAIL: %s - read succeeded but file was not written: %s",
          openalex_id,
          obj_path
        ))
        paper_obj <- NULL
      }
    }
  }

  if (extract_links && !is.null(paper_obj)) {
    osf_links <- tryCatch(metacheck::osf_links(paper_obj), error = function(e) {
      NULL
    })
    git_links <- tryCatch(
      metacheck::github_links(paper_obj),
      error = function(e) NULL
    )

    if (!is.null(osf_links) && nrow(osf_links) > 0) {
      osf_text <- paste(osf_links$text, collapse = "||")
      print(osf_text)
    }
    if (!is.null(git_links) && nrow(git_links) > 0) {
      git_text <- paste(git_links$text, collapse = "||")
      print(git_text)
    }
  }

  return(list(
    openalex_id = openalex_id,
    object_path = object_path,
    osf_links = osf_text,
    git_links = git_text
  ))
}

papers_to_process <- data |>
  filter(
    journal_short %in% c("ijet"),
    !is.na(tei_local_path),
    nzchar(tei_local_path)
  ) |>
  select(openalex_id, tei_local_path)


if (nrow(papers_to_process) > 0) {
  n_cores <- min(32L, detectCores())
  chunk_size <- n_cores
  pb <- txtProgressBar(min = 0, max = nrow(papers_to_process), style = 3)
  processed <- 0

  conn <- dbConnect(RSQLite::SQLite(), db_index_path)
  for (start in seq(1, nrow(papers_to_process), by = chunk_size)) {
    end <- min(start + chunk_size - 1, nrow(papers_to_process))
    chunk <- papers_to_process[start:end, ]

    chunk_results <- mclapply(
      seq_len(nrow(chunk)),
      function(i) {
        row <- chunk[i, ]
        read_and_cache_paper(row$openalex_id, row$tei_local_path)
      },
      mc.cores = min(n_cores, nrow(chunk)),
      mc.preschedule = FALSE
    )

    for (result in chunk_results) {
      if (
        is.na(result$object_path) ||
          !nzchar(result$object_path) ||
          !file.exists(result$object_path)
      ) {
        message(sprintf(
          "DB SKIP: %s - no valid object path",
          result$openalex_id
        ))
        next
      }
      dbExecute(
        conn,
        "INSERT OR REPLACE INTO paper_objects (openalex_id, object_path, updated_at) VALUES (?, ?, datetime('now'))",
        params = list(result$openalex_id, result$object_path)
      )
      dbExecute(
        conn,
        "INSERT OR REPLACE INTO paper_links (openalex_id, osf_links, git_links, updated_at) VALUES (?, ?, ?, datetime('now'))",
        params = list(result$openalex_id, result$osf_links, result$git_links)
      )
    }

    processed <- processed + nrow(chunk)
    setTxtProgressBar(pb, processed)
  }
  close(pb)
  dbDisconnect(conn)
}

print(papers_to_process)
