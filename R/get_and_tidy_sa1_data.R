library(data.table)
library(dplyr)
library(stringr)

# a little setup ---------------------------------------------------------------
if (!dir.exists("data-raw")) dir.create("data-raw")
if (!dir.exists("output/sqlite")) dir.create("output/sqlite", recursive = TRUE)
if (!dir.exists("output/csv")) dir.create("output/csv", recursive = TRUE)

# import geographic labels -----------------------------------------------------
# supply geographic labels and descriptions
#
# it's impractical to do this programmatically since the target features are 
# hosted on the Stats NZ Geographic data service, and features can only be 
# extracted after creating a login or API key, and then creating downloadable
# resource via the web UI or koordinates API.  
#
# it would be nice if Stats just made these available in common formats via a
# direct link.
geo <- readRDS("data/geo.rds")

# fetch data -------------------------------------------------------------------
# download SA1 long files
download.file(
  "https://www3.stats.govt.nz/2018census/SA1Dataset/Statistical%20Area%201%20dataset%20for%20Census%202018%20%E2%80%93%20total%20New%20Zealand%20%E2%80%93%20Long%20format_updated_16-7-20.zip", 
  destfile = "data-raw/data.zip"
)

# unzip the archive
utils::unzip(
  "data-raw/data.zip", exdir = "data-raw"
)

# helper functions -------------------------------------------------------------
# extract categories for a dataset
get_cats <- function(x) {
  cols <- colnames(x)
  
  cols <- cols[
    !tolower(cols) %in% 
      c("area_code_and_description", "area_code", "area_description", "year")
  ]
  
  count_col <- grep("^.*count.*$", cols, perl = TRUE, value = TRUE)
  codes <- grep("^.*code$", tolower(cols), perl = TRUE, value = TRUE)
  
  descriptors <- grep(
    "^.*(description|descriptor)$", tolower(cols), perl = TRUE, value = TRUE
  )
  
  vars <- gsub("_code", "", codes)
  list(vars, codes, descriptors)
}

# recode
recode <- function(file) {
  cat(file, "...\n")
  d <- strsplit(file, "/")[[1]][1]
  f <- strsplit(file, "/")[[1]][2]
  f <- gsub("-", "_", f)
  topic <- tolower(gsub("_", " ", substr(f, 1, str_locate(f, "_long")[1] - 1)))
  
  unit <- 
    if (!is.na(str_locate(toupper(d), "DWELLING")[1])) "dwelling"
    else if (!is.na(str_locate(toupper(d), "HOUSEHOLD")[1])) "household"
    else "individual"
  
  code <- function(x) if (x %in% c("C", "..")) x else NA
  num <- function(x) if (x %in% c("C", "..")) NA else as.numeric(x)
  res <- fread(sprintf("data-raw/%s", file)) 
  colnames(res) <-  gsub("-", "_", tolower(colnames(res)))
  colnames(res)[grepl("^.*area_code_and_description$", colnames(res))] <-
    "area_code_and_description"
  count_col <- grep("^.*count.*$", colnames(res), value = TRUE)
  
  if (count_col != "count")
    res <- res |>
      rename(count = !!rlang::parse_quo(count_col, env = parent.frame())) |>
      setDT()
  
  cats <- get_cats(res)
  
  if (length(cats[[1]]) > 0) {
    vars <- lapply(cats[[1]], function(z) z)
    names(vars) <- sprintf("var_%s", 1:length(cats[[1]]))
    
    codes <- lapply(
      cats[[2]], function(z) rlang::parse_quo(z, env = parent.frame())
    )
    
    names(codes) <- sprintf("code_%s", 1:length(cats[[2]]))
    
    descriptions <- lapply(
      cats[[3]], function(z) rlang::parse_quo(z, env = parent.frame())
    )
    
    names(descriptions) <- sprintf("description_%s", 1:length(cats[[3]]))

    res <- res |>
      rename(!!!codes) |>
      rename(!!!descriptions) |>
      mutate(!!!vars) |>
      setDT()
  }
  
  res[, area_code := str_trim(area_code)]
  res[, area_description := str_trim(area_description)]
  res[, area_code_and_description := str_trim(area_code_and_description)]
  res[, flag := sapply(count, code, USE.NAMES = FALSE)]
  res[, count := sapply(count, num, USE.NAMES = FALSE)]
  res[, unit := `unit`]
  res[, topic := `topic`]
  
  res[, area_description := 
        ifelse(area_code == 'total', "Total NZ", area_description)]
  
  res[, area_code_and_description := 
        ifelse(area_code == 'total', "Total NZ", area_code_and_description)]
  
  res <- unique(res)
  
  cols <- c(
    "unit", "topic", "geography", "area_code", "area_description", "year", 
    if (length(cats[[1]]) > 0) c(names(vars), names(codes), names(descriptions)), 
    "count", "flag"
  )
  
  merge(
    setDT(geo), 
    res, 
    by =  c("area_code", "area_description")
  )[, cols, with = FALSE][]
}

# normalize, i.e. split repeating info into separate tables, etc.
# supporting tables (labels and metadata) stored as attributes.
normalize <- function(x) {
  vars <- grep("^var.*$", colnames(x), perl = TRUE, value = TRUE)
  codes <- grep("^code.*$", colnames(x), perl = TRUE, value = TRUE)
  meta <- unique(x[, c("unit", "topic", vars), with = FALSE])
  labels <- if (length(vars) > 0) {
    lapply(1:length(vars), function(z){
      oldnames <- sprintf(c("var_%d", "code_%d", "description_%d"), z)
      res <- unique(x[, oldnames, with = FALSE])
      setnames(res, oldnames, c("variable", "code", "description"))
    }) 
  } else c()
  res <- x[, c("geography", "area_code", "year", codes, "count", "flag"), with = FALSE]
  setkeyv(res, c("geography", "area_code", "year", codes))
  attr(res, 'meta') <- meta
  attr(res, 'labels') <- rbindlist(labels)
  
  res
}

recode_and_normalize <- function(file) {
  normalize(recode(file))
}

# reorganise data so it is fit for use with a RDMS
# using for loop and mutable datasets... 
# would like to use tail recursion, but stack overflow!
refactor <- function(x, n) {
  meta <- copy(attr(x[[1]], "meta"))[, 
    id := 1L][, c("id", colnames(attr(x[[1]], "meta"))), with = FALSE]
  
  labels <- copy(attr(x[[1]], "labels"))
  data <- copy(x[[1]])[, `id` := 1L]

  if (missing(n)) n <- as.integer(length(x))
  
  for (i in 2L:n) { 
    meta_ <- copy(attr(x[[i]], "meta"))[, id := `i`]
    labels_ <- copy(attr(x[[i]], "labels"))
    data_ <- copy(x[[i]])[, id := `i`] 
    
    meta <- rbind(meta, meta_, fill = TRUE)
    labels <- unique(rbind(labels, labels_))
    data <- rbind(data, data_, fill = TRUE)
  
    setkeyv(labels, c("variable", "code", "description"), physical = TRUE)
  }
  
  cols <- c(
    "id", "geography", "area_code", "year", "code_1", "code_2", "count", "flag"
  )
  
  cols <- intersect(cols, colnames(data))
  
  data <- data[, cols, with = FALSE]
  data[, id := as.integer(id)]
  meta[, id := as.integer(id)]
  
  list(
    data = data, 
    metadata = meta, 
    labels = labels
  )
}

# process all files, output sqlite db, and csv files ---------------------------
## create data -----------------------------------------------------------------
files <- list.files(path = "data-raw", pattern = "*.csv", recursive = TRUE)
x <- parallel::mclapply(files, recode_and_normalize, mc.cores = 2)
z <- refactor(x)

## create SQLite database ------------------------------------------------------
con <- DBI::dbConnect(RSQLite::SQLite(), "output/sqlite/sa1.db")

DBI::dbSendQuery(
  con, 
  "
  CREATE TABLE geo (
    geography TEXT,
    area_code TEXT,
    area_description TEXT
  )
  "
)

DBI::dbSendQuery(
  con, 
  "
  CREATE TABLE metadata (
    id INTEGER,
    unit TEXT,
    topic TEXT,
    var_1 TEXT,
    var_2 TEXT
  )
  "
)

DBI::dbSendQuery(
  con,
  "
  CREATE TABLE labels (
    variable TEXT,
    code TEXT,
    description TEXT
  )
  "
)

DBI::dbSendQuery(
  con,
  "
  CREATE TABLE data (
    id INTEGER,
    geography TEXT,
    area_code TEXT,
    year INTEGER,
    code_1 TEXT NULL,
    code_2 TEXT NULL,
    count REAL,
    flag TEXT,
    FOREIGN KEY(geography, area_code) REFERENCES geo(geography, area_code),
    FOREIGN KEY(id) REFERENCES metadata(id),
    FOREIGN KEY(code_1) REFERENCES labels(code),
    FOREIGN KEY(code_2) REFERENCES labels(code)
  )
  "
)

DBI::dbAppendTable(con, "geo", geo)
DBI::dbSendQuery(con, "create index g1 on geo (geography, area_code)")
DBI::dbAppendTable(con, "metadata", z$metadata)
DBI::dbSendQuery(con, "create index m1 on metadata (id)")
DBI::dbAppendTable(con, "labels", z$labels)
DBI::dbSendQuery(con, "create index l1 on labels(variable, code)")
DBI::dbAppendTable(con, "data", z$data)
# this index adds nearly 1GB to the database!!  
DBI::dbSendQuery(con, "create index d1 on data (id, geography, area_code)")

# create a view
q <- "
create view if not exists all_data_v as
select 
  d.geography, d.area_code, g.area_description, 
  m.id, m.unit, m.topic, 
  m.var_1, d.code_1, l1.variable as desc_1, 
  m.var_2, d.code_2, l2.variable as desc_2,
  d.count, d.flag
from 
  metadata m 
inner join 
  data d 
on 
  m.id = d.id
inner join
  geo g
on
  d.geography = g.geography
  and d.area_code = g.area_code
left join
  labels l1
on 
  d.code_1 = l1.code
left join
  labels l2
on 
  d.code_2 = l2.code
"

DBI::dbSendStatement(con, q)

DBI::dbDisconnect(con)

## create csv files ------------------------------------------------------------
data.table::fwrite(geo, "output/csv/geo.csv.gz")
data.table::fwrite(z$metadata, "output/csv/metadata.csv.gz")
data.table::fwrite(z$labels, "output/csv/labels.csv.gz")
data.table::fwrite(z$data, "output/csv/data.csv.gz")

DBI::dbAppendTable(con, "geo", geo)
DBI::dbAppendTable(con, "metadata", z$metadata)
DBI::dbAppendTable(con, "labels", z$labels)
DBI::dbAppendTable(con, "data", z$data)

# tidy up ----------------------------------------------------------------------
rm(list=ls())
gc()