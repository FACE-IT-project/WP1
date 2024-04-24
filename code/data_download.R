# code/data_download.R
# The code used to download data directly
# This is generally for very large datasets or those with many small files


# Setup -------------------------------------------------------------------

source("code/functions.R")
library(refdb) # Access both GenBank and BOLD, but a bit wonky 
library(rentrez) # Access GenBank
library(bold) # Access BOLD
library(rgbif) # Access GBIF


# ERSST -------------------------------------------------------------------

# Create vector of URLs for downloading
ERSST_files <- expand_grid(year = 1854:2023, 
                           month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) |> 
  mutate(file_name = paste0("ersst.v5.",year,month,".nc"))

doParallel::registerDoParallel(cores = 15)
plyr::l_ply(ERSST_files$file_name,
            file_URL_save, .parallel = T,
            base_URL = "https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/netcdf/", 
            save_folder = "~/pCloudDrive/FACE-IT_data/ERSST/")


# HadCRUT.5 ---------------------------------------------------------------

# NB: As of 2024-01-04, 2023-12 data are not yet available
download.file("https://www.metoffice.gov.uk/hadobs/hadcrut5/data/HadCRUT.5.0.2.0/analysis/HadCRUT.5.0.2.0.analysis.anomalies.ensemble_mean.nc",
              "~/pCloudDrive/FACE-IT_data/HadCRUT.5/HadCRUT.5.0.2.0.analysis.anomalies.ensemble_mean.nc")


# Barcode databases -------------------------------------------------------

# https://cran.r-project.org/web/packages/refdb/vignettes/ncbi_bold.html

# Taxon of interest
taxon_list <- c("Bangiophyceae", "Florideophyceae", "Phaeophyceae", "Ulvophyceae", "Xanthophyceae")


## redfdb -----------------------------------------------------------------

# Get them all in one go
# NB: The download is extremely slow...but don't run them in parallel
bold1 <- refdb_import_BOLD("Xanthophyceae", ncbi_taxo = FALSE, full = TRUE)
ncbi1 <- refdb_import_NCBI(taxon_list[5], full = TRUE)
# NB: One of these BOLD taxa calls has an error in a column that prevents the internal rbind from working
# Running all of these at once is too time consuming/error prone
# Better to run and save each one individually
## That being said, on 2024-02-12 the full BOLD query managed to run
bold_algae <- plyr::ldply(taxon_list, refdb_import_BOLD, .parallel = FALSE, ncbi_taxo = FALSE, full = TRUE)

# The same goes for GenBank
# ncbi_algae <- plyr::ldply(taxon_list, refdb_import_NCBI, .parallel = FALSE, full = TRUE, seq_bin = 1000)
ncbi_ban <- refdb_import_NCBI(taxon_list[1], full = TRUE, seq_bin = 2000)
ncbi_flo <- refdb_import_NCBI(taxon_list[2], full = TRUE, seq_bin = 2000)
# ncbi_pha <- refdb_import_NCBI(taxon_list[3], full = TRUE, seq_bin = 2000) # Some sort of internal error via refdb
ncbi_ulv <- refdb_import_NCBI(taxon_list[4], full = TRUE, seq_bin = 2000)
ncbi_xan <- refdb_import_NCBI(taxon_list[5], full = TRUE, seq_bin = 2000)
ncbi_algae <- rbind(ncbi_ban, ncbi_flo, ncbi_ulv, ncbi_xan)

# Save
write_csv(bold_algae, "~/pCloudDrive/FACE-IT_data/barcode/bold_algae.csv")
write_csv(ncbi_algae, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_algae.csv")

# Indivudal files
write_csv(ncbi_ban, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_ban.csv")
write_csv(ncbi_flo, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_flo.csv")
# write_csv(ncbi_pha, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_pha.csv") #throws an error
write_csv(ncbi_ulv, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_ulv.csv")
write_csv(ncbi_xan, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_xan.csv")

# Or try a location
bold_arctic <- refdb_import_BOLD(geo = "Arctic", ncbi_taxo = FALSE)
ncbi_arctic <- refdb_import_NCBI("Arctic", full = TRUE)

# Filter to rough Arctic circle
# NB: Roughly 80% of GenBank records have no lon/lat
# But they do have a country record, so we could use that
bold_algae_Arctic <- filter(bold_algae, lat >= 60)
ncbi_algae_Arctic <- filter(ncbi_algae, latitude >= 60)

# Save
write_csv(bold_algae_Arctic, "~/pCloudDrive/FACE-IT_data/barcode/bold_algae_Arctic.csv")
write_csv(ncbi_algae_Arctic, "~/pCloudDrive/FACE-IT_data/barcode/ncbi_algae_Arctic.csv")


## rentrez ----------------------------------------------------------------

# Info
# https://docs.ropensci.org/rentrez/articles/rentrez_tutorial.html
entrez_dbs()
(tax_fields <- entrez_db_searchable("taxonomy"))
(bio_fields <- entrez_db_searchable("biosample"))
rentrez::entrez_db_summary("nuccore")
rentrez::entrez_db_summary("biosample")
rentrez::entrez_db_summary("geoprofiles")

r_search <- entrez_search(db = "pubmed", term = "R Language")
r_search$ids
web_env_search <- entrez_search(db = "biosample", term = paste0(taxon_list[1],"[ORGN]"), 
                               retmax = 10, use_history = TRUE)
rentrez_algae <- entrez_fetch(db = "biosample", id = web_env_search$ids, WebEnv = web_env_search$WebEnv, 
                              query_key = web_env_search$QueryKey, rettype = "fasta")
cat(strwrap(substr(rentrez_algae, 1, 500)), sep="\n")

Tt <- entrez_search(db="taxonomy", term="(Tetrahymena thermophila[ORGN]) AND Species[RANK]")
tax_rec <- entrez_fetch(db="taxonomy", id=Tt$ids, rettype="xml", parsed=TRUE)
class(tax_rec)
tax_list <- XML::xmlToList(tax_rec)
tax_list$Taxon$GeneticCode


# refdb_import_NCBI() source code
ff <- tempfile("refdb_NCBI_", fileext = ".csv")
fx <- tempfile("refdb_NCBI_", fileext = ".xml")
query <- paste0(query, " AND ( \"0\"[SLEN] : \"", max_seq_length, 
                "\"[SLEN] )")
req <- rentrez::entrez_search(db = "nuccore", term = query, 
                              use_history = TRUE)
if (req$count == 0) {
  if (verbose) 
    cat("No sequence found\n")
  return(NULL)
}
if (verbose) 
  cat("Downloading", req$count, "sequences from NCBI...\n")
for (seq_start in seq(0, req$count, seq_bin)) {
  recs <- entrez_fetch_retry(db = "nuccore", web_history = req$web_history, 
                             rettype = "gb", retmode = "xml", retmax = seq_bin, 
                             retstart = seq_start, delay_retry = 60, n_retry = 50, 
                             verbose = verbose)
  if (is.na(recs)) {
    next
  }
  readr::write_lines(recs, file = fx, append = FALSE)
  NCBI_xml <- xml2::read_xml(fx)
  NCBI_xml <- xml2::xml_children(NCBI_xml)
  NCBI_table <- make_ncbi_table(NCBI_xml)
  taxo_id <- xml2::xml_text(xml2::xml_find_all(NCBI_xml, 
                                               ".//GBQualifier_name[text()=\"db_xref\"]/following-sibling::GBQualifier_value"))
  taxo_id <- taxo_id[stringr::str_detect(taxo_id, "taxon:[0-9]+")]
  taxo_id <- stringr::str_extract(taxo_id, "(?<=taxon:)[0-9]+")
  taxo_id <- tibble::tibble(taxonomy = NCBI_table$taxonomy, 
                            id = taxo_id)
  taxo_id <- taxo_id[!duplicated(taxo_id$taxonomy), ]
  gtax <- get_ncbi_taxonomy_retry(taxo_id$id, delay_retry = 60, 
                                  n_retry = 50, verbose = verbose)
  taxo_id <- dplyr::left_join(taxo_id, gtax[, -ncol(gtax)], 
                              by = "id")
  NCBI_table <- dplyr::left_join(NCBI_table, taxo_id, by = "taxonomy", 
                                 suffix = c("", "_taxonomy"))
  NCBI_table <- tibble::tibble(source = "NCBI", NCBI_table)
  NCBI_table <- dplyr::mutate(NCBI_table, species = .data$organism)
  if (full == FALSE) {
    NCBI_table <- dplyr::select(NCBI_table, .data$source, 
                                .data$id, .data$gene, .data$sequence, .data$superkingdom, 
                                .data$kingdom, .data$phylum, .data$subphylum, 
                                .data$class, .data$subclass, .data$infraclass, 
                                .data$order, .data$suborder, .data$infraorder, 
                                .data$superfamily, .data$family, .data$genus, 
                                .data$species, .data$country_location, .data$lat_lon)
  }
  if (seq_start == 0) {
    readr::write_csv(NCBI_table[0, ], file = ff)
  }
  readr::write_csv(NCBI_table, file = ff, append = TRUE, 
                   col_names = FALSE)
  if (verbose) {
    cat("\r > ", seq_start + nrow(NCBI_table), " (", 
        round((seq_start + nrow(NCBI_table))/req$count * 
                100, digits = 1), "%) ", "sequences downloaded.", 
        sep = "")
  }
}
out <- readr::read_csv(ff, col_types = readr::cols())
out <- process_geo_ncbi(out)
out <- refdb_set_fields_NCBI(out)
out <- refdb_set_fields(out, latitude = "latitude", longitude = "longitude")
file.remove(ff, fx)


## bold -------------------------------------------------------------------

boldq <- bold::bold_stats(taxon = taxon_list[1])
bold1 <- bold::bold_seqspec(taxon = taxon_list[1])

bold_algae <- plyr::ldply(taxon_list, bold::bold_seqspec, .parallel = TRUE)
out <- dplyr::select(out, .data$source, .data$sequenceID, 
                     .data$markercode, .data$phylum_name, .data$class_name, 
                     .data$order_name, .data$family_name, .data$subfamily_name, 
                     .data$genus_name, .data$species_name, .data$subspecies_name, 
                     .data$nucleotides, .data$country, .data$province_state, 
                     .data$lat, .data$lon)


## rgbif ------------------------------------------------------------------


## Analyses ---------------------------------------------------------------

# Load files for analysis below if needed
if(!exists("bold_algae")) bold_algae <- read_csv("~/pCloudDrive/FACE-IT_data/barcode/bold_algae.csv")
if(!exists("bold_algae_Arctic")) bold_algae_Arctic <- read_csv("~/pCloudDrive/FACE-IT_data/barcode/bold_algae_Arctic.csv")

# Check number of BOLD records with no lon/lat coords
filter(bold_algae, is.na(lon) | is.na(lat)) |> nrow()
filter(bold_algae_Arctic, lat >= 66) |> nrow()

# Extract just unique records per location
# Not currently interested in quantifying the multiples
bold_Arctic_unq <- bold_algae_Arctic |> 
  dplyr::select(phylum_name, class_name, order_name, family_name, genus_name, species_name, lon, lat, country) |> 
  distinct()
bold_high_Arctic_unq <- bold_Arctic_unq |> filter(lat >= 66)
ncbi_Arctic_unq <- ncbi_algae_Arctic |> 
  dplyr::select(phylum, class, order, family, genus, species, longitude, latitude, country_location) |> 
  distinct()

# Create map of records
ggplot(bold_Arctic_unq, aes(x = lon, y = lat)) + 
  borders() +
  geom_point(aes(colour = class_name), size = 5, alpha = 0.7,
             position = position_dodge(width = 5.0)) +
  coord_quickmap(ylim = c(59, 81), xlim = c(-172, 30)) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom",
        panel.border = element_rect(fill = NA, colour = "black"))
ggsave("~/pCloudDrive/FACE-IT_data/barcode/bold_Arctic_unq_map.png", height = 3, width = 7)

ggplot(ncbi_Arctic_unq, aes(x = longitude, y = latitude)) + 
  borders() +
  geom_point(aes(colour = class), size = 5, alpha = 0.7,
             position = position_dodge(width = 5.0)) +
  coord_quickmap(ylim = c(59, 81), xlim = c(-172, 30)) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom",
        panel.border = element_rect(fill = NA, colour = "black"))
ggsave("~/pCloudDrive/FACE-IT_data/barcode/ncbi_Arctic_unq_map.png", height = 3, width = 7)

