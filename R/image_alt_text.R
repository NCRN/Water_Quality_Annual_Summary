#' Generate Alternative Text for Stream Images
#'
#' Constructs a data frame containing unique file names and their
#' corresponding alternative text descriptions for images.  
#' The function combines `Park` and `Site` values to create a
#' `file_name` identifier and then assigns human‑readable alt text
#' based on predefined mappings.
#'
#' @param metadata A data frame containing metadata for images.  
#'   Must include fields referenced by `Park` and `Site`.
#' @param Park A column name or vector representing park identifiers
#'   to be combined into the `file_name`.
#' @param Site A column name or vector representing site identifiers
#'   to be combined into the `file_name`.
#'
#' @return A data frame with two columns:
#'   \describe{
#'     \item{file_name}{A unique identifier created by combining `Park` and `Site`.}
#'     \item{alt_text}{A descriptive alternative-text string for each file name.}
#'   }
#'   Any file names not included in the predefined mapping receive an empty string.
#'
#' @details
#' This function is intended to support accessibility by generating
#' alt text for images of streams across various parks and sites.
#' The alt text strings are explicitly defined in the internal
#' `case_when()` mapping.
#'
#' @examples
#' \dontrun{
#' make_alt_text(metadata_df, Park = metadata_df$Park, Site = metadata_df$Site)
#' }
#'
#' @export


library(dplyr)
library(tidyr)
library(glue)

make_alt_text <- function(metadata, Park, Site) {
  # mapping defined once and readable
  alt_map <- tibble::tribble(
    ~file_name,   ~alt_text,
    "ANTI_SHCK",  "Leafy branches partially obscure a wide stream that runs under a concrete overpass.",
    "CATO_BGHC",  "A wide stream filled with large rocks and surrounded by green trees.",
    "CATO_BLBZ",  "A forest stream with rocky banks retreats under a fenced walking path.",
    "CATO_OWCK",  "A stream filled with rocks and surrounded by grass and trees flows under a small bridge.",
    "GWMP_MICR",  "A stream filled with large rocks and woody debris, and surrounded by leafy plants and bare trees flows under a tall bridge.",
    "GWMP_MIRU",  "A wide, shallow stream filled with rocks and surrounded by standing and fallen trees flows under a road in the distance.",
    "GWMP_PIRU",  "A stream filled with large rocks surrounded by green plants and trees.",
    "GWMP_TURU",  "A stream, with rocky banks, retreats into the distance.",
    "HAFE_FLSP",  "A stream retreats into the distance in a lush green forest.",
    "MANA_YOBR",  "A wide stream with muddy water retreats into the distance, with some bare tree trunks along its banks.",
    "MONO_BUCK",  "A wide stream surrounded by a sloping, rocky bank on the left, a grassy bank on the right, and bare trees flows around a bend.",
    "MONO_GAMI",  "A shallow stream with a pile of sediment in the middle surrounded by bare trees and patchy grass on the stream bank.",
    "NACE_HECR",  "A wide stream surrounded by lush green shrubs and trees.",
    "NACE_OXRU",  "A wide stream in an open field surrounded by grass on the left and tan pebbles on the right, with trees in the distance.",
    "NACE_STCK",  "A small stream surrounded by sediment, pebbles, and grass on the right side, and plants and trees on the left side.",
    "PRWI_BONE",  "A narrow stream surrounded by sparse bare trees, brown leaves on the ground, and some mosses on the stream bank.",
    "PRWI_CARU",  "A narrow stream filled with sediment and surrounded by green plants. Multiple fallen or leaning trees lay across the stream.",
    "PRWI_MARU",  "A narrow stream flows through a forest of trees with sparse or no leaves.",
    "PRWI_MBBR",  "A narrow stream filled with rocks flows through a lush, green forest.",
    "PRWI_NFQC",  "A wide stream surrounded by bare trees and some green plants flows around a bend.",
    "PRWI_ORRU",  "A narrow stream surrounded by grass, dead leaves, and a bare tree flows into a small pond.",
    "PRWI_SFQC",  "A wide stream surrounded by lush green trees.",
    "PRWI_SORU",  "A wide stream surrounded by bare trees and some shrubs.",
    "PRWI_TARU",  "A shallow, narrow stream surrounded by green trees and plants flows into a culvert.",
    "ROCR_BAKE",  "A shallow stream filled with rocks and surrounded by green trees.",
    "ROCR_BRBR",  "A wide stream filled with rocks surrounded by a stone wall on the right, a hill on the left, and bare trees flows under a bridge road.",
    "ROCR_DUOA",  "A narrow stream filled with rocks and woody debris, surrounded by a sandy and muddy stream bank, flows through a green forest.",
    "ROCR_FEBR",  "A wide stream filled with rocks and surrounded by green plants and trees.",
    "ROCR_KLVA",  "A rocky stream with a wall and fence to the right surrounded by lush green plants and trees.",
    "ROCR_LUBR",  "A rocky stream surrounded by lush green plants and trees.",
    "ROCR_NOST",  "A wide, rocky stream surrounded by trees with sparse or no leaves and fallen leaves on the ground flows under a small bridge.",
    "ROCR_PHBR",  "A rocky stream surrounded by lush green plants and trees.",
    "ROCR_PYBR",  "A rocky stream surrounded by lush green plants and trees flows under a tall, green bridge.",
    "ROCR_R630",  "A rocky stream surrounded by lush green plants and trees.",
    "ROCR_ROC3",  "A wide stream with large rocks surrounded by trees.",
    "WOTR_CHCK",  "A wide stream runs through a forest of leaning green trees.",
    "WOTR_WOTR",  "A wide stream surrounded by bare trees and some green plants and mosses."
  )
  
  out <- metadata %>%
    mutate(file_name = paste({{ Park }}, {{ Site }}, sep = "_")) %>%  # tidy-eval
    select(file_name) %>%
    distinct() %>%
    left_join(alt_map, by = "file_name") %>%
    mutate(alt_text = tidyr::replace_na(alt_text, ""))
  
  # warn if some file_names don't have alt text
  missing <- out %>% filter(alt_text == "") %>% pull(file_name)
  if (length(missing) > 0) {
    warning(glue("No alt text for {length(missing)} file_name(s): {paste(missing, collapse = ', ')}"))
  }
  
  return(out)
}