source("MongoDBConnection.R")
#' Title
#'
#' @param entrezhsallrecords
#'
#' @return
#' @export
#'
#' @examples
HsEntrezGeneQueries <-
  function(entrezhsallrecords) {
    # find all entries for Hs tax id
    # hstax_id is count of all rows where tax id = 9606
    hstax_idcount <-  nrow(entrezhsallrecords %>% select({
    }))
    return (hstax_idcount)
  }

#' Title
#'
#' @param homosapiensmongodbconnection
#'
#' @return
#' @export
#'
#' @examples
entrezhsdistinctgeneids <- function(entrezhumangenedataallrecords) {
  entrezhumangenedistinctgeneids <-
    entrezhumangenedataallrecords %>% select("GeneID", "Symbol")
  return (entrezhumangenedistinctgeneids)
}
#' Title
#'
#' @param homosapiensmongodbconnection
#'
#' @return
#' @export
#'
#' @examples
entrezhsallrecords <- function(homosapiensmongodbconnection) {
  entrezhumangenedatallrecords <-
    homosapiensmongodbconnection$find('{}')
  return (entrezhumangenedatallrecords)
}

#' Title
#'
#' @param entrezhumangenedataallrecords
#'
#' @return
#' @export
#'
#' @examples
hsSymbolsSynonymsFeaturetype <-
  function(entrezhumangenedataallrecords) {
    entrezSymbolSynonymsfeaturetype <-
      entrezhumangenedataallrecords %>% select("Symbol", "Synonyms", "Feature_type")
    return (entrezSymbolSynonymsfeaturetype)
  }
# Count all symbols and display along with gene ids, synonyms and
#' Title
#'
#' @param entrezhumangenedataallrecords
#'
#' @return
#' @export
#'
#' @examples
hsGeneSymbolsFeaturetypes <-
  function(entrezhumangenedataallrecords) {
    entrezhsGenesWithSymbolsAndFeatureTypes <-
      entrezhumangenedataallrecords %>% select("Symbol", "Feature_type")
    return(entrezhsGenesWithSymbolsAndFeatureTypes)
  }
#' Title
#'
#' @param entrezhumangenedataallrecords
#'
#' @return
#' @export
#'
#' @examples
hsLocusTags <- function(entrezhumangenedataallrecords) {
  entrezhsLocusTag <-
    unique(entrezhumangenedataallrecords %>% select("LocusTag"))
  return (entrezhsLocusTag)
}
#' Title
#'
#' @param homosapiensmongodbconnection
#'
#' @return
#' @export
#'
#' @examples
hsSynonyms <- function(homosapiensmongodbconnection) {
  entrezhsSynonyms <-
    unique(homosapiensmongodbconnection$distinct("Synonyms"))
  return (entrezhsSynonyms)
}
#' Title
#'
#' @param entrezhumangenedataallrecords
#'
#' @return
#' @export
#'
#' @examples
geneSymbolsAnddbCrossRefs <-
  function(entrezhumangenedataallrecords) {
    entrezhsgenesymbolsanddbXrefs <-
      entrezhumangenedataallrecords %>% select("Symbol", "dbXrefs")
    return (entrezhsgenesymbolsanddbXrefs)
  }
#' Title
#'
#' @param entrezhumangenedataallrecords
#'
#' @return
#' @export
#'
#' @examples
genesymbolandmaplocation <-
  function(entrezhumangenedataallrecords) {
    entrezhsgenesymbolsandmap_location <-
      entrezhumangenedataallrecords %>% select("Symbol", "map_location")
    return (entrezhsgenesymbolsandmap_location)
  }
#' Title
#'
#' @param entrezhumangenedataallrecords
#'
#' @return
#' @export
#'
#' @examples
extractTypeOfGene <- function(entrezhumangenedataallrecords) {
  entrezhstype_of_gene <-
    unique(entrezhumangenedataallrecords %>% select("type_of_gene"))
  return (entrezhstype_of_gene)
}
#' Title
#'
#' @param entrezhumangenedataallrecords
#'
#' @return
#' @export
#'
#' @examples
extractfeaturetype <- function(entrezhumangenedataallrecords) {
  entrezhsfeature_type <-
    unique(entrezhumangenedataallrecords %>% select("Feature_type"))
  return(entrezhsfeature_type)
}
#' Title
#'
#' @param entrezhumangenedataallrecords
#'
#' @return
#' @export
#'
#' @examples
extractdescriptionfeaturetype <-
  function(entrezhumangenedataallrecords) {
    entrezgene_description_featuretype <-
      entrezhumangenedataallrecords %>% select("Symbol", "description", "Feature_type")
    return(entrezgene_description_featuretype)
  }
