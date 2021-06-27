source("MongoDBConnection.R")

#' Title
#'
#' @param entrezMmallrecords 
#'
#' @return
#' @export
#'
#' @examples
MmEntrezGeneQueries <-
  function(entrezMmallrecords) {
    # find all entries for Mm tax id
    # Mmtax_id is count of all rows where tax id = 9606
    Mmtax_idcount <-  nrow(entrezMmallrecords %>% select({}))
    return (Mmtax_idcount)
  }


#' Title
#'
#' @param musmusculusentrezgenedataallrecords 
#'
#' @return
#' @export
#'
#' @examples
entrezMmdistinctgeneids <- function(musmusculusentrezgenedataallrecords) {
  entrezmousegenedistinctgeneids <- musmusculusentrezgenedataallrecords %>% select("GeneID", "Symbol") 
  return (entrezmousegenedistinctgeneids)
}

#' Title
#'
#' @param musmusculusentrezgenedbconnection 
#'
#' @return
#' @export
#'
#' @examples
entrezMmallrecords <- function(musmusculusentrezgenedbconnection) {
  entrezmousegenedatallrecords <- musmusculusentrezgenedbconnection$find('{}')
  return (entrezmousegenedatallrecords)
}


#' Title
#'
#' @param entrezmousegenedataallrecords 
#'
#' @return
#' @export
#'
#' @examples
MmSymbolsSynonymsFeaturetype <- function(entrezmousegenedataallrecords) {
  entrezSymbolSynonymsfeaturetype <-
    entrezmousegenedataallrecords %>% select("Symbol", "Synonyms", "Feature_type")
  return (entrezSymbolSynonymsfeaturetype)
}

#' Title
#'
#' @param entrezmousegenedataallrecords 
#'
#' @return
#' @export
#'
#' @examples
MmGeneSymbolsFeaturetypes <- function(entrezmousegenedataallrecords) {
  entrezMmGenesWitMmymbolsAndFeatureTypes <- entrezmousegenedataallrecords %>% select("Symbol", "Feature_type")
  return(entrezMmGenesWitMmymbolsAndFeatureTypes)                         
}

#' Title
#'
#' @param entrezmousegenedataallrecords 
#'
#' @return
#' @export
#'
#' @examples
MmLocusTags <- function(entrezmousegenedataallrecords) {
  entrezMmLocusTag <- unique(entrezmousegenedataallrecords %>% select("LocusTag"))
  return (entrezMmLocusTag)
}

#' Title
#'
#' @param musmusculusentrezgenedbconnection 
#'
#' @return
#' @export
#'
#' @examples
MmSynonyms <- function(musmusculusentrezgenedbconnection) {
  entrezMmSynonyms <- unique(musmusculusentrezgenedbconnection$distinct("Synonyms"))
  return (entrezMmSynonyms)
}

#' Title
#'
#' @param entrezmousegenedataallrecords 
#'
#' @return
#' @export
#'
#' @examples
geneSymbolsAnddbCrossRefs <- function(entrezmousegenedataallrecords) {
  entrezMmgenesymbolsanddbXrefs <-
    entrezmousegenedataallrecords %>% select("Symbol", "dbXrefs")
  return (entrezMmgenesymbolsanddbXrefs)
}

#' Title
#'
#' @param entrezmousegenedataallrecords 
#'
#' @return
#' @export
#'
#' @examples
genesymbolandmaplocation <- function(entrezmousegenedataallrecords) {
  entrezMmgenesymbolsandmap_location <-
    entrezmousegenedataallrecords %>% select("Symbol", "map_location")
  return ( entrezMmgenesymbolsandmap_location)
}

#' Title
#'
#' @param entrezmousegenedataallrecords 
#'
#' @return
#' @export
#'
#' @examples
extractTypeOfGene <- function(entrezmousegenedataallrecords) {
  entrezMmtype_of_gene <- unique(entrezmousegenedataallrecords %>% select("type_of_gene"))
  return (entrezMmtype_of_gene)
}

#' Title
#'
#' @param entrezmousegenedataallrecords 
#'
#' @return
#' @export
#'
#' @examples
extractfeaturetype <- function(entrezmousegenedataallrecords) {
  entrezMmfeature_type <-
    unique(entrezmousegenedataallrecords %>% select("Feature_type"))
  return(entrezMmfeature_type)
}

#' Title
#'
#' @param entrezmousegenedataallrecords
#'
#' @return
#' @export
#'
#' @examples
extractdescriptionfeaturetype <- function(entrezmousegenedataallrecords) {
  entrezgene_description_featuretype <- entrezmousegenedataallrecords %>% select("Symbol", "description", "Feature_type")
  return(entrezgene_description_featuretype)
}


