#' Title
#'
#' @param musmusculusentrezgenedbconnection 
#'
#' @return
#' @export
#'
#' @examples
findNumberOfProteinCodingGenesPerChromosome <- function(musmusculusentrezgenedbconnection) {
  # chr1 genes which are protein coding for which we display chromsome, type of genes.
  # symbol, synonyms, dbxrefs and modification date greater than ???
  # chr1 genes which are protein coding for which we display chromsome, type of genes.
  # symbol, synonyms, dbxrefs
  #jsonlite::fromJSON(chr1mmgenes)
  chr1mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "1", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1mmgenes)
  chr2mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "2", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  chr3mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "3", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr4mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "4", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  chr5mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "5", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )    #jsonlite::fromJSON(chr1mmgenes)
  
  chr6mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "6", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr7mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "7", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr8mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "8", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr9mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "9", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1mmgenes)
  
  chr10mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "10", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr11mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "11", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr12mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "12", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1mmgenes)
  chr13mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "13", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr14mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "14", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1mmgenes)
  chr15mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "15", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr16mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "16", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr17mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "17", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr18mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "18", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr19mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "19", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr20mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "20", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr21mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "21", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr22mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "22", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  print("Coercing into dataframe gene counts for mm genes across 22 chromosomes ")
  
  msproteincodingclass.df <-
    data.frame(
      "Chromosomenumber" = 1:23,
      "Numberofproteincodinggenesperchromosome" =
        c(
          nrow(chr1mmgenes),
          nrow(chr2mmgenes),
          nrow(chr3mmgenes),
          nrow(chr4mmgenes),
          nrow(chr4mmgenes),
          nrow(chr5mmgenes),
          nrow(chr6mmgenes),
          nrow(chr7mmgenes),
          nrow(chr8mmgenes),
          nrow(chr9mmgenes),
          nrow(chr10mmgenes),
          nrow(chr11mmgenes),
          nrow(chr12mmgenes),
          nrow(chr13mmgenes),
          nrow(chr14mmgenes),
          nrow(chr15mmgenes),
          nrow(chr16mmgenes),
          nrow(chr17mmgenes),
          nrow(chr18mmgenes),
          nrow(chr19mmgenes),
          nrow(chr20mmgenes),
          nrow(chr21mmgenes),
          nrow(chr22mmgenes)
        )
    )
  return(msproteincodingclass.df)
}



#' Title
#'
#' @param musmusculusentrezgenedbconnection 
#'
#' @return
#' @export
#'
#' @examples
findNumberOflysosomalGenesPerChromosome <- function(musmusculusentrezgenedbconnection) {

  chr1mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "1", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1mmgenes)
  chr2mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "2", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  chr3mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "3", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr4mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "4", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  chr5mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "5", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )    #jsonlite::fromJSON(chr1mmgenes)
  
  chr6mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "6", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr7mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "7", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr8mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "8", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr9mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "9", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1mmgenes)
  
  chr10mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "10", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr11mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "11", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr12mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "12", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1mmgenes)
  chr13mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "13", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr14mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "14", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1mmgenes)
  chr15mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "15", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr16mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "16", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr17mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "17", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr18mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "18", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr19mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "19", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr20mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "20", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr21mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "21", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr22mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "22", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  print("Coercing into dataframe gene counts for mm genes across 22 chromosomes ")
  
  mslysosomalclass.df <-
    data.frame(
      "Chromosomenumber" = 1:23,
      "Numberoflysosomalgenesperchromosome" =
        c(
          nrow(chr1mmgenes),
          nrow(chr2mmgenes),
          nrow(chr3mmgenes),
          nrow(chr4mmgenes),
          nrow(chr4mmgenes),
          nrow(chr5mmgenes),
          nrow(chr6mmgenes),
          nrow(chr7mmgenes),
          nrow(chr8mmgenes),
          nrow(chr9mmgenes),
          nrow(chr10mmgenes),
          nrow(chr11mmgenes),
          nrow(chr12mmgenes),
          nrow(chr13mmgenes),
          nrow(chr14mmgenes),
          nrow(chr15mmgenes),
          nrow(chr16mmgenes),
          nrow(chr17mmgenes),
          nrow(chr18mmgenes),
          nrow(chr19mmgenes),
          nrow(chr20mmgenes),
          nrow(chr21mmgenes),
          nrow(chr22mmgenes)
        )
    )
  return(mslysosomalclass.df)
}


#' Title
#'
#' @param musmusculusentrezgenedbconnection 
#'
#' @return
#' @export
#'
#' @examples
findNumberOfskeletalmuscleGenesPerChromosome <- function(musmusculusentrezgenedbconnection) {
  
  chr1mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "1", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1mmgenes)
  chr2mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "2", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  chr3mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "3", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr4mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "4", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  chr5mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "5", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )    #jsonlite::fromJSON(chr1mmgenes)
  
  chr6mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "6", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr7mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "7", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr8mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "8", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr9mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "9", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1mmgenes)
  
  chr10mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "10", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr11mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "11", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr12mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "12", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1mmgenes)
  chr13mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "13", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr14mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "14", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1mmgenes)
  chr15mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "15", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr16mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "16", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr17mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "17", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr18mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "18", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr19mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "19", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr20mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "20", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr21mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "21", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1mmgenes)
  chr22mmgenes <- musmusculusentrezgenedbconnection$find(
    query = '{"chromosome" : "22", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  print("Coercing into dataframe gene counts for mm genes across 22 chromosomes ")
  
  msskeletalmusclecodingclass.df <-
    data.frame(
      "Chromosomenumber" = 1:23,
      "Numberofskeletalmusclegenesperchromosome" =
        c(
          nrow(chr1mmgenes),
          nrow(chr2mmgenes),
          nrow(chr3mmgenes),
          nrow(chr4mmgenes),
          nrow(chr4mmgenes),
          nrow(chr5mmgenes),
          nrow(chr6mmgenes),
          nrow(chr7mmgenes),
          nrow(chr8mmgenes),
          nrow(chr9mmgenes),
          nrow(chr10mmgenes),
          nrow(chr11mmgenes),
          nrow(chr12mmgenes),
          nrow(chr13mmgenes),
          nrow(chr14mmgenes),
          nrow(chr15mmgenes),
          nrow(chr16mmgenes),
          nrow(chr17mmgenes),
          nrow(chr18mmgenes),
          nrow(chr19mmgenes),
          nrow(chr20mmgenes),
          nrow(chr21mmgenes),
          nrow(chr22mmgenes)
        )
    )
  return(msskeletalmusclecodingclass.df)
}

# Other protein types for which counts can be obtained
#non-receptortyrosinekinase
#alpha1-3-N-acetylgalactosaminyltransferaseandalpha1-3-galactosyltransferase
#tartrateresistant
#smoothmuscle