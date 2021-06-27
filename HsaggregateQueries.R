
#' Title
#'
#' @param homosapiensmongodbconnection 
#'
#' @return
#' @export
#'
#' @examples
findNumberOfProteinCodingGenesPerChromosome <- function(homosapiensmongodbconnection) {
  # chr1 genes which are protein coding for which we display chromsome, type of genes.
  # symbol, synonyms, dbxrefs and modification date greater than ???
  # chr1 genes which are protein coding for which we display chromsome, type of genes.
  # symbol, synonyms, dbxrefs
  #jsonlite::fromJSON(chr1hsgenes)
  chr1hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "1", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1hsgenes)
  chr2hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "2", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  chr3hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "3", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr4hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "4", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  chr5hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "5", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )    #jsonlite::fromJSON(chr1hsgenes)
  
  chr6hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "6", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr7hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "7", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr8hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "8", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr9hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "9", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1hsgenes)
  
  chr10hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "10", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr11hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "11", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr12hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "12", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1hsgenes)
  chr13hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "13", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr14hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "14", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1hsgenes)
  chr15hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "15", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr16hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "16", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr17hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "17", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr18hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "18", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr19hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "19", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr20hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "20", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr21hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "21", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr22hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "22", "type_of_gene" : "protein-coding"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  print("Coercing into dataframe gene counts for hs genes across 22 chromosomes ")
  
  hsproteincodingclass.df <-
    data.frame(
      "Chromosomenumber" = 1:23,
      "Numberofgenesperchromosome" =
        c(
          nrow(chr1hsgenes),
          nrow(chr2hsgenes),
          nrow(chr3hsgenes),
          nrow(chr4hsgenes),
          nrow(chr4hsgenes),
          nrow(chr5hsgenes),
          nrow(chr6hsgenes),
          nrow(chr7hsgenes),
          nrow(chr8hsgenes),
          nrow(chr9hsgenes),
          nrow(chr10hsgenes),
          nrow(chr11hsgenes),
          nrow(chr12hsgenes),
          nrow(chr13hsgenes),
          nrow(chr14hsgenes),
          nrow(chr15hsgenes),
          nrow(chr16hsgenes),
          nrow(chr17hsgenes),
          nrow(chr18hsgenes),
          nrow(chr19hsgenes),
          nrow(chr20hsgenes),
          nrow(chr21hsgenes),
          nrow(chr22hsgenes)
        )
    )
  return(hsproteincodingclass.df)
}



#' Title
#'
#' @param homosapiensmongodbconnection 
#'
#' @return
#' @export
#'
#' @examples
findNumberOflysosomalGenesPerChromosome <- function(homosapiensmongodbconnection) {
  
  chr1hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "1", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1hsgenes)
  chr2hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "2", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  chr3hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "3", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr4hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "4", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  chr5hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "5", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )    #jsonlite::fromJSON(chr1hsgenes)
  
  chr6hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "6", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr7hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "7", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr8hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "8", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr9hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "9", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1hsgenes)
  
  chr10hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "10", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr11hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "11", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr12hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "12", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1hsgenes)
  chr13hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "13", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr14hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "14", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1hsgenes)
  chr15hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "15", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr16hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "16", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr17hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "17", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr18hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "18", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr19hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "19", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr20hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "20", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr21hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "21", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr22hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "22", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  print("Coercing into dataframe gene counts for mm genes across 22 chromosomes ")
  
  hslysosomalclass.df <-
    data.frame(
      "Chromosomenumber" = 1:23,
      "Numberofgenesperchromosome" =
        c(
          nrow(chr1hsgenes),
          nrow(chr2hsgenes),
          nrow(chr3hsgenes),
          nrow(chr4hsgenes),
          nrow(chr4hsgenes),
          nrow(chr5hsgenes),
          nrow(chr6hsgenes),
          nrow(chr7hsgenes),
          nrow(chr8hsgenes),
          nrow(chr9hsgenes),
          nrow(chr10hsgenes),
          nrow(chr11hsgenes),
          nrow(chr12hsgenes),
          nrow(chr13hsgenes),
          nrow(chr14hsgenes),
          nrow(chr15hsgenes),
          nrow(chr16hsgenes),
          nrow(chr17hsgenes),
          nrow(chr18hsgenes),
          nrow(chr19hsgenes),
          nrow(chr20hsgenes),
          nrow(chr21hsgenes),
          nrow(chr22hsgenes)
        )
    )
  return(hslysosomalclass.df)
}


#' Title
#'
#' @param homosapiensmongodbconnection 
#'
#' @return
#' @export
#'
#' @examples
findNumberOfskeletalmuscleGenesPerChromosome <- function(homosapiensmongodbconnection) {
  
  chr1hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "1", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1hsgenes)
  chr2hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "2", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  chr3hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "3", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr4hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "4", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  chr5hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "5", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )    #jsonlite::fromJSON(chr1hsgenes)
  
  chr6hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "6", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr7hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "7", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr8hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "8", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr9hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "9", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1hsgenes)
  
  chr10hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "10", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr11hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "11", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr12hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "12", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1hsgenes)
  chr13hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "13", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr14hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "14", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  #jsonlite::fromJSON(chr1hsgenes)
  chr15hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "15", "type_of_gene" : "lysosomal"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr16hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "16", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr17hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "17", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr18hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "18", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr19hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "19", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr20hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "20", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr21hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "21", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  #jsonlite::fromJSON(chr1hsgenes)
  chr22hsgenes <- homosapiensmongodbconnection$find(
    query = '{"chromosome" : "22", "type_of_gene" : "skeletalmuscle"}',
    fields = '{"chromosome": true, "tax_id" :true, "GeneID" :true, "Symbol": true,
          "map_location": true, "dbXrefs" :true, "descripotion" : true}'
  )
  
  print("Coercing into dataframe gene counts for hs genes across 22 chromosomes ")
  
  hsskeletalmusclecodingclass.df <-
    data.frame(
      "Chromosomenumber" = 1:23,
      "Numberofgenesperchromosome" =
        c(
          nrow(chr1hsgenes),
          nrow(chr2hsgenes),
          nrow(chr3hsgenes),
          nrow(chr4hsgenes),
          nrow(chr4hsgenes),
          nrow(chr5hsgenes),
          nrow(chr6hsgenes),
          nrow(chr7hsgenes),
          nrow(chr8hsgenes),
          nrow(chr9hsgenes),
          nrow(chr10hsgenes),
          nrow(chr11hsgenes),
          nrow(chr12hsgenes),
          nrow(chr13hsgenes),
          nrow(chr14hsgenes),
          nrow(chr15hsgenes),
          nrow(chr16hsgenes),
          nrow(chr17hsgenes),
          nrow(chr18hsgenes),
          nrow(chr19hsgenes),
          nrow(chr20hsgenes),
          nrow(chr21hsgenes),
          nrow(chr22hsgenes)
        )
    )
  return(hsskeletalmusclecodingclass.df)
}

#non-receptortyrosinekinase
#alpha1-3-N-acetylgalactosaminyltransferaseandalpha1-3-galactosyltransferase
#tartrateresistant
#smoothmuscle