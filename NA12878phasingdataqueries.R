source("MongoDBConnection.R")
library(ggplot2)
library(dplyr)
library(maps)
library(ggmap)


#' Title
#'
#' @param dbconnectionphasing 
#'
#' @return
#' @export
#'
#' @examples
NA12878phasingallrecords <- function(dbconnectionphasing) {
  phasingdata <-
    dbconnectionphasing$find('{}')
  
  return (phasingdata)
}

#' Title
#'
#' @param NA12878phasingallrecords 
#'
#' @return
#' @export
#'
#' @examples
NA12878phasingdataQueries <-
  function(NA12878phasingallrecords) {
    # find all entries for Hs tax id
    # hstax_id is count of all rows where tax id = 9606
    Na12878phasingallrecords %>% select({
    })
    return (NA12878phasingallrecords)
  }

#' Title
#'
#' @param phasingdata 
#'
#' @return
#' @export
#'
#' @examples
getchromosomephase1length <- function(phasingdata) {
  phasingdatachrandphaselengthphase1 <-
    phasingdata %>% select("X_chrom1", "end1", "start1", "end2", "start2")
  return (phasingdatachrandphaselengthphase1)
}

#' Title
#'
#' @param dbconnectionphasing 
#'
#' @return
#' @export
#'
#' @examples
updateNA12878phasingrecords <- function(dbconnectionphasing) {
  
  na12878phasingdata <-
    NA12878phasingallrecords(dbconnectionphasing)
  print(na12878phasingdata)
  dbconnectionphasing$find()
  dbconnectionphasing$update('{"chrom2":"chr1"}', '{"$set":{"chrom2": 1}}', multiple = TRUE)
  dbconnectionphasing$update('{"X_chrom1":"chr1"}',
                             '{"$set":{"X_chrom1": 1}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"X_chrom1":"chr5"}',
                             '{"$set":{"X_chrom1": 5}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr2"}',
                             '{"$set":{"chrom2": 2, "X_chrom1": 2}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr3"}',
                             '{"$set":{"chrom2": 3, "X_chrom1": 2}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr4"}',
                             '{"$set":{"chrom2": 4, "X_chrom1": 4}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr5"}',
                             '{"$set":{"chrom2": 5, "X_chrom1": 5}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr6"}',
                             '{"$set":{"chrom2": 6, "X_chrom1": 6}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr7"}',
                             '{"$set":{"chrom2": 7, "X_chrom1": 7}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr8"}',
                             '{"$set":{"chrom2": 8, "X_chrom1": 8}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr9"}',
                             '{"$set":{"chrom2": 9, "X_chrom1": 9}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr10"}',
                             '{"$set":{"chrom2": 10, "X_chrom1": 10}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr11"}',
                             '{"$set":{"chrom2": 10, "X_chrom1": 11}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr12"}',
                             '{"$set":{"chrom2": 10, "X_chrom1": 12}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr13"}',
                             '{"$set":{"chrom2": 10, "X_chrom1": 13}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr14"}',
                             '{"$set":{"chrom2": 10, "X_chrom1": 14}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr15"}',
                             '{"$set":{"chrom2": 10, "X_chrom1": 15}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr16"}',
                             '{"$set":{"chrom2": 10, "X_chrom1": 16}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr17"}',
                             '{"$set":{"chrom2": 10, "X_chrom1": 17}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr18"}',
                             '{"$set":{"chrom2": 10, "X_chrom1": 18}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr19"}',
                             '{"$set":{"chrom2": 10, "X_chrom1": 19}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr20"}',
                             '{"$set":{"chrom2": 10, "X_chrom1": 20}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr21"}',
                             '{"$set":{"chrom2": 10, "X_chrom1": 21}}',
                             multiple = TRUE)
  dbconnectionphasing$update('{"chrom2":"chr22"}',
                             '{"$set":{"chrom2": 10, "X_chrom1": 22}}',
                             multiple = TRUE)
}

#' Title
#'
#' @param dbconnectionphasing 
#'
#' @return
#' @export
#'
#' @examples
getdistributionofphasedsequencelengthphase1 <-
  function(dbconnectionphasing)
  {
    chromphase1data <-
      getchromosomephase1length(NA12878phasingallrecords(dbconnectionphasing))
    
    chromphase1data["lengthphase1"] = chromphase1data$end1 - chromphase1data$start1
    chromphase1data["lengthphase2"] = chromphase1data$end2 - chromphase1data$start2
    print(chromphase1data)
    
    # Compute aggregate on
    #db.sales.aggregate( [ { $project: { item: 1, total: { $subtract: [ { $add: [ "$price", "$fee" ] }, "$discount" ] } } } ] )
    
    # stats <- musmusculusphasingdatadbconnection$aggregate(
    #    `[ { "$project": { "X_chr": 1, "total": { "$subtract": [ "$end", "$start" ] } } } ]`
    #    )
    #  print(stats)
    
    #stats <- chromphase1data$aggregate(
    #  '[
    #   {"$group":{"_id":"$X_chrom1",
    #  "count": {"$sum":1},
    #  "average length on phase 1":{"$avg": "$lengthphase1"} }
    #  }
    #  ]'
    #)
    #names(stats) <- c("chromosome ", "count", "average length of phasing block")
    #print(stats)
    print(list(chromphase1data$Name))
    test <-
      aggregate(chromphase1data[, 6:7], list(chromphase1data$X_chrom1), mean)
    print(test)
    #ggplot(aes(Group.1, lengthphase1),
    #       data = test) + geom_col()
    #plotdata <-
    #    ((test[, c("X_chrom1",
    #               "lengthphase1")]))
    
    
    boxplotdata <- boxplot(
      chromphase1data$X_chrom1 ~ chromphase1data$lengthphase1,
      data = chromphase1data,
      xlab = "Chromosome number",
      ylab = "Phase length (phase 1)",
      border = "green",
      notch = TRUE,
      varwidth = TRUE,
      main = "Box plot distribution
              of average phasing length per chromosome",
      col.bg = "yellow",
      col.grid = "black"
    )
    return (boxplotdata)
  }

#' Title
#'
#' @param dbconnectionphasing 
#'
#' @return
#' @export
#'
#' @examples
getdistributionofphasedsequencelengthphase2 <-
  function(dbconnectionphasing)
  {
    chromphase1data <-
      getchromosomephase1length(NA12878phasingallrecords(dbconnectionphasing))
    
    chromphase1data["lengthphase1"] = chromphase1data$end1 - chromphase1data$start1
    chromphase1data["lengthphase2"] = chromphase1data$end2 - chromphase1data$start2
    print(chromphase1data)
    
    # Compute aggregate on
    #db.sales.aggregate( [ { $project: { item: 1, total: { $subtract: [ { $add: [ "$price", "$fee" ] }, "$discount" ] } } } ] )
    
    # stats <- musmusculusphasingdatadbconnection$aggregate(
    #    `[ { "$project": { "X_chr": 1, "total": { "$subtract": [ "$end", "$start" ] } } } ]`
    #    )
    #  print(stats)
    
    #stats <- chromphase1data$aggregate(
    #  '[
    #   {"$group":{"_id":"$X_chrom1",
    #  "count": {"$sum":1},
    #  "average length on phase 1":{"$avg": "$lengthphase1"} }
    #  }
    #  ]'
    #)
    #names(stats) <- c("chromosome ", "count", "average length of phasing block")
    #print(stats)
    print(list(chromphase1data$Name))
    test <-
      aggregate(chromphase1data[, 6:7], list(chromphase1data$X_chrom1), mean)
    print(test)
    
    
    boxplotdata <- boxplot(
      chromphase1data$X_chrom1 ~ chromphase1data$lengthphase1,
      data = chromphase1data,
      xlab = "Chromosome number",
      ylab = "Phase length (phase 2)",
      border = "green",
      notch = TRUE,
      varwidth = TRUE,
      main = "Box plot distribution
              of average phasing length per chromosome",
      col.bg = "yellow",
      col.grid = "black"
    )
    return (boxplotdata)
  }
