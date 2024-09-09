#' Macro Factors - Commodities
#'
#' This function calculates the surprise impact of commodity factors on different segments based on their sensitivities to these factors.
#'
#' @param segment_classification A matrix indicating the classification of each observation into different segments.
#' Rows represent observations, and columns represent segments.
#' @param segments_with_positive_sensibility_to_surprise A list of vectors indicating the segments with a positive sensitivity to commodity surprises.
#' @param segments_with_negative_sensibility_to_surprise A list of vectors indicating the segments with a negative sensitivity to commodity surprises.
#' @param surprise_matrix A matrix containing the surprise values for different commodities and time periods.
#' Rows represent commodities, and columns represent time periods.
#'
#' @return A matrix representing the impact of commodity factors on different segments.
#' Rows represent observations, and columns represent time periods.
#'
#' @export
commodity_factor_surprise <- function(segment_classification, 
                                      segments_with_positive_sensibility_to_surprise,
                                      segments_with_negative_sensibility_to_surprise,
                                      surprise_matrix){
  #Check if 
  if (!(dim(segment_classification)[2] == dim(surprise_matrix)[2])){ #THe number of columns (time periods) should be equal
    stop("Number of columns between segment_classification and surprise_matrix should match.")
  } else {}
  if(!is.list(segments_with_positive_sensibility_to_surprise) | !is.list(segments_with_negative_sensibility_to_surprise)){
    stop("segments_with_positive_sensibility_to_surprise and segments_with_negative_sensibility_to_surprise should be lists.")
  } else {}
  
  if(length(segments_with_positive_sensibility_to_surprise) != length(segments_with_negative_sensibility_to_surprise) #There should be matching number sectors to each commodity surprise
  | length(segments_with_positive_sensibility_to_surprise) != nrow(surprise_matrix)){
    stop("There should be matching number of elements between segments_with_positive_sensibility_to_surprise, 
         segments_with_negative_sensibility_to_surprise and surprise_matrix")
  } else {}
 
  if(!all(rownames(surprise_matrix) %in% names(segments_with_positive_sensibility_to_surprise)) ||
     !all(rownames(surprise_matrix) %in% names(segments_with_negative_sensibility_to_surprise))){
    stop("surprise_matrix rownames should match names of segments lists")
  } else {}
      
    commodity_surprise_matrix <- matrix(0, #0 for those segments that are not expected to be impacted
                                        nrow = nrow(segment_classification),
                                        ncol = ncol(segment_classification))
    for(j in 1:ncol(surprise_matrix)){
      #Loop all commodities
      for(i in 1:nrow(surprise_matrix)){
        #Which companies frm segment_classification have positive/negative sensibility to surprise
        if(all(segments_with_positive_sensibility_to_surprise[[i]] != c("none"))){
          #Take positive sensibility ones
          cia_ref_positive_sensibility <- which(segment_classification[,j] %in% segments_with_positive_sensibility_to_surprise[[i]])
          if(!identical(cia_ref_positive_sensibility, integer(0))){ #Checks if there are matching CIAs
            #Take Surprise
            commodity_surprise_matrix[cia_ref_positive_sensibility, j] <- surprise_matrix[i, j]
          } else {}
         
        } else {}
        
        #Which companies frm segment_classification have positive/negative sensibility to surprise  
        if(all(segments_with_negative_sensibility_to_surprise[[i]] != c("none"))){
          #Take negative sensibility ones
          cia_ref_negative_sensibility <- which(segment_classification[,j] %in% segments_with_negative_sensibility_to_surprise[[i]])
          if(!identical(cia_ref_negative_sensibility, integer(0))){ #Checks if there are matching CIAs
          #Take Surprise
          commodity_surprise_matrix[cia_ref_negative_sensibility, j] <- surprise_matrix[i, j]*(-1)
        } else {}
        
      } else {}
    }
      
    }   
  #Return  matrix
  return(commodity_surprise_matrix)
}


