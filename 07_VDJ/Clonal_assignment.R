library(igraph)

# Function to compare V & J genes
compare_vj <- function(str_1, str_2) {
  match <- 0
  arr1 <- unlist(strsplit(str_1, ","))
  arr2 <- unlist(strsplit(str_2, ","))
  
  for (a1 in arr1) {
    for (a2 in arr2) {
      a1 <- gsub("\\*.*", "", a1)
      a2 <- gsub("\\*.*", "", a2)
      if (a1 == a2) {
        match <- 1
        return(match)
      }
    }
  }
  return(match)
}

# Function to compare CDR3 sequences
compare_cdr3 <- function(cdr3_1, cdr3_2, thr) {
  match <- 0
  if (nchar(cdr3_1) == nchar(cdr3_2)) {
    len <- nchar(cdr3_1)
    count <- sum(strsplit(cdr3_1, "")[[1]] != strsplit(cdr3_2, "")[[1]])
    per <- 1 - (count / len)
    
    if (per >= thr) {
      match <- 1
    }
  }
  return(match)
}


get_clonotype <- function(paired_data,thr=0.7){

  # Create a graph
  g <- graph.empty(n = nrow(paired_data), directed = FALSE)
  
  # Compare sequences and add edges to the graph
  for (i in 1:nrow(paired_data)) 
  {
    for (j in 1:nrow(paired_data)) 
    {
      if(i == j | 
         (nchar(paired_data$cdr3.x[i]) != nchar(paired_data$cdr3.x[j])) | 
         (nchar(paired_data$cdr3.y[i]) != nchar(paired_data$cdr3.y[j])) ){next}
      
      hv_check <- compare_vj(paired_data$v_call.x[i], paired_data$v_call.x[j])
      if (hv_check) 
      {
        hj_check <- compare_vj(paired_data$j_call.x[i], paired_data$j_call.x[j])
        if (hj_check) 
        {
          hcdr3_check <- compare_cdr3(paired_data$cdr3.x[i], paired_data$cdr3.x[j], thr)
          if (hcdr3_check) 
          {
            lv_check <- compare_vj(paired_data$v_call.y[i], paired_data$v_call.y[j])
            if (lv_check) 
            {
              lj_check <- compare_vj(paired_data$j_call.y[i], paired_data$j_call.y[j])
              if (lj_check) 
              {
                lcdr3_check <- compare_cdr3(paired_data$cdr3.y[i], paired_data$cdr3.y[j], thr)
                if (lcdr3_check) 
                {
                  # Add edge to the graph
                  g <- add_edges(g, c(i, j))
                } 
              } 
            }
          }
        }
      }
    }
  }
  
  # Find connected components
  clusters <- clusters(g)$membership
  paired_data <- data.frame(CloneID = clusters, paired_data)
  return(paired_data)
}

