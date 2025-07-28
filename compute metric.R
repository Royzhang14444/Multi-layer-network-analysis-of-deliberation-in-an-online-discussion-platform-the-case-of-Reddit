rm(list = ls())

library(igraph)
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(purrr)



###########define the dyadic conversation 

# Check if a sequence is strictly alternating 
# (no zeros allowed, only two unique values, and values must alternate)
is_strictly_alternating <- function(vec) {
  if ("0" %in% vec) return(FALSE)
  if (length(unique(vec)) != 2) return(FALSE)
  all(vec[-1] != vec[-length(vec)])
}

# Extract all dyadic sequences (length >= 3) from a user_id sequence that follow the strict alternation rule
extract_dyadics <- function(s) {
  tokens <- unlist(strsplit(s, "_"))
  n <- length(tokens)
  results <- list()
  
  for (start in 1:(n - 2)) {
    for (end in (start + 2):n) {
      subvec <- tokens[start:end]
      if (is_strictly_alternating(subvec)) {
        results[[length(results) + 1]] <- paste(subvec, collapse = "_")
      } else {
        break
      }
    }
  }
  unique(results)
}

# Extract dyadic conversations + corresponding structure_with_name segments (aligned positions)）
extract_dyadic_conversations <- function(thread_data_sink) {
  result <- list()
  
  for (i in seq_len(nrow(thread_data_sink))) {
    thread_id_val <- thread_data_sink$thread_id[i]
    structure_user <- thread_data_sink$structure_user_name[i]
    structure_name <- thread_data_sink$structure_with_name[i]
    
    dyadics <- extract_dyadics(structure_user)  
    
    if (length(dyadics) > 0) {
      user_parts <- strsplit(structure_user, "_")[[1]]
      struct_parts <- strsplit(structure_name, "_")[[1]]
      
      for (dyad in dyadics) {
        dyad_parts <- strsplit(dyad, "_")[[1]]
        dyad_len <- length(dyad_parts)
        
        # Find the start position of the dyadic sequence in user_parts
        match_pos <- NA
        for (j in 1:(length(user_parts) - dyad_len + 1)) {
          if (all(user_parts[j:(j + dyad_len - 1)] == dyad_parts)) {
            match_pos <- j
            break
          }
        }
        
        # If found and the structure is long enough, extract the corresponding structure segment
        if (!is.na(match_pos) && (match_pos + dyad_len - 1) <= length(struct_parts)) {
          truncated_structure <- paste(struct_parts[match_pos:(match_pos + dyad_len - 1)], collapse = "_")
          
          result[[length(result) + 1]] <- list(
            thread_id = thread_id_val,
            dyadic_conversation = dyad,
            structure_with_name = truncated_structure,
            conversation_length = dyad_len
          )
        }
      }
    }
  }
  
  # Return an empty data frame if no results
  if (length(result) == 0) {
    return(data.frame(
      thread_id = character(),
      dyadic_conversation = character(),
      structure_with_name = character(),
      conversation_length = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  final_df <- do.call(rbind, lapply(result, as.data.frame, stringsAsFactors = FALSE))
  rownames(final_df) <- NULL
  return(final_df)
}

remove_contained_structure_rows <- function(df, column = "structure_with_name") {
  if (!column %in% colnames(df)) {
    stop("Specified column does not exist in the dataframe.")
  }
  
  structure_vec <- df[[column]]
  
  # 1. Remove rows that are strictly contained in other rows (excluding exact matches)
  is_contained <- sapply(seq_along(structure_vec), function(i) {
    target <- structure_vec[i]
    others <- structure_vec[-i]
    any(others != target & str_detect(others, fixed(target)))
  })
  
  df_filtered <- df[!is_contained, ]
  
  # 2. Remove exact duplicates, keeping only one
  df_unique <- df_filtered %>%
    distinct(!!sym(column), .keep_all = TRUE)
  
  return(df_unique)
}





folder_path <- "G:/BaiduSyncdisk/Roy/upload_to_github/collected_data"  # replace this with the folder path of the collected Reddit data

activity_layer_info_total <-data.frame() # To create a dataframe to record the activity layer information

file_names <- list.files(folder_path,full.names = TRUE)

reddit_number=1

dyadic_df_total <- data.frame(
  thread_id = NA_character_,
  dyadic_conversation = NA_character_,
  structure_with_name = NA_character_,
  conversation_length=NA_character_,
  stringsAsFactors = FALSE
)


for (file in file_names) {
  processed_data<- read.csv(file) # read the collected Reddit data

  # Calculate the depth of each comment by counting the number of underscores in the 'structure_with_name' column
  processed_data<- mutate(processed_data, depth = str_count(structure_with_name, "_"))
  
  # Create a dataframe where each row corresponds to a unique post (thread), enabling the computation of post-level properties.
  
  activity_layer_info<-as.data.frame(unique(processed_data$thread_id))
  
  colnames(activity_layer_info)<- 'thread_id' # rename the colnames of 'activity_layer_info'
  activity_layer_info$subreddit <- processed_data$subreddit[1] # add subreddit name in the dataframe
  
  # Iterate over each thread (post)
  for (post_index in 1:nrow(activity_layer_info)){
    # Find the row(s) corresponding to the target thread and extract the thread data 
    thread_data<- processed_data[which(processed_data$thread_id==activity_layer_info$thread_id[post_index]),]
    # Record the number of comments for the thread
    activity_layer_info$no_comment[post_index] <-thread_data$num_comments[1] 
    # Compute the maximum depth of the thread
    activity_layer_info$max_depth[post_index] <- max(thread_data$depth) 
    
    #find the sink node, the way was splite 'structure_with_name' by '_', each peice is a unique comment name, the sink node is ones that only show up once.
    structure_split_strings <- strsplit(thread_data$structure_with_name, "_")
    structure_vector <- unlist(structure_split_strings) 
    stringcounts <- table(structure_vector)
    sink_node <- names(stringcounts[stringcounts == 1])
    sink_node_depth <- thread_data$depth[unique(unlist(sapply(sink_node, function(x) grep(x, thread_data$structure_with_name))))] # Find the depth of each sink node by matching its name in 'structure_with_name'
    sink_node_df <- data.frame(
      sink_node = sink_node,
      sink_node_depth = sink_node_depth,
      stringsAsFactors = FALSE
    )
    activity_layer_info$ave_depth[post_index] <- mean(sink_node_depth)
    
  #obtain maximum and average width by first table 'depth' of each comment and then find the maximum/average freq
    activity_layer_info$max_width[post_index] <- max(as.data.frame(table(thread_data$depth))$Freq)
    activity_layer_info$ave_width[post_index] <- mean(as.data.frame(table(thread_data$depth))$Freq)
    activity_layer_info$unique_user[post_index] <- length(unique(thread_data$user))
    

    ##################################################compute Dyadic Conversation
    ########################################################################################## 
    # The key steps are:
    # 1. `structure_with_name` represents the reply chain where each element is a thread_id. 
    #    Replace these thread_ids with user names so we can identify who is replying to whom.
    # 2. Focus only on sink nodes that involve more than 3 comments, since a dyadic conversation 
    #    requires at least 3 comments. This also helps reduce computation.
    # 3. Extract all dyadic conversations from these chains and record both the 
    #    `structure_with_name` and `structure_user_name`. At this stage, many duplicate 
    #    dyadic conversations will appear because different sink-node chains can capture 
    #    the same dyadic conversation.
    # 4. Remove duplicate dyadic conversations by using the `structure_with_name` as a key.
    
     
    
    
    
 
    # First, replace the first element of structure_with_name with the comment's name
    thread_data$structure_user_name <-sub("^[^_]+", thread_data$author[1], thread_data$structure_with_name) 
    
    # Create a mapping vector from comm_id to user
    commid_to_user <- setNames(thread_data$user, thread_data$comm_id)
    
    # Function: replace all comm_id values in a string with the corresponding user ID
    
    replace_commid_with_user <- function(structure_str) {
      
      # Split the string into parts by underscore
      parts <- str_split(structure_str, "_")[[1]]
      
      # Replace each part if it exists in the mapping, otherwise keep it as is
      replaced <- sapply(parts, function(x) ifelse(x %in% names(commid_to_user), commid_to_user[x], x))
      # Join the parts back into a string
      paste0(replaced, collapse = "_")
    }
    
    # Apply to the entire column: generate the updated structure_user_name column
    thread_data$structure_user_name <- sapply(thread_data$structure_user_name, replace_commid_with_user)
 

    
    
    #find the sink node that is larger than 3 comments
    sink_node_large <- sink_node_df[which(sink_node_df$sink_node_depth>=2),]
   
    
    if  (nrow(sink_node_large)!=0){
      
      thread_data_sink <- thread_data[which(thread_data$comm_id%in% sink_node_large$sink_node),]
      dyadic_df_post <- extract_dyadic_conversations(thread_data_sink) 
      dyadic_df_post$conversation_length <- sapply(strsplit(as.character(dyadic_df_post$dyadic_conversation), "_"), length)
      
      dyadic_df_post_cleaned <- remove_contained_structure_rows(dyadic_df_post)
      dyadic_df_total=rbind(dyadic_df_total,dyadic_df_post_cleaned) 
      activity_layer_info$dyadic_number[post_index]<-nrow(dyadic_df_post_cleaned)
      
      if (nrow(dyadic_df_post_cleaned) == 0) {
        activity_layer_info$dyadic_ave_length[post_index] <- 0
      } else {
        activity_layer_info$dyadic_ave_length[post_index] <- mean(dyadic_df_post_cleaned$conversation_length)
      }
      
    }
    else{activity_layer_info$dyadic_number[post_index] <- 0
    activity_layer_info$dyadic_ave_length[post_index] <- 0}
  }
  reddit_number=reddit_number+1
  print(reddit_number)
  activity_layer_info_total <- rbind(activity_layer_info_total,activity_layer_info)   
  
}

write.csv(activity_layer_info_total,'G:/BaiduSyncdisk/Roy/upload_to_github/new_activity.csv')



