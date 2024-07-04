library(ggplot2)
library(ivd)
library(coda)

.extract_to_mcmc <- function(obj) {
  e_to_mcmc <- lapply(obj$samples, FUN = function(x) mcmc(x$samples))
  return(e_to_mcmc)
}

.summary_table <- function(stats, Kr ) {
  ## Convert row names into a format that makes it easy to identify the rows to exclude
  rows_to_exclude <- c()
  for (row_name in rownames(stats)) {
    if (grepl("^R\\[", row_name)) {
      ## Extract the numeric indices from the row name
      elements <- as.numeric(unlist(regmatches(row_name, gregexpr("[0-9]+", row_name))))
      if (length(elements) == 2) {
        ## Exclude if row index is less than or equal to column index
        ## This removes both the diagonal and the upper triangular part
        if (elements[1] <= elements[2]) {
          rows_to_exclude <- c(rows_to_exclude, row_name)
        }
      }
    } else if (grepl("^ss\\[", row_name)) {
      ## Exclude the rows that pertain to location as SS is always 1
      row_number <- as.numeric(unlist(regmatches(row_name, gregexpr("[0-9]+", row_name)))[1])
      if (row_number <= Kr) {
        rows_to_exclude <- c(rows_to_exclude, row_name)
      }
    } else if (grepl("^sigma\\_rand\\[", row_name)) {
      ## Extract the numeric indices from the row name
      elements <- as.numeric(unlist(regmatches(row_name, gregexpr("[0-9]+", row_name))))
      if (length(elements) == 2) {
        ## Keep only diagonal elements by excluding if row index is not equal to column index
        if (elements[1] != elements[2]) {
          rows_to_exclude <- c(rows_to_exclude, row_name)
        }
      }
    } else if (grepl("^u\\[", row_name)) {
      rows_to_exclude <- c(rows_to_exclude,  row_name )
    }
  }
  ## Exclude rows
  stats_filtered <- stats[!rownames(stats) %in% rows_to_exclude, ]
  return(stats_filtered )
}

plot.ivd <- function(x, type = "pip", pip_level = .75, variable = NULL, col_id = TRUE, legend = TRUE, ...) {
  obj <- x
  ## Get scale variable names
  ranef_scale_names <- colnames(obj$Z_scale)
  fixef_scale_names <- colnames(obj$X_scale)
  
  col_names <- dimnames(.summary_table(obj$samples[[1]]$samples ))[[2]]
  Kr <- obj$nimble_constants$Kr
  
  cols_to_keep <- c( )
  ## Find spike and slab variables
  col_ss <- col_names[ grepl( "^ss\\[", col_names ) ]
  for(col_name in col_ss) {
    col_number <- as.numeric(unlist(regmatches(col_name, gregexpr("[0-9]+", col_name)))[1])
    if(col_number >  Kr ) {
      cols_to_keep <- c(cols_to_keep,  col_name )
    }
  }
  
  ## Subset each MCMC matrix to keep only the relevant columns
  subsamples <- lapply(.extract_to_mcmc(obj), function(x) x[, cols_to_keep])
  
  ## Calculate column means for each subsetted MCMC matrix
  means_list <- lapply(subsamples, colMeans)
  
  ## Average across the lists and chains
  final_means <- Reduce("+", means_list) / length(means_list)
  
  ## assign the means to the specific random effects
  ss_means <- list()
  ## Select the ss effect(s)
  Sr <- obj$nimble_constants$Sr
  for(i in 1:Sr ) {
    index <- paste0("\\[",  i+Kr)
    position_ss_value <- grepl(index, names(means_list[[1]]) )
    ss_means[[i]] <- final_means[position_ss_value]
  }
  
  ## Get number of random scale effects:
  no_ranef_s <- obj$nimble_constants$Sr
  
  
  ## With multiple random effects, ask user which one to be plotted:
  if(no_ranef_s == 1) {  
    ## Define ordered dataset
    df_pip <- data.frame(id = seq_len(length(ss_means[[1]])),
                         pip = ss_means[[1]])
    df_pip <- df_pip[order(df_pip$pip), ]
    df_pip$ordered <- 1:nrow(df_pip)
  } else if (no_ranef_s > 1 ) {
    if(is.null(variable)) {
      ## Prompt user for action when there are multiple random effects
      variable <- readline(prompt="There are multiple random effects. Please provide the variable name to be plotted or type 'list' \n(or specify as plot(fitted, type = 'funnel', variable = 'variable_name'): ")
      if (tolower(variable) == "list") {
        variable <- readline(prompt = cat(ranef_scale_names, ": "))
      }
    }
    
    ## Find position of user requested random effect
    scale_ranef_position_user <- which(ranef_scale_names == variable)
    
    ## Define ordered dataset
    df_pip <- data.frame(id = seq_len(length(ss_means[[scale_ranef_position_user]])),
                         pip = ss_means[[scale_ranef_position_user]])
    df_pip <- df_pip[order(df_pip$pip), ]
    df_pip$ordered <- 1:nrow(df_pip)
  }
  
  
  ## find scale random effects
  ## Extract numbers and find locations
  column_indices <- sapply(col_names, function(x) {
    if (grepl("^u\\[", x)) {  # Check if the name starts with 'u['
      ## Extracting numbers
      nums <- as.numeric(unlist(strsplit(gsub("[^0-9,]", "", x), ",")))
      ## Check if second number (column index) is greater than Kr
      return(nums[2] > Kr)
    } else {
      return(FALSE )
    }
  })
  
  ## Indices of columns where column index is greater than Kr
  scale_ranef_pos <- which(column_indices)
  
  ## Create tau locally
  if(no_ranef_s == 1) {
    ## Extract the posterior mean of the fixed effect:
    zeta <- mean( unlist( lapply(.extract_to_mcmc( obj ), FUN = function(x) mean(x[, "zeta[1]"])) ) )
    ## Extract the posterior mean of each random effect:
    u <- colMeans(do.call(rbind, lapply(.extract_to_mcmc( obj ), FUN = function(x) colMeans(x[, scale_ranef_pos]))))
    tau <- exp(zeta + u )
  } else if (no_ranef_s > 1 ) {
    ## if(is.null(variable)) {
    ##   ## Prompt user for action when there are multiple random effects
    ##   variable <- readline(prompt="There are multiple random effects. Please provide the variable name to be plotted or type 'list' \n(or specify as plot(fitted, type = 'funnel', variable = 'variable_name'): ")
    ##   if (tolower(variable) == "list") {
    ##     variable <- readline(prompt = cat(ranef_scale_names, ": "))
    ##   }
    ## }
    
    ## Find position of user requested random effect
    scale_ranef_position_user <-
      which(ranef_scale_names == variable)
    
    ## Find position of user requested fixed effect
    ## TODO: When interactions are present plot will change according to moderator...
    ## Currently only main effect is selected
    scale_fixef_position_user <-
      which(fixef_scale_names == variable)
    
    ## Use ranef_position_user to select corresponding fixed effect
    zeta <- mean( unlist( lapply(.extract_to_mcmc(obj), FUN = function(x) mean(x[, paste0("zeta[", scale_fixef_position_user, "]")])) ) )
    
    ## Extract the posterior mean of each random effect:        
    pos <- scale_ranef_pos[ grepl( paste0(Kr + scale_ranef_position_user, "\\]"),  names(scale_ranef_pos ) ) ]
    
    u <-
      colMeans(do.call(rbind, lapply(.extract_to_mcmc(obj ), FUN = function(x) colMeans(x[, pos]))))
    tau <- exp(zeta + u )
    
  } else {
    print("Invalid action specified. Exiting.")
  }
  
  if( type == "pip") {
    ## 
    plt <- ggplot(df_pip, aes(x = ordered, y = pip)) +
      geom_point(data = subset(df_pip, pip < pip_level), alpha = .4 , size = 3) +
      geom_point(data = subset(df_pip, pip >= pip_level),
                 aes(color = as.factor(id)), size = 3) +
      geom_text(data = subset(df_pip, pip >= pip_level),
                aes(label = id),
                nudge_x = -10,
                size = 3) +
      geom_abline(intercept = pip_level, slope = 0, lty =  3)+
      geom_abline(intercept = pip_level - .5, slope = 0, lty =  3)+
      ylim(c(0, 1 ) ) + ggtitle(variable )+
      guides(color = "none")
    print(plt )
  } else if ( type == "funnel" ) {
    
    ## Add tau to data frame -- ensure correct order
    df_funnel <-
      cbind(df_pip[order(df_pip$id), ], tau )
    ## Make nudge scale dependent:
    ## (not used)
    nx <- (max(df_funnel$tau ) - min(df_funnel$tau ))/50
    
    plt <- ggplot(df_funnel, aes(x = tau, y = pip)) +
      geom_point(data = subset(df_funnel, pip < pip_level), alpha = .4, size=3) +
      geom_point(data = subset(df_funnel, pip >= pip_level),
                 aes(color = as.factor(id)), size=3) +
      labs(x = "Within-Cluster SD") +
      geom_text(data = subset(df_funnel, pip >= pip_level),
                aes(label = id),
                nudge_x = -.005,
                size = 3)+
      geom_abline(intercept = pip_level, slope = 0, lty =  3)+
      geom_abline(intercept = pip_level - .5, slope = 0, lty =  3)+
      ylim(c(0, 1 ) )+ggtitle(variable) +
      guides(color = "none")
    print( plt )
  } else if ( type == "outcome") {
    df_y <- merge(df_pip,
                  aggregate(Y ~ group_id, data = obj$Y, FUN = mean),
                  by.x = "id", by.y = "group_id")
    df_y$tau <- tau
    ## 
    plt <- ggplot(df_y, aes(x = Y, y = pip)) +
      geom_point(data = subset(df_y, pip < pip_level), aes(size=tau), alpha = .4) +
      geom_point(data = subset(df_y, pip >= pip_level),
                 aes(color = as.factor(id), size = tau)) +
      geom_text(data = subset(df_y, pip >= pip_level),
                aes(label = id),
                nudge_x = -.07,
                size = 3)+
      geom_abline(intercept = pip_level, slope = 0, lty =  3)+
      geom_abline(intercept = pip_level - .5, slope = 0, lty =  3)+
      ylim(c(0, 1 ) ) + 
      ggtitle(variable ) +
      guides(size = "none", color="none")
    print(plt )
  } else {
    stop("Invalid plot type. Please choose between 'pip', 'funnel' or 'outcome'.")
  }
  return(invisible(plt))  
}
