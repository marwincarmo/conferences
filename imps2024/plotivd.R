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
    # Extract the posterior mean and 95% credible interval for each random effect:
    posterior_samples <- do.call(rbind, lapply(.extract_to_mcmc(obj), FUN = function(x) x[, scale_ranef_pos]))
    ## Extract the posterior mean of the fixed effect:
    zeta <- mean( unlist( lapply(.extract_to_mcmc( obj ), FUN = function(x) mean(x[, "zeta[1]"])) ) )
    ## Extract the posterior mean of each random effect:
    u <- colMeans(posterior_samples)
    # 95% credible intervals:
    u_median <- apply(posterior_samples, 2, function(x) quantile(x, probs = 0.5))
    u_lower <- apply(posterior_samples, 2, function(x) quantile(x, probs = 0.025))
    u_upper <- apply(posterior_samples, 2, function(x) quantile(x, probs = 0.975))
    
    tau <- exp(zeta + u )
    tau_lower <- exp(zeta + u_lower)
    tau_upper <- exp(zeta + u_upper)
    
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
  
  ## Add tau to data frame -- ensure correct order
  df_pip <-
    cbind(df_pip[order(df_pip$id), ], tau )
  ## Make nudge scale dependent:
  ## (not used)
  ord <- (max(df_pip$ordered ) - min(df_pip$ordered ))/50
  nx <- (max(df_pip$tau ) - min(df_pip$tau ))/50
  
  if( type == "pip") {
    ## 
    plt <- ggplot(df_pip, aes(x = ordered, y = pip)) +
      geom_point(data = subset(df_pip, pip < pip_level), 
                 alpha = .3, size = 4, stroke = 1, shape = 21, , fill = "black", color = "black") +
      geom_point(data = subset(df_pip, pip >= pip_level), position = "jitter", 
                 aes(fill = pip), size = 4, stroke = 1, shape = 21, color = "black") +
      # geom_point(data = subset(df_pip, pip < pip_level), alpha = .3 , size = 4) +
      # geom_point(data = subset(df_pip, pip >= pip_level),
      #            aes(color = pip), #as.factor(id))
      #            , size = 4) +
      # geom_text(data = subset(df_pip, pip >= pip_level),
      #           aes(label = id),
      #           nudge_x = -4.5,
      #           #nudge_y = -.009,
      #           size = 4) +
      geom_abline(intercept = pip_level, slope = 0, lty =  3)+
      #geom_abline(intercept = pip_level - .5, slope = 0, lty =  3)+
      ylim(c(0, 1 ) ) + ggtitle(variable )+
      scale_fill_viridis_c(option = "plasma", alpha = 0.8, begin = 0.5) +
      guides(fill = "none")
    print(plt )
  } else if ( type == "funnel" ) {
    
    # ## Add tau to data frame -- ensure correct order
    # df_funnel <-
    #   cbind(df_pip[order(df_pip$id), ], tau )
    
    plt <- ggplot(df_pip, aes(x = tau, y = pip)) +
      geom_point(data = subset(df_pip, pip < pip_level), 
                 alpha = .3, size = 4, stroke = 1, shape = 21, fill = "black", color = "black") +
      geom_point(data = subset(df_pip, pip >= pip_level),
                 aes(fill = pip), size = 4, shape = 21,  position = "jitter") + 
      # geom_point(data = subset(df_funnel, pip < pip_level), alpha = .4, size=3) +
      # geom_point(data = subset(df_funnel, pip >= pip_level),
      #            aes(color = as.factor(id)), size=3) +
      labs(x = "Within-Cluster SD") +
      geom_text(data = subset(df_pip, pip >= pip_level),
                aes(label = id),
                nudge_x = -nx,
                #nudge_x = -.005,
                size = 4)+
      geom_abline(intercept = pip_level, slope = 0, lty =  3)+
      #geom_abline(intercept = pip_level - .5, slope = 0, lty =  3)+
      ylim(c(0, 1 ) )+ggtitle(variable) +
      scale_fill_viridis_c(option = "plasma", alpha = 0.8, begin = 0.5) +
      guides(fill = "none")
    print( plt )
  } else if ( type == "outcome") {
    df_y <- merge(df_pip,
                  aggregate(Y ~ group_id, data = obj$Y, FUN = mean),
                  by.x = "id", by.y = "group_id")
    df_y$tau <- tau
    nx <- (max(df_y$tau ) - min(df_y$tau ))/50
    ## 
    plt <- ggplot(df_y, aes(x = Y, y = pip)) +
      geom_point(data = subset(df_y, pip < pip_level), 
                 alpha = .3, stroke = 1, aes(size=tau),
                 shape = 21, fill = "black", color = "black") +
      geom_point(data = subset(df_y, pip >= pip_level),
                 aes(fill = pip, size = tau),  shape = 21, color = "black") +
      #geom_point(data = subset(df_y, pip < pip_level), aes(size=tau), alpha = .4) +
      # geom_point(data = subset(df_y, pip >= pip_level),
      #            aes(color = as.factor(id), size = tau)) +
      geom_text(data = subset(df_y, pip >= pip_level),
                aes(label = id),
                nudge_x = -.05,
                size = 4)+
      geom_abline(intercept = pip_level, slope = 0, lty =  3)+
      #geom_abline(intercept = pip_level - .5, slope = 0, lty =  3)+
      ylim(c(0, 1 ) ) + 
      ggtitle(variable ) +
      #scale_fill_manual(values = distinct_colors) +
      scale_fill_viridis_c(option = "plasma", alpha = 0.8, begin = 0.5) +
      guides(size = "none", fill="none")
    print(plt )
  } else if ( type == "caterpillar") {
    
    u_summary <- data.frame(
      RandomEffect = 1:length(u),
      Median = u_median,
      Lower95CI = u_lower,
      Upper95CI = u_upper,
      pip = df_pip$pip
    )
    
    u_ordered <- u_summary[order(u_summary[, "Median"]), ]
    u_ordered$ordered <- 1:nrow(u_summary )
    
    plt <- ggplot(u_ordered, aes(x = ordered, y = Median, color = Median)) +
      geom_pointrange(
        aes(ymin = Lower95CI, ymax = Upper95CI),
        size = 0.7,  # Thicker lines for better visibility
        linetype = "solid",
        color = "black",
        alpha = 0.7
      ) +
      geom_point(data = subset(u_ordered, pip < pip_level), 
                 size = 2.5, stroke = 1, aes(size=tau),
                 shape = 21, fill = "black", color = "black") +
      geom_point(data = subset(u_ordered, pip >= pip_level),
                 aes(fill = factor(category)), size = 2.5,  shape = 21, color = "black") +
      # geom_point(
      #   size = 2.5,  # Larger points
      #   stroke = 1,  # Stroke width
      #   shape = 21,  # Shape 21 allows for both fill and stroke
      #   aes(fill = Median),  # Set fill color to white for contrast
      #   color = "black"
      #   ) +
      #viridis::scale_fill_viridis(option = "D", direction = 1) +
      #scale_fill_viridis_c(option = "plasma", alpha = 0.8, begin = 0.5) +
      scale_fill_manual(values = distinct_colors) +
      geom_abline(intercept = 0, slope = 0, lty =  3) +
      guides(color = "none", fill = "none")
      
      
      
      # ggplot(u_ordered, aes(x = ordered, y = Mean)) +
      # geom_point() +  # Plot the mean of tau
      # geom_errorbar(aes(ymin = Lower95CI, ymax = Upper95CI), width = 0.2)+
      # viridis::scale_color_viridis(option = "D", direction = 1)
    print(plt)
    } else {
    stop("Invalid plot type. Please choose between 'pip', 'funnel' or 'outcome'.")
  }
  return(invisible(plt))  
}
distinct_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")

u_ordered$category <- cut(u_ordered$pip, breaks = nrow(u_ordered))
