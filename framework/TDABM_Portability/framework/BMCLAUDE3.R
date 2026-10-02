# =============================================================================
# BMCLAUDE3.R
# Updated TDABM summary, stability and publication-plot utilities
#
# Changes from BMCLAUDE2.R:
#   * BMStats() constructs igraph objects directly rather than plotting inside
#     every repetition.
#   * BMStats() validates only the functions it actually requires.
#   * BMStats() accepts a reproducible base_seed argument.
#   * BMStatsMembership() preserves original observation identities and returns
#     membership, isolation and optional co-membership summaries.
#   * BMStatsRunSpec() and BMStatsBatch() connect BMStats to a run matrix.
#   * ColorIgraphPlot5aPublication() standardises final paper/article figures.
#   * ggplot2 is loaded when the file is sourced so bmsum() works immediately.
#   * BMStatsMembership() supports resumable batch checkpoints.
# =============================================================================

# Core plotting dependency. Loading it here ensures bgraph() and bmsum() work
# immediately after source("BMCLAUDE3.R").
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop(
    "Package 'ggplot2' is required by the BMStats plotting summaries. ",
    "Install it with install.packages('ggplot2').",
    call. = FALSE
  )
}
suppressPackageStartupMessages(library(ggplot2))

# Helper function to clean up connections
cleanup_connections <- function() {
  cat("=== CONNECTION CLEANUP ===\n")
  
  # Show current connections
  open_cons <- showConnections()
  cat("Open connections before cleanup:\n")
  if(nrow(open_cons) > 0) {
    print(open_cons)
  } else {
    cat("No open connections found\n")
  }
  
  # Stop any existing clusters
  tryCatch({
    stopImplicitCluster()
    cat("Stopped implicit cluster\n")
  }, error = function(e) {
    cat("No implicit cluster to stop\n")
  })
  
  # Close all connections
  tryCatch({
    closeAllConnections()
    cat("Closed all connections\n")
  }, error = function(e) {
    cat("Error closing connections:", e$message, "\n")
  })
  
  # Force garbage collection
  gc()
  
  # Show final state
  open_cons_after <- showConnections()
  cat("Open connections after cleanup:\n")
  if(nrow(open_cons_after) > 0) {
    print(open_cons_after)
    cat("WARNING: Some connections remain open\n")
  } else {
    cat("All connections successfully closed\n")
  }
  
  cat("=== CLEANUP COMPLETE ===\n")
}

latout<-function(data,filename){
 if(missing(filename)) filename<-"latout.txt"
 a001<-nrow(data)
 a002<-ncol(data)
 a003<-a002-1
 mat1<-matrix("",nrow=a001,ncol=1)
 mat1[,1]<-data[,1]
 for(i in 1:a001){
   for(k in 2:a003){
     mat1[i,1]<-paste(mat1[i,1],data[i,k],sep="&")
   }
 }
for(i in 1:a001){
 mat1[i,1]<-paste0(mat1[i,1],"&",data[i,a002],"\\","\\")
 }
 write.table(as.data.frame(mat1),filename,sep="\n",row.names=FALSE,quote=FALSE)
} 

# Plotting functions

ColorIgraphPlot5a <- function( outputFromBallMapper, showVertexLabels = TRUE , ltext="Coloration", showLegend = FALSE , minimal_ball_radius = 7 , maximal_ball_scale=20, maximal_color_scale=10 , seed_for_plotting = -1 , store_in_file = "" , default_x_image_resolution = 1024 , default_y_image_resolution = 1024 , number_of_colors = 100, minc = -99999, maxc = 99999)
{
  vertices = outputFromBallMapper$vertices
  vertices[,2] <- maximal_ball_scale*vertices[,2]/max(vertices[,2])+minimal_ball_radius
  net = igraph::graph_from_data_frame(outputFromBallMapper$edges,vertices = vertices,directed = F)

  jet.colors <- grDevices::colorRampPalette(c("red","orange","yellow","green","cyan","blue","violet"))
  color_spectrum <- jet.colors( number_of_colors )

  #and over here we map the pallete to the order of values on vertices
  min_ <- ifelse(minc==-99999,min(outputFromBallMapper$coloring),minc)
  max_ <- ifelse(maxc==99999,max(outputFromBallMapper$coloring),maxc)
  
  #In some cases, the maximal values are achieved, and then the balls are not colored. To avoid this situaiton, we shift the values by a bit to get something slightly below min and slightly above max (relative to the distnace between the minimym and the maximum).
  min_ <- min_-0.01*(max_-min_)
  max_ <- max_+0.01*(max_-min_)
  print(max_)
  color <- vector(length = length(outputFromBallMapper$coloring),mode="double")
  for ( i in 1:length( outputFromBallMapper$coloring ) )
  {
    position <- base::max(base::ceiling(number_of_colors*(outputFromBallMapper$coloring[i]-min_)/(max_-min_)),1)
    color[ i ] <- color_spectrum [ position ]
  }
  igraph::V(net)$color <- color

  if ( showVertexLabels == FALSE  )igraph::V(net)$label = NA

  if ( seed_for_plotting != -1 )base::set.seed(seed_for_plotting)

  if ( store_in_file != "" ) grDevices::png(store_in_file, default_x_image_resolution, default_y_image_resolution)

  igraph::V(net)$label.cex = 2 #Change this line if you would like to have labels of different sizes.
 
  par(oma=c(0,0,0,2.5))
  par(mar=c(0,0,0,3.5))
  
  graphics::plot(net,edge.width=5)

  fields::image.plot(legend.only=T, zlim=c(min_,max_), col=color_spectrum, legend.width=1.5, legend.shrink=0.8, axis.args=list(cex.axis=1.5), legend.args=list(text=ltext,side=4, font=1, line=5, cex=1.8))

  if ( store_in_file != "" )grDevices::dev.off()

  return(net)
}#ColorIgraphPlot5a

#

ColorIgraphPlot1a <- function( outputFromBallMapper, showVertexLabels = TRUE , ltext="Coloration", showLegend = FALSE , minimal_ball_radius = 7 , maximal_ball_scale=20, maximal_color_scale=10 , seed_for_plotting = -1 , store_in_file = "" , default_x_image_resolution = 1024 , default_y_image_resolution = 1024 , number_of_colors = 100, minc = -99999, maxc = 99999)
{
  vertices = outputFromBallMapper$vertices
  vertices[,2] <- maximal_ball_scale*vertices[,2]/max(vertices[,2])+minimal_ball_radius
  net = igraph::graph_from_data_frame(outputFromBallMapper$edges,vertices = vertices,directed = F)

  jet.colors <- grDevices::colorRampPalette(c("red","orange","yellow","green","cyan","blue","violet"))
  color_spectrum <- jet.colors( number_of_colors )

  #and over here we map the pallete to the order of values on vertices
  min_ <- ifelse(minc==-99999,min(outputFromBallMapper$coloring),minc)
  max_ <- ifelse(maxc==99999,max(outputFromBallMapper$coloring),maxc)
  
  #In some cases, the maximal values are achieved, and then the balls are not colored. To avoid this situaiton, we shift the values by a bit to get something slightly below min and slightly above max (relative to the distnace between the minimym and the maximum).
  min_ <- min_-0.01*(max_-min_)
  max_ <- max_+0.01*(max_-min_)
  print(max_)
  color <- vector(length = length(outputFromBallMapper$coloring),mode="double")
  for ( i in 1:length( outputFromBallMapper$coloring ) )
  {
    position <- base::max(base::ceiling(number_of_colors*(outputFromBallMapper$coloring[i]-min_)/(max_-min_)),1)
    color[ i ] <- color_spectrum [ position ]
  }
  igraph::V(net)$color <- color

  if ( showVertexLabels == FALSE  )igraph::V(net)$label = NA

  if ( seed_for_plotting != -1 )base::set.seed(seed_for_plotting)

  if ( store_in_file != "" ) grDevices::png(store_in_file, default_x_image_resolution, default_y_image_resolution)

  igraph::V(net)$label.cex = 2 #Change this line if you would like to have labels of different sizes.
 
  par(oma=c(0,0,0,2.5))
  par(mar=c(0,0,0,3.5))
  
  graphics::plot(net,edge.width=1)

  fields::image.plot(legend.only=T, zlim=c(min_,max_), col=color_spectrum, legend.width=1.5, legend.shrink=0.8, axis.args=list(cex.axis=1.5), legend.args=list(text=ltext,side=4, font=1, line=5, cex=1.8))

  if ( store_in_file != "" )grDevices::dev.off()

  return(net)
}#ColorIgraphPlot1a

ColorIgraphPlot2a <- function( outputFromBallMapper, showVertexLabels = TRUE , ltext="Coloration", showLegend = FALSE , minimal_ball_radius = 7 , maximal_ball_scale=20, maximal_color_scale=10 , seed_for_plotting = -1 , store_in_file = "" , default_x_image_resolution = 1024 , default_y_image_resolution = 1024 , number_of_colors = 100, minc = -99999, maxc = 99999)
{
  vertices = outputFromBallMapper$vertices
  vertices[,2] <- maximal_ball_scale*vertices[,2]/max(vertices[,2])+minimal_ball_radius
  net = igraph::graph_from_data_frame(outputFromBallMapper$edges,vertices = vertices,directed = F)

  jet.colors <- grDevices::colorRampPalette(c("red","orange","yellow","green","cyan","blue","violet"))
  color_spectrum <- jet.colors( number_of_colors )

  #and over here we map the pallete to the order of values on vertices
  min_ <- ifelse(minc==-99999,min(outputFromBallMapper$coloring),minc)
  max_ <- ifelse(maxc==99999,max(outputFromBallMapper$coloring),maxc)
  
  #In some cases, the maximal values are achieved, and then the balls are not colored. To avoid this situaiton, we shift the values by a bit to get something slightly below min and slightly above max (relative to the distnace between the minimym and the maximum).
  min_ <- min_-0.01*(max_-min_)
  max_ <- max_+0.01*(max_-min_)
  print(max_)
  color <- vector(length = length(outputFromBallMapper$coloring),mode="double")
  for ( i in 1:length( outputFromBallMapper$coloring ) )
  {
    position <- base::max(base::ceiling(number_of_colors*(outputFromBallMapper$coloring[i]-min_)/(max_-min_)),1)
    color[ i ] <- color_spectrum [ position ]
  }
  igraph::V(net)$color <- color

  if ( showVertexLabels == FALSE  )igraph::V(net)$label = NA

  if ( seed_for_plotting != -1 )base::set.seed(seed_for_plotting)

  if ( store_in_file != "" ) grDevices::png(store_in_file, default_x_image_resolution, default_y_image_resolution)

  igraph::V(net)$label.cex = 2 #Change this line if you would like to have labels of different sizes.
 
  par(oma=c(0,0,0,2.5))
  par(mar=c(0,0,0,3.5))
  
  graphics::plot(net,edge.width=2)

  fields::image.plot(legend.only=T, zlim=c(min_,max_), col=color_spectrum, legend.width=1.5, legend.shrink=0.8, axis.args=list(cex.axis=1.5), legend.args=list(text=ltext,side=4, font=1, line=5, cex=1.8))

  if ( store_in_file != "" )grDevices::dev.off()

  return(net)
}#ColorIgraphPlot2a

ColorIgraphPlot5a <- function( outputFromBallMapper, showVertexLabels = TRUE , ltext="Coloration", showLegend = FALSE , minimal_ball_radius = 7 , maximal_ball_scale=20, maximal_color_scale=10 , seed_for_plotting = -1 , store_in_file = "" , default_x_image_resolution = 1024 , default_y_image_resolution = 1024 , number_of_colors = 100, minc = -99999, maxc = 99999)
{
  vertices = outputFromBallMapper$vertices
  vertices[,2] <- maximal_ball_scale*vertices[,2]/max(vertices[,2])+minimal_ball_radius
  net = igraph::graph_from_data_frame(outputFromBallMapper$edges,vertices = vertices,directed = F)

  jet.colors <- grDevices::colorRampPalette(c("red","orange","yellow","green","cyan","blue","violet"))
  color_spectrum <- jet.colors( number_of_colors )

  #and over here we map the pallete to the order of values on vertices
  min_ <- ifelse(minc==-99999,min(outputFromBallMapper$coloring),minc)
  max_ <- ifelse(maxc==99999,max(outputFromBallMapper$coloring),maxc)
  
  #In some cases, the maximal values are achieved, and then the balls are not colored. To avoid this situaiton, we shift the values by a bit to get something slightly below min and slightly above max (relative to the distnace between the minimym and the maximum).
  min_ <- min_-0.01*(max_-min_)
  max_ <- max_+0.01*(max_-min_)
  print(max_)
  color <- vector(length = length(outputFromBallMapper$coloring),mode="double")
  for ( i in 1:length( outputFromBallMapper$coloring ) )
  {
    position <- base::max(base::ceiling(number_of_colors*(outputFromBallMapper$coloring[i]-min_)/(max_-min_)),1)
    color[ i ] <- color_spectrum [ position ]
  }
  igraph::V(net)$color <- color

  if ( showVertexLabels == FALSE  )igraph::V(net)$label = NA

  if ( seed_for_plotting != -1 )base::set.seed(seed_for_plotting)

  if ( store_in_file != "" ) grDevices::png(store_in_file, default_x_image_resolution, default_y_image_resolution)

  igraph::V(net)$label.cex = 2 #Change this line if you would like to have labels of different sizes.
 
  par(oma=c(0,0,0,2.5))
  par(mar=c(0,0,0,3.5))
  
  graphics::plot(net,edge.width=5)

  fields::image.plot(legend.only=T, zlim=c(min_,max_), col=color_spectrum, legend.width=1.5, legend.shrink=0.8, axis.args=list(cex.axis=1.5), legend.args=list(text=ltext,side=4, font=1, line=5, cex=1.8))

  if ( store_in_file != "" )grDevices::dev.off()

  return(net)
}#ColorIgraphPlot5a

#

ColorIgraphPlot1b <- function( outputFromBallMapper, showVertexLabels = TRUE , ltext="Coloration", showLegend = FALSE , minimal_ball_radius = 7 , maximal_ball_scale=20, maximal_color_scale=10 , seed_for_plotting = -1 , store_in_file = "" , default_x_image_resolution = 1024 , default_y_image_resolution = 1024 , number_of_colors = 100, minc = -99999, maxc = 99999)
{
  vertices = outputFromBallMapper$vertices
  vertices[,2] <- maximal_ball_scale*vertices[,2]/max(vertices[,2])+minimal_ball_radius
  net = igraph::graph_from_data_frame(outputFromBallMapper$edges,vertices = vertices,directed = F)

  jet.colors <- grDevices::colorRampPalette(c("red","orange","yellow","green","cyan","blue","violet"))
  color_spectrum <- jet.colors( number_of_colors )

  #and over here we map the pallete to the order of values on vertices
  min_ <- ifelse(minc==-99999,min(outputFromBallMapper$coloring),minc)
  max_ <- ifelse(maxc==99999,max(outputFromBallMapper$coloring),maxc)
  
  #In some cases, the maximal values are achieved, and then the balls are not colored. To avoid this situaiton, we shift the values by a bit to get something slightly below min and slightly above max (relative to the distnace between the minimym and the maximum).
  min_ <- min_-0.01*(max_-min_)
  max_ <- max_+0.01*(max_-min_)
  print(max_)
  color <- vector(length = length(outputFromBallMapper$coloring),mode="double")
  for ( i in 1:length( outputFromBallMapper$coloring ) )
  {
    position <- base::max(base::ceiling(number_of_colors*(outputFromBallMapper$coloring[i]-min_)/(max_-min_)),1)
    color[ i ] <- color_spectrum [ position ]
  }
  igraph::V(net)$color <- color

  if ( showVertexLabels == FALSE  )igraph::V(net)$label = NA

  if ( seed_for_plotting != -1 )base::set.seed(seed_for_plotting)

  if ( store_in_file != "" ) grDevices::png(store_in_file, default_x_image_resolution, default_y_image_resolution)

  igraph::V(net)$label.cex = 1 #Change this line if you would like to have labels of different sizes.
 
  par(oma=c(0,0,0,2.5))
  par(mar=c(0,0,0,3.5))
  
  graphics::plot(net,edge.width=1)

  fields::image.plot(legend.only=T, zlim=c(min_,max_), col=color_spectrum, legend.width=1.5, legend.shrink=0.8, axis.args=list(cex.axis=1.5), legend.args=list(text=ltext,side=4, font=1, line=5, cex=1.8))

  if ( store_in_file != "" )grDevices::dev.off()

  return(net)
}#ColorIgraphPlot1b

ColorIgraphPlot2b <- function( outputFromBallMapper, showVertexLabels = TRUE , ltext="Coloration", showLegend = FALSE , minimal_ball_radius = 7 , maximal_ball_scale=20, maximal_color_scale=10 , seed_for_plotting = -1 , store_in_file = "" , default_x_image_resolution = 1024 , default_y_image_resolution = 1024 , number_of_colors = 100, minc = -99999, maxc = 99999)
{
  vertices = outputFromBallMapper$vertices
  vertices[,2] <- maximal_ball_scale*vertices[,2]/max(vertices[,2])+minimal_ball_radius
  net = igraph::graph_from_data_frame(outputFromBallMapper$edges,vertices = vertices,directed = F)

  jet.colors <- grDevices::colorRampPalette(c("red","orange","yellow","green","cyan","blue","violet"))
  color_spectrum <- jet.colors( number_of_colors )

  #and over here we map the pallete to the order of values on vertices
  min_ <- ifelse(minc==-99999,min(outputFromBallMapper$coloring),minc)
  max_ <- ifelse(maxc==99999,max(outputFromBallMapper$coloring),maxc)
  
  #In some cases, the maximal values are achieved, and then the balls are not colored. To avoid this situaiton, we shift the values by a bit to get something slightly below min and slightly above max (relative to the distnace between the minimym and the maximum).
  min_ <- min_-0.01*(max_-min_)
  max_ <- max_+0.01*(max_-min_)
  print(max_)
  color <- vector(length = length(outputFromBallMapper$coloring),mode="double")
  for ( i in 1:length( outputFromBallMapper$coloring ) )
  {
    position <- base::max(base::ceiling(number_of_colors*(outputFromBallMapper$coloring[i]-min_)/(max_-min_)),1)
    color[ i ] <- color_spectrum [ position ]
  }
  igraph::V(net)$color <- color

  if ( showVertexLabels == FALSE  )igraph::V(net)$label = NA

  if ( seed_for_plotting != -1 )base::set.seed(seed_for_plotting)

  if ( store_in_file != "" ) grDevices::png(store_in_file, default_x_image_resolution, default_y_image_resolution)

  igraph::V(net)$label.cex = 1 #Change this line if you would like to have labels of different sizes.
 
  par(oma=c(0,0,0,2.5))
  par(mar=c(0,0,0,3.5))
  
  graphics::plot(net,edge.width=2)

  fields::image.plot(legend.only=T, zlim=c(min_,max_), col=color_spectrum, legend.width=1.5, legend.shrink=0.8, axis.args=list(cex.axis=1.5), legend.args=list(text=ltext,side=4, font=1, line=5, cex=1.8))

  if ( store_in_file != "" )grDevices::dev.off()

  return(net)
}#ColorIgraphPlot2b



BMStats <- function(xvars, yvar,
                    epsf, epsint, x001, rep,
                    thresh2,                   # min ball size for summary
                    xvar = TRUE,               # include x-variable range summaries
                    ncores = 4,
                    bmcpp_path = NULL,         # optional path to BallMapper.cpp
                    coref_path = NULL,         # optional path to coref.R
                    checkpoint_file = NULL,    # optional: save intermediate results
                    checkpoint_every = 0,     # 0 = no checkpointing
                    base_seed = 12345) {      # reproducible landmark-order seeds
  
  # ---- Input validation ----
  if (is.null(bmcpp_path)) {
    stop("bmcpp_path must be provided - path to BallMapper.cpp file")
  }
  if (is.null(coref_path)) {
    stop("coref_path must be provided - path to coref.R file")
  }
  if (!file.exists(bmcpp_path)) {
    stop("BallMapper.cpp file not found at: ", bmcpp_path)
  }
  if (!file.exists(coref_path)) {
    stop("coref.R file not found at: ", coref_path)
  }
  
  # ---- Packages ----
  suppressPackageStartupMessages({
    library(doSNOW)
    library(foreach)
    library(dplyr)
    library(igraph)
    library(Rcpp)
  })
  
  # ---- Inputs ----
  cat("Processing inputs...\n")
  cat("Input xvars type:", class(xvars), "\n")
  cat("Input yvar type:", class(yvar), "\n")
  
  # Handle xvars - ensure it's a proper data.frame with numeric columns
  if(is.list(xvars) && !is.data.frame(xvars)) {
    cat("Converting list to data.frame...\n")
    xvars <- as.data.frame(xvars, stringsAsFactors = FALSE)
  } else {
    xvars <- as.data.frame(xvars, stringsAsFactors = FALSE)
  }
  
  # Check for and fix list columns in xvars
  list_cols <- sapply(xvars, is.list)
  if(any(list_cols)) {
    cat("Found list columns in xvars:", names(xvars)[list_cols], "\n")
    for(col in names(xvars)[list_cols]) {
      cat("Converting column", col, "from list to numeric...\n")
      xvars[[col]] <- tryCatch({
        as.numeric(unlist(xvars[[col]]))
      }, error = function(e) {
        warning("Could not convert column ", col, " to numeric: ", e$message)
        rep(NA_real_, nrow(xvars))
      })
    }
  }
  
  # Ensure all xvars columns are numeric
  for(i in 1:ncol(xvars)) {
    if(!is.numeric(xvars[[i]])) {
      cat("Converting column", i, "to numeric...\n")
      xvars[[i]] <- tryCatch({
        as.numeric(xvars[[i]])
      }, error = function(e) {
        warning("Could not convert column ", i, " to numeric: ", e$message)
        rep(NA_real_, nrow(xvars))
      })
    }
  }
  
  # Handle yvar - ensure it's a numeric vector
  if(is.list(yvar)) {
    cat("Converting yvar from list to numeric...\n")
    yvar <- tryCatch({
      as.numeric(unlist(yvar))
    }, error = function(e) {
      stop("Could not convert yvar from list to numeric: ", e$message)
    })
  } else {
    yvar <- tryCatch({
      as.numeric(yvar)
    }, error = function(e) {
      stop("Could not convert yvar to numeric: ", e$message)
    })
  }
  
  # Validate dimensions
  cat("xvars dimensions:", nrow(xvars), "x", ncol(xvars), "\n")
  cat("yvar length:", length(yvar), "\n")
  
  if(nrow(xvars) != length(yvar)) {
    stop("Dimension mismatch: xvars has ", nrow(xvars), " rows but yvar has ", length(yvar), " elements")
  }
  
  n_xvars <- ncol(xvars)
  cat("Number of predictor variables:", n_xvars, "\n")
  
  # Column names
  xv_colnames <- as.vector(sapply(1:n_xvars, function(h) 
    c(paste0("mean_xvar",h), paste0("min_xvar",h), paste0("max_xvar",h))))
  xv_colnames <- c(xv_colnames, "rep","eps")
  
  m_colnames <- c("eps","rep","nballs","minc","maxc","sdc",
                  "minb","maxb","sdb","zero","con","sdcon",
                  "wsd","wsdmax","wsdmin","winr","winsd","winmin","winmax",
                  "nthresh","tminc","tmaxc","tsdc","ccom1","ccom3")
  
  # ---- Epsilon values ----
  eps_values <- epsf + (0:(x001-1)) * epsint
  
  # ---- Load required functions ----
  source(coref_path)
  
  # Check only the external helper used by BMStats. Graph summaries are
  # computed directly with igraph; no plotting function is called in the loop.
  required_functions <- c("points_to_balls")
  missing_functions <- required_functions[!vapply(required_functions, exists, logical(1))]
  if (length(missing_functions) > 0) {
    stop("Missing required functions: ", paste(missing_functions, collapse = ", "))
  }
  
  # ---- Parallel setup ----
  cl <- makeCluster(ncores)
  registerDoSNOW(cl)
  
  # Setup progress bar
  pb <- txtProgressBar(max = length(eps_values) * rep, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # Load libraries and functions on all workers
  clusterEvalQ(cl, {
    library(Rcpp)
    library(dplyr)
    library(igraph)
  })
  
  # Source files on workers
  clusterCall(cl, source, coref_path)
  clusterCall(cl, sourceCpp, bmcpp_path)
  
  # Define BallMapperCpp function on workers
  clusterEvalQ(cl, {
    BallMapperCpp <- function(points, values, epsilon) {
      out <- BallMapperCppInterface(points, values, epsilon)
      colnames(out$vertices) <- c("id","size")
      return(out)
    }
  })
  
  # Export variables to workers
  clusterExport(cl, c("xvars","yvar","rep","thresh2","xvar",
                      "n_xvars","eps_values","base_seed"),
                envir = environment())
  
  # Worker-level streams are initialised reproducibly. Each iteration below
  # also receives a deterministic seed based on radius and repetition.
  parallel::clusterSetRNGStream(cl, iseed = base_seed)
  
  # ---- Main computation ----
  cat("Starting computation with", length(eps_values), "epsilon values and", rep, "repetitions...\n")
  
  # Create all combinations explicitly to avoid nested foreach issues
  param_grid <- expand.grid(eps_idx = 1:length(eps_values), rep_idx = 1:rep)
  cat("Total iterations:", nrow(param_grid), "\n")
  
  # Use single foreach loop instead of nested to avoid list coercion issues
  results_list <- tryCatch({
    foreach(idx = 1:nrow(param_grid), 
            .packages = c("dplyr", "igraph"),
            .errorhandling = "pass",
            .options.snow = opts) %dopar% {
        
        j <- param_grid$eps_idx[idx]
        i <- param_grid$rep_idx[idx]
        
        tryCatch({
          e001 <- eps_values[j]
          
          # Deterministic but distinct landmark-order seed for each run.
          iteration_seed <- base_seed + i * 100000L + j
          set.seed(iteration_seed)
          
          # Create randomized data order - this is the key to variation
          random_indices <- sample(nrow(xvars))
          
          # Reorder BOTH xvars and yvar by the same random order
          xvars_shuffled <- xvars[random_indices, , drop = FALSE]
          yvar_shuffled <- yvar[random_indices]
          
          # Create data frame with sequential pt identifier (after shuffling)
          cloud2a <- xvars_shuffled
          cloud2a$yvar <- yvar_shuffled
          cloud2a$pt <- 1:nrow(cloud2a)  # CORRECT: Sequential point identifier
          
          # Extract data for ball mapper (already in random order)
          cloud2b <- cloud2a[, 1:n_xvars, drop = FALSE]
          cloud2c <- cloud2a[, "yvar", drop = FALSE]
          
          # Ball mapper computation on pre-shuffled data
          bmg1 <- BallMapperCpp(cloud2b, cloud2c, epsilon = e001)
          ukc <- as.data.frame(bmg1$coloring)
          ukp <- points_to_balls(bmg1)
          
          # Merge with the shuffled data using sequential pt identifiers
          cloud2d <- merge(ukp, cloud2a, by = "pt")
          cloud2e <- as.data.frame(table(cloud2d$ball))
          names(cloud2e) <- c("ball", "freq")
          
          # Basic statistics
          nballs <- nrow(ukc)
          minc <- if(nballs > 0) min(ukc[,1], na.rm = TRUE) else NA_real_
          maxc <- if(nballs > 0) max(ukc[,1], na.rm = TRUE) else NA_real_
          sdc  <- if(nballs > 0) sd(ukc[,1], na.rm = TRUE) else NA_real_
          minb <- if(nrow(cloud2e) > 0) min(cloud2e$freq, na.rm = TRUE) else NA_real_
          maxb <- if(nrow(cloud2e) > 0) max(cloud2e$freq, na.rm = TRUE) else NA_real_
          sdb  <- if(nrow(cloud2e) > 0) sd(cloud2e$freq, na.rm = TRUE) else NA_real_
          
          # Graph analysis without drawing a plot. Plotting inside thousands
          # of parallel iterations is unnecessary and substantially slower.
          temp_edges_graph <- as.data.frame(bmg1$edges)
          if (ncol(temp_edges_graph) == 2L) {
            names(temp_edges_graph) <- c("from", "to")
          } else if (nrow(temp_edges_graph) == 0L) {
            temp_edges_graph <- data.frame(from = integer(), to = integer())
          } else {
            stop("Ball Mapper edges must have exactly two columns.")
          }
          vertices_graph <- as.data.frame(bmg1$vertices)
          if (ncol(vertices_graph) >= 2L) {
            names(vertices_graph)[1:2] <- c("id", "size")
          }
          bmg01 <- igraph::graph_from_data_frame(
            temp_edges_graph, vertices = vertices_graph, directed = FALSE
          )
          bmg02 <- igraph::components(bmg01)
          bmg03 <- data.frame(size = as.numeric(bmg02$csize))
          bmg04 <- subset(bmg03, size == 1)
          ccom1 <- nrow(bmg03)
          ccom2 <- nrow(bmg04)
          ccom3 <- ccom1 - ccom2
          
          # Edge analysis
          temp_edges <- as.data.frame(bmg1$edges)
          if(ncol(temp_edges) == 2) names(temp_edges) <- c("from", "to")
          
          x901 <- nballs
          if(x901 > 0) {
            deg_from <- if(nrow(temp_edges) > 0) table(factor(temp_edges$from, levels = 1:x901)) else rep(0, x901)
            deg_to   <- if(nrow(temp_edges) > 0) table(factor(temp_edges$to, levels = 1:x901)) else rep(0, x901)
            deg      <- as.numeric(deg_from) + as.numeric(deg_to)
            zero     <- sum(deg == 0)
            sdcon    <- sd(deg)
          } else { 
            zero <- 0
            sdcon <- NA_real_ 
          }
          con <- nrow(temp_edges)
          
          # Threshold analysis
          cloud2f <- if(nrow(cloud2e) > 0) subset(cloud2e, freq > thresh2) else cloud2e[0,]
          x701 <- nrow(cloud2f)
          wsd <- wsdmax <- wsdmin <- winr <- winsd <- winmin <- winmax <- tminc <- tmaxc <- tsdc <- NA_real_
          
          if(x701 > 0) {
            cloud2g <- merge(cloud2d, cloud2f, by = "ball")
            names(cloud2g)[names(cloud2g) == "yvar"] <- "outcome"
            cloud2ag <- group_by(cloud2g, ball)
            cloud2as <- summarise(cloud2ag,
                                  sdout = sd(outcome, na.rm = TRUE),
                                  mio = min(outcome, na.rm = TRUE),
                                  mao = max(outcome, na.rm = TRUE),
                                  meo = mean(outcome, na.rm = TRUE),
                                  .groups = "drop")
            cloud2as$range <- cloud2as$mao - cloud2as$mio
            wsd <- mean(cloud2as$sdout, na.rm = TRUE)
            wsdmax <- max(cloud2as$sdout, na.rm = TRUE)
            wsdmin <- min(cloud2as$sdout, na.rm = TRUE)
            winr <- mean(cloud2as$range, na.rm = TRUE)
            winsd <- sd(cloud2as$range, na.rm = TRUE)
            winmin <- min(cloud2as$range, na.rm = TRUE)
            winmax <- max(cloud2as$range, na.rm = TRUE)
            tminc <- min(cloud2as$meo, na.rm = TRUE)
            tmaxc <- max(cloud2as$meo, na.rm = TRUE)
            tsdc  <- if(nrow(cloud2as) > 1) sd(cloud2as$meo, na.rm = TRUE) else 0
          }
          
          # Create result row
          m_row <- data.frame(eps = e001, rep = i, nballs, minc, maxc, sdc,
                              minb, maxb, sdb, zero, con, sdcon,
                              wsd, wsdmax, wsdmin, winr, winsd, winmin, winmax,
                              nthresh = x701, tminc, tmaxc, tsdc, ccom1, ccom3, 
                              check.names = FALSE, stringsAsFactors = FALSE)
          
          # X-variable analysis
          xv_vals <- rep(NA_real_, n_xvars * 3)
          if(xvar && x701 > 0) {
            cloud2g <- merge(cloud2d, cloud2f, by = "ball")
            for(hh in 1:n_xvars) {
              xv1 <- cloud2g[[hh + 2]]  # adjust for merge columns
              cloud2g$xv1 <- xv1
              cloud2ag <- group_by(cloud2g, ball)
              cloud2as <- summarise(cloud2ag,
                                    minxv1 = min(xv1, na.rm = TRUE),
                                    maxxv1 = max(xv1, na.rm = TRUE),
                                    .groups = "drop")
              rang <- cloud2as$maxxv1 - cloud2as$minxv1
              base <- (hh - 1) * 3
              xv_vals[base + 1] <- mean(rang, na.rm = TRUE)
              xv_vals[base + 2] <- min(rang, na.rm = TRUE)
              xv_vals[base + 3] <- max(rang, na.rm = TRUE)
            }
          }
          
          xv_row <- setNames(as.list(c(xv_vals, i, e001)), xv_colnames)
          final_result <- cbind(m_row, as.data.frame(xv_row, check.names = FALSE, stringsAsFactors = FALSE))
          
          # Ensure all columns are proper data types
          for(col in names(final_result)) {
            if(is.list(final_result[[col]])) {
              final_result[[col]] <- unlist(final_result[[col]])
            }
          }
          
          return(final_result)
          
        }, error = function(e) {
          # Return error information as a data frame
          error_df <- data.frame(eps = eps_values[j], rep = i, error = as.character(e$message),
                               stringsAsFactors = FALSE)
          # Add empty columns to match expected structure
          for(col in m_colnames) {
            if(!col %in% names(error_df)) {
              error_df[[col]] <- NA_real_
            }
          }
          for(col in xv_colnames) {
            if(!col %in% names(error_df)) {
              error_df[[col]] <- NA_real_
            }
          }
          return(error_df)
        })
      }
  }, finally = {
    # Ensure cluster is stopped
    close(pb)
    stopCluster(cl)
  })
  
  # Convert list results to data frame
  cat("Converting", length(results_list), "results to dataframe...\n")
  valid_results <- list()
  error_count <- 0
  
  for(k in 1:length(results_list)) {
    result_item <- results_list[[k]]
    if(is.data.frame(result_item)) {
      if("error" %in% names(result_item) && !is.na(result_item$error[1])) {
        error_count <- error_count + 1
        cat("Error in iteration", k, ":", result_item$error[1], "\n")
      } else {
        # Remove error column if it exists but is NA
        if("error" %in% names(result_item)) {
          result_item$error <- NULL
        }
        valid_results[[length(valid_results) + 1]] <- result_item
      }
    } else {
      cat("Warning: Got non-dataframe result in iteration", k, ":", class(result_item), "\n")
    }
  }
  
  if(length(valid_results) == 0) {
    stop("No valid results obtained from parallel computation")
  }
  
  cat("Combining", length(valid_results), "valid results...\n")
  if(error_count > 0) {
    cat("Had", error_count, "errors during computation\n")
  }
  
  # Safely combine results
  results_df <- tryCatch({
    # Check that all results have compatible structure
    col_counts <- sapply(valid_results, ncol)
    if(length(unique(col_counts)) > 1) {
      cat("Warning: Results have different numbers of columns:", unique(col_counts), "\n")
      # Find common columns
      all_names <- lapply(valid_results, names)
      common_names <- Reduce(intersect, all_names)
      cat("Using", length(common_names), "common columns\n")
      valid_results <- lapply(valid_results, function(df) df[, common_names, drop = FALSE])
    }
    do.call(rbind, valid_results)
  }, error = function(e) {
    cat("Error combining results:", e$message, "\n")
    stop("Failed to combine parallel results")
  })
  
  # ---- Process results ----
  cat("Processing results...\n")
  cat("Results dataframe dimensions:", nrow(results_df), "x", ncol(results_df), "\n")
  
  if(nrow(results_df) == 0) {
    stop("No successful computations completed")
  }
  
  # Split results - be more careful about column selection
  m_keep <- intersect(m_colnames, names(results_df))
  cat("M columns found:", length(m_keep), "out of", length(m_colnames), "\n")
  
  if(length(m_keep) > 0) {
    m001 <- results_df[, m_keep, drop = FALSE]
  } else {
    warning("No m001 columns found in results")
    m001 <- data.frame()
  }
  
  xv_keep <- intersect(xv_colnames, names(results_df))
  cat("XV columns found:", length(xv_keep), "out of", length(xv_colnames), "\n")
  
  if(length(xv_keep) > 0) {
    xv01 <- results_df[, xv_keep, drop = FALSE]
  } else {
    warning("No xv01 columns found in results")
    xv01 <- data.frame()
  }
  
  # ---- Derived variables ----
  if(nrow(m001) > 0) {
    # Check that required columns exist before computing derived variables
    required_cols <- c("maxc", "minc", "maxb", "minb", "nballs", "zero", "con", "tmaxc", "tminc")
    missing_cols <- required_cols[!required_cols %in% names(m001)]
    
    if(length(missing_cols) > 0) {
      warning("Missing required columns for derived variables: ", paste(missing_cols, collapse = ", "))
    } else {
      m001$rangec <- m001$maxc - m001$minc
      m001$rangeb <- m001$maxb - m001$minb
      m001$avcon <- ifelse(m001$nballs == m001$zero, 0, m001$con / (m001$nballs - m001$zero))
      m001$trangec <- m001$tmaxc - m001$tminc
    }
    
    # Filter data
    m001 <- subset(m001, eps > 0 & !is.na(sdc))
    cat("M001 after filtering:", nrow(m001), "rows\n")
  } else {
    warning("m001 is empty, skipping derived variables")
  }
  
  # ---- Summary table ----
  if(nrow(m001) > 0) {
    m001s <- m001 %>%
      group_by(eps) %>%
      summarise(
        mn = mean(nballs), sdn = sd(nballs),
        n025 = quantile(nballs, 0.025, na.rm = TRUE), n975 = quantile(nballs, 0.975, na.rm = TRUE),
        mminc = mean(minc), sminc = sd(minc), minc025 = quantile(minc, 0.025, na.rm = TRUE), minc975 = quantile(minc, 0.975, na.rm = TRUE),
        mmaxc = mean(maxc), smaxc = sd(maxc), maxc025 = quantile(maxc, 0.025, na.rm = TRUE), maxc975 = quantile(maxc, 0.975, na.rm = TRUE),
        msdc = mean(sdc), ssdc = sd(sdc), sdc025 = quantile(sdc, 0.025, na.rm = TRUE), sdc975 = quantile(sdc, 0.975, na.rm = TRUE),
        mminb = mean(minb), sminb = sd(minb), minb025 = quantile(minb, 0.025, na.rm = TRUE), minb975 = quantile(minb, 0.975, na.rm = TRUE),
        mmaxb = mean(maxb), smaxb = sd(maxb), maxb025 = quantile(maxb, 0.025, na.rm = TRUE), maxb975 = quantile(maxb, 0.975, na.rm = TRUE),
        msdb = mean(sdb), ssdb = sd(sdb), sdb025 = quantile(sdb, 0.025, na.rm = TRUE), sdb975 = quantile(sdb, 0.975, na.rm = TRUE),
        mrangec = mean(rangec), srangec = sd(rangec), rangec025 = quantile(rangec, 0.025, na.rm = TRUE), rangec975 = quantile(rangec, 0.975, na.rm = TRUE),
        mrangeb = mean(rangeb), srangeb = sd(rangeb), rangeb025 = quantile(rangeb, 0.025, na.rm = TRUE), rangeb975 = quantile(rangeb, 0.975, na.rm = TRUE),
        mzero = mean(zero), szero = sd(zero), zero025 = quantile(zero, 0.025, na.rm=TRUE), zero975 = quantile(zero, 0.975, na.rm=TRUE),
        mcon = mean(con), scon = sd(con), con025 = quantile(con, 0.025, na.rm=TRUE), con975 = quantile(con, 0.975, na.rm=TRUE),
        mavcon = mean(avcon, na.rm=TRUE), savcon = sd(avcon, na.rm=TRUE), avcon025 = quantile(avcon, 0.025, na.rm=TRUE), avcon975 = quantile(avcon, 0.975, na.rm=TRUE),
        mwsd = mean(wsd, na.rm = TRUE), swsd = sd(wsd, na.rm = TRUE), wsd025 = quantile(wsd, 0.025, na.rm=TRUE), wsd975 = quantile(wsd, 0.975, na.rm=TRUE),
        mwsdmin = mean(wsdmin, na.rm=TRUE), swsdmin = sd(wsdmin, na.rm=TRUE), wsdmin025 = quantile(wsdmin, 0.025, na.rm=TRUE), wsdmin975 = quantile(wsdmin, 0.975, na.rm=TRUE),
        mwsdmax = mean(wsdmax, na.rm=TRUE), swsdmax = sd(wsdmax, na.rm=TRUE), wsdmax025 = quantile(wsdmax, 0.025, na.rm=TRUE), wsdmax975 = quantile(wsdmax, 0.975, na.rm=TRUE),
        mwinr = mean(winr, na.rm = TRUE), swinr = sd(winr, na.rm = TRUE), winr025 = quantile(winr, 0.025, na.rm=TRUE), winr975 = quantile(winr, 0.975, na.rm=TRUE),
        mwinsd = mean(winsd, na.rm=TRUE), swinsd = sd(winsd, na.rm=TRUE), winsd025 = quantile(winsd, 0.025, na.rm=TRUE), winsd975 = quantile(winsd, 0.975, na.rm=TRUE),
        mwinmin = mean(winmin, na.rm=TRUE), swinmin = sd(winmin, na.rm=TRUE), winmin025 = quantile(winmin, 0.025, na.rm=TRUE), winmin975 = quantile (winmin, 0.975, na.rm=TRUE), 
        mwinmax = mean(winmax, na.rm=TRUE), swinmax = sd(winmax, na.rm=TRUE), winmax025 = quantile(winmax, 0.025, na.rm=TRUE), winmax975 = quantile (winmax, 0.975, na.rm=TRUE),
        mtminc = mean(tminc, na.rm = TRUE), stminc = sd(tminc, na.rm=TRUE), tminc025 = quantile(tminc, 0.025, na.rm=TRUE), tminc975 = quantile(tminc, 0.975, na.rm = TRUE),
        mtmaxc = mean(tmaxc, na.rm = TRUE), stmaxc = sd(tmaxc, na.rm=TRUE), tmaxc025 = quantile(tmaxc, 0.025, na.rm=TRUE), tmaxc975 = quantile(tmaxc, 0.975, na.rm = TRUE),
        mtrangec = mean(trangec, na.rm=TRUE), strangec = sd(trangec, na.rm=TRUE), trangec025 = quantile(trangec, 0.025, na.rm=TRUE), trangec975 = quantile(trangec, 0.975, na.rm=TRUE),
        mccom1 = mean(ccom1, na.rm = TRUE), sccom1 = sd(ccom1, na.rm = TRUE), ccom1025 = quantile(ccom1, 0.025), ccom1975 = quantile(ccom1, 0.975),
        mccom3 = mean(ccom3, na.rm = TRUE), sccom3 = sd(ccom3, na.rm = TRUE), ccom3025 = quantile(ccom3, 0.025), ccom3975 = quantile(ccom3, 0.975),
        .groups = "drop"
      )
  } else {
    m001s <- data.frame()
  }
  
  cat("Computation completed successfully!\n")
  cat("Returning list with:\n")
  cat("- m001:", nrow(m001), "rows x", ncol(m001), "columns\n")
  cat("- xv01:", nrow(xv01), "rows x", ncol(xv01), "columns\n")
  cat("- m001s:", nrow(m001s), "rows x", ncol(m001s), "columns\n")
  
  # Ensure we return a proper list
  result_list <- list(
    m001 = m001,
    xv01 = xv01,
    m001s = m001s,
    settings = list(
      eps_values = eps_values,
      repetitions = rep,
      threshold = thresh2,
      base_seed = base_seed,
      ncores = ncores,
      checkpoint_every = checkpoint_every
    )
  )

  # Save a final checkpoint. Earlier versions exposed checkpoint arguments but
  # did not write the completed object.
  if (!is.null(checkpoint_file) && nzchar(checkpoint_file)) {
    dir.create(dirname(checkpoint_file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result_list, checkpoint_file)
    cat("Saved completed checkpoint to:", checkpoint_file, "\n")
  }

  return(result_list)
}

# z is a flag for an additional variable to be included in the plot
# w is a flag for a further additional variable to be included in the plot

bgraph<-function(x,y,xtit,ytit,filename,xlims=c(-99,99),ylims=c(-99,99),z,w){
 xax<-x
 xl01<-min(xax)
 xl02<-max(xax)
 if(xlims[1]==-99){
  xlims<-c(xl01,xl02)
 }
 yax<-y
 yco<-ncol(yax)
 yl01<-min(y[,1])
 yl02<-max(y[,1])
 for(yy in 2:yco){
  yl03<-min(y[,yy])
  yl04<-max(y[,yy])
  yl01<-min(yl01,yl03)
  yl02<-max(yl02,yl04)
 }
 auto_ylims <- ylims[1] == -99
 if(auto_ylims){
  ylims<-c(yl01,yl02)
 }
 df001<-as.data.frame(cbind.data.frame(x,y))
 names(df001)<-c("x","y","yl","yu")

 if(missing(z)){ 
  g<-ggplot(df001,aes(x=x,y=y))+
   geom_ribbon(aes(ymin=yl,ymax=yu),alpha= .5, fill="darkseagreen3", color="transparent")+
   geom_line(color="aquamarine4", linewidth=.7) +
   labs(x=xtit,y=ytit)+
   coord_cartesian(xlim=xlims,ylim=ylims,expand=FALSE)+
   theme(axis.title.x = element_text(vjust=0,size=40),
   axis.title.y = element_text(vjust=0,size=40,margin=margin(t=0,r=30,b=0,l=0)),
   axis.text = element_text(size=30),
   panel.grid.major = element_line(color="gray10", linetype= "dashed", linewidth = .5),
   panel.grid.minor = element_line(color="gray90", linetype= "dashed", linewidth = .8))

  g

  ggsave(filename, width=12, height = 8)
  return(print("Only 1 Axis Included"))
 }
 if(missing(w)){
  zl01<-min(z[,1])
  zl02<-max(z[,1])
  for(yy in 2:yco){
   zl03<-min(z[,yy])
   zl04<-max(z[,yy])
   zl01<-min(zl01,zl03)
   zl02<-max(zl02,zl04)
  }
  yl01<-min(zl01,yl01)
  yl02<-max(zl02,yl02)
  if(auto_ylims) ylims <- c(yl01,yl02)
  df001<-as.data.frame(cbind.data.frame(x,y,z))
  names(df001)<-c("x","y","yl","yu","z","zl","zu")

  g<-ggplot(df001,aes(x=x,y=y))+
   geom_ribbon(aes(ymin=yl,ymax=yu),alpha= .5, fill="darkseagreen3", color="transparent")+
   geom_line(color="aquamarine4", linewidth=.7) +
   geom_ribbon(aes(ymin=zl,ymax=zu),alpha= .5, fill="firebrick", color="transparent")+
   geom_line(aes(x=x,y=z),color="red", linewidth=.7) +
   labs(x=xtit,y=ytit)+
   coord_cartesian(xlim=xlims,ylim=ylims,expand=FALSE)+
   theme(axis.title.x = element_text(vjust=0,size=40),
   axis.title.y = element_text(vjust=0,size=40,margin=margin(t=0,r=30,b=0,l=0)),
   axis.text = element_text(size=30),
   panel.grid.major = element_line(color="gray10", linetype= "dashed", linewidth = .5),
   panel.grid.minor = element_line(color="gray90", linetype= "dashed", linewidth = .8))

  g

  ggsave(filename, width=12, height = 8)
  return(print("Only 2 Axes Included"))
 }
 wl01<-min(w[,1])
 wl02<-max(w[,1])
 for(yy in 2:yco){
  wl03<-min(w[,yy])
  wl04<-max(w[,yy])
  wl01<-min(wl01,wl03)
  wl02<-max(wl02,wl04)
 }
 yl01<-min(wl01,yl01)
 yl02<-max(wl02,yl02)
 if(auto_ylims) ylims <- c(yl01,yl02)
 df001<-as.data.frame(cbind.data.frame(x,y,z,w))
 names(df001)<-c("x","y","yl","yu","z","zl","zu","w","wl","wu")

 g<-ggplot(df001,aes(x=x,y=y))+
  geom_ribbon(aes(ymin=yl,ymax=yu),alpha= .5, fill="darkseagreen3", color="transparent")+
  geom_line(color="aquamarine4", linewidth=.7) +
  geom_ribbon(aes(ymin=zl,ymax=zu),alpha= .5, fill="firebrick", color="transparent")+
  geom_line(aes(x=x,y=z),color="red", linewidth=.7) +
  geom_ribbon(aes(ymin=wl,ymax=wu),alpha= .5, fill="blue", color="transparent")+
  geom_line(aes(x=x,y=w),color="blue", linewidth=.7) +
  labs(x=xtit,y=ytit)+
   coord_cartesian(xlim=xlims,ylim=ylims,expand=FALSE)+
  theme(axis.title.x = element_text(vjust=0,size=40),
  axis.title.y = element_text(vjust=0,size=40,margin=margin(t=0,r=30,b=0,l=0)),
  axis.text = element_text(size=30),
  panel.grid.major = element_line(color="gray10", linetype= "dashed", linewidth = .5),
  panel.grid.minor = element_line(color="gray90", linetype= "dashed", linewidth = .8))

 ggsave(filename, width=12, height = 8)
 return(print("3 Axes Included"))
}

bmsum<-function(m001s,epstab,suff,dp=2,col1 = "Ball Colour", col2 = "Ball Colour SD", col3 = "Colouration Range"){
 x<-m001s$eps

 # Number of balls
 
 y<-as.data.frame(cbind.data.frame(m001s$mn,m001s$n025,m001s$n975))
 ytit<-"Number of Balls"
 xtit<-"Ball Radius"
 filename<-paste0("balls",suff,".png")
 bgraph(x,y,xtit,ytit,filename) 

 # Colouration

 y<-as.data.frame(cbind.data.frame(m001s$mminc,m001s$minc025,m001s$minc975))
 z<-as.data.frame(cbind.data.frame(m001s$mmaxc,m001s$maxc025,m001s$maxc975))
 ytit<-col1
 xtit<-"Ball Radius"
 filename<-paste0("colours",suff,".png")
 bgraph(x,y,xtit,ytit,filename, z=z) 

 # Standard deviation of colouration

 y<-as.data.frame(cbind.data.frame(m001s$msdc,m001s$sdc025,m001s$sdc975))
 ytit<-col2
 xtit<-"Ball Radius"
 filename<-paste0("csd",suff,".png")
 bgraph(x,y,xtit,ytit,filename) 
 
 # Ball size

 y<-as.data.frame(cbind.data.frame(m001s$mminb,m001s$minb025,m001s$minb975))
 z<-as.data.frame(cbind.data.frame(m001s$mmaxb,m001s$maxb025,m001s$maxb975))
 ytit<-"Ball Size"
 xtit<-"Ball Radius"
 filename<-paste0("size",suff,".png")
 bgraph(x,y,xtit,ytit,filename, z=z) 

 # Standard deviation of ball size

 y<-as.data.frame(cbind.data.frame(m001s$msdb,m001s$sdb025,m001s$sdb975))
 ytit<-"Ball Size SD"
 xtit<-"Ball Radius"
 filename<-paste0("sizesd",suff,".png")
 bgraph(x,y,xtit,ytit,filename) 

 # Range of colouration

 y<-as.data.frame(cbind.data.frame(m001s$mrangec,m001s$rangec025,m001s$rangec975))
 ytit<-col3
 xtit<-"Ball Radius"
 filename<-paste0("crange",suff,".png")
 bgraph(x,y,xtit,ytit,filename) 

 # Range of ball size

 y<-as.data.frame(cbind.data.frame(m001s$mrangeb,m001s$rangeb025,m001s$rangeb975))
 ytit<-"Ball Size Range"
 xtit<-"Ball Radius"
 filename<-paste0("brange",suff,".png")
 bgraph(x,y,xtit,ytit,filename) 

 # Zero connected balls

 y<-as.data.frame(cbind.data.frame(m001s$mzero,m001s$zero025,m001s$zero975))
 ytit<-"Outlier Balls"
 xtit<-"Ball Radius"
 filename<-paste0("zero",suff,".png")
 bgraph(x,y,xtit,ytit,filename) 

 # Connected Components

 y<-as.data.frame(cbind.data.frame(m001s$mccom3,m001s$ccom3025,m001s$ccom3975))
 ytit<-"Connected Components"
 xtit<-"Ball Radius"
 filename<-paste0("ccom",suff,".png")
 bgraph(x,y,xtit,ytit,filename) 

 # Average number of connections

 y<-as.data.frame(cbind.data.frame(m001s$mavcon,m001s$avcon025,m001s$avcon975))
 ytit<-"Average Connections"
 xtit<-"Ball Radius"
 filename<-paste0("avcon",suff,".png")
 bgraph(x,y,xtit,ytit,filename) 

 # Standard deviation of colouration within balls

 y<-as.data.frame(cbind.data.frame(m001s$mwsd,m001s$wsd025,m001s$wsd975))
 ytit<-"Within Ball SD"
 xtit<-"Ball Radius"
 filename<-paste0("wsd",suff,".png")
 bgraph(x,y,xtit,ytit,filename) 

 # Standard deviation of colouration within balls Min Max

 y<-as.data.frame(cbind.data.frame(m001s$mwsdmin,m001s$wsdmin025,m001s$wsdmin975))
 z<-as.data.frame(cbind.data.frame(m001s$mwsdmax,m001s$wsdmax025,m001s$wsdmax975))
 ytit<-"Within Ball SD"
 xtit<-"Ball Radius"
 filename<-paste0("wsdminmax",suff,".png")
 bgraph(x,y,xtit,ytit,filename, z=z) 

 # Within ball range

 y<-as.data.frame(cbind.data.frame(m001s$mwinr,m001s$winr025,m001s$winr975))
 ytit<-"Within Ball Range"
 xtit<-"Ball Radius"
 filename<-paste0("winr",suff,".png")
 bgraph(x,y,xtit,ytit,filename) 

 # Within ball range standard deviation

 y<-as.data.frame(cbind.data.frame(m001s$mwinsd,m001s$winsd025,m001s$winsd975))
 ytit<-"Within Ball Range"
 xtit<-"Ball Radius"
 filename<-paste0("winrsd",suff,".png")
 bgraph(x,y,xtit,ytit,filename) 

 # Within Ball Range Min Max

 y<-as.data.frame(cbind.data.frame(m001s$mwinmin,m001s$winmin025,m001s$winmin975))
 z<-as.data.frame(cbind.data.frame(m001s$mwinmax,m001s$winmax025,m001s$winmax975))
 ytit<-"Within Ball Range"
 xtit<-"Ball Radius"
 filename<-paste0("winrminmax",suff,".png")
 bgraph(x,y,xtit,ytit,filename, z=z) 

 # Colouration above threshold

 y<-as.data.frame(cbind.data.frame(m001s$mtminc,m001s$tminc025,m001s$tminc975))
 z<-as.data.frame(cbind.data.frame(m001s$mtmaxc,m001s$tmaxc025,m001s$tmaxc975))
 ytit<-col1
 xtit<-"Ball Radius"
 filename<-paste0("tcolours",suff,".png")
 bgraph(x,y,xtit,ytit,filename, z=z) 
 
 # Range of colouration above threshold

 y<-as.data.frame(cbind.data.frame(m001s$mtrangec,m001s$trangec025,m001s$trangec975))
 ytit<-col3
 xtit<-"Ball Radius"
 filename<-paste0("trange",suff,".png")
 bgraph(x,y,xtit,ytit,filename) 

 
 epstab<-as.data.frame(epstab)
 names(epstab)<-c("eps")
 a001<-nrow(epstab)
 a002<-a001+1

 stab1<-matrix(0,nrow=45,ncol=a002)

 m001s$eps<-round(m001s$eps,10)
 epstab$eps<-round(epstab$eps,10)

 tempa<-merge(m001s,epstab,by="eps")

 for(i in 2:a002){
  a003<-i-1
  temp<-tempa[a003,]
  stab1[1,i]<-epstab[a003,1]
  stab1[2,i]<-round(temp$mn,dp) # Number of balls
  stab1[3,i]<-paste0("(",round(temp$sdn,dp),")")
  stab1[4,i]<-round(temp$mminc,dp) # Minimum Coloration
  stab1[5,i]<-paste0("(",round(temp$sminc,dp),")")
  stab1[6,i]<-round(temp$mmaxc,dp) # Maximum Coloration
  stab1[7,i]<-paste0("(",round(temp$smaxc,dp),")")
  stab1[8,i]<-round(temp$msdc,dp) # Standard deviation of Coloration
  stab1[9,i]<-paste0("(",round(temp$ssdc,dp),")")
  stab1[10,i]<-round(temp$mrangec,dp) # Range Coloration
  stab1[11,i]<-paste0("(",round(temp$srangec,dp),")")
  stab1[12,i]<-round(temp$mminb,dp) # Minimum ball size
  stab1[13,i]<-paste0("(",round(temp$sminb,dp),")")
  stab1[14,i]<-round(temp$mmaxb,dp) # Maximum ball size
  stab1[15,i]<-paste0("(",round(temp$smaxb,dp),")")
  stab1[16,i]<-round(temp$mrangeb,dp) # Range of ball size
  stab1[17,i]<-paste0("(",round(temp$srangeb,dp),")")
  stab1[18,i]<-round(temp$mzero,dp) # Zero connections
  stab1[19,i]<-paste0("(",round(temp$szero,dp),")")
  stab1[20,i]<-round(temp$mccom3,dp) # Connected components
  stab1[21,i]<-paste0("(",round(temp$sccom3,dp),")")
  stab1[22,i]<-round(temp$mavcon,dp) # Average connections
  stab1[23,i]<-paste0("(",round(temp$savcon,dp),")")
  stab1[24,i]<-round(temp$mcon,dp) # Total connections
  stab1[25,i]<-paste0("(",round(temp$scon,dp),")")
  stab1[26,i]<-round(temp$mwsd,dp) # Within ball standard deviation
  stab1[27,i]<-paste0("(",round(temp$swsd,dp),")")
  stab1[28,i]<-round(temp$mwsdmin,dp) # Within ball standard deviation minimum
  stab1[29,i]<-paste0("(",round(temp$swsdmin,dp),")")
  stab1[30,i]<-round(temp$mwsdmax,dp) # Within ball standard deviation maximum
  stab1[31,i]<-paste0("(",round(temp$swsdmax,dp),")")
  stab1[32,i]<-round(temp$mwinr,dp) # Within ball range
  stab1[33,i]<-paste0("(",round(temp$swinr,dp),")")
  stab1[34,i]<-round(temp$mwinsd,dp) # Within ball range standard deviation
  stab1[35,i]<-paste0("(",round(temp$swinsd,dp),")")
  stab1[36,i]<-round(temp$mwinmin,dp) # Within ball range
  stab1[37,i]<-paste0("(",round(temp$swinmin,dp),")")
  stab1[38,i]<-round(temp$mwinmax,dp) # Within ball range
  stab1[39,i]<-paste0("(",round(temp$swinmax,dp),")")
  stab1[40,i]<-round(temp$mtminc,dp) # Minimum coloration meeting threshold
  stab1[41,i]<-paste0("(",round(temp$stminc,dp),")")
  stab1[42,i]<-round(temp$mtmaxc,dp) # Maximum coloration meeting threshold
  stab1[43,i]<-paste0("(",round(temp$stmaxc,dp),")")
  stab1[44,i]<-round(temp$mtrangec,dp) # Range of coloration meeting threshold
  stab1[45,i]<-paste0("(",round(temp$strangec,dp),")")
 } 
 stab1[1,1]<-"Epsilon"
 stab1[2,1]<-"Number of Balls"
 stab1[3,1]<-""
 stab1[4,1]<-"Minimum Coloration"
 stab1[5,1]<-""
 stab1[6,1]<-"Maximum Coloration"
 stab1[7,1]<-""
 stab1[8,1]<-"Coloration Standard Deviation"
 stab1[9,1]<-""
 stab1[10,1]<-"Range of Coloration"
 stab1[11,1]<-""
 stab1[12,1]<-"Minimum Ball Size"
 stab1[13,1]<-""
 stab1[14,1]<-"Maximum Ball Size"
 stab1[15,1]<-""
 stab1[16,1]<-"Ball Size Range"
 stab1[17,1]<-""
 stab1[18,1]<-"Disconnected Balls"
 stab1[19,1]<-""
 stab1[20,1]<-"Connected Components >1 Ball"
 stab1[21,1]<-""
 stab1[22,1]<-"Average Connections"
 stab1[23,1]<-""
 stab1[24,1]<-"Total Connections"
 stab1[25,1]<-""
 stab1[26,1]<-"Within Ball Standard Deviation"
 stab1[27,1]<-""
 stab1[28,1]<-"Within Ball SD Min"
 stab1[29,1]<-""
 stab1[30,1]<-"Within Ball SD Max"
 stab1[31,1]<-""
 stab1[32,1]<-"Within Ball Coloration Range"
 stab1[33,1]<-""
 stab1[34,1]<-"Within Ball Coloration Range SD"
 stab1[35,1]<-""
 stab1[36,1]<-"Within Ball Coloration Range Min"
 stab1[37,1]<-""
 stab1[38,1]<-"Within Ball Coloration Range Max"
 stab1[39,1]<-""
 stab1[40,1]<-"Minimum Coloration (Threshold)"
 stab1[41,1]<-""
 stab1[42,1]<-"Maximum Coloration (Threshold)"
 stab1[43,1]<-""
 stab1[44,1]<-"Coloration Range (Threshold)"
 stab1[45,1]<-""

 filename2<-paste0("summarystats",suff,".txt")
 latout(stab1,filename2)

}

xgraph<-function(x001,suff,dp){
 x002<-ncol(x001) # Number of columns in x data
 x003<-x002-2 # Do not want to count either rep or eps
 x004<-x003/3 # Gives the number of x variables
 t001<-as.data.frame(table(x001$eps))
 names(t001)<-c("eps","freq")
 for(xx in 1:x004){
  x005<-(xx-1)*3+1
  x006<-xx*3
  df1<-as.data.frame(cbind.data.frame(x001$eps,x001[,x005:x006]))
  names(df1)<-c("eps","meanx","minx","maxx")
  df1g<-group_by(df1,eps)
  df1s<-summarise(df1g,meanxm=mean(meanx,na.rm=TRUE),meanx025=quantile(meanx,0.025,na.rm=TRUE),meanx975=quantile(meanx,0.975,na.rm=TRUE),minxm=mean(minx, na.rm=TRUE),minx025=quantile(minx,0.025,na.rm=TRUE),minx975=quantile(minx,0.975,na.rm=TRUE),maxxm=mean(maxx,na.rm=TRUE),maxx025=quantile(maxx,0.025,na.rm=TRUE),maxx975=quantile(maxx,0.975,na.rm=TRUE),.groups="drop")
  df1s<-as.data.frame(df1s)
  names(df1s)[1]<-"eps"
  names(df1s)[2]<-paste0("meanxm",xx)
  names(df1s)[3]<-paste0("meanx025",xx)
  names(df1s)[4]<-paste0("meanx975",xx)
  names(df1s)[5]<-paste0("minxm",xx)
  names(df1s)[6]<-paste0("minx025",xx)
  names(df1s)[7]<-paste0("minx975",xx)
  names(df1s)[8]<-paste0("maxxm",xx)
  names(df1s)[9]<-paste0("maxx025",xx)
  names(df1s)[10]<-paste0("maxx975",xx)
  t001<-merge(t001,df1s,by="eps")
  x<-df1s$eps
  y<-as.data.frame(cbind.data.frame(df1s[,2],df1s[,3],df1s[,4]))
  z<-as.data.frame(cbind.data.frame(df1s[,5],df1s[,6],df1s[,7]))
  w<-as.data.frame(cbind.data.frame(df1s[,8],df1s[,9],df1s[,10]))
  ytit<-"Within Variation"
  xtit<-"Ball Radius"
  filename<-paste0("xvar",xx,suff,".png")
  bgraph(x,y,xtit,ytit,filename, z=z,w=w) 
 }
 return(t001)
}

# Example usage:
# result <- BMStats(
#   xvars = your_data[, 1:3], 
#   yvar = your_data$outcome,
#   epsf = 0.1, epsint = 0.1, x001 = 10, rep = 5,
#   thresh2 = 10,
#   bmcpp_path = "/path/to/BallMapper.cpp",
#   coref_path = "/path/to/coref.R",
#   ncores = 4
# )


# ==============================================================================
# Identity-preserving TDABM stability summaries
# ==============================================================================

BMStatsMembership <- function(
  xvars,
  yvar = NULL,
  observation_ids = NULL,
  epsf,
  epsint,
  x001,
  rep,
  ncores = 4,
  bmcpp_path = NULL,
  coref_path = NULL,
  base_seed = 12345,
  pairwise = c("none", "focal", "all"),
  focal_ids = NULL,
  checkpoint_file = NULL,
  checkpoint_every = 0L
) {
  pairwise <- match.arg(pairwise)
  checkpoint_every <- as.integer(checkpoint_every)
  if (is.na(checkpoint_every) || checkpoint_every < 0L) {
    stop("checkpoint_every must be a non-negative integer.")
  }

  if (is.null(bmcpp_path) || !file.exists(bmcpp_path)) {
    stop("A valid bmcpp_path must be supplied.")
  }
  if (is.null(coref_path) || !file.exists(coref_path)) {
    stop("A valid coref_path must be supplied.")
  }

  suppressPackageStartupMessages({
    library(doSNOW)
    library(foreach)
    library(igraph)
    library(Rcpp)
  })

  xvars <- as.data.frame(xvars, stringsAsFactors = FALSE)
  xvars[] <- lapply(xvars, function(z) as.numeric(unlist(z)))

  if (anyNA(xvars)) {
    stop("xvars contains missing values. Complete or impute the axes first.")
  }

  n <- nrow(xvars)
  if (is.null(yvar)) yvar <- rep(1, n)
  yvar <- as.numeric(unlist(yvar))
  if (length(yvar) != n) stop("yvar must have one value per observation.")

  if (is.null(observation_ids)) observation_ids <- seq_len(n)
  observation_ids <- as.character(observation_ids)
  if (length(observation_ids) != n) {
    stop("observation_ids must have one value per observation.")
  }
  if (anyDuplicated(observation_ids)) {
    stop("observation_ids must be unique.")
  }

  if (pairwise == "focal") {
    if (is.null(focal_ids) || length(focal_ids) == 0L) {
      stop("focal_ids must be supplied when pairwise = 'focal'.")
    }
    focal_ids <- intersect(as.character(focal_ids), observation_ids)
    if (length(focal_ids) == 0L) stop("None of focal_ids occur in observation_ids.")
  }

  eps_values <- epsf + (0:(x001 - 1L)) * epsint
  n_xvars <- ncol(xvars)

  source(coref_path)
  if (!exists("points_to_balls")) {
    stop("points_to_balls was not found after sourcing coref_path.")
  }

  cl <- parallel::makeCluster(ncores)
  doSNOW::registerDoSNOW(cl)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)

  parallel::clusterEvalQ(cl, {
    library(Rcpp)
    library(igraph)
  })
  parallel::clusterCall(cl, source, coref_path)
  parallel::clusterCall(cl, Rcpp::sourceCpp, bmcpp_path)
  parallel::clusterEvalQ(cl, {
    BallMapperCpp <- function(points, values, epsilon) {
      out <- BallMapperCppInterface(points, values, epsilon)
      colnames(out$vertices) <- c("id", "size")
      out
    }
  })

  parallel::clusterExport(
    cl,
    c("xvars", "yvar", "observation_ids", "eps_values", "n_xvars",
      "base_seed", "pairwise", "focal_ids"),
    envir = environment()
  )
  parallel::clusterSetRNGStream(cl, iseed = base_seed)

  param_grid <- expand.grid(
    eps_idx = seq_along(eps_values),
    rep_idx = seq_len(rep)
  )

  pb <- utils::txtProgressBar(max = nrow(param_grid), style = 3)
  on.exit(try(close(pb), silent = TRUE), add = TRUE)

  # checkpoint_every is interpreted as a number of radius-repetition jobs.
  # A value of zero runs all outstanding jobs in one batch. Partial raw results
  # are written after each batch and can be resumed if the same checkpoint file
  # already exists.
  completed_indices <- integer()
  results <- vector("list", nrow(param_grid))

  if (!is.null(checkpoint_file) && nzchar(checkpoint_file) &&
      file.exists(checkpoint_file)) {
    prior <- tryCatch(readRDS(checkpoint_file), error = function(e) NULL)
    if (is.list(prior) && identical(prior$checkpoint_type, "BMStatsMembership_partial") &&
        identical(prior$settings$eps_values, eps_values) &&
        identical(prior$settings$repetitions, rep) &&
        identical(prior$settings$base_seed, base_seed)) {
      prior_indices <- as.integer(prior$completed_indices)
      prior_indices <- prior_indices[
        prior_indices >= 1L & prior_indices <= nrow(param_grid)
      ]
      if (length(prior_indices) > 0L) {
        results[prior_indices] <- prior$results[prior_indices]
        completed_indices <- sort(unique(prior_indices))
        utils::setTxtProgressBar(pb, length(completed_indices))
        message("Resuming membership checkpoint with ",
                length(completed_indices), " completed jobs.")
      }
    }
  }

  outstanding <- setdiff(seq_len(nrow(param_grid)), completed_indices)
  batch_size <- if (checkpoint_every > 0L) checkpoint_every else max(1L, length(outstanding))

  if (length(outstanding) > 0L) {
    batches <- split(outstanding, ceiling(seq_along(outstanding) / batch_size))

    for (batch_indices in batches) {
      batch_results <- foreach::foreach(
        idx = batch_indices,
        .packages = c("igraph"),
        .errorhandling = "pass"
      ) %dopar% {
        j <- param_grid$eps_idx[idx]
        i <- param_grid$rep_idx[idx]
        epsilon <- eps_values[j]

        tryCatch({
          set.seed(base_seed + i * 100000L + j)
          random_indices <- sample.int(nrow(xvars))

          cloud <- xvars[random_indices, , drop = FALSE]
          cloud$yvar <- yvar[random_indices]
          cloud$original_id <- observation_ids[random_indices]
          cloud$pt <- seq_len(nrow(cloud))

          points <- cloud[, seq_len(n_xvars), drop = FALSE]
          values <- cloud[, "yvar", drop = FALSE]
          bm <- BallMapperCpp(points, values, epsilon)
          point_ball <- points_to_balls(bm)
          membership <- merge(point_ball, cloud[, c("pt", "original_id")], by = "pt")
          membership <- unique(membership[, c("ball", "original_id")])
          membership$eps <- epsilon
          membership$rep <- i

          edges <- as.data.frame(bm$edges)
          if (ncol(edges) == 2L) {
            names(edges) <- c("from", "to")
          } else if (nrow(edges) == 0L) {
            edges <- data.frame(from = integer(), to = integer())
          } else {
            stop("Ball Mapper edges must have exactly two columns.")
          }
          vertices <- as.data.frame(bm$vertices)
          if (ncol(vertices) >= 2L) names(vertices)[1:2] <- c("id", "size")
          graph <- igraph::graph_from_data_frame(edges, vertices = vertices, directed = FALSE)
          degree_values <- igraph::degree(graph)
          isolated_balls <- as.integer(names(degree_values)[degree_values == 0])

          membership$is_isolated_ball <- membership$ball %in% isolated_balls

          point_summary <- aggregate(
            cbind(
              n_ball_memberships = rep(1L, nrow(membership)),
              n_isolated_memberships = as.integer(membership$is_isolated_ball)
            ) ~ original_id,
            data = membership,
            FUN = sum
          )
          point_summary$eps <- epsilon
          point_summary$rep <- i
          point_summary$only_isolated <-
            point_summary$n_ball_memberships == point_summary$n_isolated_memberships

          pair_output <- NULL
          if (pairwise != "none") {
            selected_membership <- membership
            if (pairwise == "focal") {
              focal_balls <- unique(selected_membership$ball[
                selected_membership$original_id %in% focal_ids
              ])
              selected_membership <- selected_membership[
                selected_membership$ball %in% focal_balls, , drop = FALSE
              ]
            }

            split_members <- split(selected_membership$original_id, selected_membership$ball)
            pair_list <- lapply(split_members, function(ids) {
              ids <- sort(unique(as.character(ids)))
              if (length(ids) < 2L) return(NULL)
              pairs <- utils::combn(ids, 2L)
              data.frame(id1 = pairs[1, ], id2 = pairs[2, ], stringsAsFactors = FALSE)
            })
            pair_list <- Filter(Negate(is.null), pair_list)
            pair_output <- if (length(pair_list) > 0L) {
              unique(do.call(rbind, pair_list))
            } else {
              data.frame()
            }
            if (nrow(pair_output) > 0L) {
              if (pairwise == "focal") {
                pair_output <- pair_output[
                  pair_output$id1 %in% focal_ids | pair_output$id2 %in% focal_ids,
                  , drop = FALSE
                ]
              }
              pair_output$eps <- epsilon
              pair_output$rep <- i
            }
          }

          graph_summary <- data.frame(
            eps = epsilon,
            rep = i,
            nballs = nrow(vertices),
            nedges = nrow(edges),
            ncomponents = igraph::components(graph)$no,
            nisolated_balls = length(isolated_balls),
            stringsAsFactors = FALSE
          )

          list(
            membership = membership[, c("eps", "rep", "ball", "original_id",
                                        "is_isolated_ball")],
            point_summary = point_summary,
            pairwise = pair_output,
            graph_summary = graph_summary
          )
        }, error = function(e) {
          list(error = data.frame(
            eps = epsilon, rep = i, message = conditionMessage(e),
            stringsAsFactors = FALSE
          ))
        })
      }

      results[batch_indices] <- batch_results
      completed_indices <- sort(unique(c(completed_indices, batch_indices)))
      utils::setTxtProgressBar(pb, length(completed_indices))

      if (!is.null(checkpoint_file) && nzchar(checkpoint_file) &&
          checkpoint_every > 0L) {
        dir.create(dirname(checkpoint_file), recursive = TRUE, showWarnings = FALSE)
        saveRDS(
          list(
            checkpoint_type = "BMStatsMembership_partial",
            completed_indices = completed_indices,
            results = results,
            param_grid = param_grid,
            settings = list(
              eps_values = eps_values,
              repetitions = rep,
              pairwise = pairwise,
              focal_ids = focal_ids,
              base_seed = base_seed,
              ncores = ncores,
              checkpoint_every = checkpoint_every
            )
          ),
          checkpoint_file
        )
      }
    }
  }

  close(pb)
  parallel::stopCluster(cl)
  cl <- NULL

  errors <- do.call(rbind, lapply(results, function(z) z$error))
  valid <- Filter(function(z) is.null(z$error), results)
  if (length(valid) == 0L) stop("No valid membership repetitions completed.")

  membership_long <- do.call(rbind, lapply(valid, `[[`, "membership"))
  point_repetitions <- do.call(rbind, lapply(valid, `[[`, "point_summary"))
  graph_repetitions <- do.call(rbind, lapply(valid, `[[`, "graph_summary"))

  pair_list <- Filter(Negate(is.null), lapply(valid, `[[`, "pairwise"))
  pairwise_long <- if (length(pair_list) > 0L) do.call(rbind, pair_list) else data.frame()

  authority_summary <- aggregate(
    cbind(
      n_ball_memberships,
      n_isolated_memberships,
      only_isolated = as.numeric(only_isolated)
    ) ~ eps + original_id,
    data = point_repetitions,
    FUN = mean
  )
  names(authority_summary)[names(authority_summary) == "only_isolated"] <-
    "isolation_frequency"

  isolation_summary <- authority_summary[, c(
    "eps", "original_id", "isolation_frequency",
    "n_ball_memberships", "n_isolated_memberships"
  )]

  pairwise_summary <- data.frame()
  if (nrow(pairwise_long) > 0L) {
    pairwise_long$present <- 1
    pairwise_summary <- aggregate(
      present ~ eps + id1 + id2,
      data = pairwise_long,
      FUN = sum
    )
    reps_per_eps <- aggregate(rep ~ eps, data = graph_repetitions,
                              FUN = function(z) length(unique(z)))
    names(reps_per_eps)[2] <- "completed_repetitions"
    pairwise_summary <- merge(pairwise_summary, reps_per_eps, by = "eps")
    pairwise_summary$comembership_frequency <-
      pairwise_summary$present / pairwise_summary$completed_repetitions
  }

  result <- list(
    membership_long = membership_long,
    point_repetitions = point_repetitions,
    authority_summary = authority_summary,
    isolation_summary = isolation_summary,
    pairwise_comembership = pairwise_summary,
    graph_repetitions = graph_repetitions,
    errors = errors,
    settings = list(
      eps_values = eps_values,
      repetitions = rep,
      pairwise = pairwise,
      focal_ids = focal_ids,
      base_seed = base_seed,
      ncores = ncores,
      checkpoint_every = checkpoint_every
    )
  )

  if (!is.null(checkpoint_file) && nzchar(checkpoint_file)) {
    dir.create(dirname(checkpoint_file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(result, checkpoint_file)
  }

  result
}


# ==============================================================================
# Wrappers for pre-specified run matrices
# ==============================================================================

BMStatsRunSpec <- function(
  data,
  run_spec,
  bmcpp_path,
  coref_path,
  ncores = 4,
  thresh2 = 4,
  xvar = TRUE,
  base_seed = 12345,
  output_dir = "."
) {
  required_spec <- c(
    "run_id", "axis_1", "axis_2", "axis_3", "colour_variable",
    "radius_min", "radius_max", "radius_step", "repetitions"
  )
  missing_spec <- setdiff(required_spec, names(run_spec))
  if (length(missing_spec) > 0L) {
    stop("run_spec is missing: ", paste(missing_spec, collapse = ", "))
  }

  axes <- c(run_spec$axis_1, run_spec$axis_2, run_spec$axis_3)
  required_data <- c(axes, run_spec$colour_variable)
  missing_data <- setdiff(required_data, names(data))
  if (length(missing_data) > 0L) {
    stop("Data is missing: ", paste(missing_data, collapse = ", "))
  }

  axes_df <- as.data.frame(data[, axes, drop = FALSE])
  colour_vector <- as.numeric(data[[run_spec$colour_variable]])

  x001 <- as.integer(round(
    (run_spec$radius_max - run_spec$radius_min) / run_spec$radius_step
  )) + 1L

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  checkpoint <- file.path(output_dir, paste0(run_spec$run_id, "_BMStats.rds"))

  result <- BMStats(
    xvars = axes_df,
    yvar = colour_vector,
    epsf = run_spec$radius_min,
    epsint = run_spec$radius_step,
    x001 = x001,
    rep = run_spec$repetitions,
    thresh2 = thresh2,
    xvar = xvar,
    ncores = ncores,
    bmcpp_path = bmcpp_path,
    coref_path = coref_path,
    checkpoint_file = checkpoint,
    base_seed = base_seed
  )

  result$run_spec <- run_spec
  saveRDS(result, checkpoint)
  result
}


BMStatsBatch <- function(
  data_list,
  run_matrix,
  bmcpp_path,
  coref_path,
  tiers = NULL,
  ncores = 4,
  thresh2 = 4,
  base_seed = 12345,
  output_dir = "BMStats_results"
) {
  run_matrix <- as.data.frame(run_matrix, stringsAsFactors = FALSE)
  if (!is.null(tiers)) {
    run_matrix <- run_matrix[run_matrix$tier %in% tiers, , drop = FALSE]
  }
  if (nrow(run_matrix) == 0L) stop("No run-matrix rows selected.")

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  results <- vector("list", nrow(run_matrix))
  names(results) <- run_matrix$run_id

  for (i in seq_len(nrow(run_matrix))) {
    spec <- run_matrix[i, , drop = FALSE]
    dataset_name <- as.character(spec$dataset_name)
    if (is.null(data_list[[dataset_name]])) {
      stop("Dataset not found in data_list: ", dataset_name)
    }

    cat("Running", spec$run_id, "using", dataset_name, "\n")
    run_dir <- file.path(output_dir, as.character(spec$run_id))
    results[[i]] <- BMStatsRunSpec(
      data = data_list[[dataset_name]],
      run_spec = spec,
      bmcpp_path = bmcpp_path,
      coref_path = coref_path,
      ncores = ncores,
      thresh2 = thresh2,
      base_seed = base_seed,
      output_dir = run_dir
    )
  }

  saveRDS(results, file.path(output_dir, "BMStats_batch_results.rds"))
  results
}


# ==============================================================================
# Publication-figure wrapper
# ==============================================================================

ColorIgraphPlot5aPublication <- function(
  outputFromBallMapper,
  legend_title = "Coloration",
  filename = "",
  seed_for_plotting = 123,
  showVertexLabels = TRUE,
  minimal_ball_radius = 7,
  maximal_ball_scale = 20,
  number_of_colors = 100,
  minc = -99999,
  maxc = 99999,
  width = 1800,
  height = 1400
) {
  ColorIgraphPlot5a(
    outputFromBallMapper = outputFromBallMapper,
    showVertexLabels = showVertexLabels,
    ltext = legend_title,
    showLegend = TRUE,
    minimal_ball_radius = minimal_ball_radius,
    maximal_ball_scale = maximal_ball_scale,
    seed_for_plotting = seed_for_plotting,
    store_in_file = filename,
    default_x_image_resolution = width,
    default_y_image_resolution = height,
    number_of_colors = number_of_colors,
    minc = minc,
    maxc = maxc
  )
}


# ==============================================================================
# Example academic verification calls
# ==============================================================================
#
# aggregate_result <- BMStats(
#   xvars = verification_data[, c("gap_age5_z", "gap_age11_z", "gap_age16_z")],
#   yvar = verification_data$example_colour_variable,
#   epsf = 0.80, epsint = 0.05, x001 = 9, rep = 1000,
#   thresh2 = 4, ncores = 120,
#   bmcpp_path = "/path/to/BallMapper.cpp",
#   coref_path = "/path/to/coref.R",
#   checkpoint_file = "outputs/P03_BMStats.rds",
#   base_seed = 12345
# )
#
# membership_result <- BMStatsMembership(
#   xvars = verification_data[, c("gap_age5_z", "gap_age11_z", "gap_age16_z")],
#   yvar = verification_data$example_colour_variable,
#   observation_ids = verification_data$la_code,
#   epsf = 0.90, epsint = 0.05, x001 = 5, rep = 1000,
#   ncores = 120,
#   bmcpp_path = "/path/to/BallMapper.cpp",
#   coref_path = "/path/to/coref.R",
#   pairwise = "focal",
#   focal_ids = verification_data$la_code[
#     verification_data$la_name %in% c(
#       "Newham", "Tower Hamlets", "Wokingham",
#       "Bath and North East Somerset"
#     )
#   ],
#   checkpoint_file = "outputs/primary_membership_stability.rds"
# )
