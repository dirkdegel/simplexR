library(ggplot2)
library(grDevices)
library(ggrepel)


#------------------------------------------------------------------------------------------

df2String <- function(df, round = 2) {
  apply(df, 1, function(x) paste0("(", paste0(round(x, round), collapse = ", "), ")"))
}

#------------------------------------------------------------------------------------------

cornerPointsCont <- function(A, b, nonneg = rep(TRUE, ncol(A))) {
  planes <- cbind(A, -b)
  nneg <- NULL
  for (i in 1:ncol(A)) {
    if (nonneg[i]) {
      v <- rep(0, ncol(A))
      v[i] <- -1
      nneg <- rbind(nneg, c(v, 0))
    }
  }
  planes <- rbind(planes, nneg)

  # Compute the vertices (all cont variables)
  n <- nrow(planes)
  m <- ncol(planes)
  vertices <- NULL
  tmp <- vector("list", m - 1)
  tmp <- lapply(tmp, function(x) 1:n)
  tmp <- expand.grid(tmp)
  tmp <- tmp[apply(tmp, 1, function(x) all(diff(x) > 0)), ]
  tmp <- as.matrix(tmp)
  for (i in 1:nrow(tmp)) {
    try(
      {
        # Intersection of the planes i, j, k
        vertex <- solve(planes[tmp[i, ], -m], -planes[tmp[i, ], m])
        # Check that it is indeed in the polyhedron
        if (all(planes %*% c(vertex, 1) <= 1e-6)) {
          vertices <- rbind(vertices, vertex)
        }
      },
      silent = TRUE
    )
  }
  # vertices <- as.data.frame(vertices)
  colnames(vertices) <- paste0("x", 1:ncol(vertices))
  rownames(vertices) <- NULL
  # vertices$lbl <- df2String(vertices)
  return(vertices)
}

#------------------------------------------------------------------------------------------

cornerPoints <- function(A, b, type = rep("c", ncol(A)), nonneg = rep(TRUE, ncol(A))) {
  if (length(type) != ncol(A)) stop("Arg 'type' must be same length as columns in A!")

  if (all(type == "c")) {
    return(cornerPointsCont(A, b, nonneg))
  }
  if (all(type == "i")) {
    iPoints <- integerPoints(A, b, nonneg)
    tri <- t(geometry::convhulln(iPoints))
    idx <- unique(as.vector(tri))
    return(iPoints[idx, ])
  }
  # else combination
  p <- slices(A, b, type, nonneg)
  p <- do.call(rbind, p)
  tri <- t(geometry::convhulln(p))
  # rgl.triangles(p[tri,1],p[tri,2],p[tri,3],col="gold2",alpha=.6)
  idx <- unique(as.vector(tri))
  p <- p[idx, ]
  # points3d(p, col="blue", size = 15)
  # p <- as.data.frame(p[idx,])
  colnames(p) <- paste0("x", 1:ncol(A))
  rownames(p) <- NULL
  # p$lbl <- df2String(p)
  return(p)
}

#------------------------------------------------------------------------------------------

plotPolytope2D <- function(
  A,
  b,
  obj = NULL,
  type = rep("c", ncol(A)),
  nonneg = rep(TRUE, ncol(A)),
  crit = "max",
  faces = rep("c", ncol(A)),
  plotFaces = TRUE,
  plotFeasible = TRUE,
  plotOptimum = FALSE,
  latex = FALSE,
  labels = NULL,
  ...) 
  {
    if (!is.null(obj) & (!is.vector(obj) | !length(obj) == ncol(A))) {
      stop("Arg. obj must be a vector of same length as the number of columns in A.")
    }
    # if (is.null(points) & is.null(rangePoints) & is.null(cPoints)) stop("Arguments cPoints, points or rangePoints must be specified!")
    # Set Custom theme
    myTheme <- theme_bw()
    myTheme <- myTheme + theme(
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      # axis.line = element_blank(),
      axis.line = element_line(
        colour = "black", size = 0.5,
        arrow = arrow(length = unit(0.3, "cm"))
      ),
      # axis.ticks = element_blank()
      # axis.text.x = element_text(margin = margin(r = 30))
      # axis.ticks.length = unit(0.5,"mm"),
      # aspect.ratio=4/3,
      legend.position = "none"
    )

    # Create solution plot
    p <- ggplot() #+ coord_fixed(ratio = 1)
    if (latex) p <- p + xlab("$x_1$") + ylab("$x_2$")
    if (!latex) p <- p + xlab(expression(x[1])) + ylab(expression(x[2]))
    # coord_cartesian(xlim = c(-0.1, max(cPoints$x1)+1), ylim = c(-0.1, max(cPoints$x2)+1), expand = F) +

    if (plotFaces) {
      cPoints <- cornerPoints(A, b, faces, nonneg)
      idx <- grDevices::chull(cPoints)
      cPoints <- cPoints[idx, ]
      p <- p + geom_polygon(
        data = as.data.frame(cPoints), aes_string(x = "x1", y = "x2"),
        fill = "gray90", size = 0.5, linetype = 1, color = "gray"
      )
    }

    # find feasible points
    if (all(type == "c")) {
      points <- cornerPoints(A, b, type, nonneg)
    } else if (all(type == "i")) {
      points <- integerPoints(A, b, nonneg)
    } else {
      pl <- slices(A, b, type, nonneg)
      pl <- lapply(pl, unique)
      for (i in 1:length(pl)) pl[[i]] <- cbind(pl[[i]], i)
      pl <- lapply(pl, function(x) {
        colnames(x) <- c("x1", "x2", "g")
        rownames(x) <- NULL
        x <- data.frame(x)
      })
      points <- do.call(rbind, pl)
      # points <- points[,1:2]
    }
    points <- as.data.frame(points)

    if (plotFeasible) {
      if (all(type == "c")) {
        # p <- p + geom_point(aes_string(x = 'x1', y = 'x2'), data=points) #+ scale_colour_grey(start = 0.6, end = 0)
      }
      if (all(type == "i")) {
        p <- p + geom_point(aes_string(x = "x1", y = "x2"), data = points, ...) #+ scale_colour_grey(start = 0.6, end = 0)
      }
      if (length(which(type == "c")) == 1) {
        # pl <- slices(A, b, type, nonneg)
        # pl <- lapply(pl, unique)
        # for (i in 1:length(pl)) pl[[i]] <- cbind(pl[[i]],i)
        # pl <- lapply(pl, function(x) {
        #    colnames(x) <- c("x1", "x2", "g")
        #    rownames(x) <- NULL
        #    x<-data.frame(x)
        # })
        # tmp <- do.call(rbind, pl)
        # points <- tmp[,1:2]
        p <- p + geom_line(aes_string(x = "x1", y = "x2", group = "g", ...), data = points)
        idx <- sapply(pl, function(x) nrow(x) == 1)
        pl <- pl[idx]
        if (length(pl) > 0) {
          tmp <- do.call(rbind, pl)
          p <- p + geom_point(aes_string(x = "x1", y = "x2", ...), data = tmp)
        }
      }
    }

    if (!is.null(labels)) {
      tmp <- points[, 1:ncol(A)]
      tmp <- as.data.frame(tmp)
      if (labels == "coord") {
        tmp$lbl <- df2String(tmp)
      } else if (labels == "n") {
        tmp$lbl <- ""
      } else {
        tmp$lbl <- 1:nrow(tmp)
      }
      if (length(tmp$lbl) > 0) {
        p <- p + geom_point(aes_string(x = "x1", y = "x2"), data = tmp)
        nudgeS <- -(max(tmp$x1) - min(tmp$x1)) / 100
        if (anyDuplicated(cbind(tmp$x1, tmp$x2), MARGIN = 1) > 0) {
          p <- p + ggrepel::geom_text_repel(aes_string(x = "x1", y = "x2", label = "lbl"),
            data = tmp, size = 3, colour = "gray50"
          )
        }
        if (anyDuplicated(cbind(tmp$x1, tmp$x2), MARGIN = 1) == 0) {
          p <- p + geom_text(aes_string(x = "x1", y = "x2", label = "lbl"),
            data = tmp,
            nudge_x = nudgeS, nudge_y = nudgeS, hjust = 1, size = 3,
            colour = "gray50"
          )
        }
      }
    }

    if (plotOptimum) {
      if (!is.null(obj)) { # add iso profit line
        tmp <- points
        tmp$lbl <- df2String(tmp)
        tmp$z <- as.matrix(points[, 1:2]) %*% obj
        if (crit == "max") i <- which.max(tmp$z)
        if (crit == "min") i <- which.min(tmp$z)
        # if (latex) str <- paste0("$x^* = (", tmp$x1[i], ",", tmp$x2[i], ")$")
        # if (!latex) str <- paste0("x* = ", tmp$lbl[1])
        if (latex) str <- paste0("$z^* = ", round(tmp$z[i], 2), "$")
        if (!latex) str <- paste0("z* = ", round(tmp$z[i], 2))
        if (obj[2] != 0) {
          p <- p + geom_abline(intercept = tmp$z[i] / obj[2], slope = -obj[1] / obj[2], lty = "dashed")
        } else {
          p <- p + geom_vline(xintercept = tmp$x1[i], lty = "dashed")
        }
        p <- p + geom_label(aes_string(x = "x1", y = "x2", label = "str"),
          data = tmp[i, ],
          nudge_x = 1.0
        )
      }
    }
    p <- p + myTheme
    return(p)
}

#------------------------------------------------------------------------------------------
# new by Dirk

plot_linear_program <- plotPolytope2D

