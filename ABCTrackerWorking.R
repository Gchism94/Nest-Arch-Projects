#############################################
## Autor: Greg CHISM
## Date: Aug 2021
## email: gchism@email.arizona.edu
## Project: Nest shape influences colony organization in ants (movement/ activity)
## Title: ABCTracker movement data processing, vizualization, and analysis
#############################################

#This code is to replicate the analyses and figures for the following in my second chapter:
#Movement raw data import and procesing
#Speed comparisons & analyses
#Traffic jams/ interquartile lengths
#Distance to the entrance & movement speeds
#Distance interquartile lengths
#Aggression performance analyses
#All visualizations

#Install and load required packages
install.packages("pacman") #Download package with function to load multiple packaged at once
pacman::p_load(assertthat, #Loading required packages for code below. p_load() will download packages that aren't in system library
          data.table,
          forcats,
          gganimate,
          ggpubr,
          janitor,
          lme4,
          lmerTest,
          magrittr,
          MuMIn,
          purrr,
          readr,
          readxl,
          tidyverse,
          wesanderson)

data_names <- c("Colony5Aggn", "Colony5Pre", 
                "Colony6Aggn","Colony6Pre", 
                "Colony7Aggn","Colony7Pre", 
                "Colony8Aggn","Colony8Pre", 
                "Colony9Aggn","Colony9Pre", 
                "Colony11Aggn","Colony11Pre", 
                "Colony13Aggn","Colony13Pre", 
                "Colony17Aggn","Colony17Pre", 
                "Colony18Aggn","Colony18Pre", 
                "Colony20Aggn","Colony20Pre",
                "AllTagsMovement","AggnAssayTestFullRaw",
                "PreAssayTestFullRaw","FullAssayTestSpeed",
                "AggnAssayTestFull","AggnAssayTestFullTwoSec",
                "PreAssayTestFull","PreAssayTestFullTwoSec",
                "FullAssayTestSpeedTwoSec","FullAssayTestSpeedTwoSecTags",
                "AggnAssayTestFullAvg","PreAssayTestFullAvg",
                "AggnAssayTestFullCircle","AggnAssayTestFullTube",
                "PreAssayTestFullCircle","PreAssayTestFullTube",
                "AggnAssayTestIQRTags","AggnAssayTestIQR",
                "PreAssayTestIQR","AggnAssayTestIQR1",
                "PreAssayTestIQR1","FullAssayTestIQR",
                "Colony5AggnDist","Colony6AggnDist",
                "Colony7AggnDist","Colony8AggnDist",
                "Colony9AggnDist","Colony11AggnDist",
                "Colony13AggnDist","Colony17AggnDist",
                "Colony18AggnDist","Colony20AggnDist",
                "Colony5PreDist","Colony6PreDist",
                "Colony7PreDist","Colony8PreDist",
                "Colony9PreDist","Colony11PreDist",
                "Colony13PreDist","Colony17PreDist",
                "Colony18PreDist","Colony20PreDist",
                "AggnAssayDistTest","AggnAssayDistTest1",
                "PreAssayDistTest","PreAssayDistTest1",
                "AggnAssayDistTestFull","PreAssayDistTestFull",
                "AggnAssayDistTestFullPlot","PreAssayDistTestFullPlot",
                "AggnAssayTestDistIQR","PreAssayTestDistIQR",
                "AggnAssayTestDistIQR1","PreAssayTestDistIQR1",
                "FullAssayTestDistIQR","Aggression_Data_Time",
                "TwoSecsList","TwentySecBin","FiveDistList",
                "Aggression_Data_Time","Aggression_Data_Dist")

for(i in 1:length(data_names)) {                              # Head of for-loop
  write.csv2(get(data_names[i]),                              # Write CSV files to folder
             paste0("C:/Users/Joach/Desktop/My Folder/",
                    data_names[i],
                    ".csv"),
             row.names = FALSE)
}

##Fixing boxplots
#Code adapted from: https://rdrr.io/github/BoulderCodeHub/CRSSIO/src/R/stat_boxplot_custom.R
#Full explanation can be found in the stat_boxplot_custom.R file
stat_boxplot_custom <- function(mapping = NULL, data = NULL,
                                geom = "boxplot", position = "dodge",
                                ...,
                                qs = c(.05, .25, 0.5, 0.75, 0.95),
                                na.rm = FALSE,
                                orientation = NA,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  assert_that(
    length(qs) == 5 && is.numeric(qs),
    msg = "`qs` should be a numeric vector with 5 values."
  )
  
  assert_that(
    all(qs == sort(qs)), 
    msg = "`qs` should be provided in ascending order."
  )
  
  assert_that(
    all(qs <= 1) && all(qs >= 0),
    msg = "`qs` should only span values [0, 1]."
  )
  
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatBoxplotCustom,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      qs = qs,
      ...
    )
  )
}

StatBoxplotCustom <- ggplot2::ggproto("StatBoxplotCustom", ggplot2::Stat,
                                      required_aes = c("y|x"),
                                      non_missing_aes = "weight",
                                      
                                      setup_data = function(data, params) {
                                        data <- ggplot2::flip_data(data, params$flipped_aes)
                                        data$x <- ggplot2:::"%||%"(data$x, 0)
                                        data <- ggplot2::remove_missing(
                                          data,
                                          na.rm = params$na.rm,
                                          vars = "x",
                                          name = "stat_boxplot_custom"
                                        )
                                        ggplot2::flip_data(data, params$flipped_aes)
                                      },
                                      
                                      setup_params = function(data, params) {
                                        params$flipped_aes <- ggplot2::has_flipped_aes(data, params, 
                                                                                       main_is_orthogonal = TRUE,
                                                                                       group_has_equal = TRUE,
                                                                                       main_is_optional = TRUE)
                                        data <- ggplot2::flip_data(data, params$flipped_aes)
                                        
                                        has_x <- !(is.null(data$x) && is.null(params$x))
                                        has_y <- !(is.null(data$y) && is.null(params$y))
                                        if (!has_x && !has_y) {
                                          abort("stat_boxplot() requires an x or y aesthetic.")
                                        }
                                        
                                        params$width <- ggplot2:::"%||%"(
                                          params$width, 
                                          (ggplot2::resolution(ggplot2:::"%||%"(data$x, 0) * 0.75))
                                        ) 
                                        
                                        if (is.double(data$x) && !ggplot2:::has_groups(data) && any(data$x != data$x[1L])) {
                                          rlang::warn(glue::glue(
                                            "Continuous {flipped_names(params$flipped_aes)$x} aesthetic -- did you forget aes(group=...)?"
                                          ))
                                        }
                                        
                                        params
                                      },
                                      
                                      extra_params = c("na.rm", "orientation"),
                                      
                                      compute_group = function(data, scales, width = NULL, na.rm = FALSE, 
                                                               qs = c(.05, .25, 0.5, 0.75, 0.95), flipped_aes = FALSE) {
                                        
                                        data <- ggplot2::flip_data(data, flipped_aes)
                                        
                                        if (!is.null(data$weight)) {
                                          mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
                                          stats <- as.numeric(stats::coef(mod))
                                        } else {
                                          stats <- as.numeric(stats::quantile(data$y, qs))
                                        }
                                        names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
                                        iqr <- diff(stats[c(2, 4)])
                                        
                                        outliers <- (data$y < stats[1]) | (data$y > stats[5])
                                        
                                        if (length(unique(data$x)) > 1)
                                          width <- diff(range(data$x)) * 0.9
                                        
                                        df <- as.data.frame(as.list(stats))
                                        df$outliers <- list(data$y[outliers])
                                        
                                        if (is.null(data$weight)) {
                                          n <- sum(!is.na(data$y))
                                        } else {
                                          # Sum up weights for non-NA positions of y and weight
                                          n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
                                        }
                                        
                                        df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
                                        df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)
                                        
                                        df$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
                                        df$width <- width
                                        df$relvarwidth <- sqrt(n)
                                        df$flipped_aes <- flipped_aes
                                        ggplot2::flip_data(df, flipped_aes)
                                      }
)

#IMPORT INDIVIDUAL WORKER TAG DATASETS
#Aggression
#Colony 5
Colony5CircleAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony5CircleAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID"))  %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony5TubeAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony5TubeAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID"))  %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
         )

#Colony 6
Colony6CircleAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony6CircleAggnTags.xlsx") %>%
setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony6TubeAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony6TubeAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

#Colony 7
Colony7CircleAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony7CircleAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony7TubeAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony7TubeAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )
Colony8CircleAggnTags
#Colony 8
Colony8CircleAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony8CircleAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony8TubeAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony8TubeAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

#Colony 9
Colony9CircleAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony9CircleAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony9TubeAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony9TubeAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

#Colony 11
Colony11CircleAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony11CircleAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony11TubeAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony11TubeAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

#Colony 13
Colony13CircleAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony13CircleAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony13TubeAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony13TubeAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

#Colony 17
Colony17CircleAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony17CircleAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony17TubeAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony17TubeAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

#Colony 18
Colony18CircleAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony18CircleAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony18TubeAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony18TubeAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )
Colony20CircleAggnTags
#Colony 20
Colony20CircleAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony20CircleAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony20TubeAggnTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony20TubeAggnTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

#Baseline
#Colony 5
Colony5CirclePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony5CirclePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony5TubePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony5TubePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

#Colony 6
Colony6CirclePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony6CirclePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony6TubePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony6TubePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

#Colony 7
Colony7CirclePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony7CirclePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony7TubePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony7TubePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

#Colony 8
Colony8CirclePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony8CirclePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony8TubePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony8TubePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

#Colony 9
Colony9CirclePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony9CirclePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony9TubePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony9TubePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

#Colony 11
Colony11CirclePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony11CirclePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony11TubePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony11TubePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

#Colony 13
Colony13CirclePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony13CirclePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony13TubePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony13TubePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

#Colony 17
Colony17CirclePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony17CirclePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony17TubePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony17TubePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

#Colony 18
Colony18CirclePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony18CirclePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony18TubePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony18TubePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

#Colony 20
Colony20CirclePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony20CirclePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

Colony20TubePreTags <- read_xlsx("/Volumes/ChismHardDriveMAC_1/ChismDatasets/IndividualTags/Colony20TubePreTags.xlsx") %>%
  setNames(c("Colony", "Nest", "Trial", "Tag", "ID")) %>%
  select(Colony, Nest, Trial, Tag, ID) %>%
  mutate(Colony = as.numeric(Colony), 
         Nest = as.character(Nest),
         Trial = as.character(Trial),
         Tag = as.character(Tag),
         ID = as.numeric(ID)
  )

AllTagsMovement<- full_join(Colony5TubeAggnTags, Colony5CircleAggnTags) %>%
  full_join(Colony5TubePreTags) %>%
  full_join(Colony5CirclePreTags) %>%
  full_join(Colony6TubeAggnTags) %>%
  full_join(Colony6CircleAggnTags) %>%
  full_join(Colony6TubePreTags) %>%
  full_join(Colony6CirclePreTags) %>%
  full_join(Colony7TubeAggnTags) %>%
  full_join(Colony7CircleAggnTags) %>%
  full_join(Colony7TubePreTags) %>%
  full_join(Colony7CirclePreTags) %>%
  full_join(Colony8TubeAggnTags) %>%
  full_join(Colony8CircleAggnTags) %>%
  full_join(Colony8TubePreTags) %>%
  full_join(Colony8CirclePreTags) %>%
  full_join(Colony9TubeAggnTags) %>%
  full_join(Colony9CircleAggnTags) %>%
  full_join(Colony9TubePreTags) %>%
  full_join(Colony9CirclePreTags) %>%
  full_join(Colony11TubeAggnTags) %>%
  full_join(Colony11CircleAggnTags) %>%
  full_join(Colony11TubePreTags) %>%
  full_join(Colony11CirclePreTags) %>%
  full_join(Colony13TubeAggnTags) %>%
  full_join(Colony13CircleAggnTags) %>% 
  full_join(Colony13TubePreTags) %>%
  full_join(Colony13CirclePreTags) %>% 
  full_join(Colony17TubeAggnTags) %>%
  full_join(Colony17CircleAggnTags) %>% 
  full_join(Colony17TubePreTags) %>%
  full_join(Colony17CirclePreTags) %>% 
  full_join(Colony18TubeAggnTags) %>%
  full_join(Colony18CircleAggnTags) %>% 
  full_join(Colony18TubePreTags) %>%
  full_join(Colony18CirclePreTags) %>%
  full_join(Colony20TubeAggnTags) %>%
  full_join(Colony20CircleAggnTags) %>% 
  full_join(Colony20TubePreTags) %>%
  full_join(Colony20CirclePreTags)

FullAssayTestSpeedTags <- left_join(FullAssayTestSpeed, AllTagsMovement) %>%
  select(Colony,Nest,Trial,ID,Tag) %>%
  distinct() %>%
  drop_na() %>%
  #Separating the true ColorID column and removing rows with more than one "X"
  #This is done by creating a column that assigns a 1 when two columns are "X"
  separate(Tag, c("Head", "Thorax", "Abd1", "Abd2"), sep = ",", remove = FALSE) %>%
  mutate(XCount = ifelse(Head == "X" & Thorax == "X" | 
                           Abd1 == "X" & Abd2 == "X" | 
                           Head == "X" & Abd1 == "X" |
                           Head == "X" & Abd2 == "X" | 
                           Thorax == "X" & Abd1 == "X"|
                           Thorax == "X" & Abd2 == "X", 1, 0),
         QCount = ifelse(Head == "Q", 1, 0)) %>%
  filter(XCount == 0) %>%
  filter(QCount == 0) %>%
  select(Colony, Nest, Trial, ID, Tag)

FullAssayTestSpeedTagsPre<-FullAssayTestSpeedTags %>%
  filter(Trial == "Pre") %>%
  mutate(CountPre = 1) %>%
  select(-c(Trial,ID))

FullAssayTestSpeedTagsAggn<-FullAssayTestSpeedTags %>%
  filter(Trial == "Aggn") %>%
  mutate(CountAggn = 1) %>%
  select(-c(Trial,ID))

AggnAssayTestSpeedTags<- full_join(FullAssayTestSpeedTagsAggn, FullAssayTestSpeedTagsPre) %>%
  mutate(FullCount = CountAggn + CountPre) %>%
  filter(FullCount == 2) %>%
  select(Colony,Nest,Tag)

AggnAssayTestSpeedTagsFull<-left_join(AggnAssayTestSpeedTags, FullAssayTestSpeedTags)

write.csv(FullAssayTestSpeedTags,"FullAssayTestSpeedTags.csv")
#IMPORT AND PROCESS ALL RAW ABCTRACKER OUTPUT DATASETS
#COLONY5 
#Aggression Assay
#Import the data and change column headers to a more compatible format
Colony5CircleAggnT  <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony5_2CircleOct_18AggnTest.csv")

Colony5CircleAggnT <- setNames(Colony5CircleAggnT, c("ID", "Frames", "X", "Y", "Orientation",
                                                     "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))

Colony5TubeAggnT  <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony5TubeAug_17AggnTNew.csv")

Colony5TubeAggnT <- setNames(Colony5TubeAggnT, c("ID", "Frames", "X", "Y", "Orientation",
                                                 "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))
#Standardizing speeds as AntLengths / sec
#NOTE, this method is used for all datasets
Colony5CircleAggn <- Colony5CircleAggnT %>%
  #Changing any values less than 0, which generally occur at the first frame
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), 
         #Creating the Colony, Nest, and Trial columns
         Colony = 5, Nest = "Circle", Trial = "Aggn",
         #Creating a seconds column using known frames/ sec
         Seconds = Frames / 23.97602398,
         #Calculating AntLenths / sec by dividing speed by the mean ant tracklet length
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  #Standardizing the length of the video (5-mins)
  filter(Seconds < 300 ) %>%
  #Removing the undesired columns
  select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

Colony5TubeAggn <- Colony5TubeAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 5, Nest = "Tube", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

#Join the final datasets
Colony5Aggn <- full_join(Colony5CircleAggn, Colony5TubeAggn)

#Pre assay
Colony5CirclePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony5_2_Oct_17_AggnPreAM2.csv")

Colony5CirclePreT <- setNames(Colony5CirclePreT, c("ID", "Frames", "X", "Y", "Orientation",
                                                   "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))

Colony5TubePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony5_24Tube_Oct_17_AggnExpPM.csv")

Colony5TubePreT <- setNames(Colony5TubePreT, c("ID", "Frames", "X", "Y", "Orientation",
                                               "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))

Colony5CirclePre <- Colony5CirclePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 5, Nest = "Circle", Trial = "Pre",
         #NOTE that the frame rate here is different 
         #Some videos were imported with this frame rate for an unknown reason
         Seconds = Frames / 29.97,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>%
select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

Colony5TubePre <- Colony5TubePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 5, Nest = "Tube", Trial = "Pre",
         Seconds = Frames / 29.97,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

Colony5Pre <- full_join(Colony5CirclePre, Colony5TubePre)

#COLONY 6 
#Aggression Assay
Colony6CircleAggnT  <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony6_3CircleOct_18AggnT.csv")

Colony6CircleAggnT <- setNames(Colony6CircleAggnT, c("ID", "Frames", "X", "Y", "Orientation",
                                                     "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))

Colony6TubeAggnT  <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony6TubeOct_17AggnT.csv")

Colony6TubeAggnT <- setNames(Colony6TubeAggnT, c("ID", "Frames", "X", "Y", "Orientation",
                                                 "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))

Colony6CircleAggn <- Colony6CircleAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 6, Nest = "Circle", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

Colony6TubeAggn <- Colony6TubeAggnT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 6, Nest = "Tube", Trial = "Aggn",
         Seconds = Frames / 23.97602398,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

Colony6Aggn <- full_join(Colony6CircleAggn, Colony6TubeAggn)

#Pre assay
Colony6CirclePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony6_3_Circle_Oct_17_AggnPreAM.csv")

Colony6CirclePreT <- setNames(Colony6CirclePreT, c("ID", "Frames", "X", "Y", "Orientation",
                                                   "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))

Colony6TubePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony6_27_Oct_17_AggnPreAM.csv")

Colony6TubePreT <- setNames(Colony6TubePreT, c("ID", "Frames", "X", "Y", "Orientation",
                                               "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "HeadX", "HeadY"))

Colony6CirclePre <- Colony6CirclePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 6, Nest = "Circle", Trial = "Pre",
         Seconds = Frames / 29.97,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

Colony6TubePre <- Colony6TubePreT %>%
  mutate(Speed.Px.s = ifelse(Speed.Px.s < 0, 0, Speed.Px.s), Colony = 6, Nest = "Tube",Trial = "Pre",
         Seconds = Frames / 29.97,
         AntLength.sec = Speed.Px.s / mean(SizeLeng.px)) %>%
  filter(Seconds < 300) %>%
  select(-c(Frames, Speed.Px.s, Orientation, Interpolated))

Colony6Pre <- full_join(Colony6CirclePre, Colony6TubePre)

#COLONY7 
#Aggression Assay
Colony7CircleAggnT  <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony7Circle_AggnT.csv")

Colony7CircleAggnT <- setNames(Colony7CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony7TubeAggnT  <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony7Tube_AggnT.csv") 

Colony7TubeAggnT <- setNames(Colony7TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony7CircleAggn<-Colony7CircleAggnT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=7, Nest="Circle",Trial="Aggn",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300) %>%
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony7TubeAggn<-Colony7TubeAggnT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=7, Nest="Tube",Trial="Aggn",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300) %>%
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony7Aggn<-full_join(Colony7CircleAggn,Colony7TubeAggn)

#Pre assay
Colony7CirclePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony7_Circle_5_Oct_17_VidAMPre.csv")

Colony7CirclePreT <- setNames(Colony7CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony7TubePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony7_23_Oct_17_AggnPreAM.csv")

Colony7TubePreT <- setNames(Colony7TubePreT, c("ID","Frames","X","Y","Orientation",
                                               "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony7CirclePre<-Colony7CirclePreT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=7, Nest="Circle",Trial="Pre",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300) %>%
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony7TubePre<-Colony7TubePreT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=7, Nest="Tube",Trial="Pre",
         Seconds=Frames/29.97,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300) %>%
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony7Pre<-full_join(Colony7CirclePre,Colony7TubePre)

#COLONY8
#Aggression assay
Colony8CircleAggnT  <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony8CircleJune_18AggnT.csv") 

Colony8CircleAggnT <- setNames(Colony8CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony8TubeAggnT  <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony8TubeJune_18AggnT.csv") 

Colony8TubeAggnT <- setNames(Colony8TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                               "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony8CircleAggn<-Colony8CircleAggnT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=8, Nest="Circle",Trial="Aggn",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300) %>%
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony8TubeAggn<-Colony8TubeAggnT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=8, Nest="Tube",Trial="Aggn",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>%
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony8Aggn<-full_join(Colony8CircleAggn,Colony8TubeAggn)

#Pre assay
Colony8CirclePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony8_25_June_18_AggnPreAM.csv") 

Colony8CirclePreT <- setNames(Colony8CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony8TubePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony8_7_June_18_AggnPreAM.csv")

Colony8TubePreT <- setNames(Colony8TubePreT, c("ID","Frames","X","Y","Orientation",
                                             "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony8CirclePre<-Colony8CirclePreT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=8, Nest="Circle",Trial="Pre",
         Seconds=Frames/29.97,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300) %>%
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony8TubePre<-Colony8TubePreT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=8, Nest="Tube",Trial="Pre",
         Seconds=Frames/29.97,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300) %>%
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony8Pre<-full_join(Colony8CirclePre,Colony8TubePre)


# COLONY 9
#Aggression assay
Colony9CircleAggnT  <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony9CircleJune_18AggnT.csv")

Colony9CircleAggnT <- setNames(Colony9CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony9TubeAggnT  <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony9TubeJune_18AggnT.csv")

Colony9TubeAggnT <- setNames(Colony9TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony9CircleAggn<-Colony9CircleAggnT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=9, Nest="Circle",Trial="Aggn",
         Framerate=23.97602398,Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>%
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony9TubeAggn<-Colony9TubeAggnT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=9, Nest="Tube",Trial="Aggn",
         Framerate=23.97602398,Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px)) %>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony9Aggn<-full_join(Colony9CircleAggn,Colony9TubeAggn)

#Pre assay
Colony9CirclePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony9CirclePre.csv")

Colony9CirclePreT <- setNames(Colony9CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))
Colony9TubePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony9_Tube_6_June_18_AggnP.csv")

Colony9TubePreT <- setNames(Colony9TubePreT, c("ID","Frames","X","Y","Orientation",
                                               "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony9CirclePre<-Colony9CirclePreT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=9, Nest="Circle",Trial="Pre",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony9TubePre<-Colony9TubePreT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=9, Nest="Tube",Trial="Pre",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony9Pre<-full_join(Colony9CirclePre,Colony9TubePre)

#COLONY11
#Aggression assay
Colony11CircleAggnT  <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony11CircleJune_18AggnT.csv") %>%
  select(-c(Tag,Completed))


Colony11CircleAggnT <- setNames(Colony11CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony11TubeAggnT  <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony11TubeJune_18AggnT.csv") 

Colony11TubeAggnT <- setNames(Colony11TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony11CircleAggn<-Colony11CircleAggnT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=11, Nest="Circle",Trial="Aggn",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
          filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony11TubeAggn<-Colony11TubeAggnT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=11, Nest="Tube",Trial="Aggn",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 & AntLength.sec < 10) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))


Colony11Aggn<-full_join(Colony11CircleAggn, Colony11TubeAggn)

#Pre assay
Colony11CirclePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony11_Circle_28_June_18_AggnPreAM.csv")

Colony11CirclePreT <- setNames(Colony11CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                  "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))
Colony11TubePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony11_10_June_18_AggnPreAM_2.csv")

Colony11TubePreT <- setNames(Colony11TubePreT, c("ID","Frames","X","Y","Orientation",
                                               "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony11CirclePre<-Colony11CirclePreT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=11, Nest="Circle",Trial="Pre",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony11TubePre<-Colony11TubePreT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=11, Nest="Tube",Trial="Pre",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony11Pre<-full_join(Colony11TubePre,Colony11CirclePre)

#COLONY13
#Aggression assay
Colony13CircleAggnT  <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony13CircleJune_18AggnT.csv")

Colony13CircleAggnT <- setNames(Colony13CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                       "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony13TubeAggnT  <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony13TubeJune_18AggnT.csv") 

Colony13TubeAggnT <- setNames(Colony13TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony13CircleAggn<-Colony13CircleAggnT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=13, Nest="Circle",Trial="Aggn",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony13TubeAggn<-Colony13TubeAggnT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=13, Nest="Tube",Trial="Aggn",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony13Aggn<-full_join(Colony13CircleAggn,Colony13TubeAggn)

#Pre assay
Colony13CirclePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony13CirclePre.csv")

Colony13CirclePreT <- setNames(Colony13CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))
Colony13TubePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony13_Tube_29_June_18_AggnPre.csv")

Colony13TubePreT <- setNames(Colony13TubePreT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony13CirclePre<-Colony13CirclePreT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=13, Nest="Circle",Trial="Pre",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony13TubePre<-Colony13TubePreT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=13, Nest="Tube",Trial="Pre",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony13Pre<-full_join(Colony13CirclePre,Colony13TubePre)

#COLONY17
#Aggression assay
Colony17CircleAggnT  <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony17CircleAug_18AggnT.csv")

Colony17CircleAggnT <- setNames(Colony17CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                       "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony17TubeAggnT  <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony17TubeJuly_18AggnT.csv")

Colony17TubeAggnT <- setNames(Colony17TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))
Colony17CircleAggn<-Colony17CircleAggnT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=17, Nest="Circle",Trial="Aggn",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony17TubeAggn<-Colony17TubeAggnT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=17, Nest="Tube",Trial="Aggn",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony17Aggn<-full_join(Colony17CircleAggn,Colony17TubeAggn)

#Pre assay
Colony17CirclePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony17_Circle_12_Aug_18_AggnPreAM.csv")

Colony17CirclePreT <- setNames(Colony17CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony17TubePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony17_21_July_18_AggnPrePM_2.csv")

Colony17TubePreT <- setNames(Colony17TubePreT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony17CirclePre<-Colony17CirclePreT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=17, Nest="Circle",Trial="Pre",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony17TubePre<-Colony17TubePreT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=17, Nest="Tube",Trial="Pre",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))


Colony17Pre<-full_join(Colony17CirclePre, Colony17TubePre)

#COLONY18
#Aggression assay
Colony18CircleAggnT  <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony18CircleAug_18AggnT.csv") 

Colony18CircleAggnT <- setNames(Colony18CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                       "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony18TubeAggnT  <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony18TubeJuly_18AggnT.csv")

Colony18TubeAggnT <- setNames(Colony18TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))
Colony18CircleAggn<-Colony18CircleAggnT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=18, Nest="Circle",Trial="Aggn",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony18TubeAggn<-Colony18TubeAggnT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=18, Nest="Tube",Trial="Aggn",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony18Aggn<-full_join(Colony18CircleAggn,Colony18TubeAggn)

#Pre assay
Colony18CirclePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony18_Circle_14_Aug_18_AggnPreAM.csv")

Colony18CirclePreT <- setNames(Colony18CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony18TubePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony18TubePreVideo.csv") 

Colony18TubePreT <- setNames(Colony18TubePreT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony18CirclePre<-Colony18CirclePreT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=18, Nest="Circle",Trial="Pre",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony18TubePre<-Colony18TubePreT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=18, Nest="Tube",Trial="Pre",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px),
         X=(X*-1)+1920,Y=(Y*-1)+1080,HeadX=(HeadX*-1)+1920,HeadY=(HeadY*-1)+1080,
         Orientation=ifelse((Orientation+180) > 360, Orientation-180,Orientation+180))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony18Pre<-full_join(Colony18CirclePre,Colony18TubePre)

#COLONY20
#Aggression assay
Colony20CircleAggnT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony20CircleAug_18AggnT.csv")

Colony20CircleAggnT <- setNames(Colony20CircleAggnT, c("ID","Frames","X","Y","Orientation",
                                                     "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony20TubeAggnT<- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony20TubeAug_18AggnT.csv") 

Colony20TubeAggnT <- setNames(Colony20TubeAggnT, c("ID","Frames","X","Y","Orientation",
                                                 "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))
Colony20CircleAggn<-Colony20CircleAggnT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=20, Nest="Circle",Trial="Aggn",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony20TubeAggn<-Colony20TubeAggnT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=20, Nest="Tube",Trial="Aggn",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony20Aggn<-full_join(Colony20CircleAggn,Colony20TubeAggn)

#Pre assay
Colony20CirclePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony20_18Aug20_CircPre.csv") 

Colony20CirclePreT <- setNames(Colony20CirclePreT, c("ID","Frames","X","Y","Orientation",
                                                   "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))

Colony20TubePreT <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony20_Tube_1_Aug_18_AggnPreAM.csv")

Colony20TubePreT <- setNames(Colony20TubePreT, c("ID","Frames","X","Y","Orientation",
                                               "SizeWidth.px","SizeLeng.px","Speed.Px.s","Interpolated","HeadX","HeadY"))
Colony20CirclePre<-Colony20CirclePreT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=20, Nest="Circle",Trial="Pre",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony20TubePre<-Colony20TubePreT%>%
  mutate(Speed.Px.s=ifelse(Speed.Px.s<0,0,Speed.Px.s),Colony=20, Nest="Tube",Trial="Pre",
         Seconds=Frames/23.97602398,
         AntLength.sec=Speed.Px.s/mean(SizeLeng.px))%>%
  filter(Seconds < 300 ) %>% 
  select(-c(Frames,Speed.Px.s,Orientation,Interpolated))

Colony20Pre<-full_join(Colony20CirclePre,Colony20TubePre)

rm("Colony5CirclePreT", "Colony5TubePreT", "Colony5CircleAggnT", "Colony5TubeAggnT",
   "Colony6CirclePreT", "Colony6TubePreT", "Colony6CircleAggnT", "Colony6TubeAggnT",
   "Colony7CirclePreT", "Colony7TubePreT", "Colony7CircleAggnT", "Colony7TubeAggnT",
   "Colony8CirclePreT", "Colony8TubePreT", "Colony8CircleAggnT", "Colony8TubeAggnT",
   "Colony9CirclePreT", "Colony9TubePreT", "Colony9CircleAggnT", "Colony9TubeAggnT",
   "Colony11CirclePreT", "Colony11TubePreT", "Colony11CircleAggnT", "Colony11TubeAggnT",
   "Colony13CirclePreT", "Colony13TubePreT", "Colony13CircleAggnT", "Colony13TubeAggnT",
   "Colony17CirclePreT", "Colony17TubePreT", "Colony17CircleAggnT", "Colony17TubeAggnT",
   "Colony18CirclePreT", "Colony18TubePreT", "Colony18CircleAggnT", "Colony18TubeAggnT",
   "Colony20CirclePreT", "Colony20TubePreT", "Colony20CircleAggnT", "Colony20TubeAggnT")

## Creating the bins used throughout the code. These bins allow us to average worker speeds within given time bins. 
#SECONDS BINS
TwoSecsList<-Colony20Aggn%>%
  ungroup()%>%
  select(c("Seconds"))%>%
  distinct()%>%
  drop_na()%>%
  mutate(Seconds=cut_width(Seconds, width=2, boundary=0))%>%
  select(Seconds)%>%
  distinct()%>%
  mutate(obs=1:n(),Secs=obs*2,Seconds=gsub("\\[|\\]", "",Seconds),
         Seconds=gsub("\\(|\\)", "", Seconds))%>%
  select(-c(obs)) 

TwentySecBin<-Colony20Aggn%>%
  ungroup()%>%
  select(Seconds)%>%
  distinct()%>%
  mutate(Seconds=cut_width(Seconds, width=20, boundary=0))%>%
  select(Seconds)%>%
  distinct()%>%
  mutate(obs=1:n(),Secs20=obs*20,Seconds=gsub("\\[|\\]", "",Seconds),
         Seconds=gsub("\\(|\\)", "", Seconds))%>%
  select(-c(obs))

# DISTANCE TO ENTRANCE (Need to remove the outlier speed that can't really exist Antlengths/sec < 10)
#Distance function
# TUBE NESTS

AggnStudyRefCoords <- read_csv("/Users/gregchism/Desktop/ArchitectureExp_Fall2017_Working/AggnStudyRefCoords.csv") 

#NOTE: This first distance to the nest entrance function is to generate a dataframe called "FiveDistList".
DistanceCoordsFunctionTrackerTube1<-function(data.table){
  TubeCoords<-AggnStudyRefCoords%>%
    filter(Colony=="11"&Trial=="Aggn"&Nest=="Tube")%>%
    select(c(Coord,XREF,YREF))
  XCoords<-TubeCoords%>%
    select(-c(YREF))
  XCoords<-t(XCoords)
  XCoords<-row_to_names(XCoords,row_number =1)
  XREF1=as.numeric(XCoords[,1])
  XREF2=as.numeric(XCoords[,2])
  XREF3=as.numeric(XCoords[,3])
  XREF4=as.numeric(XCoords[,4])
  XREF5=as.numeric(XCoords[,5])
  XREF6=as.numeric(XCoords[,6])
  XREF7=as.numeric(XCoords[,7])
  YCoords<-TubeCoords%>%
    select(-c(XREF))
  YCoords<-t(YCoords)
  YCoords<-row_to_names(YCoords,row_number =1)
  YREF1=as.numeric(YCoords[,1])
  YREF2=as.numeric(YCoords[,2])
  YREF3=as.numeric(YCoords[,3])
  YREF4=as.numeric(YCoords[,4])
  YREF5=as.numeric(YCoords[,5])
  YREF6=as.numeric(YCoords[,6])
  YREF7=as.numeric(YCoords[,7])
  Width=YREF2-YREF3
  Dist1=sqrt(((XREF1-XREF2)^2)+((YREF1-YREF2)^2))
  Dist2=sqrt(((XREF2-XREF3)^2)+((YREF2-YREF3)^2))+Dist1
  Dist3=sqrt(((XREF4-XREF3)^2)+((YREF3-YREF4)^2))+Dist2
  Dist4=sqrt(((XREF5-XREF4)^2)+((YREF5-YREF4)^2))+Dist3
  Dist5=sqrt(((XREF6-XREF5)^2)+((YREF6-YREF5)^2))+Dist4
  MaxDist=sqrt(((XREF6-XREF7)^2)+((YREF6-YREF7)^2))+Dist5
  
  Colony11TubeAggnDist<<-data.table%>%
    filter(Nest=="Tube")%>%
    mutate(X1=
             ifelse(X>=XREF2&X<=(XREF2+(1.25*Width))&Y>=YREF3,
                    "YES","NO"),
           X2=
             ifelse(X<XREF2&Y>=YREF3,
                    "YES","NO"),
           X3=
             ifelse(X<=XREF4&Y<YREF3,
                    "YES","NO"),
           X4=
             ifelse(X>XREF4&Y<=YREF5,
                    "YES","NO"),
           X5=
             ifelse(X>=XREF6&Y>YREF5,
                    "YES","NO"),
           X6=
             ifelse(X>=XREF7&Y>=YREF7,
                    "YES","NO"),
           Y1=ifelse(X1=="YES","YES","NO"),
           Y2=ifelse(X2=="YES","YES","NO"),
           Y3=ifelse(X3=="YES","YES","NO"),
           Y4=ifelse(X4=="YES","YES","NO"),
           Y5=ifelse(X5=="YES","YES","NO"),
           Y6=ifelse(X6=="YES","YES","NO"),
           DistanceX = ifelse(X1=="YES",
                              X-XREF1,
                              ifelse(X2=="YES",
                                     XREF2-X,
                                     ifelse(X3=="YES",
                                            XREF3-X,
                                            ifelse(X4=="YES",
                                                   X-XREF4,
                                                   ifelse(X5=="YES",
                                                          X-XREF5,
                                                          ifelse(X6=="YES",
                                                                 XREF6-X,
                                                                 NA)))))),
           DistanceY = ifelse(Y1=="YES",
                              YREF1-Y,
                              ifelse(Y2=="YES",
                                     YREF2-Y,
                                     ifelse(Y3=="YES" | Y4 == "YES" & X < XREF4,
                                            YREF3-Y,
                                            ifelse(Y4=="YES",
                                                   YREF4-Y,
                                                   ifelse(Y5=="YES",
                                                          Y-YREF5,
                                                          ifelse(Y6=="YES",
                                                                 YREF6-Y,
                                                                 NA)))))),
           PythagDist=ifelse(X1=="YES"&Y1=="YES",
                             sqrt((DistanceX^2)+(DistanceY^2)),
                             ifelse(X2=="YES"&Y2=="YES",
                                    sqrt((DistanceX^2)+(DistanceY^2))+Dist1,
                                    ifelse(X3=="YES"&Y3=="YES",
                                           sqrt((DistanceX^2)+(DistanceY^2))+Dist2,
                                           ifelse(X4=="YES"&Y4=="YES",
                                                  sqrt((DistanceX^2)+(DistanceY^2))+Dist3,
                                                  ifelse(X5=="YES"&Y5=="YES",
                                                         sqrt((DistanceX^2)+(DistanceY^2))+Dist4,
                                                         ifelse(X6=="YES"&Y6=="YES",
                                                                sqrt((DistanceX^2)+(DistanceY^2))+Dist5,
                                                                NA)))))),
           ScaledDist=PythagDist/MaxDist,
           ScaledDist=ifelse(ScaledDist>1,1,ScaledDist))%>%
    ungroup()%>%
    mutate(DistBin=cut_width(ScaledDist, width=0.05, boundary=0),
           DistBin=gsub("\\[|\\]", "",DistBin),
           DistBin=gsub("\\(|\\)", "", DistBin))
}
DistanceCoordsFunctionTrackerTube1(Colony11TubeAggn)

## Creating the distance bin we used throughout the code. These bins allow us to average worker speeds within given distance bins 
## from the nest entrance to the very back of the nest
FiveDistList<-Colony11TubeAggnDist%>% # The code requires a reference dataset. We chose one with workers throughout the entire nest 
  ungroup()%>%                        # Ensuring there are no groupings
  select(ScaledDist) %>%
  distinct() %>%
  mutate(DistBin=cut_width(ScaledDist, width=0.05,boundary = 0))%>% #creating a set of bins by cutting scaled distance to the entrance (0 - 1) by 0.05 increments.
  select(DistBin)%>% # Selecting only the distance bin column. 
  distinct()%>% # Removing duplicate values
  drop_na()%>% # Dropping any NA values 
  arrange(DistBin)%>% # Arranging distance bins from smallest to largest. 
  mutate(obs=1:n(),Distance=0.05*obs, # Creating references for the bins
         DistBin=gsub("\\[|\\]", "",DistBin), # Next two lines create the format ex. 0.05,0.10 
         DistBin=gsub("\\(|\\)", "", DistBin))%>%
  select(-c(obs)) # Remove the obs reference column

#DIST TO ENTRANCE (WORKING)
#TUBE NEST
#AGGRESSION
DistanceCoordsFunctionTrackerTube1<-function(data.table){
  TubeCoords<-AggnStudyRefCoords%>%
    filter(Colony=="20"&Trial=="Aggn"&Nest=="Tube")%>%
    select(c(Coord,XREF,YREF))
  XCoords<-TubeCoords%>%
    select(-c(YREF))
  XCoords<-t(XCoords)
  XCoords<-row_to_names(XCoords,row_number =1)
  XREF1=as.numeric(XCoords[,1])
  XREF2=as.numeric(XCoords[,2])
  XREF3=as.numeric(XCoords[,3])
  XREF4=as.numeric(XCoords[,4])
  XREF5=as.numeric(XCoords[,5])
  XREF6=as.numeric(XCoords[,6])
  XREF7=as.numeric(XCoords[,7])
  YCoords<-TubeCoords%>%
    select(-c(XREF))
  YCoords<-t(YCoords)
  YCoords<-row_to_names(YCoords,row_number =1)
  YREF1=as.numeric(YCoords[,1])
  YREF2=as.numeric(YCoords[,2])
  YREF3=as.numeric(YCoords[,3])
  YREF4=as.numeric(YCoords[,4])
  YREF5=as.numeric(YCoords[,5])
  YREF6=as.numeric(YCoords[,6])
  YREF7=as.numeric(YCoords[,7])
  Width=YREF2-YREF3
  Dist1=sqrt(((XREF1-XREF2)^2)+((YREF1-YREF2)^2))
  Dist2=sqrt(((XREF2-XREF3)^2)+((YREF2-YREF3)^2))+Dist1
  Dist3=sqrt(((XREF4-XREF3)^2)+((YREF3-YREF4)^2))+Dist2
  Dist4=sqrt(((XREF5-XREF4)^2)+((YREF5-YREF4)^2))+Dist3
  Dist5=sqrt(((XREF6-XREF5)^2)+((YREF6-YREF5)^2))+Dist4
  MaxDist=sqrt(((XREF6-XREF7)^2)+((YREF6-YREF7)^2))+Dist5
  
  Colony20TubeAggnDist<<-data.table%>%
    filter(Nest=="Tube")%>%
    mutate(X1=
             ifelse(X>=XREF2&X<=(XREF2+(1.25*Width))&Y>=YREF3,
                    "YES","NO"),
           X2=
             ifelse(X<XREF2&Y>=YREF3,
                    "YES","NO"),
           X3=
             ifelse(X<=XREF4&Y<YREF3,
                    "YES","NO"),
           X4=
             ifelse(X>XREF4&Y<=YREF5,
                    "YES","NO"),
           X5=
             ifelse(X>=XREF6&Y>YREF5,
                    "YES","NO"),
           X6=
             ifelse(X>=XREF7&Y>=YREF7,
                    "YES","NO"),
           Y1=ifelse(X1=="YES","YES","NO"),
           Y2=ifelse(X2=="YES","YES","NO"),
           Y3=ifelse(X3=="YES","YES","NO"),
           Y4=ifelse(X4=="YES","YES","NO"),
           Y5=ifelse(X5=="YES","YES","NO"),
           Y6=ifelse(X6=="YES","YES","NO"),
           DistanceX = ifelse(X1=="YES",
                              X-XREF1,
                              ifelse(X2=="YES",
                                     XREF2-X,
                                     ifelse(X3=="YES",
                                            XREF3-X,
                                            ifelse(X4=="YES",
                                                   X-XREF4,
                                                   ifelse(X5=="YES",
                                                          X-XREF5,
                                                          ifelse(X6=="YES",
                                                                 XREF6-X,
                                                                 NA)))))),
           DistanceY = ifelse(Y1=="YES",
                              YREF1-Y,
                              ifelse(Y2=="YES",
                                     YREF2-Y,
                                     ifelse(Y3=="YES" | Y4 == "YES" & X < XREF4,
                                            YREF3-Y,
                                            ifelse(Y4=="YES",
                                                   YREF4-Y,
                                                   ifelse(Y5=="YES",
                                                          Y-YREF5,
                                                          ifelse(Y6=="YES",
                                                                 YREF6-Y,
                                                                 NA)))))),
           PythagDist=ifelse(X1=="YES"&Y1=="YES",
                             sqrt((DistanceX^2)+(DistanceY^2)),
                             ifelse(X2=="YES"&Y2=="YES",
                                    sqrt((DistanceX^2)+(DistanceY^2))+Dist1,
                                    ifelse(X3=="YES"&Y3=="YES",
                                           sqrt((DistanceX^2)+(DistanceY^2))+Dist2,
                                           ifelse(X4=="YES"&Y4=="YES",
                                                  sqrt((DistanceX^2)+(DistanceY^2))+Dist3,
                                                  ifelse(X5=="YES"&Y5=="YES",
                                                         sqrt((DistanceX^2)+(DistanceY^2))+Dist4,
                                                         ifelse(X6=="YES"&Y6=="YES",
                                                                sqrt((DistanceX^2)+(DistanceY^2))+Dist5,
                                                                NA)))))),
           ScaledDist=PythagDist/MaxDist,
           ScaledDist=ifelse(ScaledDist>1,1,ScaledDist))%>%
    ungroup()%>%
    mutate(DistBin=cut_width(ScaledDist, width=0.05, boundary=0),
           DistBin=gsub("\\[|\\]", "",DistBin),
           DistBin=gsub("\\(|\\)", "", DistBin)) %>%
full_join(FiveDistList)%>%
  group_by(ID,DistBin)%>%
  mutate(AvgSpeed=mean(AntLength.sec))%>%
    select(c("Colony","Nest","Trial","AvgSpeed","DistBin","ScaledDist","Distance","ID","Seconds")) %>%
  distinct()
}
DistanceCoordsFunctionTrackerTube1(Colony20TubeAggn)

#BASELINE
DistanceCoordsFunctionTrackerTube2<-function(data.table){
  TubeCoords<-AggnStudyRefCoords%>%
    filter(Colony=="20"&Trial=="Pre"&Nest=="Tube")%>%
    select(c(Coord,XREF,YREF))
  XCoords<-TubeCoords%>%
    select(-c(YREF))
  XCoords<-t(XCoords)
  XCoords<-row_to_names(XCoords,row_number =1)
  XREF1=as.numeric(XCoords[,1])
  XREF2=as.numeric(XCoords[,2])
  XREF3=as.numeric(XCoords[,3])
  XREF4=as.numeric(XCoords[,4])
  XREF5=as.numeric(XCoords[,5])
  XREF6=as.numeric(XCoords[,6])
  XREF7=as.numeric(XCoords[,7])
  YCoords<-TubeCoords%>%
    select(-c(XREF))
  YCoords<-t(YCoords)
  YCoords<-row_to_names(YCoords,row_number =1)
  YREF1=as.numeric(YCoords[,1])
  YREF2=as.numeric(YCoords[,2])
  YREF3=as.numeric(YCoords[,3])
  YREF4=as.numeric(YCoords[,4])
  YREF5=as.numeric(YCoords[,5])
  YREF6=as.numeric(YCoords[,6])
  YREF7=as.numeric(YCoords[,7])
  Width=YREF2-YREF3
  Dist1=sqrt(((XREF1-XREF2)^2)+((YREF1-YREF2)^2))
  Dist2=sqrt(((XREF2-XREF3)^2)+((YREF2-YREF3)^2))+Dist1
  Dist3=sqrt(((XREF4-XREF3)^2)+((YREF3-YREF4)^2))+Dist2
  Dist4=sqrt(((XREF5-XREF4)^2)+((YREF5-YREF4)^2))+Dist3
  Dist5=sqrt(((XREF6-XREF5)^2)+((YREF6-YREF5)^2))+Dist4
  MaxDist=sqrt(((XREF6-XREF7)^2)+((YREF6-YREF7)^2))+Dist5
  
  Colony20TubePreDist<<-data.table%>%
    filter(Nest=="Tube")%>%
    mutate(X1=
             ifelse(X>=XREF2&X<=(XREF2+(1.25*Width))&Y>=YREF3,
                    "YES","NO"),
           X2=
             ifelse(X<XREF2&Y>=YREF3,
                    "YES","NO"),
           X3=
             ifelse(X<=XREF4&Y<YREF3,
                    "YES","NO"),
           X4=
             ifelse(X>XREF4&Y<=YREF5,
                    "YES","NO"),
           X5=
             ifelse(X>=XREF6&Y>YREF5,
                    "YES","NO"),
           X6=
             ifelse(X>=XREF7&Y>=YREF7,
                    "YES","NO"),
           Y1=ifelse(X1=="YES","YES","NO"),
           Y2=ifelse(X2=="YES","YES","NO"),
           Y3=ifelse(X3=="YES","YES","NO"),
           Y4=ifelse(X4=="YES","YES","NO"),
           Y5=ifelse(X5=="YES","YES","NO"),
           Y6=ifelse(X6=="YES","YES","NO"),
           DistanceX = ifelse(X1=="YES",
                              X-XREF1,
                              ifelse(X2=="YES",
                                     XREF2-X,
                                     ifelse(X3=="YES",
                                            XREF3-X,
                                            ifelse(X4=="YES",
                                                   X-XREF4,
                                                   ifelse(X5=="YES",
                                                          X-XREF5,
                                                          ifelse(X6=="YES",
                                                                 XREF6-X,
                                                                 NA)))))),
           DistanceY = ifelse(Y1=="YES",
                              YREF1-Y,
                              ifelse(Y2=="YES",
                                     YREF2-Y,
                                     ifelse(Y3=="YES" | Y4 == "YES" & X < XREF4,
                                            YREF3-Y,
                                            ifelse(Y4=="YES",
                                                   YREF4-Y,
                                                   ifelse(Y5=="YES",
                                                          Y-YREF5,
                                                          ifelse(Y6=="YES",
                                                                 YREF6-Y,
                                                                 NA)))))),
           PythagDist=ifelse(X1=="YES"&Y1=="YES",
                             sqrt((DistanceX^2)+(DistanceY^2)),
                             ifelse(X2=="YES"&Y2=="YES",
                                    sqrt((DistanceX^2)+(DistanceY^2))+Dist1,
                                    ifelse(X3=="YES"&Y3=="YES",
                                           sqrt((DistanceX^2)+(DistanceY^2))+Dist2,
                                           ifelse(X4=="YES"&Y4=="YES",
                                                  sqrt((DistanceX^2)+(DistanceY^2))+Dist3,
                                                  ifelse(X5=="YES"&Y5=="YES",
                                                         sqrt((DistanceX^2)+(DistanceY^2))+Dist4,
                                                         ifelse(X6=="YES"&Y6=="YES",
                                                                sqrt((DistanceX^2)+(DistanceY^2))+Dist5,
                                                                NA)))))),
           ScaledDist=PythagDist / MaxDist,
           ScaledDist=ifelse(ScaledDist>1,1,ScaledDist))%>%
    ungroup() %>%
    mutate(DistBin=cut_width(ScaledDist, width=0.05, boundary = 0),
           DistBin=gsub("\\[|\\]", "",DistBin),
           DistBin=gsub("\\(|\\)", "", DistBin)) %>%
    full_join(FiveDistList)%>%
    group_by(ID,DistBin)%>%
    mutate(AvgSpeed=mean(AntLength.sec))%>%
    select(c("Colony","Nest","Trial","AvgSpeed","DistBin","ScaledDist","Distance","ID","Seconds"))%>%
    distinct()
}
DistanceCoordsFunctionTrackerTube2(Colony20TubePre)

#Reference coordinates for the circle nest
AggnStudyCircleRefCoords <- read_csv("/Users/gregchism/Desktop/ArchitectureExp_Fall2017_Working/AggnStudyCircleRefCoords.csv") 

#CIRCLE NEST
DistanceCoordsFunctionTrackerCircle1<-function(data.table){
  CircleCoords<-AggnStudyCircleRefCoords%>%
    filter(Colony=="20"&Trial=="Aggn"&Nest=="Circle")%>%
    select(c("Coord","XREF","YREF"))
  XCoords<-CircleCoords%>%
    select(-c("YREF"))
  XCoords<-t(XCoords)
  XCoords<-row_to_names(XCoords,row_number =1)
  XREF2=as.numeric(XCoords[,2])
  YCoords<-CircleCoords%>%
    select(-c("XREF"))
  YCoords<-t(YCoords)
  YCoords<-row_to_names(YCoords,row_number =1)
  YREF1=as.numeric(YCoords[,1])
  YREF2=as.numeric(YCoords[,2])
  MaxDist=abs(YREF1-YREF2)
  
  Colony20CircleAggnDist<<-data.table%>%
    mutate(DistanceX=XREF2-X,
           DistanceY=max(Y)-Y,
           PythagDist=sqrt((DistanceX^2)+(DistanceY^2)),
           DistanceTotal=PythagDist,
           ScaledDist=DistanceTotal/MaxDist,
           ScaledDist=ifelse(ScaledDist>1,1,ScaledDist))%>%
    ungroup()%>%
    mutate(DistBin=cut_width(ScaledDist, width=0.05, boundary=0),
           DistBin=gsub("\\[|\\]", "",DistBin),
           DistBin=gsub("\\(|\\)", "", DistBin))%>%
    full_join(FiveDistList)%>%
    group_by(ID,DistBin)%>%
    mutate(AvgSpeed=mean(AntLength.sec))%>%
    select(c("Colony","Nest","Trial","AvgSpeed","DistBin","ScaledDist","Distance","ID","Seconds"))
}
DistanceCoordsFunctionTrackerCircle1(Colony20CircleAggn)

DistanceCoordsFunctionTrackerCircle2<-function(data.table){
  CircleCoords<-AggnStudyCircleRefCoords%>%
    filter(Colony=="20"&Trial=="Pre"&Nest=="Circle")%>%
    select(c("Coord","XREF","YREF"))
  XCoords<-CircleCoords%>%
    select(-c("YREF"))
  XCoords<-t(XCoords)
  XCoords<-row_to_names(XCoords,row_number =1)
  XREF2=as.numeric(XCoords[,2])
  YCoords<-CircleCoords%>%
    select(-c("XREF"))
  YCoords<-t(YCoords)
  YCoords<-row_to_names(YCoords,row_number =1)
  YREF1=as.numeric(YCoords[,1])
  YREF2=as.numeric(YCoords[,2])
  MaxDist=abs(YREF1-YREF2)
  
  Colony20CirclePreDist<<-data.table%>%
    mutate(DistanceX=XREF2-X,
           DistanceY=max(Y)-Y,
           PythagDist=sqrt((DistanceX^2)+(DistanceY^2)),
           DistanceTotal=PythagDist,
           ScaledDist=DistanceTotal/MaxDist,
           ScaledDist=ifelse(ScaledDist>1,1,ScaledDist))%>%
    ungroup()%>%
    mutate(DistBin=cut_width(ScaledDist, width=0.05, boundary=0),
           DistBin=gsub("\\[|\\]", "",DistBin),
           DistBin=gsub("\\(|\\)", "", DistBin))%>%
    full_join(FiveDistList)%>%
    group_by(ID,DistBin)%>%
    mutate(AvgSpeed=mean(AntLength.sec))%>%
    select(c("Colony","Nest","Trial","AvgSpeed","DistBin","ScaledDist","Distance","ID","Seconds"))
}
DistanceCoordsFunctionTrackerCircle2(Colony20CirclePre)


## Combine datasets for each colony speeds
AggnAssayTest<-
  Colony9Aggn%>%
  full_join(Colony13Aggn)%>%
  full_join(Colony18Aggn)%>%
  full_join(Colony20Aggn)%>%
  full_join(Colony6Aggn)%>%
  full_join(Colony7Aggn)%>%
  full_join(Colony8Aggn)%>%
  select(Colony,Nest,Trial,Seconds,AntLength.sec,ID)

AggnAssayTest1<-Colony5Aggn%>%
  full_join(Colony11Aggn)%>%
  full_join(Colony17Aggn)%>%
  select(Colony,Nest,Trial,Seconds,AntLength.sec,ID)

PreAssayTest<-
  Colony9Pre%>%
  full_join(Colony5Pre)%>%
  full_join(Colony13Pre)%>%
  full_join(Colony18Pre)%>%
  full_join(Colony20Pre)%>%
  full_join(Colony6Pre)%>%
  full_join(Colony7Pre)%>%
  select(Colony,Nest,Trial,Seconds,AntLength.sec,ID)

PreAssayTest1<-Colony8Pre%>%
  full_join(Colony9Pre)%>%
  full_join(Colony11Pre)%>%
  full_join(Colony17Pre)%>%
  select(Colony,Nest,Trial,Seconds,AntLength.sec,ID)

AggnAssayTestFullRaw<-full_join(AggnAssayTest,AggnAssayTest1)

PreAssayTestFullRaw<-full_join(PreAssayTest,PreAssayTest1)

FullAssayTestSpeed<-full_join(AggnAssayTestFullRaw,PreAssayTestFullRaw)

rm("Colony5Aggn", "Colony5Pre", "Colony6Aggn", "Colony6Pre",
   "Colony7Aggn", "Colony7Pre", "Colony8Aggn", "Colony8Pre",
   "Colony9Aggn", "Colony9Pre", "Colony11Aggn", "Colony11Pre",
   "Colony13Aggn", "Colony13Pre", "Colony17Aggn", "Colony17Pre",
   "Colony18Aggn", "Colony18Pre", "Colony20Aggn", "Colony20Pre",
   "AggnAssayTest", "AggnAssayTest1", "PreAssayTest", "PreAssayTest1")

AggnAssayTestFull<-AggnAssayTestFullRaw%>%
  group_by(Colony,Nest)%>%
  mutate(Seconds=cut_width(Seconds, width=20, boundary=0),
         Seconds=gsub("\\[|\\]", "",Seconds),
         Seconds=gsub("\\(|\\)", "", Seconds))%>%
  left_join(TwentySecBin)

AggnAssayTestFullTwoSec<-AggnAssayTestFullRaw%>%
  group_by(Colony,Nest)%>%
  mutate(Seconds=cut_width(Seconds, width=2, boundary=0),
         Seconds=gsub("\\[|\\]", "",Seconds),
         Seconds=gsub("\\(|\\)", "", Seconds))%>%
  left_join(TwoSecsList)

PreAssayTestFull<-PreAssayTestFullRaw%>%
  group_by(Colony,Nest)%>%
  mutate(Seconds=cut_width(Seconds, width=20, boundary=0),
         Seconds=gsub("\\[|\\]", "",Seconds),
         Seconds=gsub("\\(|\\)", "", Seconds))%>%
  left_join(TwentySecBin)


PreAssayTestFullTwoSec<-PreAssayTestFullRaw%>%
  group_by(Colony,Nest)%>%
  mutate(Seconds=cut_width(Seconds, width=2, boundary=0),
         Seconds=gsub("\\[|\\]", "",Seconds),
         Seconds=gsub("\\(|\\)", "", Seconds))%>%
  left_join(TwoSecsList)

FullAssayTestSpeedTwoSec<-full_join(AggnAssayTestFullTwoSec,PreAssayTestFullTwoSec)%>%
  group_by(Colony,Nest,Trial,Seconds,ID)%>%
  mutate(AvgSpeed = mean (AntLength.sec))%>%
  select(-c(AntLength.sec))%>%
  left_join(AggnAssayTestSpeedTagsFull) %>%
  distinct()

FullAssayTestSpeedTwoSecTags<- FullAssayTestSpeedTwoSec %>%
  select(-c(Tag)) %>%
  left_join(AggnAssayTestSpeedTagsFull) %>%
  drop_na()

## Overall speed 
#Max values speed data
#This function rounds data to the nearest thousandths 
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=3))

AggnMax=AggnAssayTestFull%>%
  filter(AntLength.sec < 10)%>% 
  group_by(Nest)%>%
  mutate(MaxValues=specify_decimal(max(AntLength.sec),3))%>%
  select(Nest,MaxValues)%>%
  ungroup()%>%
  distinct()

PreMax=PreAssayTestFull%>%
  filter(AntLength.sec < 10)%>% 
  group_by(Nest)%>%
  mutate(MaxValues=specify_decimal(max(AntLength.sec),3))%>%
  select(Nest,MaxValues)%>%
  ungroup()%>%
  distinct()

CirclePreMax=PreAssayTestFull%>%
  filter(Nest=="Circle" & AntLength.sec < 10)%>% 
  group_by(Secs20)%>%
  mutate(MaxValues=specify_decimal(max(AntLength.sec),3))%>%
  select(Trial,Nest,Secs20,MaxValues)%>%
  ungroup()%>%
  distinct()

CircleAggnMax=AggnAssayTestFull%>%
  filter(Nest=="Circle" & AntLength.sec < 10)%>% 
  group_by(Secs20)%>%
  mutate(MaxValues=specify_decimal(max(AntLength.sec),3))%>%
  select(Trial,Nest,Secs20,MaxValues)%>%
  ungroup()%>%
  distinct()

TubeAggnMax=AggnAssayTestFull%>%
  filter(Nest=="Tube" & AntLength.sec < 10)%>% 
  group_by(Secs20)%>%
  mutate(MaxValues=specify_decimal(max(AntLength.sec),3))%>%
  select(Trial,Nest,Secs20,MaxValues)%>%
  ungroup()%>%
  distinct()

TubePreMax=PreAssayTestFull%>%
  filter(Nest=="Tube" & AntLength.sec < 10)%>% 
  group_by(Secs20)%>%
  mutate(MaxValues=specify_decimal(max(AntLength.sec),3))%>%
  select(Trial,Nest,Secs20,MaxValues)%>%
  ungroup()%>%
  distinct()

#Full comparison Aggn, Pre
AggnAssayTestFullAvg<-AggnAssayTestFull%>%
  filter(AntLength.sec < 10)%>%
  group_by(Colony,ID,Secs20)%>%
  mutate(AvgSpeed=mean(AntLength.sec))%>%
  ungroup()%>%
  select(-c(AntLength.sec))%>%
  distinct()

PreAssayTestFullAvg<-PreAssayTestFull%>%
  filter(AntLength.sec < 10)%>%
  group_by(Colony,ID,Secs20)%>%
  mutate(AvgSpeed=mean(AntLength.sec))%>%
  ungroup()%>%
  select(-c(AntLength.sec))%>%
  distinct()

#Components compare 
AggnAssayTestFullCircle<-AggnAssayTestFull%>%
  filter(Nest=="Circle" & AntLength.sec < 10)%>%
  group_by(Colony,ID,Secs20)%>%
  mutate(AvgSpeed=mean(AntLength.sec))%>%
  ungroup()%>%
  select(-c(AntLength.sec))%>%
  distinct()

AggnAssayTestFullTube<-AggnAssayTestFull%>%
  filter(Nest=="Tube" & AntLength.sec < 10)%>%
  group_by(Colony,ID,Secs20)%>%
  mutate(AvgSpeed=mean(AntLength.sec))%>%
  ungroup()%>%
  select(-c(AntLength.sec))%>%
  distinct()

PreAssayTestFullCircle<-PreAssayTestFull%>%
  filter(Nest=="Circle" & AntLength.sec < 10)%>%
  group_by(Colony,ID,Secs20)%>%
  mutate(AvgSpeed=mean(AntLength.sec))%>%
  ungroup()%>%
  select(-c(AntLength.sec))%>%
  distinct()

PreAssayTestFullTube<-PreAssayTestFull%>%
  filter(Nest=="Tube" & AntLength.sec < 10)%>%
  group_by(Colony,ID,Secs20)%>%
  mutate(AvgSpeed=mean(AntLength.sec))%>%
  ungroup()%>%
  select(-c(AntLength.sec))%>%
  distinct()

AggnTime<-
  ggplot(data=AggnAssayTestFullAvg,aes(y=AvgSpeed,x=Nest,fill=Nest))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00), alpha=0.65)+
  stat_summary(geom = 'text', label = AggnMax$MaxValues, 
               fun = max, 
               hjust = 0.5,
               vjust = -0.5,
               size = 4.5,
               family="Arial")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Aggn assay")+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=14,family="Arial"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=14, color = "white",family="Arial"),
        legend.key=element_blank(),
        legend.position = "none",
        legend.justification = c(1,1),
        legend.text=element_text(size = 14,family="Arial"),
        legend.title=element_text(size=16,family="Arial"),
        plot.title=element_text(size=18,face="bold",family="Arial"))+
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name="Nest",
                    values=c("blue", "red"))+
  ylim(0,0.01)

PreTime<-
  ggplot(data=PreAssayTestFullAvg,aes(y=AvgSpeed,x=Nest,fill=Nest))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00), alpha=0.65)+
  stat_summary(geom = 'text', label = PreMax$MaxValues, 
               fun = max, 
               hjust = 0.5,
               vjust = -0.5,
               size = 4.5,
               family="Arial")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Baseline")+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=14,family="Arial"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=14,family="Arial"),
        legend.key=element_blank(),
        legend.position = "none",
        legend.justification = c(1,1),
        legend.text=element_text(size = 14,family="Arial"),
        legend.title=element_text(size=16,family="Arial"),
        plot.title=element_text(size=18,face="bold",family="Arial"))+
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name="Nest",
                    values=c("blue", "red"))+
  ylim(0,0.01)

Speed.Fig<-ggarrange(PreTime, AggnTime,
                     labels = c("A","B"),
                     ncol = 2, nrow = 1,
                     common.legend = TRUE,
                     font.label = list(size=18,family="Arial"))

annotate_figure(Speed.Fig,
                top = NULL,
                bottom = text_grob("Nest shape", color = "black",
                                   size = 16, x = 0.53,family="Arial"),
                left = text_grob("Average ant-lengths / sec", color = "black",
                                 size = 16, rot = 90,family="Arial"),
                right = NULL
)

#Sample size 
PreAssayTestFullAvg %>% 
  group_by(Nest, Trial, Seconds) %>%
  mutate(Count = n()) %>%
  ungroup() %>%
  mutate(AvgCount = mean(Count), StdDev = sd(Count)) %>%
  select(c(AvgCount), StdDev) %>%
  distinct()

AggnAssayTestFullAvg %>% 
  group_by(Nest, Trial, Seconds) %>%
  mutate(Count = n()) %>%
  ungroup() %>%
  mutate(AvgCount = mean(Count), StdDev = sd(Count)) %>%
  select(c(AvgCount), StdDev) %>%
  distinct()

summary(lmer(AvgSpeed ~ Nest * Trial + (1|Colony) + (1|Tag), data = FullAssayTestSpeedTwoSec))
r.squaredGLMM(lmer(AvgSpeed ~ Nest * Trial + (1|Colony) + (1|Tag), data=FullAssayTestSpeedTwoSec))

Circle.AggnTime<-
  ggplot(data=AggnAssayTestFullCircle,aes(y=AvgSpeed,x=factor(Secs20)))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00), alpha=0.65, fill="blue")+
  ggtitle("Circle Aggn")+
  xlab(NULL)+
  ylab(NULL)+
  theme_pubclean()+
  ylim(0,0.04)+
  theme(axis.text.x = element_text(angle = 50,hjust = 1,size=16,family="Arial"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=16, color = "white",family="Arial"),
        axis.title = element_blank(),
        legend.text=element_text(size = 14,family="Arial"),
        legend.title=element_text(size=16,family="Arial"),
        plot.title=element_text(size=25,face="bold",family="Arial"))

Circle.PreTime<-ggplot(data=PreAssayTestFullCircle,aes(y=AvgSpeed,x=as.factor(Secs20)))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00),alpha=0.65, fill="blue")+
  ggtitle("Circle Baseline")+
  xlab(NULL)+
  ylab(NULL)+
  theme_pubclean()+
  ylim(0,0.04)+
  theme(axis.text.x = element_text(angle = 50,hjust = 1,size=16,family="Arial"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=16,family="Arial"),
        axis.title = element_blank(),
        plot.title=element_text(size=25,face="bold",family="Arial"))


Tube.PreTime<-ggplot(data=PreAssayTestFullTube,aes(y=AvgSpeed,x=as.factor(Secs20)))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65, fill="red")+
  ggtitle("Tube Baseline")+
  xlab(NULL)+
  ylab(NULL)+
  theme_pubclean()+
  ylim(0,0.04)+
  theme(axis.text.x = element_text(angle = 50,hjust = 1,size=16,family="Arial"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=16,family="Arial"),
        axis.title = element_blank(),
        plot.title=element_text(size=25,face="bold",family="Arial"))


Tube.AggnTime<-ggplot(data=AggnAssayTestFullTube,aes(y=AvgSpeed,x=as.factor(Secs20)))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65, fill="red")+
  ggtitle("Tube Aggn")+
  xlab(NULL)+
  ylab(NULL)+
  theme_pubclean()+
  ylim(0,0.04)+
  theme(axis.text.x = element_text(angle = 50,hjust = 1,size=16,family="Arial"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=16, color = "white",family="Arial"),
        axis.title = element_blank(),
        plot.title=element_text(size=25,face="bold",family="Arial"))


Speed.FigFull<-ggarrange(Circle.PreTime, Circle.AggnTime,
                         Tube.PreTime, Tube.AggnTime,
                         labels = c("A", "B","C","D"),
                         font.label = list(size = 24,family="Arial"),
                         ncol = 2, nrow = 2)

annotate_figure(Speed.FigFull,
                top = NULL,
                bottom = text_grob("Time (s)", color = "black",
                                   size = 30, x = 0.53,family="Arial"),
                left = text_grob("Average ant-lengths / sec", color = "black",
                                 size = 30, rot = 90,family="Arial"),
                right = NULL
)

stable <- desc_statby(AggnAssayTestFullMax, measure.var = "MaxValues",
                      grps = "Secs20")
stable <- AggnAssayTestFullMax[, c("Nest","Trial","Secs20", "MaxValues")]

# Summary table plot, WORK TO IMPROVE
stable.p <- ggtexttable(MaxFullTest, rows = NULL, cols = NULL, 
                        theme = ttheme("blank"))%>%
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2)%>%
  tab_add_hline(at.row = 5, row.side = "bottom", linewidth = 2)
MaxTablePlot<-stable.p%>%
  table_cell_font(row = 1, column = 1:17, face = "italic", color = "black", size = 14)%>%
  table_cell_font(row = 2:5, column = 1:17, size = 13)
write.csv(MaxFullTest,"MaxFullTest.csv")
view (AggnAssayTestFullMax)
TposeCirclePreMax<-CirclePreMax%>%
  select(Secs20,MaxValues)%>%
  rename(DistBin = Secs20, Circle = MaxValues)
TposeCirclePreMaxT<-t(TposeCirclePreMax)

TposeTubePreMax<-TubePreMax%>%
  select(MaxValues)%>%
  rename(" " = MaxValues)

TposeTubePreMaxT<-t(TposeTubePreMax)

TposeCircleAggnMax<-CircleAggnMax%>%
  select(MaxValues)%>%
  rename(Tube = MaxValues)
TposeCircleAggnMaxT<-t(TposeCircleAggnMax)

TposeTubeAggnMax<-TubeAggnMax%>%
  select(MaxValues)%>%
  rename(" " = MaxValues)
TposeTubeAggnMaxT<-t(TposeTubeAggnMax)

TposeMaxTest
TposeMaxTest<-rbind(TposeCirclePreMaxT,
                    TposeTubePreMaxT,
                    TposeCircleAggnMaxT,
                    TposeTubeAggnMaxT)

MaxNames = c("Dist Bin", "Circle", " ", "Tube", " ")
MaxNames
MaxFullTest<-cbind(MaxNames,TposeMaxTest)
MaxFullTest
TposeCircleAggnMaxT<-t(TposeCircleAggnMax)
TposeCircleAggnMaxT<-TposeCircleAggnMaxT%>%
  mutate(Trial == "Aggn")
AggnAssayTestFullMax<-CircleAggnMax%>%
  full_join(CirclePreMax)%>%
  full_join(TubeAggnMax)%>%
  full_join(TubePreMax)%>%
  mutate(MaxValues=as.numeric(MaxValues))
is.numeric(AggnAssayTestFullMax$MaxValues)

## Speed comparison plots
## IQR
#IQR calculations for overall speed
AggnAssayTestIQRTags<-FullAssayTestSpeedTwoSecTags%>%
  group_by(Colony,Nest,Trial,Tag)%>%
  mutate(IQRSecs=IQR(AvgSpeed))%>%
  select(-c(AvgSpeed))%>%
  distinct()%>%
  group_by(Colony,Nest,Trial)%>%
  mutate(AvgIQR=mean(IQRSecs))%>%
  select(Colony,Nest,Trial,AvgIQR)%>%
  distinct()

AggnAssayTestIQR<-AggnAssayTest%>%
  group_by(Colony,Nest)%>%
  mutate(Seconds=cut_width(Seconds, width=20, boundary=0),
         Seconds=gsub("\\[|\\]", "",Seconds),
         Seconds=gsub("\\(|\\)", "", Seconds))%>%
  left_join(TwentySecBin)%>%
  group_by(Colony,Nest,Secs20)%>%
  mutate(IQRSecs=IQR(AntLength.sec))%>%
  select(-c(AntLength.sec))%>%
  distinct()%>%
  group_by(Colony,Nest)%>%
  mutate(AvgIQR=mean(IQRSecs))%>%
  select(Colony,Nest,Trial,AvgIQR)%>%
  distinct()

PreAssayTestIQR<-PreAssayTest%>%
  group_by(Colony,Nest)%>%
  mutate(Seconds=cut_width(Seconds, width=20, boundary=0),
         Seconds=gsub("\\[|\\]", "",Seconds),
         Seconds=gsub("\\(|\\)", "", Seconds))%>%
  left_join(TwentySecBin)%>%
  group_by(Colony,Nest,Secs20)%>%
  mutate(IQRSecs=IQR(AntLength.sec))%>%
  select(-c(AntLength.sec))%>%
  distinct()%>%
  group_by(Colony,Nest)%>%
  mutate(AvgIQR=mean(IQRSecs))%>%
  select(Colony,Nest,Trial,AvgIQR)%>%
  distinct()

AggnAssayTestIQR1<-AggnAssayTest1%>%
  group_by(Colony,Nest)%>%
  drop_na() %>%
  mutate(Seconds=cut_width(Seconds, width=20, boundary=0),
         Seconds=gsub("\\[|\\]", "",Seconds),
         Seconds=gsub("\\(|\\)", "", Seconds))%>%
  left_join(TwentySecBin)%>%
  group_by(Colony,Nest,Secs20)%>%
  mutate(IQRSecs=IQR(AntLength.sec))%>%
  select(-c(AntLength.sec))%>%
  distinct()%>%
  group_by(Colony,Nest)%>%
  mutate(AvgIQR=mean(IQRSecs))%>%
  select(Colony,Nest,Trial,AvgIQR)%>%
  distinct()

PreAssayTestIQR1<-PreAssayTest1%>%
  group_by(Colony,Nest)%>%
  mutate(Seconds=cut_width(Seconds, width=20, boundary=0),
         Seconds=gsub("\\[|\\]", "",Seconds),
         Seconds=gsub("\\(|\\)", "", Seconds))%>%
  left_join(TwentySecBin)%>%
  group_by(Colony,Nest,Secs20)%>%
  mutate(IQRSecs=IQR(AntLength.sec))%>%
  select(-c(AntLength.sec))%>%
  distinct()%>%
  group_by(Colony,Nest)%>%
  mutate(AvgIQR=mean(IQRSecs))%>%
  select(Colony,Nest,Trial,AvgIQR)%>%
  distinct()

FullAssayTestIQR<-AggnAssayTestIQR%>%
  full_join(PreAssayTestIQR)%>%
  full_join(PreAssayTestIQR1)%>%
  full_join(AggnAssayTestIQR1)

AggnIQRMax=FullAssayTestIQR%>%
  filter(Trial=="Aggn")%>%
  group_by(Nest)%>%
  drop_na()%>%
  mutate(MaxValues=specify_decimal(max(AvgIQR),3))%>%
  select(Nest,MaxValues)%>%
  ungroup()%>%
  distinct()

PreIQRMax=FullAssayTestIQR%>%
  filter(Trial=="Pre")%>%
  group_by(Nest)%>%
  drop_na()%>%
  mutate(MaxValues=specify_decimal(max(AvgIQR),3))%>%
  select(Nest,MaxValues)%>%
  ungroup()%>%
  distinct()

SpeedPreIQR<-ggplot(data=FullAssayTestIQR%>%filter(Trial=="Pre"),aes(y=AvgIQR,x=Nest,fill=Nest))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00), alpha=0.65)+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Baseline")+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=14,family="Arial"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=14,family="Arial"),
        legend.key=element_blank(),
        legend.position = "none",
        legend.justification = c(1,1),
        legend.text=element_text(size = 14,family="Arial"),
        legend.title=element_text(size=16,family="Arial"),
        plot.title=element_text(size=18,face="bold",family="Arial"))+
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name="Nest",
                    values=c("blue", "red")) + 
  ylim(0, 0.0041)

SpeedAggnIQR<-ggplot(data=FullAssayTestIQR%>%filter(Trial=="Aggn"),aes(y=AvgIQR,x=Nest,fill=Nest))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00), alpha=0.65) +
  annotate("segment", x = 0.0001, xend = 0.45, y = 0.004, yend = 0.004,linetype="solid",color="gray50",size=1) +
  annotate("text", x = 0.001, y = 0.002, label = "Plot A y-lim",color="gray40",hjust=0)+
  annotate("text", x = 0.001, y = 0.005, label = "0.004",color="gray40",hjust=0)+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Aggn assay")+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=14,family="Arial"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=14, color = "black",family="Arial"),
        legend.key=element_blank(),
        legend.position = "none",
        legend.justification = c(1,1),
        legend.text=element_text(size = 14,family="Arial"),
        legend.title=element_text(size=16,family="Arial"),
        plot.title=element_text(size=18,face="bold",family="Arial"))+
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name="Nest",
                    values=c("blue", "red")) +
  ylim(0, 0.0305)


SpeedIQR.Fig<-ggarrange(SpeedPreIQR, SpeedAggnIQR,
                        labels = c("A","B"),
                        ncol = 2, nrow = 1,
                        common.legend = TRUE,
                        font.label = list(size = 18,
                                          family="Arial"))

annotate_figure(SpeedIQR.Fig,
                top = NULL,
                bottom = text_grob("Nest shape", color = "black",
                                   size = 16, x = 0.53,family="Arial"),
                left = text_grob("Average IQR ant-lengths / sec", color = "black",
                                 size = 16, rot = 90,family="Arial"),
                right = NULL
)

summary(lmer(AvgIQR ~ Nest * Trial + (1|Colony),data=FullAssayTestIQR))
r.squaredGLMM(lmer(AvgIQR ~ Nest * Trial + (1|Colony),data=FullAssayTestIQR))

SpeedPreIQRTags<-ggplot(data=AggnAssayTestIQRTags%>%filter(Trial=="Pre"),aes(y=AvgIQR,x=Nest,fill=Nest))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00), alpha=0.65)+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Baseline")+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=14,family="Arial"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=14,family="Arial"),
        legend.key=element_blank(),
        legend.position = "none",
        legend.justification = c(1,1),
        legend.text=element_text(size = 14,family="Arial"),
        legend.title=element_text(size=16,family="Arial"),
        plot.title=element_text(size=18,face="bold",family="Arial"))+
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name="Nest",
                    values=c("blue", "red")) +
  ylim(0, 0.015)

SpeedAggnIQRTags<-ggplot(data=AggnAssayTestIQRTags%>%filter(Trial=="Aggn"),aes(y=AvgIQR,x=Nest,fill=Nest))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00), alpha=0.65)+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Aggn assay")+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=14,family="Arial"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=14, color = "white",family="Arial"),
        legend.key=element_blank(),
        legend.position = "none",
        legend.justification = c(1,1),
        legend.text=element_text(size = 14,family="Arial"),
        legend.title=element_text(size=16,family="Arial"),
        plot.title=element_text(size=18,face="bold",family="Arial"))+
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name="Nest",
                    values=c("blue", "red"))  +
  ylim(0, 0.015)


SpeedIQR.FigTags<-ggarrange(SpeedPreIQRTags, SpeedAggnIQRTags,
                        labels = c("A","B"),
                        ncol = 2, nrow = 1,
                        common.legend = TRUE,
                        font.label = list(size = 18,
                                          family="Arial"))

annotate_figure(SpeedIQR.FigTags,
                top = NULL,
                bottom = text_grob("Nest shape", color = "black",
                                   size = 16, x = 0.53,family="Arial"),
                left = text_grob("Average IQR ant-lengths / sec", color = "black",
                                 size = 16, rot = 90,family="Arial"),
                right = NULL
)

#Sample size 
FullAssayTestSpeedTwoSecTags %>% 
  filter(Trial == "Pre") %>%
  group_by(Nest, Trial, Seconds) %>%
  mutate(Count = n()) %>%
  ungroup() %>%
  mutate(AvgCount = mean(Count), StdDev = sd(Count)) %>%
  select(c(AvgCount), StdDev) %>%
  distinct()

FullAssayTestSpeedTwoSecTags %>% 
  filter(Trial == "Aggn") %>%
  group_by(Nest, Trial, Seconds) %>%
  mutate(Count = n()) %>%
  ungroup() %>%
  mutate(AvgCount = mean(Count), StdDev = sd(Count)) %>%
  select(c(AvgCount), StdDev) %>%
  distinct()

summary(lmer(AvgIQR ~ Nest * Trial + (1|Colony),data=AggnAssayTestIQRTags))
r.squaredGLMM(lmer(AvgIQR ~ Nest * Trial + (1|Colony),data=AggnAssayTestIQRTags))

##DISTANCE TO ENTRANCE
## Combine datasets for each colony
Colony5AggnDist<-full_join(Colony5CircleAggnDist,Colony5TubeAggnDist)
Colony6AggnDist<-full_join(Colony6CircleAggnDist,Colony6TubeAggnDist)
Colony7AggnDist<-full_join(Colony7CircleAggnDist,Colony7TubeAggnDist)
Colony8AggnDist<-full_join(Colony8CircleAggnDist,Colony8TubeAggnDist)
Colony9AggnDist<-full_join(Colony9CircleAggnDist,Colony9TubeAggnDist)
Colony11AggnDist<-full_join(Colony11CircleAggnDist,Colony11TubeAggnDist)
Colony13AggnDist<-full_join(Colony13CircleAggnDist,Colony13TubeAggnDist)
Colony17AggnDist<-full_join(Colony17CircleAggnDist,Colony17TubeAggnDist)
Colony18AggnDist<-full_join(Colony18CircleAggnDist,Colony18TubeAggnDist)
Colony20AggnDist<-full_join(Colony20CircleAggnDist,Colony20TubeAggnDist)

Colony5PreDist<-full_join(Colony5TubePreDist,Colony5CirclePreDist)
Colony6PreDist<-full_join(Colony6CirclePreDist,Colony6TubePreDist)
Colony7PreDist<-full_join(Colony7TubePreDist,Colony7CirclePreDist)
Colony8PreDist<-full_join(Colony8CirclePreDist,Colony8TubePreDist)
Colony9PreDist<-full_join(Colony9CirclePreDist,Colony9TubePreDist)
Colony11PreDist<-full_join(Colony11CirclePreDist,Colony11TubePreDist)
Colony13PreDist<-full_join(Colony13CirclePreDist,Colony13TubePreDist)
Colony17PreDist<-full_join(Colony17CirclePreDist,Colony13TubePreDist)
Colony18PreDist<-full_join(Colony18CirclePreDist,Colony18TubePreDist)
Colony20PreDist<-full_join(Colony20CirclePreDist,Colony20TubePreDist)
Colony5AggnDist
AggnAssayDistTest<-
  Colony5AggnDist%>%
  full_join(Colony6AggnDist)%>%
  full_join(Colony7AggnDist)%>%
  full_join(Colony8AggnDist)%>%
  full_join(Colony9AggnDist)

AggnAssayDistTest1<-Colony13AggnDist%>%
  full_join(Colony17AggnDist)%>%
  full_join(Colony18AggnDist)%>%
  full_join(Colony20AggnDist)%>%
  full_join(Colony11AggnDist)

PreAssayDistTest<-Colony13PreDist%>%
  full_join(Colony18PreDist)%>%
  full_join(Colony20PreDist)%>%
  full_join(Colony6PreDist)%>%
  full_join(Colony7PreDist)%>%
  full_join(Colony5PreDist)

PreAssayDistTest1<-Colony8PreDist%>%
  full_join(Colony9PreDist)%>%
  full_join(Colony17PreDist)%>%
  full_join(Colony11PreDist)

AggnAssayDistTestFull<-full_join(AggnAssayDistTest,AggnAssayDistTest1)%>%
  drop_na()%>%
  select(-c(ScaledDist))%>%
  distinct()

PreAssayDistTestFull<-full_join(PreAssayDistTest,PreAssayDistTest1)%>%
  drop_na()%>%
  select(-c(ScaledDist))%>%
  distinct()

AggnAssayDistTestFullPlot<-AggnAssayDistTestFull%>%
  ungroup() %>%
  select(Colony,Nest,AvgSpeed,Distance)%>%
  group_by(Colony,Nest,Distance)%>%
  mutate(MinSpeed=min(AvgSpeed),MaxSpeed=max(AvgSpeed))%>%
  distinct()

PreAssayDistTestFullPlot<-PreAssayDistTestFull%>%
  ungroup() %>%
  select(Colony,Nest,AvgSpeed,Distance)%>%
  group_by(Colony,Nest,Distance)%>%
  mutate(MinSpeed=min(AvgSpeed),MaxSpeed=max(AvgSpeed))%>%
  distinct()

#Dist max values
AggnDistMax=AggnAssayDistTestFull%>%
  group_by(Nest)%>%
  drop_na()%>%
  mutate(MaxValues=signif(max(AvgSpeed),2))%>%
  select(Nest,MaxValues)%>%
  ungroup()%>%
  distinct()

PreDistMax=PreAssayDistTestFull%>%
  group_by(Nest)%>%
  drop_na()%>%
  mutate(MaxValues=signif(max(AvgSpeed),2))%>%
  select(Nest,MaxValues)%>%
  ungroup()%>%
  distinct()

CircleDistAggnMax=AggnAssayDistTestFull%>%
  drop_na()%>%
  filter(Nest=="Circle")%>% 
  group_by(Distance)%>%
  mutate(MaxValues=signif(max(AvgSpeed),2))%>%
  select(Distance,MaxValues)%>%
  ungroup()%>%
  distinct()

CircleDistPreMax=PreAssayDistTestFull%>%
  drop_na()%>%
  filter(Nest=="Circle")%>% 
  group_by(Distance)%>%
  mutate(MaxValues=signif(max(AvgSpeed),2))%>%
  select(Distance,MaxValues)%>%
  ungroup()%>%
  distinct()

TubeDistAggnMax=AggnAssayDistTestFull%>%
  drop_na()%>%
  filter(Nest=="Tube")%>% 
  group_by(Distance)%>%
  mutate(MaxValues=signif(max(AvgSpeed),2))%>%
  select(Distance,MaxValues)%>%
  ungroup()%>%
  distinct()

TubeDistPreMax=PreAssayDistTestFull%>%
  drop_na()%>%
  filter(Nest=="Tube")%>% 
  group_by(Distance)%>%
  mutate(MaxValues=signif(max(AvgSpeed),2))%>%
  select(Distance,MaxValues)%>%
  ungroup()%>%
  distinct()

#Dist plots
Circ.AggnDist<-ggplot(data=AggnAssayDistTestFull%>%filter(Nest=="Circle"),aes(y=AvgSpeed,x=as.factor(Distance)))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00),
                      fill="blue", alpha=0.65)+  
  ggtitle("Circle Aggn")+
  xlab(NULL)+
  ylab(NULL)+
  theme_pubclean()+
  ylim(0,0.045)+
  theme(axis.text.x = element_text(angle = 50,hjust = 1,size=16,family="Arial"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=14, color = "white",family="Arial"),
        plot.title=element_text(size=25,face="bold",family="Arial"))

Circ.PreDist<-ggplot(data=PreAssayDistTestFull%>%filter(Nest=="Circle"),aes(y=AvgSpeed,x=as.factor(Distance)))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00),fill="blue", alpha=0.65)+
  ggtitle("Circle Baseline")+
  xlab(NULL)+
  ylab(NULL)+
  theme_pubclean()+
  ylim(0,0.045)+
  theme(axis.text.x = element_text(angle = 50,hjust = 1,size=16,family="Arial"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=16,family="Arial"),
        axis.title = element_blank(),
        plot.title=element_text(size=25,face="bold",family="Arial"))
  
Tube.AggnDist<-ggplot(data=AggnAssayDistTestFull%>%filter(Nest=="Tube"),aes(y=AvgSpeed,x=as.factor(Distance)))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00),
                      fill="red", alpha=0.65)+  
  ggtitle("Tube Aggn")+
  xlab(NULL)+
  ylab(NULL)+
  theme_pubclean()+
  ylim(0,0.045)+
  theme(axis.text.x = element_text(angle = 50,hjust = 1,size=16,family="Arial"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=14, color = "white",family="Arial"),
        plot.title=element_text(size=25,face="bold",family="Arial"))

Tube.PreDist<-ggplot(data=PreAssayDistTestFull%>%filter(Nest=="Tube"),aes(y=AvgSpeed,x=as.factor(Distance)))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00),
                      fill="red", alpha=0.65)+  
  ggtitle("Tube Baseline")+
  xlab(NULL)+
  ylab(NULL)+
  theme_pubclean()+
  ylim(0,0.045)+
  theme(axis.text.x = element_text(angle = 50,hjust = 1,size=16,family="Arial"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=16,family="Arial"),
        axis.title = element_blank(),
        plot.title=element_text(size=25,face="bold",family="Arial"))

SpeedDist<-ggarrange(Circ.PreDist, Circ.AggnDist,
          Tube.PreDist, Tube.AggnDist,
          labels = c("A", "B","C","D"),
          font.label = list(size = 24,
                            family="Arial"),
          ncol = 2, nrow = 2)

annotate_figure(SpeedDist,
                top = NULL,
                bottom = text_grob("Scaled distance to the nest entrance", color = "black",
                                   size = 30, x = 0.53,family="Arial"),
                left = text_grob("Average ant-lengths / sec", color = "black",
                                 size = 30, rot = 90,family="Arial"),
                right = NULL
)

#Distance IQR 
AggnAssayTestDistIQR<-AggnAssayDistTest%>%
  drop_na()%>%
  ungroup()%>%
  select(Colony,Nest,Distance,AvgSpeed,Trial)%>%
  group_by(Colony,Nest,Distance)%>%
  mutate(Dist=as.numeric(Distance),IQRDist=IQR(as.numeric(AvgSpeed)))%>%
  select(-c(AvgSpeed))%>%
  distinct()%>%
  group_by(Colony,Nest)%>%
  mutate(AvgIQR=mean(IQRDist))%>%
  select(Colony,Nest,Trial,AvgIQR)%>%
  distinct()

PreAssayTestDistIQR<-PreAssayDistTest%>%
  drop_na()%>%
  ungroup()%>%
  select(Colony,Nest,Distance,AvgSpeed,Trial)%>%
  group_by(Colony,Nest,Distance)%>%
  mutate(Dist=as.numeric(Distance),IQRDist=IQR(as.numeric(AvgSpeed)))%>%
  select(-c(AvgSpeed))%>%
  distinct()%>%
  group_by(Colony,Nest)%>%
  mutate(AvgIQR=mean(IQRDist))%>%
  select(Colony,Nest,Trial,AvgIQR)%>%
  distinct()

AggnAssayTestDistIQR1<-AggnAssayDistTest1%>%
  drop_na()%>%
  ungroup()%>%
  select(Colony,Nest,Distance,AvgSpeed,Trial)%>%
  group_by(Colony,Nest,Distance)%>%
  mutate(Dist=as.numeric(Distance),IQRDist=IQR(as.numeric(AvgSpeed)))%>%
  select(-c(AvgSpeed))%>%
  distinct()%>%
  group_by(Colony,Nest)%>%
  mutate(AvgIQR=mean(IQRDist))%>%
  select(Colony,Nest,Trial,AvgIQR)%>%
  distinct()

PreAssayTestDistIQR1<-PreAssayDistTest1%>%
  drop_na()%>%
  ungroup()%>%
  select(Colony,Nest,Distance,AvgSpeed,Trial)%>%
  group_by(Colony,Nest,Distance)%>%
  mutate(Dist=as.numeric(Distance),IQRDist=IQR(as.numeric(AvgSpeed)))%>%
  select(-c(AvgSpeed))%>%
  distinct()%>%
  group_by(Colony,Nest)%>%
  mutate(AvgIQR=mean(IQRDist))%>%
  select(Colony,Nest,Trial,AvgIQR)%>%
  distinct()

FullAssayTestDistIQR<-AggnAssayTestDistIQR%>%
  full_join(PreAssayTestDistIQR)%>%
  full_join(AggnAssayTestDistIQR1)%>%
  full_join(PreAssayTestDistIQR1)

#Distance IQR Max
AggnIQRDistMax=FullAssayTestDistIQR%>%
  filter(Trial=="Aggn")%>%
  group_by(Nest)%>%
  drop_na()%>%
  mutate(MaxValues=specify_decimal(max(AvgIQR),3))%>%
  select(Nest,MaxValues)%>%
  ungroup()%>%
  distinct()

PreIQRDistMax=FullAssayTestDistIQR%>%
  filter(Trial=="Pre")%>%
  group_by(Nest)%>%
  drop_na()%>%
  mutate(MaxValues=specify_decimal(max(AvgIQR),3))%>%
  select(Nest,MaxValues)%>%
  ungroup()%>%
  distinct()


SpeedPreDistIQR<-ggplot(data=FullAssayTestDistIQR%>%filter(Trial=="Pre"),aes(y=AvgIQR,x=Nest,fill=Nest))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00), alpha=0.65) +
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Baseline")+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=14,family="Arial"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=14,family="Arial"),
        legend.key=element_blank(),
        legend.position = "none",
        legend.justification = c(1,1),
        legend.text=element_text(size = 14,family="Arial"),
        legend.title=element_text(size=16,family="Arial"),
        plot.title=element_text(size=18,face="bold",family="Arial"))+
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name="Nest",
                    values=c("blue", "red")) +
  ylim(0, 0.004)

SpeedAggnDistIQR<-ggplot(data=FullAssayTestDistIQR%>%filter(Trial=="Aggn"),aes(y=AvgIQR,x=Nest,fill=Nest))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00), alpha=0.65)+
  annotate("segment", x = 0.0001, xend = 0.45, y = 0.004, yend = 0.004,linetype="solid",color="gray50",size=1) +
  annotate("text", x = 0.001, y = 0.002, label = "Plot A y-lim",color="gray40",hjust=0, size = 4.5)+
  annotate("text", x = 0.001, y = 0.003, label = "0.004",color="gray40",hjust=0, size = 4.5)+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Aggn assay") +
  theme_pubclean()+ 
  theme(axis.text.x = element_text(size=14,family="Arial"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=14, color = "black",family="Arial"),
        legend.key=element_blank(),
        legend.position = "none",
        legend.justification = c(1,1),
        legend.text=element_text(size = 14,family="Arial"),
        legend.title=element_text(size=16,family="Arial"),
        plot.title=element_text(size=18,face="bold",family="Arial"))+
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name="Nest",
                    values=c("blue", "red")) +
  ylim(0, 0.015)

SpeedDist.IQR<-ggarrange(SpeedPreDistIQR, SpeedAggnDistIQR,
                         labels = c("A","B"),
                         ncol = 2, nrow = 1,
                         common.legend = TRUE,
                         font.label = list(size = 18,
                                           family="Arial"))

annotate_figure(SpeedDist.IQR,
                top = NULL,
                bottom = text_grob("Nest shape", color = "black",
                                   size = 16, x = 0.53,family="Arial"),
                left = text_grob("Average IQR avg. speed / distance bin", color = "black",
                                 size = 16, rot = 90,family="Arial"),
                right = NULL
)

#Sample size 
AggnAssayDistTestFull %>% 
  group_by(Nest, Trial, DistBin) %>%
  mutate(Count = n()) %>%
  ungroup() %>%
  mutate(AvgCount = mean(Count), StdDev = sd(Count)) %>%
  select(c(AvgCount), StdDev) %>%
  distinct()

PreAssayDistTestFull %>% 
  group_by(Nest, Trial, DistBin) %>%
  mutate(Count = n()) %>%
  ungroup() %>%
  mutate(AvgCount = mean(Count), StdDev = sd(Count)) %>%
  select(c(AvgCount), StdDev) %>%
  distinct()

summary(lmer(AvgIQR ~ Nest * Trial + (1|Colony),data=FullAssayTestDistIQR))
r.squaredGLMM(lmer(AvgIQR ~ Nest * Trial + (1|Colony),data=FullAssayTestDistIQR))

#Aggression Data
Aggression_Data_Full<-Aggression_Data_Working%>%
  mutate(Removed=Inv.Remov-Inv.Insert) 

AggnPlot1<- ggplot(data=Aggression_Data_Full,aes(y=Removed,x=Nest,fill=Nest))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00), alpha=0.65, width = 0.33)+
  xlab("Nest shape")+
  ylab("Time to remove invader (secs)")+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=20,family = "Arial", color = "black"),
        axis.title = element_text(size = 22,family="Arial", color = "black"),
        axis.text.y = element_text(size=20,family="Arial", color = "black"),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = "none") +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name="Nest",
                    values=c("blue", "red"))


summary(lmer(Removed ~ Nest + (1|Colony), data = Aggression_Data_Full))

summary(lmer(Removed ~ Nest * ScaledDist + (1|Colony), data = Aggression_Data_Full))

r.squaredGLMM(lmer(Removed ~ Nest * ScaledDist + (1|Colony), data = Aggression_Data_Full))

summary(lmer(Removed ~ Nest * Attacking.Max + (1|Colony), data=Aggression_Data_Full))

r.squaredGLMM(lmer(Removed ~ Nest * Attacking.Max + (1|Colony), data=Aggression_Data_Full))

hist(Aggression_Data_Full$Attacking.Max)

r.squaredGLMM(lmer(Removed ~ Nest * Attacking.Max + (1|Colony),data=Aggression_Data_Full_Test))
Aggression_Data_Full_Test<-Aggression_Data_Full %>%
  filter(ScaledDist > 0.2)
view(Aggression_Data_Dist)
view(Aggression_Data_Full)
Aggression_Data_Full 

AggnPlot2<-ggplot(data=Aggression_Data_Full,aes(y=Removed,x=ScaledDist))+
  geom_point(alpha=0.5,size=2.75, aes(color=Nest,shape=Nest))+
  xlab("Invader scaled distance to entrance ")+
  ylab("Time to remove invader (secs)")+
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_pubclean()+
  theme(axis.text.x = element_text(size=20,family = "Arial", colour = "black"),
        axis.title = element_text(size = 22,family="Arial"),
        axis.text.y = element_text(size=20,family="Arial",colour = "black"),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.justification = c(1,1),
        legend.text=element_text(size = 20,family="Arial"),
        legend.title=element_text(size = 22,family="Arial"))+
  scale_color_manual(breaks = c("Circle", "Tube"), 
                    name="Nest",
                    values=c("blue", "red")) + 
  xlim (0, 1)


AggnPlot3 <- ggplot(data=Aggression_Data_Full,aes(y=Removed, x=Attacking.Max))+
  geom_jitter(alpha=0.5,size=2.75, aes(color=Nest,shape=Nest), width = 0.15)+
  xlab("Maximum number of attacking workers")+
  ylab("Time to remove invader (secs)")+
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_pubclean()+
  theme(axis.text.x = element_text(size=20,family = "Arial", colour = "black"),
        axis.title = element_text(size = 22,family="Arial"),
        axis.text.y = element_text(size=20,family="Arial",colour = "black"),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.justification = c(1,1),
        legend.text=element_text(size = 20,family="Arial"),
        legend.title=element_text(size = 22,family="Arial"))+
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name="Nest",
                     values=c("blue", "red")) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8))

ggarrange(AggnPlot1, AggnPlot2, AggnPlot3,
                         labels = c("A","B","C"),
                         ncol = 2, nrow = 2,
                         common.legend = FALSE,
                         font.label = list(size = 22,
                                           family="Arial"),
          vjust = 1) 

## ANIMATED
styled <- labelled +
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = c(0, 1),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank()
  )


pal <- wes_palette("Zissou1", 100, type = "continuous")
p<-ggplot(Colony8CircleAggn,aes(X,Y,colour=Speed,label=ID,group=Nest))+
  geom_point(show.legend = F,alpha=0.75,size=2.5)+
  scale_color_gradientn(colours = pal,breaks=seq(min(Colony8CircleAggn$Speed),median(Colony8CircleAggn$Speed),max(Colony8CircleAggn$Speed))) +
  theme_pubclean()+
  facet_wrap(Nest~Trial,
             labeller = labeller(Nest = as_labeller(facet_labeller_top),
                                 Trial = as_labeller(facet_labeller_bottom)))+
  labs_pubr()+
  theme(strip.background = element_rect(color="white", fill="white", linetype="solid",size = 1))+
  theme(strip.text.x = element_text(size = 20, face = "bold"))+
  theme(strip.text.x = element_text(angle = 0, hjust = 0))+
  theme(axis.text.x = element_text(size=14),axis.title = element_text(size = 18))+
  theme(axis.text.y = element_text(size=14),axis.title = element_text(size = 18))+
  theme(plot.title = element_text(size = 22, face = "bold"))+
  xlab("X (px)")+
  ylab("Y (px)")+
  labs(title = "Seconds: {round(frame_time,2)}")+
  transition_time(Seconds) + 
  shadow_wake(wake_length = 0.1, alpha = FALSE)

facet_labeller_top <- function(variable, value) {
  c(
    "Circle", 
    "",
    "",
    ""
  )
}

facet_labeller_bottom <- function(variable, value) {
  c(
    "Aggn", 
    "Pre",
    "Tube",
    ""
  )
}
labs(title = "Seconds: {round(frame_time,2)}")+
  transition_time(Seconds) + 
  shadow_wake(wake_length = 0.1, alpha = FALSE)

enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')
p

animate(p, fps = 20, renderer = gifski_renderer())
anim_save("output.gif")
animate(p1,renderer=av_renderer("points_anim1.mp4"),height = 1000, width =1000)
gganimate::animate(p, format = "mp4", fps = 1, renderer = ffmpeg_renderer(format = "mp4"))
gganimate::anim_save("animateOutput.mp4", animation = last_animation(), path = "graphics")

pgif<-animate(p)
animate(p, nframes = 24, renderer = gifski_renderer("gganim.gif"))

anim_save("filenamehere.gif", animation=pgif, path=NULL)

install.packages("av")
library(av)

p1
p1<-ggplot(Colony20CircleAggn%>%filter(Seconds<10),aes(X,Y,colour=Speed,label=ID,group=Nest))+
  geom_point(show.legend = F,alpha=0.75,size=2.5)+
  scale_color_gradientn(colours = pal,breaks=seq(min(Colony20CircleAggn$Speed),median(Colony20CircleAggn$Speed),max(Colony20CircleAggn$Speed))) +
  theme_pubclean()+
  labs_pubr()+
  theme(axis.text.x = element_text(size=14),axis.title = element_text(size = 18))+
  theme(axis.text.y = element_text(size=14),axis.title = element_text(size = 18))+
  theme(plot.title = element_text(size = 22, face = "bold"))+
  xlab("X (px)")+
  ylab("Y (px)")
labs(title = "Seconds: {round(frame_time,2)}")+
  transition_time(Seconds) + 
  shadow_wake(wake_length = 0.1, alpha = FALSE)

ggplot(Colony20TubeAggn,aes(X,Y,colour=Speed,label=ID))+
  geom_point(show.legend = F,alpha=0.5,size=2)+
  scale_color_gradientn(colours = pal,breaks=seq(min(Colony20TubeAggn$Speed),median(Colony20TubeAggn$Speed),max(Colony20TubeAggn$Speed))) +
  theme_pubclean()+
  labs_pubr()+
  theme(axis.text.x = element_text(size=14),axis.title = element_text(size = 18))+
  theme(axis.text.y = element_text(size=14),axis.title = element_text(size = 18))+
  theme(plot.title = element_text(size = 22, face = "bold"))+
  xlab("X (px)")+
  ylab("Y (px)")


## SINGLE COLONY & EXTRA

## Time series plotting, analyses
Colony20CircleAggnTest<-Colony20CircleAggn%>%
  select(Seconds,Speed)%>%
  mutate(Seconds=cut_width(Seconds, width=5, boundary=0),
         Seconds=gsub("\\[|\\]", "",Seconds),
         Seconds=gsub("\\(|\\)", "", Seconds))%>%
  left_join(FiveSecsList)%>%
  group_by(Secs)%>%
  mutate(AvgSpeed=mean(Speed),StdSpeed=sd(Speed))%>%
  select(Secs,AvgSpeed,StdSpeed)%>%
  distinct()%>%
  ungroup()%>%
  mutate(observation = 1:n())%>%
  left_join(Colony20CircleAggnAvgTest)%>%
  mutate(SigChange=ifelse(AvgSpeed > SpeedCompPos | AvgSpeed < SpeedCompNeg, "1", "0"))
view(Colony20CircleAggn)
Colony20CircleAggnAvgTest<-Colony20CircleAggnTest%>%
  mutate(observation = observation+1, SpeedCompPos=AvgSpeed + 0.15 * AvgSpeed, SpeedCompNeg = AvgSpeed - 0.15*AvgSpeed)%>%
  select(SpeedCompPos,SpeedCompNeg,observation)


Colony8TubeAggnTest<-Colony8TubeAggn%>%
  select(Seconds,Speed)%>%
  mutate(Seconds=cut_width(Seconds, width=2, boundary=0),
         Seconds=gsub("\\[|\\]", "",Seconds),
         Seconds=gsub("\\(|\\)", "", Seconds))%>%
  left_join(TwoSecsList)%>%
  group_by(Secs)%>%
  mutate(AvgSpeed=mean(Speed),StdSpeed=sd(Speed))%>%
  select(Secs,AvgSpeed,StdSpeed)%>%
  distinct()%>%
  ungroup()%>%
  mutate(observation = 1:n())%>%
  left_join(Colony8TubeAggnAvgTest)%>%
  mutate(SigChange=ifelse(AvgSpeed > SpeedCompPos | AvgSpeed < SpeedCompNeg, "1", "0"))

Colony8TubeAggnAvgTest<-Colony8TubeAggnTest%>%
  mutate(observation = observation+1, SpeedCompPos=AvgSpeed + 0.15 * AvgSpeed, SpeedCompNeg = AvgSpeed - 0.15*AvgSpeed)%>%
  select(SpeedCompPos,SpeedCompNeg,observation)
max(Colony8CircleAggnTest$AvgSpeed)
ggplot(data=Colony8CircleAggnTest%>%drop_na(), aes(x=Secs, y = AvgSpeed))+
  geom_line()+
  geom_point(aes(color = SigChange),size=2)+
  ggtitle("Colony 8 Circle Aggn")+
  xlab("Time (s)")+
  ylab("Average ant-lengths / sec")+
  theme_pubclean()+
  scale_color_discrete(name = "Sign change", labels = c("No", "Yes"))+
  theme(axis.text.x = element_text(size=11),axis.title = element_text(size = 16))+
  theme(axis.text.y = element_text(size=11),axis.title = element_text(size = 16))

ggplot(data=Colony20CircleAggnTest%>%drop_na(), aes(x=Secs, y = AvgSpeed))+
  geom_line()+
  geom_point(aes(color = SigChange),size=2)+
  ggtitle("Colony 20 Circle Aggn")+
  xlab("Time (s)")+
  ylab("Average ant-lengths / sec")+
  theme_pubclean()+
  scale_color_discrete(name = "Signif change", labels = c("No", "Yes"))+
  theme(axis.text.x = element_text(size=11),axis.title = element_text(size = 16))+
  theme(axis.text.y = element_text(size=11),axis.title = element_text(size = 16))

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )

options(defaultPackages=...)

geom_hline(yintercept=0.01, linetype='dashed', col = 'gray50',size=1)+
  annotate("text", x = "200", y = 0.031, label = "A, C, D y-lim",color="gray40",hjust=0)+
  annotate("segment", x = "280", xend = "320", y = 0.031, yend = 0.031,linetype="dashed",color="gray50",size=1)

# inverse hyperbolic sine transformation function
ihs <- function(x) {
  y <- log(x + sqrt(x^2 + 1))
  return(y)
}

IQR = purrr::partial(IQR, na.rm = TRUE)
IQR = IQR(x)
IQR()
