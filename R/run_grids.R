#' @title this runs the grid plots
#'
#' @description Runs a bunch of correlations and then picks the best ones
#' @param xvector choose experiment for the x axis
#' @param yvector choose experiment for the y axis
#' @param type choose experiment for the y axis
#' @import tidyverse
#' @import purrr
#' @import DBI
#' @import stringr
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @export plot_group
#' @examples \dontrun{
#' hello()
#' }

blood_expts = c("bloodMultiplexData","bloodCobasData","bloodElisasData", "bloodIgfData")

hump_colours = c('1' = '#76c100', '6' = '#fc0f00')

colors = list(
  'AIT' = c('#FF6D68', '#E92323'),
  'MIT' = c('#68B7FF', '#0037F6'),
  'LIT' = c('#D2FFBA', '#58F520')
)

group_parameters = list(

  'ALL' = list(
    'colours' = "Exercise",
    'has.x' = F,
    'has.y' = T,
    'lim' = 0.1
  ),


  'AIT' = list(
    'colours' = "#E92323",
    'has.x' = F,
    'has.y' = F,
    'lim' = 0.1
  ),

  'MIT' = list(
    'colours' = "#0037F6",
    'has.x' = T,
    'has.y' = T,
    'lim' = 0.3
  ),

  'LIT' = list(
    'colours' = "#58F520",
    'has.x' = T,
    'has.y' = F,
    'lim' = 0.1
  )
)


blood_parameters = list(

  'hgh' = list(
    'title' = '(ng/ml)',
    'limit' = 5,
    'annot' = 3
  ),

  'betahydroxy' = list(
    'title' = '(ng/ml)',
    'limit' = 110,
    'annot' = 20,
    'percent' = 300
  ),

  'cortisol' = list(
    'title' = '(ug/L)',
    'limit' = 180,
    'annot' = 60
  ),

  'gh' = list(
    'title' = '(ug/L)',
    'limit' = 4000,
    'annot' = 1000
  ),

  'insulin' = list(
    'title' = '(pmol/L)',
    'limit' = 160,
    'annot' = 60
  ),

  'insulin' = list(
    'title' = '(pmol/L)',
    'limit' = 160,
    'annot' = 60
  ),

  'leptin' = list(
    'title' = '(pg/ml)',
    'limit' = 20000,
    'annot' = 5000
  ),

  'bdnf' = list(
    'title' = '(pg/ml)',
    'limit' = 10000,
    'annot' = 2000
  ),

  'prolactin' = '(ng/ml)',

  'igf1' = '(ng/ml)'
)

# y limits lookup
limit_dict = c(
  'hgh' = 5,
  'betahydroxy' = 70,
  'cortisol' = 180,
  'gh' = 4000,
  'insulin' = 160,
  'leptin' = 20000,
  'prolactin' = 20,
  'bdnf' = 8000
)


ytitle = c(
  'hgh' = '(ng/ml)', # check this set of units
  'betahydroxy' = '(ng/ml)',
  'cortisol' = '(ug/L)',
  'gh' = '(ng/ml)',
  'insulin' = '(pmol/L)',
  'ketone' = '(ng/uL)',
  'leptin' = '(pg/ml)',
  'prolactin' = '(ng/ml)', # also check this
  'bdnf' = '(pg/ml)',
  'igf1' = '(ng/ml)'
)


cognitive_limits = list(

  'paltea' = 60,
  'paltea8' = 50,
  'hgh' = 5,
  'betahydroxy' = 70,
  'cortisol' = 180,
  'gh' = 4000,
  'insulin' = 160,
  'leptin' = 20000,
  'prolactin' = 20,
  'bdnf' = 8000,
  'left_CA1' = 0.1,
  'left_CA23/DG' = 0.2,
  'left_SUB' = 0.3,
  'left_Whole_hipp' = 0.3,
  "Whole_hipp" = 0.5,
  'right_CA1' = 0.1,
  'right_CA23DG' = 0.2,
  'right_SUB' = 0.3,
  'SUB' = 0.4
)


plot_group = function(ex,
                      df,
                      xvar,
                      yvar,
                      xlimit=c(0,110),
                      ylimit=c(0,50),
                      annotx){

  blood = str_split(xvar, '_')[[1]][1]

  df %>%
    ggscatter(x = xvar,
              y = yvar,
              color = group_parameters[[ex]]$colours,
              add = "reg.line",
              conf.int = F,
              cor.coef = TRUE,
              cor.coeff.args = list(method = "pearson",
                                    label.x = annotx,
                                    label.y = 0.3),
              # ylim = NA,
              # xlim = NA
              ylim = ylimit,
              xlim = xlimit
    ) +
    labs(x = '', y = '') +
    theme(
      legend.position = 'none',
      axis.title = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    scale_color_manual(values=group_colours) +
    {if(group_parameters[[ex]]$has.x == F)
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank())} +
            {if(group_parameters[[ex]]$has.y == F)
              theme(axis.ticks.y = element_blank(),
                    axis.text.y = element_blank())}

}

