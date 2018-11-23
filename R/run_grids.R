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
#' @export grid_scatter
#' @export run_grids
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

grid_scatter = function(xvar,
                        yvar,
                        sel_interval=0,
                        height = 7,
                        width = 7,
                        output_dir,
                        type = 'COGNITIVE'){

  if(type == 'COGNITIVE'){

    data = full_join(
      fetchdata(exercise_db,
                experiments = 'cantabPAL')@data %>%
        filter(interval %in% c(1,3,6)) %>%
        dplyr::mutate(interval = ifelse(interval == 1, 0, interval)),

      fetchdata(exercise_db,
                experiments = blood_expts[2]
                # omit_subjects = c('1102RC', '1110MD')
      )@data %>%
        filter(interval %in% c(0,3,6)),

      by = c('Subject', 'Exercise', 'interval', 'Gender')

    ) %>%
      dplyr::mutate(Exercise = factor(Exercise, levels = c('AIT', 'MIT', 'LIT')))


    data_pk = data %>%
      group_by(interval) %>%
      nest() %>%
      dplyr::mutate(Exercise = 'ALL') %>%
      bind_rows(

        data %>%
          group_by(Exercise, interval) %>%
          nest()

      ) %>%
      filter(interval == sel_interval)

  } else if(type == 'LEARNING'){

    data = full_join(

      fetchdata(exercise_db,
                experiments = blood_expts[2]
                # omit_subjects = c('1102RC', '1110MD')
      )@data %>%
        filter(interval %in% c(0,3,6)),

      (fetchdata(exercise_db,
                 experiments = 'cantabPAL',
                 omit_subjects = '1128LW'
      ) %>%
        delta_change(start = 1, end = 6) %>%
        reshape_delta_change())@data


    )

    data_pk = data %>%
      group_by(interval) %>%
      nest() %>%
      dplyr::mutate(Exercise = 'ALL') %>%
      bind_rows(

        data %>%
          group_by(Exercise, interval) %>%
          nest()

      ) %>%
      filter(interval == sel_interval)

  } else if(type == 'CHANGE'){

    data = full_join(

      (fetchdata(exercise_db,
                 experiments = blood_expts[2]
                 # omit_subjects = c('1102RC', '1110MD')
      ) %>%
        delta_change(start = 0, end = 6) %>%
        reshape_delta_change())@data,


      (fetchdata(exercise_db,
                 experiments = 'cantabPAL',
                 omit_subjects = '1128LW'
      ) %>%
        delta_change(start = 1, end = 6) %>%
        reshape_delta_change())@data

    )

    data_pk = data %>%
      nest() %>%
      dplyr::mutate(Exercise = 'ALL') %>%
      bind_rows(

        data %>%
          group_by(Exercise) %>%
          nest()

      )


  }

  bloodtype = str_split(xvar,'_')[[1]][1]

  if(type %in% c('LEARNING', 'COGNITIVE')){

    xheader = str_c(xvar,
                    blood_parameters[[str_split(xvar,'_')[[1]][1]]]$title,
                    ' at ',
                    sel_interval,
                    ' Month')
    fname = str_c(type,'-',xvar,'_', yvar, '_at_', sel_interval, '.png')

    print(fname)



    if(str_detect(xvar, 'percent')){

      xlimit = c(-1*blood_parameters[[bloodtype]]$percent, blood_parameters[[bloodtype]]$percent)
      xheader = str_c(xvar,
                      '(%)',
                      ' at ',
                      sel_interval,
                      ' Month')

    } else if(str_detect(xvar, 'prepost')) {

      xlimit = c(-blood_parameters[[bloodtype]]$limit, blood_parameters[[bloodtype]]$limit)

    } else {

      xlimit = c(0, blood_parameters[[bloodtype]]$limit)

    }

  } else if (type == 'CHANGE') {

    xheader = str_c(xvar,  blood_parameters[[str_split(xvar,'_')[[1]][1]]]$title,' change (baseline to 6 months)')

    fname = str_c(type,'-',xvar,'_', yvar, 'LEARNINGVSDELTA.png')

    if(str_detect(xvar, 'percent')){

      xlimit = c(-1*blood_parameters[[bloodtype]]$percent, blood_parameters[[bloodtype]]$percent)
      xheader = str_c(xvar,  '(%)',' learning (baseline to 6 months)')

    } else {

      xlimit = c(-1*blood_parameters[[bloodtype]]$limit, blood_parameters[[bloodtype]]$limit)

    }


  }

  if(type == 'COGNITIVE'){

    yheader = str_c(yvar, ' at ', sel_interval, ' Month')
    ylimit = c(0, 60)

    if (sel_interval == 0 ){

      yheader = str_c(yvar, ' at baseline')
    }

    print(yheader)

  } else if (type %in% c('LEARNING', 'CHANGE')){

    yheader = str_c(yvar, ' learning (baseline to 6 months)')
    ylimit = c(-60,60)

  }

  p = data_pk %>%
    dplyr::mutate(

      plots = pmap(

        list(Exercise, data),

        .f = plot_group,

        xvar = xvar,
        yvar = yvar,
        xlimit = xlimit,
        ylimit = ylimit,
        annotx = blood_parameters[[bloodtype]]$limit/4

      )

    ) %>%
    arrange(match(Exercise, c("ALL", "AIT", "MIT", "LIT")))


  # filter(p, Exercise == 'ALL') %>%
  #   unnest(data) %>%
  #   filter(!is.na(betahydroxy_prepost)) %>%
  #   count(Exercise1)
  #
  #
  # filter(p, Exercise == 'ALL') %>%
  #   unnest(data) %>%
  #   filter(!is.na(betahydroxy_prepost)) %>%
  #   pull(Subject) %>%
  #   unique() %>%
  #   as.list()



  # add legend
  plot = cowplot::plot_grid(

    cowplot::plot_grid(

      cowplot::get_legend(
        filter(p, Exercise == 'ALL')$plots[[1]] +
          theme(legend.position = 'top')
      )
    ),


    cowplot::plot_grid(
      plotlist = p$plots[1:4],

      rel_widths = c(1.08,0.9)

    ),

    ncol = 1,
    rel_heights = c(0.1,0.9)
  )


  # add padding and headings




  # MAKING PLOTS
  p1 <- cowplot::add_sub(plot,
                         xheader,
                         vpadding=grid::unit(2,"lines"),
                         y=2,
                         x=0.5,
                         vjust=6,
                         size=12)

  p2 <- ggdraw() + draw_label(yheader, fontface='plain', angle = 90, size = 12)


  # return plot
  psave = cowplot::plot_grid(p2, cowplot::ggdraw(p1), rel_widths = c(0.03,0.97))

  # return(plot_return)
  # save plot

  #
  ggsave(filename = fname,
         plot = psave,
         # width = width,
         # height = height,
         path = outputdir)

}


run_grids = function(xvector,
                     yvector,
                     type = c('COGNITIVE','LEARNING', 'CHANGE')
                     ){

  expand.grid(
    x = xvector,
    y = yvector,
    i = c(0,3,6),
    t = type
  ) %>%
    as.tibble() %>%
    dplyr::mutate(

      x = as.character(x),
      y = as.character(y),
      i = as.integer(i),
      t = as.character(t),

      run = pmap(
        list(x, y, i, t),

        .f = try(grid_scatter)

      ))



}
