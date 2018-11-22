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
#' @export run_grids
#' @examples \dontrun{
#' hello()
#' }


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
                        output_dir = outputdir,
                        type = 'COGNITIVE'){

  if(type == 'COGNITIVE'){

    data = full_join(
      fetchdata(exercise_db,
                experiments = cantab_expts[1])@data %>%
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
                 experiments = cantab_expts[1],
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
                 experiments = cantab_expts[1],
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

