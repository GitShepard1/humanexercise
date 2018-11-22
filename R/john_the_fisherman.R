#' @title Compute delta change
#'
#' @description Runs a bunch of correlations and then picks the best ones
#' @param experiment1 choose experiment for the x axis
#' @param experiment2 choose experiment for the y axis
#' @param filter_exercise filter by group. If null, uses all.
#' @param stat choose statistic r/p.value to sort the results by
#' @param hit_limit choose number of results to print out / plot
#' @param plot choose how to plot e.g. matrix for printouts or plot for plots
#' @param selected_x choose how to plot e.g. matrix for printouts or plot for plots
#' @import tidyverse
#' @import purrr
#' @import DBI
#' @import stringr
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @export john_the_fisherman
#' @examples \dontrun{
#' hello()
#' }

john_the_fisherman = function(experiment1,
                              experiment2,
                              filter_exercise = NULL,
                              stat = 'P',
                              hit_limit = 15,
                              plot = 'matrix',
                              selected_x = NULL,
                              selected_y = NULL,
                              xinterval = 0,
                              yinterval = 0,
                              split.gender = F
){

  if(is.null(filter_exercise)) { filter_exercise = c('AIT', 'MIT', 'LIT')}

  colour = if(length(filter_exercise)>1){

    'black'

  } else {

    if(filter_exercise == 'AIT'){

      'red'

    } else if (filter_exercise == 'MIT'){

      'blue'

    } else if (filter_exercise == 'LIT'){

      'darkgreen'
    }

  }


  data_object = map(list(experiment1, experiment2),
                    function(exp){

                      fetchdata(exercise_db,
                                experiments = exp,
                                omit_subjects = c('1075RM', '1014PJ')
                      )

                    }) %>%
    structure(names = c('x', 'y'))

  fields = structure(map(data_object, ~.@fields), names = c('x', 'y'))

  data = full_join(

    data_object[['x']] %>%
    {if(length(xinterval) == 1){
      filter(.@data, interval == xinterval) %>%
        dplyr::select(-interval)
    } else if(length(xinterval) == 2) {

      delta_change(., start=xinterval[1], end=xinterval[2], method = 'none') %>%
        dplyr::select(Subject, Gender, Exercise, variable, delta) %>%
        spread(variable, delta)

    }
    },

    data_object[['y']] %>%
    {if(length(yinterval) == 1){
      filter(.@data, interval == xinterval) %>%
        dplyr::select(-interval)
    } else if(length(yinterval) == 2) {

      delta_change(., start=yinterval[1], end=yinterval[2], method = 'none') %>%
        dplyr::select(Subject, Gender, Exercise, variable, delta) %>%
        spread(variable, delta)

    }
    },

    by = c('Subject', 'Gender', 'Exercise')

  )

  wk_df = data %>%
    dplyr::filter(Exercise %in% filter_exercise) %>%
    dplyr::select(-Subject, -Exercise, -Gender)


  corr_df = lapply(c('r', 'P'),

                   function(i){

                     rcorr(as.matrix(wk_df))[[i]][fields[['x']], fields[['y']]] %>%
                       as.tibble %>%
                       dplyr::mutate(x = fields[[1]]) %>%
                       gather(y, value, -x) %>%
                       setNames(c('x','y', i))

                   }) %>%
    reduce(left_join) %>%
    arrange(get(stat)) %>%
    filter(if (!is.null(selected_y))
      y %in% selected_y
      else y %in% y
    ) %>%
    filter(if (!is.null(selected_x))
      x %in% selected_x
      else x %in% x
    ) %>%
    slice(1:hit_limit) %>%
    dplyr::mutate(topplots = pmap(list(x,y,r,P), .f = function(X,Y,R,P){

      data %>%
        ggplot(aes(x = get(X), y = get(Y), colour = Exercise)) +
        geom_smooth(method = 'lm', se = F) +
        geom_point() +
        # scale_shape_manual(values=colour)+
        theme_bw() +
        ggtitle(paste('R =',round(R,3),', p =', round(P,3))) +
        theme(legend.position = 'none') +
        {if(length(xinterval) == 2)

          # xchange_lab = substitute(expression(paste(Delta, x), list(x = 'left_ca1')))

          list(
            geom_vline(xintercept = 0, linetype ='dashed'),
            xlab(paste('Delta', X))
          )
        } +
        {if(length(yinterval) == 2)

          list(
            geom_hline(yintercept = 0, linetype ='dashed'),
            ylab(paste('Delta', Y))
          )

        } +
        {if(split.gender == T) facet_wrap(~Gender)}


    }))


  if(is.null(plot)){

    corr_df %>% dplyr::select(-topplots)

  } else if (plot == 'matrix'){

    cowplot::plot_grid(
      plotlist = corr_df$topplots
    )

  }
}
