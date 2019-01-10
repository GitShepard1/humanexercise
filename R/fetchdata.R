#' @title fetchdata
#'
#' @description Fetches data from in-memory / local sqlite database
#' @param db name of local database
#' @param experiments vector of queried experiments
#' @param fields vector of queried fields
#' @param omit_subjects vector of subjects to omit from query
#' @param selecte_interval vector of intervals to query
#' @param exact_match exact match?
#' @param include_age include age
#' @import tidyverse
#' @import purrr
#' @import DBI
#' @import stringr
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @export fetchdata
#' @examples \dontrun{
#' hello()
#' }

setClass("human_exercise",
         representation(
           fields = 'character',
           data = 'tbl'
         )
)

fetchdata = function(db = exercise_db,
                     experiments,
                     omit_subjects = '1014PJ',
                     selected_interval = NULL,
                     selected_fields = NULL,
                     exact_match = T,
                     include_age = F,
                     proj = 'P1'){

  DBI::dbConnect(exercise_db)

  parse_sql = function(input)  paste('(', paste0(input, collapse = ','), ')', sep='')

  dflist = experiments %>%
    map(function(e){

      print(str_c('QUERYING EXPERIMENT:', e))

      if (!is.null(e)){

        xnat = paste0(dplyr::filter(dplyr::select(fields, e),
                                    get(e) != '') %>%
                        pull(),
                      collapse = ',')

        xnat = ifelse(str_detect(e, 'blood'), paste(xnat,'prepost', sep=','), xnat)
        xsi_type = e


      } else {

        xnat = fields
        xsi_type = e

      }


      relabels = paste(
        "xnat_subjectdata_subject_label AS Subject",
        "xnat_subjectdata_gender_text AS Gender",
        "xnat_subjectdata_sub_group AS Exercise",
        'interval',
        'age',
        sep = ', '
      )
      query = paste(

        "SELECT ", relabels, ",", xnat, " FROM ", paste('opex_', e, sep=''),
        str_c(' WHERE project="',proj,'"'),

        sep = ' '

      )
      print(query)

      # set age aside
      age_df = tbl(exercise_db, sql(query)) %>%
        dplyr::select(Subject, Exercise, interval, Gender, age) %>%
        collect()

      wk_df = tbl(exercise_db, sql(query)) %>%
        dplyr::select(-age) %>%
        filter(!Subject %in% omit_subjects) %>%
        collect()

      # Reshape if bloods
      if(str_detect(e, 'blood')){

        wklist = wk_df %>%
          split(.$prepost) %>%
          map(function(df){

            bloods_names = df %>%
              dplyr::select(-Subject, -Gender, -Exercise, -interval, -prepost) %>%
              colnames()

            df %>%
              rename_at(vars(bloods_names), function(x) paste(x, unique(df$prepost), sep='_')) %>%
              dplyr::select(-prepost)

          }) %>%
          enframe() %>%
          dplyr::mutate(nrows = map(value, nrow)) %>%
          tidyr::unnest(nrows) %>%
          arrange(desc(nrows)) %>%
          dplyr::select(value)

        wk_df = wklist$value %>%
          reduce(full_join,
                 by = c('Subject', 'Gender', 'Exercise', 'interval'))


        wk_fields = dplyr::filter(dplyr::select(fields, e),
                                         get(e) != '') %>% pull()

        for (f in wk_fields){

          wk_df[,paste(f, 'prepost', sep ='_')] = wk_df[,paste(f, 'post', sep='_')] - wk_df[,paste(f, 'pre', sep='_')]
          wk_df[,paste(f, 'max', sep ='_')] = apply(wk_df[,paste0(f, c('_pre','_post'))], 1, function(x) max(x))
          wk_df[,paste(f, 'percent', sep ='_')] = (wk_df[,paste(f, 'post', sep='_')]-wk_df[,paste(f, 'pre', sep='_')])/wk_df[,paste(f, 'pre', sep='_')]*100

        }


      }

      if(include_age == T){

        wk_df = wk_df %>%
          left_join(age_df)

      }


      return(wk_df)


    }) %>%
    structure(names = experiments) %>%
    enframe() %>%
    dplyr::mutate(nrows = map(value, nrow)) %>%
    tidyr::unnest(nrows) %>%
    arrange(desc(nrows))

  joining_fields = c('Subject', 'Gender', 'Exercise', 'interval')

  matchExpression = paste(selected_fields, collapse ='|')

  data = dflist$value %>%
    reduce(full_join, by = c('Subject', 'Gender', 'Exercise', 'interval')) %>%
    dplyr::filter(Exercise %in% c('AIT', 'MIT', 'LIT')) %>%
    dplyr::mutate(Exercise = factor(Exercise, levels = c('AIT', 'MIT', 'LIT'))) %>%
    dplyr::filter(if (!is.null(selected_interval))
      interval %in% selected_interval
      else interval %in% interval
    ) %>%
    arrange(Subject, interval) %>%
    {if(!is.null(selected_fields))

      if(exact_match == F)
        dplyr::select(., joining_fields, matches(matchExpression))
      else
        dplyr::select(.,joining_fields, selected_fields)

      else . }

  fields = dplyr::select(data, -Subject, -Gender, -Exercise, -interval) %>%
    colnames()

  return_class = new('human_exercise',
                     fields = fields,
                     data = data %>% {if('project' %in% colnames(data)) dplyr::select(., -project) else .}
  )

  return(return_class)

  DBI::dbDisconnect(exercise_db)

}


