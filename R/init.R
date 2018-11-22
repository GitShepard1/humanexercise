#' @title init
#'
#' @description Initialise Project
#' @param
#' @import tidyverse
#' @import DBI
#' @import tools
#' @export init
#' @examples \dontrun{
#' hello()
#' }


init = function(home = getwd(),
                datadir = './data'){

  print(paste("INITALISING PROJECT:", home, '*********************'))
  db_loc = file.path(home, 'exercise.sqlite')
  exercise_db <- DBI::dbConnect(RSQLite::SQLite(), 'exercise.sqlite')


  print('LOADING FILES    *******************')
  lapply(dir(datadir)[file_ext(dir(datadir)) == "csv"], function(f) {

    msg = paste('Loading', f)
    print(msg)

    dbWriteTable(exercise_db,
                 name = file_path_sans_ext(f),
                 value = read.csv(file.path(datadir, f)),
                 row.names = F,
                 overwrite = T)
  })

  print('******************* LOADING COMPLETE *******************')

  for(f in colnames(fields)){

    msg = paste('LOADING FIELDS FOR', f)
    print(msg)

  }



  # fills the exercise_db with data from './data

}


