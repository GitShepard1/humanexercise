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

  files = dir(datadir)

  print('LOADING FILES    *******************')
  lapply(dir(filedir)[file_ext(dir(filedir)) == "csv"], function(file) {

    msg = paste('Loading', f)
    print(msg)

    dbWriteTable(wk_db,
                 name = file_path_sans_ext(file),
                 value = read.csv(file.path(filedir, file)),
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


