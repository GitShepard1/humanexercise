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
  for(f in files[file_ext(files) == "csv"]){
    msg = paste('Loading', f)
    print(msg)

  }
  print('LOADING COMPLETE ******************* NEW')

  for(f in colnames(fields)){

    msg = paste('LADING FIELDS FOR', f)
    print(msg)

  }




  # fills the exercise_db with data from './data

}


