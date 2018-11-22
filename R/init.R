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

  xnatfields <<- read.csv('./data/xnatfields.csv')

  files = dir(datadir)

  print('LOADING FILES    *******************')
  for(f in files[file_ext(files) == "csv"]){
    msg = paste('Loading', f)
    print(msg)

  }
  print('LOADING COMPLETE ******************* NEW')


  # fills the exercise_db with data from './data

}


