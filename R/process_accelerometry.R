#' @title process_accelerometry
#'
#' @description Processes accelerometry data from .agd files
#' @param n Number of files
#' @import tidyverse
#' @export process_accelerometry
#' @examples \dontrun{
#' hello()
#' }

process_accelerometry = function(Subject,
                                 nci.methods = F,
                                 start.date = as.Date("2014/1/5"), # actually I take the minimum date within agd file
                                 start.time = "00:00:00",
                                 id = NULL,
                                 brevity = 2,
                                 valid.days = 1,
                                 valid.week.days = 0 ,
                                 valid.weekend.days = 0,
                                 int.axis = "vert",
                                 int.cuts = c(100, 760, 1100, 1500),
                                 cpm.nci = FALSE,
                                 hourly.axis = "vert",
                                 days.distinct = FALSE,
                                 nonwear.axis = "vert",
                                 nonwear.window = 90, # 60 minutes is also an option but 90 min window could be more optimal
                                 nonwear.tol = 0,
                                 nonwear.tol.upper = 99,
                                 nonwear.nci = FALSE,
                                 weartime.minimum = 600,
                                 weartime.maximum = 1440,
                                 partialday.minimum = 1440,
                                 active.bout.length = 10,
                                 active.bout.tol = 0,
                                 mvpa.bout.tol.lower = 0,
                                 vig.bout.tol.lower = 0,
                                 active.bout.nci = F,
                                 sed.bout.tol = 0,
                                 artifact.axis = "vert",
                                 artifact.thresh = 25000,
                                 artifact.action = 1,
                                 weekday.weekend = F,
                                 return.form = 2){


  # list of parameters

  parameters <-
    list(
      "nci.methods"= nci.methods,
      "start.date"= start.date,
      "start.time"= start.time,
      "id"= id,
      "brevity"= brevity,
      "valid.days"= valid.days,
      "valid.week.days"= valid.week.days,
      "valid.weekend.days"= valid.weekend.days,
      "int.axis"= int.axis,
      "int.cuts"= int.cuts,
      "cpm.nci"= cpm.nci,
      "hourly.axis"= hourly.axis,
      "days.distinct"= days.distinct,
      "nonwear.axis"= nonwear.axis,
      "nonwear.window"= nonwear.window,
      "nonwear.tol"= nonwear.tol,
      "nonwear.tol.upper"= nonwear.tol.upper,
      "nonwear.nci"= nonwear.nci,
      "weartime.minimum"= weartime.minimum,
      "weartime.maximum"= weartime.maximum,
      "partialday.minimum"= partialday.minimum,
      "active.bout.length"= active.bout.length,
      "active.bout.tol"= active.bout.tol,
      "mvpa.bout.tol.lower"= mvpa.bout.tol.lower,
      "vig.bout.tol.lower"= vig.bout.tol.lower,
      "active.bout.nci"= active.bout.nci,
      "sed.bout.tol"= sed.bout.tol,
      "artifact.axis"= artifact.axis,
      "artifact.thresh"= artifact.thresh,
      "artifact.action"= artifact.action,
      "weekday.weekend"= weekday.weekend,
      "return.form"= return.form

    )


  sub = str_extract(Subject, '\\d{4}')

  acc_dat = tibble(filename = dir(filepath)[str_detect(dir(filepath), sub) &
                                              file_ext(dir(filepath)) == 'agd']
  ) %>%
    dplyr::mutate(

      Subject = map(filename, get_subjects),

      interval = map(filename, get_interval)

    ) %>%
    unnest(Subject, interval) %>%
    arrange(Subject, interval) %>%
    dplyr::mutate(

      acc_data_raw = map(filename, function(f){

        read_agd(file.path(filepath, f), tz = "GMT")[[2]]

      }),

      acc_startdate = map(acc_data_raw, ~min(.$timedate)),

      acc_data_sum = pmap(

        list(acc_data_raw, acc_startdate),

        .f = function(acc, date){

          accel.process.tri(counts.tri = acc[, 2:4],
                            steps = acc[, 5],
                            nci.methods = nci.methods,
                            start.date =  as.Date(date),
                            start.time =  start.time,
                            id = id,
                            brevity = brevity,
                            valid.days = valid.days,
                            valid.week.days = valid.week.days,
                            valid.weekend.days = valid.weekend.days,
                            int.axis = int.axis,
                            int.cuts = int.cuts,
                            cpm.nci = cpm.nci,
                            hourly.axis = hourly.axis,
                            days.distinct = days.distinct,
                            nonwear.axis = nonwear.axis,
                            nonwear.window = nonwear.window,
                            nonwear.tol = nonwear.tol,
                            nonwear.tol.upper = nonwear.tol.upper,
                            nonwear.nci = nonwear.nci,
                            weartime.minimum = weartime.minimum,
                            weartime.maximum = weartime.maximum,
                            partialday.minimum = partialday.minimum,
                            active.bout.length = active.bout.length,
                            active.bout.tol = active.bout.tol,
                            mvpa.bout.tol.lower = mvpa.bout.tol.lower,
                            vig.bout.tol.lower = vig.bout.tol.lower,
                            active.bout.nci = active.bout.nci,
                            sed.bout.tol =  sed.bout.tol,
                            artifact.axis = artifact.axis,
                            artifact.thresh = artifact.thresh,
                            artifact.action = artifact.action,
                            weekday.weekend = weekday.weekend,
                            return.form = return.form
          ) %>%
            as.tibble()


        }
      )
    )

  # daily_PA = map(acc_data_sum , function(acc){
  #
  #   mean(acc[, "cpm_vert"], na.rm = T)
  #
  # }),




  return_class = new("accelerometry",
                     start_date = unique(acc_dat$acc_startdate),
                     parameters = parameters,
                     raw_data = unnest(acc_dat, acc_data_raw),
                     sum_data = unnest(acc_dat, acc_data_sum) %>%
                       dplyr::select(-filename)
  )


  return(return_class)


}

