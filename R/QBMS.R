#' QBMS conexion
#'
#' @param url go to QBMS documentation
#' @param engine "bms" or "breedbase"
#' @param path go to QBMS documentation
#' @param time_out go to QBMS documentation
#' @param no_auth go to QBMS documentation
#' @param username username
#' @param password password
#'
#' @return a list
#' @noRd
qbmsbrapi <- function(url = "https://bms.ciat.cgiar.org/ibpworkbench/controller/auth/login",
                      engine = c("bms", "breedbase","deltabreed"), # This was "" in the BI mod version for deltabreed
                      path = ifelse(engine == "bms", "bmsapi", "deltabreed"),
                      time_out = ifelse(engine == "bms", 120, 300),
                      no_auth = FALSE,
                      username = NULL,
                      password = NULL, 
                      brapi_ver = 'v2') {
  if (is.null(url) | url == "") {
    return()
  }

  bmsbase <- QBMS::set_qbms_config(
    url = url,
    path = path,
    time_out = time_out, # deleted in the mod version
    no_auth = no_auth,   # deleted in the mod version
    engine = ifelse(engine == "deltabreed", "bms", engine),
    page_size = 5000,
    brapi_ver = brapi_ver
  )

  if (!no_auth) {
    
    if(engine != "deltabreed"){
      if (is.null(username) | is.null(password))   return()
      if(username == "" | password == "") return()
    } else {
      if (is.null(password)) return() else if(password == "") return()
    }
    
    if (engine == "bms") {
      bmslogin <- QBMS::login_bms(username = username, password = password)
    } else if (engine == "breedbase"){
      bmslogin <- QBMS::login_breedbase(username = username, password = password)
    } else if(engine == "deltabreed"){
      bmslogin <- QBMS::set_token(password)
    }
  } else {
    bmslogin <- NULL
  }
  crops <- QBMS::list_crops()
  if(engine == "deltabreed") result <- list(bmsbase = bmsbase, crops = crops) else result <- list(bmsbase = bmsbase, bmslogin = bmslogin, crops = crops)
  return(result)
}

#' Get Programs
#'
#' @param crop crop
#'
#' @return a list with programs
#' @noRd
qbmsprograms <- function(crop = NULL) {
  if (is.null(crop)) {
    return()
  }
  if (QBMS::debug_qbms()$config$engine == "bms") {
    QBMS::set_crop(crop)
  }
  programs <- QBMS::list_programs()
  return(programs)
}

#' Get trials
#'
#' @param program program
#'
#' @return a list with trials
#' @noRd
qbmstrials <- function(program = NULL) {
  if (is.null(program)) {
    return()
  }
  QBMS::set_program(program)
  trials <- QBMS::list_trials()
  return(trials)
}

#' Get studies
#'
#' @param trial trial
#'
#' @return a list with studies
#' @noRd
#'
qbmsstudies <- function(trial = NULL) {
  if (is.null(trial)) {
    return()
  }
  QBMS::set_trial(trial)
  studies <- QBMS::list_studies()
  return(studies)
}

#' Get dataset
#'
#' @param studies string studies
#' @param dt_studies data.frame studies
#'
#' @return data.frame
#' @noRd
dataqbms <- function(studies = NULL, dt_studies = NULL) {
  if (is.null(studies)) {
    return()
  }
  if (is.null(dt_studies)) {
    return()
  }

  trial_study <- function(study, dt_studies) {
    trial <- dt_studies[dt_studies$studyName == study, "trial"] %>% as.character()
    QBMS::set_trial(trial)
    QBMS::set_study(study)
    data <- QBMS::get_study_data() %>%
      data.frame(check.names = TRUE, stringsAsFactors = T) %>%
      utils::type.convert(as.is = FALSE)
    return(data)
  }

  mult_dt <- lapply(studies, trial_study, dt_studies = dt_studies)
  engine <- QBMS::debug_qbms()$config$engine
  if(engine == "breedbase") {
    mult_dt <- data.table::rbindlist(
      l = mult_dt,
      fill = TRUE
    ) %>% 
      as.data.frame()
  } else if(engine == "deltabreed"){
    names(mult_dt) <- dplyr::filter(dt_studies, trimws(studyName) %in% trimws(studies))$trial
    mult_dt <- data.table::rbindlist(
      l = mult_dt,
      fill = TRUE,
      idcol = "trial"
    ) %>% 
      as.data.frame()
  } else {
    names(mult_dt) <- dt_studies$trial
    mult_dt <- data.table::rbindlist(
      l = mult_dt,
      fill = TRUE,
      idcol = "trial"
    ) %>% 
      as.data.frame()
  }
  
  return(mult_dt)
}
