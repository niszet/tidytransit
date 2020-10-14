#' @importFrom dplyr mutate pull select right_join
#' @importFrom tibble deframe
#' @importFrom tidyr unite
#' @importFrom forcats fct_recode
create_timetable = function(gtfs_obj, stop_names, min_pattern_count = 5) {
	stop_name <- stop_id <- trip_id <- agency_id <- agency_name <- route_long_name <- NULL
	trip_short_name <- departure_pattern <- n <- N <- tmp <- NULL
	if(!feed_contains(gtfs_obj, "date_service_table")) {
		message("gtfs_obj does not contain date_service_table, see ?set_date_service_table")
		gtfs_obj <- set_date_service_table(gtfs_obj)
	}
	if(!all(c("departure_time_hms", "arrival_time_hms") %in% colnames(gtfs_obj$stop_times))) {
		message("gtfs_obj does not contain hms times, see ?set_hms_times")
		gtfs_obj <- set_hms_times(gtfs_obj)
	}

	# only stop_times from trips passing throug stop_names
	stop_ids = gtfs_obj$stops %>% filter(stop_name %in% stop_names) %>% dplyr::pull(stop_id)
	trip_ids = gtfs_obj$stop_times %>% filter(stop_id %in% stop_ids) %>% dplyr::pull(trip_id) %>% unique()

	stop_times <- gtfs_obj$stop_times %>%
		filter(trip_id %in% trip_ids) %>%
		left_join(gtfs_obj$trips, "trip_id") %>%
		left_join(gtfs_obj$routes, "route_id") %>%
		left_join(gtfs_obj$stops, "stop_id") %>%
		left_join(dplyr::select(gtfs_obj$agency, agency_id, agency_name), "agency_id") %>%
		dplyr::select(-route_long_name, -agency_id, -trip_short_name)

	# duplicate relevant stop_times for each date of a year
	stop_times_year = stop_times %>%
		dplyr::right_join(gtfs_obj$.$date_service_table, "service_id") %>%
		filter(!is.na(trip_id)) %>%
		add_departure_patterns()

	dates_dps = stop_times_year %>%
		group_by(date) %>%
		summarise(departure_pattern = first(departure_pattern)) %>%
		ungroup()

	# count departure patterns
	dps_counts = dates_dps %>%
		group_by(departure_pattern) %>%
		dplyr::count() %>%
		ungroup() %>% arrange(desc(n))

	# rename departure patterns
	if(length(unique(dps_counts$n)) == nrow(dps_counts)) {
		dps_counts_vec = stats::setNames(dps_counts$departure_pattern, dps_counts$n)
	} else {
		.letters = c(letters, paste0(letters, "2"), paste0(letters, "3"),  paste0(letters, "4"),  paste0(letters, "5"))
		add_letter = function(rownums) {
			if(length(rownums) > 1) {
				return(.letters[rownums])
			} else {
				return("")
			}
		}
		dps_counts_vec = dps_counts %>%
			group_by(n) %>%
			dplyr::mutate(tmp = add_letter(dplyr::row_number())) %>% ungroup() %>%
			tidyr::unite(N, n, tmp, sep = "") %>%
			select(N, departure_pattern) %>%
			tibble::deframe()
	}
	
	browser()

	stop_times_year$departure_pattern <- fct_recode(stop_times_year$departure_pattern, !!!dps_counts_vec)
	dates_dps$departure_pattern <- fct_recode(dates_dps$departure_pattern, !!!dps_counts_vec)
	dates_dps$departure_pattern <- factor(dates_dps$departure_pattern, levels = names(dps_counts_vec))

	dps_date = dates_dps %>%
		group_by(departure_pattern) %>%
		summarise(date = first(date)) %>% deframe()

	#
	stop_times_dps = stop_times_year %>% 
	  filter(date %in% dps_date)
	
	# time offsets / takt
	stop_times_dps <- .add_ids(stop_times_dps)
	stop_times_dps <- .add_time_offsets(stop_times_dps)
	stop_times_dps$example_date <- stop_times_dps$date
	stop_times_dps$date <- NULL

	# build object
	timetable = list()
	class(timetable) <- "timetable"
	timetable$stop_name <- stop_names
	timetable$stop_times <- stop_times_dps
	timetable$dates_dps <- dates_dps
	timetable$dps_date <- dps_date
	timetable$departure_patterns <- names(dps_date)

	return(timetable)
}

add_departure_patterns = function(stop_times_year) {
	departure_time_hms <- route_id <- trip_headsign <- NULL
	stopifnot("date" %in% colnames(stop_times_year))
	stop_times_year <- stop_times_year %>%
		group_by(date) %>%
		dplyr::mutate(departure_pattern = .create_departure_pattern(departure_time_hms, route_id, trip_headsign)) %>%
		ungroup()

	stop_times_year$departure_pattern <- substr(stop_times_year$departure_pattern, 0, 12)
	return(stop_times_year)
}

.create_departure_pattern = function(departure_time_hms, route_id, trip_headsign) {
	dig_df = list(departure_time_hms, route_id, trip_headsign)
	x = digest::digest(dig_df)
	return(x)
}

.add_ids = function(stop_times_year) {
#   warning(".add_ids")
# 	r_type = stop_times_year$route_type
# 	if(.na_quota(stop_times_year$route_desc) < 0.05) {
# 		r_type <- stop_times_year$route_desc
# 	}
# 
# 	r_name = stop_times_year$route_id
# 	if(.na_quota(stop_times_year$route_short_name) < 0.05) {
# 	  warning("TODO use route_short_name")
# 		# r_name <- stop_times_year$route_short_name %>% stringr::str_replace("-Y", "")
# 	}
# 
# 	stop_times_year$id <- paste(r_type, r_name, "nach", stop_times_year$trip_headsign)
# 	stop_times_year$route_name <- paste(r_type, r_name)
  stop_times_year$id <- stop_times_year$route_id
  stop_times_year$route_name <- stop_times_year$route_short_name
	return(stop_times_year)
}

.add_time_offsets = function(dd) {
  warning(".add_time_offsets")
	id <- i <- j <- departure_time_hms <- NULL
	browser()
	stopifnot("departure_time_hms" %in% colnames(dd))
	x1 = dd %>% arrange(departure_time_hms) %>% 
	  # group_by(id, stop_id, departure_pattern) %>% 
	  mutate(time_offset = c(departure_time_hms[1:(length(departure_time_hms)-1)], NA) -
	           c(NA, departure_time_hms[2:length(departure_time_hms)])) %>% 
	  ungroup()
	x2 = x1 %>% select(id, i=j, departure_time_hms)
	x1 = x1 %>% select(-j)
	
	# TODO rolling delta within group (departure_pattern, id, stop_id) instead of appending columns
	xx = left_join(x1, x2, c("id", "i"))
	delta <- as.numeric(xx$departure_time_hms.x-xx$departure_time_hms.y)
	dd$time_offset <- delta

	return(dd)
}

.na_quota = function(vec) {
	length(which(is.na(vec)))/length(vec)
}
