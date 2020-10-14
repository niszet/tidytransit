context("Frequencies are calculated correctly")

local_gtfs_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")

g = read_gtfs(local_gtfs_path) %>% set_date_service_table() %>% set_hms_times()

x = create_timetable(g, "Times Sq - 42 St")

x


