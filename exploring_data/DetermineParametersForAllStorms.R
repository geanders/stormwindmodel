pull_will_inputs <- function(storm_track){
  full_track <- create_full_track(hurr_track = storm_track)
  with_wind_radii <- add_wind_radii(full_track = full_track)
  return(with_wind_radii)
}

library(stormwindmodel)
library(hurricaneexposure)
library(hurricaneexposuredata)
library(dplyr)
library(tidyr)
data(hurr_tracks)
track_list <- split(hurr_tracks, f = hurr_tracks$storm_id)
will_input_summaries <- lapply(track_list, pull_will_inputs)
will_input_summaries <- bind_rows(will_input_summaries, .id = "storm_id")

library(ggplot2)
will_input_summaries %>%
  select(tcspd, tcdir, vmax_sfc_sym, vmax_gl, Rmax, X1, n, A,
         eq3_right, xi, R1, R2) %>%
  gather(input, value) %>%
  ggplot(aes(x = value)) + geom_histogram(bins = 30) +
  facet_wrap(~ input, scales = "free_x")

summary(will_input_summaries)

func_constant <- will3_right(
  n = mean(will_input_summaries$n, na.rm = TRUE),
  A = mean(will_input_summaries$A, na.rm = TRUE),
  X1 = mean(will_input_summaries$X1, na.rm = TRUE),
  Rmax = mean(will_input_summaries$Rmax, na.rm = TRUE))

func_constant <- will3_right(
  n = max(will_input_summaries$n, na.rm = TRUE),
  A = max(will_input_summaries$A, na.rm = TRUE),
  X1 = max(will_input_summaries$X1, na.rm = TRUE),
  Rmax = min(will_input_summaries$Rmax, na.rm = TRUE))

to_plot <- data_frame(xi = seq(-100, 100, by = 1)) %>%
  mutate(f_xi = sapply(xi, function(xi){
    out <- will3_deriv_func(xi = xi, eq3_right = func_constant)
    return(out[2])
    })) %>%
  mutate(f_xi_prime = sapply(xi, function(xi){
    out <- will3_deriv_func(xi = xi, eq3_right = func_constant)
    return(out[1])
  }))
ggplot(to_plot, aes(x = xi, y = f_xi)) + geom_line() +
  geom_hline(yintercept = 0, linetype = 3)
ggplot(to_plot, aes(x = xi, y = f_xi_prime)) + geom_line()

to_plot <- data_frame(xi = seq(-1, 1, by = 0.01)) %>%
  mutate(f_xi = sapply(xi, function(xi){
    out <- will3_deriv_func(xi = xi, eq3_right = func_constant)
    return(out[2])
  })) %>%
  mutate(f_xi_prime = sapply(xi, function(xi){
    out <- will3_deriv_func(xi = xi, eq3_right = func_constant)
    return(out[1])
  }))
ggplot(to_plot, aes(x = xi, y = f_xi)) + geom_line() +
  geom_hline(yintercept = 0, linetype = 3)
ggplot(to_plot, aes(x = xi, y = f_xi_prime)) + geom_line()

to_plot <- data_frame(xi = seq(0, 1, by = 0.01)) %>%
  mutate(f_xi = sapply(xi, function(xi){
    out <- will3_deriv_func(xi = xi, eq3_right = func_constant)
    return(out[2])
  })) %>%
  mutate(f_xi_prime = sapply(xi, function(xi){
    out <- will3_deriv_func(xi = xi, eq3_right = func_constant)
    return(out[1])
  }))
ggplot(to_plot, aes(x = xi, y = f_xi)) + geom_line() +
  geom_hline(yintercept = 0, linetype = 3)
ggplot(to_plot, aes(x = xi, y = f_xi_prime)) + geom_line()

will_input_summaries %>%
  filter(Rmax > 250) %>%
  select(storm_id) %>%
  distinct()

library(ggmap)
us_map <- get_map(location = "georgia", zoom = 5)
a <- ggmap(us_map)
a + geom_point(data = filter(will_input_summaries, storm_id == "Andrea-2013"),
               aes(x = -tclon, y = tclat, size = Rmax, color = vmax_gl),
               alpha = 1) +
  geom_line(data = filter(will_input_summaries, storm_id == "Andrea-2013"),
            aes(x = -tclon, y = tclat))

map_int_tracks <- function(storm_track, a = a, storm_name){
  full_track <- create_full_track(hurr_track = storm_track)
  track_map <- a + geom_path(data = full_track, aes(x = -tclon, y = tclat)) +
    ggtitle(storm_name)
  print(track_map)
}

map_tracks <- function(storm_track, a = a, storm_name){
  track_map <- a + geom_point(data = storm_track,
                             aes(x = longitude, y = latitude,
                                 color = date)) +
    ggtitle(storm_name)
  print(track_map)
}

pdf("Ex_tracks.pdf")
mapply(map_int_tracks, storm_track = track_list,
       storm_name = names(track_list),
       MoreArgs = list(a = a))
dev.off()


