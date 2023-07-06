activity_types <- c("fishing",
                    "tourism",
                    "research",
                    "supply",
                    "other")
subset_edge_lists <- function(vessel_filter,all_vessels,unsummarised_observations){
  activity_edge_lists<-list()
  for(i in 1:length(vessel_filter)){
    tmp<-as.character(vessel_filter[i])
    vessels<- all_vessels %>%
      filter(activity_type == tmp)
    edge_attributes <- unsummarised_observations %>% 
      filter(vessel_id %in% vessels$vessel_id) %>% 
      arrange(move) %>%  
      group_by(move, from_place, to_place,from_ecoregion, to_ecoregion, from_province, to_province, from_realm, to_realm) %>% 
      summarise(n_voyages = n(), n_ships = n_distinct(vessel_id), n_trips = n_distinct(trip_id.x)) %>% 
      mutate(from = from_place, to = to_place) %>% 
      dplyr::select(from, to, everything())
    times_spent <- unsummarised_observations %>% 
      filter(vessel_id %in% vessels$vessel_id) %>% 
      filter(arrival_qualifier %!in% c("B", "A", "T") & sailing_qualifier %!in% c("B", "A", "T")) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(time_spent = lubridate::as.duration(from_tms %--% to_tms_2)) %>% 
      dplyr::filter(time_spent != lubridate::as.duration(0)) %>% 
      dplyr::group_by(move, from_place, to_place, from_ecoregion, to_ecoregion) %>% 
      dplyr::summarise(total_time = as.duration(sum(as.double(time_spent))),
                       median_time = as.duration(median(as.double(time_spent))),
                       mean_time = as.duration(mean(as.double(time_spent))),
                       n_time = n())
    edge_list <- edge_attributes %>% 
      full_join(times_spent, by = c("move", "from_place", "to_place","from_ecoregion", "to_ecoregion"))
    activity_edge_lists[[i]]<-assign(paste("edge_list_",tmp,sep=""),edge_list)
  }
  names(activity_edge_lists) <- activity_types
  return(activity_edge_lists)
}


