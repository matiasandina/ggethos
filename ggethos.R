ethogram <- function(behavior){
  df <- 
    tibble::tibble(behavior = behavior,
                   time_col = 1:length(behavior)) %>% 
    dplyr::mutate(lg = dplyr::lag(behavior, default = "first frame"),
                  # check if there's continuity
                  flag = lg != behavior) %>% 
    dplyr::filter(flag) %>% 
    dplyr::mutate(time_end = dplyr::lead(time_col,
                                         n = 1,
                                         default = dplyr::last(time_col))) %>% 
    dplyr::rename(x = time_col,
                  xend = time_end)
  cols_to_keep <- c("x", "xend", "behavior")
  return(df[, cols_to_keep])
  
}

setup_data <- function(data, params){
  #if (anyDuplicated(data$group)) {
  #  data$group <- paste(data$group, seq_len(nrow(data)), sep = "-")
  #}
  if ("x" %in% names(data) == FALSE){
    data$x <- 1:nrow(data)
  }
  return(data)
  
}


StatEtho <- ggproto("StatEtho", Stat, 
                    setup_data = setup_data,
                      compute_panel = function(data, scales) {
                        print(head(data, n=10))
                        summ_data <- 
                          data %>% 
                          group_by(y) %>% 
                          summarise(ethogram(behavior)) %>% 
                          mutate(yend = y)
                        if ("colour" %in% names(data)){
                          color_df <- distinct(data, y, behavior, colour)
                          summ_data <- left_join(summ_data, color_df, by=c("y", "behavior"))
                        }
                        print(head(summ_data))
                        return(summ_data)
                      },
                      required_aes = c("y")
)


geom_ethogram <- function(mapping = NULL,
                          data = NULL,
                          stat = "etho",
                          position = "identity",
                          ...,
                          size=5,
                          arrow = NULL,
                          lineend = "butt",
                          linejoin = "round",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSegment,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      size=size,
      arrow = arrow,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}


# This is working
ggplot(df, aes(y=y, behavior=target))+
  geom_ethogram()

ggplot(df, aes(y=y, behavior=target, color=target))+
  geom_ethogram()
# 

# This fails to produce expected result
# problem is length(behavior) is 1 when passed as the grouping
ggplot(df, aes(x=1:nrow(df), 
               y=target, 
               behavior=target, 
               color=target))+
  geom_ethogram() +
  facet_wrap(~y)
