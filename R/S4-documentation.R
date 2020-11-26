


# celltracer-object -------------------------------------------------------

cto <- setClass(Class = "cto", 
                slots = c(
                  data = "list",
                  cluster_info = "list",
                  wp_info = "list",
                  set_up = "list",
                  storage_info = "list",
                  background_info = "list" 
                  )
                )
