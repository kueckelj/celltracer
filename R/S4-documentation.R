


# celltracer-object -------------------------------------------------------

cto <- setClass(Class = "cto", 
                slots = c(
                  analysis = "list",
                  compatibility = "list",
                  data = "list",
                  default = "list",
                  information = "list",
                  name = "character",
                  set_up = "list",
                  well_plates = "list", 
                  variable_sets = "list",
                  variable_statistics = "list",
                  version = "list"
                  )
                )
