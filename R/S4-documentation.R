


# celltracer-object -------------------------------------------------------

cto <- setClass(Class = "cto", 
                slots = c(
                  analysis = "list",
                  compatibility = "list",
                  cdata = "list",
                  vdata = "list",
                  default = "list",
                  information = "list",
                  name = "character",
                  set_up = "list",
                  well_plates = "list", 
                  variable_sets = "list",
                  version = "list"
                )
)