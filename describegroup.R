require("dplyr")
require("lazyeval")
require("Hmisc")

describegroup <- function(data=dh5, by="ittyp", vars=c("ebay_is_ratio", "income")){
  levels <- data %>% select_(.dots = by) %>% as.data.frame  %>% unique()  
  levels <- levels[,1]
  for (i in 1:length(levels)) {
    paste(by, "==", levels[i]) %>% print()
    filter_crit = interp(~ filter_var %in% levels[i], filter_var = as.name(by))
    data %>% filter_(filter_crit) %>% select_(.dots = vars) %>% Hmisc::describe() %>% print()
  }
}
