worst <- function(state, outcome, st = "a") {
       if (!state %in% unique(rates$State))
              return("Invalid State")
       if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
              return("Invalid Outcome")
       
       order_selected <- hosp_sort(state,outcome)
       
       return(order_selected[nrow(order_selected),1])
}

rankhospital <- function(state, outcome, num = "best", st = "a"){
       if (num == "best")
              return(best(state,outcome))
       if (num == "worst")
              return(worst(state,outcome))       
       else {order_selected <- hosp_sort(state,outcome) 
              return(order_selected[num,1])}
}
