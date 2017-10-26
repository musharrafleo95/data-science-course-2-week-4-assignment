rankhospital<-function(state, outcome, num = "best") {
        Decreasing_order = FALSE
        if (num=="worst") {Decreasing_order = TRUE}
        oldw <- getOption("warn") ## ignoring warnings
        options(warn = -1)
        
        x<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        x<-x[order(x$Hospital.Name,na.last=NA),] ## hospital name are alphabetically arranged
        if(outcome=="heart attack") {
                y<-x[order(as.numeric(x[,11]),na.last = NA,decreasing=Decreasing_order),] ##arranging w.r.t heart attack
                k=11
        }
        else if(outcome=="heart failure") {
                y<-x[order(as.numeric(x[,17]),na.last = NA,decreasing=Decreasing_order),] ##arrangin w.r.t heart failure
                k=17
        }
        else if(outcome=="pneumonia") {
                y<-x[order(as.numeric(x[,23]),na.last = NA,decreasing=Decreasing_order),] ##arranging w.r.t pneumonia
                k=23
        }
        else {
                message(sprintf("Error in best(%s,%s) : invalid outcome",state,outcome))
                return(invisible(outcome))} ## if wrong outcome then exit
        
        not_state<-names(table(x$State))
        if(!(sum(state==not_state))) {
                message(sprintf("Error in best(%s,%s) : invalid state",state,outcome))
                return(invisible(state)) ##if wrong state then exit
        }
        y<-y[y$State==state,]
        row_y<-nrow(y)
        options(warn = oldw)
        if(num=="best"|num=="worst"){
                d<-data.frame(Provider.No = y$Provider.Number,Hospital.Name = y$Hospital.Name,
                              Rate=y[,k],Rank = 1:row_y )
                return(d)
        }
        else if(num>row_y) (return(print(NA)))
        else{
                d<-data.frame(Provider.No = y$Provider.Number,Hospital.Name = y$Hospital.Name,
                              Rate=y[,k],Rank = 1:row_y )
                return(d[d$Rank==num,])
        }
        
}