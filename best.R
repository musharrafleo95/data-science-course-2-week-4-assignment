best<-function(state,outcome){
        oldw <- getOption("warn") ## ignoring warnings
        options(warn = -1)
        x<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        if(outcome=="heart attack") {
                y<-x[order(as.numeric(x[,11])),] ##arranging w.r.t heart attack
                k=11
                }
        else if(outcome=="heart failure") {
                y<-x[order(as.numeric(x[,17])),] ##arrangin w.r.t heart failure
                k=17
                }
        else if(outcome=="pneumonia") {
                y<-x[order(as.numeric(x[,23])),] ##arranging w.r.t pneumonia
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
        
        y<-y[y$State==state,] ##arranging w.r.t the state
        l<-y[,k] ##getting values of the outcome column
        l<-as.numeric(unique(l)) ##getting unique values of outcome
        z<-y[(as.numeric(y[,k])==l[1]),2] ##getting names w.r.t first unique value
        z=sort(z) ## arranging name
        options(warn = oldw) ##returning default setting
        z[1] ##printing the first name
}