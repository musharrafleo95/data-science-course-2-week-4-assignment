rankall<-function(outcome, num = "best") {
        Decreasing_order = FALSE
        if (num=="worst") {Decreasing_order = TRUE}
        oldw <- getOption("warn") ## ignoring warnings
        options(warn = -1)
        
        x<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        x<-x[order(x$Hospital.Name),] ## hospital name are alphabetically arranged
        if(outcome=="heart attack") {
                y<-x[order(as.numeric(x[,11]),na.last = NA,decreasing=Decreasing_order),] ##arranging w.r.t heart attack
                
        }
        else if(outcome=="heart failure") {
                y<-x[order(as.numeric(x[,17]),na.last = NA,decreasing=Decreasing_order),] ##arrangin w.r.t heart failure
                
        }
        else if(outcome=="pneumonia") {
                y<-x[order(as.numeric(x[,23]),na.last = NA,decreasing=Decreasing_order),] ##arranging w.r.t pneumonia

        }
        else {
                message(sprintf("Error in best(%s,%s) : invalid outcome",state,outcome))
                return(invisible(outcome))} ## if wrong outcome then exit
        
        
        state<-y$State #arrangin all unique names in alphabetical order
        state<-unique(state)
        state<-sort(state)
        
        k<-data.frame()
        for( i in state){ #arranging the data w.r.t state and giving them their rank
                z<-y[y$State==i,]
                n_row<-nrow(z)
                z<-cbind(z,Rank=1:n_row)
                k<-rbind(k,z)
        }
        l<-data.frame(hospital=k$Hospital.Name,state=k$State,Rank=k$Rank)#only getting the required variable for showing in result and calculation
        options(warn = oldw) #again activating warning
        row_y<-max(l$Rank) #getting maximum rank so that error can be produced if 
                           #rank is out of range
        if(num=="best"|num=="worst"){ #arranging w.r.t bestest or worstest
                l<-l[l$Rank==1,]
                l$Rank <- NULL
                return(l)
        }
        else if(num>row_y) return(print(NA)) #if rank out of range then exit
        else{
                
               l<- l[as.numeric(l$Rank)==num,] # giving result according to rank
               l$Rank=NULL
               return(l)
        }
}