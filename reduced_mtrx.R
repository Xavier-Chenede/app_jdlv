#function to remove from the edge of the matrix the empty rows and columns 


reduced_mtrx <- function(in_m){
        
        #check the top of the input grid
        st <-  sum(in_m[1,],na.rm=TRUE)
        while(st < 1){
                in_m <- in_m[c(2:nrow(in_m)),]
                st <-  sum(in_m[1,],na.rm=TRUE)
        }

        #check the bottom 
        sb <-  sum(in_m[nrow(in_m),],na.rm=TRUE)
        while(sb < 1){
                in_m <- in_m[c(1:nrow(in_m)-1),]
                sb <-  sum(in_m[nrow(in_m),],na.rm=TRUE)
        }

        #check the left 
        sl <-  sum(in_m[,1],na.rm=TRUE)
        while(sl < 1){
                in_m <- in_m[,c(2:ncol(in_m))]
                sl <-  sum(in_m[,1],na.rm=TRUE)        
        }

        #check the right 
        sr <-  sum(in_m[,ncol(in_m)],na.rm=TRUE)
        while(sr < 1){
                in_m <- in_m[,c(1:ncol(in_m)-1)]
                sr <-  sum(in_m[,ncol(in_m)],na.rm=TRUE)
        }
        in_m
} 

