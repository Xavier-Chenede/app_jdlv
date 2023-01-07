## Coating with NA if needed the grids to avoid calculation errors on the edge.
## 2 lines are necessary



setup_matrix <- function(in_m){

        gen_x <- matrix(nrow=0,ncol=0)
        
        #check the edge of the input grid
        s <- sum(in_m[1,], in_m[nrow(in_m),],   in_m[,1], in_m[,ncol(in_m)],
                 in_m[2,], in_m[nrow(in_m)-1,], in_m[,2], in_m[,ncol(in_m)-1],
                 na.rm=TRUE)
        
        # NA Coating
        if(s > 0){
                
                col_na <- matrix(rep(NA, times=nrow(in_m)))
                pregen_x <- cbind(col_na,col_na,in_m,col_na,col_na)
                
                row_add <- matrix(ncol=ncol(pregen_x))
                gen_x <<- rbind(row_add,row_add,  pregen_x,row_add,row_add)

                
        } else  gen_x <<- in_m
        
        
}


