## Coating with NA if needed the grids to avoid calculation errors on the edge.
## 2 lines are necessary



setup_matrix <- function(in_m){
        
        gen_x <- matrix(nrow=0,ncol=0)
        
        
        #check if only one row or one column
        nr <- nrow(in_m)
        nc <- ncol(in_m)
        
        if (nr>1 & nc >1){
                
                #check the edge of the input grid
                s <- sum(in_m[1,], in_m[nrow(in_m),],   in_m[,1], in_m[,ncol(in_m)],
                         in_m[2,], in_m[nrow(in_m)-1,], in_m[,2], in_m[,ncol(in_m)-1],
                         na.rm=TRUE)} else  s <- 0
        
        # 0 Coating
        if(s > 0 | nr==1 | nc ==1){
                
                col_0 <- matrix(rep(0, times=nrow(in_m)))
                pregen_x <- cbind(col_0,col_0,in_m,col_0,col_0)
                
                row_add <- matrix(rep(0,ncol(pregen_x)),ncol=ncol(pregen_x))
                gen_x <<- rbind(row_add,row_add, pregen_x,row_add,row_add)
                
                
        } else  gen_x <<- in_m
        
}


