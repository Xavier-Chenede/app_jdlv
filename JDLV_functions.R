
# Function to generate the new generation of cells according to the rule
# of the Conway's game of life


newgen <- function (gen_to_run) {
        
        gen_next <-  matrix(nrow=nrow(gen_to_run), ncol=ncol(gen_to_run))
        
        # rules#1
        for (r in 2:nrow(gen_to_run)-1){
                for (c in 2:ncol(gen_to_run)-1){
                        if (((sum(gen_to_run[r-1,c-1],
                                  gen_to_run[r-1,c],
                                  gen_to_run[r-1,c+1],
                                  gen_to_run[r,c+1],
                                  gen_to_run[r,c-1],
                                  gen_to_run[r+1,c-1],
                                  gen_to_run[r+1,c],
                                  gen_to_run[r+1,c+1],
                                  na.rm=TRUE)%in% c(2,3))) & (!is.na(gen_to_run[r,c]))) {
                                gen_next[r,c]=1
                                
                        }
                }
        }
        
        # rules#2
        for (r in 2:nrow(gen_to_run)-1){
                for (c in 2:ncol(gen_to_run)-1){
                        if (((sum(gen_to_run[r-1,c-1],
                                  gen_to_run[r-1,c],
                                  gen_to_run[r-1,c+1],
                                  gen_to_run[r,c+1],
                                  gen_to_run[r,c-1],
                                  gen_to_run[r+1,c-1],
                                  gen_to_run[r+1,c],
                                  gen_to_run[r+1,c+1],
                                  na.rm=TRUE)==3)) & (is.na(gen_to_run[r,c])))  {
                                gen_next[r,c]=1
                                
                        }
                }
        }
        gen_next <- reduced_mtrx(gen_next)
        setup_matrix(gen_next)
}



## Coating with 0 if needed the grids to avoid calculation errors on the edge.
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


#function to remove from the edge of the matrix the empty rows and columns 

reduced_mtrx <- function(in_m){
        
        #check the top of the input grid
        st <-  sum(in_m[1,],na.rm=TRUE)
        while(st < 1){
                in_m <- as.matrix(in_m[c(2:nrow(in_m)),])
                st <-  sum(in_m[1,],na.rm=TRUE)
        }
        
        #check the bottom 
        sb <-  sum(in_m[nrow(in_m),],na.rm=TRUE)
        while(sb < 1){
                in_m <- as.matrix(in_m[c(1:nrow(in_m)-1),])
                if (ncol(in_m)==1 & nrow(in_m) >1 ) {in_m <- t(in_m)}
                sb <-  sum(in_m[nrow(in_m),],na.rm=TRUE)
        }
        
        #check the left 
        sl <-  sum(in_m[,1],na.rm=TRUE)
        while(sl < 1){
                
                if (nrow(in_m) == 1) {
                        in_m <- t(as.matrix(in_m[, c(2:ncol(in_m))]))
                } else {
                        in_m <- as.matrix(in_m[, c(2:ncol(in_m))])
                }
                
                sl <-  sum(in_m[,1],na.rm=TRUE)        
        }
        
        #check the right 
        sr <-  sum(in_m[,ncol(in_m)],na.rm=TRUE)
        while(sr < 1){
                if (nrow(in_m) == 1) {
                        in_m <- t(as.matrix(in_m[,c(1:ncol(in_m)-1)]))
                } else {
                        in_m <- as.matrix(in_m[,c(1:ncol(in_m)-1)])
                }
                sr <-  sum(in_m[,ncol(in_m)],na.rm=TRUE)
        }
        in_m
} 








