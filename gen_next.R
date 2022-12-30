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
        setup_matrix(gen_next)
}

