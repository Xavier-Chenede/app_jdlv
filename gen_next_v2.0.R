# Function to generate the new generation of cells according to the rule
# of the Conway's game of life


newgen <- function (data_in) {
        
        data_out <-  matrix(rep (0, nrow(data_in)*ncol(data_in)),nrow=nrow(data_in), ncol=ncol(data_in))
        
        #function to apply rules
        rules <- function(pos) {
                s <-  sum(
                        data_in [pos[1] - 1, pos[2] - 1],
                        data_in [pos[1] - 1, pos[2]]    ,
                        data_in [pos[1] - 1, pos[2] + 1],
                        data_in [pos[1]    , pos[2] + 1],
                        data_in [pos[1]    , pos[2] - 1],
                        data_in [pos[1] + 1, pos[2] - 1],
                        data_in [pos[1] + 1, pos[2]]    ,
                        data_in [pos[1] + 1, pos[2] + 1],
                        na.rm = TRUE
                )
                
                if ( 
                        (data_in [pos[1], pos[2]] == 1 & s %in% c(2, 3)) #rule 1
                        |
                        (data_in [pos[1], pos[2]] != 1 & s==3) #rule 2
                ){
                        data_out[pos[1], pos[2]] <<-  1
                        
                }
                else {
                        data_out[pos[1], pos[2]] <<-  0
                }
        }
        
        #create a list 'l' of the matrix coordinates for which the algorithm has to be applied
        nb_c <- ncol(data_in)
        nb_r <- nrow(data_in)
        coord_r <- rep(2:(nb_r-1), each=(nb_c-2))
        coord_c <- rep(2:(nb_c-1), times=(nb_r-2))
        bas_list <- as.list(c(1:((nb_c-2)*(nb_r-2))))
        l <- lapply(bas_list, function(rc) c(coord_r[rc],coord_c[rc])) 
        
        
        
        #apply rules function to all positions of the matrix
        lapply(l,rules)
        data_out <- reduced_mtrx(data_out)
        setup_matrix(data_out)
        
        
}

