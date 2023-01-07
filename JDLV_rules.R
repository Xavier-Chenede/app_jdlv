

# Input structure
val <- c(rep(0,11), 1,1,1,rep(0,11))
m3 <- matrix(val,nrow=5, ncol=5)
m3 <- reduced_mtrx(m3)
m3 <- setup_matrix(m3)
m3


mini_struct <- matrix(nrow=3, ncol=7)
mini_struct[,1] <- c(0,0,1)
mini_struct[,2] <- c(1,0,1)
mini_struct[,3] <- c(0,0,0)
mini_struct[,4] <- c(0,1,0)
mini_struct[,5] <- c(0,0,1)
mini_struct[,6] <- c(0,0,1)
mini_struct[,7] <- c(0,0,1)

mini_struct <- reduced_mtrx(mini_struct)
mini_struct <- setup_matrix(mini_struct)

# Struc_in <- mini_struct
# Struc_in <- m3
Struc_in <- w



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
        nb_c <- ncol(Struc_in)
        nb_r <- nrow(Struc_in)
        coord_r <- rep(2:(nb_r-1), each=(nb_c-2))
        coord_c <- rep(2:(nb_c-1), times=(nb_r-2))
        bas_list <- as.list(c(1:((nb_c-2)*(nb_r-2))))
        l <- lapply(bas_list, function(rc) c(coord_r[rc],coord_c[rc])) 
        
        
        
        # l <- list (
        #         c(2, 2),
        #         c(2, 3),
        #         c(2, 4),
        #         c(3, 2),
        #         c(3, 3),
        #         c(3, 4),
        #         c(4, 2),
        #         c(4, 3),
        #         c(4, 4)
        # )      
        
        
        #apply rules function to all positions of the matrix
        lapply(l,rules)
        data_out <- reduced_mtrx(data_out)
        h <- setup_matrix(data_out)
        
        setup_matrix(data_out)
        

}
        
Struc_in
ng <- newgen(Struc_in)
ng
plot(ng)
plot(Struc_in)
plot(w)
y <- newgen(w)
y
plot(y)

Struc_in <- w

nb_c <- ncol(Struc_in)
nb_r <- nrow(Struc_in)
coord_r <- rep(2:(nb_r-1), each=(nb_c-2))
coord_c <- rep(2:(nb_c-1), times=(nb_r-2))
bas_list <- as.list(c(1:((nb_c-2)*(nb_r-2))))
l <- lapply(bas_list, function(rc) c(coord_r[rc],coord_c[rc]))


