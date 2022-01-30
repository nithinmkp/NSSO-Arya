# Functions ---------------------------------------------------------------


# Functions ---------------------------------------------------------------

## ---- Package Function
package_fn<-function(pkg){
        new.pkg<-setdiff(pkg,installed.packages()[,"Package"])
        if(length(new.pkg)){
                install.packages(new.pkg,dependencies = T)
                sapply(pkg,library,character.only=T)
        }else{
                sapply(pkg,library,character.only=T)
        }
}


## ---- type conversion function
convert_fn<-function(df, col_ind,fn,...) {
        df <- df %>% mutate(across(.cols = col_ind, .fns = fn,...))
}

