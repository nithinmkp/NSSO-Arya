# Functions ---------------------------------------------------------------

#Package Function
package_fn<-function(pkg){
        new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
        if(length(new.pkg)){
                install.packages(new.pkg,dependencies = T)
        }else{
                sapply(pkg,library,character.only=T)
        }
}
